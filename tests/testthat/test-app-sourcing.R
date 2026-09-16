# =============================================================================
# test-app-sourcing.R -- GARDE : app.R source TOUT fichier de R/ et modules/
# =============================================================================
# POURQUOI CE FICHIER
#
# Incident du 2026-09-16 : l'application ne demarrait plus du tout --
#   Erreur dans bulk_gene_set_choices(.tr_plain) :
#   impossible de trouver la fonction "bulk_gene_set_choices"
#   Called from: hasGroups(choices)
#
# Cause : R/bulk/bulk_gene_sets.R (ajoute par 440878b) existait et etait
# TESTE, mais n'etait PAS source par app.R. Le test le sourcait lui-meme
# (source_project_file), donc il passait pendant que l'app etait cassee :
# le harnais de test contournait la liste de sources de app.R.
#
# MEME CLASSE, trouvee en verifiant : R/plotting/plot_dims.R (PLOT-S6) n'etait
# pas source non plus, alors que global.R enveloppe renderPlot() et appelle
# ts_render_plot_args() a CHAQUE renderPlot() => deuxieme P0, masque par le
# premier (l'erreur d'UI tombe avant le premier plot).
#
# PRECEDENT (le "patch CellChat") : test-sc-communication-engine-ui.R portait
# deja un test_that("app.R sources the engine") pour UN fichier precis. Ici on
# generalise : la propriete verifiee est l'EXHAUSTIVITE, pas un fichier.
#
# ASCII pur : un garde ne doit pas dependre de la locale (cf. CONVENTIONS C7).
# =============================================================================

.app_src <- paste(readLines(file.path(ts_project_root(), "app.R"), warn = FALSE),
                  collapse = "\n")

# --- 1. Sources declarees explicitement : source("...") ---------------------
.app_explicit <- function(src) {
  hits <- regmatches(src, gregexpr('source\\("([^"]+)"\\)', src))[[1L]]
  if (!length(hits)) return(character(0))
  sub('"\\)$', "", sub('^source\\("', "", hits))
}

# --- 2. Sources declarees par list.files(<dir>, pattern = "\\.R$") ----------
# app.R fait :
#   for (f in list.files("modules/bulk_de", pattern = "\\.R$", ...)) source(f)
.app_listed <- function(src) {
  dirs <- regmatches(src, gregexpr('list\\.files\\(\\s*"([^"]+)"', src))[[1L]]
  if (!length(dirs)) return(character(0))
  # ATTENTION : regmatches() renvoie la correspondance ENTIERE, guillemet
  # fermant INCLUS ("list.files(\"modules/bulk_de\""). Ne retirer que le prefixe
  # laissait un " en fin de chaine => dir.exists() FALSE => 0 fichier, et le
  # garde signalait 9 faux manquants (mesure 2026-09-16). D'ou les DEUX sub().
  dirs <- sub('^list\\.files\\(\\s*"', "", dirs)
  dirs <- sub('"$', "", dirs)
  out <- character(0)
  for (d in dirs) {
    full <- file.path(ts_project_root(), d)
    if (dir.exists(full)) {
      out <- c(out, file.path(d, list.files(full, pattern = "\\.R$")))
    }
  }
  out
}

# --- 3. Tous les fichiers .R du domaine, sur disque -------------------------
.app_on_disk <- function() {
  out <- character(0)
  for (d in c("R", "modules")) {
    full <- file.path(ts_project_root(), d)
    if (dir.exists(full)) {
      out <- c(out, file.path(d, list.files(full, pattern = "\\.R$",
                                            recursive = TRUE)))
    }
  }
  sort(gsub("\\\\", "/", out))
}

# --- 4. Exceptions DECLAREES (chacune justifiee) ----------------------------
.app_allowlist <- c(
  # Re-export LEGACY : la definition canonique est R/core/state.R, source
  # avant lui. Son propre en-tete dit "app.R no longer sources this file as
  # canonical" -- il ne sert qu'aux tests/appels externes qui le sourcent
  # directement.
  "R/sc/sc_state.R",
  # Module PARQUE (vague 8 / backlog, session separee) : definit
  # mod_spatial_lr_ui/_server mais AUCUN appelant dans le depot (verifie :
  # grep "mod_spatial_lr" hors de ce fichier => 0). L'app demarre sans.
  # A RETIRER de cette liste le jour ou il est branche.
  "modules/spatial/mod_spatial_lr.R"
)

.app_missing <- function(on_disk, declared, allow = .app_allowlist) {
  setdiff(setdiff(on_disk, declared), allow)
}

# --- 5. Le detecteur lui-meme (cas NEGATIF, obligatoire) -------------------
test_that("the detector itself works (negative case)", {
  # Sans ce test, un detecteur casse renverrait "0 manquant" et le garde
  # serait vert POUR TOUJOURS -- exactement le faux temoin qu'on veut eviter
  # (meme classe que le filtre de run_tests.R, corrige le meme jour).
  fake_disk <- c("R/a.R", "R/b.R", "R/sc/sc_state.R")
  expect_identical(
    .app_missing(fake_disk, declared = c("R/a.R"), allow = c("R/sc/sc_state.R")),
    "R/b.R"
  )
  # et il ne doit RIEN signaler quand tout est declare
  expect_length(.app_missing(fake_disk, fake_disk), 0L)
})

test_that("both declaration mechanisms are actually detected", {
  # Si une regex casse (app.R reformate), l'extraction renverrait peu ou rien
  # et le garde deviendrait vert a tort : on verifie que les DEUX formes sont
  # bien vues.
  expect_gt(length(.app_explicit(.app_src)), 50L)
  expect_gt(length(.app_listed(.app_src)), 0L)
})

test_that("every R/ and modules/ file is sourced by app.R", {
  missing <- .app_missing(.app_on_disk(),
                          c(.app_explicit(.app_src), .app_listed(.app_src)))
  expect_identical(
    missing, character(0),
    info = paste0(
      "Fichier(s) present(s) sur disque mais JAMAIS source(s) par app.R : ",
      paste(missing, collapse = ", "),
      " -- l'app plantera au premier appel (impossible de trouver la fonction). ",
      "Ajouter le source() dans app.R, ou inscrire le fichier dans ",
      ".app_allowlist avec sa justification."
    )
  )
})

test_that("no stale source() entry points at a missing file", {
  declared <- .app_explicit(.app_src)
  declared <- declared[grepl("\\.R$", declared)]
  absent <- declared[!file.exists(file.path(ts_project_root(), declared))]
  expect_identical(
    absent, character(0),
    info = paste("source() vers un fichier inexistant :",
                 paste(absent, collapse = ", "))
  )
})

test_that("the two 2026-09-16 P0 fixes stay pinned", {
  # Regression exacte de l'incident : ces deux lignes ne doivent jamais
  # disparaitre.
  expect_true(grepl('source("R/bulk/bulk_gene_sets.R")', .app_src, fixed = TRUE))
  expect_true(grepl('source("R/plotting/plot_dims.R")', .app_src, fixed = TRUE))
})
