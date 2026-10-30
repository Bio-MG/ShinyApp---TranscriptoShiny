# =============================================================================
# test-mod-sc-communication-ui.R — modules/sc/mod_sc_communication.R (onglet
# « Limites »)
# =============================================================================
# Item 2G du packet round-7 (O28 batch B3).
#
# Constat 2G : à HEAD, l'onglet « Limites » empilait 5 bannières `alert-*`
# pleine largeur (1 warning, 2 info, 2 light ; preuve
# mod_sc_communication.R:247-256 à HEAD 88f9a43 ; spéc §2.15). Correctif : UNE
# carte synthétique à puces thématiques, hiérarchie de sévérité conservée via
# la couleur des puces (warning > info > light).
#
# ⚠️ Coordination 3D (packet row 2G) : le TEXTE de la 5ᵉ bannière (objet RDS
# CellChat) est l'objet du constat 3D (batch B2). Ce lot le reproduit VERBATIM
# et le verrou ci-dessous garantit qu'il n'a PAS été reformulé ici.
#
# ⚠️ Limite documentée : mod_sc_communication_ui() embarque les UI de 4
# sous-modules (spatial/trajectory/velocity_ctx/perturbation) — le rendu
# headless complet n'est pas réalisable à coût raisonnable ; la preuve est un
# verrou source structurel (pattern test-mod-sc-velocity.R).
# =============================================================================

.MSCC_UI_FILE <- "modules/sc/mod_sc_communication.R"

.n_hits <- function(src, pattern) {
  lengths(regmatches(src, gregexpr(pattern, src, fixed = TRUE)))[1]
}

test_that("2G : l'onglet Limites est une carte synthétique (header + 5 puces)", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_UI_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Carte synthétique à en-tête (spéc §2.15) :
  expect_match(src, "Points d'attention \u2014 lecture des r\u00e9sultats", fixed = TRUE)
  # Les 5 puces thématiques :
  for (chip in c("Validit\u00e9 technique vs biologique",
                 "Comparabilit\u00e9 des scores",
                 "Causalit\u00e9",
                 "Labels cellulaires",
                 "Objets RDS CellChat")) {
    expect_match(src, chip, fixed = TRUE, info = chip)
  }
})

test_that("2G : plus aucune bannière alert-* empilée — la seule restante est l'en-tête du module", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_UI_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # À HEAD : 6 `class = "alert alert-` (1 bannière d'en-tête L26 + 5 dans
  # l'onglet Limites). Après 2G : il ne reste que la bannière d'en-tête.
  expect_identical(.n_hits(src, "class = \"alert alert-"), 1L,
                   info = "des bannières alert-* subsistent dans l'onglet Limites (constat 2G)")
  # Hiérarchie de sévérité conservée sur les puces (1 warning > 2 info > 2 light) :
  expect_identical(.n_hits(src, "badge bg-warning"), 1L)
  expect_identical(.n_hits(src, "badge bg-info"), 2L)
  expect_identical(.n_hits(src, "badge bg-secondary"), 2L)
})

test_that("2G : les 5 textes d'origine sont préservés VERBATIM (coordination 3D)", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_UI_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "Validite technique uniquement : un import valide n'implique aucune validite biologique.",
               fixed = TRUE)
  expect_match(src, "Les scores ne sont pas compares entre sources differentes (CellChat vs CellPhoneDB).",
               fixed = TRUE)
  expect_match(src, "Une association ligand-receptor n'etablit aucune causalite.",
               fixed = TRUE)
  expect_match(src, "Les labels sans correspondance exacte dans la colonne choisie sont conserves tels quels et listes — jamais renommes.",
               fixed = TRUE)
  # 5ᵉ texte = objet du constat 3D (batch B2) — VERBATIM ici, jamais reformulé :
  expect_match(src, "Objet CellChat (.rds) : extraction de resultats deja calcules uniquement — le mapping ligand-receptor vers pathway n'est pas reconstitue.",
               fixed = TRUE)
})
