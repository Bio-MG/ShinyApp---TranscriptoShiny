# =============================================================================
# test-mod-sc-corr.R — modules/sc/mod_sc_corr.R
# =============================================================================
# Items 1D et 3C du packet round-7 (O28 batch B1).
#
# 1D — le graphe ET les deux exports étaient codés `from = input$target_gene`
# (à HEAD : :169 graphe, :262 export PNG, :283 export edge list) : si
# l'utilisateur déplace le sélecteur sans relancer « Rechercher Genes
# Correles », le graphe connecte le NOUVEAU gène à la liste des corrélés de
# l'ANCIEN seed. Correctif (spéc §2.4) : le seed est attaché au résultat
# (attr "seed_gene", posé à la fois par le run manuel et par le miroir de
# l'auto-pipeline), résolu par la fonction pure `.corr_seed_gene()`, et un
# badge d'écart au-dessus du graphe affiche « Gène cible calculé : %s » +
# « Modification non recalculée » si le sélecteur diverge.
#
# 3C — « Ajouter sélection à Viz » : `req(corr_rv(),
# input$correlation_table_rows_selected)` avortait EN SILENCE quand aucune
# ligne n'était sélectionnée (le warning du else n'était jamais atteint :
# req() jette avant). Correctif (spéc §2.13) : test conditionnel +
# showNotification warning visible.
#
# ⚠️ Piège documenté (test-mod-sc-metadata.R) : testServer() ne fait pas
# échouer le test quand un observer lève — on assère les EFFETS OBSERVABLES
# (notifications capturées, shared_rv$selected_genes, HTML du badge).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")        # create_sc_shared_state
source_project_file("modules/sc/mod_sc_corr.R")

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))   # renderDT() : appelé à l'ENREGISTREMENT
                                              # de output$correlation_table dans le
                                              # corps du module (pas seulement au rendu)

# .t_fmt (global.R) n'est pas sourcé par le bootstrap de test — extraction de
# la VRAIE définition (pattern de test-mod-bulk-de-multimethod.R:84).
assign(".t_fmt", ts_ast_assignment("global.R", ".t_fmt"), envir = globalenv())

.b1_global <- function() {
  shiny::reactiveValues(sc_obj = NULL, i18n = NULL, language = "fr")
}

.b1_corr_df <- function() {
  data.frame(gene         = c("G1", "G2", "G3"),
             correlation  = c(0.9, -0.4, 0.6),
             abs_correlation = c(0.9, 0.4, 0.6),
             stringsAsFactors = FALSE)
}

# Capture des notifications : showNotification() est résolu depuis globalenv
# (les fichiers modules/ y sont sys.source()és par source_project_file) — le
# shim est posé PUIS RESTAURÉ (pattern on.exit de test-mod-sc-drive.R:418).
.capture_notifications <- function(code) {
  old <- get0("showNotification", envir = globalenv())
  box <- new.env(parent = emptyenv())
  box$items <- list()
  assign("showNotification", function(ui, ..., type = "message", duration = NULL) {
    box$items[[length(box$items) + 1L]] <- list(ui = as.character(ui), type = type)
    invisible(NULL)
  }, envir = globalenv())
  on.exit({
    if (is.null(old)) rm(list = "showNotification", envir = globalenv())
    else assign("showNotification", old, envir = globalenv())
  }, add = TRUE)
  force(code)
  box$items
}

# ---------------------------------------------------------------------------
# 1D — résolution du seed (fonction pure, testable headless)
# ---------------------------------------------------------------------------
test_that("1D : .corr_seed_gene — l'attr du résultat prime, puis shared, puis sélecteur", {
  df <- .b1_corr_df()
  # Seed attaché au résultat : c'est LUI qui gagne (constat 1D).
  expect_identical(
    .corr_seed_gene(structure(df, seed_gene = "SKAP1"), "AUTRE", "PICKER"),
    "SKAP1")
  # Seed inconnu (NA / "") => repli sur le miroir de l'auto-pipeline :
  expect_identical(
    .corr_seed_gene(structure(df, seed_gene = NA_character_), "SHARED", "PICKER"),
    "SHARED")
  expect_identical(
    .corr_seed_gene(structure(df, seed_gene = ""), "SHARED", "PICKER"),
    "SHARED")
  expect_identical(.corr_seed_gene(df, "SHARED", "PICKER"), "SHARED")
  # Rien de connu (état legacy) => comportement d'antan : le sélecteur.
  expect_identical(.corr_seed_gene(df, NULL, "PICKER"), "PICKER")
})

# ---------------------------------------------------------------------------
# 1D — badge d'écart au-dessus du graphe (spéc §2.4)
# ---------------------------------------------------------------------------
test_that("1D : le badge affiche le seed calculé et alerte si le sélecteur diverge", {
  gd <- .b1_global()
  sr <- create_sc_shared_state()
  badge_html <- NULL
  badge_synced <- NULL

  testServer(mod_sc_corr_server,
             args = list(id = "corr", global_data = gd, shared_rv = sr), {
    # Ce que fait l'auto-pipeline (sc_pipeline.R:668-671) :
    sr$correlated_genes <- .b1_corr_df()
    sr$corr_target_gene <- "SKAP1"
    session$flushReact()
    # Sélecteur encore sur sa valeur initiale (NULL) => écart signalé :
    badge_html <<- paste(as.character(output$corr_seed_badge), collapse = " ")
    session$setInputs(target_gene = "SKAP1")
    badge_synced <<- paste(as.character(output$corr_seed_badge), collapse = " ")
  })

  expect_match(badge_html, "Gène cible calculé : SKAP1", fixed = TRUE)
  expect_match(badge_html, "Modification non recalculée — cliquez sur Rechercher", fixed = TRUE)
  # Sélecteur réaligné sur le seed => plus d'alerte :
  expect_match(badge_synced, "Gène cible calculé : SKAP1", fixed = TRUE)
  expect_false(grepl("Modification non recalculée", badge_synced, fixed = TRUE))
})

test_that("1D : le badge porte le seed de l'auto-pipeline même si le sélecteur a bougé", {
  gd <- .b1_global()
  sr <- create_sc_shared_state()
  badge_html <- NULL

  testServer(mod_sc_corr_server,
             args = list(id = "corr", global_data = gd, shared_rv = sr), {
    sr$correlated_genes <- .b1_corr_df()
    sr$corr_target_gene <- "SKAP1"
    session$flushReact()
    # L'utilisateur tape un AUTRE gène sans relancer « Rechercher » :
    session$setInputs(target_gene = "CXCL10")
    badge_html <<- paste(as.character(output$corr_seed_badge), collapse = " ")
  })

  # Le graphe/exports restent enracinés sur SKAP1 — le badge le dit :
  expect_match(badge_html, "Gène cible calculé : SKAP1", fixed = TRUE)
  expect_match(badge_html, "Modification non recalculée", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# 1D — verrou source : plus AUCUN from=/source= input$target_gene (3 sites)
# ---------------------------------------------------------------------------
test_that("1D verrou source : graphe et exports enracinés sur .corr_seed_gene()", {
  src <- paste(readLines(file.path(ts_project_root(), "modules/sc/mod_sc_corr.R"),
                         warn = FALSE), collapse = "\n")
  expect_false(grepl("from[[:space:]]*=[[:space:]]*input[$]target_gene", src),
               label = "site graphe/export PNG : from=input$target_gene subsiste")
  expect_false(grepl("source[[:space:]]*=[[:space:]]*input[$]target_gene", src),
               label = "site export edge list : source=input$target_gene subsiste")
  pos <- gregexpr(".corr_seed_gene(", src, fixed = TRUE)[[1]]
  n_uses <- if (pos[1] == -1L) 0L else length(pos)
  # 3 sites (graphe, export PNG, export edges) + badge :
  expect_identical(n_uses, 4L)
})

# ---------------------------------------------------------------------------
# 3C — « Ajouter sélection à Viz » : plus de req muet (spéc §2.13)
# ---------------------------------------------------------------------------
test_that("3C : clic sans sélection => notification warning VISIBLE (plus de silence)", {
  gd <- .b1_global()
  sr <- create_sc_shared_state()
  notifs <- NULL
  selected_after <- NULL
  tab_after <- NULL

  testServer(mod_sc_corr_server,
             args = list(id = "corr", global_data = gd, shared_rv = sr), {
    sr$correlated_genes <- .b1_corr_df()
    sr$corr_target_gene <- "SKAP1"
    session$flushReact()
    # AUCUNE ligne sélectionnée — à HEAD, req() avortait en silence :
    notifs <<- .capture_notifications({ session$setInputs(quick_add_corr = 1L) })
    # lecture DANS la session (un reactiveValues n'est pas lisible hors bloc) :
    selected_after <<- sr$selected_genes
    tab_after      <<- sr$active_tab
  })

  expect_length(notifs, 1L)
  expect_identical(notifs[[1]]$type, "warning")
  expect_identical(notifs[[1]]$ui,
                   "Sélectionnez au moins une ligne dans la table des corrélations")
  # Rien n'a été ajouté à Viz :
  expect_length(selected_after, 0L)
  expect_null(tab_after)
})

test_that("3C : sélection valide => ajout, bascule tab_viz et message de succès", {
  gd <- .b1_global()
  sr <- create_sc_shared_state()
  notifs <- NULL
  selected_after <- NULL
  tab_after <- NULL

  testServer(mod_sc_corr_server,
             args = list(id = "corr", global_data = gd, shared_rv = sr), {
    sr$correlated_genes <- .b1_corr_df()
    sr$corr_target_gene <- "SKAP1"
    session$flushReact()
    session$setInputs(correlation_table_rows_selected = c(1L, 3L))
    notifs <<- .capture_notifications({ session$setInputs(quick_add_corr = 1L) })
    selected_after <<- sr$selected_genes
    tab_after      <<- sr$active_tab
  })

  expect_setequal(selected_after, c("G1", "G3"))
  expect_identical(tab_after, "tab_viz")
  expect_length(notifs, 1L)
  expect_identical(notifs[[1]]$type, "message")
  expect_identical(notifs[[1]]$ui,
                   "✓ 2 gène(s) ajouté(s) au module Visualisation")
})

test_that("3C : sans analyse en cours (corr_rv NULL) => garde conservée", {
  gd <- .b1_global()
  sr <- create_sc_shared_state()
  notifs <- NULL
  selected_after <- NULL

  testServer(mod_sc_corr_server,
             args = list(id = "corr", global_data = gd, shared_rv = sr), {
    notifs <<- .capture_notifications({ session$setInputs(quick_add_corr = 1L) })
    selected_after <<- sr$selected_genes
  })

  # corr_rv() est NULL : le req conservé avorte (garde d'entrée inchangée) —
  # aucune notification de succès, rien d'ajouté.
  expect_length(notifs, 0L)
  expect_length(selected_after, 0L)
})

# ---------------------------------------------------------------------------
# 2B (O28, batch B3) — graphe + table CÔTE À CÔTE (spéc §2.7)
# ---------------------------------------------------------------------------
# Constat 2B : `col_widths = c(12)` empilait verticalement le graphe et la
# table (deux ascenseurs étriqués à max_height = 400px ; preuve
# mod_sc_corr.R:54-62 à HEAD 88f9a43). Correctif : layout_columns
# col_widths = c(7, 5), min_height 520px, plot 480px.

test_that("2B verrou source : col_widths c(7, 5), plus aucun empilement c(12)", {
  code <- readLines(file.path(ts_project_root(), "modules/sc/mod_sc_corr.R"),
                    warn = FALSE, encoding = "UTF-8")
  # Code seul (commentaires exclus) : le constat 2B est DOCUMENTÉ au-dessus du
  # layout — le verrou ne doit pas se mordre la queue.
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
  src <- paste(code, collapse = "\n")
  expect_match(src, "col_widths=c(7, 5)", fixed = TRUE,
               info = "la disposition côte à côte c(7,5) est absente (spéc §2.7)")
  expect_false(grepl("col_widths[[:space:]]*=[[:space:]]*c[(]12[)]", src),
               label = "l'empilement col_widths=c(12) subsiste (constat 2B)")
})

test_that("2B rendu réel : la carte de sortie expose graphe ET table, sans erreur", {
  suppressPackageStartupMessages(library(bslib))
  # Shim i18n (l'UI appelle i18n$t à la construction — résolu depuis globalenv)
  old_i18n <- get0("i18n", envir = globalenv())
  assign("i18n", list(t = function(x) x), envir = globalenv())
  on.exit({
    if (is.null(old_i18n)) rm(list = "i18n", envir = globalenv())
    else assign("i18n", old_i18n, envir = globalenv())
  }, add = TRUE)

  ui <- mod_sc_corr_output_ui("corr")
  expect_s3_class(ui, "shiny.tag")
  html <- paste(as.character(ui), collapse = " ")
  # Les deux consommateurs sont toujours présents (aucune perte de contenu) :
  expect_match(html, "correlation_network", fixed = TRUE)
  expect_match(html, "correlation_table", fixed = TRUE)
})
