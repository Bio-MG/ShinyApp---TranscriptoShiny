# =============================================================================
# test-mod-sc-trajectory.R — modules/sc/mod_sc_trajectory.R
# =============================================================================
# Item 1C du packet round-7 (O28 batch B1) : le sélecteur de réduction de
# l'onglet Trajectoire restait sur son défaut `selected = "pca"` alors que le
# pipeline auto écrit `shared_rv$traj_reduction` ("umap"/"pca") après le
# pseudotemps (tools/pkg_skeleton/R/sc_pipeline.R:674, modules/sc/mod_sc.R:1294)
# — un clic sur « Actualiser Plot » recalculait alors le graphe kNN sur une
# réduction DIFFÉRENTE de celle du pipeline (aucun observer de resynchronisation
# à HEAD ; seul le reset langue touchait le sélecteur).
#
# Correctif testé ici (spéc §2.3 de la proposition) :
#   1. l'observer `updateSelectInput` sur `shared_rv$traj_reduction` ;
#   2. le badge « Réduction active synchronisée : %s » (variante d'alerte
#      quand le sélecteur est déplacé sans recalcul).
#
# ⚠️ MESURÉ (sonde dédiée) : sous testServer (shiny 1.14), un
# `updateSelectInput(selected=...)` ne modifie PAS input$<id> — le mock ne
# rejoue pas les messages update côté client. L'effet observable de
# l'observer est donc la REQUÊTE de mise à jour elle-même : on capture les
# appels `updateSelectInput()` via le shim globalenv+on.exit (pattern de
# test-mod-sc-drive.R:418), et `session$setInputs()` simule le navigateur
# qui applique la mise à jour pour tester les états du badge.
# i18n est NULL ⇒ le proxy .tr() du module renvoie la clé FR telle quelle.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/io_helpers.R")      # %||%
source_project_file("tools/pkg_skeleton/R/error_log.R")
source_project_file("tools/pkg_skeleton/R/state.R")           # state_set/state_get
                                                              # (observer traj_genes, ignoreNULL=FALSE)
source_project_file("tools/pkg_skeleton/R/drive_allowlist.R") # ts_drive_publish_export (init serveur)
source_project_file("tools/pkg_skeleton/R/drive_watcher.R")   # ts_drive_registry
source_project_file("modules/sc/mod_sc_trajectory.R")

suppressPackageStartupMessages(library(shiny))

# shared_rv minimal au schema production (create_sc_shared_state, champs lus ici)
.b1_shared <- function(traj_reduction = NULL) {
  shiny::reactiveValues(
    traj_reduction = traj_reduction,
    traj_method    = NULL,
    traj_genes     = character(0),
    active_tab     = NULL
  )
}

.b1_global <- function() {
  shiny::reactiveValues(sc_obj = NULL, i18n = NULL, language = "fr")
}

# Capture des updateSelectInput() émis par le module : la fonction est résolue
# depuis globalenv (fichier modules/ sys.source()é par source_project_file) —
# shim posé PUIS RESTAURÉ. Chaque appel est transmis au vrai shiny:: pour ne
# pas dégrader le comportement du mock.
.capture_select_updates <- function(code) {
  old <- get0("updateSelectInput", envir = globalenv())
  box <- new.env(parent = emptyenv())
  box$items <- list()
  assign("updateSelectInput", function(session, inputId, label = NULL,
                                       choices = NULL, selected = NULL, ...) {
    box$items[[length(box$items) + 1L]] <- list(inputId = inputId, selected = selected)
    shiny::updateSelectInput(session = session, inputId = inputId, label = label,
                             choices = choices, selected = selected, ...)
  }, envir = globalenv())
  on.exit({
    if (is.null(old)) rm(list = "updateSelectInput", envir = globalenv())
    else assign("updateSelectInput", old, envir = globalenv())
  }, add = TRUE)
  force(code)
  box$items
}

# ---------------------------------------------------------------------------
# 1C — resynchronisation du sélecteur sur la réduction écrite par le pipeline
# ---------------------------------------------------------------------------
test_that("1C : le sélecteur suit shared_rv$traj_reduction quand le pipeline écrit 'umap'", {
  gd <- .b1_global()
  sr <- .b1_shared()
  updates <- NULL

  testServer(mod_sc_trajectory_server,
             args = list(id = "trajectory", global_data = gd, shared_rv = sr), {
    # Ce que fait le pipeline auto après le pseudotemps (sc_pipeline.R:674) :
    sr$traj_reduction <- "umap"
    session$flushReact()
    updates <<- .capture_select_updates({
      sr$traj_reduction <- "pca"      # second écrit, pour lever tout doute d'init
      session$flushReact()
    })
    # …et le navigateur applique la mise à jour => badge synchronisé :
    session$setInputs(traj_reduction = "pca")
    badge_html <<- paste(as.character(output$traj_reduction_badge), collapse = " ")
  })

  # LE fix : l'observer a demandé la mise à jour du sélecteur sur la réduction
  # active ("pca") — à HEAD, aucune requête n'était émise (défaut "pca" figé).
  expect_length(updates, 1L)
  expect_identical(updates[[1]]$inputId, "traj_reduction")
  expect_identical(updates[[1]]$selected, "pca")
  expect_match(badge_html, "Réduction active synchronisée : PCA", fixed = TRUE)
})

test_that("1C : le cas 'umap' du pipeline auto synchronise aussi (badge vert)", {
  gd <- .b1_global()
  sr <- .b1_shared()
  updates <- NULL
  badge_html <- NULL

  testServer(mod_sc_trajectory_server,
             args = list(id = "trajectory", global_data = gd, shared_rv = sr), {
    updates <<- .capture_select_updates({
      sr$traj_reduction <- "umap"
      session$flushReact()
    })
    session$setInputs(traj_reduction = "umap")   # le navigateur applique
    badge_html <<- paste(as.character(output$traj_reduction_badge), collapse = " ")
  })

  expect_length(updates, 1L)
  expect_identical(updates[[1]]$inputId, "traj_reduction")
  expect_identical(updates[[1]]$selected, "umap")
  expect_match(badge_html, "Réduction active synchronisée : UMAP", fixed = TRUE)
})

test_that("1C : un sélecteur déplacé sans recalcul est signalé par le badge (écart)", {
  gd <- .b1_global()
  sr <- .b1_shared(traj_reduction = "umap")   # déjà posé par le pipeline auto
  badge_html <- NULL

  testServer(mod_sc_trajectory_server,
             args = list(id = "trajectory", global_data = gd, shared_rv = sr), {
    session$flushReact()                      # observer init : sync demandée sur "umap"
    # L'utilisateur déplace le sélecteur SANS relancer le calcul :
    session$setInputs(traj_reduction = "pca")
    badge_html <<- paste(as.character(output$traj_reduction_badge), collapse = " ")
  })

  expect_match(badge_html, "Réduction active : UMAP", fixed = TRUE)
  expect_match(badge_html, "PCA (non recalculé)", fixed = TRUE)
  # …et le badge synchronisé ne doit PAS être affiché dans cet état.
  expect_false(grepl("Réduction active synchronisée", badge_html, fixed = TRUE))
})

test_that("1C : aucune mise à jour ni badge tant que le pipeline n'a rien écrit", {
  gd <- .b1_global()
  sr <- .b1_shared(traj_reduction = NULL)
  updates <- NULL
  badge_html <- "sentinel"

  testServer(mod_sc_trajectory_server,
             args = list(id = "trajectory", global_data = gd, shared_rv = sr), {
    updates <<- .capture_select_updates({ session$flushReact() })
    # Pas de réduction active => pas de badge (pas d'information fabriquée) :
    badge_html <<- output$traj_reduction_badge
  })

  expect_length(updates, 0L)
  expect_true(is.null(badge_html) || length(badge_html) == 0L)
})
