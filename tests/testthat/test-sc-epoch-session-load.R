# =============================================================================
# test-sc-epoch-session-load.R — F08 (revue externe 2026-10-28)
# =============================================================================
# Le contrat d'invalidation (`tools/pkg_skeleton/R/state.R:80-110`) dit :
# l'epoch est incremente par les REMPLACEMENTS DE JEU, et `mod_sc.R` purge les
# 13 champs resultats sur `observeEvent(global_data$sc_obj_epoch)`.
#
# Mesure : l'epoch n'etait bumpe QUE par les imports (mod_import_sc x5,
# mod_geo, mod_import_bulk x3, mod_bulk_merge, mod_bulk_datasets) — JAMAIS par
# `app.R::load_session_file` ni par `confirm_reset`. Consequence : charger une
# session, ou tout effacer, laissait en place les resultats partages du jeu
# precedent (marqueurs, pathways, velo, DA, pseudobulk…) qui se melangeaient
# ensuite au nouveau jeu dans le rapport consolide.
#
# 🔴 Le defaut est CROSS-DOMAINE : `bulk_obj_epoch` a exactement le meme trou.
# D'ou un helper UNIQUE qui bumpe les deux — corriger un seul domaine laisserait
# l'autre en place, et deux copies de la meme incrementation divergeraient.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/state.R")

test_that("F08 : le bump d'epoch couvre les DEUX domaines", {
  gd <- list(sc_obj_epoch = 2L, bulk_obj_epoch = 5L)
  out <- .ts_bump_all_epochs(gd)

  expect_identical(out$sc_obj_epoch, 3L)
  expect_identical(out$bulk_obj_epoch, 6L)
})

test_that("F08 : le bump est NULL-safe (epoch jamais initialise)", {
  out <- .ts_bump_all_epochs(list())
  expect_identical(out$sc_obj_epoch, 1L)
  expect_identical(out$bulk_obj_epoch, 1L)
})

test_that("F08 : le bump est idempotent par appel (une session = un increment)", {
  gd <- list(sc_obj_epoch = 0L, bulk_obj_epoch = 0L)
  gd <- .ts_bump_all_epochs(gd)
  gd <- .ts_bump_all_epochs(gd)
  expect_identical(gd$sc_obj_epoch, 2L)
  expect_identical(gd$bulk_obj_epoch, 2L)
})

test_that("F08 : chargement de session ET reset passent par le helper (verrou source)", {
  p     <- ts_ast_parse("app.R")
  calls <- ts_ast_find_all(p, function(x) TRUE)

  # Anti-vacuite : un garde qui ne visite rien passerait au vert a vide.
  expect_true(length(calls) > 0L)

  d <- vapply(calls, ts_ast_deparse, character(1))

  # Les DEUX chemins destructifs doivent purger. `< 2L` = un seul cable.
  expect_true(sum(grepl(".ts_bump_all_epochs(", d, fixed = TRUE)) >= 2L)

  # Et aucun des deux ne doit ecrire un epoch a la main (deux verites).
  expect_identical(sum(grepl("$sc_obj_epoch <- (", d, fixed = TRUE)), 0L)
})
