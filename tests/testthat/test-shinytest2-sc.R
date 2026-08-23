# Golden-path smoke test — Single-Cell domain (backlog #7).
# UPDATE Vague suivante : mod_sc_corr.R / mod_sc_trajectory.R ONT été retrouvés
# cette session (modules/sc/) — leurs ids réels sont désormais pinnés ci-dessous
# (montages ns("corr") / ns("trajectory") dans mod_sc.R:46/50 ; widgets :
# mod_sc_corr.R:21/26/30, mod_sc_trajectory.R:115/126/151).
# Les inputs renderUI-only (slingshot_controls : traj_cluster_col, ...) sont
# volontairement exclus — ils n'existent au DOM qu'après méthode=="slingshot".

test_that("Single-Cell domain: app boots, tab navigates, no Shiny error", {
  app <- new_app_driver("sc_smoke")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Single-Cell Analysis")
  assert_no_shiny_error(app)
})

test_that("Single-Cell domain: expected namespaced inputs exist", {
  app <- new_app_driver("sc_ids")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Single-Cell Analysis")

  inputs <- names(app$get_values()$input)
  expected_ids <- c(
    "sc-mapping-run_mapping",   # regression guard: real NESTED mapping module
    "sc-pipeline-run_pipeline", "sc-annotation-run_annot",
    "sc-viz-viz_type", "sc-markers-run_markers", "sc-pathways-run_pathway",
    # Corrélation (mod_sc_corr.R -> ns("corr"))
    "sc-corr-target_gene", "sc-corr-cor_method", "sc-corr-find_correlated",
    # Trajectory (mod_sc_trajectory.R -> ns("trajectory"))
    "sc-trajectory-traj_method", "sc-trajectory-traj_reduction",
    "sc-trajectory-calc_trajectory"
  )
  missing <- setdiff(expected_ids, inputs)
  expect_true(length(missing) == 0,
              info = paste("Missing SC ids (namespace drift?):", paste(missing, collapse = ", ")))

  # Guard against the Phase 0 bug CLASS: a bare (un-domained) "mapping-*"
  # module should never resurface.
  expect_length(grep("^mapping-", inputs, value = TRUE), 0)
})
