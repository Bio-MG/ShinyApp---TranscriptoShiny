# Golden-path smoke test — Spatial domain (backlog #7).
# Ids vérifiés contre les sources : mod_spatial_pipeline.R:162, mod_spatial_qc.R:67/71,
# mod_spatial_cluster.R:73 (input_task_button), mod_spatial_deconv_ui.R:15,
# mod_spatial_viz.R:22, mod_spatial_multi.R:72, mod_spatial_niche.R:71.
# NB : cliquer cet onglet déclenche l'init lazy des 6 daemons mirai (premier
# run lent — attendu, cf. app.R et mod_spatial.R).

test_that("Spatial domain: app boots, tab navigates, no Shiny error", {
  app <- new_app_driver("spatial_smoke")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Spatial Analysis")
  assert_no_shiny_error(app)
})

test_that("Spatial domain: expected namespaced inputs exist", {
  app <- new_app_driver("spatial_ids")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Spatial Analysis")

  inputs <- names(app$get_values()$input)
  expected_ids <- c(
    "spatial-pipeline-btn_run_all", "spatial-qc-btn_apply_qc", "spatial-qc-min_features",
    "spatial-cluster-btn_cluster", "spatial-deconv-mode", "spatial-viz-color_by",
    "spatial-multi-npcs", "spatial-niche-k_neighbors"
  )
  missing <- setdiff(expected_ids, inputs)
  expect_true(length(missing) == 0,
              info = paste("Missing Spatial ids (namespace drift?):", paste(missing, collapse = ", ")))
})
