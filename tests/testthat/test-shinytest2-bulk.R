# Golden-path smoke test — Bulk RNA-seq domain (backlog #7).
# Pins the namespaced input-id contract (mod_bulk.R -> mod_bulk_mapping_ui(
# ns("mapping")), mod_bulk_filter_ui(ns("filter")), ...) so a UI/server
# namespace drift shows up as a missing id instead of silently rotting.
# Ids vérifiés contre les sources : mod_bulk_mapping.R:65, mod_bulk_filter.R:44/51,
# mod_bulk_de_ui.R:25/52, mod_bulk_pathways.R:56, mod_bulk_report.r:50.

test_that("Bulk RNA domain: app boots, tab navigates, no Shiny error", {
  app <- new_app_driver("bulk_smoke")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Bulk RNA Analysis")
  assert_no_shiny_error(app)
})

test_that("Bulk RNA domain: expected namespaced inputs exist", {
  app <- new_app_driver("bulk_ids")
  on.exit(app$stop(), add = TRUE)
  click_nav_by_text(app, "Bulk RNA Analysis")

  inputs <- names(app$get_values()$input)
  expected_ids <- c(
    "bulk-mapping-run_mapping", "bulk-filter-min_count", "bulk-filter-run_filter_norm",
    "bulk-de-condition_col", "bulk-de-run_de",
    "bulk-pathways-pathway_db", "bulk-report-report_title"
  )
  missing <- setdiff(expected_ids, inputs)
  expect_true(length(missing) == 0,
              info = paste("Missing Bulk ids (namespace drift?):", paste(missing, collapse = ", ")))
})
