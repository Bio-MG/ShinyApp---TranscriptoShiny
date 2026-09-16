# Golden-path smoke test — Import domain incl. the GEO tab (mod_geo.R).
# Boots the real app headless (shiny.testmode = TRUE), opens Import >
# "Source publique (GEO)" and asserts no Shiny error; then pins the namespaced
# input-id contract (mod_geo_ui) so a UI drift shows up as a missing id.

test_that("Import domain: GEO tab navigates without Shiny error", {
  app <- new_app_driver("import_geo_smoke")
  on.exit(app$stop(), add = TRUE)

  click_nav_by_text(app, "Import Données")
  assert_no_shiny_error(app)

  # GEO is a nested nav_menu item — click any anchor bearing its label
  app$get_js(sprintf("(function() {
    const links = Array.from(document.querySelectorAll('a'));
    const el = links.find(a => a.textContent.includes('%s'));
    if (el) { el.click(); return true; }
    return false;
  })()", "Source publique (GEO)"))
  app$wait_for_idle()
  assert_no_shiny_error(app)
})

test_that("Import domain: GEO module exposes its namespaced inputs", {
  app <- new_app_driver("import_geo_ids")
  on.exit(app$stop(), add = TRUE)

  click_nav_by_text(app, "Import Données")

  expected_ids <- c(
    "geo-mode", "geo-accession", "geo-btn_fetch", "geo-goto_mapping"
  )
  # NB: geo-file_counts / geo-file_meta (fileInput) are NULL until an upload,
  # so they are deliberately not pinned here.
  inputs <- names(app$get_values()$input)
  missing <- setdiff(expected_ids, inputs)
  expect_true(length(missing) == 0,
              info = paste("Missing GEO ids (namespace drift?):", paste(missing, collapse = ", ")))
})
