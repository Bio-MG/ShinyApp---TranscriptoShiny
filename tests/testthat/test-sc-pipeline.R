# Tests for R/sc/sc_pipeline.R
source_project_file("R/sc/sc_helpers.R")
source_project_file("R/sc/sc_pipeline.R")

test_that("resolve_sketch_preset returns valid structure", {
  for (preset in c("fast","light","medium","standard","high","max")) {
    result <- resolve_sketch_preset(preset, n_total_cells = 200000)
    expect_true(all(c("ncells","max_per_cluster","npcs") %in% names(result)))
    expect_true(result$ncells > 0)
    expect_true(result$npcs > 0)
  }
})

test_that("resolve_sketch_preset caps at n_total_cells", {
  result <- resolve_sketch_preset("high", n_total_cells = 5000)
  expect_equal(result$ncells, 5000)  # capped below 100000
})
