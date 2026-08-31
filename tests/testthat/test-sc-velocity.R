# Tests for R/sc/sc_velocity.R
source_project_file("R/sc/sc_velocity.R")

test_that("detect_velocity_orientation detects genes_x_cells", {
  skip_if_not_installed("Matrix")
  mat <- Matrix::Matrix(0, nrow = 10, ncol = 5, sparse = TRUE)
  rownames(mat) <- paste0("gene", 1:10)
  colnames(mat) <- paste0("cell", 1:5)
  result <- detect_velocity_orientation(mat,
    seurat_cells = paste0("cell", 1:5),
    seurat_genes = paste0("gene", 1:10),
    orientation = "auto_strict")
  expect_equal(result, "genes_x_cells")
})

test_that("normalize_velocity_cell_barcodes strips suffix", {
  ids <- c("AAAC-1", "AAAG-1", "AAAT-1")
  result <- normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE)
  expect_equal(result, c("AAAC", "AAAG", "AAAT"))
})

test_that("normalize_velocity_cell_barcodes detects collisions", {
  ids <- c("AAAC-1", "AAAC-2")
  expect_error(normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE),
               "collision")
})
