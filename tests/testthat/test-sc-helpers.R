# =============================================================================
# test-helpers_sc.R — pure-function tests for helpers_sc.R (QA-2, Cerberus 1.0)
# =============================================================================
# Scope: resolve_sketch_preset() (pure, zero extra deps), calculate_pseudotime()
# (needs RANN + igraph, skip-guarded), subsample_seurat_for_analysis() (needs
# Seurat/SeuratObject, skip-guarded, tiny synthetic object -- no real counts
# data or Bioconductor annotation package required).
#
# OUT OF SCOPE: robust_find_clusters()'s fallback path only fires once
# Seurat::FindClusters() fails on a REAL neighbor graph (FindNeighbors()
# output) -- not worth a fragile full-pipeline fixture for a unit test. Plot
# builders (plot_trajectory(), plot_genes_vs_pseudotime(), ...), Slingshot,
# and the RNA Velocity validators are likewise out of scope here -- see RF-1
# in the Cerberus 1.0 sprint plan (extract Velocity to its own
# helpers_sc_velocity.R + a dedicated test file).
# =============================================================================

source_project_file("R/core/io_helpers.R")   # defines %||%, sourced first in app.R
source_project_file("R/plotting/palettes.R")   # required by sc_trajectory (scale helpers)
source_project_file("R/sc/sc_helpers.R")
source_project_file("R/sc/sc_trajectory.R")   # calculate_pseudotime moved here (Block 7 refactor)

# ---------------------------------------------------------------------------
# resolve_sketch_preset()
# ---------------------------------------------------------------------------
test_that("resolve_sketch_preset caps ncells at n_total_cells for every named preset", {
  for (p in c("fast", "light", "medium", "standard", "high")) {
    out <- resolve_sketch_preset(p, n_total_cells = 1000)
    expect_lte(out$ncells, 1000)
    expect_true(out$npcs > 0)
    expect_true(out$max_per_cluster > 0)
  }
})

test_that("resolve_sketch_preset 'max' uses the full dataset size", {
  out <- resolve_sketch_preset("max", n_total_cells = 12345)
  expect_equal(out$ncells, 12345)
})

test_that("resolve_sketch_preset 'custom' uses custom_ncells, capped at n_total_cells", {
  out <- resolve_sketch_preset("custom", n_total_cells = 5000, custom_ncells = 3000)
  expect_equal(out$ncells, 3000)
  out2 <- resolve_sketch_preset("custom", n_total_cells = 2000, custom_ncells = 3000)
  expect_equal(out2$ncells, 2000)   # capped even for a custom request above dataset size
})

test_that("resolve_sketch_preset 'custom' defaults to 20000 when custom_ncells is NULL", {
  out <- resolve_sketch_preset("custom", n_total_cells = 100000, custom_ncells = NULL)
  expect_equal(out$ncells, 20000)
})

test_that("resolve_sketch_preset falls back to 'standard' for an unknown preset name", {
  expect_equal(resolve_sketch_preset("not_a_real_preset", n_total_cells = 60000),
               resolve_sketch_preset("standard", n_total_cells = 60000))
})

# ---------------------------------------------------------------------------
# calculate_pseudotime() -- needs RANN + igraph
# ---------------------------------------------------------------------------
test_that("calculate_pseudotime orders cells monotonically along a 1D line", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  set.seed(1)
  emb <- matrix(seq(0, 10, length.out = 50), ncol = 1)
  rownames(emb) <- paste0("cell", seq_len(50))
  res <- calculate_pseudotime(emb, k = 5, root_cells = 1, root_method = "manual")
  expect_length(res$pseudotime, 50)
  expect_equal(unname(res$pseudotime[1]), 0)   # root cell -> pseudotime 0 after min-max scaling
  expect_gt(stats::cor(seq_len(50), unname(res$pseudotime), method = "spearman"), 0.95)
})

test_that("calculate_pseudotime errors on too few cells", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(1:4, ncol = 2)
  expect_error(calculate_pseudotime(emb, k = 2), "at least 3 cells")
})

test_that("calculate_pseudotime errors on non-finite embedding values", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
  expect_error(calculate_pseudotime(emb, k = 2), "NA, NaN, or infinite")
})

test_that("calculate_pseudotime errors on an out-of-range manual root cell", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(seq(0, 10, length.out = 30), ncol = 1)
  expect_error(calculate_pseudotime(emb, k = 5, root_cells = 999), "Invalid root cell index")
})

# ---------------------------------------------------------------------------
# subsample_seurat_for_analysis() -- needs Seurat/SeuratObject
# ---------------------------------------------------------------------------
.load_seurat_pkgs <- function() {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  suppressPackageStartupMessages({
    library(SeuratObject)
    library(Seurat)
  })
}

.toy_seurat <- function(n_cells = 60, n_groups = 3) {
  counts <- matrix(stats::rpois(n_cells * 20, lambda = 2), nrow = 20,
                   dimnames = list(paste0("gene", 1:20), paste0("cell", 1:n_cells)))
  obj <- SeuratObject::CreateSeuratObject(counts = counts)
  obj$seurat_clusters <- factor(rep(seq_len(n_groups), length.out = n_cells))
  obj
}

test_that("subsample_seurat_for_analysis caps cells per group and reports the split", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 90, n_groups = 3)   # 30 cells/cluster
  res <- subsample_seurat_for_analysis(obj, max_per_group = 10, group_col = "seurat_clusters")
  expect_true(res$was_subsampled)
  expect_equal(res$n_before, 90)
  expect_equal(ncol(res$object), 30)   # 3 clusters x 10 cells/cluster cap
})

test_that("subsample_seurat_for_analysis is a no-op when the cap disables subsampling", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 40, n_groups = 2)
  for (cap in list(Inf, NA, 0, -5)) {
    res <- subsample_seurat_for_analysis(obj, max_per_group = cap, group_col = "seurat_clusters")
    expect_false(res$was_subsampled)
    expect_equal(ncol(res$object), 40)
  }
})

test_that("subsample_seurat_for_analysis falls back to a flat subsample when group_col is absent", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 50, n_groups = 2)
  res <- subsample_seurat_for_analysis(obj, max_per_group = 15, group_col = "not_a_real_column")
  expect_true(res$was_subsampled)
  expect_equal(ncol(res$object), 15)
})
