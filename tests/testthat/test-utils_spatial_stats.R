# =============================================================================
# test-utils_spatial_stats.R — tests for R/utils_spatial_stats.R
# =============================================================================
# compute_composition_differential() is pure base-R (stats::chisq.test only)
# and is fully exercised here with exact-value assertions.
#
# spatial_neighborhood_enrichment(), compute_getis_ord_hotspots() and
# ripley_k_random_labeling() all hard-require the 'RANN' package (no
# fallback — see each function's own requireNamespace() guard) and the
# permutation-based ones (enrichment, Ripley's K) have no internal
# set.seed(), so exact numeric values are not meaningfully reproducible
# across R versions/BLAS. They're skipped gracefully when RANN is absent
# and, when present, asserted on STRUCTURE + a directionally-obvious signal
# (seeded via set.seed() at the call site) rather than exact floating point
# values — robust across R/BLAS versions while still catching a broken
# formula or a shape/sign regression.
# =============================================================================

source_project_file("R/spatial/spatial_stats.R")

# ---------------------------------------------------------------------------
# compute_composition_differential() — pure base R, no skip needed
# ---------------------------------------------------------------------------
test_that("compute_composition_differential returns the expected list shape", {
  set.seed(1)
  emb <- data.frame(
    dataset = rep(c("A", "B"), each = 40),
    cluster = c(sample(c("c1","c2"), 40, replace = TRUE, prob = c(0.8, 0.2)),
               sample(c("c1","c2"), 40, replace = TRUE, prob = c(0.2, 0.8)))
  )
  res <- compute_composition_differential(emb)
  expect_true(all(c("contingency","chisq","residuals","proportions") %in% names(res)))
  expect_true(all(c("statistic","p_value","method") %in% names(res$chisq)))
  expect_identical(colnames(res$residuals), c("dataset","cluster","std_resid"))
  expect_identical(colnames(res$proportions), c("dataset","cluster","proportion","n"))
})

test_that("compute_composition_differential detects an obviously different composition", {
  # Dataset A is 100% c1, dataset B is 100% c2 -> maximally different,
  # chi-squared statistic should be large and p-value tiny.
  emb <- data.frame(
    dataset = rep(c("A", "B"), each = 50),
    cluster = c(rep("c1", 50), rep("c2", 50))
  )
  res <- compute_composition_differential(emb)
  expect_gt(res$chisq$statistic, 50)
  expect_lt(res$chisq$p_value, 0.001)
})

test_that("compute_composition_differential proportions sum to 1 within each dataset", {
  emb <- data.frame(
    dataset = rep(c("A","B","C"), each = 30),
    cluster = sample(c("x","y","z"), 90, replace = TRUE)
  )
  res <- compute_composition_differential(emb)
  totals <- stats::aggregate(proportion ~ dataset, data = res$proportions, FUN = sum)
  expect_equal(totals$proportion, rep(1, nrow(totals)), tolerance = 1e-9)
})

test_that("compute_composition_differential errors with fewer than 2 datasets", {
  emb <- data.frame(dataset = rep("A", 10), cluster = sample(c("x","y"), 10, replace = TRUE))
  expect_error(compute_composition_differential(emb), "Au moins 2 echantillons")
})

test_that("compute_composition_differential errors when required columns are missing", {
  emb <- data.frame(foo = 1:5, bar = 1:5)
  expect_error(compute_composition_differential(emb), "dataset")
})

test_that("compute_composition_differential falls back to simulated p-value for sparse tables", {
  # Tiny counts -> expected cell counts < 5 -> must use simulate.p.value=TRUE
  emb <- data.frame(dataset = c("A","A","B","B"), cluster = c("x","y","x","y"))
  res <- compute_composition_differential(emb)
  expect_match(res$chisq$method, "simule")
})

# ---------------------------------------------------------------------------
# compute_getis_ord_hotspots() — needs RANN
# ---------------------------------------------------------------------------
test_that("compute_getis_ord_hotspots flags a synthetic hot region as significant", {
  skip_if_not_installed("RANN")
  set.seed(42)
  n <- 200
  coords <- data.frame(id = paste0("s", seq_len(n)),
                       x = runif(n, 0, 100), y = runif(n, 0, 100))
  # "Hot" cluster: a tight group of high values in one corner; background low.
  is_hot <- coords$x < 15 & coords$y < 15
  values <- ifelse(is_hot, rnorm(n, mean = 50, sd = 2), rnorm(n, mean = 0, sd = 2))
  names(values) <- coords$id

  res <- compute_getis_ord_hotspots(coords, values, k_neighbors = 10)
  expect_identical(colnames(res), c("id","value","gi_star","p_value","hotspot"))
  expect_equal(nrow(res), n)
  # Points inside the hot region should have systematically higher Gi* than
  # points outside it — checks the sign/direction of the statistic, not an
  # exact numeric value.
  mean_gi_hot  <- mean(res$gi_star[is_hot])
  mean_gi_cold <- mean(res$gi_star[!is_hot])
  expect_gt(mean_gi_hot, mean_gi_cold)
  expect_true(any(res$hotspot[is_hot] == "Hotspot (chaud)"))
})

test_that("compute_getis_ord_hotspots errors with fewer than 10 valid points", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:5), x = 1:5, y = 1:5)
  values <- stats::setNames(1:5, coords$id)
  expect_error(compute_getis_ord_hotspots(coords, values), "10 elements")
})

test_that("compute_getis_ord_hotspots errors on a zero-variance metric", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:20), x = runif(20), y = runif(20))
  values <- stats::setNames(rep(5, 20), coords$id)   # constant -> zero variance
  expect_error(compute_getis_ord_hotspots(coords, values), "Variance nulle")
})

# ---------------------------------------------------------------------------
# spatial_neighborhood_enrichment() — needs RANN, permutation-based
# ---------------------------------------------------------------------------
test_that("spatial_neighborhood_enrichment detects self-attraction of a spatially segregated group", {
  skip_if_not_installed("RANN")
  set.seed(7)
  n <- 240
  # Two tight, spatially SEPARATE blobs, one per group -> strong self-
  # attraction (A near A, B near B), strong mutual exclusion (A near B rare).
  coords <- data.frame(
    id = paste0("s", seq_len(n)),
    x = c(rnorm(n / 2, mean = 0, sd = 3), rnorm(n / 2, mean = 100, sd = 3)),
    y = c(rnorm(n / 2, mean = 0, sd = 3), rnorm(n / 2, mean = 100, sd = 3))
  )
  labels <- stats::setNames(rep(c("A", "B"), each = n / 2), coords$id)

  res <- spatial_neighborhood_enrichment(coords, labels, k_neighbors = 10, n_perm = 50)
  expect_true(all(c("enrichment","matrix","levels","k_neighbors","n_perm") %in% names(res)))
  z_AA <- res$enrichment$z_score[res$enrichment$from == "A" & res$enrichment$to == "A"]
  z_AB <- res$enrichment$z_score[res$enrichment$from == "A" & res$enrichment$to == "B"]
  expect_gt(z_AA, 0)     # A next to A: enriched vs random labeling
  expect_lt(z_AB, 0)     # A next to B: depleted vs random labeling
})

test_that("spatial_neighborhood_enrichment errors with a single-level grouping", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:20), x = runif(20), y = runif(20))
  labels <- stats::setNames(rep("only_one", 20), coords$id)
  expect_error(spatial_neighborhood_enrichment(coords, labels), "seule categorie")
})

# ---------------------------------------------------------------------------
# ripley_k_random_labeling() — needs RANN, permutation-based
# ---------------------------------------------------------------------------
test_that("ripley_k_random_labeling detects aggregation of a tightly clustered target label", {
  skip_if_not_installed("RANN")
  set.seed(11)
  n_bg <- 150; n_target <- 40
  # Target points tightly packed in a small corner; background spread widely.
  coords <- data.frame(
    id = paste0("s", seq_len(n_bg + n_target)),
    x = c(runif(n_bg, 0, 100), runif(n_target, 0, 8)),
    y = c(runif(n_bg, 0, 100), runif(n_target, 0, 8))
  )
  labels <- stats::setNames(c(rep("bg", n_bg), rep("tight", n_target)), coords$id)

  res <- ripley_k_random_labeling(coords, labels, target_level = "tight", n_perm = 49)
  expect_identical(res$target_level, "tight")
  expect_equal(res$n_target, n_target)
  expect_false(res$subsampled)
  # At the smallest radius, a tightly packed target should show clear
  # spatial aggregation relative to the random-labeling null envelope.
  expect_gt(res$curve$k_observed[1], res$curve$k_perm_hi[1])
})

test_that("ripley_k_random_labeling errors when the target level doesn't exist", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:25), x = runif(25), y = runif(25))
  labels <- stats::setNames(rep("A", 25), coords$id)
  expect_error(ripley_k_random_labeling(coords, labels, target_level = "not_there"), "introuvable")
})

test_that("ripley_k_random_labeling errors with fewer than 10 target points", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:25), x = runif(25), y = runif(25))
  labels <- stats::setNames(c(rep("A", 5), rep("B", 20)), coords$id)
  expect_error(ripley_k_random_labeling(coords, labels, target_level = "A"), "non fiable")
})
