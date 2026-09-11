# =============================================================================
# test-bulk-batch-qc.R — Diagnostics batch Bulk (roadmap Bulk V2, M1)
# =============================================================================
# Couvre : garde anti-counts-bruts, contrôle du plan d'expérience (batch ×
# condition, collinéarité via check_design_confounding réutilisé), décomposition
# de variance (voie variancePartition OU repli R pur — les deux déclarées),
# plafond mémoire de gènes déterministe, plots consommateurs purs.
# =============================================================================
source_project_file("R/core/validation.R")
source_project_file("R/bulk/bulk_helpers.R")
source_project_file("R/bulk/bulk_batch_qc.R")

.vst_like <- function(genes = 60, samples = 12, seed = 1) {
  set.seed(seed)
  m <- matrix(rnorm(genes * samples), genes, samples,
              dimnames = list(paste0("g", seq_len(genes)),
                              paste0("s", seq_len(samples))))
  m + 10  # amplitude continue bornée, typique d'une VST
}

.meta_bq <- function(n = 12, collinear = FALSE) {
  b <- if (collinear) rep(c("b1", "b2"), each = n / 2) else rep(c("b1", "b2"), length.out = n)
  data.frame(row.names = paste0("s", seq_len(n)),
             batch = b,
             condition = rep(c("A", "B"), each = n / 2))
}

test_that("surface publique figée", {
  expect_setequal(
    bulk_batch_qc_public_api(),
    c("bulk_batch_qc_public_api", "bulk_assert_transformed_matrix",
      "bulk_batch_design_check", "bulk_variance_partition",
      "plot_bulk_varpart", "plot_bulk_batch_scree")
  )
})

test_that("garde anti-counts-bruts : VST acceptée, counts Poissonniens refusés", {
  expect_no_error(bulk_assert_transformed_matrix(.vst_like()))
  # counts simulés lambda modéré (max ~600 — sous tout seuil d'amplitude naïf)
  set.seed(2)
  counts <- matrix(rpois(50 * 12, lambda = 300), 50, 12)
  err <- tryCatch(bulk_assert_transformed_matrix(counts, "test GSVA"),
                  error = function(e) e)
  expect_s3_class(err, "bulk_batch_qc_error")
  expect_identical(err$state, "raw_counts_rejected")
  expect_match(conditionMessage(err), "VST")
  # erreurs structurelles
  expect_error(bulk_assert_transformed_matrix(NULL),
               class = "bulk_batch_qc_error")
  e1 <- tryCatch(bulk_assert_transformed_matrix(matrix(1.5, 5, 1)), error = function(e) e)
  expect_identical(e1$state, "invalid_input")
})

test_that("contrôle du plan : équilibré silencieux, confondu détecté + message", {
  chk <- bulk_batch_design_check(.meta_bq(12), "batch", "condition")
  expect_false(chk$fully_collinear)
  expect_identical(chk$empty_cells, 0L)
  expect_length(chk$warning_messages, 0L)
  expect_identical(dim(chk$cross_table), c(2L, 2L))

  conf <- bulk_batch_design_check(.meta_bq(12, collinear = TRUE), "batch", "condition")
  expect_true(conf$fully_collinear)
  expect_match(conf$warning_messages[1], "ENTI[ÈE]REMENT collin[ée]aires")

  # batch sans condition : contrôle lot seul
  solo <- bulk_batch_design_check(.meta_bq(12), "batch")
  expect_identical(solo$n_condition_levels, NA_integer_)
  expect_false(solo$fully_collinear)
  # modalité unique de batch
  one <- bulk_batch_design_check(
    data.frame(row.names = paste0("s", 1:4), batch = "b1", condition = c("A", "A", "B", "B")),
    "batch", "condition")
  expect_match(one$warning_messages[1], "une seule modalit[ée]")
  # colonne inconnue -> erreur classée
  expect_error(bulk_batch_design_check(.meta_bq(6), "inexistant"),
               class = "bulk_batch_qc_error")
})

test_that("variance partition : résultat structuré, fractions bornées [0,1]", {
  vp <- bulk_variance_partition(.vst_like(30), .meta_bq(12), c("batch", "condition"))
  expect_true(vp$method %in% c("variancePartition::fitExtractVarPartModel", "pur_lm_partial_r2"))
  expect_identical(nrow(vp$var_part), 30L)
  expect_true(all(c("batch", "condition") %in% names(vp$var_part)))
  vals <- as.vector(as.matrix(vp$var_part))
  vals <- vals[is.finite(vals)]
  expect_true(all(vals >= 0 & vals <= 1))
  expect_identical(vp$n_genes_total, 30L)
  expect_identical(vp$n_genes_used, 30L)
  expect_match(vp$formula, "^~ batch \\+ condition$")
  expect_true(nzchar(vp$timestamp_utc))
})

test_that("variance partition : plafond mémoire déterministe (seed)", {
  big <- .vst_like(genes = 300, samples = 12, seed = 5)
  vp1 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 7)
  vp2 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 7)
  expect_identical(vp1$n_genes_total, 300L)
  expect_identical(vp1$n_genes_used, 40L)
  expect_identical(rownames(vp1$var_part), rownames(vp2$var_part))  # même seed, mêmes gènes
  vp3 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 8)
  expect_false(identical(rownames(vp1$var_part), rownames(vp3$var_part)))
})

test_that("variance partition : erreurs classées", {
  expect_error(bulk_variance_partition(.vst_like(), .meta_bq(12), "inexistant"),
               class = "bulk_batch_qc_error")
  expect_error(bulk_variance_partition(.vst_like(10), .meta_bq(12), "batch",
                                       max_genes = 40, seed = 1)[1], NA)
  # covariable à modalité unique
  meta1 <- .meta_bq(12); meta1$batch <- "b1"
  e <- tryCatch(bulk_variance_partition(.vst_like(), meta1, "batch"), error = function(e) e)
  expect_s3_class(e, "bulk_batch_qc_error")
  # moins de 3 échantillons communs
  m2 <- .vst_like(genes = 10, samples = 2)
  expect_error(bulk_variance_partition(m2, .meta_bq(2), "batch"),
               class = "bulk_batch_qc_error")
})

test_that("repli R pur : fractions cohérentes, somme par gène <= 1", {
  expr <- .vst_like(8, 12, seed = 3)
  out <- .varpart_pure_r_fallback(expr, .meta_bq(12), c("batch", "condition"))
  expect_identical(nrow(out), 8L)
  expect_identical(colnames(out), c("batch", "condition", "Residuals"))
  expect_true(all(complete.cases(out)))
  expect_true(all(rowSums(out) <= 1 + 1e-8))
  expect_true(all(as.matrix(out) >= 0))
})

test_that("plots : consommateurs purs ggplot", {
  vp <- bulk_variance_partition(.vst_like(20), .meta_bq(12), "batch")
  expect_s3_class(plot_bulk_varpart(vp), "ggplot")
  expect_s3_class(plot_bulk_batch_scree(.vst_like(20)), "ggplot")
})
