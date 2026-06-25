# Tests unitaires des helpers purs (pas de Shiny runtime nécessaire)
library(testthat)

# Jeu de données simulé : 200 gènes x 6 échantillons, 2 groupes x 3 réplicats
set.seed(123)
toy_counts <- matrix(
  rpois(200 * 6, lambda = 50),
  nrow = 200, ncol = 6,
  dimnames = list(paste0("Gene", 1:200), paste0("S", 1:6))
)
# Injecter un signal différentiel sur les 20 premiers gènes
toy_counts[1:20, 4:6] <- toy_counts[1:20, 4:6] + 200

toy_meta <- data.frame(
  condition = rep(c("Control", "Treated"), each = 3),
  batch     = rep(c("A", "B", "A"), 2),
  row.names = paste0("S", 1:6)
)

test_that("filter_bulk_counts removes low-count genes correctly", {
  filtered <- filter_bulk_counts(toy_counts, min_count = 50, min_samples = 1)
  expect_true(nrow(filtered) <= nrow(toy_counts))
  expect_true(all(rowSums(filtered) >= 50))
})

test_that("filter_bulk_counts errors clearly when threshold too strict", {
  expect_error(filter_bulk_counts(toy_counts, min_count = 1e9), "Aucun gène")
})

test_that("build_dds aligns samples and runs DESeq2", {
  skip_if_not_installed("DESeq2")
  dds <- build_dds(toy_counts, toy_meta, design_formula = "~ condition")
  expect_s4_class(dds, "DESeqDataSet")
  expect_equal(ncol(dds), 6)
})

test_that("build_dds rounds non-integer counts with a warning", {
  skip_if_not_installed("DESeq2")
  noisy <- toy_counts + 0.5
  expect_warning(
    dds <- build_dds(noisy, toy_meta, design_formula = "~ condition", run_deseq = FALSE),
    "non-entières"
  )
  expect_true(all(DESeq2::counts(dds) == round(DESeq2::counts(dds))))
})

test_that("extract_deseq2_contrast returns a properly ordered, normalized result table", {
  skip_if_not_installed("DESeq2")
  dds <- build_dds(toy_counts, toy_meta, design_formula = "~ condition")
  res <- extract_deseq2_contrast(dds, "condition", "Treated", "Control", shrink = FALSE)

  expect_true(all(c("gene", "log2FoldChange", "pvalue", "padj") %in% colnames(res)))
  expect_true(is.numeric(res$padj))
  # Résultats triés par p-adj croissant
  ord <- order(res$padj)
  expect_equal(res$padj, res$padj[ord])
  # Le signal injecté (Gene1-20) doit ressortir avec un LFC positif marqué
  expect_true(mean(res$log2FoldChange[res$gene %in% paste0("Gene", 1:20)]) > 0)
})

test_that("run_edger_de produces normalized column names", {
  skip_if_not_installed("edgeR")
  res <- run_edger_de(toy_counts, toy_meta, "condition", "Treated", "Control")
  expect_true(all(c("gene", "log2FoldChange", "pvalue", "padj") %in% colnames(res)))
})

test_that("run_bulk_de_dispatch errors on unknown engine", {
  expect_error(
    run_bulk_de_dispatch("unknown_engine", toy_counts, toy_meta, "condition", "Treated", "Control"),
    "non supporté"
  )
})

test_that(".normalize_de_cols fills baseMean from counts when missing", {
  fake_res <- data.frame(gene = c("Gene1", "Gene2"),
                         log2FoldChange = c(1, -1),
                         pvalue = c(0.01, 0.02),
                         padj   = c(0.01, 0.02))
  out <- .normalize_de_cols(fake_res, counts_for_basemean = toy_counts)
  expect_true("baseMean" %in% colnames(out))
  expect_false(any(is.na(out$baseMean)))
})

test_that("plot_bulk_pca returns a ggplot object without error", {
  skip_if_not_installed("DESeq2")
  dds <- build_dds(toy_counts, toy_meta, design_formula = "~1", run_deseq = FALSE)
  dds <- DESeq2::estimateSizeFactors(dds)
  vst_mat <- get_vst_matrix(dds)
  p <- plot_bulk_pca(vst_mat, toy_meta, color_by = "condition")
  expect_s3_class(p, "ggplot")
})