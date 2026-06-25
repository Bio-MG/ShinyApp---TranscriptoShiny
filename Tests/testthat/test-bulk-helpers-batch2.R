# Tests unitaires complémentaires — points 1 & 2 (confounding check, design validation)
# A ajouter à côté de test-bulk-helpers.R (tests/testthat/test-bulk-helpers.R)
library(testthat)

toy_meta_confounded <- data.frame(
  condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated"),
  batch     = c("A", "A", "A", "B", "B", "B"),          # fully confounded with condition
  row.names = paste0("S", 1:6)
)

toy_meta_balanced <- data.frame(
  condition = c("Control", "Control", "Control", "Treated", "Treated", "Treated"),
  batch     = c("A", "B", "A", "B", "A", "B"),           # balanced across condition
  row.names = paste0("S", 1:6)
)

test_that("check_design_confounding detects a fully confounded covariate", {
  expect_true(check_design_confounding(toy_meta_confounded, "condition", "batch"))
})

test_that("check_design_confounding does not flag a balanced covariate", {
  expect_false(check_design_confounding(toy_meta_balanced, "condition", "batch"))
})

test_that("check_design_confounding handles missing columns gracefully", {
  expect_false(check_design_confounding(toy_meta_balanced, "condition", "nonexistent_col"))
})

test_that("validate_bulk_design flags confounded covariates with an actionable message", {
  problems <- validate_bulk_design(toy_meta_confounded, "condition", covariates = "batch")
  expect_true(any(grepl("confondue", problems)))
})

test_that("validate_bulk_design flags low-replicate groups", {
  toy_meta_low_rep <- data.frame(
    condition = c("Control", "Treated", "Treated", "Treated"),
    row.names = paste0("S", 1:4)
  )
  problems <- validate_bulk_design(toy_meta_low_rep, "condition")
  expect_true(any(grepl("seul réplicat", problems)))
})

test_that("validate_bulk_design returns no problems for a clean balanced design", {
  problems <- validate_bulk_design(toy_meta_balanced, "condition", covariates = "batch")
  expect_length(problems, 0)
})

test_that("validate_bulk_design flags NA values in the condition column", {
  toy_meta_na <- toy_meta_balanced
  toy_meta_na$condition[1] <- NA
  problems <- validate_bulk_design(toy_meta_na, "condition")
  expect_true(any(grepl("valeur manquante", problems)))
})

test_that("detect_gene_id_type correctly identifies HGNC symbols (GSE164073-style)", {
  symbols <- c("A1BG", "A1BG-AS1", "A1CF", "A2M", "TP53", "ACTB")
  expect_equal(detect_gene_id_type(symbols), "symbol")
})

test_that("detect_gene_id_type correctly identifies Ensembl IDs", {
  ensembl_ids <- c("ENSG00000121410", "ENSG00000175899", "ENSG00000256069")
  expect_equal(detect_gene_id_type(ensembl_ids), "ensembl")
})

test_that("detect_gene_id_type correctly identifies Entrez IDs", {
  entrez_ids <- as.character(c(1, 2, 3, 100, 7157))
  expect_equal(detect_gene_id_type(entrez_ids), "entrez")
})

test_that("infer_metadata_from_names splits consistent sample names correctly", {
  names_consistent <- c("MW1_cornea_mock_1", "MW4_cornea_CoV2_1", "MW7_retina_mock_1")
  meta <- infer_metadata_from_names(names_consistent,
                                    col_names = c("batch_id", "tissue", "condition", "replicate"))
  expect_equal(nrow(meta), 3)
  expect_equal(meta["MW1_cornea_mock_1", "tissue"], "cornea")
  expect_equal(meta["MW4_cornea_CoV2_1", "condition"], "CoV2")
  expect_true(is.integer(meta$replicate))
})

test_that("infer_metadata_from_names warns and truncates on inconsistent segment counts", {
  names_inconsistent <- c("MW1_cornea_mock_1", "MW4_cornea_1")  # one has 3 segments, other has 4
  expect_warning(
    meta <- infer_metadata_from_names(names_inconsistent, col_names = c("a", "b", "c")),
    "inégal"
  )
  expect_equal(ncol(meta), 3)
})
