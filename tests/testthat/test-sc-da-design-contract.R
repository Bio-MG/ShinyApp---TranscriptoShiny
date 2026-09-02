# =============================================================================
# test-sc-da-design-contract.R — Stage 13 (4E-0) : contrat de validation du
# plan experimental (abundance differentielle)
# =============================================================================
# Invariants : les cellules ne sont PAS des replicats (pseudoreplication
# bloquee), deux niveaux d'echec separes (erreur structurale vs resultat
# bloque rapporte), bloqueurs collectes, eligibility Milo/scCODA, provenance
# produite meme pour un design bloque, empreinte v2 reutilisee de velocity.
# Fixtures : helper-da-fixtures.R (metadonnees synthetiques deterministes).
# =============================================================================

# ── Contrat : champs, etats, libelles ───────────────────────────────────────
test_that("DA design contract exposes documented fields, states and labels", {
  expect_setequal(da_design_contract_fields(), c(
    "type", "status", "composition_unit", "config",
    "condition_summary", "sample_summary", "condition_batch_table",
    "identity_coverage", "missingness", "exclusions",
    "milo_eligibility", "sccoda_eligibility",
    "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc"
  ))
  expect_setequal(da_design_validity_states(), c(
    "valid", "valid_with_warnings", "invalid_design",
    "invalid_input", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(da_design_status_labels()), da_design_validity_states())
  expect_true(da_design_status_is_valid("valid"))
  expect_true(da_design_status_is_valid("valid_with_warnings"))
  expect_false(da_design_status_is_valid("invalid_design"))
  # Le principe central est explicite dans les libelles.
  expect_match(da_design_status_labels()[["valid"]], "ne sont PAS des replicats")
  expect_match(da_design_status_labels()[["invalid_design"]], "BLOQUE")
})

# ── Fixture 1 : design valide (2 conditions x 2 echantillons) ───────────────
test_that("Fixture 1 - valid design produces a reusable canonical result", {
  canonical <- .da_validate_and_finalize()
  missing <- setdiff(da_design_contract_fields(), names(canonical))
  expect_identical(missing, character(0))
  expect_identical(canonical$type, "da_design")
  expect_identical(canonical$status, "valid")
  expect_true(da_design_status_is_valid(canonical$status))
  # Unite de composition explicite.
  expect_identical(canonical$composition_unit, "sample")
  # Resume conditions : 2 conditions x 2 echantillons x 2 replicats.
  expect_identical(nrow(canonical$condition_summary), 2L)
  expect_true(all(canonical$condition_summary$n_samples == 2L))
  expect_true(all(canonical$condition_summary$n_replicates == 2L))
  expect_identical(sum(canonical$condition_summary$n_cells), 160L)
  # Table echantillons : 4 lignes, 3 identites par echantillon.
  expect_identical(nrow(canonical$sample_summary), 4L)
  expect_true(all(canonical$sample_summary$n_identities == 3L))
  # Table condition x batch croisee (2x2).
  expect_identical(nrow(canonical$condition_batch_table), 4L)
  # Eligibilite Milo/scCODA.
  expect_true(isTRUE(canonical$milo_eligibility$eligible))
  expect_true(isTRUE(canonical$sccoda_eligibility$eligible))
  expect_identical(canonical$milo_eligibility$blockers, character(0))
  # Provenance produite a la validation.
  expect_identical(canonical$provenance$analysis_id, "sc-da-design")
  expect_identical(canonical$provenance$method, "da_design_validation")
  expect_identical(canonical$provenance$analysis_type, "da_design")
  expect_match(canonical$provenance$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_identical(canonical$provenance$parameters$min_replicates_required,
                   TS_DA_MIN_REPLICATES_PER_CONDITION)
  # Empreinte v2 reutilisee de velocity.
  expect_identical(canonical$object_identity$fingerprint,
                   velocity_object_fingerprint(.da_stub_obj()))
  expect_match(canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("Fixture 1b - explicit 'sample IS the replicate' is recorded, not inferred", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(),
    sample_id = "sample_id", condition = "condition",
    replicate_id = NULL, batch = "batch", identity = "cell_type"
  )
  expect_true(isTRUE(canonical$config$replicate_equals_sample))
  expect_true(is.na(canonical$config$replicate_id))
  expect_true(canonical$provenance$parameters$replicate_equals_sample)
  expect_identical(canonical$status, "valid")
})

# ── B4 : pseudoreplication bloquee ──────────────────────────────────────────
test_that("Fixture 2 - single-sample condition is BLOCKED (cells are not replicates)", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B"))
  )
  # Le resultat EST produit (rapport + raisons), mais bloque.
  expect_identical(canonical$status, "invalid_design")
  expect_false(isTRUE(canonical$milo_eligibility$eligible))
  expect_false(isTRUE(canonical$sccoda_eligibility$eligible))
  expect_true(any(grepl("Pseudoreplication", canonical$milo_eligibility$blockers)))
  expect_match(canonical$milo_eligibility$blockers[
    grepl("Pseudoreplication", canonical$milo_eligibility$blockers)][1],
    "NE SONT PAS des replicats")
  # Provenance produite MEME pour un design bloque (statut trace).
  expect_identical(canonical$provenance$status, "invalid_design")
})

test_that("Fixture 2b - explicit replicate column with insufficient distinct replicates is blocked", {
  # 2 echantillons par condition mais UN SEUL ID de replicat par condition.
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(replicates = c(s1 = "r1", s2 = "r1", s3 = "r2", s4 = "r2")),
    replicate_id = "replicate_id"
  )
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("Pseudoreplication", canonical$milo_eligibility$blockers)))
  expect_true(all(canonical$condition_summary$n_replicates < TS_DA_MIN_REPLICATES_PER_CONDITION))
})

# ── B1 : IDs d'echantillon = barcodes ───────────────────────────────────────
test_that("Fixture 3 - sample IDs equal to cell barcodes are blocked", {
  meta <- .da_meta()
  meta$bad_sample <- rownames(meta)
  canonical <- .da_validate_and_finalize(
    metadata = meta, sample_id = "bad_sample", condition = "condition"
  )
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("barcodes", canonical$milo_eligibility$blockers)))
})

# ── B2/B3 : coherence des echantillons ──────────────────────────────────────
test_that("Fixture 4 - a sample mapping to two conditions is blocked", {
  meta <- .da_meta()
  # s1 : moitie A (originales), moitie B — sample_id s1 -> 2 conditions.
  idx_s1 <- which(meta$sample_id == "s1")
  meta$condition[idx_s1[seq_len(20)]] <- "B"
  canonical <- .da_validate_and_finalize(metadata = meta)
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("plusieurs conditions", canonical$milo_eligibility$blockers)))
})

test_that("Fixture 4b - a sample without exploitable condition is blocked", {
  meta <- .da_meta()
  meta$condition[meta$sample_id == "s1"] <- NA_character_
  canonical <- .da_validate_and_finalize(metadata = meta)
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("sans condition exploitable",
                        canonical$milo_eligibility$blockers)))
})

test_that("Fixture 4c - fewer than two conditions is blocked", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(conditions = c(s1 = "A", s2 = "A", s3 = "A", s4 = "A"))
  )
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("AU MOINS deux conditions",
                        canonical$milo_eligibility$blockers)))
})

# ── B5 : batch ──────────────────────────────────────────────────────────────
test_that("Fixture 5 - batch fully confounded with condition is blocked", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(batch = c(s1 = "b1", s2 = "b1", s3 = "b2", s4 = "b2"))
  )
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("parfaitement confondue", canonical$milo_eligibility$blockers)))
})

test_that("Fixture 5b - crossed batch design is valid and reported", {
  canonical <- .da_validate_and_finalize()
  expect_identical(canonical$status, "valid")
  expect_identical(nrow(canonical$condition_batch_table), 4L)
})

test_that("Fixture 5c - batch requested with missing values is blocked", {
  meta <- .da_meta()
  meta$batch[meta$sample_id == "s1"] <- NA_character_
  canonical <- .da_validate_and_finalize(metadata = meta)
  expect_identical(canonical$status, "invalid_design")
  expect_true(any(grepl("sans batch renseigne", canonical$milo_eligibility$blockers)))
  # La missingness est surfcee colonne par colonne.
  expect_identical(canonical$missingness$n_missing[canonical$missingness$column == "batch"], 40L)
})

# ── Avertissements (valid_with_warnings) ────────────────────────────────────
test_that("Fixture 6 - small sample yields a warning, not a block", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(samples = c(s1 = 5, s2 = 40, s3 = 40, s4 = 40))
  )
  expect_identical(canonical$status, "valid_with_warnings")
  expect_true(da_design_status_is_valid(canonical$status))
  expect_true(any(grepl("sous le seuil", canonical$warnings)))
  expect_true(isTRUE(canonical$milo_eligibility$eligible))
})

test_that("Fixture 6b - cell-count imbalance is warned", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(samples = c(s1 = 200, s2 = 10, s3 = 40, s4 = 40))
  )
  expect_identical(canonical$status, "valid_with_warnings")
  expect_true(any(grepl("Desequilibre", canonical$warnings)))
})

test_that("Fixture 6c - identities present in fewer than 2 samples are warned", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(identities_by_sample = list(
      s1 = c("CD4 T", "B", "NK"),
      s2 = c("CD4 T", "B"),
      s3 = c("CD4 T", "B"),
      s4 = c("CD4 T", "B")
    )),
    identity = "cell_type"
  )
  expect_identical(canonical$status, "valid_with_warnings")
  expect_true(any(grepl("moins de 2 echantillons", canonical$warnings)))
  # Couverture : NK presente dans 1 seul echantillon.
  nk <- canonical$identity_coverage[canonical$identity_coverage$identity == "NK", ]
  expect_identical(nk$n_samples, 1L)
  expect_identical(nrow(canonical$identity_coverage), 3L)
})

# ── Exclusions / missingness ────────────────────────────────────────────────
test_that("Fixture 7 - cells with missing sample_id are excluded and counted", {
  canonical <- .da_validate_and_finalize(
    metadata = .da_meta(na_sample_id = 10L)
  )
  expect_identical(canonical$status, "valid")
  expect_identical(canonical$exclusions$n_rows, 10L)
  expect_identical(sum(canonical$condition_summary$n_cells), 150L)
  # Provenance : cellules exclues tracees.
  expect_identical(canonical$provenance$cells_excluded, 10L)
})

test_that("Fixture 7b - all sample_id missing is a blocked dead end (classed error)", {
  meta <- .da_meta()
  meta$sample_id <- NA_character_
  # Aucun echantillon evaluable : erreur classee (etat invalid_design),
  # aucun rapport produit.
  e <- tryCatch(.da_validate_and_finalize(metadata = meta), error = function(e) e)
  expect_s3_class(e, "da_design_error")
  expect_identical(da_design_error_state(e), "invalid_design")
  expect_match(conditionMessage(e), "sample_id manquant")
})

# ── Erreurs structurales : invalid_input, AUCUN resultat ────────────────────
test_that("Fixture 8 - structural failures raise classed errors (no result)", {
  e1 <- tryCatch(validate_da_design(NULL, "sample_id", "condition"),
                 error = function(e) e)
  expect_identical(da_design_error_state(e1), "invalid_input")
  e2 <- tryCatch(validate_da_design(data.frame(), "sample_id", "condition"),
                 error = function(e) e)
  expect_identical(da_design_error_state(e2), "invalid_input")
  meta <- .da_meta()
  e3 <- tryCatch(validate_da_design(meta, "sample_id", "inexistante"),
                 error = function(e) e)
  expect_identical(da_design_error_state(e3), "invalid_input")
  expect_match(conditionMessage(e3), "inexistante")
  e4 <- tryCatch(validate_da_design(meta, "sample_id", "condition",
                                    batch = "pas_la"),
                 error = function(e) e)
  expect_identical(da_design_error_state(e4), "invalid_input")
})

# ── Garde de contrat + peremption ───────────────────────────────────────────
test_that("assert_da_design_result guards consumers and method eligibility", {
  canonical <- .da_validate_and_finalize()
  blocked <- .da_validate_and_finalize(
    metadata = .da_meta(conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B"))
  )
  expect_error(assert_da_design_result(NULL), class = "da_design_error")
  bad <- canonical; bad$type <- "autre"
  expect_error(assert_da_design_result(bad), class = "da_design_error")
  # method = "any" accepte un design bloque (le rapport reste lisible)...
  res <- assert_da_design_result(blocked, method = "any")
  expect_identical(res, blocked)
  # ...mais method = "milo"/"sccoda" REFUSE — un design bloque n'est jamais
  # consommable.
  e <- tryCatch(assert_da_design_result(blocked, method = "milo"),
                error = function(e) e)
  expect_identical(da_design_error_state(e), "invalid_design")
  expect_match(conditionMessage(e), "Pseudoreplication")
  e2 <- tryCatch(assert_da_design_result(blocked, method = "sccoda"),
                 error = function(e) e)
  expect_identical(da_design_error_state(e2), "invalid_design")
  # Design valide : passage transparent pour les deux methodes.
  expect_identical(assert_da_design_result(canonical, method = "milo"), canonical)
  expect_identical(assert_da_design_result(canonical, method = "sccoda"), canonical)
})

test_that("stale detection follows the reused v2 fingerprint", {
  canonical <- .da_validate_and_finalize()
  expect_false(da_design_result_is_stale(canonical, .da_stub_obj()))
  other <- matrix(0, nrow = 3, ncol = 2, dimnames = list(c("g1", "g2", "g3"), c("x", "y")))
  expect_true(da_design_result_is_stale(canonical, other))
  expect_true(is.na(da_design_result_is_stale(canonical, NULL)))
  # Meme empreinte (deterministe) : pas de refus.
  same_obj <- .da_stub_obj()
  expect_identical(assert_da_design_result(canonical, seurat_obj = same_obj),
                   canonical)
  stale_obj <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("x", "y")))
  e2 <- tryCatch(assert_da_design_result(canonical, seurat_obj = stale_obj),
                 error = function(e) e)
  expect_identical(da_design_error_state(e2), "stale_against_current_seurat_object")
})

# ── Exports ──────────────────────────────────────────────────────────────────
test_that("design summary export is a one-row traceable data.frame", {
  canonical <- .da_validate_and_finalize()
  df <- build_da_design_summary(canonical)
  expect_identical(nrow(df), 1L)
  expect_identical(df$analysis_id, "sc-da-design")
  expect_identical(df$status, "valid")
  expect_identical(df$composition_unit, "sample")
  expect_identical(df$milo_eligible, "TRUE")
  expect_identical(df$sccoda_eligible, "TRUE")
  expect_match(df$object_fingerprint, "^v2::")
  expect_true(isTRUE(df$replicate_equals_sample == "FALSE"))
})

test_that("blocked design summary carries blockers and non-eligibility", {
  blocked <- .da_validate_and_finalize(
    metadata = .da_meta(conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B"))
  )
  df <- build_da_design_summary(blocked)
  expect_identical(df$milo_eligible, "FALSE")
  expect_match(df$blockers, "Pseudoreplication")
})

test_that("samples export reproduces the sample table with analysis_id", {
  canonical <- .da_validate_and_finalize()
  df <- build_da_design_sample_export(canonical)
  expect_identical(nrow(df), nrow(canonical$sample_summary))
  expect_true(all(df$analysis_id == "sc-da-design"))
  expect_true(all(c("sample", "condition", "n_cells") %in% colnames(df)))
  # Sans table : erreur classee.
  e <- tryCatch(
    build_da_design_sample_export(list(type = "da_design", status = "valid")),
    error = function(e) e
  )
  expect_identical(da_design_error_state(e), "invalid_input")
})

test_that("export filenames are traced by analysis_id", {
  canonical <- .da_validate_and_finalize()
  fn <- da_design_export_filename(canonical, "da_design_summary", "csv")
  expect_match(fn, "^da_design_summary_sc-da-design_\\d{4}-\\d{2}-\\d{2}\\.csv$")
  expect_error(da_design_export_filename(NULL, "k", "csv"), class = "da_design_error")
})
