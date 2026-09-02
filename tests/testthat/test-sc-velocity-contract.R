# =============================================================================
# test-sc-velocity-contract.R — Stage 8 (Velocity 3B-1) : contrat de resultat
# canonique RNA velocity
# =============================================================================
# 12 fixtures deterministes + invariants : dimensions, ordre d'alignement,
# etats de validite, classes/messages d'erreur, completude de provenance,
# tables d'alignement, absence de fabrication silencieuse de vecteurs.
# Aucune donnee biologique : fixtures synthetiques genes x cellules.
# Les fixtures partages vivent dans helper-velocity-fixtures.R (Stages 8-9).
# =============================================================================

skip_if_not_installed("Matrix")
skip_if_not_installed("digest")

# ── Etats de validite : contrat explicite ───────────────────────────────────
test_that("velocity_validity_states exposes the 9 documented contract states", {
  expected <- c(
    "valid", "valid_no_vectors", "valid_partial_embedding", "invalid_input",
    "invalid_orientation", "invalid_cell_alignment", "invalid_gene_alignment",
    "invalid_vector_projection", "stale_against_current_seurat_object"
  )
  expect_setequal(velocity_validity_states(), expected)
  expect_setequal(names(velocity_status_labels()), expected)
  expect_true(velocity_status_is_valid("valid"))
  expect_true(velocity_status_is_valid("valid_no_vectors"))
  expect_true(velocity_status_is_valid("valid_partial_embedding"))
  expect_false(velocity_status_is_valid("invalid_input"))
  expect_false(velocity_status_is_valid("invalid_vector_projection"))
  expect_false(velocity_status_is_valid("stale_against_current_seurat_object"))
  # Validite technique != validite biologique : explicite dans les libelles.
  expect_match(velocity_status_labels()[["valid"]], "technique")
})

# ── Fixture 1 : RDS valide, cellules/genes parfaitement alignes ─────────────
test_that("Fixture 1 - valid RDS with matching cells/genes and validated vectors", {
  vec <- matrix(seq_len(10), nrow = 5, ncol = 2)
  rownames(vec) <- .vel_cells
  colnames(vec) <- c("dx", "dy")
  path <- .vel_rds_fixture(tempfile(fileext = ".rds"),
    extra = list(embedding_reduction = "umap", vectors = vec,
                 velocity_source = "scvelo-test"))
  vi <- read_velocity_rds(path)
  validated <- .vel_validate_and_enrich(vi)
  vectors <- validate_precomputed_velocity_vectors(
    vectors = vi$vectors, cell_names = validated$cell_names,
    selected_reduction = "umap", stored_reduction = "umap"
  )

  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    input_files = list(rds = "velocity.rds"),
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    velocity_vectors = vectors,
    vector_validation = list(field = "vectors", ok = TRUE),
    analysis_id = "sc-velocity"
  )

  expect_identical(canonical$type, "rna_velocity")
  expect_identical(canonical$status, "valid")
  expect_identical(canonical$dimensions, c(cells = 5L, genes = 10L))
  # Ordre d'alignement : ordre Seurat, deterministe.
  expect_identical(canonical$cell_names, .vel_cells)
  expect_identical(canonical$gene_names, .vel_genes)
  expect_identical(colnames(canonical$spliced), .vel_cells)
  expect_identical(rownames(canonical$spliced), .vel_genes)
  expect_identical(nrow(canonical$velocity_vectors), 5L)
  expect_null(canonical$ambiguous)   # absent => NULL, jamais fabrique
  expect_true(velocity_status_is_valid(canonical$status))
})

test_that("Fixture 1b - same RDS without vectors yields valid_no_vectors", {
  path <- .vel_rds_fixture(tempfile(fileext = ".rds"))
  validated <- .vel_validate_and_enrich(read_velocity_rds(path))
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    seurat_obj = .vel_seurat_stub()
  )
  expect_identical(canonical$status, "valid_no_vectors")
  expect_null(canonical$velocity_vectors)   # aucune fabrication de fleches
})

# ── Fixture 2 : MTX valide, orientation declaree ────────────────────────────
test_that("Fixture 2 - valid MTX input with declared orientation stays sparse", {
  dir <- tempfile()
  dir.create(dir)
  Matrix::writeMM(.vel_mat(), file.path(dir, "spliced.mtx"))
  writeLines(.vel_cells, file.path(dir, "barcodes.tsv"))
  writeLines(.vel_genes, file.path(dir, "features.tsv"))
  Matrix::writeMM(.vel_mat() * 2L, file.path(dir, "unspliced.mtx"))
  writeLines(.vel_cells, file.path(dir, "barcodes_u.tsv"))
  writeLines(.vel_genes, file.path(dir, "features_u.tsv"))

  spliced <- read_velocity_mtx(
    file.path(dir, "spliced.mtx"), file.path(dir, "barcodes.tsv"),
    file.path(dir, "features.tsv")
  )
  unspliced <- read_velocity_mtx(
    file.path(dir, "unspliced.mtx"), file.path(dir, "barcodes_u.tsv"),
    file.path(dir, "features_u.tsv")
  )
  validated <- validate_velocity_matrices(
    spliced = spliced, unspliced = unspliced,
    seurat_cells = .vel_cells, seurat_genes = .vel_genes,
    orientation = "genes_x_cells"
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "mtx",
    input_files = list(spliced = "spliced.mtx", unspliced = "unspliced.mtx"),
    seurat_obj = .vel_seurat_stub()
  )
  expect_identical(canonical$status, "valid_no_vectors")
  expect_identical(canonical$dimensions, c(cells = 5L, genes = 10L))
  # Compatibilite sparse : readMM n'est jamais densifie (forme triplet dgT).
  expect_true(inherits(canonical$spliced, "dMatrix"))
  expect_true(inherits(canonical$spliced, "sparseMatrix"))
  expect_identical(canonical$cell_names, .vel_cells)
})

# ── Fixture 3 : entree en orientation inversee ──────────────────────────────
test_that("Fixture 3 - reversed-orientation input is transposed deterministically", {
  m <- Matrix::Matrix(seq_len(50), nrow = 5, ncol = 10, sparse = TRUE)
  dimnames(m) <- list(.vel_cells, .vel_genes)   # cellules x genes

  expect_identical(
    detect_velocity_orientation(m, .vel_cells, .vel_genes, "auto_strict"),
    "cells_x_genes"
  )

  validated <- validate_velocity_matrices(
    spliced = m, unspliced = m * 2L,
    seurat_cells = .vel_cells, seurat_genes = .vel_genes,
    orientation = "cells_x_genes"
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = .vel_seurat_stub()
  )
  expect_identical(canonical$orientation, "cells_x_genes")
  expect_identical(dim(canonical$spliced), c(10L, 5L))  # genes x cellules
  expect_identical(canonical$cell_names, .vel_cells)
  expect_identical(canonical$gene_names, .vel_genes)
})

# ── Fixtures 4-5 : doublons de barcodes / de genes ──────────────────────────
test_that("Fixture 4 - duplicate cell barcodes fail with invalid_input state", {
  m <- .vel_mat()
  colnames(m)[2] <- "cell1"
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes)
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_input")
  expect_match(conditionMessage(err), "dupliques")
})

test_that("Fixture 5 - duplicate gene identifiers fail with invalid_input state", {
  m <- .vel_mat()
  rownames(m)[2] <- "gene1"
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes)
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_input")
  expect_match(conditionMessage(err), "dupliques")
})

# ── Fixtures 6-7 : recouvrement nul ─────────────────────────────────────────
test_that("Fixture 6 - no cell overlap fails with invalid_cell_alignment", {
  foreign_cells <- paste0("foreign", 1:5)
  m <- .vel_mat(cells = foreign_cells)
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes)
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_cell_alignment")
  expect_match(conditionMessage(err), "Aucune cellule alignee")
})

test_that("Fixture 7 - no gene overlap fails with invalid_gene_alignment", {
  foreign_genes <- paste0("foreign_gene", 1:10)
  m <- .vel_mat(genes = foreign_genes)
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes)
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_gene_alignment")
  expect_match(conditionMessage(err), "Aucun gene")
})

test_that("Partial overlap below threshold without override fails loudly", {
  cells <- c("cell1", "cell2", "fx1", "fx2", "fx3")
  m <- .vel_mat(cells = cells)   # recouvrement 2/5 = 40% < 80%
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes)
    NULL
  }, error = function(e) e)
  expect_identical(velocity_error_state(err), "invalid_cell_alignment")
  expect_match(conditionMessage(err), "Recouvrement des cellules")
})

# ── Fixture 8 : recouvrement partiel avec override explicite ────────────────
test_that("Fixture 8 - partial overlap with explicit override aligns deterministically", {
  cells <- c("cell1", "cell2", "fx1", "fx2", "fx3")
  m <- .vel_mat(cells = cells)
  validated <- validate_velocity_matrices(
    spliced = m, unspliced = m * 2L,
    seurat_cells = .vel_cells, seurat_genes = .vel_genes,
    allow_low_overlap = TRUE
  )
  expect_identical(validated$n_cells_matched, 2L)
  expect_identical(validated$n_cells_missing, 3L)
  expect_identical(validated$cell_names, c("cell1", "cell2"))  # ordre Seurat
  expect_equal(validated$normalized_cell_overlap, 0.4)
  expect_match(validated$warnings, "Recouvrement des cellules faible")
  # Table d'alignement : 2 alignees + 3 Seurat sans velocity + 3 hors Seurat.
  cm <- validated$cell_mapping
  expect_identical(nrow(cm), 8L)
  expect_identical(sum(cm$aligned), 2L)
  expect_setequal(cm$seurat_cell[cm$aligned], c("cell1", "cell2"))

  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "mtx", seurat_obj = .vel_seurat_stub()
  )
  expect_identical(canonical$status, "valid_no_vectors")
  expect_true(canonical$cell_alignment$low_overlap_override)
})

# ── Fixture 9 : empreinte objet / perennite ─────────────────────────────────
test_that("Fixture 9 - object fingerprint is deterministic and staleness-aware", {
  stub <- .vel_seurat_stub()
  fp <- velocity_object_fingerprint(stub)
  expect_identical(fp, velocity_object_fingerprint(stub))       # deterministe
  expect_match(fp, "^v2::")                                     # format documente
  expect_null(velocity_object_fingerprint(NULL))

  cells_changed <- .vel_seurat_stub()
  colnames(cells_changed)[1] <- "renamed_cell"
  genes_changed <- .vel_seurat_stub()
  rownames(genes_changed)[1] <- "renamed_gene"

  canonical <- finalize_velocity_result(
    validated = .vel_validate_and_enrich(
      read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))
    ),
    input_mode = "rds", seurat_obj = stub
  )
  expect_identical(velocity_result_is_stale(canonical, stub), FALSE)
  expect_identical(velocity_result_is_stale(canonical, cells_changed), TRUE)
  expect_identical(velocity_result_is_stale(canonical, genes_changed), TRUE)
  expect_identical(velocity_result_is_stale(canonical, NULL), NA)
  expect_identical(velocity_result_is_stale(NULL, stub), NA)
})

# ── Fixture 10 : coordonnees sans vecteurs velocity ─────────────────────────
test_that("Fixture 10 - partial embedding without vectors is valid_partial_embedding", {
  emb <- matrix(seq_len(6), nrow = 3, ncol = 2)
  rownames(emb) <- c("cell1", "cell2", "cell3")   # 3/5 cellules seulement
  path <- .vel_rds_fixture(tempfile(fileext = ".rds"),
    extra = list(umap_embedding = emb, embedding_reduction = "umap"))
  vi <- read_velocity_rds(path)
  validated <- .vel_validate_and_enrich(vi)

  ali <- align_velocity_embedding(
    embedding = validated$umap_embedding,
    cell_names = validated$cell_names,
    selected_reduction = "umap",
    stored_reduction = validated$embedding_reduction
  )
  expect_identical(ali$n_embedding_matched, 3L)
  expect_identical(ali$n_embedding_missing, 2L)
  expect_identical(ali$n_velocity_cells, 5L)

  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = .vel_seurat_stub(),
    requested_reduction = "umap", embedding_alignment = ali
  )
  expect_identical(canonical$status, "valid_partial_embedding")
  expect_null(canonical$velocity_vectors)   # coordonnees != vecteurs : jamais
  expect_identical(canonical$embedding_alignment$n_embedding_matched, 3L)
})

# ── Fixture 11 : champ de vecteurs malforme ─────────────────────────────────
test_that("Fixture 11 - malformed vector field fails with invalid_vector_projection", {
  bad_vec <- matrix(seq_len(15), nrow = 5, ncol = 3)   # 3 colonnes : refuse
  rownames(bad_vec) <- .vel_cells
  err <- tryCatch({
    validate_precomputed_velocity_vectors(bad_vec, .vel_cells, "umap")
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_vector_projection")
  expect_match(conditionMessage(err), "deux colonnes")

  # Voie module : vecteurs rejetes en amont => statut dedie, fleches interdites,
  # matrices alignees conserves et avertissement explicite.
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = .vel_seurat_stub(),
    requested_reduction = "umap",
    vector_validation = list(field = "vectors", ok = FALSE,
                             reason = "test : champ malforme")
  )
  expect_identical(canonical$status, "invalid_vector_projection")
  expect_false(velocity_status_is_valid(canonical$status))
  expect_null(canonical$velocity_vectors)
  expect_match(canonical$warnings, "Vecteurs velocity pre-calcules rejetes")
})

# ── Fixture 12 : override bas-recouvrement visible dans la provenance ───────
test_that("Fixture 12 - low-overlap override and provenance are fully traceable", {
  cells <- c("cell1", "cell2", "fx1", "fx2", "fx3")
  m <- .vel_mat(cells = cells)
  validated <- validate_velocity_matrices(
    spliced = m, unspliced = m * 2L, seurat_cells = .vel_cells,
    seurat_genes = .vel_genes, allow_low_overlap = TRUE
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "mtx",
    input_files = list(spliced = "s.mtx", unspliced = "u.mtx",
                       feature_column = "1"),
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    analysis_id = "sc-velocity"
  )
  p <- canonical$provenance
  # Completude du manifeste (champs minimum Stage 8).
  expect_identical(p$analysis_id, "sc-velocity")
  expect_identical(p$analysis_type, "rna_velocity")
  expect_identical(p$status, canonical$status)
  expect_identical(p$input_mode, "mtx")
  expect_match(p$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  expect_true(isTRUE(p$parameters$allow_low_overlap))
  expect_identical(p$parameters$n_cells_matched, 2L)
  expect_identical(p$parameters$n_cells_missing, 3L)
  expect_false(is.na(p$dataset_hash))
  expect_true("R" %in% names(p$versions))
  expect_match(p$warnings, "Recouvrement des cellules faible")
  expect_false(is.na(p$parameters$object_fingerprint))
})

# ── Seuil de recouvrement declare (config/thresholds.R) ─────────────────────
test_that("Declared overlap threshold is consumed, not hardcoded", {
  cells <- c("cell1", "cell2", "fx1", "fx2", "fx3")
  m <- .vel_mat(cells = cells)
  # 40% < seuil explicite 50% : refuse sans override.
  err <- tryCatch({
    validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                               seurat_cells = .vel_cells, seurat_genes = .vel_genes,
                               min_cell_overlap = 0.5)
    NULL
  }, error = function(e) e)
  expect_identical(velocity_error_state(err), "invalid_cell_alignment")
  # Seuil explicite 30% : 40% passe sans aucun override.
  ok <- validate_velocity_matrices(spliced = m, unspliced = m * 2L,
                                   seurat_cells = .vel_cells, seurat_genes = .vel_genes,
                                   min_cell_overlap = 0.3)
  expect_identical(ok$n_cells_matched, 2L)
  expect_identical(ok$warnings, character(0))
})

# ── Exports du contrat ──────────────────────────────────────────────────────
test_that("Validation summary export preserves identity, settings and status", {
  vec <- matrix(seq_len(10), nrow = 5, ncol = 2)
  rownames(vec) <- .vel_cells
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(embedding_reduction = "umap", vectors = vec)))
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    input_files = list(rds = "velocity.rds"),
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    velocity_vectors = validate_precomputed_velocity_vectors(
      vec, validated$cell_names, "umap", "umap"),
    vector_validation = list(field = "vectors", ok = TRUE)
  )
  df <- build_velocity_validation_summary(canonical)
  expect_identical(nrow(df), 1L)
  expect_true(all(c("analysis_id", "analysis_type", "status", "timestamp_utc",
                    "input_mode", "input_files", "n_cells_matched",
                    "n_cells_missing", "vectors_available", "vector_field",
                    "object_fingerprint", "warnings", "package_versions")
                    %in% names(df)))
  expect_identical(df$analysis_id, "sc-velocity")
  expect_identical(df$status, "valid")
  expect_identical(df$vectors_available, "TRUE")
  expect_match(df$input_files, "velocity.rds")
  expect_match(df$package_versions, "R=")
})

test_that("Alignment mapping export covers cells and genes with counts", {
  canonical <- finalize_velocity_result(
    validated = .vel_validate_and_enrich(
      read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))
    ),
    input_mode = "rds", seurat_obj = .vel_seurat_stub()
  )
  df <- build_velocity_alignment_mapping(canonical)
  expect_identical(nrow(df), 15L)   # 5 cellules + 10 genes
  expect_identical(sum(df$axis == "cell"), 5L)
  expect_identical(sum(df$axis == "gene"), 10L)
  expect_true(all(df$aligned))
  expect_true(all(c("axis", "seurat_id", "velocity_id_original",
                    "velocity_id_normalized", "aligned") %in% names(df)))
  expect_error(
    build_velocity_alignment_mapping(list(spliced = 1)),
    "table d'alignement"
  )
})

test_that("Suffix-stripping alignment is recorded in the mapping table", {
  cells <- paste0("cell", 1:5, "-1")
  m <- .vel_mat(cells = cells)
  validated <- validate_velocity_matrices(
    spliced = m, unspliced = m * 2L, seurat_cells = .vel_cells,
    seurat_genes = .vel_genes, strip_cell_suffix = TRUE
  )
  expect_identical(validated$n_cells_matched, 5L)
  expect_identical(validated$cell_names, .vel_cells)   # identifiants Seurat
  cm <- validated$cell_mapping
  aligned_rows <- cm[cm$aligned, ]
  expect_identical(aligned_rows$velocity_cell_original, cells)  # suffixe -1 garde
  expect_identical(aligned_rows$velocity_cell_normalized, .vel_cells)
  expect_identical(aligned_rows$seurat_cell, .vel_cells)
})

test_that("RDS export roundtrip preserves the canonical contract", {
  canonical <- finalize_velocity_result(
    validated = .vel_validate_and_enrich(
      read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))
    ),
    input_mode = "rds", seurat_obj = .vel_seurat_stub()
  )
  out <- tempfile(fileext = ".rds")
  saveRDS(canonical, out)
  re <- readRDS(out)
  expect_identical(re$status, canonical$status)
  expect_identical(re$analysis_id, "sc-velocity")
  expect_identical(re$dimensions, canonical$dimensions)
  expect_identical(re$provenance$analysis_type, "rna_velocity")
  expect_identical(re$object_identity$fingerprint,
                   canonical$object_identity$fingerprint)
})

# ── Garde-fous du finaliseur ────────────────────────────────────────────────
test_that("finalize_velocity_result refuses structures that were not validated", {
  err <- tryCatch({
    finalize_velocity_result(validated = list(), input_mode = "rds")
    NULL
  }, error = function(e) e)
  expect_s3_class(err, "velocity_validation_error")
  expect_identical(velocity_error_state(err), "invalid_input")
})

test_that("canonical warnings merge without duplication", {
  path <- .vel_rds_fixture(tempfile(fileext = ".rds"))
  validated <- .vel_validate_and_enrich(read_velocity_rds(path))
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = .vel_seurat_stub(),
    extra_warnings = c("Embedding UMAP ignore : test", "Embedding UMAP ignore : test")
  )
  expect_identical(canonical$warnings, "Embedding UMAP ignore : test")
})
