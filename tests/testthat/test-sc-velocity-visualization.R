# =============================================================================
# test-sc-velocity-visualization.R — Stage 9 (Velocity 3B-2) : contrats de
# rendu
# =============================================================================
# Les visualisations sont des CONSOMMATRICES pures du resultat canonique
# (Stage 8) : fleches uniquement depuis des vecteurs strictement valides,
# resultat perime refuse, portee de donnees (preview/full) affichee,
# aucun effet directionnel sans vecteurs, aucune mutation du resultat
# scientifique par les fonctions de rendu ou d'export.
# Fixtures partages : helper-velocity-fixtures.R.
# =============================================================================

skip_if_not_installed("Matrix")
skip_if_not_installed("digest")
skip_if_not_installed("ggplot2")

# Resultats canoniques reutilisables -----------------------------------------
.vel_canonical_no_vectors <- function(seurat_stub = .vel_seurat_stub()) {
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))
  )
  finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = seurat_stub
  )
}

.vel_canonical_with_vectors <- function(seurat_stub = .vel_seurat_stub()) {
  vec <- matrix(seq_len(10), nrow = 5, ncol = 2)
  rownames(vec) <- .vel_cells
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(embedding_reduction = "umap", vectors = vec)))
  )
  finalize_velocity_result(
    validated = validated, input_mode = "rds", seurat_obj = seurat_stub,
    requested_reduction = "umap",
    velocity_vectors = validate_precomputed_velocity_vectors(
      vec, validated$cell_names, "umap", "umap"),
    vector_validation = list(field = "vectors", ok = TRUE)
  )
}

.vel_embedding_fixture <- function(cells = .vel_cells) {
  emb <- matrix(seq_len(2 * length(cells)), nrow = length(cells), ncol = 2)
  rownames(emb) <- cells
  emb
}

# ── Garde de contrat des visualisations ─────────────────────────────────────
test_that("assert_velocity_result enforces canonicality, states and staleness", {
  expect_error(
    assert_velocity_result(list()),
    class = "velocity_validation_error"
  )
  err <- tryCatch(assert_velocity_result(list()), error = function(e) e)
  expect_identical(velocity_error_state(err), "invalid_input")

  canonical <- .vel_canonical_no_vectors()
  returned <- assert_velocity_result(canonical)
  expect_identical(returned, canonical)

  bad <- canonical
  bad$status <- "statut_fantome"
  err_bad <- tryCatch(assert_velocity_result(bad), error = function(e) e)
  expect_identical(velocity_error_state(err_bad), "invalid_input")

  other <- .vel_seurat_stub()
  colnames(other)[1] <- "renamed_cell"
  err_stale <- tryCatch(
    assert_velocity_result(canonical, seurat_obj = other),
    error = function(e) e
  )
  expect_identical(
    velocity_error_state(err_stale),
    "stale_against_current_seurat_object"
  )

  # Vue fleches refusee sans vecteurs valides (message exploitable).
  err_vec <- tryCatch(
    assert_velocity_result(canonical, view = "vectors"),
    error = function(e) e
  )
  expect_identical(velocity_error_state(err_vec), "invalid_vector_projection")
  expect_match(conditionMessage(err_vec), "coordonnees")
})

# ── Phase portrait ──────────────────────────────────────────────────────────
test_that("valid phase portrait renders with declared data scope", {
  canonical <- .vel_canonical_no_vectors()
  p <- plot_velocity_phase_portrait(canonical, "gene1",
                                    seurat_obj = .vel_seurat_stub())
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "gene1")
  expect_match(p$labels$subtitle, "complet")
  expect_match(p$labels$subtitle, "sc-velocity")

  # Fixture elargi : 60 cellules pour exercer le cap affiche.
  big_cells <- paste0("cell", 1:60)
  big_stub <- .vel_seurat_stub(cells = big_cells)
  big_validated <- validate_velocity_matrices(
    spliced = .vel_mat(cells = big_cells),
    unspliced = .vel_mat(cells = big_cells) * 2L,
    seurat_cells = big_cells, seurat_genes = .vel_genes
  )
  big_validated$velocity_source <- "test"
  big_validated$velocity_method <- "precomputed"
  big_validated$input_orientation <- NA_character_
  big_validated$embedding_reduction <- NA_character_
  big_canonical <- finalize_velocity_result(
    validated = big_validated, input_mode = "rds", seurat_obj = big_stub
  )

  p_preview <- plot_velocity_phase_portrait(big_canonical, "gene1",
                                            data_scope = "preview",
                                            max_cells = 20L)
  expect_identical(nrow(p_preview$data), 20L)
  expect_match(p_preview$labels$subtitle, "sous-echantillonne")

  p_full <- plot_velocity_phase_portrait(big_canonical, "gene1",
                                         data_scope = "full",
                                         max_cells = 20L)
  expect_identical(nrow(p_full$data), 60L)
  expect_match(p_full$labels$subtitle, "complet")

  expect_error(
    plot_velocity_phase_portrait(canonical, "gene_absent"),
    class = "velocity_validation_error"
  )
})

# ── Embedding : fleches / points seuls / vecteurs rejetes ───────────────────
test_that("embedding plot draws arrows only from validated vectors", {
  canonical_v <- .vel_canonical_with_vectors()
  canonical_p <- .vel_canonical_no_vectors()
  emb <- .vel_embedding_fixture()

  p_arrows <- plot_velocity_embedding(
    canonical_v, emb, selected_reduction = "umap",
    seurat_obj = .vel_seurat_stub()
  )
  expect_s3_class(p_arrows, "ggplot")
  expect_identical(length(p_arrows$layers), 2L)   # points + segments (fleches)
  expect_match(p_arrows$labels$subtitle, "couvertes par un vecteur")

  p_points <- plot_velocity_embedding(canonical_p, emb)
  expect_identical(length(p_points$layers), 1L)   # points seuls, aucune fleche
  expect_match(p_points$labels$subtitle, "aucun vecteur velocity valide")

  canonical_r <- finalize_velocity_result(
    validated = .vel_validate_and_enrich(
      read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))),
    input_mode = "rds", seurat_obj = .vel_seurat_stub(),
    vector_validation = list(field = "vectors", ok = FALSE,
                             reason = "test : malforme")
  )
  p_rejected <- plot_velocity_embedding(canonical_r, emb)
  expect_identical(length(p_rejected$layers), 1L)
  expect_match(p_rejected$labels$subtitle, "rejetes")

  # Embedding d'affichage invalide : refuse.
  expect_error(
    plot_velocity_embedding(canonical_v, matrix(1:10, nrow = 5L)),
    class = "velocity_validation_error"
  )

  # Peremption : refuse.
  other <- .vel_seurat_stub()
  colnames(other)[1] <- "renamed"
  err_stale <- tryCatch(
    plot_velocity_embedding(canonical_v, emb, seurat_obj = other),
    error = function(e) e
  )
  expect_identical(
    velocity_error_state(err_stale),
    "stale_against_current_seurat_object"
  )
})

# ── Vue champ de vecteurs ────────────────────────────────────────────────────
test_that("vector field view requires validated vectors", {
  canonical_v <- .vel_canonical_with_vectors()
  canonical_p <- .vel_canonical_no_vectors()
  emb <- .vel_embedding_fixture()

  err <- tryCatch(
    plot_velocity_vector_field(canonical_p, emb),
    error = function(e) e
  )
  expect_identical(velocity_error_state(err), "invalid_vector_projection")

  p_field <- plot_velocity_vector_field(canonical_v, emb,
                                        selected_reduction = "umap")
  expect_s3_class(p_field, "ggplot")
  expect_identical(length(p_field$layers), 2L)
})

# ── Couverture partielle affichee ───────────────────────────────────────────
test_that("partial embedding coverage is stated on the plot", {
  emb_partial <- matrix(seq_len(6), nrow = 3, ncol = 2)
  rownames(emb_partial) <- c("cell1", "cell2", "cell3")
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(umap_embedding = emb_partial,
                   embedding_reduction = "umap")))
  )
  ali <- align_velocity_embedding(
    validated$umap_embedding, validated$cell_names, "umap",
    validated$embedding_reduction
  )
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    embedding_alignment = ali
  )
  expect_identical(canonical$status, "valid_partial_embedding")
  p <- plot_velocity_embedding(canonical, ali$embedding,
                               selected_reduction = "umap")
  expect_match(p$labels$subtitle, "Couverture embedding RDS : 3/5")
})

# ── Vues couverture + QC ────────────────────────────────────────────────────
test_that("coverage view: histogram with vectors, explicit message without", {
  canonical_v <- .vel_canonical_with_vectors()
  p_cov <- plot_velocity_coverage(canonical_v)
  expect_s3_class(p_cov, "ggplot")
  expect_match(p_cov$labels$subtitle, "vecteur valide")

  canonical_p <- .vel_canonical_no_vectors()
  p_none <- plot_velocity_coverage(canonical_p)
  expect_s3_class(p_none, "ggplot")
  expect_match(p_none$labels$subtitle, "Aucun vecteur valide")
})

test_that("alignment QC view summarizes counts from the canonical result", {
  canonical <- .vel_canonical_with_vectors()
  p_qc <- plot_velocity_alignment_qc(canonical)
  expect_s3_class(p_qc, "ggplot")
  expect_match(p_qc$labels$subtitle, "sc-velocity")
  expect_true(all(c("Cellules", "Genes") %in% unique(p_qc$data$axis)))
  expect_identical(sum(p_qc$data$n[p_qc$data$categorie == "Alignees"]), 5L)
  expect_identical(sum(p_qc$data$n[p_qc$data$categorie == "Alignes"]), 10L)
})

# ── Exports ─────────────────────────────────────────────────────────────────
test_that("per-cell vectors export preserves identity and availability", {
  canonical_v <- .vel_canonical_with_vectors()
  df <- build_velocity_cell_vectors_export(canonical_v)
  expect_identical(nrow(df), 5L)
  expect_true(all(df$has_vector))
  expect_false(any(df$in_embedding))     # pas d'embedding fourni => NA
  expect_true(all(is.na(df$embedding_x)))
  expect_identical(unique(df$analysis_id), "sc-velocity")
  expect_identical(unique(df$status), "valid")
  expect_match(unique(df$timestamp_utc), "^\\d{4}-\\d{2}-\\d{2}T")

  # Avec embedding partiel : in_embedding coherente.
  emb_partial <- matrix(seq_len(6), nrow = 3, ncol = 2)
  rownames(emb_partial) <- c("cell1", "cell2", "cell3")
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(umap_embedding = emb_partial,
                   embedding_reduction = "umap")))
  )
  ali <- align_velocity_embedding(validated$umap_embedding,
                                  validated$cell_names, "umap",
                                  validated$embedding_reduction)
  canonical_partial <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    seurat_obj = .vel_seurat_stub(), embedding_alignment = ali
  )
  df2 <- build_velocity_cell_vectors_export(canonical_partial)
  expect_identical(sum(df2$in_embedding), 3L)
})

test_that("export filenames carry the analysis id and extension", {
  canonical <- .vel_canonical_no_vectors()
  fn <- velocity_export_filename(canonical, "velocity_phase_portrait", "png")
  expect_match(fn, "^velocity_phase_portrait_sc-velocity_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.png$")
  fn_pdf <- velocity_export_filename(canonical, "velocity_embedding", "pdf")
  expect_match(fn_pdf, "^velocity_embedding_sc-velocity_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.pdf$")
  expect_error(
    velocity_export_filename(NULL, "k", "png"),
    class = "velocity_validation_error"
  )
})

# ── Non-mutation du resultat scientifique ───────────────────────────────────
test_that("plot and export functions never mutate the scientific result", {
  emb_partial <- matrix(seq_len(6), nrow = 3, ncol = 2)
  rownames(emb_partial) <- c("cell1", "cell2", "cell3")
  vec <- matrix(seq_len(10), nrow = 5, ncol = 2)
  rownames(vec) <- .vel_cells
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(umap_embedding = emb_partial,
                   embedding_reduction = "umap", vectors = vec)))
  )
  ali <- align_velocity_embedding(validated$umap_embedding,
                                  validated$cell_names, "umap",
                                  validated$embedding_reduction)
  canonical <- finalize_velocity_result(
    validated = validated, input_mode = "rds",
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    embedding_alignment = ali,
    velocity_vectors = validate_precomputed_velocity_vectors(
      vec, validated$cell_names, "umap", "umap"),
    vector_validation = list(field = "vectors", ok = TRUE)
  )

  before <- digest::digest(canonical)
  invisible(plot_velocity_phase_portrait(canonical, "gene1"))
  invisible(plot_velocity_embedding(canonical, ali$embedding,
                                    selected_reduction = "umap"))
  invisible(plot_velocity_vector_field(canonical, ali$embedding))
  invisible(plot_velocity_coverage(canonical))
  invisible(plot_velocity_alignment_qc(canonical))
  invisible(build_velocity_cell_vectors_export(canonical))
  invisible(velocity_export_filename(canonical, "k", "png"))
  invisible(build_velocity_validation_summary(canonical))
  expect_identical(digest::digest(canonical), before)
})
