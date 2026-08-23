# =============================================================================
# R/utils_spatial_deconv_tasks.R — Shared deconvolution mirai task body
# =============================================================================
# v1 (Phase 2 refactor): extracted from mod_spatial_deconv.R AND
# mod_spatial_pipeline.R, which each carried a ~200-line verbatim copy of
# the same 3-branch mirai body (rctd / labeltransfer / stdeconvolve).
#
# This file is PRELOADED into every mirai daemon (add to source_files in
# init_spatial_daemons(), R/utils_spatial_async.R) so the function is
# available inside the daemon without re-serialization.
#
# Contains ONLY the computation body — no Shiny reactivity, no ExtendedTask,
# no UI. Callers wrap it in mirai::mirai({ ... }) with .timeout as before.
#
# Depends on: BPCells, Seurat, SeuratObject, Matrix, spacexr (optional),
#             STdeconvolve + topicmodels + slam (optional), future.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Load a prepared reference artifact (manifest.rds → counts + cell_types)
#'
#' Called inside the daemon. Reads the small self-contained manifest written
#' by prepare_reference_artifact() (R/utils_spatial_reference.R, main process).
#'
#' @param manifest_path Character path to manifest.rds.
#' @return list(counts = <dgCMatrix or BPCells IterableMatrix>, cell_types = <factor>).
.load_reference_artifact <- function(manifest_path) {
  manifest <- tryCatch(readRDS(manifest_path), error = function(e) {
    stop("Lecture du manifest de reference impossible (", manifest_path, ") : ",
         conditionMessage(e))
  })
  counts <- if (identical(manifest$backend, "bpcells")) {
    if (!requireNamespace("BPCells", quietly = TRUE)) {
      stop("Package 'BPCells' requis pour lire la reference preparee (backend bpcells).")
    }
    BPCells::open_matrix_dir(manifest$counts_path)
  } else {
    readRDS(manifest$counts_path)
  }
  list(counts = counts, cell_types = manifest$cell_types)
}

#' Run SCTransform with sequential future plan (safe inside daemon)
#'
#' @param obj_to_transform Seurat object.
#' @param ncells Integer, ncells parameter for SCTransform.
#' @return SCTransform-ed Seurat object.
.sctransform_sequential <- function(obj_to_transform, ncells = 3000) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("sequential")
  Seurat::SCTransform(obj_to_transform, ncells = ncells, verbose = FALSE)
}

#' Shared deconvolution computation body (runs inside mirai daemon)
#'
#' @param bpcells_dir Character path to the BPCells count matrix directory.
#' @param pass_idx Integer vector of QC-passing indices, or NULL.
#' @param coords data.frame(id, x, y) full-resolution spatial coordinates.
#' @param mode One of "rctd", "labeltransfer", "stdeconvolve".
#' @param ref_path Character path to manifest.rds (rctd/labeltransfer only).
#' @param n_topics Integer, LDA topics (stdeconvolve only).
#' @param n_top_od Integer, max overdispersed genes (stdeconvolve only).
#' @param lt_npcs Integer, PCA dims for label transfer query.
#' @param lt_norm_method "lognorm" or "sct".
#' @param lt_ncells Integer, SCTransform ncells.
#' @param min_shared_genes Integer, minimum shared genes check.
#' @param log_file Character path for write_mirai_log() progress.
#' @return data.frame(id, <proportion columns>).
run_spatial_deconv_body <- function(bpcells_dir, pass_idx, coords, mode,
                                     ref_path, n_topics, n_top_od,
                                     lt_npcs, lt_norm_method, lt_ncells,
                                     min_shared_genes, log_file) {

  write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
  mat <- BPCells::open_matrix_dir(bpcells_dir)
  if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
  coords_df <- coords[match(colnames(mat), coords$id), c("x", "y")]
  rownames(coords_df) <- colnames(mat)
  keep <- stats::complete.cases(coords_df)
  mat <- mat[, keep, drop = FALSE]
  coords_df <- coords_df[keep, , drop = FALSE]

  # ── RCTD ────────────────────────────────────────────────────────────────
  if (identical(mode, "rctd")) {
    if (!requireNamespace("spacexr", quietly = TRUE)) {
      stop("Package 'spacexr' requis (remotes::install_github('dmcable/spacexr')).")
    }
    write_mirai_log(log_file, "Chargement de la reference preparee (artifact disque)...", 2, 5)
    reloaded   <- .load_reference_artifact(ref_path)
    ref_counts <- reloaded$counts
    if (!inherits(ref_counts, c("dgCMatrix", "matrix"))) {
      ref_counts <- methods::as(ref_counts, "dgCMatrix")
    }
    write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                       ncol(ref_counts), nrow(ref_counts)), 2, 5)
    reference <- spacexr::Reference(counts = ref_counts, cell_types = reloaded$cell_types)

    write_mirai_log(log_file, "Construction du 'puck' spatial (SpatialRNA)...", 3, 5)
    counts_dense <- methods::as(mat, "dgCMatrix")
    puck <- spacexr::SpatialRNA(coords = coords_df, counts = counts_dense)

    write_mirai_log(log_file, "RCTD (mode full, mono-coeur — pas de sous-processus imbrique)...", 4, 5)
    rctd <- spacexr::create.RCTD(puck, reference, max_cores = 1)
    rctd <- spacexr::run.RCTD(rctd, doublet_mode = "full")
    w <- as.matrix(rctd@results$weights)
    w <- sweep(w, 1, rowSums(w), "/")

    write_mirai_log(log_file, "Termine.", 5, 5)
    return(data.frame(id = rownames(w), w, row.names = NULL, check.names = FALSE))
  }

  # ── Label Transfer ──────────────────────────────────────────────────────
  if (identical(mode, "labeltransfer")) {
    use_sct <- identical(lt_norm_method, "sct")

    if (use_sct) {
      has_glmgampoi <- requireNamespace("glmGamPoi", quietly = TRUE)
      write_mirai_log(log_file, sprintf(
        "Preparation de la reference (SCTransform, ncells=%d%s)...",
        lt_ncells, if (has_glmgampoi) "" else " -- 'glmGamPoi' absent, SCTransform sera plus lent"
      ), 2, 5)
    } else {
      write_mirai_log(log_file, "Preparation de la reference (LogNormalize, rapide)...", 2, 5)
    }

    reloaded <- .load_reference_artifact(ref_path)
    ref_obj  <- Seurat::CreateSeuratObject(counts = reloaded$counts)
    ref_obj$cell_type <- as.character(reloaded$cell_types)[match(colnames(ref_obj), names(reloaded$cell_types))]
    ref_obj  <- subset(ref_obj, cells = colnames(ref_obj)[!is.na(ref_obj$cell_type)])
    if (ncol(ref_obj) < 10) {
      stop("Reference trop petite apres filtrage des annotations manquantes (< 10 cellules annotees).")
    }
    write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                       ncol(ref_obj), nrow(ref_obj)), 2, 5)

    if (use_sct) {
      ref_obj <- .sctransform_sequential(ref_obj, ncells = lt_ncells)
    } else {
      ref_obj <- Seurat::NormalizeData(ref_obj, verbose = FALSE)
      ref_obj <- Seurat::FindVariableFeatures(ref_obj, verbose = FALSE)
      ref_obj <- Seurat::ScaleData(ref_obj, verbose = FALSE)
    }
    ref_obj <- Seurat::RunPCA(ref_obj, npcs = 50, verbose = FALSE)

    write_mirai_log(log_file, sprintf(
      "Preparation de la requete spatiale (%s)...",
      if (use_sct) "SCTransform" else "LogNormalize, rapide"
    ), 3, 5)
    query <- Seurat::CreateSeuratObject(counts = mat)
    if (use_sct) {
      query <- .sctransform_sequential(query, ncells = lt_ncells)
    } else {
      query <- Seurat::NormalizeData(query, verbose = FALSE)
      query <- Seurat::FindVariableFeatures(query, verbose = FALSE)
      query <- Seurat::ScaleData(query, verbose = FALSE)
    }
    n_pc  <- max(2, min(lt_npcs, ncol(query) - 1, nrow(query) - 1, 50))
    query <- Seurat::RunPCA(query, npcs = n_pc, verbose = FALSE)

    shared_genes <- intersect(rownames(ref_obj), rownames(query))
    if (length(shared_genes) < min_shared_genes) {
      stop(sprintf(
        paste0(
          "Seulement %d gene(s) commun(s) entre la reference et les donnees spatiales ",
          "(minimum requis : %d). Causes probables : conventions d'identifiants differentes ",
          "(symboles vs Ensembl) ou organismes differents (humain/souris)."
        ),
        length(shared_genes), min_shared_genes
      ), call. = FALSE)
    }
    write_mirai_log(log_file, sprintf("%d genes communs reference/requete.", length(shared_genes)), 3, 5)

    write_mirai_log(log_file, sprintf("FindTransferAnchors (methode %s)...",
                                       if (use_sct) "SCT" else "LogNormalize"), 4, 5)
    anchors <- Seurat::FindTransferAnchors(
      reference = ref_obj, query = query,
      normalization.method = if (use_sct) "SCT" else "LogNormalize",
      npcs = min(30, n_pc)
    )
    predictions <- Seurat::TransferData(
      anchorset = anchors, refdata = ref_obj$cell_type,
      prediction.assay = TRUE,
      weight.reduction = query[["pca"]], dims = seq_len(n_pc)
    )

    write_mirai_log(log_file, "Termine.", 5, 5)
    pred_mat <- t(as.matrix(SeuratObject::LayerData(predictions, layer = "data")))
    pred_mat <- pred_mat[, setdiff(colnames(pred_mat), "max"), drop = FALSE]
    return(data.frame(id = rownames(pred_mat), pred_mat, row.names = NULL, check.names = FALSE))
  }

  # ── STdeconvolve (LDA) ─────────────────────────────────────────────────
  if (!requireNamespace("STdeconvolve", quietly = TRUE) ||
      !requireNamespace("topicmodels", quietly = TRUE) ||
      !requireNamespace("slam", quietly = TRUE)) {
    stop("Packages 'STdeconvolve', 'topicmodels' et 'slam' requis.")
  }
  write_mirai_log(log_file, "Pretraitement (genes surdisperses)...", 2, 5)
  counts_dense <- as.matrix(methods::as(mat, "dgCMatrix"))
  storage.mode(counts_dense) <- "integer"
  corpus <- STdeconvolve::restrictCorpus(
    counts_dense, alpha = 0.05,
    nTopOD = min(n_top_od, nrow(counts_dense)), verbose = FALSE, plot = FALSE
  )

  write_mirai_log(log_file, sprintf("Ajustement LDA (K=%d, mono-coeur, iterations plafonnees)...", n_topics), 3, 5)
  corpus_stm <- slam::as.simple_triplet_matrix(t(as.matrix(corpus)))
  lda_model <- topicmodels::LDA(
    corpus_stm, k = n_topics,
    control = list(seed = 0, verbose = 0, keep = 0, estimate.alpha = FALSE,
                   em = list(iter.max = 100), var = list(iter.max = 50))
  )

  write_mirai_log(log_file, "Extraction des proportions (theta)...", 4, 5)
  post  <- topicmodels::posterior(lda_model)
  theta <- post$topics
  beta  <- post$terms
  theta[theta < 0.05] <- 0
  theta <- theta / rowSums(theta)
  theta[is.na(theta)] <- 0

  top_marker_genes <- apply(beta, 1, function(row) {
    ord <- order(row, decreasing = TRUE)
    paste(colnames(beta)[ord[seq_len(min(3, length(ord)))]], collapse = ".")
  })
  colnames(theta) <- paste0("T", seq_len(ncol(theta)), "_", top_marker_genes)
  colnames(theta) <- gsub("[/_\\\\]", "-", colnames(theta))

  write_mirai_log(log_file, "Termine.", 5, 5)
  data.frame(id = rownames(theta), theta, row.names = NULL, check.names = FALSE)
}