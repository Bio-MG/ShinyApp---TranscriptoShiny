# =============================================================================
# R/utils_spatial_reference.R — scRNA-seq reference I/O for spatial deconvolution
# =============================================================================
# NEW (Chantier 3 — architecture fix). Root cause being fixed: the spatial
# deconvolution reference pipeline (mod_spatial_deconv.R, RCTD + Label
# Transfer) was written assuming two functions --
# load_single_cell_data() / prepare_seurat_object() -- existed in
# helpers_sc.R and could be preloaded into the mirai daemon pool. Neither
# function is actually defined in this codebase's helpers_sc.R. This is not
# just a missing-preload bug: relying on a daemon being able to re-parse an
# arbitrary raw reference file (.rds/.h5ad/.h5/.loom, needing schard/
# SeuratDisk/hdf5r) is fragile by design, regardless of which file sources
# which function.
#
# Fix, in two independent pieces:
#   1. read_reference_scrna() / prepare_reference_seurat() -- a small,
#      self-contained multi-format reader. Runs ONLY on the main Shiny
#      process (called from mod_spatial_deconv.R's run_reference_pipeline()).
#      Never sourced into a mirai daemon.
#   2. prepare_reference_artifact() -- once the user has picked the
#      "cell type" column, this writes a SELF-CONTAINED on-disk artifact
#      (raw counts + cell-type labels only) that a daemon can load with
#      NOTHING but base R + BPCells (see the `.load_reference_artifact()`
#      closure inlined directly inside mod_spatial_deconv.R's mirai body --
#      deliberately NOT a project helper function either, so there is
#      nothing left for a future refactor to silently break).
#
# Net effect: multi-format parsing happens EXACTLY ONCE, in the main
# process, at upload/recheck time. Every downstream deconvolution run reads
# the same small prepared artifact -- no schard/SeuratDisk/hdf5r, and no
# project helper function, is ever required inside a daemon again.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# 1. Multi-format reference reader (main process only)
# -----------------------------------------------------------------------------

#' Read a single-cell reference file (.rds / .h5ad / .h5 / .loom)
#'
#' Self-contained: does not depend on any single-cell import helper from
#' another module. Dispatches purely on file extension.
#'
#' @param path Character path to the staged reference file (already copied
#'   under its original extension by mod_spatial_deconv.R's upload handler).
#' @return A Seurat object, OR list(counts=, meta=) for prepare_reference_seurat()
#'   to normalize into one.
read_reference_scrna <- function(path) {
  if (!file.exists(path)) stop("Fichier de reference introuvable : ", path)
  ext <- tolower(tools::file_ext(path))

  if (identical(ext, "rds")) {
    obj <- readRDS(path)
    if (inherits(obj, "Seurat")) return(obj)
    if (is.list(obj) && !is.null(obj$counts)) {
      return(list(counts = obj$counts, meta = obj$meta %||% obj$metadata %||% NULL))
    }
    if (methods::is(obj, "Matrix") || is.matrix(obj)) {
      return(list(counts = obj, meta = NULL))
    }
    stop(".rds ne contient ni objet Seurat, ni liste counts/meta, ni matrice exploitable.")
  }

  if (identical(ext, "h5ad")) {
    if (requireNamespace("schard", quietly = TRUE)) {
      return(schard::h5ad2seurat(path))
    }
    if (requireNamespace("SeuratDisk", quietly = TRUE)) {
      h5seurat <- sub("\\.h5ad$", ".h5seurat", path, ignore.case = TRUE)
      SeuratDisk::Convert(path, dest = h5seurat, overwrite = TRUE, verbose = FALSE)
      return(SeuratDisk::LoadH5Seurat(h5seurat))
    }
    stop("Aucun lecteur .h5ad disponible -- installez 'schard' ",
         "(remotes::install_github('cellgeni/schard'), recommande) ou 'SeuratDisk'.")
  }

  if (identical(ext, "h5")) {
    if (!requireNamespace("hdf5r", quietly = TRUE)) {
      stop("Package 'hdf5r' requis pour lire les fichiers .h5.")
    }
    counts <- Seurat::Read10X_h5(path)
    if (is.list(counts)) counts <- counts[[1]]  # multi-modal h5 safety net
    return(list(counts = counts, meta = NULL))
  }

  if (identical(ext, "loom")) {
    if (requireNamespace("SeuratDisk", quietly = TRUE)) {
      lfile <- SeuratDisk::Connect(filename = path, mode = "r")
      on.exit(tryCatch(lfile$close_all(), error = function(e) NULL), add = TRUE)
      return(Seurat::as.Seurat(lfile))
    }
    stop("Aucun lecteur .loom disponible -- installez 'SeuratDisk' ",
         "(remotes::install_github('mojaveazure/seurat-disk')).")
  }

  stop(sprintf("Format de reference non supporte : '.%s' (attendu : .rds, .h5ad, .h5, .loom).", ext))
}

#' Normalize a read_reference_scrna() result into a clean, single-assay Seurat object
#'
#' @param raw_ref Output of read_reference_scrna() -- Seurat object OR
#'   list(counts=, meta=).
#' @param project_name Character, Seurat project name.
#' @return A Seurat object (raw counts, layers joined if Seurat v5).
prepare_reference_seurat <- function(raw_ref, project_name = "Reference") {
  if (inherits(raw_ref, "Seurat")) {
    obj <- raw_ref
    if (grepl("^5", as.character(utils::packageVersion("Seurat")))) {
      obj <- tryCatch(SeuratObject::JoinLayers(obj), error = function(e) obj)
    }
    return(obj)
  }
  if (is.list(raw_ref) && !is.null(raw_ref$counts)) {
    obj <- Seurat::CreateSeuratObject(counts = raw_ref$counts, project = project_name)
    if (!is.null(raw_ref$meta) && is.data.frame(raw_ref$meta)) {
      common <- intersect(rownames(raw_ref$meta), colnames(obj))
      if (length(common) > 0) {
        for (cn in colnames(raw_ref$meta)) obj@meta.data[common, cn] <- raw_ref$meta[common, cn]
      }
    }
    return(obj)
  }
  stop("Format de reference non reconnu apres lecture (ni objet Seurat, ni liste counts/meta).")
}

# -----------------------------------------------------------------------------
# 2. Self-contained on-disk artifact for the daemon (base R + BPCells only)
# -----------------------------------------------------------------------------

#' Prepare a self-contained on-disk artifact for a scRNA-seq reference
#'
#' Extracts raw counts + a cell-type label vector from an already-parsed
#' Seurat reference (main process) and writes them to disk so a mirai daemon
#' can load them with ONLY base R + BPCells -- no multi-format reader
#' package, no project helper function. Cells with a missing/empty
#' cell-type label are dropped here (previously they were passed through as
#' NA all the way to spacexr::Reference()/TransferData(), which can error or
#' produce a spurious class).
#'
#' @param ref_obj Seurat object (already parsed/validated on the main process).
#' @param celltype_col Character, metadata column holding the cell-type label.
#' @param merge_rare_types Logical, merge types with < min_cells_per_type
#'   cells into "Autre" (mirrors RCTD's own hard minimum).
#' @param min_cells_per_type Integer threshold for the merge above.
#' @param bpcells_threshold Integer cell-count above which counts are written
#'   as an on-disk BPCells matrix instead of an in-memory dgCMatrix RDS
#'   (RAM safety for very large external references).
#' @return list(path = <manifest RDS path>, n_cells=, n_genes=, backend=
#'   "dgCMatrix"|"bpcells", celltype_counts = table).
prepare_reference_artifact <- function(ref_obj, celltype_col, merge_rare_types = TRUE,
                                        min_cells_per_type = 25L, bpcells_threshold = 40000L) {
  if (!inherits(ref_obj, "Seurat")) stop("prepare_reference_artifact() attend un objet Seurat.")
  if (!celltype_col %in% colnames(ref_obj@meta.data)) {
    stop(sprintf("Colonne '%s' absente des metadonnees de la reference.", celltype_col))
  }

  cell_types_raw <- as.character(ref_obj@meta.data[[celltype_col]])
  names(cell_types_raw) <- colnames(ref_obj)
  cell_types_raw[!nzchar(cell_types_raw %||% "")] <- NA_character_

  keep <- !is.na(cell_types_raw)
  if (sum(keep) < 10L) {
    stop("Moins de 10 cellules annotees (non-NA) dans la colonne choisie -- reference inexploitable.")
  }
  cell_types_raw <- cell_types_raw[keep]

  if (isTRUE(merge_rare_types)) {
    tab  <- table(cell_types_raw)
    rare <- names(tab)[tab < min_cells_per_type]
    if (length(rare) > 0) cell_types_raw[cell_types_raw %in% rare] <- "Autre"
  }

  cells_keep <- names(cell_types_raw)
  cell_types <- factor(unname(cell_types_raw))
  names(cell_types) <- cells_keep

  counts <- tryCatch(
    SeuratObject::LayerData(ref_obj, layer = "counts"),
    error = function(e) SeuratObject::GetAssayData(ref_obj, slot = "counts")
  )
  counts <- counts[, cells_keep, drop = FALSE]

  artifact_dir <- file.path(tempdir(),
                             paste0("ts_ref_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                                    sample.int(1e6, 1)))
  dir.create(artifact_dir, showWarnings = FALSE, recursive = TRUE)

  use_bpcells <- ncol(counts) > bpcells_threshold && requireNamespace("BPCells", quietly = TRUE)
  if (use_bpcells) {
    bp_dir <- file.path(artifact_dir, "counts_bpcells")
    BPCells::write_matrix_dir(counts, bp_dir, overwrite = TRUE)
    backend     <- "bpcells"
    counts_path <- bp_dir
  } else {
    counts_path <- file.path(artifact_dir, "counts.rds")
    saveRDS(methods::as(counts, "dgCMatrix"), counts_path, compress = TRUE)
    backend <- "dgCMatrix"
  }

  manifest <- list(
    backend     = backend,
    counts_path = counts_path,
    cell_types  = cell_types,
    genes       = rownames(counts),
    n_cells     = ncol(counts),
    n_genes     = nrow(counts),
    created_at  = Sys.time()
  )
  manifest_path <- file.path(artifact_dir, "manifest.rds")
  saveRDS(manifest, manifest_path)

  list(path = manifest_path, n_cells = manifest$n_cells, n_genes = manifest$n_genes,
       backend = backend, celltype_counts = table(cell_types))
}
