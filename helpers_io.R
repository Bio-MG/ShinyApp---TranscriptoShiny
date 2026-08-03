# =============================================================================
# helpers_io.R — Multi-format data loading + gene-ID / metadata mapping
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure I/O and
# identifier-mapping helpers shared across the Import modules
# (mod_import_sc.R, mod_import_bulk.R, mod_import_spatial.R). No Shiny
# reactivity here: every function below is a plain R function operating on
# paths/data.frames, callable and testable outside the app.
#
# Contents:
#   - Spatial      : is_visium_hd_flat_dir(), is_visium_hd_dir(),
#                     get_visium_import_mode(), list_visium_hd_bin_sizes(),
#                     load_spatial_visium(), load_spatial_visium_hd(),
#                     prepare_spatial_object()
#   - Bulk metadata: infer_metadata_from_names(), preview_metadata_split(),
#                    parse_geo_series_matrix()
#   - Gene IDs     : detect_gene_id_type(), detect_organism_from_ids(),
#                    remap_gene_ids_to_symbol()
#
# NOTE: load_single_cell_data() / prepare_seurat_object() (single-cell
# import, used by mod_import_sc.R AND by mod_spatial_deconv.R's reference
# loader) are NOT in this file — they live in helpers_sc.R. Both files must
# be sourced (a) on the main Shiny process (app.R already does this) AND
# (b) inside every mirai daemon that calls them — see
# R/utils_spatial_async.R's `source_files` argument to init_spatial_daemons().
# Historically only utils_spatial_io.R/_multi.R/_niche.R were preloaded into
# daemons, which is why deconvolution (RCTD/Label Transfer, both of which
# call load_single_cell_data()/prepare_seurat_object() from inside the
# daemon to re-read the reference) failed with "could not find function" —
# see mod_spatial_deconv.R's own changelog for the full diagnosis.
#
# =============================================================================
# REWRITE NOTICE (this version): the previous revision of this file had
# is_visium_hd_dir(), list_visium_hd_bin_sizes() and load_spatial_visium_hd()
# EACH DEFINED TWICE — a correct/robust version near the top, silently
# SHADOWED by an older, incomplete copy-pasted lower in the same file (R
# keeps only the LAST definition of a name when a file is source()'d
# top-to-bottom). The active code in production was therefore always the
# inferior duplicate: is_visium_hd_dir() no longer detected the "flat"
# Visium HD layout at all, and load_spatial_visium_hd() had no
# flat-HD handling and no 'arrow' guard. This is why prior targeted fixes
# "worked in the diff" but were never observed in production — they were
# overwritten a few hundred lines later in the very same file. This
# rewrite keeps exactly ONE definition of every function.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# =============================================================================
# Spatial helpers — robust Visium / Visium HD import
# =============================================================================

#' Detect flat/feature-slice Visium HD exports
#'
#' Some 10x Visium HD datasets are distributed as a single root .h5 file
#' (10x's "feature slice" / imaging export, e.g. "*_spatial.h5") plus a
#' spatial/ folder, without binned_outputs/. Confirmed (community reports,
#' e.g. satijalab/seurat#9649) that this .h5 uses a DIFFERENT internal
#' schema than the spot/bin count matrices Seurat::Load10X_Spatial() reads
#' — it is not a partially-supported edge case, it is simply not the right
#' kind of file. See .hd_flat_unsupported_message() below.
#'
#' @param visium_dir Character path.
#' @return Logical.
is_visium_hd_flat_dir <- function(visium_dir) {
  if (!dir.exists(visium_dir)) return(FALSE)
  h5_files <- list.files(visium_dir, pattern = "\\.h5$", full.names = FALSE, ignore.case = TRUE)
  has_spatial <- dir.exists(file.path(visium_dir, "spatial"))
  if (!has_spatial || length(h5_files) == 0) return(FALSE)
  any(grepl("visium[_-]?hd", h5_files, ignore.case = TRUE)) ||
    any(grepl("_spatial\\.h5$", h5_files, ignore.case = TRUE)) ||
    any(grepl("feature[_-]?slice", h5_files, ignore.case = TRUE))
}

#' Detect whether the selected directory looks like any Visium HD layout
#' (binned OR flat/feature-slice).
#'
#' @param visium_dir Character path.
#' @return Logical.
is_visium_hd_dir <- function(visium_dir) {
  dir.exists(file.path(visium_dir, "binned_outputs")) || is_visium_hd_flat_dir(visium_dir)
}

#' Return the Visium import mode — SINGLE source of truth used by both the
#' UI (mod_import_spatial.R) and the loaders below.
#'
#' @param visium_dir Character path.
#' @return One of "visium", "visium_hd_binned", "visium_hd_flat".
get_visium_import_mode <- function(visium_dir) {
  if (dir.exists(file.path(visium_dir, "binned_outputs"))) return("visium_hd_binned")
  if (is_visium_hd_flat_dir(visium_dir)) return("visium_hd_flat")
  "visium"
}

#' List available Visium HD bin sizes (um) that contain at least one .h5
#'
#' 2um is intentionally never returned here (RAM safety on a 32 Go CPU-only
#' workstation) — see load_spatial_visium_hd()'s own guard.
#'
#' @param visium_dir Character path to the Visium HD root folder.
#' @return Integer vector.
list_visium_hd_bin_sizes <- function(visium_dir) {
  binned_dir <- file.path(visium_dir, "binned_outputs")
  if (!dir.exists(binned_dir)) return(integer(0))
  bin_dirs <- list.dirs(binned_dir, full.names = FALSE, recursive = FALSE)
  bin_dirs <- bin_dirs[grepl("^square_[0-9]+um$", bin_dirs)]
  sizes <- as.integer(sub("^square_0*([0-9]+)um$", "\\1", bin_dirs))
  has_h5 <- vapply(
    bin_dirs,
    function(d) length(list.files(file.path(binned_dir, d), pattern = "\\.h5$", recursive = TRUE)) > 0L,
    logical(1)
  )
  sort(unique(sizes[has_h5 & sizes %in% c(8L, 16L)]))
}

#' Return the first matching QC column in meta.data
#'
#' @param obj Seurat object.
#' @param prefix Prefix to match, e.g. "nCount_" or "nFeature_".
#' @return Character scalar or NA_character_.
get_first_qc_col <- function(obj, prefix) {
  cols <- grep(paste0("^", prefix), colnames(obj@meta.data), value = TRUE)
  if (length(cols) == 0) return(NA_character_)
  cols[1]
}

#' Validate that a loaded object is a Seurat object
#'
#' @param obj Any object returned by a loader.
#' @param context Character, error context for the message.
#' @return The same object if valid.
assert_seurat_object <- function(obj, context = "import spatial") {
  if (is.null(obj) || inherits(obj, "error") || !inherits(obj, "Seurat")) {
    stop(sprintf("Echec %s : l'objet charge n'est pas un objet Seurat valide.", context), call. = FALSE)
  }
  obj
}

#' User-facing message for a flat/feature-slice Visium HD export
#'
#' Single string reused by both loaders below AND mod_import_spatial.R's
#' pre-import banner — one place to edit if the wording ever needs updating.
.hd_flat_unsupported_message <- function() {
  paste0(
    "Ce fichier .h5 utilise un schema interne (\"feature slice\"/imagerie) non pris en charge ",
    "par cette application ni par Seurat::Load10X_Spatial() — ce n'est pas une matrice de comptage ",
    "par spot/bin, meme si le nom du fichier ressemble a un export Visium HD standard. ",
    "Reexportez au format Space Ranger standard : un dossier 'outs/' contenant soit ",
    "'binned_outputs/square_008um|016um/' (Visium HD), soit 'filtered_feature_bc_matrix.h5' + ",
    "'spatial/' (Visium classique)."
  )
}

# ── Arrow-free HD-binned fallback ───────────────────────────────────────────

#' Read a Visium HD tissue_positions file (CSV preferred, Parquet fallback)
#'
#' Space Ranger's own HD output usually ships tissue_positions.parquet under
#' spatial/, which Seurat's own loader reads via `arrow::read_parquet()` —
#' 'arrow' has no CRAN binary for every Windows/R combination and can fail
#' to compile from source without a working Rtools toolchain. Some HD
#' exports also ship a CSV sibling (tissue_positions.csv /
#' tissue_positions_list.csv) — read that first, zero extra dependency. If
#' only the Parquet file exists, try 'arrow' then the much lighter
#' 'nanoparquet' (CRAN, no Arrow C++ toolchain, single small compiled
#' package — generally easier to obtain a Windows binary for).
#'
#' @param spatial_dir Character path to a .../spatial/ folder.
#' @return data.frame, lower-case column names.
read_hd_tissue_positions <- function(spatial_dir) {
  csv_candidates <- list.files(spatial_dir, pattern = "^tissue_positions(_list)?\\.csv$",
                                full.names = TRUE, ignore.case = TRUE)
  if (length(csv_candidates) > 0) {
    df <- utils::read.csv(csv_candidates[1], header = TRUE, stringsAsFactors = FALSE)
    colnames(df) <- tolower(colnames(df))
    return(df)
  }

  parquet_candidates <- list.files(spatial_dir, pattern = "^tissue_positions\\.parquet$",
                                    full.names = TRUE, ignore.case = TRUE)
  if (length(parquet_candidates) == 0) {
    stop("Ni tissue_positions.csv/tissue_positions_list.csv ni tissue_positions.parquet ",
         "trouve dans ", spatial_dir, ".", call. = FALSE)
  }

  if (requireNamespace("arrow", quietly = TRUE)) {
    df <- as.data.frame(arrow::read_parquet(parquet_candidates[1]))
  } else if (requireNamespace("nanoparquet", quietly = TRUE)) {
    df <- nanoparquet::read_parquet(parquet_candidates[1])
  } else {
    stop(
      "Aucun lecteur Parquet disponible (ni 'arrow', ni 'nanoparquet') pour lire ",
      basename(parquet_candidates[1]), ". Installez l'un des deux : ",
      "install.packages('nanoparquet') (leger, pas de dependance C++ Arrow, binaires CRAN ",
      "generalement disponibles sous Windows) ou ",
      "install.packages('arrow', repos = c('https://apache.r-universe.dev', getOption('repos'))) ",
      "si aucun binaire CRAN standard n'existe pour votre version de R.",
      call. = FALSE
    )
  }
  colnames(df) <- tolower(colnames(df))
  df
}

.PNG_SIGNATURE <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))

#' Cheap PNG validity check (magic-byte header only, no image library needed)
.is_valid_png <- function(path) {
  if (!file.exists(path)) return(FALSE)
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  header <- tryCatch(readBin(con, "raw", n = 8L), error = function(e) raw(0))
  length(header) == 8L && identical(header, .PNG_SIGNATURE)
}

#' Repair broken Visium HD tissue-image symlinks before Load10X_Spatial()
#' ever touches them
#'
#' Space Ranger writes `binned_outputs/square_0XXum/spatial/tissue_*_image.png`
#' as SYMLINKS back to the canonical `outs/spatial/` copy. These links have
#' been reported broken/unreadable on real exports (satijalab/seurat
#' issues #9533, #9688 — "file is not in PNG format" from inside
#' Load10X_Spatial()'s own image loader). Best-effort, silent on failure:
#' never blocks import, just leaves the underlying error to surface
#' normally if the repair itself can't help.
#'
#' @param bin_spatial_dir Character path to binned_outputs/square_0XXum/spatial/.
#' @param root_spatial_dir Character path to the root outs/spatial/.
#' @return invisible(TRUE) if anything was repaired, invisible(FALSE) otherwise.
repair_hd_symlinked_images <- function(bin_spatial_dir, root_spatial_dir) {
  if (!dir.exists(bin_spatial_dir) || !dir.exists(root_spatial_dir)) return(invisible(FALSE))
  repaired <- FALSE
  for (fname in c("tissue_lowres_image.png", "tissue_hires_image.png")) {
    bin_file  <- file.path(bin_spatial_dir, fname)
    root_file <- file.path(root_spatial_dir, fname)
    if (file.exists(root_file) && !.is_valid_png(bin_file)) {
      ok <- tryCatch({
        if (file.exists(bin_file)) unlink(bin_file)
        file.copy(root_file, bin_file, overwrite = TRUE)
      }, error = function(e) FALSE)
      if (isTRUE(ok)) repaired <- TRUE
    }
  }
  invisible(repaired)
}

#' Degraded Visium HD binned loader — used ONLY when 'arrow' is unavailable
#'
#' Bypasses Seurat::Load10X_Spatial(bin.size=) entirely (it hard-requires
#' 'arrow' for the .parquet coordinates on most real HD exports) and
#' assembles the object by hand from pieces that need no Arrow C++ install:
#'   - counts   : Seurat::Read10X_h5() (hdf5r — already a hard requirement)
#'   - positions: read_hd_tissue_positions() (CSV first, then arrow/
#'                nanoparquet only if a CSV sibling isn't shipped)
#'
#' Trade-off, stated plainly rather than hidden: the resulting object has
#' NO Seurat image slot (Images(obj) is empty) — this app's own histology
#' overlay does not need it (extract_histology_image() reads PNG/JSON
#' straight off disk from `raw_dir`, see R/utils_spatial_io.R and its
#' relaxed guard for `ts_manual_hd_loader`), but any OTHER code calling
#' Seurat::SpatialFeaturePlot() etc. directly on this object will not have
#' a background image. Coordinates are exposed both as standard metadata
#' columns (coord_x/coord_y, pixel-space) for this app's own
#' get_spatial_coords() fallback, AND the object is fully usable for
#' clustering/Moran/deconvolution (none of which read the image slot).
#'
#' @inheritParams load_spatial_visium_hd
#' @return Seurat object, attr(obj, "ts_manual_hd_loader") == TRUE.
load_spatial_visium_hd_manual <- function(visium_dir, bin_size, sample_name,
                                          min_counts, min_features) {
  bin_dir      <- file.path(visium_dir, "binned_outputs", sprintf("square_%03dum", bin_size))
  spatial_dir  <- file.path(bin_dir, "spatial")
  if (!dir.exists(spatial_dir)) {
    stop("Dossier 'spatial' introuvable sous ", bin_dir, ".", call. = FALSE)
  }

  h5_candidates <- list.files(bin_dir, pattern = "^filtered_feature_bc_matrix\\.h5$",
                               full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (length(h5_candidates) == 0) {
    h5_candidates <- list.files(bin_dir, pattern = "\\.h5$",
                                 full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }
  if (length(h5_candidates) == 0) {
    stop("Aucun fichier .h5 de comptage trouve sous ", bin_dir, ".", call. = FALSE)
  }

  counts <- Seurat::Read10X_h5(h5_candidates[1])
  if (is.list(counts)) counts <- counts[[1]]  # multi-modal h5 safety net

  pos <- read_hd_tissue_positions(spatial_dir)
  needed <- c("barcode", "pxl_row_in_fullres", "pxl_col_in_fullres")
  missing_cols <- setdiff(needed, colnames(pos))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans tissue_positions (", paste(missing_cols, collapse = ", "),
         ") — fichier de positions non standard.", call. = FALSE)
  }
  rownames(pos) <- pos$barcode

  common <- intersect(colnames(counts), pos$barcode)
  if (length(common) < 10) {
    stop("Moins de 10 bins communs entre la matrice de comptage et tissue_positions — ",
         "fichiers incoherents pour ce dossier.", call. = FALSE)
  }
  counts <- counts[, common, drop = FALSE]
  pos    <- pos[common, , drop = FALSE]

  obj <- Seurat::CreateSeuratObject(counts = counts, assay = "Spatial", project = sample_name)
  obj$orig.ident <- sample_name
  # imagecol/imagerow convention (matches get_spatial_coords()'s own naming).
  obj$coord_x <- as.numeric(pos$pxl_col_in_fullres)
  obj$coord_y <- as.numeric(pos$pxl_row_in_fullres)

  if ("in_tissue" %in% colnames(pos)) {
    keep_tissue <- pos$in_tissue %in% c(1, "1", TRUE)
    if (any(keep_tissue)) obj <- subset(obj, cells = colnames(obj)[keep_tissue])
  }

  count_col <- get_first_qc_col(obj, "nCount_")
  feat_col  <- get_first_qc_col(obj, "nFeature_")
  if (!is.na(count_col) && !is.na(feat_col)) {
    keep <- obj@meta.data[[count_col]] >= min_counts & obj@meta.data[[feat_col]] >= min_features
    obj <- subset(obj, cells = colnames(obj)[keep])
  }

  attr(obj, "ts_spatial_mode")     <- "visium_hd_binned"
  attr(obj, "ts_manual_hd_loader") <- TRUE
  obj
}

#' Load a Visium (classic) dataset
#'
#' Linear flow: validate inputs -> resolve import mode (single source of
#' truth: get_visium_import_mode()) -> refuse immediately if flat/unsupported
#' -> delegate to load_spatial_visium_hd() if binned -> otherwise load ->
#' QC filter -> return. One stop() per distinct failure cause.
#'
#' @param visium_dir Character path to the selected directory.
#' @param sample_name Character sample name.
#' @param min_counts Minimum count threshold for spot filtering.
#' @param min_features Minimum feature threshold for spot filtering.
#' @return Seurat object.
load_spatial_visium <- function(visium_dir, sample_name = "Spatial_Sample",
                                min_counts = 100, min_features = 200) {
  visium_dir <- normalizePath(enc2utf8(visium_dir), winslash = "/", mustWork = FALSE)

  if (!dir.exists(visium_dir)) {
    stop("Le dossier specifie n'existe pas : ", visium_dir, call. = FALSE)
  }
  if (!dir.exists(file.path(visium_dir, "spatial"))) {
    stop("Dossier 'spatial' introuvable dans ", visium_dir, call. = FALSE)
  }
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Le package 'hdf5r' est requis pour lire les fichiers 10X .h5.", call. = FALSE)
  }

  import_mode <- get_visium_import_mode(visium_dir)

  # Explicit, immediate refusal — never attempt Load10X_Spatial() on a
  # schema it cannot read (previously: silent fallback into a doomed call,
  # surfacing only as a raw "feature_slices/data does not exist" error).
  if (identical(import_mode, "visium_hd_flat")) {
    stop(.hd_flat_unsupported_message(), call. = FALSE)
  }

  # Routed here on a binned HD folder (e.g. a direct/console call bypassing
  # the UI's own dedicated bin-size selector) -- redirect rather than
  # picking an arbitrary .h5 among several bin resolutions.
  if (identical(import_mode, "visium_hd_binned")) {
    return(load_spatial_visium_hd(visium_dir, sample_name = sample_name,
                                   min_counts = min_counts, min_features = min_features))
  }

  h5_files <- list.files(visium_dir, pattern = "\\.h5$", full.names = FALSE, ignore.case = TRUE)
  h5_filename <- if (length(h5_files) == 0) {
    "filtered_feature_bc_matrix.h5"
  } else if (any(grepl("^filtered_feature_bc_matrix", h5_files, ignore.case = TRUE))) {
    h5_files[grepl("^filtered_feature_bc_matrix", h5_files, ignore.case = TRUE)][1]
  } else if (any(grepl("^raw_feature_bc_matrix", h5_files, ignore.case = TRUE))) {
    warning(
      "Seul un fichier 'raw_feature_bc_matrix.h5' a ete trouve (pas de version filtree) — ",
      "les spots hors-tissu ne seront pas exclus automatiquement.",
      call. = FALSE
    )
    h5_files[grepl("^raw_feature_bc_matrix", h5_files, ignore.case = TRUE)][1]
  } else {
    h5_files[1]
  }

  spatial_obj <- tryCatch({
    Seurat::Load10X_Spatial(
      data.dir = visium_dir, filename = h5_filename,
      assay = "Spatial", slice = sample_name, filter.matrix = TRUE
    )
  }, error = function(e) {
    if (dir.exists(file.path(visium_dir, "filtered_feature_bc_matrix"))) {
      tryCatch(
        Seurat::Load10X_Spatial(data.dir = visium_dir, assay = "Spatial",
                                slice = sample_name, filter.matrix = TRUE),
        error = function(e2) {
          stop("Impossible de charger les donnees Visium (fichier tente : '", h5_filename,
               "') : ", conditionMessage(e2), call. = FALSE)
        }
      )
    } else {
      stop("Impossible de charger les donnees Visium (fichier tente : '", h5_filename,
           "') : ", conditionMessage(e), call. = FALSE)
    }
  })

  spatial_obj <- assert_seurat_object(spatial_obj, context = paste("import", import_mode))
  spatial_obj$orig.ident <- sample_name
  attr(spatial_obj, "ts_spatial_mode") <- import_mode

  count_col <- get_first_qc_col(spatial_obj, "nCount_")
  feat_col  <- get_first_qc_col(spatial_obj, "nFeature_")
  if (!is.na(count_col) && !is.na(feat_col)) {
    keep <- spatial_obj@meta.data[[count_col]] >= min_counts &
      spatial_obj@meta.data[[feat_col]] >= min_features
    spatial_obj <- subset(spatial_obj, cells = colnames(spatial_obj)[keep])
  } else {
    warning("Colonnes QC nCount_/nFeature_ introuvables — filtrage QC ignore.", call. = FALSE)
  }

  spatial_obj
}

#' Load one Visium HD dataset (binned) or refuse a flat/feature-slice export
#'
#' Linear flow: validate inputs -> resolve mode -> refuse flat immediately ->
#' delegate to load_spatial_visium() if not actually HD -> validate bin_size
#' -> repair known-broken tissue-image symlinks (best-effort) -> load
#' (Seurat native if 'arrow' present, degraded manual loader otherwise) ->
#' QC filter (skipped if the manual loader already applied it) -> return.
#'
#' @param visium_dir Character path.
#' @param bin_size Integer, 8 or 16 for binned HD (2um intentionally not
#'   offered — see list_visium_hd_bin_sizes()).
#' @param sample_name Character sample name.
#' @param min_counts Minimum count threshold.
#' @param min_features Minimum feature threshold.
#' @return Seurat object.
load_spatial_visium_hd <- function(visium_dir, bin_size = 8L, sample_name = "Spatial_HD_Sample",
                                   min_counts = 100, min_features = 200) {
  visium_dir <- normalizePath(enc2utf8(visium_dir), winslash = "/", mustWork = FALSE)

  if (!dir.exists(visium_dir)) {
    stop("Le dossier specifie n'existe pas : ", visium_dir, call. = FALSE)
  }
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Le package 'hdf5r' est requis pour lire les fichiers 10X .h5.", call. = FALSE)
  }

  import_mode <- get_visium_import_mode(visium_dir)

  if (identical(import_mode, "visium_hd_flat")) {
    stop(.hd_flat_unsupported_message(), call. = FALSE)
  }

  if (!identical(import_mode, "visium_hd_binned")) {
    return(load_spatial_visium(visium_dir, sample_name = sample_name,
                               min_counts = min_counts, min_features = min_features))
  }

  if (!bin_size %in% c(8L, 16L)) {
    stop("bin_size doit etre 8 ou 16 (2um non propose : trop volumineux pour 32 Go CPU-only).",
         call. = FALSE)
  }

  available <- list_visium_hd_bin_sizes(visium_dir)
  if (!bin_size %in% available) {
    stop(sprintf(
      "Bin %dum introuvable sous binned_outputs/ (detectes : %s).",
      bin_size, if (length(available)) paste(available, collapse = ", ") else "aucun"
    ), call. = FALSE)
  }

  bin_dir          <- file.path(visium_dir, "binned_outputs", sprintf("square_%03dum", bin_size))
  bin_spatial_dir  <- file.path(bin_dir, "spatial")
  root_spatial_dir <- file.path(visium_dir, "spatial")
  # Best-effort, silent: see repair_hd_symlinked_images() header. A wrapping
  # tryCatch() guarantees this NEVER blocks the import even if e.g. file
  # permissions prevent the copy.
  tryCatch(repair_hd_symlinked_images(bin_spatial_dir, root_spatial_dir), error = function(e) NULL)

  has_arrow <- requireNamespace("arrow", quietly = TRUE)

  spatial_obj <- if (has_arrow) {
    tryCatch({
      Seurat::Load10X_Spatial(
        data.dir = visium_dir, bin.size = bin_size,
        assay = "Spatial", slice = sample_name, filter.matrix = TRUE
      )
    }, error = function(e) {
      stop("Impossible de charger les donnees Visium HD (bin ", bin_size, "um) : ",
           conditionMessage(e), call. = FALSE)
    })
  } else {
    message(sprintf(
      "[spatial] Package 'arrow' absent -- lecteur HD binned degrade utilise pour le bin %dum ",
      bin_size
    ), "(coordonnees + comptages uniquement ; le fond histologique reste disponible via le ",
    "dossier racine 'spatial/', voir R/utils_spatial_io.R).")
    tryCatch({
      load_spatial_visium_hd_manual(visium_dir = visium_dir, bin_size = bin_size,
                                    sample_name = sample_name,
                                    min_counts = min_counts, min_features = min_features)
    }, error = function(e) {
      stop(
        "Impossible de charger les donnees Visium HD (bin ", bin_size, "um) sans le package ",
        "'arrow' : ", conditionMessage(e), " Installez 'arrow' (install.packages('arrow'), ou ",
        "install.packages('arrow', repos = c('https://apache.r-universe.dev', getOption('repos'))) ",
        "si aucun binaire CRAN n'existe pour votre version de R) ou l'alternative legere ",
        "'nanoparquet' (install.packages('nanoparquet')) pour activer le chargement complet.",
        call. = FALSE
      )
    })
  }

  spatial_obj <- assert_seurat_object(spatial_obj, context = "import Visium HD")
  spatial_obj$orig.ident <- sample_name
  attr(spatial_obj, "ts_spatial_mode") <- "visium_hd_binned"

  # The manual loader already applied min_counts/min_features QC itself
  # (and sets ts_manual_hd_loader) -- do not re-filter an already-filtered
  # object, and do not clobber that attribute.
  if (!isTRUE(attr(spatial_obj, "ts_manual_hd_loader"))) {
    count_col <- get_first_qc_col(spatial_obj, "nCount_")
    feat_col  <- get_first_qc_col(spatial_obj, "nFeature_")
    if (!is.na(count_col) && !is.na(feat_col)) {
      keep <- spatial_obj@meta.data[[count_col]] >= min_counts &
        spatial_obj@meta.data[[feat_col]] >= min_features
      spatial_obj <- subset(spatial_obj, cells = colnames(spatial_obj)[keep])
    } else {
      warning("Colonnes QC nCount_/nFeature_ introuvables — filtrage QC ignore.", call. = FALSE)
    }
  }

  spatial_obj
}

#' Prepare a spatial Seurat object for downstream use
#'
#' @param obj A Seurat object.
#' @return A Seurat object.
prepare_spatial_object <- function(obj) {
  if (!inherits(obj, "Seurat")) {
    stop("L'objet doit etre un objet Seurat.", call. = FALSE)
  }
  if (!"Spatial" %in% names(obj@assays) && length(obj@images) == 0) {
    warning("L'objet ne contient pas de donnees spatiales detectees.", call. = FALSE)
  }
  if (grepl("^5", packageVersion("Seurat"))) {
    tryCatch({ obj <- Seurat::JoinLayers(obj) }, error = function(e) NULL)
  }
  obj
}

# =============================================================================
# Bulk / GEO metadata helpers (unchanged)
# =============================================================================

#' Infer sample-level metadata by splitting sample names on a delimiter
#'
#' GEO almost never ships clean tabular metadata — but sample names usually
#' encode the experimental design (e.g. "MW1_cornea_mock_1"). This replaces
#' manual/LLM-assisted metadata reconstruction with a deterministic,
#' inspectable split.
#'
#' @param sample_names Character vector of sample/column names from the counts matrix.
#' @param delimiter Regex delimiter to split on (default underscore or hyphen).
#' @param col_names Character vector naming each resulting segment. Length must match
#'   the number of segments produced by the split (after trimming to the minimum
#'   segment count across all samples, to handle slightly irregular naming).
#' @return data.frame, rownames = sample_names, one column per segment.
infer_metadata_from_names <- function(sample_names, delimiter = "[_-]", col_names = NULL) {
  segments <- strsplit(sample_names, delimiter)
  n_seg <- sapply(segments, length)

  if (length(unique(n_seg)) > 1) {
    min_n <- min(n_seg)
    warning(sprintf(
      "Nombre de segments inégal entre échantillons (min=%d, max=%d). Troncature à %d segments — vérifiez le résultat.",
      min_n, max(n_seg), min_n
    ))
    segments <- lapply(segments, function(s) head(s, min_n))
    n_seg <- min_n
  } else {
    n_seg <- n_seg[1]
  }

  mat <- do.call(rbind, segments)
  mat <- as.data.frame(mat, stringsAsFactors = FALSE)

  if (is.null(col_names)) {
    col_names <- paste0("segment_", seq_len(n_seg))
  } else if (length(col_names) != n_seg) {
    stop(sprintf("col_names doit avoir %d éléments (segments détectés), reçu %d.", n_seg, length(col_names)))
  }
  colnames(mat) <- col_names
  rownames(mat) <- sample_names

  # Auto-detect purely numeric segments (e.g. replicate number) and convert
  for (cn in colnames(mat)) {
    if (all(grepl("^[0-9]+$", mat[[cn]]))) mat[[cn]] <- as.integer(mat[[cn]])
  }
  mat
}

#' Preview a metadata-from-names split without committing — feeds the live UI preview
#'
#' @param sample_names Character vector of sample names.
#' @param delimiter Regex delimiter.
#' @param n_preview Number of samples to preview.
#' @return List: segments (list of character vectors), n_seg_consistent (logical), n_seg (integer vector).
preview_metadata_split <- function(sample_names, delimiter = "[_-]", n_preview = 5) {
  segments <- strsplit(head(sample_names, n_preview), delimiter)
  n_seg    <- sapply(segments, length)
  list(segments = segments, n_seg_consistent = length(unique(n_seg)) == 1, n_seg = n_seg)
}

#' Detect the likely gene ID type from a vector of row identifiers
#'
#' @param gene_ids Character vector (rownames of the counts matrix).
#' @return One of "symbol", "ensembl", "entrez", "affy_probe", "unknown".
detect_gene_id_type <- function(gene_ids) {
  sample_ids <- trimws(head(gene_ids[!is.na(gene_ids)], 50))
  if (length(sample_ids) == 0) return("unknown")

  pct_match <- function(pattern) mean(grepl(pattern, sample_ids))

  if (pct_match("^ENSG[0-9]{11}") > 0.7 || pct_match("^ENSMUSG[0-9]{11}") > 0.7) return("ensembl")
  if (pct_match("^[0-9]+$") > 0.7) return("entrez")
  if (pct_match("^[0-9]+(_[a-z]_)?_at$") > 0.5) return("affy_probe")
  if (pct_match("^[A-Za-z0-9.-]+$") > 0.7 && pct_match("^[0-9]+$") < 0.3) return("symbol")
  "unknown"
}

#' Detect species (human/mouse) from Ensembl gene ID prefixes
#'
#' Step-3.8A: used to auto-pick the correct org.*.eg.db package instead of
#' relying on the currently-selected reference/UI default (which defaults to
#' "human" and silently breaks mouse ENSMUSG... datasets).
#'
#' @param gene_ids Character vector (rownames of the counts/expression matrix).
#' @return "human", "mouse", or NA_character_ (ambiguous/unknown).
#'
#' Step-3.8B fix: previously returned the STRING "unknown" for the ambiguous
#' case. Every call site checks `is.na(detected_org)` (mod_sc_annotation.R,
#' mod_sc.R, remap_seurat_ids_to_symbol()) -- is.na("unknown") is FALSE, so
#' "unknown" silently passed every one of those checks as if it were a real
#' detected organism, defeating the whole auto-detect mechanism. Root cause of
#' the real-world mouse dataset defaulting to organism="human".
detect_organism_from_ids <- function(gene_ids) {
  gene_ids <- head(stats::na.omit(as.character(gene_ids)), 200)
  if (length(gene_ids) == 0L) return(NA_character_)

  gene_ids <- trimws(gene_ids)
  pct_mouse <- mean(grepl("^ENSMUSG[0-9]+", gene_ids))
  pct_human <- mean(grepl("^ENSG[0-9]+", gene_ids))

  if (pct_mouse >= 0.50) return("mouse")
  if (pct_human >= 0.50) return("human")
  NA_character_
}

#' Parse a GEO "series_matrix.txt" file into a clean sample x variable metadata table
#'
#' GEO series_matrix.txt files (downloaded directly from a GSE page, no GEOquery/
#' internet access needed) embed per-sample metadata as repeated
#' "!Sample_characteristics_chN" tab-separated lines (one line per characteristic,
#' one quoted "key: value" cell per sample) plus a "!Sample_geo_accession" line
#' giving the GSM IDs in the same column order as the expression/counts matrix.
#' This replaces manual/LLM-assisted extraction (see metadata_GSE52778.csv-style
#' files) with a deterministic local parse — biologist drops the raw file GEO
#' gives them, no curation step required.
#'
#' @param filepath Path to a *_series_matrix.txt file.
#' @return data.frame, one row per sample (rownames = GSM accession), one column
#'   per "!Sample_characteristics_chN" key (+ "title" if present).
parse_geo_series_matrix <- function(filepath) {
  raw_lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)

  split_quoted_tsv <- function(line) {
    parts <- strsplit(line, "\t")[[1]][-1]            # drop the "!Sample_xxx" tag itself
    trimws(gsub('^"|"$', "", parts))
  }

  geo_line   <- raw_lines[startsWith(raw_lines, "!Sample_geo_accession")]
  title_line <- raw_lines[startsWith(raw_lines, "!Sample_title")]
  char_lines <- raw_lines[grepl("^!Sample_characteristics_ch[0-9]+", raw_lines)]

  if (length(geo_line) == 0) {
    stop(paste0(
      "Pas une ligne '!Sample_geo_accession' trouvee. Ce fichier n'est probablement ",
      "pas un series_matrix.txt GEO valide (ou c'est en fait la matrice de counts — ",
      "elle s'importe via 'Option B/C', pas ici)."
    ))
  }

  geo_acc <- split_quoted_tsv(geo_line[1])
  n_samples <- length(geo_acc)
  if (n_samples == 0) stop("Aucun echantillon (GSM) trouve dans ce series_matrix.txt.")

  meta <- data.frame(row.names = geo_acc)

  if (length(title_line) > 0) {
    meta$title <- split_quoted_tsv(title_line[1])[seq_len(n_samples)]
  }

  used_keys <- character(0)
  for (ln in char_lines) {
    vals <- split_quoted_tsv(ln)[seq_len(n_samples)]
    has_kv <- grepl(":", vals)
    key <- if (any(has_kv)) trimws(sub(":.*$", "", vals[has_kv][1])) else "characteristic"
    vals_clean <- ifelse(has_kv, trimws(sub("^[^:]*:", "", vals)), vals)

    key <- make.unique(c(used_keys, key))[length(used_keys) + 1]
    used_keys <- c(used_keys, key)
    meta[[key]] <- vals_clean
  }

  meta
}

#' Convert counts matrix row identifiers to gene symbols
#'
#' @param counts_matrix Matrix, genes in rows (any supported ID type).
#' @param from_type One of "ensembl", "entrez", "affy_probe" (output of detect_gene_id_type()).
#' @param organism "human" or "mouse".
#' @param collapse_method How to merge counts when multiple original IDs map to the
#'   same symbol: "sum" (recommended for counts) or "max_mean" (keep the ID with the
#'   highest mean expression, discard the rest — useful for probe-level redundancy).
#' @return List: matrix (remapped, deduplicated), n_mapped, n_unmapped, n_collapsed.
remap_gene_ids_to_symbol <- function(counts_matrix, from_type, organism = "human",
                                     collapse_method = "sum") {
  if (!from_type %in% c("ensembl", "entrez", "affy_probe")) {
    stop("from_type doit être 'ensembl', 'entrez' ou 'affy_probe'.")
  }

  orgdb <- if (organism == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop("Package 'org.Hs.eg.db' requis.")
    org.Hs.eg.db::org.Hs.eg.db
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop("Package 'org.Mm.eg.db' requis.")
    org.Mm.eg.db::org.Mm.eg.db
  }

  from_key <- switch(from_type,
                     ensembl    = "ENSEMBL",
                     entrez     = "ENTREZID",
                     affy_probe = if (organism == "human") "PROBEID" else stop("Mapping de probes Affymetrix non supporté pour la souris dans ce module — fournissez un fichier d'annotation de plateforme dédié.")
  )

  ids_clean <- rownames(counts_matrix)

  # ── Pre-flight sanity check ────────────────────────────────────────────────
  # AnnotationDbi::select() throws a low-level, untranslated error ("None of
  # the keys entered are valid keys for 'ENSEMBL'") when NONE of the supplied
  # identifiers look anything like the chosen from_type — typically because
  # the data isn't gene-level at all (e.g. "eye_count"-style custom row IDs),
  # or the wrong from_type was left selected. Catch this BEFORE the call with
  # an actionable French message instead of the raw Bioconductor text.
  expected_pattern <- switch(from_type,
                             ensembl    = "^ENS(MUS)?G[0-9]{6,}",
                             entrez     = "^[0-9]+$",
                             affy_probe = "^[0-9]+(_[a-z]_)?_at$"
  )
  sample_ids <- head(ids_clean[!is.na(ids_clean)], 200)
  pct_match  <- if (length(sample_ids) > 0) mean(grepl(expected_pattern, sample_ids)) else 0

  if (pct_match < 0.05) {
    stop(sprintf(
      paste0(
        "Vos identifiants ne ressemblent pas à des ID '%s' (seulement %.0f%% correspondent au format attendu, ex: %s). ",
        "Causes probables : (1) vos données n'utilisent pas d'identifiants de gènes standards — dans ce cas, ",
        "ignorez cette étape facultative ; ou (2) le type source sélectionné ne correspond pas à vos données. ",
        "Exemple d'identifiant trouvé dans vos données : '%s'."
      ),
      from_type, pct_match * 100, expected_pattern,
      if (length(sample_ids) > 0) sample_ids[1] else "?"
    ), call. = FALSE)
  }

  map_df <- tryCatch({
    AnnotationDbi::select(orgdb, keys = ids_clean, keytype = from_key, columns = "SYMBOL")
  }, error = function(e) {
    if (grepl("not valid keys|None of the keys", conditionMessage(e), ignore.case = TRUE)) {
      stop(sprintf(
        paste0(
          "Aucun de vos identifiants n'est reconnu comme '%s' chez l'organisme '%s'. ",
          "Causes probables : (1) l'organisme sélectionné ne correspond pas à vos données — essayez l'autre option ; ",
          "ou (2) vos données n'utilisent pas d'identifiants de gènes standards, auquel cas ignorez cette étape facultative."
        ),
        from_type, organism
      ), call. = FALSE)
    }
    stop("Échec du mapping d'identifiants : ", conditionMessage(e),
         ". Vérifiez que l'organisme sélectionné correspond bien à vos données.", call. = FALSE)
  })

  map_df <- map_df[!is.na(map_df$SYMBOL), ]
  map_df <- map_df[!duplicated(map_df[[from_key]]), ]  # one symbol per original ID
  id_to_symbol <- setNames(map_df$SYMBOL, map_df[[from_key]])

  mapped_symbols <- id_to_symbol[ids_clean]
  n_unmapped <- sum(is.na(mapped_symbols))
  n_mapped   <- sum(!is.na(mapped_symbols))

  keep <- !is.na(mapped_symbols)
  mat  <- counts_matrix[keep, , drop = FALSE]
  syms <- mapped_symbols[keep]

  n_before_collapse <- nrow(mat)
  if (collapse_method == "sum") {
    # rowsum() is vastly faster than aggregate() on large matrices (no
    # data.frame round-trip) — same fix pattern as smart_read()'s duplicate
    # GeneID collapse in mod_import_bulk.R.
    mat_num <- mat
    suppressWarnings(mode(mat_num) <- "numeric")
    mat <- rowsum(mat_num, group = syms, reorder = FALSE, na.rm = TRUE)
  } else {
    # max_mean: keep highest-expressed probe/ID per symbol, discard the rest
    mean_expr <- rowMeans(mat)
    ord <- order(syms, -mean_expr)
    mat <- mat[ord, , drop = FALSE]
    syms_ord <- syms[ord]
    keep_first <- !duplicated(syms_ord)
    mat <- mat[keep_first, , drop = FALSE]
    rownames(mat) <- syms_ord[keep_first]
  }

  list(
    matrix      = mat,
    n_mapped    = n_mapped,
    n_unmapped  = n_unmapped,
    n_collapsed = n_before_collapse - nrow(mat)
  )
}
