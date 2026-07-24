# =============================================================================
# R/utils_spatial_io.R — BPCells conversion + FOV standardization
# =============================================================================
# Pure functions (no Shiny reactivity). Called once by
# modules/import/mod_import_spatial.R right after a raw object is loaded
# (helpers_io.R::load_spatial_visium() / Seurat::LoadXenium() /
# Seurat::LoadNanostring()). Produces the on-disk BPCells matrix + the
# lightweight list that gets stored in global_data$spatial_obj.
#
# v3 (vignette coverage — Phase 3): build_sketch()/convert_to_bpcells_and_fov()
# gained an optional `norm_method = "sct"` (SCTransform, vignette default) —
# OPT-IN, off by default (norm_method="lognorm" unchanged). Applied only to
# the already-subsampled (<= max_cells) sketch, never the full disk-backed
# dataset, to keep it tractable on a 32GB CPU-only workstation with import
# still synchronous (no mirai — see mod_import_spatial.R header). Every
# downstream reader (gene coloring, Top-SVG grid, sketch PCA+UMAP) already
# reads via DefaultAssay(sketch) without hardcoding an assay name, so no
# other file needed to change for this to work end-to-end.
#
# v2 (vignette parity fix): added histology image capture (extract_histology_image())
# — the previous contract only kept spot coordinates (x,y), never the actual
# H&E/histology image, so the tissue background required by the Seurat
# Spatial Vignette (SpatialDimPlot()/SpatialFeaturePlot() with a visible
# tissue section) was structurally impossible to render downstream. Visium
# only (see function doc) — the lowres image is tiny (~<1MB) so it is kept
# in RAM like `coords`, no BPCells/disk involvement needed for it.
#
# CONTRACT — global_data$spatial_obj is a *list*, NOT a Seurat object:
#   list(
#     sketch      = <Seurat obj, <= max_sketch cells/spots, in-RAM. Default
#                    assay is "sketch" (norm_method="lognorm", default) or
#                    "SCT" (norm_method="sct", opt-in) — see build_sketch()>,
#     bpcells_dir = <character, on-disk BPCells directory (full resolution)>,
#     coords      = <data.frame: id, x, y, fov (full resolution, ~2 numeric
#                    cols only -> cheap to keep in RAM even for millions of
#                    spots/cells)>,
#     histology   = <NULL, or list(raster=<raster matrix, hex colors>,
#                    scale_factors=<list from Seurat::ScaleFactors()>,
#                    dim=c(nrow,ncol)) — Visium only; small lowres H&E image
#                    used by mod_spatial_viz.R to render the tissue
#                    background (vignette parity). NULL for Xenium/CosMx or
#                    if extraction failed — every downstream reader must
#                    treat NULL as "no background available", never error>,
#     technology  = "visium" | "xenium" | "cosmx",
#     n_total     = <integer, full-resolution spot/cell count>,
#     images      = <character vector, Seurat::Images() names>,
#     project     = <character, dataset id>,
#     created_at  = <POSIXct>
#   )
# Every module downstream (mod_spatial*.R) must read $sketch / $bpcells_dir
# explicitly — see mod_spatial.R header comment for the migration note.
#
# Depends on: Seurat (>= 5.0, for LayerData<-/CreateAssay5Object/
# GetTissueCoordinates), BPCells. Imaging-only helpers (Simplify) additionally
# need the FOV/Segmentation classes created by LoadXenium()/LoadNanostring().
# extract_histology_image() additionally needs Seurat::GetImage()/
# Seurat::ScaleFactors() — both exported Seurat functions for VisiumV1/V2
# image objects; verify against your installed Seurat version if extraction
# silently returns NULL (it degrades gracefully, see function doc).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Root cache directory for all persisted BPCells matrices
#'
#' Uses tools::R_user_dir() rather than tempdir() so imported datasets
#' survive across R sessions / app restarts (local-first requirement). A
#' saved Shiny session (.rds, see app.R) only stores the *pointer* (this
#' path) — the directory itself must outlive the R process for that pointer
#' to remain valid. Never wiped automatically; add a housekeeping/"vider le
#' cache" action later if disk usage becomes a concern (evolutivity hook).
#'
#' @return Character path, created if missing.
bpcells_cache_root <- function() {
  root <- tools::R_user_dir("TranscriptoShiny", which = "cache")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  root
}

#' Extract the Visium histology (H&E) image as an in-RAM raster + scale factors
#'
#' Only meaningful for Visium (a real tissue section imaged behind a regular
#' spot grid) — Xenium/CosMx do not carry a single background image in the
#' same sense (per-FOV morphology images are a distinct, heavier concept,
#' out of scope here) — returns NULL immediately for those.
#'
#' Deliberately uses the LOW-RES image: it is tiny (typically <1MB, ~600x600
#' px) and is exactly what Seurat's SpatialDimPlot()/SpatialFeaturePlot()
#' use as tissue background by default — no reason to keep the much heavier
#' hi-res image in memory just to draw a background layer under spots.
#'
#' Fully defensive: any failure (missing image slot, Seurat API mismatch,
#' etc.) returns NULL with a warning rather than aborting the whole import —
#' the rest of the spatial_obj contract (sketch/bpcells_dir/coords) must
#' remain usable even without a histology background.
#'
#' @param seurat_obj Seurat object as returned by load_spatial_visium()
#'   (helpers_io.R), BEFORE the counts layer is swapped to BPCells (image
#'   slot is untouched by that swap either way, order does not matter).
#' @param technology One of "visium", "xenium", "cosmx" — no-op (NULL) for
#'   the latter two.
#' @return NULL, or list(raster = <raster matrix, hex colors>,
#'   scale_factors = <list: spot/fiducial/hires/lowres>, dim = c(nrow, ncol)).
extract_histology_image <- function(seurat_obj, technology) {
  if (!identical(technology, "visium")) return(NULL)

  img_name <- tryCatch(Seurat::Images(seurat_obj)[1], error = function(e) NA_character_)
  if (is.na(img_name) || length(img_name) == 0) return(NULL)

  tryCatch({
    img_obj     <- seurat_obj[[img_name]]
    raster_img  <- Seurat::GetImage(img_obj, mode = "raster")   # small (lowres), matrix of hex colors
    scale_facts <- Seurat::ScaleFactors(img_obj)                # list: spot, fiducial, hires, lowres
    list(raster = raster_img, scale_factors = scale_facts, dim = dim(raster_img))
  }, error = function(e) {
    warning("Extraction de l'image histologique echouee (", conditionMessage(e),
            ") — le fond de coupe sera indisponible ; le reste de l'import (spots, ",
            "comptages) n'est pas affecte.")
    NULL
  })
}

#' Convert a raw Seurat spatial object to an on-disk BPCells-backed assay
#'
#' Writes the counts matrix to disk in BPCells' bit-packed format and swaps
#' it in as the assay's "counts" layer — the full matrix is NEVER coerced
#' via as.matrix()/as.data.frame() here. For imaging technologies
#' (Xenium/CosMx) also simplifies segmentation polygons via Seurat::Simplify().
#' Also extracts the histology background image (Visium only, see
#' extract_histology_image()). Finally builds a small in-RAM "sketch" (see
#' build_sketch()) for fast plotting without touching the disk-backed matrix.
#'
#' @param seurat_obj Seurat object as returned by load_spatial_visium()
#'   (helpers_io.R) / Seurat::LoadXenium() / Seurat::LoadNanostring() — raw
#'   counts, not yet normalized.
#' @param dataset_id Character, unique slug for this dataset (sanitized into
#'   the BPCells subdirectory name) — e.g. accession or sample name.
#' @param technology One of "visium", "xenium", "cosmx".
#' @param assay Character, assay holding raw counts (defaults to the
#'   object's current DefaultAssay()).
#' @param simplify_tol Numeric tolerance passed to Seurat::Simplify() for
#'   imaging segmentation polygons (ignored for Visium). Higher = fewer
#'   vertices = faster rendering, coarser boundaries.
#' @param max_sketch Integer, max cells/spots kept in the in-RAM sketch.
#' @param norm_method "lognorm" (default) or "sct" — see build_sketch().
#' @return List — see file header CONTRACT (now includes $histology).
convert_to_bpcells_and_fov <- function(seurat_obj, dataset_id,
                                        technology = c("visium", "xenium", "cosmx"),
                                        assay = NULL,
                                        simplify_tol = 20,
                                        max_sketch = 50000,
                                        norm_method = c("lognorm", "sct")) {
  technology  <- match.arg(technology)
  norm_method <- match.arg(norm_method)
  if (!requireNamespace("BPCells", quietly = TRUE)) {
    stop("Package 'BPCells' requis pour l'import spatial (stockage sur disque). ",
         "Installez via remotes::install_github('bnprks/BPCells/r').")
  }
  if (!inherits(seurat_obj, "Seurat")) stop("seurat_obj doit etre un objet Seurat.")

  assay <- assay %||% Seurat::DefaultAssay(seurat_obj)
  dataset_id <- gsub("[^A-Za-z0-9_-]", "_", dataset_id)
  bpcells_dir <- file.path(bpcells_cache_root(), dataset_id, "counts_bpcells")
  dir.create(dirname(bpcells_dir), showWarnings = FALSE, recursive = TRUE)
  if (dir.exists(bpcells_dir)) unlink(bpcells_dir, recursive = TRUE)  # re-import = overwrite

  # --- 1. Counts -> disk (BPCells). write_matrix_dir() streams from the
  #        source matrix; it does not require the destination to already be
  #        in a particular in-memory format, but the *source* here is
  #        whatever Seurat already holds (dgCMatrix on first import) — this
  #        is the one unavoidable pass over the raw data every pipeline needs
  #        anyway. From this point on nothing downstream forces a copy.
  counts <- SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts")
  BPCells::write_matrix_dir(counts, bpcells_dir, overwrite = TRUE)
  bpcells_mat <- BPCells::open_matrix_dir(bpcells_dir)

  # Swap the layer in place (keeps the existing Assay5 object, its key and
  # any image/FOV associations intact) instead of replacing the whole assay.
  SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts") <- bpcells_mat

  # --- 2. Imaging-only: simplify segmentation polygons for rendering
  if (technology %in% c("xenium", "cosmx")) {
    seurat_obj <- .simplify_all_fovs(seurat_obj, tol = simplify_tol)
  }

  # --- 3. Full-resolution coordinates (small: 2-3 numeric columns) — kept
  #        in RAM at the session level, passed as plain data (not a Seurat
  #        object) to mirai daemons that need spatial neighborhoods (BANKSY).
  coords <- tryCatch({
    ct <- Seurat::GetTissueCoordinates(seurat_obj)
    if (!"cell" %in% colnames(ct)) ct$cell <- rownames(ct) %||% colnames(seurat_obj)
    xy_cols <- intersect(c("x", "y", "imagecol", "imagerow"), colnames(ct))
    data.frame(id = ct$cell,
               x  = ct[[xy_cols[1]]],
               y  = ct[[xy_cols[2]]],
               fov = if ("fov" %in% colnames(ct)) ct$fov else NA_character_,
               row.names = NULL, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("GetTissueCoordinates() a echoue : ", conditionMessage(e),
            " — le clustering spatial (BANKSY) sera indisponible pour ce jeu de donnees.")
    NULL
  })

  # --- 3.5. Histology image (Visium only, vignette parity — see file header
  #        changelog). Small (lowres), kept in RAM alongside coords.
  hist_img <- extract_histology_image(seurat_obj, technology)

  # --- 4. In-RAM sketch for instant plotting / QC previews
  sketch <- build_sketch(seurat_obj, max_cells = max_sketch, assay = assay,
                          norm_method = norm_method)

  list(
    sketch      = sketch,
    bpcells_dir = bpcells_dir,
    coords      = coords,
    histology   = hist_img,
    technology  = technology,
    n_total     = ncol(seurat_obj),
    images      = tryCatch(Seurat::Images(seurat_obj), error = function(e) character(0)),
    project     = dataset_id,
    created_at  = Sys.time()
  )
}

#' Simplify polygon segmentation boundaries on every FOV of an object
#'
#' Verified against SeuratObject source: `Simplify(coords, tol,
#' topologyPreserve = TRUE)` takes a *Segmentation* object as `coords` (NOT
#' the whole FOV) — so this walks each FOV's boundaries (SeuratObject::
#' Boundaries()) and simplifies them one at a time, writing back into the
#' FOV's S4 `boundaries` slot directly (no dedicated FOV boundary setter
#' exists in the package as of this writing). Best-effort: any single boundary that fails
#' is skipped with a warning rather than aborting the whole import.
#'
#' @param obj Seurat object with one or more imaging FOVs (Xenium/CosMx).
#' @param tol Numeric, SeuratObject::Simplify() tolerance (Douglas-Peucker).
#' @return The Seurat object, each FOV's segmentation simplified in place.
.simplify_all_fovs <- function(obj, tol = 20) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' manquant : simplification des polygones ignoree (Simplify() en depend).")
    return(obj)
  }
  fov_names <- tryCatch(Seurat::Images(obj), error = function(e) character(0))
  for (fv in fov_names) {
    fov_obj <- obj[[fv]]
    if (!methods::is(fov_obj, "FOV")) next  # e.g. VisiumV1/V2 images: nothing to simplify

    bnames <- tryCatch(SeuratObject::Boundaries(fov_obj), error = function(e) character(0))
    boundaries_slot <- tryCatch(methods::slot(fov_obj, "boundaries"), error = function(e) NULL)
    if (is.null(boundaries_slot)) next

    for (bn in bnames) {
      seg <- tryCatch(fov_obj[[bn]], error = function(e) NULL)
      if (is.null(seg)) next
      seg_simple <- tryCatch(
        SeuratObject::Simplify(coords = seg, tol = tol),
        error = function(e) {
          warning(sprintf("Simplify() a echoue sur '%s'/'%s' : %s — polygone non simplifie.",
                           fv, bn, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(seg_simple)) next
      idx <- which(names(boundaries_slot) == bn)
      if (length(idx) == 1) boundaries_slot[[idx]] <- seg_simple
    }
    tryCatch({
      methods::slot(fov_obj, "boundaries") <- boundaries_slot
      obj[[fv]] <- fov_obj
    }, error = function(e) {
      warning(sprintf("Impossible d'ecrire les polygones simplifies pour le FOV '%s' : %s",
                       fv, conditionMessage(e)))
    })
  }
  obj
}

#' Crop an object's FOV to a bounding box for zoomed-in rendering
#'
#' Thin, defensive wrapper around SeuratObject::Crop() used by
#' mod_spatial_viz.R when the user zooms past a density threshold and
#' polygon boundaries should be drawn (see spec: "affichage des limites
#' polygonales"). Signature verified against SeuratObject source:
#' `Crop(object, x=NULL, y=NULL, coords=c('plot','tissue'), ...)`.
#'
#' @param obj Seurat object (or a single FOV object).
#' @param fov Character, FOV/image name (ignored if `obj` is already a FOV).
#' @param x,y Numeric length-2 vectors, bounding box in tissue coordinates.
#' @return A cropped FOV object, or NULL on failure (caller should fall back
#'   to the uncropped/scattermore rendering).
crop_fov_bbox <- function(obj, fov, x, y) {
  tryCatch({
    target <- if (inherits(obj, "Seurat")) obj[[fov]] else obj
    SeuratObject::Crop(target, x = x, y = y, coords = "plot")
  }, error = function(e) {
    warning("Crop() a echoue : ", conditionMessage(e))
    NULL
  })
}

#' Build (or rebuild) the in-RAM plotting sketch for a spatial object
#'
#' Primary path: Seurat's own `SketchData(method = "Uniform")` — matches the
#' project spec's "Sketch" terminology exactly (it IS the Seurat v5 sketch-
#' based-analysis feature) and is exercised/maintained upstream rather than
#' reinvented here. Requires a normalized "data" layer, so this runs a
#' (cheap, BPCells-lazy) NormalizeData() first. `subset()` + DietSeurat()
#' afterwards guarantee the returned object's meta.data and assay both cover
#' exactly the sketch cells (SketchData() alone only trims the assay, not
#' top-level meta.data).
#'
#' Falls back to a plain uniform `sample()` + manual Assay5 materialization
#' if SketchData()/NormalizeData() error for any reason (version mismatch,
#' unusual assay layout, etc.) — never blocks the import.
#'
#' @param obj Seurat object (full, BPCells-backed or not).
#' @param max_cells Integer, sketch size cap (spec: 30000-50000).
#' @param assay Character, assay to materialize into RAM for the sketch.
#' @param norm_method "lognorm" (default, `NormalizeData()` — fast, matches
#'   every other pipeline in this app) or "sct" (`SCTransform()` — the
#'   Seurat Spatial Vignette's recommended default, regularized-NB variance
#'   stabilization). SCT is opt-in only: meaningfully heavier per cell than
#'   LogNormalize, and import stays synchronous (no mirai — see
#'   mod_import_spatial.R header), so this cost lands directly on the UI
#'   during import. Deliberately applied AFTER subsampling (bounded to
#'   <= max_cells cells, never the full disk-backed dataset) to keep it
#'   tractable on a 32GB CPU-only workstation. Install Bioconductor's
#'   `glmGamPoi` for a meaningful SCTransform speed-up (optional, auto-used
#'   by Seurat if present — not added to global.R's package list here,
#'   your call). Falls back to LogNormalize with a warning on failure.
#' @return Seurat object, <= max_cells cells/spots, in-RAM Assay5. Default
#'   assay is "sketch" (norm_method="lognorm") or "SCT" (norm_method="sct").
build_sketch <- function(obj, max_cells = 50000, assay = NULL,
                          norm_method = c("lognorm", "sct")) {
  norm_method <- match.arg(norm_method)
  assay <- assay %||% Seurat::DefaultAssay(obj)

  sk <- tryCatch({
    # NOTE: this initial NormalizeData() is ALWAYS LogNormalize regardless of
    # `norm_method` — it exists purely so SketchData() has a "data" layer to
    # subsample from. The user-facing normalization choice is applied AFTER
    # subsampling, below, on the small (<= max_cells) sketch only.
    obj_norm <- Seurat::NormalizeData(obj, assay = assay, verbose = FALSE)
    obj_norm <- Seurat::SketchData(obj_norm, assay = assay, ncells = max_cells,
                                    sketched.assay = "sketch", method = "Uniform",
                                    cast = "dgCMatrix", verbose = FALSE)
    sketch_cells <- SeuratObject::Cells(obj_norm[["sketch"]])
    sk <- subset(obj_norm, cells = sketch_cells)
    sk <- Seurat::DietSeurat(sk, assays = "sketch")  # FIX: DietSeurat lives in Seurat, not SeuratObject
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  }, error = function(e) {
    warning("Seurat::SketchData() a echoue (", conditionMessage(e),
            ") — repli sur un sous-echantillonnage aleatoire simple.")
    n <- ncol(obj)
    idx <- if (n <= max_cells) seq_len(n) else { set.seed(42); sort(sample(seq_len(n), max_cells)) }
    sk <- obj[, idx]  # lazy: BPCells column subset, no full materialization
    sk_counts <- methods::as(SeuratObject::LayerData(sk, assay = assay, layer = "counts"), "dgCMatrix")
    sk[["sketch"]] <- SeuratObject::CreateAssay5Object(counts = sk_counts)
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  })

  # --- User-facing normalization, applied on the small (<= max_cells)
  #     sketch only (see @param norm_method). SCTransform replaces the
  #     "sketch" assay's data/scale.data with its own variance-stabilized
  #     values (Pearson residuals) in a NEW "SCT" assay, which becomes
  #     DefaultAssay — every downstream consumer (gene coloring in
  #     mod_spatial_viz.R, Top-SVG grid in mod_spatial_qc.R, sketch PCA+UMAP)
  #     reads LayerData(sk, layer="data")/rownames(sk) WITHOUT hardcoding an
  #     assay name, so it automatically follows DefaultAssay — no other file
  #     needs to know which normalization was used.
  if (identical(norm_method, "sct")) {
    sk <- tryCatch({
      # SCTransform() parallelizes its per-gene regression via the `future`
      # package WHEN an ambient future::plan() other than "sequential" is
      # active. This app sets plan(multisession) globally in global.R (for
      # other pipelines) — multisession workers are SEPARATE R processes, so
      # everything the worker needs must be serialized first. Because this
      # call happens deep inside a Shiny module-server closure, future's
      # globals-scanner walks the whole enclosing environment chain
      # (reactiveValues, session, other locals) and can sweep several GB of
      # unrelated state into the payload — same class of bug already fixed
      # elsewhere in this app for Banksy/RCTD/STdeconvolve (packages that
      # spawn their own nested/ambient parallelism where none is expected),
      # just triggered here by the *ambient* plan rather than a nested mirai
      # daemon. Force sequential (single-process, in-place) for this call
      # only, then always restore whatever plan was active before.
      run_sct <- function() {
        old_plan <- future::plan()
        on.exit(future::plan(old_plan), add = TRUE)
        future::plan("sequential")
        Seurat::SCTransform(sk, assay = "sketch", new.assay.name = "SCT",
                             variable.features.n = 3000, verbose = FALSE)
      }
      run_sct()
    }, error = function(e) {
      warning("SCTransform() a echoue sur le sketch (", conditionMessage(e),
              ") — repli sur NormalizeData (LogNormalize) standard.")
      Seurat::NormalizeData(sk, assay = "sketch", verbose = FALSE)
    })
    Seurat::DefaultAssay(sk) <- if ("SCT" %in% SeuratObject::Assays(sk)) "SCT" else "sketch"
  }

  # FIX (post-test-2): guarantee a populated "data" layer no matter which
  # path above ran — the fallback never normalizes at all, and SketchData()
  # is not guaranteed to carry every layer through on every version. Every
  # downstream consumer (mod_spatial_viz.R gene-coloring, sketch UMAP) reads
  # layer="data" directly (no assay= — follows DefaultAssay), so this must
  # never be silently empty.
  has_data <- tryCatch({
    d <- SeuratObject::LayerData(sk, layer = "data")
    !is.null(d) && length(d) > 0 && nrow(d) > 0
  }, error = function(e) FALSE)
  if (!has_data) {
    sk <- tryCatch(Seurat::NormalizeData(sk, verbose = FALSE),
                    error = function(e) { warning("Normalisation du sketch echouee : ", conditionMessage(e)); sk })
  }
  sk
}

#' Fast, streamed QC stats directly from the on-disk BPCells matrix
#'
#' No Seurat object is constructed here — colSums()/rowSums() on a BPCells
#' IterableMatrix stream through the file rather than materializing it, so
#' this is safe to run synchronously on the Shiny main thread for typical
#' spatial dataset sizes (spec explicitly reserves mirai/async for the
#' heavier Moran's I step only, not these basic per-spot totals).
#'
#' @param bpcells_dir Character path (global_data$spatial_obj$bpcells_dir).
#' @param mt_pattern Regex for mitochondrial genes (rownames).
#' @param ribo_pattern Regex for ribosomal genes (rownames).
#' @return data.frame(id, nCount, nFeature, pct_mt, pct_ribo).
compute_qc_metrics_fast <- function(bpcells_dir, mt_pattern = "^MT-", ribo_pattern = "^RP[SL]") {
  if (!requireNamespace("BPCells", quietly = TRUE)) stop("Package 'BPCells' requis.")
  mat <- BPCells::open_matrix_dir(bpcells_dir)

  n_count   <- Matrix::colSums(mat)
  n_feature <- Matrix::colSums(mat > 0)

  gene_names <- rownames(mat)
  mt_idx   <- grepl(mt_pattern, gene_names)
  ribo_idx <- grepl(ribo_pattern, gene_names)

  pct_mt   <- if (any(mt_idx))   100 * Matrix::colSums(mat[mt_idx, , drop = FALSE])   / pmax(n_count, 1) else rep(NA_real_, ncol(mat))
  pct_ribo <- if (any(ribo_idx)) 100 * Matrix::colSums(mat[ribo_idx, , drop = FALSE]) / pmax(n_count, 1) else rep(NA_real_, ncol(mat))

  data.frame(
    id        = colnames(mat),
    nCount    = as.numeric(n_count),
    nFeature  = as.numeric(n_feature),
    pct_mt    = as.numeric(pct_mt),
    pct_ribo  = as.numeric(pct_ribo),
    row.names = NULL, stringsAsFactors = FALSE
  )
}
