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
extract_histology_image <- function(seurat_obj, technology, image_resolution = c("lowres", "hires")) {
  if (!identical(technology, "visium")) return(NULL)
  
  image_resolution <- match.arg(image_resolution)
  
  img_name <- tryCatch(Seurat::Images(seurat_obj)[1], error = function(e) NA_character_)
  if (is.na(img_name) || length(img_name) == 0) return(NULL)
  
  tryCatch({
    img_obj <- seurat_obj[[img_name]]
    
    scale_facts <- Seurat::ScaleFactors(img_obj)
    
    # Compatible avec le contrat actuel :
    # - lowres = léger, recommandé en interactif
    # - hires  = possible si disponible, mais plus lourd RAM / export
    raster_img <- Seurat::GetImage(
      img_obj,
      mode = if (identical(image_resolution, "hires")) "plot" else "raster"
    )
    
    if (inherits(raster_img, "ggplot")) {
      warning(
        "GetImage(..., mode='plot') n'a pas renvoyé un raster exploitable directement ; ",
        "repli automatique sur le mode lowres/raster."
      )
      raster_img <- Seurat::GetImage(img_obj, mode = "raster")
      image_resolution <- "lowres"
    }
    
    list(
      raster = raster_img,
      scale_factors = scale_facts,
      dim = dim(raster_img),
      resolution = image_resolution
    )
  }, error = function(e) {
    warning(
      "Extraction de l'image histologique echouee (", conditionMessage(e),
      ") — le fond de coupe sera indisponible ; le reste de l'import (spots, comptages) n'est pas affecte."
    )
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
  #        Uses get_spatial_coords() (name-based column matching + explicit
  #        full-res scale) — see that function's doc for the rotation-bug
  #        root cause this fixes.
  coords <- tryCatch(
    get_spatial_coords(seurat_obj),
    error = function(e) {
      warning("GetTissueCoordinates() a echoue : ", conditionMessage(e),
              " — le clustering spatial (BANKSY) sera indisponible pour ce jeu de donnees.")
      NULL
    }
  )
  if (is.null(coords)) {
    warning("Extraction des coordonnees spatiales impossible (get_spatial_coords() a retourne NULL) — ",
            "le clustering spatial (BANKSY) sera indisponible pour ce jeu de donnees.")
  }

  # --- 3.5. Histology image (Visium only, vignette parity — see file header
  #        changelog). Small (lowres), kept in RAM alongside coords.
  hist_img <- extract_histology_image(seurat_obj, technology)

  # --- 3.6. Defensive regression guard: warns (does not block import) if
  #        spots and image ever drift out of alignment, e.g. after a
  #        Seurat upgrade changes GetTissueCoordinates()'s columns/default
  #        scale again (see get_spatial_coords() doc).
  if (!is.null(coords) && !is.null(hist_img)) {
    check_histology_coord_alignment(coords, hist_img)
  }

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

debug_histology <- function(global_data) {
  hist_data <- global_data$spatial_obj$histology
  cat("=== Histology Debug ===\n")
  cat("histology exists:", !is.null(hist_data), "\n")
  if (!is.null(hist_data)) {
    cat("names:", paste(names(hist_data), collapse=", "), "\n")
    cat("raster class:", class(hist_data$raster)[1], "\n")
    if (is.array(hist_data$raster) || is.matrix(hist_data$raster)) {
      cat("raster dim:", paste(dim(hist_data$raster), collapse=" x "), "\n")
    }
    cat("scale_factors:", paste(names(hist_data$scale_factors), collapse=", "), "\n")
  }
  cat("========================\n")
}

#' Convertir une matrice raster (hex ou SpatRaster) en tableau RGBA (dim: H x W x 4)
#'
#' @param r Matrice 2D (hex character), SpatRaster ou RasterLayer/Brick
#' @return Array 3D RGBA de dimensions (Hauteur x Largeur x 4) avec valeurs [0, 1]
#' Convert a histology raster to an RGBA PNG array
#'
#' R matrices are indexed as [row, column] whereas PNG/Plotly images use
#' [x, y] pixel orientation. The transpose + vertical flip below converts
#' Seurat's Visium raster into the same orientation as tissue coordinates.
#'
#' @param r Matrix of hexadecimal colours, SpatRaster, Raster* object or array.
#' @return Numeric RGBA array in PNG orientation: height x width x 4.
#' Convertir une matrice raster (hex ou SpatRaster) en tableau RGBA (dim: H x W x 4)
#'
#' IMPORTANT:
#' Ne PAS transposer / retourner l'image ici. Le raster doit rester dans son
#' orientation native Seurat, puis être positionné géométriquement dans le
#' repère spatial au moment du rendu :
#' - ggplot2::annotation_raster(..., ymin = -ymax, ymax = -ymin)
#' - Plotly layout(images = ...)
#'
#' Toute rotation/flips ici crée un décalage systématique entre image brute,
#' rendu Plotly et export PNG.
#'
#' @param r Matrice 2D (hex character), SpatRaster ou RasterLayer/Brick
#' @return Array 3D RGBA de dimensions (Hauteur x Largeur x 4) avec valeurs [0, 1]
# =============================================================================
# Correction dans R/utils_spatial_io.R
# =============================================================================

raster_to_rgba_array <- function(r) {
  if (is.null(r)) return(NULL)
  
  # Transposer la matrice 2D pour aligner [imagecol, imagerow] vers [hauteur(Y), largeur(X)]
  # indispensable pour png::writePNG() et le rendu Plotly.
  if (is.matrix(r)) {
    r <- t(r)
  }
  
  # Cas 1 : Matrice 2D de chaînes hexadécimales (Visium Seurat::GetImage)
  if (is.matrix(r) && is.character(r)) {
    hex <- as.vector(r)
    rgb_mat <- grDevices::col2rgb(hex, alpha = FALSE)
    
    arr <- array(1, dim = c(nrow(r), ncol(r), 4L))
    arr[, , 1L] <- matrix(rgb_mat[1L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    arr[, , 2L] <- matrix(rgb_mat[2L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    arr[, , 3L] <- matrix(rgb_mat[3L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    return(arr)
  }
  
  # Cas 2 : Objet terra / SpatRaster
  if (inherits(r, "SpatRaster")) {
    arr <- terra::as.array(r)
    if (length(dim(arr)) == 2L) {
      arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
    }
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  # Cas 3 : Objet raster classique (RasterBrick/Stack/Layer)
  if (inherits(r, c("RasterBrick", "RasterStack", "RasterLayer"))) {
    arr <- raster::as.array(r)
    if (length(dim(arr)) == 2L) {
      arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
    }
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  # Cas 4 : Tableau 3D direct
  if (is.array(r) && length(dim(r)) == 3L) {
    arr <- r
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  stop("Format raster non reconnu : ", class(r)[1L])
}

#' Diagnostic de la qualité et des valeurs du raster d'histologie
diagnose_histology_raster <- function(r) {
  if (is.null(r)) return(NULL)
  arr <- raster_to_rgba_array(r)
  if (is.null(arr)) return(NULL)
  
  rgb_vals <- arr[,, 1:3]
  near_white <- (rgb_vals[,,1] > 0.95 & rgb_vals[,,2] > 0.95 & rgb_vals[,,3] > 0.95)
  pct_white <- mean(near_white) * 100
  
  unique_cols <- if (is.matrix(r) && is.character(r)) length(unique(as.vector(r))) else NA
  
  list(
    pct_near_white = round(pct_white, 2),
    n_unique_colors = unique_cols,
    mean_rgb = round(c(mean(rgb_vals[,,1]), mean(rgb_vals[,,2]), mean(rgb_vals[,,3])), 3)
  )
}

#' Extraction sécurisée des coordonnées spatiales Visium (Seurat v4 et v5)
#'
#' FIX (root cause of the reported 90° rotation): `Seurat::GetTissueCoordinates()`
#' column ORDER/NAMING for VisiumV1 has genuinely differed between Seurat
#' versions (`cols = c('imagerow','imagecol')` vs `c('imagecol','imagerow')`
#' depending on the exact release — verified by diffing two fetches of
#' satijalab/seurat's own source days apart). Reading columns by fixed
#' POSITION (`tc[[1]]`/`tc[[2]]`, or an `intersect()` whose result order
#' happens to follow a hardcoded search-vector order) is therefore
#' version-fragile: on some Seurat versions x/y silently end up swapped,
#' which — since a swap is exactly a transpose — looks like a 90° rotation
#' once plotted. This function reads columns by NAME instead ("imagecol" ->
#' x, "imagerow" -> y, case-insensitive, with an "x"/"y" fallback for
#' FOV-based technologies), which is correct regardless of column order.
#'
#' Also requests FULL-RESOLUTION coordinates explicitly (`scale = NULL`) —
#' VisiumV1's `GetTissueCoordinates()` defaults to `scale = 'lowres'`
#' (silently pre-scaled down, ~50-60x smaller than full-res pixels) unless
#' told otherwise. Every other consumer of `coords` (BPCells full-res
#' columns, and mod_spatial_viz.R's histology bounds, which reconstruct
#' full-res image bounds as `dim(raster) / scale_factors$lowres`) assumes
#' full-resolution units, so this must stay explicit — see
#' check_histology_coord_alignment() for the regression guard.
#'
#' @param spatial_obj Objet Seurat spatial (Visium/Xenium/CosMx).
#' @return data.frame avec id, x (imagecol / horizontal), y (imagerow /
#'   vertical), fov — ou NULL si GetTissueCoordinates() echoue.
get_spatial_coords <- function(spatial_obj) {
  tc <- tryCatch(Seurat::GetTissueCoordinates(spatial_obj, scale = NULL), error = function(e) NULL)
  if (is.null(tc)) return(NULL)
  
  colnames_tc <- tolower(colnames(tc))
  
  #Identification explicite des colonnes X (colonnes / imagecol) et Y (lignes / imagerow)
  
  col_x <- if ("imagecol" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "imagecol")]
  } else if ("x" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "x")]
  } else {
    colnames(tc)[2] # fallback Seurat v4 (deuxième colonne = imagecol)
  }
  
  col_y <- if ("imagerow" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "imagerow")]
  } else if ("y" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "y")]
  } else {
    colnames(tc)[1] # fallback Seurat v4 (première colonne = imagerow)
  }
  
  df <- data.frame(
    id = rownames(tc) %||% tc$cell %||% paste0("spot_", seq_len(nrow(tc))),
    x = as.numeric(tc[[col_x]]),
    y = as.numeric(tc[[col_y]]),
    fov = if ("fov" %in% colnames_tc) tc[[colnames(tc)[which(colnames_tc == "fov")]]] else NA_character_,
    stringsAsFactors = FALSE
  )
  
  df
}

#' Sanity-check that spot coordinates and the histology raster share the
#' same pixel scale (defensive regression guard)
#'
#' Compares the spot coordinate bounding box (expected: full-resolution
#' pixel units, see get_spatial_coords()) against the histology raster's
#' own full-resolution bounding box (raster dim / lowres scale factor). A
#' large mismatch (spots collapsed into a tiny corner of the image, or
#' spilling far outside it) means the two are no longer in the same
#' coordinate space — most likely a Seurat version regression on
#' GetTissueCoordinates()'s default scale/columns. Warns only; never blocks
#' import.
#'
#' @param coords data.frame(id, x, y, ...) full-resolution coordinates.
#' @param hist_img list as returned by extract_histology_image() (raster,
#'   scale_factors, dim).
#' @return invisible(TRUE)/(FALSE) — TRUE if aligned within tolerance.
check_histology_coord_alignment <- function(coords, hist_img) {
  # Support des deux structures (ancienne et nouvelle API du patch multi-res)
  sf_lowres <- if (!is.null(hist_img$lowres$scale_factor)) {
    hist_img$lowres$scale_factor
  } else if (!is.null(hist_img$scale_factors$lowres)) {
    hist_img$scale_factors$lowres
  } else {
    1
  }
  
  # Récupération de la dimension de l'image
  img_dim <- if (!is.null(hist_img$lowres$dim)) {
    hist_img$lowres$dim
  } else {
    hist_img$dim
  }
  
  if (is.null(img_dim) || any(is.na(img_dim)) || length(img_dim) < 2) return(invisible(TRUE))
  
  img_w <- img_dim[2] / (sf_lowres %||% 1)
  img_h <- img_dim[1] / (sf_lowres %||% 1)
  
  spot_w <- diff(range(coords$x, na.rm = TRUE))
  spot_h <- diff(range(coords$y, na.rm = TRUE))
  
  ratio_w <- spot_w / img_w
  ratio_h <- spot_h / img_h
  
  # Évaluation sécurisée contre les NA/Inf
  ok <- is.finite(ratio_w) && is.finite(ratio_h) &&
    ratio_w > 0.02 && ratio_w < 5 && 
    ratio_h > 0.02 && ratio_h < 5
  
  if (is.na(ok) || !ok) {
    warning(
      "Incoherence ou verification impossible entre les coordonnees des spots et l'image histologique.",
      call. = FALSE
    )
  }
  invisible(identical(ok, TRUE))
}

#' Materialize a small, on-RAM Seurat object for a subset of full-resolution ids
#'
#' Phase 3 — "Subset out anatomical regions" (vignette parity) / ROI
#' workflow: turns a lasso/rectangle spatial selection (mod_spatial_viz.R,
#' "5. ROI isolee") into an actual, downloadable, self-contained Seurat
#' object — raw counts only. Reopens the full-resolution BPCells matrix and
#' subsets to `cell_ids` — the ONLY place in the app that fully
#' materializes a raw BPCells column-subset into a dense in-RAM object
#' outside of the (already-bounded) sketch, so callers must keep
#' `cell_ids` reasonably small (mod_spatial_viz.R only ever calls this on
#' selections drawn from the sketch, <= max_sketch cells, a safe upper
#' bound on a 32GB CPU-only workstation).
#'
#' @param spatial_obj The full global_data$spatial_obj LIST (sketch,
#'   bpcells_dir, coords, ...) — NOT a Seurat object, see file header
#'   CONTRACT. Only $bpcells_dir/$coords/$project are actually used.
#' @param cell_ids Character vector of ids to keep (full-resolution ids,
#'   e.g. from $coords$id / the sketch's colnames — both share the same id
#'   space).
#' @param project Character, Seurat project name for the new object
#'   (defaults to spatial_obj$project).
#' @return A Seurat object (raw counts, single "RNA" assay), <= length(cell_ids) cells.
materialize_seurat_subset <- function(spatial_obj, cell_ids, project = NULL) {
  if (!requireNamespace("BPCells", quietly = TRUE)) stop("Package 'BPCells' requis.")
  if (length(cell_ids) == 0) stop("Aucun identifiant fourni pour la ROI.")
  bpcells_dir <- spatial_obj$bpcells_dir
  if (is.null(bpcells_dir) || !dir.exists(bpcells_dir)) {
    stop("bpcells_dir introuvable sur disque pour ce jeu de donnees.")
  }

  mat <- BPCells::open_matrix_dir(bpcells_dir)
  cell_ids <- intersect(cell_ids, colnames(mat))
  if (length(cell_ids) == 0) stop("Aucun des identifiants de la ROI n'est present dans la matrice BPCells.")

  mat_sub <- mat[, cell_ids, drop = FALSE]
  counts_dense <- methods::as(mat_sub, "dgCMatrix")  # small, deliberate: this IS the requested subset
  obj <- Seurat::CreateSeuratObject(counts = counts_dense, project = project %||% spatial_obj$project %||% "ROI")

  coords <- spatial_obj$coords
  if (!is.null(coords)) {
    m <- match(colnames(obj), coords$id)
    obj$roi_x <- coords$x[m]
    obj$roi_y <- coords$y[m]
  }
  obj
}

# =============================================================================
# R/utils_spatial_io.R — Extrait révisé pour gestion multi-résolution
# =============================================================================

#' Extract histology background images (both lowres and hires if available)
#'
#' @param seurat_obj Seurat spatial object
#' @param technology Technology name ("visium", "xenium", etc.)
#' @return NULL or list containing rasters for lowres/hires and scale factors
extract_histology_image <- function(seurat_obj, technology) {
  if (!identical(technology, "visium")) return(NULL)
  
  img_name <- tryCatch(Seurat::Images(seurat_obj)[1], error = function(e) NA_character_)
  if (is.na(img_name) || length(img_name) == 0) return(NULL)
  
  tryCatch({
    img_obj <- seurat_obj[[img_name]]
    scale_facts <- Seurat::ScaleFactors(img_obj)
    
    # 1. Image lowres (défaut léger)
    raster_lowres <- tryCatch(Seurat::GetImage(img_obj, mode = "raster"), error = function(e) NULL)
    
    # 2. Image hires (si disponible dans le conteneur Visium)
    raster_hires <- tryCatch({
      # Tente de récupérer la matrice hires si présente
      if (is.function(img_obj@image$hires)) {
        img_obj@image$hires
      } else {
        Seurat::GetImage(img_obj, mode = "hires")
      }
    }, error = function(e) NULL)
    
    # Fallback si lowres est indisponible
    if (is.null(raster_lowres) && !is.null(raster_hires)) {
      raster_lowres <- raster_hires
    }
    
    list(
      lowres = list(
        raster = raster_lowres,
        scale_factor = scale_facts$lowres %||% 1,
        dim = if (!is.null(raster_lowres)) dim(raster_lowres) else NULL
      ),
      hires = list(
        raster = raster_hires,
        scale_factor = scale_facts$hires %||% 1,
        dim = if (!is.null(raster_hires)) dim(raster_hires) else NULL
      ),
      scale_factors = scale_facts,
      active_resolution = "lowres"
    )
  }, error = function(e) {
    warning("Extraction histologique échouée : ", conditionMessage(e))
    NULL
  })
}

#' Récupérer le raster RGBA et le scale factor selon la résolution choisie
#'
#' @param hist_data Le sous-objet global_data$spatial_obj$histology
#' @param resolution "lowres" ou "hires"
#' @return list(raster_rgba, scale_factor, dim) ou NULL
get_histology_raster <- function(hist_data, resolution = c("lowres", "hires")) {
  if (is.null(hist_data)) return(NULL)
  resolution <- match.arg(resolution)
  
  target <- hist_data[[resolution]]
  
  # Repli sur lowres si hires absent
  if (is.null(target$raster) && resolution == "hires") {
    target <- hist_data[["lowres"]]
  }
  
  if (is.null(target$raster)) return(NULL)
  
  # Conversion en tableau RGBA [Y, X, 4] transposé pour Plotly / png::writePNG
  rgba_arr <- raster_to_rgba_array(target$raster)
  
  list(
    rgba = rgba_arr,
    raw_raster = target$raster,
    scale_factor = target$scale_factor,
    dim = target$dim
  )
}

#' Convertir une matrice raster (hex ou SpatRaster) en tableau RGBA (dim: H x W x 4)
#' Transposition t(r) incluse pour corriger la rotation Plotly vs ggplot.
raster_to_rgba_array <- function(r) {
  if (is.null(r)) return(NULL)
  
  # Transposition de la matrice 2D pour aligner [imagecol, imagerow] vers [Y, X]
  if (is.matrix(r)) {
    r <- t(r)
  }
  
  if (is.matrix(r) && is.character(r)) {
    hex <- as.vector(r)
    rgb_mat <- grDevices::col2rgb(hex, alpha = FALSE)
    
    arr <- array(1, dim = c(nrow(r), ncol(r), 4L))
    arr[, , 1L] <- matrix(rgb_mat[1L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    arr[, , 2L] <- matrix(rgb_mat[2L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    arr[, , 3L] <- matrix(rgb_mat[3L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    return(arr)
  }
  
  if (inherits(r, "SpatRaster")) {
    arr <- terra::as.array(r)
    if (length(dim(arr)) == 2L) arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  if (inherits(r, c("RasterBrick", "RasterStack", "RasterLayer"))) {
    arr <- raster::as.array(r)
    if (length(dim(arr)) == 2L) arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  if (is.array(r) && length(dim(r)) == 3L) {
    arr <- r
    if (max(arr, na.rm = TRUE) > 1) arr <- arr / 255
    out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
    k <- min(3L, dim(arr)[3L])
    out[, , seq_len(k)] <- arr[, , seq_len(k), drop = FALSE]
    return(out)
  }
  
  stop("Format raster non reconnu : ", class(r)[1L])
}

#' Génère la spécification d'image pour Plotly.js (sans double-nesting ni newlines)
make_plotly_histology_image <- function(hist_data, resolution = "lowres", opacity = 0.8) {
  if (is.null(hist_data)) return(NULL)
  
  # 1. Extraction rétro-compatible (structure plate vs nouvelle multi-résolution)
  raster_raw <- if (!is.null(hist_data$lowres$raster) || !is.null(hist_data$hires$raster)) {
    target <- hist_data[[resolution]] %||% hist_data[["lowres"]]
    target$raster
  } else {
    hist_data$raster # Fallback ancienne structure
  }
  
  scale_factor <- if (!is.null(hist_data$lowres$scale_factor)) {
    (hist_data[[resolution]] %||% hist_data[["lowres"]])$scale_factor
  } else if (!is.null(hist_data$scale_factors[[resolution]])) {
    hist_data$scale_factors[[resolution]]
  } else {
    hist_data$scale_factors$lowres %||% 1
  }
  
  if (is.null(raster_raw)) return(NULL)
  
  # 2. Conversion RGBA
  rgba_arr <- raster_to_rgba_array(raster_raw)
  if (is.null(rgba_arr)) return(NULL)
  
  # 3. Encodage PNG Base64 sans retours à la ligne (\n)
  png_bytes <- png::writePNG(rgba_arr)
  data_uri <- base64enc::dataURI(png_bytes, mime = "image/png")
  data_uri <- gsub("[\r\n]", "", data_uri)
  
  # 4. Calcul des dimensions réelles en pixels (coordonnées Visium)
  img_h <- dim(rgba_arr)[1] / scale_factor
  img_w <- dim(rgba_arr)[2] / scale_factor
  
  # 5. Retourne un OBJET UNIQUE (pas une liste de listes)
  list(
    source  = data_uri,
    xref    = "x",
    yref    = "y",
    x       = 0,
    y       = img_h,       # Origine Visium en haut à gauche
    sizex   = img_w,
    sizey   = img_h,
    sizing  = "stretch",
    opacity = opacity,
    layer   = "below"
  )
}