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
# v4 (multi‑resolution histology patch): unified extract_histology_image(),
# get_histology_raster(), raster_to_rgba_array() with no transposition/flip
# inside the raster converter — all geometry alignment is delegated to the
# rendering layer (ggplot2::annotation_raster() and Plotly images layout).
# =============================================================================
# =============================================================================
# R/utils_spatial_io.R — BPCells conversion + FOV standardization
# =============================================================================
# v4 (multi‑resolution histology patch): unified extract_histology_image(),
# get_histology_raster(), raster_to_rgba_array() with no transposition/flip
# inside the raster converter — all geometry alignment is delegated to the
# rendering layer (ggplot2::annotation_raster() and Plotly images layout).
# =============================================================================

# =============================================================================
# R/utils_spatial_io.R — BPCells conversion + FOV standardization
# =============================================================================
# v4 (multi‑resolution histology patch): unified extract_histology_image(),
# get_histology_raster(), raster_to_rgba_array() with no transposition/flip
# inside the raster converter — all geometry alignment is delegated to the
# rendering layer (ggplot2::annotation_raster() and Plotly images layout).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Root cache directory for all persisted BPCells matrices
#'
#' Uses tools::R_user_dir() rather than tempdir() so imported datasets
#' survive across R sessions / app restarts. 
#' @return Character path, created if missing.
bpcells_cache_root <- function() {
  root <- tools::R_user_dir("TranscriptoShiny", which = "cache")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  root
}

# -----------------------------------------------------------------------------
# Histology extraction (multi‑resolution) — Visium only
# -----------------------------------------------------------------------------

#' Extract Visium histology backgrounds at low and high resolution
#' and scan the spatial directory for additional image files.
#'
#' @param seurat_obj A Visium Seurat object.
#' @param technology Technology name.
#' @return NULL or a multi-resolution histology list with an $images slot.
extract_histology_image <- function(seurat_obj, technology) {
  if (!identical(technology, "visium")) {
    return(NULL)
  }
  
  img_name <- tryCatch(
    Seurat::Images(seurat_obj)[1L],
    error = function(e) NA_character_
  )
  
  if (is.na(img_name) || !nzchar(img_name)) {
    return(NULL)
  }
  
  read_histology_file <- function(path) {
    ext <- tolower(tools::file_ext(path))
    
    tryCatch({
      if (ext %in% c("png", "jpg", "jpeg")) {
        if (!requireNamespace("png", quietly = TRUE) && ext == "png") {
          stop("Package 'png' requis pour lire les fichiers PNG.")
        }
        
        if (!requireNamespace("jpeg", quietly = TRUE) && ext %in% c("jpg", "jpeg")) {
          stop("Package 'jpeg' requis pour lire les fichiers JPEG.")
        }
        
        if (ext == "png") {
          return(png::readPNG(path))
        }
        
        return(jpeg::readJPEG(path))
      }
      
      if (ext %in% c("tif", "tiff", "jp2", "j2k", "jpf")) {
        if (!requireNamespace("magick", quietly = TRUE)) {
          stop(
            "Package 'magick' requis pour lire les images TIFF/JPEG2000/JPF. ",
            "Installez-le avec install.packages('magick')."
          )
        }
        
        img <- magick::image_read(path)
        img <- magick::image_convert(img, format = "png")
        img_raw <- magick::image_write(img, format = "png")
        return(png::readPNG(img_raw))
      }
      
      NULL
    }, error = function(e) {
      warning(
        sprintf(
          "Lecture impossible du fond histologique '%s' : %s",
          basename(path),
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    })
  }
  
  get_visium_spatial_dir <- function(obj, image_obj) {
    candidates <- character(0)
    
    image_path <- tryCatch(
      methods::slot(image_obj, "image"),
      error = function(e) NULL
    )
    
    if (is.character(image_path) && length(image_path) == 1L) {
      candidates <- c(
        candidates,
        dirname(image_path),
        file.path(dirname(image_path), "spatial")
      )
    }
    
    image_dir <- tryCatch(
      methods::slot(image_obj, "misc")$spatial_dir,
      error = function(e) NULL
    )
    
    if (is.character(image_dir) && length(image_dir) == 1L) {
      candidates <- c(candidates, image_dir)
    }
    
    candidates <- unique(normalizePath(
      candidates[file.exists(candidates)],
      winslash = "/",
      mustWork = FALSE
    ))
    
    if (length(candidates) == 0L) {
      return(NULL)
    }
    
    spatial_dirs <- candidates[basename(candidates) == "spatial"]
    
    if (length(spatial_dirs) > 0L) {
      return(spatial_dirs[1L])
    }
    
    candidates[1L]
  }
  
  tryCatch({
    img_obj <- seurat_obj[[img_name]]
    scale_factors <- Seurat::ScaleFactors(img_obj)
    
    raster_lowres <- tryCatch(
      Seurat::GetImage(img_obj, mode = "raster"),
      error = function(e) NULL
    )
    
    raster_hires <- tryCatch({
      if ("image" %in% methods::slotNames(img_obj)) {
        image_slot <- methods::slot(img_obj, "image")
        
        if (is.list(image_slot) && !is.null(image_slot$hires)) {
          image_slot$hires
        } else {
          Seurat::GetImage(img_obj, mode = "hires")
        }
      } else {
        Seurat::GetImage(img_obj, mode = "hires")
      }
    }, error = function(e) NULL)
    
    if (is.null(raster_lowres) && !is.null(raster_hires)) {
      raster_lowres <- raster_hires
      scale_factors$lowres <- scale_factors$hires %||% 1
    }
    
    if (is.null(raster_lowres)) {
      warning(
        "Aucune image histologique lowres exploitable n'a ete extraite.",
        call. = FALSE
      )
      return(NULL)
    }
    
    images <- list(
      lowres = list(
        raster = raster_lowres,
        scale_factor = scale_factors$lowres %||% 1,
        dim = dim(raster_lowres),
        source = "Seurat::GetImage(mode = 'raster')"
      )
    )
    
    if (!is.null(raster_hires)) {
      images$hires <- list(
        raster = raster_hires,
        scale_factor = scale_factors$hires %||% NA_real_,
        dim = dim(raster_hires),
        source = "Seurat::GetImage(mode = 'hires')"
      )
    }
    
    spatial_dir <- get_visium_spatial_dir(seurat_obj, img_obj)
    
    if (!is.null(spatial_dir) && dir.exists(spatial_dir)) {
      image_files <- list.files(
        spatial_dir,
        pattern = "\\.(png|jpg|jpeg|tif|tiff|jp2|j2k|jpf)$",
        ignore.case = TRUE,
        full.names = TRUE
      )
      
      if (length(image_files) > 0L) {
        for (path in image_files) {
          file_name <- basename(path)
          file_stem <- tools::file_path_sans_ext(file_name)
          
          is_lowres <- grepl("lowres", file_stem, ignore.case = TRUE)
          is_hires <- grepl("hires|highres", file_stem, ignore.case = TRUE)
          
          key <- if (is_lowres) {
            "lowres_file"
          } else if (is_hires) {
            "hires_file"
          } else {
            paste0(
              "file_",
              gsub("[^A-Za-z0-9_]+", "_", tolower(file_stem))
            )
          }
          
          if (key %in% names(images)) {
            key <- paste0(
              key,
              "_",
              sprintf("%02d", sum(startsWith(names(images), key)) + 1L)
            )
          }
          
          raster_file <- read_histology_file(path)
          
          if (is.null(raster_file)) {
            next
          }
          
          file_dim <- dim(raster_file)
          
          # Registration automatique uniquement pour les images Visium
          # lowres/hires. Les fichiers additionnels conservent par défaut
          # le repère full-resolution : 1 pixel image = 1 unité spatiale.
          file_scale_factor <- if (is_lowres) {
            scale_factors$lowres %||% 1
          } else if (is_hires) {
            scale_factors$hires %||% 1
          } else {
            1
          }
          
          images[[key]] <- list(
            raster = raster_file,
            scale_factor = file_scale_factor,
            dim = file_dim,
            source = normalizePath(path, winslash = "/", mustWork = FALSE)
          )
        }
      }
    }
    
    list(
      images = images,
      lowres = images$lowres,
      hires = images$hires %||% NULL,
      scale_factors = scale_factors,
      active_resolution = "lowres",
      spatial_dir = spatial_dir
    )
  }, error = function(e) {
    warning(
      "Extraction histologique echouee : ",
      conditionMessage(e),
      call. = FALSE
    )
    NULL
  })
}

#' Convert a histology raster into a PNG-compatible RGBA array
#'
#' No transpose and no flip are applied here. Spatial placement and axis
#' orientation are handled only by the rendering layer.
#'
#' @param r Character raster matrix, SpatRaster, Raster* object, or RGB array.
#' @return Numeric RGBA array with dimensions height x width x 4, values in [0, 1].
raster_to_rgba_array <- function(r) {
  if (is.null(r)) return(NULL)
  
  if (is.matrix(r) && is.character(r)) {
    hex <- as.vector(r)
    rgb <- grDevices::col2rgb(hex, alpha = FALSE)
    
    out <- array(1, dim = c(nrow(r), ncol(r), 4L))
    out[, , 1L] <- matrix(rgb[1L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    out[, , 2L] <- matrix(rgb[2L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    out[, , 3L] <- matrix(rgb[3L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    
    return(out)
  }
  
  if (inherits(r, "SpatRaster")) {
    arr <- terra::as.array(r)
  } else if (inherits(r, c("RasterBrick", "RasterStack", "RasterLayer"))) {
    arr <- raster::as.array(r)
  } else if (is.array(r) && length(dim(r)) == 3L) {
    arr <- r
  } else {
    stop("Format raster non reconnu : ", class(r)[1L])
  }
  
  if (length(dim(arr)) == 2L) {
    arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
  }
  
  if (max(arr, na.rm = TRUE) > 1) {
    arr <- arr / 255
  }
  
  out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
  n_channels <- min(3L, dim(arr)[3L])
  out[, , seq_len(n_channels)] <- arr[, , seq_len(n_channels), drop = FALSE]
  
  out
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

#' Get one histology image and its spatial scale
#'
#' @param hist_data Histology object returned by extract_histology_image().
#' @param resolution Requested resolution: "lowres" or "hires" or any key from hist_data$images.
#' @return NULL or list(raw_raster, rgba, scale_factor, dim, resolution).
get_histology_raster <- function(hist_data, resolution = "lowres") {
  if (is.null(hist_data)) {
    return(NULL)
  }
  
  available_images <- if (
    !is.null(hist_data$images) &&
    is.list(hist_data$images)
  ) {
    hist_data$images
  } else {
    candidates <- names(hist_data)[
      vapply(
        hist_data,
        function(x) is.list(x) && !is.null(x$raster),
        logical(1)
      )
    ]
    
    stats::setNames(
      lapply(candidates, function(name) hist_data[[name]]),
      candidates
    )
  }
  
  if (length(available_images) == 0L) {
    return(NULL)
  }
  
  if (!resolution %in% names(available_images)) {
    resolution <- if ("lowres" %in% names(available_images)) {
      "lowres"
    } else {
      names(available_images)[1L]
    }
  }
  
  target <- available_images[[resolution]]
  
  if (is.null(target) || is.null(target$raster)) {
    return(NULL)
  }
  
  list(
    raw_raster = target$raster,
    rgba = raster_to_rgba_array(target$raster),
    scale_factor = target$scale_factor %||% 1,
    dim = target$dim %||% dim(target$raster),
    resolution = resolution,
    source = target$source %||% NA_character_
  )
}

# -----------------------------------------------------------------------------
# Spatial coordinates (full resolution)
# -----------------------------------------------------------------------------

#' Extraction sécurisée des coordonnées spatiales Visium (Seurat v4 et v5)
#'
#' FIX (root cause of the reported 90° rotation): `Seurat::GetTissueCoordinates()`
#' column ORDER/NAMING for VisiumV1 has genuinely differed between Seurat
#' versions. This function reads columns by NAME instead of position.
#'
#' @param spatial_obj Objet Seurat spatial (Visium/Xenium/CosMx).
#' @return data.frame avec id, x (imagecol / horizontal), y (imagerow /
#'   vertical), fov — ou NULL si GetTissueCoordinates() echoue.
get_spatial_coords <- function(spatial_obj) {
  tc <- tryCatch(Seurat::GetTissueCoordinates(spatial_obj, scale = NULL), error = function(e) NULL)
  if (is.null(tc)) return(NULL)
  
  colnames_tc <- tolower(colnames(tc))
  
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
#' @param coords data.frame(id, x, y, ...) full-resolution coordinates.
#' @param hist_img list as returned by extract_histology_image().
#' @return invisible(TRUE) if aligned within tolerance.
check_histology_coord_alignment <- function(coords, hist_img) {
  # Support des deux structures (ancienne et nouvelle API)
  sf_lowres <- if (!is.null(hist_img$lowres$scale_factor)) {
    hist_img$lowres$scale_factor
  } else if (!is.null(hist_img$scale_factors$lowres)) {
    hist_img$scale_factors$lowres
  } else {
    1
  }
  
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

# -----------------------------------------------------------------------------
# Main conversion pipeline
# -----------------------------------------------------------------------------

#' Convert a raw Seurat spatial object to an on-disk BPCells-backed assay
#'
#' @param seurat_obj Seurat object as returned by load_spatial_visium()
#'   / Seurat::LoadXenium() / Seurat::LoadNanostring().
#' @param dataset_id Character, unique slug for this dataset.
#' @param technology One of "visium", "xenium", "cosmx".
#' @param assay Character, assay holding raw counts.
#' @param simplify_tol Numeric tolerance passed to Seurat::Simplify().
#' @param max_sketch Integer, max cells/spots kept in the in-RAM sketch.
#' @param norm_method "lognorm" (default) or "sct".
#' @return List — see file header CONTRACT.
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
  if (dir.exists(bpcells_dir)) unlink(bpcells_dir, recursive = TRUE)
  
  # --- 1. Counts -> disk (BPCells).
  counts <- SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts")
  BPCells::write_matrix_dir(counts, bpcells_dir, overwrite = TRUE)
  bpcells_mat <- BPCells::open_matrix_dir(bpcells_dir)
  SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts") <- bpcells_mat
  
  # --- 2. Imaging-only: simplify segmentation polygons.
  if (technology %in% c("xenium", "cosmx")) {
    seurat_obj <- .simplify_all_fovs(seurat_obj, tol = simplify_tol)
  }
  
  # --- 3. Full-resolution coordinates.
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
  
  # --- 4. Histology image (multi‑resolution).
  hist_img <- extract_histology_image(seurat_obj, technology)
  
  if (!is.null(coords) && !is.null(hist_img)) {
    check_histology_coord_alignment(coords, hist_img)
  }
  
  # --- 5. In-RAM sketch.
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
#' Best-effort: any single boundary that fails is skipped with a warning.
#'
#' @param obj Seurat object with one or more imaging FOVs.
#' @param tol Numeric, SeuratObject::Simplify() tolerance.
#' @return The Seurat object, each FOV's segmentation simplified in place.
.simplify_all_fovs <- function(obj, tol = 20) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' manquant : simplification des polygones ignoree (Simplify() en depend).")
    return(obj)
  }
  fov_names <- tryCatch(Seurat::Images(obj), error = function(e) character(0))
  for (fv in fov_names) {
    fov_obj <- obj[[fv]]
    if (!methods::is(fov_obj, "FOV")) next
    
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
#' Thin, defensive wrapper around SeuratObject::Crop().
#'
#' @param obj Seurat object (or a single FOV object).
#' @param fov Character, FOV/image name (ignored if `obj` is already a FOV).
#' @param x,y Numeric length-2 vectors, bounding box in tissue coordinates.
#' @return A cropped FOV object, or NULL on failure.
crop_fov_bbox <- function(obj, fov, x, y) {
  tryCatch({
    target <- if (inherits(obj, "Seurat")) obj[[fov]] else obj
    SeuratObject::Crop(target, x = x, y = y, coords = "plot")
  }, error = function(e) {
    warning("Crop() a echoue : ", conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# Sketch construction
# -----------------------------------------------------------------------------

#' Build (or rebuild) the in-RAM plotting sketch for a spatial object
#'
#' Primary path: Seurat's own `SketchData(method = "Uniform")`.
#' Falls back to uniform `sample()` if SketchData fails.
#'
#' @param obj Seurat object (full, BPCells-backed or not).
#' @param max_cells Integer, sketch size cap.
#' @param assay Character, assay to materialize into RAM for the sketch.
#' @param norm_method "lognorm" (default) or "sct".
#' @return Seurat object, <= max_cells cells/spots, in-RAM Assay5.
build_sketch <- function(obj, max_cells = 50000, assay = NULL,
                         norm_method = c("lognorm", "sct")) {
  norm_method <- match.arg(norm_method)
  assay <- assay %||% Seurat::DefaultAssay(obj)
  
  sk <- tryCatch({
    # Initial LogNormalize so SketchData has a "data" layer.
    obj_norm <- Seurat::NormalizeData(obj, assay = assay, verbose = FALSE)
    obj_norm <- Seurat::SketchData(obj_norm, assay = assay, ncells = max_cells,
                                   sketched.assay = "sketch", method = "Uniform",
                                   cast = "dgCMatrix", verbose = FALSE)
    sketch_cells <- SeuratObject::Cells(obj_norm[["sketch"]])
    sk <- subset(obj_norm, cells = sketch_cells)
    sk <- Seurat::DietSeurat(sk, assays = "sketch")
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  }, error = function(e) {
    warning("Seurat::SketchData() a echoue (", conditionMessage(e),
            ") — repli sur un sous-echantillonnage aleatoire simple.")
    n <- ncol(obj)
    idx <- if (n <= max_cells) seq_len(n) else { set.seed(42); sort(sample(seq_len(n), max_cells)) }
    sk <- obj[, idx]
    sk_counts <- methods::as(SeuratObject::LayerData(sk, assay = assay, layer = "counts"), "dgCMatrix")
    sk[["sketch"]] <- SeuratObject::CreateAssay5Object(counts = sk_counts)
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  })
  
  # User-facing normalization (SCT opt‑in) on the small sketch only.
  if (identical(norm_method, "sct")) {
    sk <- tryCatch({
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
  
  # Guarantee a populated "data" layer.
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

# -----------------------------------------------------------------------------
# Fast QC metrics from BPCells
# -----------------------------------------------------------------------------

#' Fast, streamed QC stats directly from the on-disk BPCells matrix
#'
#' @param bpcells_dir Character path (global_data$spatial_obj$bpcells_dir).
#' @param mt_pattern Regex for mitochondrial genes.
#' @param ribo_pattern Regex for ribosomal genes.
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

# -----------------------------------------------------------------------------
# ROI materialization
# -----------------------------------------------------------------------------

#' Materialize a small, on-RAM Seurat object for a subset of full-resolution ids
#'
#' @param spatial_obj The full global_data$spatial_obj LIST.
#' @param cell_ids Character vector of ids to keep.
#' @param project Character, Seurat project name.
#' @return A Seurat object (raw counts, single "RNA" assay).
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
  counts_dense <- methods::as(mat_sub, "dgCMatrix")
  obj <- Seurat::CreateSeuratObject(counts = counts_dense, project = project %||% spatial_obj$project %||% "ROI")
  
  coords <- spatial_obj$coords
  if (!is.null(coords)) {
    m <- match(colnames(obj), coords$id)
    obj$roi_x <- coords$x[m]
    obj$roi_y <- coords$y[m]
  }
  obj
}

# -----------------------------------------------------------------------------
# Debug helper
# -----------------------------------------------------------------------------

debug_histology <- function(global_data) {
  hist_data <- global_data$spatial_obj$histology
  cat("=== Histology Debug ===\n")
  cat("histology exists:", !is.null(hist_data), "\n")
  if (!is.null(hist_data)) {
    cat("names:", paste(names(hist_data), collapse=", "), "\n")
    for (res in c("lowres", "hires")) {
      if (!is.null(hist_data[[res]])) {
        cat(res, "raster class:", class(hist_data[[res]]$raster)[1], "\n")
        if (!is.null(hist_data[[res]]$dim)) {
          cat(res, "dim:", paste(hist_data[[res]]$dim, collapse=" x "), "\n")
        }
        cat(res, "scale_factor:", hist_data[[res]]$scale_factor, "\n")
      }
    }
    if (!is.null(hist_data$images)) {
      cat("Additional images in $images:", paste(names(hist_data$images), collapse=", "), "\n")
    }
  }
  cat("========================\n")
}