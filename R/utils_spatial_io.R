# R/utils_spatial_io.R

#' Convert Raw Spatial Data to BPCells and Standardize FOV
#'
#' Takes a raw Seurat object (loaded via Load10X_Spatial, LoadXenium, etc.),
#' converts the count matrix to on-disk BPCells format, and standardizes
#' geometry columns (Simplify/Crop).
#'
#' @param seurat_obj Seurat. The input Seurat object containing spatial data.
#' @param output_dir Character. Directory path to store BPCells files.
#' @param simplify_tolerance Numeric. Tolerance for simplifying polygon geometries (default: 0.5).
#' @param max_sketch_size Integer. Maximum number of cells to keep in the RAM "Sketch" (default: 50000).
#' @return A list containing:
#'   \item{bpcells_path}{Path to the BPCells directory.}
#'   \item{sketch_obj}{A downsampled Seurat object for visualization.}
#'   \item{full_obj_ptr}{An externalptr reference to the full on-disk object.}
#' @importFrom Seurat Load10X_Spatial GetAssayData CreateAssayObject DefaultAssay
#' @importFrom BPCells write_matrix_dir open_matrix_dir
#' @importFrom sf st_make_valid st_simplify st_centroid st_as_sf st_coordinates
convert_to_bpcells_and_fov <- function(seurat_obj, output_dir, simplify_tolerance = 0.5, max_sketch_size = 50000) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  bpcells_matrix_dir <- file.path(output_dir, "counts_bpcells")
  
  # 1. Convert Count Matrix to BPCells (On-Disk)
  # Extract the counts matrix and write it to disk in BPCells format
  counts_matrix <- Seurat::GetAssayData(seurat_obj, slot = "counts")
  
  # Ensure it's a sparse matrix before writing
  if (!inherits(counts_matrix, "dgCMatrix")) {
    counts_matrix <- as(counts_matrix, "dgCMatrix")
  }
  
  BPCells::write_matrix_dir(counts_matrix, bpcells_matrix_dir)
  
  # 2. Reopen as BPCells object and update Seurat Assay
  bpcells_mat <- BPCells::open_matrix_dir(bpcells_matrix_dir)
  
  # Create a new assay with the BPCells matrix
  spatial_assay <- Seurat::CreateAssayObject(counts = bpcells_mat)
  seurat_obj[["spatial"]] <- spatial_assay
  
  # Set default assay to spatial if not already
  Seurat::DefaultAssay(seurat_obj) <- "spatial"
  
  # 3. Process Geometry (FOV Standardization)
  # Check if geometry exists (e.g., for Xenium/CosMx)
  if ("geometry" %in% colnames(seurat_obj@meta.data)) {
    
    # Simplify polygons to reduce memory footprint and rendering load
    tryCatch({
      # Convert to sf object temporarily for processing
      geo_sf <- sf::st_as_sf(seurat_obj@meta.data, geometry = "geometry")
      
      # Make valid and simplify
      geo_sf_clean <- sf::st_make_valid(geo_sf)
      geo_sf_simple <- sf::st_simplify(geo_sf_clean, dTolerance = simplify_tolerance)
      
      # Update centroids if needed (for scatter plots)
      centroids <- sf::st_centroid(geo_sf_simple)
      
      # Store simplified geometry and centroids back in meta.data
      coords <- sf::st_coordinates(centroids)
      seurat_obj@meta.data$center_x <- coords[,1]
      seurat_obj@meta.data$center_y <- coords[,2]
      
      # Update the geometry column with simplified version
      seurat_obj@meta.data$geometry <- geo_sf_simple$geometry
      
    }, error = function(e) {
      warning("Could not process geometry: ", e$message)
    })
  }
  
  # 4. Create "Sketch" for Visualization (RAM-resident subset)
  n_cells <- ncol(seurat_obj)
  if (n_cells > max_sketch_size) {
    # Random sampling for the sketch
    set.seed(123) # Reproducibility
    sketch_indices <- sample(seq_len(n_cells), max_sketch_size)
    sketch_obj <- seurat_obj[, sketch_indices]
    
    # For the sketch, we can keep the matrix in memory for fast plotting
    sketch_counts <- Seurat::GetAssayData(sketch_obj, slot = "counts")
    # Force to sparse matrix in RAM for the sketch
    sketch_counts_ram <- as(sketch_counts, "dgCMatrix") 
    sketch_assay <- Seurat::CreateAssayObject(counts = sketch_counts_ram)
    sketch_obj[["spatial"]] <- sketch_assay
  } else {
    sketch_obj <- seurat_obj
  }
  
  # 5. Return references
  return(list(
    bpcells_path = bpcells_matrix_dir,
    sketch_obj = sketch_obj,
    full_obj = seurat_obj # This object now references the BPCells matrix on disk
  ))
}

#' Safe Downsampling Utility
#'
#' Helper to ensure we never exceed RAM limits when subsetting.
#' @param obj Seurat object.
#' @param max_cells Integer. Max cells allowed.
#' @return Seurat object (subsetted if necessary).
safe_downsample <- function(obj, max_cells = 50000) {
  n_cells <- ncol(obj)
  if (n_cells <= max_cells) return(obj)
  
  set.seed(42)
  idx <- sample(seq_len(n_cells), max_cells)
  return(obj[, idx])
}
