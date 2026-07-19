# modules/spatial/mod_spatial.R

#' Spatial Analysis Parent Module
#'
#' This module serves as the main entry point for Spatial analysis.
#' It initializes the mirai daemon pool, manages the shared reactive state (shared_rv),
#' and routes traffic to child modules (QC, Clustering, Deconvolution, Visualization).
#'
#' @param id Character. The module ID.
#' @param global_data ReactiveVal/ReactiveEnvironment. Contains 'spatial_obj' (BPCells-backed) and 'bpcells_path'.
#' @import shiny
#' @import bslib
#' @importFrom mirai daemons status
#' @importFrom shinyjs useShinyjs disable enable
mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  # Main Container
  card(
    header = "Spatial Transcriptomics Analysis",
    
    # Navigation Tabs
    navset_card_underline(
      id = ns("spatial_nav"),
      
      # 1. Quality Control Tab
      nav_panel("Quality Control", 
                mod_spatial_qc_ui(ns("qc"))
      ),
      
      # 2. Spatial Clustering Tab
      nav_panel("Clustering", 
                mod_spatial_cluster_ui(ns("cluster"))
      ),
      
      # 3. Deconvolution Tab
      nav_panel("Deconvolution", 
                mod_spatial_deconv_ui(ns("deconv"))
      ),
      
      # 4. Visualization Tab (Always visible, updates based on others)
      nav_panel("Visualization", 
                mod_spatial_viz_ui(ns("viz"))
      )
    ),
    
    # Global Progress Indicator (Optional: can be moved to specific tabs if preferred)
    # Displays a toast or progress bar when async tasks are running globally
    div(id = ns("global_progress_container"), style = "margin-top: 10px;")
  )
}

mod_spatial_server <- function(id, global_data, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ----------------------------------------------------------------------
    # 1. Initialization & Asynchronous Setup
    # ----------------------------------------------------------------------
    
    # Initialize Mirai Daemons (Pool of 6 workers)
    # We do this once at the parent level so children share the same pool
    observeOnce({
      req(global_data()) # Ensure data is loaded
      tryCatch({
        initialize_spatial_mirai(n_daemons = 6)
        showNotification("Spatial Async Engine Initialized (6 Workers)", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error initializing mirai:", e$message), type = "error")
      })
    })
    
    # ----------------------------------------------------------------------
    # 2. Shared Reactive State (The "Brain")
    # ----------------------------------------------------------------------
    # This list is passed by reference to all children. 
    # Children write results here; Viz reads from here.
    shared_rv <- reactiveVal(list(
      active_tab = "QC",
      
      # QC Results
      qc_metrics = NULL,
      moran_results = NULL,
      
      # Clustering Results
      cluster_labels = NULL,      # Named vector: CellID -> ClusterID
      cluster_params = NULL,      # List: {resolution, k_neighbors}
      
      # Deconvolution Results
      deconv_props = NULL,        # Matrix/DataFrame: CellID -> CellType Proportions
      deconv_method = NULL,       # "RCTD" or "STdeconvolve"
      
      # Visualization State
      current_zoom = 1,
      show_polygons = FALSE,
      color_by = "original"       # "original", "cluster", "deconv"
    ))
    
    # Update active tab in shared state when user switches
    observeEvent(input$spatial_nav, {
      rv <- shared_rv()
      rv$active_tab <- input$spatial_nav
      shared_rv(rv)
    })
    
    # ----------------------------------------------------------------------
    # 3. Instantiate Child Modules
    # ----------------------------------------------------------------------
    # Pass global_data (for raw access) and shared_rv (for state sync)
    
    # QC Module
    qc_results <- mod_spatial_qc_server("qc", global_data, shared_rv, session)
    
    # Clustering Module
    cluster_results <- mod_spatial_cluster_server("cluster", global_data, shared_rv, session)
    
    # Deconvolution Module
    deconv_results <- mod_spatial_deconv_server("deconv", global_data, shared_rv, session)
    
    # Visualization Module
    # Viz depends on shared_rv updates from QC, Cluster, and Deconv
    mod_spatial_viz_server("viz", global_data, shared_rv, session)
    
    # ----------------------------------------------------------------------
    # 4. Return Reactive Exports (if needed by upper levels)
    # ----------------------------------------------------------------------
    return(shared_rv)
  })
}