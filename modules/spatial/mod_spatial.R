# modules/spatial/mod_spatial.R
# Parent Module: UI/Server Router for Spatial Analysis
# Declares shared_rv and instantiates child modules (QC, Cluster, Deconv, Viz)

library(shiny)
library(bslib)
library(bsicons)
library(mirai)

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Status indicator
    uiOutput(ns("spatial_status_ui")),
    
    # Main navigation tabs
    navset_card_underline(
      id = ns("spatial_nav"),
      
      # Tab 1: Quality Control
      nav_panel("QC & Metrics",
                mod_spatial_qc_ui(ns("qc"))
      ),
      
      # Tab 2: Spatial Clustering
      nav_panel("Clustering",
                mod_spatial_cluster_ui(ns("cluster"))
      ),
      
      # Tab 3: Deconvolution
      nav_panel("Deconvolution",
                mod_spatial_deconv_ui(ns("deconv"))
      ),
      
      # Tab 4: Visualization
      nav_panel("Visualization",
                mod_spatial_viz_ui(ns("viz"))
      )
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize mirai daemons for spatial processing (6 workers max)
    initialize_spatial_mirai(n_daemons = 6)
    
    # Shared reactive values for inter-module communication
    shared_rv <- reactiveValues(
      active_tab = "QC",
      cluster_labels = NULL,      # Vector: Cell_ID -> Cluster_ID
      deconv_props = NULL,        # Matrix: Cell_ID -> CellType proportions
      current_fov_crop = NULL,    # Current field of view coordinates
      qc_metrics = NULL,          # QC results (nCount, nFeature, %MT)
      moran_results = NULL,       # Moran's I results for top variable genes
      bpcells_path = "",          # Path to on-disk BPCells matrix
      sketch_obj = NULL,          # Downsampled object for visualization (max 50k cells)
      task_status = "idle",       # Current async task status
      log_file = tempfile("spatial_log_", fileext = ".txt")
    )
    
    # Update status UI based on data availability
    output$spatial_status_ui <- renderUI({
      if (is.null(global_data$spatial_obj)) {
        div(class = "alert alert-danger",
            bsicons::bs_icon("exclamation-triangle"),
            " Aucune donnée spatiale chargée. Allez dans l'onglet 'Import Données > Spatial'.")
      } else {
        n_spots <- ncol(global_data$spatial_obj)
        div(class = "alert alert-success",
            bsicons::bs_icon("check-circle"),
            paste0(" Objet Spatial chargé: ", format(n_spots, big.mark = ","), " spots"))
      }
    })
    
    # Extract BPCells path and create sketch when spatial object is loaded
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj)
      
      obj <- global_data$spatial_obj
      
      # Get or create BPCells directory path
      # Assuming the import module already converted to BPCells
      # If not, we would call convert_to_bpcells_and_fov() here
      
      # Store path in shared_rv for child modules
      # This is a placeholder - actual path should come from import module
      shared_rv$bpcells_path <- tempfile("bpcells_spatial_")
      
      # Create sketch for visualization (max 50k cells)
      shared_rv$sketch_obj <- safe_downsample(obj, max_cells = 50000)
      
      showNotification("Spatial data prepared for analysis", type = "message", duration = 3)
    }, ignoreInit = TRUE)
    
    # Instantiate child modules
    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)
    
    # Track active tab for context-aware operations
    observeEvent(input$spatial_nav, {
      shared_rv$active_tab <- input$spatial_nav
    })
    
    # Cleanup on session end
    session$onSessionEnded(function() {
      # Optionally clean up temp files
      if (file.exists(shared_rv$log_file)) {
        unlink(shared_rv$log_file)
      }
    })
  })
}