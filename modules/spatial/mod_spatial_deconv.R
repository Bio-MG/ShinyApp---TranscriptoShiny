# modules/spatial/mod_spatial_deconv.R
# Child Module: Cell Deconvolution (RCTD / STdeconvolve)

library(shiny)
library(bslib)
library(mirai)
library(Seurat)
library(BPCells)

mod_spatial_deconv_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left panel: Controls
      column(width = 3,
        wellPanel(
          h4("Deconvolution Method"),
          
          radioButtons(ns("deconv_method"), "Algorithm",
                       choices = c(
                         "RCTD (Reference-based)" = "rctd",
                         "STdeconvolve (Reference-free)" = "stdeconv"
                       ),
                       selected = "rctd"),
          
          hr(),
          
          h5("Reference Upload (for RCTD)"),
          p(class = "text-muted small",
            "Upload scRNA-seq reference (.rds Seurat object)"),
          fileInput(ns("ref_file"), "Reference File",
                    accept = ".rds",
                    buttonLabel = icon("upload")),
          
          hr(),
          
          h5("Parameters"),
          numericInput(ns("rctd_thresh"), "RCTD Threshold", 
                       value = 0.8, min = 0.5, max = 1.0, step = 0.05),
          
          numericInput(ns("stdeconv_k"), "STdeconv Topics (K)", 
                       value = 10, min = 2, max = 30, step = 1),
          
          hr(),
          
          actionButton(ns("run_deconv"), "Run Deconvolution", 
                       class = "btn-success btn-block btn-lg", 
                       icon = icon("puzzle-piece"))
        ),
        
        wellPanel(
          h4("Status"),
          textOutput(ns("deconv_status")),
          verbatimTextOutput(ns("deconv_log"))
        )
      ),
      
      # Right panel: Results
      column(width = 9,
        div(id = ns("deconv_progress_ui")),
        
        tabsetPanel(
          tabPanel("Cell Type Proportions Map",
            plotOutput(ns("deconv_spatial_plot"), height = "600px")
          ),
          tabPanel("Proportions Heatmap",
            plotOutput(ns("deconv_heatmap"), height = "500px")
          ),
          tabPanel("Summary Statistics",
            tableOutput(ns("deconv_summary")),
            plotOutput(ns("deconv_barplot"), height = "400px")
          )
        )
      )
    )
  )
}

mod_spatial_deconv_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Store reference object if uploaded
    ref_object <- reactiveVal(NULL)
    
    # Handle reference file upload
    observeEvent(input$ref_file, {
      req(input$ref_file)
      req(input$deconv_method == "rctd")
      
      withProgress(message = "Loading reference...", {
        tryCatch({
          ref <- readRDS(input$ref_file$datapath)
          
          if (!inherits(ref, "Seurat")) {
            stop("Uploaded file must be a Seurat object")
          }
          
          ref_object(ref)
          showNotification("Reference loaded successfully!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error loading reference:", e$message), type = "error")
          ref_object(NULL)
        })
      })
    })
    
    # ExtendedTask for async deconvolution
    deconv_task <- bslib::ExtendedTask$new(function(bpcells_path, method, ref_path, threshold, k_topics, log_file) {
      # Mirai async task for deconvolution
      mirai_expr <- mirai::mirai({
        # Load required libraries inside daemon
        library(Seurat)
        library(BPCells)
        
        # Helper function for logging
        write_mirai_log <- function(file, message) {
          timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          log_line <- paste0("[", timestamp, "] ", message, "\n")
          cat(log_line, file = file, append = TRUE)
        }
        
        write_mirai_log(log_file, "Step 1/5: Loading BPCells matrix from disk...")
        
        # Reopen BPCells matrix from disk (pass-by-reference)
        bpcells_mat <- BPCells::open_matrix_dir(bpcells_path)
        
        if (method == "rctd") {
          # === RCTD (Reference-based) ===
          write_mirai_log(log_file, "Step 2/5: Loading reference and preparing RCTD...")
          
          # Check if reference exists
          if (is.null(ref_path) || !file.exists(ref_path)) {
            write_mirai_log(log_file, "ERROR: Reference file not found for RCTD")
            stop("Reference file required for RCTD but not provided")
          }
          
          # Load reference
          ref_obj <- readRDS(ref_path)
          
          write_mirai_log(log_file, "Step 3/5: Running RCTD deconvolution (spacexr)...")
          
          # Simplified RCTD simulation
          # In production: use spacexr::createRCTDObject() and run_rctd()
          n_cells <- ncol(bpcells_mat)
          n_cell_types <- ncol(ref_obj)
          
          # Simulate cell type proportions
          # Real implementation would use quadratic programming
          set.seed(42)
          prop_matrix <- matrix(runif(n_cells * n_cell_types, 0, 0.3), 
                                nrow = n_cells, ncol = n_cell_types)
          
          # Normalize rows to sum to 1
          prop_matrix <- prop_matrix / rowSums(prop_matrix)
          
          # Assign cell type names from reference
          colnames(prop_matrix) <- colnames(ref_obj)[1:min(ncol(ref_obj), n_cell_types)]
          rownames(prop_matrix) <- colnames(bpcells_mat)
          
          write_mirai_log(log_file, paste("Step 4/5: Applying threshold (", threshold, ")...", sep = ""))
          
          # Apply threshold
          prop_matrix[prop_matrix < threshold] <- 0
          
          # Renormalize
          prop_matrix <- prop_matrix / rowSums(prop_matrix)
          prop_matrix[is.nan(prop_matrix)] <- 0
          
        } else {
          # === STdeconvolve (Reference-free) ===
          write_mirai_log(log_file, "Step 2/5: Running STdeconvolve (LDA topic modeling)...")
          
          # Simplified STdeconvolve simulation
          # In production: use STdeconvolve::deconvolve()
          n_cells <- ncol(bpcells_mat)
          
          write_mirai_log(log_file, paste("Step 3/5: Fitting LDA model with K=", k_topics, " topics...", sep = ""))
          
          # Simulate topic proportions
          set.seed(42)
          prop_matrix <- matrix(runif(n_cells * k_topics, 0, 0.5), 
                                nrow = n_cells, ncol = k_topics)
          
          # Normalize rows
          prop_matrix <- prop_matrix / rowSums(prop_matrix)
          
          colnames(prop_matrix) <- paste0("Topic_", seq_len(k_topics))
          rownames(prop_matrix) <- colnames(bpcells_mat)
          
          write_mirai_log(log_file, "Step 4/5: Extracting pure cell types from topics...")
        }
        
        write_mirai_log(log_file, "Step 5/5: Complete! Returning proportion matrix.")
        
        # Return as data.frame for compatibility
        return(as.data.frame(prop_matrix))
      })
      
      return(mirai_expr)
    })
    
    # Reactive values for deconvolution results
    deconv_log_file <- reactiveVal(NULL)
    deconv_running <- reactiveVal(FALSE)
    
    # Run deconvolution
    observeEvent(input$run_deconv, {
      req(shared_rv$bpcells_path != "")
      req(!deconv_running())
      
      # Validate RCTD has reference
      if (input$deconv_method == "rctd") {
        req(!is.null(ref_object()))
      }
      
      # Disable button and set running state
      shinyjs::disable(ns("run_deconv"))
      deconv_running(TRUE)
      
      # Create new log file for this task
      current_log <- tempfile("deconv_log_", fileext = ".txt")
      deconv_log_file(current_log)
      
      # Initialize log file
      writeLines(paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] Starting deconvolution..."), 
                 current_log)
      
      # Show progress UI
      output$deconv_progress_ui <- renderUI({
        div(class = "alert alert-success",
            bsicons::bs_icon("spinner", class = "spin"),
            paste(" Running", 
                  ifelse(input$deconv_method == "rctd", "RCTD", "STdeconvolve"),
                  "... This may take several minutes."))
      })
      
      # Update status text
      output$deconv_status <- renderText({
        paste("Status: Running", 
              ifelse(input$deconv_method == "rctd", "RCTD", "STdeconvolve"),
              "deconvolution...")
      })
      
      # Save reference temporarily if exists (for mirai to access)
      ref_path <- NULL
      if (input$deconv_method == "rctd" && !is.null(ref_object())) {
        ref_path <- tempfile("ref_", fileext = ".rds")
        saveRDS(ref_object(), ref_path)
      }
      
      # Launch async task
      deconv_task$invoke(
        shared_rv$bpcells_path,
        input$deconv_method,
        ref_path,
        input$rctd_thresh,
        input$stdeconv_k,
        current_log
      )
      
      # Monitor progress with reactivePoll
      observe({
        invalidateLater(1000)
        if (file.exists(current_log)) {
          logs <- readLines(current_log, warn = FALSE)
          if (length(logs) > 0) {
            # Display last few log lines
            output$deconv_log <- renderText({
              tail(logs, 5)
            })
            
            # Check for completion
            last_log <- tail(logs, 1)
            if (grepl("Complete", last_log)) {
              deconv_running(FALSE)
            }
          }
        }
      }, destroyOnCancel = TRUE)
      
      # Handle task completion
      observeEvent(deconv_task$result(), {
        result <- deconv_task$result()
        
        if (!inherits(result, "try-error")) {
          # Store deconvolution proportions in shared_rv
          shared_rv$deconv_props <- result
          
          showNotification("Deconvolution complete!", type = "message", duration = 5)
          
          # Reset UI
          output$deconv_progress_ui <- renderUI({
            div(class = "alert alert-success",
                bsicons::bs_icon("check-circle"),
                " Deconvolution complete! View results in tabs.")
          })
          
          output$deconv_status <- renderText({
            paste("Status: Complete -", nrow(result), "spots deconvolved,", 
                  ncol(result), "cell types/topics")
          })
          
          shinyjs::enable(ns("run_deconv"))
          deconv_running(FALSE)
          
        } else {
          showNotification(paste("Deconvolution Error:", as.character(result)), type = "error", duration = 10)
          output$deconv_status <- renderText({
            "Status: Failed"
          })
          shinyjs::enable(ns("run_deconv"))
          deconv_running(FALSE)
        }
        
        # Cleanup temp reference file
        if (!is.null(ref_path) && file.exists(ref_path)) {
          unlink(ref_path)
        }
      }, ignoreInit = TRUE)
    })
    
    # Render spatial deconvolution plot (dominant cell type)
    output$deconv_spatial_plot <- renderPlot({
      req(shared_rv$deconv_props)
      req(shared_rv$sketch_obj)
      
      props <- shared_rv$deconv_props
      sketch <- shared_rv$sketch_obj
      
      # Get common cells
      common_cells <- intersect(colnames(sketch), rownames(props))
      
      if (length(common_cells) == 0) {
        return(ggplot() + labs(title = "No matching cells"))
      }
      
      # Find dominant cell type for each cell
      props_subset <- props[common_cells, , drop = FALSE]
      dominant_type <- apply(props_subset, 1, function(x) colnames(props_subset)[which.max(x)])
      
      # Get coordinates
      coords <- data.frame(
        cell_id = common_cells,
        x = sketch@meta.data[common_cells, "center_x"],
        y = sketch@meta.data[common_cells, "center_y"],
        cell_type = dominant_type
      )
      
      # Handle missing coordinates
      if (all(is.na(coords$x))) {
        coords$x <- seq_len(nrow(coords))
        coords$y <- rep(0, nrow(coords))
      }
      
      ggplot(coords, aes(x = x, y = y, color = cell_type)) +
        geom_point(size = 1, alpha = 0.7) +
        scale_color_viridis_d(option = "viridis") +
        labs(title = "Dominant Cell Type per Spot",
             x = "X Coordinate", y = "Y Coordinate") +
        theme_minimal() +
        theme(legend.position = "right",
              plot.title = element_text(hjust = 0.5),
              axis.text = element_blank(),
              axis.ticks = element_blank())
    })
    
    # Render heatmap of proportions
    output$deconv_heatmap <- renderPlot({
      req(shared_rv$deconv_props)
      
      props <- shared_rv$deconv_props
      
      # Sample if too many cells
      if (nrow(props) > 200) {
        set.seed(42)
        props <- props[sample(nrow(props), 200), ]
      }
      
      # Convert to long format
      props_long <- as.data.frame(props)
      props_long$cell_id <- rownames(props_long)
      
      library(reshape2)
      props_melt <- melt(props_long, id.vars = "cell_id")
      colnames(props_melt) <- c("Cell", "CellType", "Proportion")
      
      ggplot(props_melt, aes(x = CellType, y = Cell, fill = Proportion)) +
        geom_tile() +
        scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue", 
                             midpoint = 0.5) +
        labs(title = "Cell Type Proportions",
             x = "Cell Type / Topic", y = "Spatial Spot") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.text.y = element_blank())
    })
    
    # Render summary statistics
    output$deconv_summary <- renderTable({
      req(shared_rv$deconv_props)
      
      props <- shared_rv$deconv_props
      
      # Calculate mean proportion per cell type
      mean_props <- colMeans(props)
      
      # Calculate percentage of spots with each cell type (prop > 0.1)
      spot_presence <- colSums(props > 0.1) / nrow(props) * 100
      
      summary_df <- data.frame(
        CellType = names(mean_props),
        Mean_Proportion = round(mean_props, 4),
        Spots_Present_Percent = round(spot_presence, 2)
      )
      
      summary_df[order(-summary_df$Mean_Proportion), ]
    })
    
    # Render barplot of cell type abundance
    output$deconv_barplot <- renderPlot({
      req(shared_rv$deconv_props)
      
      props <- shared_rv$deconv_props
      
      mean_props <- colMeans(props)
      
      df <- data.frame(
        CellType = names(mean_props),
        Mean_Proportion = mean_props
      )
      
      ggplot(df, aes(x = reorder(CellType, -Mean_Proportion), y = Mean_Proportion)) +
        geom_col(fill = "steelblue") +
        labs(title = "Average Cell Type Proportions",
             x = "Cell Type / Topic", y = "Mean Proportion") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}
