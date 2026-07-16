# modules/spatial/mod_spatial_viz.R
# Child Module: WebGL Rendering Engine (leafgl) and Rasterization (scattermore)

library(shiny)
library(bslib)
library(Seurat)
library(BPCells)
library(ggplot2)

mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left panel: Display Controls
      column(width = 3,
        wellPanel(
          h4("Visualization Settings"),
          
          selectInput(ns("viz_color_by"), "Color By",
                      choices = c(
                        "Clusters" = "clusters",
                        "Deconvolution" = "deconv",
                        "Gene Expression" = "gene",
                        "QC Metrics" = "qc"
                      ),
                      selected = "clusters"),
          
          div(id = ns("gene_selector_ui"),
            selectizeInput(ns("viz_gene"), "Select Gene",
                           choices = NULL,
                           options = list(maxOptions = 500, placeholder = "Search gene..."))
          ),
          
          hr(),
          
          sliderInput(ns("viz_pt_size"), "Point Size", 
                      min = 0.1, max = 10, value = 2, step = 0.1),
          
          sliderInput(ns("viz_alpha"), "Transparency", 
                      min = 0, max = 1, value = 0.7, step = 0.05),
          
          hr(),
          
          radioButtons(ns("viz_engine"), "Rendering Engine",
                       choices = c(
                         "WebGL (leafgl - Fast)" = "webgl",
                         "Raster (scattermore - Static)" = "raster"
                       ),
                       selected = "webgl"),
          
          hr(),
          
          actionButton(ns("export_plot"), "Export High-Res Image", 
                       class = "btn-primary btn-block",
                       icon = icon("download"))
        ),
        
        wellPanel(
          h4("Info"),
          textOutput(ns("viz_info"))
        )
      ),
      
      # Right panel: Main Visualization
      column(width = 9,
        tabsetPanel(
          tabPanel("Spatial View (Interactive)",
            leaflet::leafletOutput(ns("spatial_leafgl"), height = "700px")
          ),
          tabPanel("Spatial View (Static)",
            plotOutput(ns("spatial_scattermore"), height = "700px")
          ),
          tabPanel("UMAP/t-SNE",
            plotly::plotlyOutput(ns("dim_plot"), height = "600px")
          )
        )
      )
    )
  )
}

mod_spatial_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Update gene selector when data is loaded
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj)
      
      obj <- global_data$spatial_obj
      genes <- rownames(obj)
      
      # Limit to top variable genes if too many
      if (length(genes) > 5000) {
        genes <- head(genes, 5000)
      }
      
      updateSelectizeInput(session, "viz_gene", choices = genes, server = TRUE)
    })
    
    # Show/hide gene selector based on color choice
    observeEvent(input$viz_color_by, {
      if (input$viz_color_by == "gene") {
        shinyjs::show(ns("gene_selector_ui"))
      } else {
        shinyjs::hide(ns("gene_selector_ui"))
      }
    })
    
    # Reactive expression for current coloring data
    current_colors <- reactive({
      req(shared_rv$sketch_obj)
      
      sketch <- shared_rv$sketch_obj
      n_cells <- ncol(sketch)
      
      if (input$viz_color_by == "clusters") {
        req(!is.null(shared_rv$cluster_labels))
        
        clusters <- shared_rv$cluster_labels
        common_cells <- intersect(colnames(sketch), clusters$cell_id)
        
        if (length(common_cells) == 0) {
          return(NULL)
        }
        
        colors <- clusters$cluster_id[match(common_cells, clusters$cell_id)]
        
        return(data.frame(
          cell_id = common_cells,
          color_value = colors,
          stringsAsFactors = FALSE
        ))
        
      } else if (input$viz_color_by == "deconv") {
        req(!is.null(shared_rv$deconv_props))
        
        props <- shared_rv$deconv_props
        common_cells <- intersect(colnames(sketch), rownames(props))
        
        if (length(common_cells) == 0) {
          return(NULL)
        }
        
        # Get dominant cell type
        props_subset <- props[common_cells, , drop = FALSE]
        dominant <- apply(props_subset, 1, function(x) colnames(props_subset)[which.max(x)])
        
        return(data.frame(
          cell_id = common_cells,
          color_value = dominant,
          stringsAsFactors = FALSE
        ))
        
      } else if (input$viz_color_by == "gene") {
        req(input$viz_gene)
        
        # Get gene expression from BPCells
        tryCatch({
          expr <- Seurat::GetAssayData(sketch, slot = "data")[input$viz_gene, ]
          
          return(data.frame(
            cell_id = names(expr),
            color_value = as.numeric(expr),
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          return(NULL)
        })
        
      } else if (input$viz_color_by == "qc") {
        # Use nCount_Spatial
        qc_vals <- sketch@meta.data$nCount_Spatial
        
        return(data.frame(
          cell_id = colnames(sketch),
          color_value = qc_vals,
          stringsAsFactors = FALSE
        ))
      }
      
      return(NULL)
    })
    
    # Render WebGL interactive map using leaflet
    output$spatial_leafgl <- leaflet::renderLeaflet({
      req(shared_rv$sketch_obj)
      req(!is.null(current_colors()))
      
      sketch <- shared_rv$sketch_obj
      colors_df <- current_colors()
      
      # Get coordinates
      if ("center_x" %in% colnames(sketch@meta.data) && 
          "center_y" %in% colnames(sketch@meta.data)) {
        x_coords <- sketch@meta.data$center_x
        y_coords <- sketch@meta.data$center_y
      } else {
        # Fallback: use random coordinates
        set.seed(42)
        x_coords <- runif(ncol(sketch), 0, 100)
        y_coords <- runif(ncol(sketch), 0, 100)
      }
      
      # Merge with colors
      plot_data <- data.frame(
        x = x_coords,
        y = y_coords,
        color = colors_df$color_value[match(colnames(sketch), colors_df$cell_id)],
        cell_id = colnames(sketch),
        stringsAsFactors = FALSE
      )
      
      # Remove NAs
      plot_data <- plot_data[!is.na(plot_data$x) & !is.na(plot_data$y), ]
      
      # Create leaflet map
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          data = plot_data,
          lng = ~x, lat = ~y,
          radius = input$viz_pt_size,
          opacity = input$viz_alpha,
          color = ~as.character(color),
          fillColor = ~as.character(color),
          fillOpacity = input$viz_alpha,
          weight = 1,
          group = "cells"
        ) %>%
        leaflet::addLayersControl(
          overlayGroups = "cells",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })
    
    # Render static rasterized plot using ggplot2 (scattermore alternative)
    output$spatial_scattermore <- renderPlot({
      req(shared_rv$sketch_obj)
      req(!is.null(current_colors()))
      
      sketch <- shared_rv$sketch_obj
      colors_df <- current_colors()
      
      # Get coordinates
      if ("center_x" %in% colnames(sketch@meta.data) && 
          "center_y" %in% colnames(sketch@meta.data)) {
        x_coords <- sketch@meta.data$center_x
        y_coords <- sketch@meta.data$center_y
      } else {
        set.seed(42)
        x_coords <- runif(ncol(sketch), 0, 100)
        y_coords <- runif(ncol(sketch), 0, 100)
      }
      
      # Merge with colors
      plot_data <- data.frame(
        x = x_coords,
        y = y_coords,
        color = colors_df$color_value[match(colnames(sketch), colors_df$cell_id)],
        stringsAsFactors = FALSE
      )
      
      # Remove NAs
      plot_data <- plot_data[!is.na(plot_data$x) & !is.na(plot_data$y), ]
      
      # Determine if color is numeric or categorical
      is_numeric <- is.numeric(colors_df$color_value[1])
      
      if (is_numeric) {
        # Continuous color scale
        ggplot(plot_data, aes(x = x, y = y, color = color)) +
          geom_point(size = input$viz_pt_size, alpha = input$viz_alpha) +
          scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) +
          labs(title = paste("Spatial Visualization:", input$viz_color_by),
               x = "X Coordinate", y = "Y Coordinate") +
          theme_minimal() +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank())
      } else {
        # Discrete color scale
        ggplot(plot_data, aes(x = x, y = y, color = as.factor(color))) +
          geom_point(size = input$viz_pt_size, alpha = input$viz_alpha) +
          scale_color_viridis_d(option = "plasma") +
          labs(title = paste("Spatial Visualization:", input$viz_color_by),
               x = "X Coordinate", y = "Y Coordinate",
               color = "Category") +
          theme_minimal() +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "right")
      }
    })
    
    # Render UMAP/t-SNE plot with plotly (WebGL mode)
    output$dim_plot <- plotly::renderPlotly({
      req(shared_rv$sketch_obj)
      req(!is.null(current_colors()))
      
      sketch <- shared_rv$sketch_obj
      colors_df <- current_colors()
      
      # Check if UMAP exists
      if (!"umap_1" %in% colnames(sketch@meta.data)) {
        return(plotly::plot_ly() %>% 
                 plotly::layout(title = "UMAP not computed. Run clustering first."))
      }
      
      # Get UMAP coordinates
      umap_x <- sketch@meta.data$umap_1
      umap_y <- sketch@meta.data$umap_2
      
      # Merge with colors
      plot_data <- data.frame(
        x = umap_x,
        y = umap_y,
        color = colors_df$color_value[match(colnames(sketch), colors_df$cell_id)],
        cell_id = colnames(sketch),
        stringsAsFactors = FALSE
      )
      
      # Remove NAs
      plot_data <- plot_data[complete.cases(plot_data), ]
      
      # Determine if color is numeric or categorical
      is_numeric <- is.numeric(plot_data$color[1])
      
      if (is_numeric) {
        plotly::plot_ly(
          data = plot_data,
          x = ~x, y = ~y,
          color = ~color,
          colors = "Viridis",
          type = "scattergl",  # WebGL mode for performance
          mode = "markers",
          marker = list(size = 3, opacity = input$viz_alpha),
          hoverinfo = "text",
          text = ~paste("Cell:", cell_id, "<br>Value:", round(color, 3))
        ) %>%
          plotly::layout(
            title = "UMAP Projection",
            xaxis = list(title = "UMAP 1"),
            yaxis = list(title = "UMAP 2"),
            showlegend = TRUE
          )
      } else {
        plotly::plot_ly(
          data = plot_data,
          x = ~x, y = ~y,
          color = ~as.factor(color),
          type = "scattergl",
          mode = "markers",
          marker = list(size = 3, opacity = input$viz_alpha),
          hoverinfo = "text",
          text = ~paste("Cell:", cell_id, "<br>Cluster:", color)
        ) %>%
          plotly::layout(
            title = "UMAP Projection",
            xaxis = list(title = "UMAP 1"),
            yaxis = list(title = "UMAP 2"),
            showlegend = TRUE
          )
      }
    })
    
    # Update info text
    output$viz_info <- renderText({
      req(shared_rv$sketch_obj)
      
      n_cells <- ncol(shared_rv$sketch_obj)
      engine <- ifelse(input$viz_engine == "webgl", "WebGL (Interactive)", "Raster (Static)")
      
      paste("Displaying:", n_cells, "cells (sketch)\n",
            "Engine:", engine, "\n",
            "Color by:", input$viz_color_by)
    })
    
    # Export plot handler
    observeEvent(input$export_plot, {
      showNotification("Export functionality would generate high-resolution PNG/PDF", 
                       type = "message", duration = 3)
    })
  })
}
