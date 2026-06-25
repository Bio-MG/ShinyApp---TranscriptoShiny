# modules/mod_spatial.R

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Pipeline Spatial",
        
        # Validation visual
        uiOutput(ns("spatial_status_ui")),
        
        accordion(
          id = ns("acc_spatial"),
          open = "1. QC & Norm",
          
          # Etape 1: QC & Normalisation
          accordion_panel("1. QC & Norm", icon = icon("filter"),
                          numericInput(ns("qc_min_features"), "Min Features", 200, step=50),
                          sliderInput(ns("qc_mt"), "% Mito Max", 0, 50, 15, step=1),
                          radioButtons(ns("norm_method"), "Normalisation", 
                                       choices = c("LogNormalize" = "log", "SCTransform" = "sct")),
                          actionButton(ns("run_spatial_qc"), "Lancer QC/Norm", class = "btn-danger w-100")
          ),
          
          # Etape 2: Clustering
          accordion_panel("2. Clustering", icon = icon("shapes"),
                          sliderInput(ns("pca_dim"), "Dims PCA", 5, 50, 20),
                          numericInput(ns("clust_res"), "Résolution", 0.5, step=0.1),
                          actionButton(ns("run_spatial_cluster"), "Lancer Clustering", class = "btn-warning w-100")
          ),
          
          # Etape 3: Paramètres Visuels
          accordion_panel("3. Visualisation", icon = icon("eye"),
                          selectInput(ns("sp_image"), "Image / Slice", choices = NULL),
                          
                          div(style = "display: flex; align-items: center; justify-content: space-between;",
                              tags$label("Feature à visualiser:", class = "control-label"),
                              tooltip(
                                bsicons::bs_icon("info-circle"),
                                "Entrez un gène (ex: ACTB) ou une métadonnée (ex: nCount_Spatial)"
                              )
                          ),
                          selectizeInput(ns("sp_gene"), NULL, choices = NULL, 
                                         options = list(maxOptions=3000, placeholder = "Rechercher...")),
                          
                          sliderInput(ns("pt_size"), "Taille Points", 0.5, 10, 1.6, step=0.1),
                          sliderInput(ns("alpha"), "Transparence", 0, 1, 1, step=0.1)
          )
        )
      ),
      
      # Visualization Tabs
      navset_card_underline(
        nav_panel("Expression Spatiale (FeaturePlot)", 
                  plotOutput(ns("spatial_plot"), height = "700px")
        ),
        nav_panel("Clusters Spatiaux (DimPlot)", 
                  plotOutput(ns("spatial_dim_plot"), height = "700px")
        ),
        nav_panel("QC Métriques",
                  plotOutput(ns("spatial_qc_plot"), height = "400px")
        )
      )
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # 0. Data Status UI
    output$spatial_status_ui <- renderUI({
      if(is.null(global_data$spatial_obj)) {
        div(class = "alert alert-danger", 
            bsicons::bs_icon("exclamation-triangle"),
            " Aucune donnée spatiale chargée. Allez dans l'onglet 'Import Données > Spatial'.")
      } else {
        div(class = "alert alert-success",
            bsicons::bs_icon("check-circle"),
            paste(" Objet Spatial:", ncol(global_data$spatial_obj), "spots"))
      }
    })
    
    # --- Helpers ---
    observe({
      req(global_data$spatial_obj)
      obj <- global_data$spatial_obj
      
      # 1. Update Images
      imgs <- Images(obj)
      if(length(imgs) > 0) {
        updateSelectInput(session, "sp_image", choices = imgs, selected = imgs[1])
      }
      
      # 2. Update Features (Genes + Metadata)
      # Optimization: Do not load all genes at once if > 30k
      # Here we load metadata + a subset or rely on server-side selectize if needed
      metadata_cols <- colnames(obj@meta.data)
      # For genes, we rely on server-side updating for performance if needed, 
      # but for now we bind them to the object rownames.
      
      updateSelectizeInput(session, "sp_gene", choices = c(metadata_cols, rownames(obj)), server = TRUE)
    })
    
    # --- 1. QC & Norm Pipeline ---
    observeEvent(input$run_spatial_qc, {
      req(global_data$spatial_obj)
      obj <- global_data$spatial_obj
      
      withProgress(message = "Spatial QC & Norm...", {
        tryCatch({
          # QC
          obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = "^MT-")
          obj <- subset(obj, subset = nFeature_Spatial > input$qc_min_features & percent.mt < input$qc_mt)
          
          if(ncol(obj) == 0) stop("Tous les spots ont été filtrés. Vérifiez vos seuils.")
          
          # Norm
          if (input$norm_method == "sct") {
            obj <- SCTransform(obj, assay = "Spatial", verbose = FALSE)
          } else {
            DefaultAssay(obj) <- "Spatial"
            obj <- NormalizeData(obj, assay = "Spatial")
            obj <- ScaleData(obj)
          }
          
          global_data$spatial_obj <- obj
          showNotification("Spatial QC & Normalisation terminés", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Erreur Spatial QC:", e$message), type = "error")
        })
      })
    })
    
    # --- 2. Clustering Pipeline ---
    observeEvent(input$run_spatial_cluster, {
      req(global_data$spatial_obj)
      obj <- global_data$spatial_obj
      
      withProgress(message = "Spatial Clustering...", {
        tryCatch({
          # Check which assay to use
          assay_use <- if("SCT" %in% names(obj@assays)) "SCT" else "Spatial"
          DefaultAssay(obj) <- assay_use
          
          if(assay_use == "Spatial") {
            obj <- FindVariableFeatures(obj)
            obj <- ScaleData(obj)
          }
          
          obj <- RunPCA(obj, assay = assay_use, verbose = FALSE, npcs = input$pca_dim)
          obj <- FindNeighbors(obj, dims = 1:input$pca_dim)
          obj <- FindClusters(obj, resolution = input$clust_res)
          obj <- RunUMAP(obj, dims = 1:input$pca_dim)
          
          global_data$spatial_obj <- obj
          showNotification("Spatial Clustering terminé", type = "message")
          
        }, error = function(e) {
          showNotification(paste("Erreur Clustering:", e$message), type = "error")
        })
      })
    })
    
    # --- 3. Visualizations ---
    output$spatial_plot <- renderPlot({
      req(global_data$spatial_obj, input$sp_gene)
      obj <- global_data$spatial_obj
      
      # Handle image selection
      img_use <- input$sp_image
      if(is.null(img_use) || !img_use %in% Images(obj)) {
        img_use <- Images(obj)[1] # Fallback
      }
      
      validate(need(!is.null(img_use), "Aucune image spatiale disponible."))
      
      SpatialFeaturePlot(obj, features = input$sp_gene, images = img_use,
                         pt.size.factor = input$pt_size, alpha = c(input$alpha, 1)) +
        theme(legend.position = "right")
    })
    
    output$spatial_dim_plot <- renderPlot({
      req(global_data$spatial_obj)
      obj <- global_data$spatial_obj
      
      img_use <- input$sp_image
      if(is.null(img_use) || !img_use %in% Images(obj)) {
        img_use <- Images(obj)[1]
      }
      
      SpatialDimPlot(obj, images = img_use, pt.size.factor = input$pt_size, alpha = c(input$alpha, 1))
    })
    
    output$spatial_qc_plot <- renderPlot({
      req(global_data$spatial_obj)
      VlnPlot(global_data$spatial_obj, features = c("nFeature_Spatial", "nCount_Spatial", "percent.mt"), 
              ncol = 3, pt.size = 0.1)
    })
    
  })
}