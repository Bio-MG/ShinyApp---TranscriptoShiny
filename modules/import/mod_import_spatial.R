# modules/mod_import_spatial.R - Import Spatial Transcriptomics

mod_import_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import Spatial Transcriptomics",
        
        div(class = "alert alert-info", style = "font-size: 0.85rem;",
            bsicons::bs_icon("info-circle"), 
            " Importez des données de transcriptomique spatiale (10X Visium, etc.)."),
        
        accordion(
          accordion_panel(
            "Option A: Dossier 10X Visium",
            icon = icon("folder"),
            
            shinyDirButton(ns("visium_dir"), "📁 Sélectionner Dossier Visium", 
                           "Dossier contenant 'filtered_feature_bc_matrix' et 'spatial'", 
                           class = "btn-secondary w-100", icon = icon("folder-open")),
            
            verbatimTextOutput(ns("visium_path_display"), placeholder = TRUE),
            
            helpText("Structure attendue :"),
            tags$ul(style = "font-size: 0.85rem;",
                    tags$li("filtered_feature_bc_matrix/ (matrix.mtx, features.tsv, barcodes.tsv)"),
                    tags$li("spatial/ (tissue_positions.csv, scalefactors_json.json, images/)")
            ),
            
            textInput(ns("sample_name_visium"), "Nom de l'échantillon", 
                      placeholder = "Ex: Visium_Sample1"),
            
            actionButton(ns("btn_load_visium"), "🚀 Charger Visium", 
                         class = "btn-success w-100 mt-2", icon = icon("play"))
          ),
          
          accordion_panel(
            "Option B: Fichier Spatial (.rds)",
            icon = icon("file"),
            
            fileInput(ns("spatial_file"), "Fichier Spatial Seurat (.rds)", 
                      accept = ".rds"),
            
            helpText("Objet Seurat déjà créé avec des données spatiales."),
            
            actionButton(ns("btn_load_spatial_file"), "Charger Fichier", 
                         class = "btn-primary w-100")
          ),
          
          accordion_panel(
            "Options Avancées",
            icon = icon("cogs"),
            
            numericInput(ns("min_counts_spatial"), "Counts minimum par spot", 
                         value = 100, min = 0, step = 10),
            
            numericInput(ns("min_features"), "Features minimum par spot", 
                         value = 200, min = 0, step = 50),
            
            sliderInput(ns("image_scale"), "Échelle de l'image", 
                        min = 0.5, max = 2, value = 1, step = 0.1),
            
            helpText("Ajuste la taille de l'image histologique pour la visualisation.")
          )
        )
      ),
      
      # Zone principale : Résumé
      card(
        card_header("Résumé des Données Spatiales"),
        layout_columns(
          value_box(
            title = "Spots",
            value = textOutput(ns("nb_spots")),
            showcase = bsicons::bs_icon("grid-3x3-gap"),
            theme = "primary"
          ),
          value_box(
            title = "Gènes",
            value = textOutput(ns("nb_genes")),
            showcase = bsicons::bs_icon("diagram-3"),
            theme = "secondary"
          ),
          value_box(
            title = "Échantillons",
            value = textOutput(ns("nb_samples")),
            showcase = bsicons::bs_icon("collection"),
            theme = "info"
          ),
          value_box(
            title = "Statut",
            value = textOutput(ns("status_obj")),
            showcase = bsicons::bs_icon("check-circle"),
            theme = "light"
          )
        ),
        
        # Aperçu de l'image spatiale
        card_body(
          h5("Aperçu de l'Image Histologique", class = "text-muted"),
          plotOutput(ns("spatial_preview"), height = "400px"),
          
          hr(),
          
          h5("Console de Log", class = "text-muted"),
          verbatimTextOutput(ns("console_log"), placeholder = TRUE)
        )
      )
    )
  )
}

mod_import_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # Logger
    logs <- reactiveVal("En attente d'import spatial...")
    add_log <- function(msg) { 
      timestamp <- format(Sys.time(), "%H:%M:%S")
      logs(paste0("[", timestamp, "] ", msg, "\n", logs())) 
    }
    
    # Navigation shinyFiles
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "visium_dir", roots = volumes, session = session)
    
    visium_path <- reactiveVal(NULL)
    observeEvent(input$visium_dir, {
      req(input$visium_dir)
      path <- parseDirPath(volumes, input$visium_dir)
      if (length(path) > 0) {
        visium_path(path)
        add_log(paste("Dossier Visium sélectionné:", path))
      }
    })
    
    output$visium_path_display <- renderText({ 
      if(is.null(visium_path())) "Aucun dossier sélectionné" else visium_path() 
    })
    
    # --- IMPORT VISIUM (Option A) ---
    observeEvent(input$btn_load_visium, {
      req(visium_path(), input$sample_name_visium)
      
      if(nchar(trimws(input$sample_name_visium)) == 0) {
        showNotification("⚠️ Veuillez entrer un nom d'échantillon.", type = "warning")
        return()
      }
      
      add_log(paste("🔄 Import Visium:", visium_path()))
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Chargement Visium...", value = 0.3)
      
      tryCatch({
        # Vérifier la structure du dossier
        matrix_dir <- file.path(visium_path(), "filtered_feature_bc_matrix")
        spatial_dir <- file.path(visium_path(), "spatial")
        
        if(!dir.exists(matrix_dir)) {
          stop("Dossier 'filtered_feature_bc_matrix' introuvable. Vérifiez la structure.")
        }
        
        if(!dir.exists(spatial_dir)) {
          stop("Dossier 'spatial' introuvable. Vérifiez la structure.")
        }
        
        # Charger les données avec Seurat
        progress$set(value = 0.5, detail = "Lecture des données...")
        add_log("  Lecture de la matrice de counts...")
        
        spatial_obj <- Load10X_Spatial(
          data.dir = visium_path(),
          filename = "filtered_feature_bc_matrix.h5",
          assay = "Spatial",
          slice = input$sample_name_visium,
          filter.matrix = TRUE
        )
        
        # Ajout du nom d'échantillon
        spatial_obj$orig.ident <- input$sample_name_visium
        
        # Filtrage QC
        progress$set(value = 0.7, detail = "Filtrage QC...")
        add_log(paste("  Spots avant filtrage:", ncol(spatial_obj)))
        
        spatial_obj <- subset(spatial_obj, 
                              subset = nCount_Spatial >= input$min_counts_spatial & 
                                nFeature_Spatial >= input$min_features)
        
        add_log(paste("  Spots après filtrage:", ncol(spatial_obj)))
        
        # Stocker dans global_data
        if(is.null(global_data$spatial_obj)) {
          global_data$spatial_obj <- spatial_obj
        } else {
          # Fusionner avec des objets existants
          global_data$spatial_obj <- merge(global_data$spatial_obj, spatial_obj)
        }
        
        add_log(paste("✅ Import réussi!", ncol(spatial_obj), "spots,", 
                      nrow(spatial_obj), "gènes"))
        
        showNotification(
          paste("✅ Import réussi:", ncol(spatial_obj), "spots"), 
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        add_log(paste("❌ Erreur:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    })
    
    # --- IMPORT FICHIER SPATIAL (Option B) ---
    observeEvent(input$btn_load_spatial_file, {
      req(input$spatial_file)
      
      add_log("🔄 Import fichier spatial .rds...")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Chargement...", value = 0.5)
      
      tryCatch({
        spatial_obj <- readRDS(input$spatial_file$datapath)
        
        # Vérifier que c'est un objet Seurat spatial
        if(!inherits(spatial_obj, "Seurat")) {
          stop("Le fichier doit contenir un objet Seurat.")
        }
        
        if(!"Spatial" %in% names(spatial_obj@assays) && 
           !"spatial" %in% names(spatial_obj@images)) {
          warning("L'objet ne semble pas contenir de données spatiales.")
        }
        
        global_data$spatial_obj <- spatial_obj
        
        add_log(paste("✅ Import réussi!", ncol(spatial_obj), "spots"))
        showNotification("✅ Import réussi!", type = "message")
        
      }, error = function(e) {
        add_log(paste("❌ Erreur:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    })
    
    # --- APERÇU IMAGE SPATIALE ---
    output$spatial_preview <- renderPlot({
      req(global_data$spatial_obj)
      
      tryCatch({
        SpatialPlot(global_data$spatial_obj, 
                    pt.size.factor = input$image_scale,
                    group.by = "orig.ident") +
          ggtitle("Aperçu de l'Image Histologique") +
          theme_minimal()
        
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 1, y = 1, 
                   label = "Aucune image spatiale disponible", 
                   size = 5, color = "grey50") +
          theme_void()
      })
    })
    
    # --- OUTPUTS INFO ---
    output$nb_spots <- renderText({ 
      if(is.null(global_data$spatial_obj)) "-" else ncol(global_data$spatial_obj)
    })
    
    output$nb_genes <- renderText({ 
      if(is.null(global_data$spatial_obj)) "-" else nrow(global_data$spatial_obj)
    })
    
    output$nb_samples <- renderText({
      if(is.null(global_data$spatial_obj)) {
        "-"
      } else {
        length(unique(global_data$spatial_obj$orig.ident))
      }
    })
    
    output$status_obj <- renderText({ 
      if(is.null(global_data$spatial_obj)) {
        "⚪ Inactif"
      } else {
        "🟢 Chargé"
      }
    })
    
    output$console_log <- renderText({ logs() })
  })
}