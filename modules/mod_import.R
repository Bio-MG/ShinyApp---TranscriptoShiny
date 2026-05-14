# modules/mod_import.R - VERSION MULTI-ÉCHANTILLONS

mod_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import des Données",
        
        accordion(
          accordion_panel(
            "Option A: Dossiers Multiples (10X)",
            div(class = "alert alert-info", style = "font-size: 0.85rem;",
                bsicons::bs_icon("info-circle"), 
                " Importez plusieurs échantillons pour Harmony (correction de batch)."),
            
            shinyDirButton(ns("dir_select"), "📁 Ajouter un Dossier", 
                           "Sélectionner dossier contenant matrix.mtx", 
                           class = "btn-secondary w-100", icon = icon("folder-open")),
            
            # Nom de l'échantillon
            textInput(ns("sample_name"), "Nom de l'échantillon", 
                      placeholder = "Ex: Patient1, Control_A"),
            
            actionButton(ns("btn_add_sample"), "➕ Ajouter à la liste", 
                         class = "btn-info w-100 mt-2"),
            
            hr(),
            
            # Liste des échantillons ajoutés
            h6("Échantillons ajoutés:", style = "font-weight: bold;"),
            div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
                uiOutput(ns("sample_list_display"))
            ),
            
            actionButton(ns("btn_clear_samples"), "🗑️ Tout Effacer", 
                         class = "btn-outline-danger btn-sm w-100 mt-2"),
            
            hr(),
            
            helpText("Pour les sorties CellRanger (matrix, features, barcodes)."),
            verbatimTextOutput(ns("path_display"), placeholder = TRUE),
            actionButton(ns("btn_load_dir"), "🚀 Charger Tous les Échantillons", 
                         class = "btn-success w-100 mt-2", icon = icon("play"))
          ),
          
          accordion_panel(
            "Option B: Fichiers Multiples (.rds, .h5)",
            div(class = "alert alert-info", style = "font-size: 0.85rem;",
                bsicons::bs_icon("info-circle"), 
                " Importez plusieurs fichiers pour les fusionner."),
            
            fileInput(ns("file_upload"), "Ajouter Fichier(s)", 
                      accept = c(".rds", ".h5", ".h5ad", ".loom"),
                      multiple = TRUE),
            
            # Liste des fichiers uploadés
            h6("Fichiers uploadés:", style = "font-weight: bold;"),
            div(style = "max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
                uiOutput(ns("file_list_display"))
            ),
            
            actionButton(ns("btn_load_file"), "🚀 Charger Tous les Fichiers", 
                         class = "btn-primary w-100", icon = icon("play"))
          ),
          
          accordion_panel(
            "Option C: Fichier Unique (Classique)",
            fileInput(ns("single_file_upload"), "Charger un seul fichier", 
                      accept = c(".rds", ".h5", ".h5ad", ".loom")),
            helpText("Pour un seul échantillon (Harmony non disponible)."),
            actionButton(ns("btn_load_single"), "Charger", 
                         class = "btn-warning w-100")
          )
        )
      ),
      
      # Zone principale : Résumé
      card(
        card_header("Résumé de l'objet chargé"),
        layout_columns(
          value_box(
            title = "Cellules",
            value = textOutput(ns("nb_cells")),
            showcase = bsicons::bs_icon("people"),
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
        card_body(
          h5("Console de Log", class = "text-muted"),
          verbatimTextOutput(ns("console_log"), placeholder = TRUE)
        )
      )
    )
  )
}

mod_import_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # Logger
    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) { 
      timestamp <- format(Sys.time(), "%H:%M:%S")
      logs(paste0("[", timestamp, "] ", msg, "\n", logs())) 
    }
    
    # Stockage des échantillons
    sample_list <- reactiveVal(list())
    
    # Navigation shinyFiles
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "dir_select", roots = volumes, session = session)
    
    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      req(input$dir_select)
      path <- parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) {
        dir_path(path)
        add_log(paste("Dossier sélectionné:", path))
      }
    })
    
    output$path_display <- renderText({ 
      if(is.null(dir_path())) "Aucun dossier sélectionné" else dir_path() 
    })
    
    # --- AJOUT D'UN ÉCHANTILLON À LA LISTE ---
    observeEvent(input$btn_add_sample, {
      req(dir_path(), input$sample_name)
      
      if(nchar(trimws(input$sample_name)) == 0) {
        showNotification("⚠️ Veuillez entrer un nom d'échantillon valide.", type = "warning")
        return()
      }
      
      # Vérifier si le nom existe déjà
      current_samples <- sample_list()
      if(input$sample_name %in% names(current_samples)) {
        showNotification("⚠️ Ce nom d'échantillon existe déjà.", type = "warning")
        return()
      }
      
      # Ajouter à la liste
      current_samples[[input$sample_name]] <- dir_path()
      sample_list(current_samples)
      
      add_log(paste("✓ Échantillon ajouté:", input$sample_name, "→", dir_path()))
      showNotification(paste("✓", input$sample_name, "ajouté"), type = "message")
      
      # Réinitialiser les inputs
      dir_path(NULL)
      updateTextInput(session, "sample_name", value = "")
    })
    
    # --- AFFICHAGE DE LA LISTE DES ÉCHANTILLONS ---
    output$sample_list_display <- renderUI({
      samples <- sample_list()
      
      if(length(samples) == 0) {
        return(tags$em("Aucun échantillon ajouté", style = "color: #999;"))
      }
      
      tags$ul(
        style = "list-style: none; padding: 0; margin: 0;",
        lapply(names(samples), function(name) {
          tags$li(
            style = "padding: 5px; border-bottom: 1px solid #eee;",
            tags$strong(name), 
            tags$br(),
            tags$small(samples[[name]], style = "color: #666;")
          )
        })
      )
    })
    
    # --- EFFACER TOUS LES ÉCHANTILLONS ---
    observeEvent(input$btn_clear_samples, {
      sample_list(list())
      add_log("Liste des échantillons effacée")
      showNotification("Liste effacée", type = "message")
    })
    
    # --- IMPORT DOSSIERS MULTIPLES ---
    observeEvent(input$btn_load_dir, {
      req(sample_list())
      samples <- sample_list()
      
      if(length(samples) == 0) {
        showNotification("⚠️ Aucun échantillon à charger.", type = "warning")
        return()
      }
      
      add_log(paste("🔄 Import de", length(samples), "échantillon(s)..."))
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Chargement des échantillons...", value = 0)
      
      tryCatch({
        obj_list <- list()
        
        for(i in seq_along(samples)) {
          sample_name <- names(samples)[i]
          sample_path <- samples[[i]]
          
          progress$set(value = i/length(samples), 
                       detail = paste("Chargement:", sample_name))
          
          add_log(paste("  📂 Lecture:", sample_name))
          
          # Charger les données
          raw <- load_single_cell_data(sample_path)
          obj <- prepare_seurat_object(raw, sample_name)
          
          # Ajouter orig.ident avec le nom de l'échantillon
          obj$orig.ident <- sample_name
          
          obj_list[[sample_name]] <- obj
          add_log(paste("    ✓", ncol(obj), "cellules"))
        }
        
        # Fusionner tous les objets
        progress$set(value = 0.9, detail = "Fusion des échantillons...")
        add_log("🔗 Fusion des objets Seurat...")
        
        if(length(obj_list) == 1) {
          merged_obj <- obj_list[[1]]
        } else {
          merged_obj <- merge(obj_list[[1]], y = obj_list[-1], 
                              add.cell.ids = names(obj_list),
                              project = "MultiSample")
        }
        
        global_data$sc_obj <- merged_obj
        
        add_log(paste("✅ Import réussi!", ncol(merged_obj), "cellules totales,", 
                      length(unique(merged_obj$orig.ident)), "échantillon(s)"))
        
        showNotification(
          paste("✅ Import réussi:", ncol(merged_obj), "cellules"), 
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        add_log(paste("❌ Erreur:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    })
    
    # --- AFFICHAGE LISTE FICHIERS UPLOADÉS ---
    output$file_list_display <- renderUI({
      req(input$file_upload)
      
      files <- input$file_upload$name
      
      if(length(files) == 0) {
        return(tags$em("Aucun fichier", style = "color: #999;"))
      }
      
      tags$ul(
        style = "list-style: none; padding: 0; margin: 0;",
        lapply(files, function(f) {
          tags$li(
            style = "padding: 5px; border-bottom: 1px solid #eee;",
            bsicons::bs_icon("file-earmark"), " ", f
          )
        })
      )
    })
    
    # --- IMPORT FICHIERS MULTIPLES ---
    observeEvent(input$btn_load_file, {
      req(input$file_upload)
      
      files <- input$file_upload
      add_log(paste("🔄 Import de", nrow(files), "fichier(s)..."))
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Chargement des fichiers...", value = 0)
      
      tryCatch({
        obj_list <- list()
        
        for(i in 1:nrow(files)) {
          file_name <- tools::file_path_sans_ext(files$name[i])
          file_path <- files$datapath[i]
          
          progress$set(value = i/nrow(files), detail = paste("Fichier:", files$name[i]))
          
          add_log(paste("  📄 Lecture:", files$name[i]))
          
          raw <- load_single_cell_data(file_path)
          obj <- prepare_seurat_object(raw, file_name)
          obj$orig.ident <- file_name
          
          obj_list[[file_name]] <- obj
          add_log(paste("    ✓", ncol(obj), "cellules"))
        }
        
        # Fusionner
        progress$set(value = 0.9, detail = "Fusion...")
        add_log("🔗 Fusion des objets...")
        
        if(length(obj_list) == 1) {
          merged_obj <- obj_list[[1]]
        } else {
          merged_obj <- merge(obj_list[[1]], y = obj_list[-1], 
                              add.cell.ids = names(obj_list),
                              project = "MultiFile")
        }
        
        global_data$sc_obj <- merged_obj
        
        add_log(paste("✅ Import réussi!", ncol(merged_obj), "cellules,", 
                      length(unique(merged_obj$orig.ident)), "échantillon(s)"))
        
        showNotification("✅ Import réussi!", type = "message", duration = 5)
        
      }, error = function(e) {
        add_log(paste("❌ Erreur:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    })
    
    # --- IMPORT FICHIER UNIQUE (Option C) ---
    observeEvent(input$btn_load_single, {
      req(input$single_file_upload)
      add_log("📄 Import fichier unique...")
      
      withProgress(message = "Chargement...", {
        tryCatch({
          raw <- load_single_cell_data(input$single_file_upload$datapath)
          obj <- prepare_seurat_object(raw, "SingleSample")
          
          global_data$sc_obj <- obj
          add_log(paste("✅ Import réussi:", ncol(obj), "cellules"))
          showNotification("✅ Import réussi!", type = "message")
          
        }, error = function(e) {
          add_log(paste("❌ Erreur:", e$message))
          showNotification(paste("Erreur:", e$message), type = "error")
        })
      })
    })
    
    # --- OUTPUTS INFO ---
    output$nb_cells <- renderText({ 
      if(is.null(global_data$sc_obj)) "-" else format(ncol(global_data$sc_obj), big.mark = ",")
    })
    
    output$nb_genes <- renderText({ 
      if(is.null(global_data$sc_obj)) "-" else format(nrow(global_data$sc_obj), big.mark = ",")
    })
    
    output$nb_samples <- renderText({
      if(is.null(global_data$sc_obj)) {
        "-"
      } else {
        length(unique(global_data$sc_obj$orig.ident))
      }
    })
    
    output$status_obj <- renderText({ 
      if(is.null(global_data$sc_obj)) {
        "⚪ Inactif"
      } else {
        n_samples <- length(unique(global_data$sc_obj$orig.ident))
        if(n_samples > 1) {
          paste("🟢 Multi-échantillons (", n_samples, ")")
        } else {
          "🟡 Mono-échantillon"
        }
      }
    })
    
    output$console_log <- renderText({ logs() })
  })
}