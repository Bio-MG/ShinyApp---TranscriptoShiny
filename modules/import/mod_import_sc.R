# modules/mod_import_sc.R - VERSION MULTI-ÉCHANTILLONS (SEURAT V5 + VALIDATION FICHIERS)

mod_import_sc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import Single-Cell",
        
        accordion(
          accordion_panel(
            "Option A: Dossiers Multiples (10X)",
            div(class = "alert alert-info", style = "font-size: 0.85rem;",
                bsicons::bs_icon("info-circle"), 
                " Importez plusieurs échantillons pour Harmony (correction de batch)."),
            
            shinyDirButton(ns("dir_select"), "📁 Ajouter un Dossier", 
                           "Sélectionner dossier contenant matrix.mtx", 
                           class = "btn-secondary w-100", icon = icon("folder-open")),
            
            textInput(ns("sample_name"), "Nom de l'échantillon", 
                      placeholder = "Ex: Patient1, Control_A"),
            
            actionButton(ns("btn_add_sample"), "➕ Ajouter à la liste", 
                         class = "btn-info w-100 mt-2"),
            
            hr(),
            
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
            "Option B: Fichiers Multiples (.rds, .h5, .h5ad)",
            div(class = "alert alert-info", style = "font-size: 0.85rem;",
                bsicons::bs_icon("info-circle"), 
                " Importez plusieurs fichiers pour les fusionner."),
            
            div(class = "alert alert-warning", style = "font-size: 0.8rem;",
                bsicons::bs_icon("exclamation-triangle"), 
                " Gros fichiers (>2GB) : préférez l'upload direct via Option C ou dossiers 10X."),
            
            fileInput(ns("file_upload"), "Ajouter Fichier(s)", 
                      accept = c(".rds", ".h5", ".h5ad", ".loom"),
                      multiple = TRUE),
            
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

mod_import_sc_server <- function(id, global_data) {
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
      
      current_samples <- sample_list()
      if(input$sample_name %in% names(current_samples)) {
        showNotification("⚠️ Ce nom d'échantillon existe déjà.", type = "warning")
        return()
      }
      
      current_samples[[input$sample_name]] <- dir_path()
      sample_list(current_samples)
      
      add_log(paste("Échantillon ajouté:", input$sample_name))
    })
    
    # Affichage de la liste des échantillons
    output$sample_list_display <- renderUI({
      samples <- names(sample_list())
      if(length(samples) == 0) {
        return(tags$em("Aucun échantillon ajouté", style = "color: #999;"))
      }
      
      tags$ul(
        lapply(samples, function(s) {
          tags$li(s)
        })
      )
    })
    
    # Effacer la liste
    observeEvent(input$btn_clear_samples, {
      sample_list(list())
      add_log("Liste des échantillons effacée")
    })
    
    # --- IMPORT DOSSIERS MULTIPLES (Option A) ---
    observeEvent(input$btn_load_dir, {
      req(sample_list())
      
      samples <- sample_list()
      add_log(paste("🔄 Import de", length(samples), "dossiers 10X..."))
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Chargement des dossiers...", value = 0)
      
      tryCatch({
        obj_list <- list()
        
        for(i in seq_along(samples)) {
          sample_name <- names(samples)[i]
          path <- samples[[i]]
          
          progress$set(value = i/length(samples), detail = paste("Dossier:", sample_name))
          
          add_log(paste("  📂 Lecture:", path))
          
          raw <- load_single_cell_data(path, add_log = add_log)
          obj <- prepare_seurat_object(raw, sample_name)
          obj$orig.ident <- sample_name
          
          obj_list[[sample_name]] <- obj
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
                              project = "MultiSample")
        }
        
        global_data$sc_obj <- merged_obj
        
        add_log(paste("✅ Import réussi!", ncol(merged_obj), "cellules,", 
                      length(unique(merged_obj$orig.ident)), "échantillon(s)"))
        
        showNotification(
          paste("✅ Import réussi:", ncol(merged_obj), "cellules"), 
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        msg <- paste("❌ Erreur:", conditionMessage(e), collapse = " ")
        add_log(msg)
        showNotification(msg, type = "error", duration = 10)
      })
    })
    
    # --- AFFICHAGE LISTE FICHIERS UPLOADÉS ---
    output$file_list_display <- renderUI({
      req(input$file_upload)
      
      files <- input$file_upload
      
      if(nrow(files) == 0) {
        return(tags$em("Aucun fichier", style = "color: #999;"))
      }
      
      tags$ul(
        style = "list-style: none; padding: 0; margin: 0;",
        lapply(1:nrow(files), function(i) {
          f <- files[i, ]
          size_mb <- round(f$size / 1024^2, 1)
          
          # Validation visuelle
          is_valid <- validate_file_integrity(f$datapath, tolower(tools::file_ext(f$name)))
          icon_status <- if(is_valid) "✅" else "⚠️"
          
          tags$li(
            style = "padding: 5px; border-bottom: 1px solid #eee;",
            icon_status, " ", f$name, 
            tags$small(style = "color: #666;", paste0(" (", size_mb, " MB)"))
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
          
          raw <- load_single_cell_data(file_path, add_log = add_log)
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
        msg <- paste("❌ Erreur:", conditionMessage(e), collapse = " ")
        add_log(msg)
        showNotification(msg, type = "error", duration = 10)
      })
    })
    
    # --- IMPORT FICHIER UNIQUE (Option C) ---
    observeEvent(input$btn_load_single, {
      req(input$single_file_upload)
      add_log("🔄 Import fichier unique...")
      
      withProgress(message = "Chargement...", {
        tryCatch({
          raw <- load_single_cell_data(input$single_file_upload$datapath, add_log= add_log)
          obj <- prepare_seurat_object(raw, "SingleSample")
          
          global_data$sc_obj <- obj
          add_log(paste("✅ Import réussi:", ncol(obj), "cellules"))
          showNotification("✅ Import réussi!", type = "message")
          
        }, error = function(e) {
          msg <- paste("❌ Erreur:", conditionMessage(e), collapse = " ")
          add_log(msg)
          showNotification(msg, type = "error", duration = 10)
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
    
    # ========================================================================
    # HELPER FUNCTIONS - SEURAT V5 COMPATIBLE + FILE VALIDATION
    # ========================================================================
    
    # Fonction de validation de l'intégrité des fichiers
    validate_file_integrity <- function(path, ext) {
      tryCatch({
        if (ext %in% c("h5", "h5ad")) {
          # Vérifier que le fichier HDF5 est lisible
          if (!requireNamespace("rhdf5", quietly = TRUE)) {
            return(TRUE)  # Skip validation si rhdf5 non installé
          }
          
          # Test rapide d'ouverture
          h5_handle <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDONLY")
          rhdf5::H5Fclose(h5_handle)
          return(TRUE)
        }
        
        if (ext == "rds") {
          # Vérifier taille non nulle
          return(file.size(path) > 100)
        }
        
        return(TRUE)
        
      }, error = function(e) {
        return(FALSE)
      })
    }
    
    # Fonction pour charger les données single-cell
    # -----------------------------------------------------------
    # Helper principal d'import : dossiers, .rds, .h5, .h5ad, .loom
    # -----------------------------------------------------------
    load_single_cell_data <- function(path, add_log = NULL) {
      
      # 1) Dossier CellRanger (Option A)
      if (dir.exists(path)) {
        h5_path <- file.path(path, "filtered_feature_bc_matrix.h5")
        if (file.exists(h5_path)) {
          return(Read10X_h5(h5_path))  # 10x HDF5 classique
        }
        return(Read10X(path))
      }
      
      ext <- tolower(tools::file_ext(path))
      
      # 2) .rds déjà Seurat / matrice
      if (ext == "rds") {
        return(readRDS(path))
      }
      
      # 3) .h5 10x (1M neurons, etc.)
      if (ext == "h5") {
        # Si BPCells dispo + gros fichier -> on-disk
        if (requireNamespace("BPCells", quietly = TRUE) && file.size(path) > 1e9) {
          mat <- BPCells::open_matrix_10x_hdf5(path)           # lecture HDF5 10x[web:52]
          tmp_dir <- tempfile(pattern = "bpcells_10x_")
          BPCells::write_matrix_dir(mat = mat, dir = tmp_dir)  # écriture bitpack[web:52]
          return(BPCells::open_matrix_dir(dir = tmp_dir))
        }
        # fallback Seurat
        return(Read10X_h5(path))
      }
      
      # 4) .h5ad (AnnData) – refonte complète
      if (ext == "h5ad") {
        msg_prefix <- "Impossible de charger le fichier .h5ad."
        
        # 4.1) BPCells: open_matrix_anndata_hdf5 -> on-disk matrice
        if (requireNamespace("BPCells", quietly = TRUE)) {
          try({
            mat <- BPCells::open_matrix_anndata_hdf5(path)     # lecture h5ad[web:52][web:55]
            tmp_dir <- tempfile(pattern = "bpcells_h5ad_")
            BPCells::write_matrix_dir(mat = mat, dir = tmp_dir)
            mat_disk <- BPCells::open_matrix_dir(dir = tmp_dir)
            return(mat_disk)
          }, silent = TRUE)
        }
        
        # 4.2) zellkonverter -> SingleCellExperiment -> Seurat
        if (requireNamespace("zellkonverter", quietly = TRUE)) {
          try({
            sce <- zellkonverter::readH5AD(
              file = path,
              use_hdf5 = TRUE,
              raw = TRUE
            )                                             # SCE on-disk[web:49][web:62]
            # Assigne les counts depuis X/raw si nécessaire
            if (!"counts" %in% SummarizedExperiment::assayNames(sce)) {
              if ("X" %in% SummarizedExperiment::assayNames(sce)) {
                SummarizedExperiment::assay(sce, "counts") <- SummarizedExperiment::assay(sce, "X")
              }
            }
            seu <- Seurat::as.Seurat(sce, counts = "counts", data = NULL)
            return(seu)
          }, silent = TRUE)
        }
        
        # 4.3) sceasy : écrit un .rds Seurat temporaire
        if (requireNamespace("sceasy", quietly = TRUE)) {
          try({
            tmp_rds <- tempfile(fileext = ".rds")
            sceasy::convertFormat(
              path,
              from    = "anndata",
              to      = "seurat",
              outFile = tmp_rds
            )                                             # conversion h5ad->Seurat[web:51][web:57]
            seu <- readRDS(tmp_rds)
            return(seu)
          }, silent = TRUE)
        }
        
        # 4.4) anndataR (optionnel, si installé)
        if (requireNamespace("anndatar", quietly = TRUE)) {
          try({
            seu <- anndatar::read_h5ad(path, to = "Seurat")    # h5ad->Seurat direct[web:34]
            return(seu)
          }, silent = TRUE)
        }
        
        # Si toutes les méthodes échouent :
        stop(
          paste0(
            msg_prefix, " Causes possibles :\n\n",
            "1. FICHIER CORROMPU/INCOMPLET\n",
            "   └─ Vérifiez que l'upload s'est terminé correctement\n",
            "   └─ Taille actuelle: ", round(file.size(path) / (1024^2), 1), " MB\n\n",
            "2. PACKAGES MANQUANTS\n",
            "   └─ Installez au moins un de ces packages :\n",
            "      • BPCells      : remotes::install_github('bnprks/BPCells')\n",
            "      • zellkonverter: BiocManager::install('zellkonverter')\n",
            "      • sceasy       : devtools::install_github('cellgeni/sceasy')\n",
            "      • anndatar     : remotes::install_github('data-intuitive/anndataR')\n"
          )
        )
      }
      
      # 5) .loom
      if (ext == "loom") {
        if (!requireNamespace("loomR", quietly = TRUE)) {
          stop("Le package 'loomR' est requis pour lire les fichiers .loom.")
        }
        lconn <- loomR::connect(path, mode = "r")
        on.exit(lconn$close())
        return(Seurat::as.Seurat(lconn))
      }
      
      stop("Format non supporté : ", ext)
    }
    
    
    # Fonction pour préparer l'objet Seurat (v5 safe)
    prepare_seurat_object <- function(raw, sample_name = NULL) {
      if (inherits(raw, "Seurat")) {
        # Vérifier si c'est un objet v5
        if (!is.null(raw[["RNA"]]) && !inherits(raw[["RNA"]], "Assay5")) {
          # Convertir en v5 si nécessaire
          raw[["RNA"]] <- as(raw[["RNA"]], "Assay5")
        }
        return(raw)
      }
      
      # Créer nouvel objet v5
      return(CreateSeuratObject(
        counts = raw, 
        project = sample_name %||% "scData"
      ))
    }
    
  })
}