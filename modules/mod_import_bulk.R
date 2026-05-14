# modules/mod_import_bulk.R - Import RNA Bulk (VERSION CORRIGÉE)
# FIXED VERSION - Addressing loading issues

mod_import_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import RNA Bulk",
        
        div(class = "alert alert-info", style = "font-size: 0.85rem;",
            bsicons::bs_icon("info-circle"), 
            " Importez des données RNA-Seq en Bulk (matrice de counts + métadonnées)."),
        
        accordion(
          accordion_panel(
            "1. Matrice de Counts",
            icon = icon("table"),
            
            fileInput(ns("counts_file"), "Fichier de Counts (CSV/TSV/TXT)", 
                      accept = c(".csv", ".tsv", ".txt", ".xlsx")),
            
            helpText("Format attendu : Lignes = Gènes, Colonnes = Échantillons."),
            
            radioButtons(ns("counts_format"), "Format de la matrice",
                         choices = c("Genes en lignes (standard)" = "rows",
                                     "Genes en colonnes (transposé)" = "cols"),
                         selected = "rows"),
            
            checkboxInput(ns("counts_has_header"), "La 1ère ligne est un en-tête", value = TRUE),
            checkboxInput(ns("counts_has_rownames"), "La 1ère colonne est le nom des gènes", value = TRUE),
            
            h6("Aperçu de la matrice:", style = "font-weight: bold; margin-top: 10px;"),
            div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                tableOutput(ns("counts_preview"))
            )
          ),
          
          accordion_panel(
            "2. Métadonnées (Optionnel)",
            icon = icon("tags"),
            
            fileInput(ns("metadata_file"), "Fichier de Métadonnées (CSV/TSV/TXT)", 
                      accept = c(".csv", ".tsv", ".txt", ".xlsx")),
            
            helpText("Format : Lignes = Échantillons, Colonnes = Variables (condition, batch, etc.)."),
            
            checkboxInput(ns("metadata_has_header"), "La 1ère ligne est un en-tête", value = TRUE),
            checkboxInput(ns("metadata_has_rownames"), "La 1ère colonne est le nom des échantillons", value = TRUE),
            
            h6("Aperçu des métadonnées:", style = "font-weight: bold; margin-top: 10px;"),
            div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                tableOutput(ns("metadata_preview"))
            )
          ),
          
          accordion_panel(
            "3. Options d'Import",
            icon = icon("cogs"),
            
            textInput(ns("project_name"), "Nom du Projet", 
                      value = "BulkRNA_Project", placeholder = "Ex: Study_2024"),
            
            numericInput(ns("min_counts"), "Counts minimum par gène", 
                         value = 10, min = 0, step = 1),
            
            helpText("Les gènes avec moins de counts seront filtrés."),
            
            hr(),
            
            div(
              id = ns("load_button_container"),
              actionButton(ns("btn_load"), "🚀 Charger les Données", 
                           class = "btn-success w-100", icon = icon("play"))
            )
          )
        )
      ),
      
      card(
        card_header("Résumé des Données Bulk"),
        layout_columns(
          value_box(
            title = "Échantillons",
            value = textOutput(ns("nb_samples")),
            showcase = bsicons::bs_icon("grid-3x3"),
            theme = "primary"
          ),
          value_box(
            title = "Gènes",
            value = textOutput(ns("nb_genes")),
            showcase = bsicons::bs_icon("diagram-3"),
            theme = "secondary"
          ),
          value_box(
            title = "Variables Métadonnées",
            value = textOutput(ns("nb_metadata_vars")),
            showcase = bsicons::bs_icon("tags"),
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

mod_import_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Logger avec initialisation explicite
    logs <- reactiveVal("En attente d'import...")
    
    # Fonction pour ajouter des logs
    add_log <- function(msg) { 
      timestamp <- format(Sys.time(), "%H:%M:%S")
      old_logs <- isolate(logs())
      new_logs <- paste0("[", timestamp, "] ", msg, "\n", old_logs)
      logs(new_logs)
    }
    
    # Stockage temporaire avec reactiveValues pour meilleure réactivité
    temp_data <- reactiveValues(
      counts = NULL,
      metadata = NULL,
      is_loaded = FALSE
    )
    
    # --- FONCTION DE LECTURE ROBUSTE ---
    smart_read <- function(filepath, has_header = TRUE, has_rownames = TRUE) {
      ext <- tolower(tools::file_ext(filepath))
      
      tryCatch({
        df <- switch(
          ext,
          "csv" = read.csv(filepath, header = has_header,
                           row.names = if (has_rownames) 1 else NULL,
                           check.names = FALSE, stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8-BOM"),
          "tsv" = read.delim(filepath, header = has_header,
                             row.names = if (has_rownames) 1 else NULL,
                             sep = "\t", check.names = FALSE,
                             stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8-BOM"),
          "txt" = read.delim(filepath, header = has_header,
                             row.names = if (has_rownames) 1 else NULL,
                             check.names = FALSE,
                             stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8-BOM"),
          "xlsx" = {
            if (!requireNamespace("readxl", quietly = TRUE)) {
              stop("Package 'readxl' nécessaire pour lire les fichiers .xlsx")
            }
            tmp <- readxl::read_excel(filepath, col_names = has_header)
            tmp <- as.data.frame(tmp, check.names = FALSE, stringsAsFactors = FALSE)
            if (has_rownames) { 
              rownames(tmp) <- tmp[[1]]
              tmp <- tmp[, -1, drop = FALSE] 
            }
            tmp
          },
          stop("Format de fichier non supporté")
        )
        
        # Nettoyage BOM résiduel
        if (!is.null(colnames(df))) {
          colnames(df) <- sub("^\ufeff", "", colnames(df))
        }
        if (!is.null(rownames(df))) {
          rownames(df) <- sub("^\ufeff", "", rownames(df))
        }
        
        return(df)
      }, error = function(e) {
        stop(paste("Erreur de lecture:", e$message))
      })
    }
    
    # --- REACTIVES POUR LES DONNÉES ---
    counts_reactive <- reactive({
      req(input$counts_file)
      
      tryCatch({
        add_log("📂 Lecture du fichier de counts...")
        
        df <- smart_read(input$counts_file$datapath, 
                         input$counts_has_header, 
                         input$counts_has_rownames)
        
        # Transposer si nécessaire
        if (input$counts_format == "cols") {
          df <- as.data.frame(t(df))
          add_log("  ↻ Matrice transposée (genes étaient en colonnes)")
        }
        
        # Vérifier dimensions
        if (nrow(df) == 0 || ncol(df) == 0) {
          stop("Matrice vide après lecture. Vérifie les options d'en-tête/rownames.")
        }
        
        add_log(paste("✓ Matrice de counts chargée:", nrow(df), "gènes ×", ncol(df), "échantillons"))
        return(df)
        
      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture counts:", e$message))
        showNotification(paste("Erreur lecture counts:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    })
    
    metadata_reactive <- reactive({
      if (is.null(input$metadata_file)) return(NULL)
      
      tryCatch({
        add_log("📂 Lecture du fichier de métadonnées...")
        
        df <- smart_read(input$metadata_file$datapath, 
                         input$metadata_has_header, 
                         input$metadata_has_rownames)
        
        add_log(paste("✓ Métadonnées chargées:", nrow(df), "échantillons ×", ncol(df), "variables"))
        return(df)
        
      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture métadonnées:", e$message))
        showNotification(paste("Erreur métadonnées:", e$message), type = "warning", duration = 10)
        return(NULL)
      })
    })
    
    # --- APERÇUS ---
    output$counts_preview <- renderTable({
      df <- counts_reactive()
      if (is.null(df)) return(NULL)
      head(df[, 1:min(5, ncol(df)), drop = FALSE], 10)
    }, rownames = TRUE)
    
    output$metadata_preview <- renderTable({
      df <- metadata_reactive()
      if (is.null(df)) return(NULL)
      head(df, 10)
    }, rownames = TRUE)
    
    # --- GESTION DU BOUTON DE CHARGEMENT ---
    observe({
      # Activer le bouton seulement si les counts sont chargés
      counts_loaded <- !is.null(counts_reactive())
      shinyjs::toggleState("btn_load", condition = counts_loaded)
      
      # Mettre à jour le texte du bouton selon l'état
      if (temp_data$is_loaded) {
        shinyjs::html("btn_load", html = "🔄 Mettre à jour les Données")
      } else {
        shinyjs::html("btn_load", html = "🚀 Charger les Données")
      }
    })
    
    # --- CHARGEMENT FINAL DES DONNÉES ---
    observeEvent(input$btn_load, {
      req(counts_reactive())
      
      add_log("🔄 Préparation de l'objet RNA Bulk...")
      
      # Show progress
      showNotification("Traitement des données...", type = "message", duration = NULL, id = ns("progress"))
      
      tryCatch({
        counts <- counts_reactive()
        metadata <- metadata_reactive()
        
        # Conversion numérique robuste
        add_log("  → Conversion des valeurs en numérique...")
        counts <- as.data.frame(counts, check.names = FALSE, stringsAsFactors = FALSE)
        
        # Conversion colonne par colonne
        for (col in colnames(counts)) {
          if (is.character(counts[[col]])) {
            counts[[col]] <- suppressWarnings(as.numeric(counts[[col]]))
          }
        }
        
        # Vérifier NA après conversion
        if (anyNA(as.matrix(counts))) {
          add_log("  ⚠️ Certaines valeurs ne sont pas numériques (NA après conversion)")
        }
        
        # Convertir en matrice
        counts_matrix <- as.matrix(counts)
        
        # Filtrer les gènes avec peu de counts
        gene_counts <- rowSums(counts_matrix, na.rm = TRUE)
        keep_genes <- gene_counts >= input$min_counts
        counts_matrix <- counts_matrix[keep_genes, , drop = FALSE]
        
        add_log(paste("  → Gènes filtrés:", sum(!keep_genes), "retirés,", sum(keep_genes), "conservés"))
        
        # Créer l'objet bulk
        bulk_obj <- list(
          counts = counts_matrix,
          metadata = NULL,
          project = input$project_name,
          type = "bulk",
          timestamp = Sys.time()
        )
        
        # Ajouter métadonnées si disponibles
        if (!is.null(metadata)) {
          sample_names <- colnames(counts_matrix)
          metadata_names <- rownames(metadata)
          
          # Aligner les métadonnées
          if (all(sample_names %in% metadata_names)) {
            bulk_obj$metadata <- metadata[sample_names, , drop = FALSE]
            add_log(paste("  ✓ Métadonnées alignées:", ncol(bulk_obj$metadata), "variables"))
          } else {
            # Créer métadonnées minimales avec warning
            bulk_obj$metadata <- data.frame(
              sample = sample_names,
              row.names = sample_names
            )
            add_log("  ⚠️ Métadonnées non alignées - création de métadonnées par défaut")
          }
        } else {
          # Métadonnées minimales
          bulk_obj$metadata <- data.frame(
            sample = colnames(counts_matrix),
            row.names = colnames(counts_matrix)
          )
          add_log("  → Métadonnées par défaut créées (1 variable: sample)")
        }
        
        # Stocker dans global_data avec assignation explicite
        global_data$bulk_obj <- bulk_obj
        temp_data$is_loaded <- TRUE
        
        add_log(paste("✅ Import réussi!", nrow(counts_matrix), "gènes ×", 
                      ncol(counts_matrix), "échantillons"))
        
        # Success notification
        removeNotification(id = ns("progress"))
        showNotification(
          paste("✅ Import réussi:", ncol(counts_matrix), "échantillons,", 
                nrow(counts_matrix), "gènes"), 
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        removeNotification(id = ns("progress"))
        add_log(paste("❌ Erreur lors du chargement:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
        temp_data$is_loaded <- FALSE
      })
    })
    
    # --- OUTPUTS INFO ---
    output$nb_samples <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) {
        "-"
      } else {
        ncol(global_data$bulk_obj$counts)
      }
    })
    
    output$nb_genes <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) {
        "-"
      } else {
        nrow(global_data$bulk_obj$counts)
      }
    })
    
    output$nb_metadata_vars <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$metadata)) {
        "-"
      } else {
        ncol(global_data$bulk_obj$metadata)
      }
    })
    
    output$status_obj <- renderText({
      if (is.null(global_data$bulk_obj)) {
        "⚪ Inactif"
      } else {
        paste("🟢 Chargé (", format(global_data$bulk_obj$timestamp, "%H:%M:%S"), ")")
      }
    })
    
    output$console_log <- renderText({
      logs()
    })
    
    # Reset state when new file is selected
    observeEvent(input$counts_file, {
      temp_data$is_loaded <- FALSE
    })
    
    observeEvent(input$metadata_file, {
      temp_data$is_loaded <- FALSE
    })
  })
}