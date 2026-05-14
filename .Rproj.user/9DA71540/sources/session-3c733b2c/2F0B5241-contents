# app.R - TranscriptoShiny v2a
source("global.R")

# Modules d'import (inchangés)
source("modules/mod_import_sc.R")
source("modules/mod_import_bulk.R")
source("modules/mod_import_spatial.R")

# ── Enfants single-cell (AVANT le parent) ────────────────────────────────────
source("modules/mod_sc_pipeline.R")
source("modules/mod_sc_annotation.R")
source("modules/mod_sc_viz.R")
source("modules/mod_sc_markers.R")
source("modules/mod_sc_corr.R")
source("modules/mod_sc_pathways.R")
source("modules/mod_sc_trajectory.R")

# ── Parent router (APRÈS les enfants) ────────────────────────────────────────
source("modules/mod_sc.R")

# Autres modules d'analyse (inchangés)
source("modules/mod_bulk.R")
source("modules/mod_spatial.R")



# --- UI ---
ui <- page_navbar(
  title = "TranscriptoShiny v2 - Multi-Omics Platform",
  theme = my_theme,
  
  # Sidebar global pour infos système
  sidebar = sidebar(
    title = "Système",
    width = 250,
    
    h6("Mémoire Utilisée", style = "font-weight: bold;"),
    textOutput("mem_usage"),
    actionButton("gc_btn", "🧹 Nettoyer RAM", 
                 class = "btn-xs btn-outline-secondary w-100 mt-2"),
    
    hr(),
    
    h6("Objets Chargés", style = "font-weight: bold;"),
    tags$ul(style = "font-size: 0.85rem; padding-left: 15px;",
            tags$li(textOutput("status_sc")),
            tags$li(textOutput("status_bulk")),
            tags$li(textOutput("status_spatial"))
    ),
    
    hr(),
    
    actionButton("help_btn", "📖 Guide / Aide", 
                 icon = icon("question-circle"), 
                 class = "btn-info w-100")
  ),
  
  # === ONGLETS D'IMPORT ===
  nav_menu(
    "📥 Import Données",
    icon = icon("upload"),
    
    nav_panel("Single-Cell", 
              icon = icon("braille"),
              mod_import_sc_ui("import_sc")),
    
    nav_panel("RNA Bulk", 
              icon = icon("table"),
              mod_import_bulk_ui("import_bulk")),
    
    nav_panel("Spatial", 
              icon = icon("map"),
              mod_import_spatial_ui("import_spatial"))
  ),
  
  # === ONGLETS D'ANALYSE ===
  nav_spacer(),
  
  nav_panel("🔬 Single-Cell Analysis", 
            icon = icon("microscope"),
            mod_sc_ui("sc")),
  
  nav_panel("📊 Bulk RNA Analysis", 
            icon = icon("chart-line"),
            mod_bulk_ui("bulk")),
  
  nav_panel("🗺️ Spatial Analysis", 
            icon = icon("layer-group"),
            mod_spatial_ui("spatial"))
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # === DONNÉES GLOBALES PARTAGÉES ===
  global_data <- reactiveValues(
    sc_obj = NULL,      # Objet Seurat Single-Cell
    bulk_obj = NULL,    # Objet Bulk (liste avec counts + metadata)
    spatial_obj = NULL  # Objet Seurat Spatial
  )
  
  # === MODULES D'IMPORT ===
  mod_import_sc_server("import_sc", global_data)
  mod_import_bulk_server("import_bulk", global_data)
  mod_import_spatial_server("import_spatial", global_data)
  
  # === MODULES D'ANALYSE ===
  mod_sc_server("sc", global_data)
  mod_bulk_server("bulk", global_data)
  mod_spatial_server("spatial", global_data)
  
  # === STATUT DES OBJETS ===
  output$status_sc <- renderText({
    if (is.null(global_data$sc_obj)) {
      "Single-Cell: ⚪ Aucun"
    } else {
      n_cells <- ncol(global_data$sc_obj)
      n_samples <- length(unique(global_data$sc_obj$orig.ident))
      paste0("Single-Cell: 🟢 ", format(n_cells, big.mark = ","), 
             " cellules (", n_samples, " échantillon(s))")
    }
  })
  
  output$status_bulk <- renderText({
    if (is.null(global_data$bulk_obj)) {
      "Bulk RNA: ⚪ Aucun"
    } else {
      n_samples <- ncol(global_data$bulk_obj$counts)
      paste0("Bulk RNA: 🟢 ", n_samples, " échantillon(s)")
    }
  })
  
  output$status_spatial <- renderText({
    if (is.null(global_data$spatial_obj)) {
      "Spatial: ⚪ Aucun"
    } else {
      n_spots <- ncol(global_data$spatial_obj)
      paste0("Spatial: 🟢 ", format(n_spots, big.mark = ","), " spots")
    }
  })
  
  # === AIDE DIDACTIQUE ===
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "📖 Guide d'Utilisation - TranscriptoShiny v2",
      size = "l",
      easyClose = TRUE,
      
      tags$div(
        h4("🎯 Workflow Recommandé", style = "color: #2C3E50;"),
        
        h5("1️⃣ Importation des Données"),
        p("Utilisez le menu ", tags$strong("'Import Données'"), " pour charger vos fichiers :"),
        tags$ul(
          tags$li(tags$strong("Single-Cell:"), " Dossiers 10X CellRanger, fichiers .rds, .h5, .h5ad"),
          tags$li(tags$strong("RNA Bulk:"), " Matrices de counts (CSV/TSV) + Métadonnées optionnelles"),
          tags$li(tags$strong("Spatial:"), " Dossiers 10X Visium ou fichiers .rds spatiaux")
        ),
        
        tags$div(class = "alert alert-info", style = "font-size: 0.9rem;",
                 bsicons::bs_icon("lightbulb"), 
                 tags$strong(" Astuce:"), 
                 " Pour Harmony (correction de batch), importez ", 
                 tags$strong("2 échantillons ou plus"), 
                 " dans l'onglet Single-Cell."
        ),
        
        hr(),
        
        h5("2️⃣ Analyse Single-Cell"),
        p("Suivez le workflow numéroté dans la barre latérale :"),
        tags$ol(
          tags$li(tags$strong("Pipeline:"), " QC → Normalisation → Réduction dimensionnelle (UMAP/PCA/t-SNE/Harmony)"),
          tags$li(tags$strong("Annotation:"), " Identification automatique des types cellulaires (SingleR)"),
          tags$li(tags$strong("Visualisation:"), " Choix de plots interactifs (DimPlot, Violin, Heatmap, etc.)"),
          tags$li(tags$strong("Marqueurs:"), " Recherche de gènes différentiels par cluster")
        ),
        
        tags$div(class = "alert alert-success", style = "font-size: 0.9rem;",
                 bsicons::bs_icon("star"), 
                 tags$strong(" Nouvelle fonctionnalité:"), 
                 " Cliquez directement sur un gène dans le tableau des marqueurs pour l'ajouter automatiquement à la visualisation !"
        ),
        
        hr(),
        
        h5("3️⃣ Analyses Bulk et Spatial"),
        tags$ul(
          tags$li(tags$strong("Bulk RNA:"), " Analyse différentielle avec DESeq2/edgeR"),
          tags$li(tags$strong("Spatial:"), " Visualisation spatiale des patterns d'expression")
        ),
        
        hr(),
        
        h5("💾 Gestion de la Mémoire"),
        p("Le bouton ", tags$code("Nettoyer RAM"), 
          " permet de libérer la mémoire entre les analyses."),
        
        hr(),
        
        h5("📚 Ressources"),
        tags$ul(
          tags$li(tags$a("Documentation Seurat", 
                         href = "https://satijalab.org/seurat/", 
                         target = "_blank")),
          tags$li(tags$a("SingleR Guide", 
                         href = "https://bioconductor.org/packages/release/bioc/vignettes/SingleR/inst/doc/SingleR.html", 
                         target = "_blank"))
        )
      ),
      
      footer = tagList(
        modalButton("Fermer"),
        actionButton("reset_app", "🔄 Réinitialiser l'App", class = "btn-warning")
      )
    ))
  })
  
  # Réinitialisation complète
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "⚠️ Confirmation",
      "Êtes-vous sûr de vouloir effacer toutes les données chargées ?",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_reset", "Oui, tout effacer", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    global_data$sc_obj <- NULL
    global_data$bulk_obj <- NULL
    global_data$spatial_obj <- NULL
    clean_mem()
    removeModal()
    showNotification("✓ Application réinitialisée", type = "message")
  })
  
  # === GESTION RAM ===
  output$mem_usage <- renderText({
    input$gc_btn
    invalidateLater(5000)  # Mise à jour toutes les 5 secondes
    mem_mb <- round(sum(gc()[, 2]) / 1024, 0)
    paste0("💾 ", mem_mb, " MB")
  })
  
  observeEvent(input$gc_btn, { 
    clean_mem() 
    showNotification("🧹 Mémoire nettoyée", type = "message", duration = 2)
  })
}

# Lancement de l'application
shinyApp(ui, server)