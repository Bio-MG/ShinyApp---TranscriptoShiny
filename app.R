# app.R - TranscriptoShiny v2a

source("global.R")

# Helpers (refactor : extraits de global.R — l'ordre entre eux n'a pas
# d'importance, R ne résout les appels de fonction qu'à l'exécution).
source("helpers_io.R")
source("helpers_sc.R")
source("helpers_sc_bpcells.R")
source("helpers_bulk.R")
source("helpers_pathway.R")

# --- NOUVEAU (module Spatial v3, BPCells + mirai) ---
# Ces deux fichiers ne dépendent que des packages chargés par global.R —
# doivent être sourcés avant tout module qui les utilise (import spatial,
# modules/spatial/*). init_spatial_daemons() est idempotent : sans risque si
# rappelé plus tard (voir mod_spatial.R, appel défensif dans le module).
source("R/utils_spatial_async.R")
source("R/utils_spatial_io.R")
tryCatch(
  init_spatial_daemons(n_daemons = 6),
  error = function(e) warning("Initialisation des daemons mirai (spatial) impossible : ", conditionMessage(e))
)

source("modules/import/mod_import_sc.R")
source("modules/import/mod_import_bulk.R")
source("modules/import/mod_import_spatial.R")
source("modules/import/mod_geo.R")

source("modules/sc/mod_sc_pipeline.R")
source("modules/sc/mod_sc_annotation.R")
source("modules/sc/mod_sc_viz.R")
source("modules/sc/mod_sc_markers.R")
source("modules/sc/mod_sc_corr.R")
source("modules/sc/mod_sc_pathways.R")
source("modules/sc/mod_sc_trajectory.R")
source("modules/sc/mod_sc_mapping.R")
source("modules/sc/mod_sc.R")


source("modules/bulk/mod_bulk_mapping.R")
source("modules/bulk/mod_bulk_filter.R")
#source("modules/bulk/mod_bulk_de.R")
for (f in list.files("modules/bulk_de", pattern = "\\.R$", full.names = TRUE)) source(f)
source("modules/bulk/mod_bulk_pathways.R")
source("modules/bulk/mod_bulk_report.R")
source("modules/bulk/mod_bulk.R")

# --- Spatial (parent + 4 sous-modules enfants) ---
source("modules/spatial/mod_spatial_qc.R")
source("modules/spatial/mod_spatial_cluster.R")
source("modules/spatial/mod_spatial_deconv.R")
source("modules/spatial/mod_spatial_viz.R")
source("modules/spatial/mod_spatial.R")







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



    h6("Paramètres RAM", style = "font-weight: bold;"),

    div(class = "small text-muted", style = "font-size:0.75rem;",

        "Bulk n'est presque jamais limité par la RAM (matrices de quelques Mo). ",

        "La pression vient surtout du Single-Cell/Spatial (objets Seurat volumineux) — ",

        "ajustez ici si vous travaillez avec de très gros jeux de données."),

    numericInput("ram_future_gb", "Limite mémoire par tâche parallèle (Go)",

                value = 10, min = 1, max = 64, step = 1),

    numericInput("ram_upload_gb", "Taille maximale d'upload (Go)",

                value = 5, min = 1, max = 50, step = 1),

    actionButton("apply_ram_settings", "Appliquer",

                class = "btn-sm btn-outline-primary w-100"),



    hr(),

    

    h6("Objets Chargés", style = "font-weight: bold;"),

    uiOutput("global_status_panel"),



    hr(),



    h6("Session", style = "font-weight: bold;"),

    downloadButton("save_session_btn", "💾 Sauvegarder Session",

                   class = "btn-sm btn-outline-primary w-100"),

    fileInput("load_session_file", "📂 Charger Session (.rds)",

              accept = ".rds", width = "100%"),

    div(class = "small text-muted", style = "font-size:0.75rem;",

        "La sauvegarde capture l'ensemble des données chargées (Single-Cell, Bulk, Spatial) — ",

        "elle est déclenchée uniquement par ce bouton, jamais automatiquement. Pour le Spatial, ",

        "seul le \"sketch\" (echantillon RAM) est garanti portable : les donnees BPCells sur ",

        "disque doivent rester disponibles au meme chemin pour relancer clustering/deconvolution."),



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

              mod_import_spatial_ui("import_spatial")),
    
    nav_panel("GEO", mod_geo_ui("geo")) ##ajout verif

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

    spatial_obj = NULL  # Spatial : liste (sketch, bpcells_dir, coords, ...) — voir R/utils_spatial_io.R

  )

  

  # === MODULES D'IMPORT ===

  mod_import_sc_server("import_sc", global_data)

  mod_import_bulk_server("import_bulk", global_data)

  mod_import_spatial_server("import_spatial", global_data)
  
  
  # AFTER (fix #1) : mod_geo_server("geo", global_data) shared_rv undefined at app scope → crash
  mod_geo_server("geo", global_data)
  

  # === MODULES D'ANALYSE ===

  mod_sc_server("sc", global_data)
  
  mod_sc_mapping_server("mapping", global_data)

  mod_bulk_server("bulk", global_data)

  mod_spatial_server("spatial", global_data)

  

  # === STATUT DES OBJETS — INDICATEUR DE PROGRESSION GLOBAL (point 8) ===

  # Goes beyond "loaded / not loaded": each module reports a finer-grained

  # state (e.g. Bulk: imported -> filtered -> DE computed) so the user does

  # not need to open every accordion just to check where they left off.

  output$global_status_panel <- renderUI({



    # ── Single-Cell state ────────────────────────────────────────────────

    sc_state <- if (is.null(global_data$sc_obj)) {

      list(icon = "⚪", label = "Single-Cell", detail = "Aucune donnée")

    } else {

      n_cells   <- ncol(global_data$sc_obj)

      n_samples <- length(unique(global_data$sc_obj$orig.ident))

      has_clusters <- "seurat_clusters" %in% colnames(global_data$sc_obj@meta.data)

      list(

        icon   = if (has_clusters) "🟢" else "🟡",

        label  = "Single-Cell",

        detail = sprintf("%s cellules, %d échantillon(s)%s",

                         format(n_cells, big.mark = ","), n_samples,

                         if (has_clusters) " — pipeline exécuté" else " — pipeline non lancé")

      )

    }



    # ── Bulk state ───────────────────────────────────────────────────────

    bulk_state <- if (is.null(global_data$bulk_obj)) {

      list(icon = "⚪", label = "Bulk RNA", detail = "Aucune donnée")

    } else {

      n_samples <- ncol(global_data$bulk_obj$counts)

      list(

        icon   = "🟢",

        label  = "Bulk RNA",

        detail = sprintf("%d échantillon(s) importé(s)", n_samples)

      )

    }



    # ── Spatial state ────────────────────────────────────────────────────
    # global_data$spatial_obj est une LISTE depuis le refactor BPCells (voir
    # R/utils_spatial_io.R) : $sketch (Seurat, RAM) + $bpcells_dir (disque,
    # pleine resolution) + $n_total — ne jamais faire ncol(spatial_obj) direct.

    spatial_state <- if (is.null(global_data$spatial_obj)) {

      list(icon = "⚪", label = "Spatial", detail = "Aucune donnée")

    } else {

      obj <- global_data$spatial_obj

      n_total  <- obj$n_total %||% ncol(obj$sketch)

      n_sketch <- ncol(obj$sketch)

      disk_ok  <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)

      list(

        icon   = if (disk_ok) "🟢" else "🟠",

        label  = "Spatial",

        detail = sprintf("%s elements (%s en RAM, sketch)%s",

                         format(n_total, big.mark = ","), format(n_sketch, big.mark = ","),

                         if (!disk_ok) " — disque introuvable" else "")

      )

    }



    render_row <- function(state) {

      div(style = "padding:4px 0;border-bottom:1px solid #eee;font-size:0.82rem;",

          tags$strong(paste(state$icon, state$label)),

          tags$br(),

          tags$span(style = "color:#666;", state$detail))

    }



    tagList(render_row(sc_state), render_row(bulk_state), render_row(spatial_state))

  })



  # === SAUVEGARDE / CHARGEMENT DE SESSION (point 7) ===

  # Deliberately a single explicit button — NOT automatic — so it never

  # interferes with testthat runs (which never touch global_data or the UI)

  # and never silently overwrites a session the user wants to keep separate.

  # Scope: the FULL global_data (sc_obj + bulk_obj + spatial_obj) rather than

  # a per-module save, because a partial save would silently desync from

  # whatever else is loaded when the user reopens the app.

  # NOTE spatial_obj: only $sketch (in-RAM Seurat) round-trips faithfully in

  # the .rds itself; $bpcells_dir is just a path string — the BPCells cache

  # directory it points to must still exist on disk for clustering/

  # deconvolution to work after reloading (see bpcells_cache_root(), a

  # persistent tools::R_user_dir() location, not tempdir()).

  output$save_session_btn <- downloadHandler(

    filename = function() paste0("transcriptoshiny_session_", Sys.Date(), ".rds"),

    content  = function(file) {

      session_snapshot <- list(

        sc_obj      = global_data$sc_obj,

        bulk_obj    = global_data$bulk_obj,

        spatial_obj = global_data$spatial_obj,

        saved_at    = Sys.time(),

        app_version = "TranscriptoShiny v2"

      )

      saveRDS(session_snapshot, file)

    }

  )



  observeEvent(input$load_session_file, {

    req(input$load_session_file)



    tryCatch({

      snapshot <- readRDS(input$load_session_file$datapath)



      if (!is.list(snapshot) || !all(c("sc_obj", "bulk_obj", "spatial_obj") %in% names(snapshot))) {

        showNotification(

          "❌ Fichier de session invalide — structure non reconnue.",

          type = "error", duration = 8

        )

        return()

      }



      global_data$sc_obj      <- snapshot$sc_obj

      global_data$bulk_obj    <- snapshot$bulk_obj

      global_data$spatial_obj <- snapshot$spatial_obj



      saved_label <- if (!is.null(snapshot$saved_at)) {

        format(snapshot$saved_at, "%Y-%m-%d %H:%M")

      } else "date inconnue"



      showNotification(

        paste("✓ Session restaurée (sauvegardée le", saved_label, ")"),

        type = "message", duration = 6

      )



      # Spatial : avertir si le cache BPCells sur disque n'est plus présent —

      # le sketch reste utilisable pour la visualisation, mais tout nouveau

      # calcul lourd (clustering/deconvolution) nécessitera un réimport.

      if (!is.null(snapshot$spatial_obj)) {

        disk_ok <- !is.null(snapshot$spatial_obj$bpcells_dir) &&

          dir.exists(snapshot$spatial_obj$bpcells_dir)

        if (!disk_ok) {

          showNotification(

            paste0("⚠️ Session Spatial : donnees BPCells introuvables sur ce disque ",

                   "(", snapshot$spatial_obj$bpcells_dir %||% "chemin inconnu", "). ",

                   "Seule la vue \"sketch\" est disponible — reimportez pour relancer ",

                   "clustering/deconvolution."),

            type = "warning", duration = 12

          )

        }

      }

    }, error = function(e) {

      showNotification(

        paste("❌ Erreur lors du chargement de la session:", conditionMessage(e)),

        type = "error", duration = 10

      )

    })

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

          tags$li(tags$strong("Spatial:"), " Visium / Xenium / CosMx — converti automatiquement en ",

                  "matrice BPCells sur disque + echantillon RAM (\"sketch\")")

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

          tags$li(tags$strong("Spatial:"), " QC → Clustering (BANKSY, asynchrone) → Deconvolution ",

                  "(RCTD/STdeconvolve, asynchrone) → Visualisation WebGL")

        ),

        

        hr(),

        

        h5("💾 Gestion de la Mémoire"),

        p("Le bouton ", tags$code("Nettoyer RAM"), 

          " permet de libérer la mémoire entre les analyses. Pour le Spatial, les calculs ",

          "lourds (clustering, déconvolution) s'exécutent dans des processus séparés (mirai) ",

          "qui ne bloquent jamais votre session."),

        

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



  # === PARAMÈTRES RAM AJUSTABLES À CHAUD ===

  # Bulk's matrices are small enough that this almost never matters there;

  # this exists primarily for Single-Cell/Spatial sessions with very large

  # objects, where the hardcoded defaults in global.R may need raising

  # (or lowering, on a shared/constrained machine) without restarting R.

  observeEvent(input$apply_ram_settings, {

    options(future.globals.maxSize = input$ram_future_gb * 1024^3)

    options(shiny.maxRequestSize    = input$ram_upload_gb * 1024^3)

    showNotification(

      sprintf("✓ Limites mises à jour : %d Go (tâches parallèles), %d Go (upload max).",

              input$ram_future_gb, input$ram_upload_gb),

      type = "message", duration = 5

    )

  })

}



# Lancement de l'application

shinyApp(ui, server)
