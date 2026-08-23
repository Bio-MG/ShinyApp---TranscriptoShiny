# app.R - TranscriptoShiny v2a

source("global.R")

# Helpers (refactor : extraits de global.R — l'ordre entre eux n'a pas
# d'importance, R ne résout les appels de fonction qu'à l'exécution).
source("helpers_io.R")
source("helpers_sc.R")
source("helpers_sc_bpcells.R")
source("helpers_bulk.R")
source("helpers_pathway.R")
source("R/palettes.R")

# --- NOUVEAU (module Spatial v3, BPCells + mirai) ---
# Ces deux fichiers ne dépendent que des packages chargés par global.R —
# doivent être sourcés avant tout module qui les utilise (import spatial,
# modules/spatial/*). init_spatial_daemons() est idempotent : sans risque si
# rappelé plus tard (voir mod_spatial.R, appel défensif dans le module).
#
# AUDIT FIX (quick win) : R/utils_spatial_io.R n'est plus sourcé qu'ICI —
# il l'était aussi en tête de global.R (avant même le chargement des
# packages), un double-sourcing confirmé par deux audits indépendants comme
# risque de divergence latente. Seule source de vérité désormais.
source("R/utils_spatial_async.R")
source("R/utils_spatial_io.R")
# Phase 4 (multi-echantillons) — pure helper, depends only on
# write_mirai_log() (utils_spatial_async.R, sourced above); no Shiny
# reactivity, safe to source alongside the other two.
source("R/utils_spatial_multi.R")
# Chantier 3 (architecture fix — spatial deconvolution reference pipeline):
# multi-format reference reader (.rds/.h5ad/.h5/.loom) + on-disk artifact
# preparation for RCTD/Label Transfer. Runs ONLY on the main Shiny process
# (mod_spatial_deconv.R) — deliberately NOT part of init_spatial_daemons()'s
# source_files: the mirai daemon now loads the PREPARED artifact with base R
# + BPCells only, never this file. See its header for the full rationale
# (fixes "could not find function load_single_cell_data").
source("R/utils_spatial_reference.R")
# FIX 2026-08 : init des daemons mirai DIFFEREE — elle n'est plus faite au
# lancement mais a la premiere ouverture de l'onglet Spatial
# (modules/spatial/mod_spatial.R : "if (!spatial_daemons_ready())
# init_spatial_daemons(6)"). Raison : au demarrage, 6 processus R de plus en
# concurrence avec les workers future faisait exploser les connectTimeout
# ("Cluster setup failed", ".dispatcher_wait fige"). Le badge d'etat du pool
# dans l'UI affichera simplement "inactive" jusqu'a cet appel paresseux.
# Pour restaurer l'init eager, decommenter :
# tryCatch(
#   init_spatial_daemons(n_daemons = 6),
#   error = function(e) warning("Initialisation des daemons mirai (spatial) impossible : ", conditionMessage(e))
# )
source("R/utils_spatial_niche.R")
# Backlog #6 (RAM STdeconvolve/RCTD) : capping HVG avant densification —
# pure helper, doit AUSSI etre dans source_files de init_spatial_daemons()
# (R/utils_spatial_async.R) car appelee depuis un daemon mirai.
source("R/utils_spatial_deconv_prep.R")
# Moyen terme (export/auto-pipeline, voir handoff_spatial_bio-mg.md) : paquet
# complet (.zip) + script R reproductible pour le module Spatial. Pure
# helpers (aucune reactivite Shiny) appelés UNIQUEMENT depuis
# modules/spatial/mod_spatial_export.R sur le thread principal (jamais dans
# un daemon mirai) -- pas besoin de l'ajouter à init_spatial_daemons().
source("R/utils_spatial_export.R")
# Feedback biologiste (rapport HTML/PDF multi-echantillons) : idem, pure
# helper (dataset-snapshot builder + resolution du chemin du template Rmd),
# appelé UNIQUEMENT depuis modules/spatial/mod_spatial_report.R sur le
# thread principal (rmarkdown::render() n'est jamais lancé dans un daemon).
source("R/utils_spatial_report.R")

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

# --- Spatial (parent + sous-modules enfants) ---
source("modules/spatial/mod_spatial_qc.R")
source("modules/spatial/mod_spatial_cluster.R")

source("modules/spatial/mod_spatial_viz.R")
source("modules/spatial/mod_spatial_multi.R")
source("modules/spatial/mod_spatial_niche.R")

# --- Spatial sub-deconv (Phase 2 split) ---
source("R/utils_spatial_deconv_tasks.R") # BEFORE daemon init if not already
source("modules/spatial/deconv/mod_spatial_deconv_ui.R")
source("modules/spatial/deconv/mod_spatial_deconv_reference.R")
source("modules/spatial/deconv/mod_spatial_deconv_refviz.R")
source("modules/spatial/deconv/mod_spatial_deconv_outputs.R")
source("modules/spatial/deconv/mod_spatial_deconv.R")
# Moyen terme (voir handoff_spatial_bio-mg.md) : pipeline automatique 1-clic
# + export (paquet .zip / script reproductible) -- sourcés avant
# mod_spatial.R, qui appelle mod_spatial_pipeline_server()/
# mod_spatial_export_server() au meme titre que les autres sous-modules.
source("modules/spatial/mod_spatial_pipeline.R")
source("modules/spatial/mod_spatial_export.R")
source("modules/spatial/mod_spatial_report.R")
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

    spatial_obj = NULL,  # Spatial : liste (sketch, bpcells_dir, coords, ...) — voir R/utils_spatial_io.R
                          # — POINTE TOUJOURS vers l'echantillon "actif" (voir spatial_datasets ci-dessous)

    spatial_datasets = list(),

    active_spatial_dataset = NULL,

    # v10 (backlog court-terme #2) : cache des resultats par-echantillon
    # (QC/clusters/deconv/niches), ecrit/lu par mod_spatial.R -- vit ici
    # (plutot que dans un reactiveVal interne au module) precisement pour
    # etre capturable par save_session_btn / restaurable par
    # load_session_file ci-dessous.
    spatial_results_cache = list(),

    # backlog court-terme #1 : reference scRNA-seq PARTAGEE (preparee une
    # fois depuis l'onglet Import > Spatial), reutilisee par RCTD ET Label
    # Transfer sans reparser -- voir mod_import_spatial.R et
    # mod_spatial_deconv.R. list(path=, n_cells=, n_genes=, backend=,
    # celltype_col=, source_label=, n_dropped_rare=, created_at=) ou NULL.
    spatial_reference = NULL,

    # Feedback biologiste (rapport multi-echantillons) : dernier resultat
    # d'integration conjointe (mod_spatial_multi.R), miroir en LECTURE pour
    # mod_spatial_report.R -- voir mod_spatial_multi.R v2. list(embeddings=,
    # n_per_dataset=, reduction_used=, datasets=, computed_at=) ou NULL.
    spatial_multi_integration = NULL,

    spatial_reimport_signal = NULL   # {name=, at=} -- signal re-import sous nom deja actif (voir mod_spatial.R)

  )

  

  # === MODULES D'IMPORT ===

  mod_import_sc_server("import_sc", global_data)

  mod_import_bulk_server("import_bulk", global_data)

  mod_import_spatial_server("import_spatial", global_data)
  
  
  # AFTER (fix #1) : mod_geo_server("geo", global_data) shared_rv undefined at app scope → crash
  mod_geo_server("geo", global_data)
  

  # === MODULES D'ANALYSE ===

  mod_sc_server("sc", global_data)

  # AUDIT FIX (quick win) : l'appel racine mod_sc_mapping_server("mapping", global_data)
  # a ete retire d'ici. Il enregistrait un moduleServer sous le namespace
  # top-level "mapping", qui n'a JAMAIS eu de UI correspondante (le vrai
  # panneau "0. Mapping IDs" du Single-Cell vit sous le namespace "sc-mapping",
  # cree par l'appel imbrique mod_sc_mapping_server("mapping", global_data)
  # DEJA present a l'interieur de mod_sc_server(), voir modules/sc/mod_sc.R).
  # C'etait donc un module fantome : aucun input ne pointait jamais vers son
  # namespace "mapping" nu, seulement de la reactivite morte. Retire.

  mod_bulk_server("bulk", global_data)

  mod_spatial_server("spatial", global_data)

  

  # === STATUT DES OBJETS — INDICATEUR DE PROGRESSION GLOBAL (point 8) ===

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

    spatial_state <- if (is.null(global_data$spatial_obj)) {

      list(icon = "⚪", label = "Spatial", detail = "Aucune donnée")

    } else {

      obj <- global_data$spatial_obj

      n_total  <- obj$n_total %||% ncol(obj$sketch)

      n_sketch <- ncol(obj$sketch)

      disk_ok  <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)

      n_ds     <- length(global_data$spatial_datasets)

      list(

        icon   = if (disk_ok) "🟢" else "🟠",

        label  = "Spatial",

        detail = sprintf("%s elements (%s en RAM, sketch)%s%s%s",

                         format(n_total, big.mark = ","), format(n_sketch, big.mark = ","),

                         if (!disk_ok) " — disque introuvable" else "",

                         if (n_ds > 1) sprintf(" — %d echantillons charges", n_ds) else "",

                         if (!is.null(global_data$spatial_reference)) {

                           sprintf(" — reference partagee : %s cellules",

                                   format(global_data$spatial_reference$n_cells %||% 0, big.mark = ","))

                         } else "")

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

  output$save_session_btn <- downloadHandler(

    filename = function() paste0("transcriptoshiny_session_", Sys.Date(), ".rds"),

    content  = function(file) {

      session_snapshot <- list(

        sc_obj      = global_data$sc_obj,

        bulk_obj    = global_data$bulk_obj,

        spatial_obj = global_data$spatial_obj,

        spatial_datasets = global_data$spatial_datasets,

        active_spatial_dataset = global_data$active_spatial_dataset,

        spatial_results_cache = global_data$spatial_results_cache,

        spatial_reference = global_data$spatial_reference,

        spatial_multi_integration = global_data$spatial_multi_integration,

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

      global_data$spatial_datasets <- snapshot$spatial_datasets %||%
        (if (!is.null(snapshot$spatial_obj)) {
          stats::setNames(list(snapshot$spatial_obj), snapshot$spatial_obj$project %||% "Echantillon_1")
        } else list())

      global_data$active_spatial_dataset <- snapshot$active_spatial_dataset %||%
        (if (length(global_data$spatial_datasets) > 0) names(global_data$spatial_datasets)[1] else NULL)

      # v10 (backlog court-terme #2/#1) : absents des sessions sauvegardees

      # AVANT cette version -- %||% list()/NULL degrade proprement (cache

      # vide / pas de reference partagee) plutot que d'echouer sur un

      # snapshot ancien.

      global_data$spatial_results_cache <- snapshot$spatial_results_cache %||% list()

      global_data$spatial_reference     <- snapshot$spatial_reference %||% NULL

      global_data$spatial_multi_integration <- snapshot$spatial_multi_integration %||% NULL



      saved_label <- if (!is.null(snapshot$saved_at)) {

        format(snapshot$saved_at, "%Y-%m-%d %H:%M")

      } else "date inconnue"



      showNotification(

        paste("✓ Session restaurée (sauvegardée le", saved_label, ")"),

        type = "message", duration = 6

      )



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

      # v10 (backlog court-terme #1) : la reference scRNA-seq PARTAGEE est un

      # artefact sur DISQUE (tempdir(), voir prepare_reference_artifact())

      # -- jamais garanti survivre a un redemarrage/changement de machine,

      # contrairement au sketch (qui, lui, est bien serialise dans le .rds).

      # Meme logique d'avertissement que pour bpcells_dir ci-dessus.

      if (!is.null(global_data$spatial_reference)) {

        ref_disk_ok <- !is.null(global_data$spatial_reference$path) &&

          file.exists(global_data$spatial_reference$path)

        if (!ref_disk_ok) {

          showNotification(

            paste0("⚠️ Reference scRNA-seq partagee : artefact introuvable sur ce disque — ",

                   "re-uploadez-la depuis l'onglet Import > Spatial avant de relancer RCTD/",

                   "Label Transfer."),

            type = "warning", duration = 12

          )

          global_data$spatial_reference <- NULL

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

                  "matrice BPCells sur disque + echantillon RAM (\"sketch\"). Importez plusieurs ",

                  "echantillons pour activer l'onglet \"5. Multi-echantillons\" (comparaison/",

                  "integration Harmony conjointe).")

        ),

        

        tags$div(class = "alert alert-info", style = "font-size: 0.9rem;",

                 bsicons::bs_icon("lightbulb"), 

                 tags$strong(" Astuce:"), 

                 " Pour Harmony (correction de batch), importez ", 

                 tags$strong("2 échantillons ou plus"), 

                 " dans l'onglet Single-Cell (ou Spatial, voir \"Multi-echantillons\")."

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

          tags$li(tags$strong("Spatial:"), " Pipeline auto (1 clic) OU QC → Clustering (BANKSY, asynchrone) → ",

                  "Deconvolution (RCTD/Label Transfer/STdeconvolve, asynchrone) → Visualisation WebGL → ",

                  "Multi-echantillons (integration Harmony conjointe, asynchrone) → Export (paquet .zip / ",

                  "script R reproductible)")

        ),

        

        hr(),

        

        h5("💾 Gestion de la Mémoire"),

        p("Le bouton ", tags$code("Nettoyer RAM"), 

          " permet de libérer la mémoire entre les analyses. Pour le Spatial, les calculs ",

          "lourds (clustering, déconvolution, intégration multi-échantillons) s'exécutent dans ",

          "des processus séparés (mirai) qui ne bloquent jamais votre session."),

        

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

    global_data$spatial_datasets <- list()

    global_data$active_spatial_dataset <- NULL

    global_data$spatial_results_cache <- list()

    global_data$spatial_reference <- NULL

    global_data$spatial_multi_integration <- NULL

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
