# =============================================================================
# mod_sc.R  —  Parent Router Module (v3.0 refactored)
# =============================================================================
# Orchestrates 7 child modules. Owns:
#   - navset_card_underline (8 tabs)
#   - shared_rv: cross-child ephemeral state
#   - tab navigation observer
#   - QC plot (declared here because QC panel lives in the parent UI)
#   - add_genes_to_viz() helper (centralised gene basket)
#   - group-by / volcano identity pickers
#
# Child modules sourced in global.R (or app.R):
#   source("R/mod_sc_pipeline.R")
#   source("R/mod_sc_annotation.R")
#   source("R/mod_sc_viz.R")
#   source("R/mod_sc_markers.R")
#   source("R/mod_sc_corr.R")
#   source("R/mod_sc_pathways.R")
#   source("R/mod_sc_trajectory.R")
#
# State contract:
#   global_data$sc_obj   — Seurat object (read/write, owned by app.R)
#   shared_rv            — ephemeral cross-child state (owned here):
#     $markers_data      : data.frame  | mod_sc_markers writes
#     $correlated_genes  : data.frame  | mod_sc_corr writes
#     $pathway_results   : data.frame  | mod_sc_pathways writes
#     $selected_genes    : character[] | all children may append
#     $active_tab        : string      | any child sets; parent observes
# =============================================================================


# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    
    # ── Sidebar ──────────────────────────────────────────────────────────
    sidebar = sidebar(
      width = 420,
      title = "Single-Cell Workflow",
      
      div(
        class = "alert alert-info",
        style = "font-size:0.8rem;padding:5px;",
        bsicons::bs_icon("info-circle"),
        "Etapes sequentielles recommandees."
      ),
      
      accordion(
        id = ns("acc_workflow"),
        open = "1. Pipeline",
        
        # ── Step 1 — Pipeline ───────────────────────────────────────────
        accordion_panel(
          "1. Pipeline", icon = icon("cogs"),
          mod_sc_pipeline_ui(ns("pipeline"))
        ),
        
        # ── Step 2 — Annotation ─────────────────────────────────────────
        accordion_panel(
          "2. Annotation", icon = icon("user-tag"),
          mod_sc_annotation_ui(ns("annotation"))
        ),
        
        # ── Step 3 — Visualisation ──────────────────────────────────────
        accordion_panel(
          "3. Visualisation", icon = icon("chart-area"),
          mod_sc_viz_ui(ns("viz"))
        ),
        
        # ── Step 4 — Marqueurs ──────────────────────────────────────────
        accordion_panel(
          "4. Marqueurs", icon = icon("magnifying-glass-chart"),
          mod_sc_markers_ui(ns("markers"))
        ),
        
        # ── Step 5 — Gene Correlation ───────────────────────────────────
        accordion_panel(
          "5. Gene Correlation", icon = icon("project-diagram"),
          mod_sc_corr_ui(ns("corr"))
        ),
        
        # ── Step 6 — Pathway Enrichment ─────────────────────────────────
        accordion_panel(
          "6. Pathway Enrichment", icon = icon("sitemap"),
          mod_sc_pathways_ui(ns("pathways"))
        ),
        
        # ── Step 7 — Trajectory ─────────────────────────────────────────
        accordion_panel(
          "7. Trajectory Analysis", icon = icon("route"),
          mod_sc_trajectory_ui(ns("trajectory"))
        ),

        # ── Step 8 — Rapport Complet ─────────────────────────────────────
        accordion_panel(
          "8. Rapport Complet", icon = icon("file-export"),

          div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
              "Génère un rapport autonome (QC, Réduction Dimensionnelle, Annotation, ",
              "Marqueurs, Corrélation, Pathways, Trajectoire) — partageable sans installation R."),

          textInput(ns("report_title"), "Titre du rapport",
                    value = "Analyse Single-Cell", placeholder = "Ex: PBMC Patient Cohort"),
          textInput(ns("report_subtitle"), "Sous-titre (optionnel)"),
          textAreaInput(ns("report_notes"), "Notes / Commentaires (markdown supporté)",
                        rows = 3, placeholder = "Ex: Clustering à résolution 0.5, voir cahier de labo."),

          checkboxGroupInput(ns("report_sections"), "Sections à inclure",
                             choices = c("QC" = "qc",
                                         "Réduction Dimensionnelle" = "dim",
                                         "Annotation" = "annotation",
                                         "Marqueurs" = "markers",
                                         "Réseau de Corrélation" = "correlation",
                                         "Pathway Enrichment" = "pathway",
                                         "Trajectoire" = "trajectory"),
                             selected = c("qc", "dim", "annotation", "markers", "pathway")),

          radioButtons(ns("report_format"), "Format de sortie",
                      choices = c("HTML interactif" = "html",
                                  "PDF statique"     = "pdf",
                                  "Les deux (.zip)"  = "both"),
                      selected = "html"),

          conditionalPanel(
            condition = "input.report_format != 'pdf'", ns = ns,
            checkboxInput(ns("report_interactive"), "Graphiques interactifs (DimPlot, Annotation, Trajectoire) — HTML uniquement",
                         value = TRUE)
          ),
          div(class = "small text-muted",
              "Le PDF requiert LaTeX (package 'tinytex') et repasse automatiquement en graphiques statiques."),

          downloadButton(ns("dl_report"), "📄 Générer le Rapport",
                         class = "btn-dark w-100 mt-2"),

          div(class = "small text-muted mt-1", textOutput(ns("report_status")))
        )
      ) # /accordion
    ), # /sidebar
    
    # ── Main panel ────────────────────────────────────────────────────────
    navset_card_underline(
      id    = ns("main_tabs"),
      title = "Resultats",
      
      # Tab: Visualisation (default landing)
      nav_panel(
        "Graphiques", value = "tab_viz",
        mod_sc_viz_output_ui(ns("viz"))
      ),
      
      # Tab: Markers table
      nav_panel(
        "Table Marqueurs", value = "tab_table",
        mod_sc_markers_output_ui(ns("markers"))
      ),
      
      # Tab: Annotation
      nav_panel(
        "Annotation", value = "tab_annotation",
        mod_sc_annotation_output_ui(ns("annotation"))
      ),
      
      # Tab: Correlation
      nav_panel(
        "Genes Correles", value = "tab_correlation",
        mod_sc_corr_output_ui(ns("corr"))
      ),
      
      # Tab: Pathways
      nav_panel(
        "Pathways", value = "tab_pathway",
        mod_sc_pathways_output_ui(ns("pathways"))
      ),
      
      # Tab: Trajectory
      nav_panel(
        "Trajectory", value = "tab_trajectory",
        mod_sc_trajectory_output_ui(ns("trajectory"))
      ),
      
      # Tab: QC (owned by parent — shows before pipeline commits sc_obj)
      nav_panel(
        "QC", value = "tab_qc",
        card(
          max_height = 750,
          div(class  = "card-header bg-light",
              h5("Controle Qualite", class = "card-title mb-0")),
          plotOutput(ns("plot_qc"), height = "650px")
        )
      )
    ) # /navset_card_underline
    
  ) # /layout_sidebar
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # ── 1. Shared ephemeral state (cross-child) ───────────────────────────
    shared_rv <- reactiveValues(
      markers_data     = NULL,   # data.frame  — written by mod_sc_markers
      correlated_genes = NULL,   # data.frame  — written by mod_sc_corr
      corr_target_gene = NULL,   # character   — written by mod_sc_corr (for report reuse)
      pathway_results  = NULL,   # data.frame  — written by mod_sc_pathways
      pathway_db       = NULL,   # character   — written by mod_sc_pathways (for report reuse)
      selected_genes   = character(0),  # gene basket — any child may append
      active_tab       = NULL    # target tab  — any child may set
    )
    
    # ── 2. Tab navigation bridge ──────────────────────────────────────────
    # Any child writes shared_rv$active_tab; the parent performs nav_select.
    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(
        id       = "main_tabs",   # bare id — NS is applied by the session
        selected = shared_rv$active_tab,
        session  = session
      )
    })
    
    # ── 3. QC plot (parent-owned — sc_obj not yet committed by pipeline) ──
    output$plot_qc <- renderPlot({
      req(global_data$sc_obj)
      VlnPlot(global_data$sc_obj,
              features = c("nFeature_RNA", "nCount_RNA", "percent.mt"),
              ncol     = 3,
              pt.size  = 0)
    })
    
    # ── 4. Call child servers ─────────────────────────────────────────────
    mod_sc_pipeline_server(   "pipeline",   global_data, shared_rv)
    mod_sc_annotation_server( "annotation", global_data, shared_rv)
    mod_sc_viz_server(        "viz",        global_data, shared_rv)
    mod_sc_markers_server(    "markers",    global_data, shared_rv)
    mod_sc_corr_server(       "corr",       global_data, shared_rv)
    mod_sc_pathways_server(   "pathways",   global_data, shared_rv)
    mod_sc_trajectory_server( "trajectory", global_data, shared_rv)

    # ── 5. Rapport Complet (point harmonisation Bulk <-> Single-Cell) ──────
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) "Importez et traitez un objet Single-Cell pour activer le rapport."
      else "Prêt — sélectionnez les sections puis cliquez sur 'Générer le Rapport'."
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html = "html", pdf = "pdf", both = "zip")
        paste0("rapport_singlecell_", Sys.Date(), ".", ext)
      },
      content  = function(file) {
        req(global_data$sc_obj)

        template_path <- file.path("modules", "sc_report_template.Rmd")
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite = TRUE)

        render_params <- list(
          sc_obj           = global_data$sc_obj,
          markers_data     = shared_rv$markers_data,
          pathway_results  = shared_rv$pathway_results,
          pathway_db       = shared_rv$pathway_db,
          correlated_genes = shared_rv$correlated_genes,
          corr_target_gene = shared_rv$corr_target_gene,
          sections         = input$report_sections %||% character(0),
          reduction        = "umap",
          group_by         = "seurat_clusters",
          report_title     = input$report_title %||% "Analyse Single-Cell",
          report_subtitle  = input$report_subtitle %||% "",
          report_notes     = input$report_notes %||% "",
          interactive      = isTRUE(input$report_interactive) && input$report_format != "pdf"
        )

        withProgress(message = "Génération du rapport...", value = 0.2, {

          formats_needed <- switch(input$report_format,
            html = "html_document", pdf = "pdf_document", both = c("html_document", "pdf_document"))

          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail = paste("Rendu", fmt))
            out_path <- file.path(tempdir(), paste0("sc_report_", fmt, "_", as.integer(Sys.time())))
            res <- tryCatch({
              rmarkdown::render(
                input = tmp_rmd, output_format = fmt, output_file = out_path,
                params = render_params, envir = new.env(parent = globalenv()), quiet = TRUE
              )
            }, error = function(e) {
              showNotification(
                paste0("❌ Erreur génération ", fmt, ": ", conditionMessage(e),
                       if (fmt == "pdf_document") " (vérifiez tinytex::install_tinytex())" else ""),
                type = "error", duration = 12
              )
              NULL
            })
            if (!is.null(res)) out_files <- c(out_files, res)
          }

          if (length(out_files) == 0) {
            stop("Aucun format n'a pu être généré — voir la notification d'erreur.")
          } else if (length(out_files) == 1) {
            file.copy(out_files[1], file, overwrite = TRUE)
          } else {
            zip::zip(file, files = out_files, mode = "cherry-pick")
          }
        })
      }
    )

  }) # /moduleServer
}
