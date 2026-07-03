# =============================================================================
# mod_sc.R  —  Parent Router Module
# Step-2.5b : SC auto-pipeline | Step-3.0 : optional FindAllMarkers + Pathway
# ns() fix  : session$ns used inside showModal for correct module scoping
# =============================================================================

mod_sc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 420, title = "Single-Cell Workflow",
      div(class="alert alert-info", style="font-size:0.8rem;padding:5px;",
          bsicons::bs_icon("info-circle"), "Etapes sequentielles recommandees."),
      actionButton(ns("btn_auto_pipeline_sc"), "\u25b6 Lancer Pipeline Complet (SC)",
                   icon = icon("play-circle"), class = "btn-outline-success w-100 mb-1"),
      verbatimTextOutput(ns("sc_auto_log")),
      accordion(
        id = ns("acc_workflow"), open = "1. Pipeline",
        accordion_panel("1. Pipeline",         icon = icon("cogs"),                 mod_sc_pipeline_ui(ns("pipeline"))),
        accordion_panel("2. Annotation",       icon = icon("user-tag"),             mod_sc_annotation_ui(ns("annotation"))),
        accordion_panel("3. Visualisation",    icon = icon("chart-area"),           mod_sc_viz_ui(ns("viz"))),
        accordion_panel("4. Marqueurs",        icon = icon("magnifying-glass-chart"),mod_sc_markers_ui(ns("markers"))),
        accordion_panel("5. Gene Correlation", icon = icon("project-diagram"),      mod_sc_corr_ui(ns("corr"))),
        accordion_panel("6. Pathway Enrichment",icon = icon("sitemap"),             mod_sc_pathways_ui(ns("pathways"))),
        accordion_panel("7. Trajectory Analysis",icon = icon("route"),              mod_sc_trajectory_ui(ns("trajectory"))),
        accordion_panel(
          "8. Rapport Complet", icon = icon("file-export"),
          div(class="alert alert-light", style="font-size:0.85em;border-left:3px solid #2C3E50;",
              "Rapport autonome (QC, R\u00e9duction, Annotation, Marqueurs, Pathways, Trajectoire)."),
          textInput(ns("report_title"), "Titre", value="Analyse Single-Cell"),
          textInput(ns("report_subtitle"), "Sous-titre (optionnel)"),
          textAreaInput(ns("report_notes"), "Notes", rows=3),
          checkboxGroupInput(ns("report_sections"), "Sections",
            choices=c("QC"="qc","R\u00e9duction Dimensionnelle"="dim","Annotation"="annotation",
                      "Marqueurs"="markers","R\u00e9seau Corr\u00e9lation"="correlation",
                      "Pathway Enrichment"="pathway","Trajectoire"="trajectory"),
            selected=c("qc","dim","annotation","markers","pathway")),
          radioButtons(ns("report_format"), "Format",
            choices=c("HTML interactif"="html","PDF statique"="pdf","Les deux (.zip)"="both"),
            selected="html"),
          conditionalPanel(condition="input.report_format != 'pdf'", ns=ns,
            checkboxInput(ns("report_interactive"), "Graphiques interactifs (HTML)", value=TRUE)),
          div(class="small text-muted", "PDF requiert tinytex::install_tinytex()."),
          downloadButton(ns("dl_report"), "\U0001f4c4 G\u00e9n\u00e9rer le Rapport", class="btn-dark w-100 mt-2"),
          div(class="small text-muted mt-1", textOutput(ns("report_status")))
        )
      )
    ),
    navset_card_underline(
      id = ns("main_tabs"), title = "Resultats",
      nav_panel("Graphiques",     value="tab_viz",         mod_sc_viz_output_ui(ns("viz"))),
      nav_panel("Table Marqueurs",value="tab_table",       mod_sc_markers_output_ui(ns("markers"))),
      nav_panel("Annotation",     value="tab_annotation",  mod_sc_annotation_output_ui(ns("annotation"))),
      nav_panel("Genes Correles", value="tab_correlation", mod_sc_corr_output_ui(ns("corr"))),
      nav_panel("Pathways",       value="tab_pathway",     mod_sc_pathways_output_ui(ns("pathways"))),
      nav_panel("Trajectory",     value="tab_trajectory",  mod_sc_trajectory_output_ui(ns("trajectory"))),
      nav_panel("QC",             value="tab_qc",
        card(max_height=750,
          div(class="card-header bg-light", h5("Controle Qualite", class="card-title mb-0")),
          plotOutput(ns("plot_qc"), height="650px")))
    )
  )
}

mod_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    shared_rv <- reactiveValues(
      markers_data=NULL, correlated_genes=NULL, corr_target_gene=NULL,
      pathway_results=NULL, pathway_db=NULL,
      selected_genes=character(0), active_tab=NULL
    )

    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(id="main_tabs", selected=shared_rv$active_tab, session=session)
    })

    output$plot_qc <- renderPlot({
      req(global_data$sc_obj)
      VlnPlot(global_data$sc_obj,
              features=c("nFeature_RNA","nCount_RNA","percent.mt"), ncol=3, pt.size=0)
    })

    # ── Child servers ──────────────────────────────────────────────────────
    mod_sc_pipeline_server(   "pipeline",   global_data, shared_rv)
    mod_sc_annotation_server( "annotation", global_data, shared_rv)
    mod_sc_viz_server(        "viz",        global_data, shared_rv)
    mod_sc_markers_server(    "markers",    global_data, shared_rv)
    mod_sc_corr_server(       "corr",       global_data, shared_rv)
    mod_sc_pathways_server(   "pathways",   global_data, shared_rv)
    mod_sc_trajectory_server( "trajectory", global_data, shared_rv)

    # ── SC Auto-pipeline ───────────────────────────────────────────────────
    sc_log_rv <- reactiveVal("")
    output$sc_auto_log <- renderText({ sc_log_rv() })

    observeEvent(input$btn_auto_pipeline_sc, {
      req(global_data$sc_obj)
      ns_m <- session$ns   # ns() fix: modal inputs must be module-scoped

      showModal(modalDialog(
        title="\u25b6 Pipeline SC \u2014 Param\u00e8tres", size="m", easyClose=TRUE,
        fluidRow(
          column(6,
            h6("QC", style="font-weight:bold;"),
            numericInput(ns_m("sc_ap_min_gene"), "Min g\u00e8nes/cellule", 200, min=0),
            numericInput(ns_m("sc_ap_max_gene"), "Max g\u00e8nes/cellule", 6000, min=0),
            sliderInput(ns_m("sc_ap_mt"), "% Mito max", 0, 50, 15, step=1)
          ),
          column(6,
            h6("Normalisation & R\u00e9duction", style="font-weight:bold;"),
            radioButtons(ns_m("sc_ap_norm"), "Normalisation",
                         c("LogNormalize"="log","SCTransform"="sct")),
            sliderInput(ns_m("sc_ap_pca_dim"), "Dims PCA", 5, 50, 20),
            numericInput(ns_m("sc_ap_res"), "R\u00e9solution clustering", 0.5, min=0.1, step=0.1)
          )
        ),
        h6("Options suppl\u00e9mentaires", style="font-weight:bold;"),
        checkboxInput(ns_m("sc_ap_markers"),
                      "\U0001f9ec FindAllMarkers apr\u00e8s clustering (top marqueurs par cluster)",
                      value=FALSE),
        checkboxInput(ns_m("sc_ap_pathway"),
                      "\U0001f9ec Pathway ORA sur top marqueurs (n\u00e9cessite FindAllMarkers)",
                      value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_pathway")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_pathway_db"), "Base",
                       c("GO BP"="GOBP","KEGG"="KEGG","Reactome"="Reactome"))),
            column(6, selectInput(ns_m("sc_ap_pathway_org"), "Organisme",
                       c("Humain"="human","Souris"="mouse")))
          )
        ),
        footer=tagList(
          modalButton("Annuler"),
          actionButton(ns_m("sc_ap_confirm"), "\u25b6 Lancer", class="btn-success")
        )
      ))
    })

    observeEvent(input$sc_ap_confirm, {
      removeModal()
      req(global_data$sc_obj)
      ll <- character(0)
      log_sc <- function(msg) {
        ll <<- c(ll, paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg))
        sc_log_rv(paste(ll, collapse="\n"))
      }
      p <- shiny::Progress$new(); on.exit(p$close())

      tryCatch({
        obj <- global_data$sc_obj

        # QC
        p$set(0.05, "QC..."); log_sc("QC...")
        mt_pat <- if (any(grepl("^MT-", rownames(obj)))) "^MT-"
                  else if (any(grepl("^mt-", rownames(obj)))) "^mt-" else NULL
        obj[["percent.mt"]] <- if (!is.null(mt_pat)) PercentageFeatureSet(obj, pattern=mt_pat) else 0
        obj <- subset(obj, subset = nFeature_RNA > input$sc_ap_min_gene &
                        nFeature_RNA < input$sc_ap_max_gene & percent.mt < input$sc_ap_mt)
        if (ncol(obj) < 10) stop("Moins de 10 cellules apr\u00e8s QC.")
        log_sc(sprintf("\u2713 QC : %d cellules", ncol(obj)))

        # Normalisation
        p$set(0.2, "Normalisation..."); log_sc("Normalisation...")
        if (input$sc_ap_norm == "sct") {
          obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")
        } else {
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj, verbose=FALSE)
          obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
          obj <- ScaleData(obj, verbose=FALSE)
        }
        log_sc("\u2713 Normalisation OK")

        # PCA
        p$set(0.40, "PCA...")
        obj <- RunPCA(obj, verbose=FALSE, npcs=input$sc_ap_pca_dim)
        log_sc(sprintf("\u2713 PCA (%d dims)", input$sc_ap_pca_dim))

        # Clustering
        p$set(0.55, "Clustering...")
        obj <- FindNeighbors(obj, dims=1:input$sc_ap_pca_dim, verbose=FALSE)
        obj <- FindClusters(obj, resolution=input$sc_ap_res, verbose=FALSE)
        n_cl <- length(levels(obj$seurat_clusters))
        log_sc(sprintf("\u2713 %d clusters (res %.1f)", n_cl, input$sc_ap_res))

        # UMAP
        p$set(0.70, "UMAP...")
        obj <- RunUMAP(obj, dims=1:input$sc_ap_pca_dim, verbose=FALSE)
        log_sc("\u2713 UMAP OK")

        # Optional: FindAllMarkers
        if (isTRUE(input$sc_ap_markers)) {
          p$set(0.82, "FindAllMarkers...")
          log_sc("FindAllMarkers (wilcox, min.pct=0.1)...")
          markers <- tryCatch({
            Idents(obj) <- obj$seurat_clusters
            FindAllMarkers(obj, only.pos=TRUE, min.pct=0.1,
                           logfc.threshold=0.25, verbose=FALSE)
          }, error=function(e) { log_sc(paste("\u26a0\ufe0f Markers:", e$message)); NULL })
          if (!is.null(markers) && nrow(markers) > 0) {
            markers <- as.data.frame(markers); rownames(markers) <- NULL
            # normalize cols (ensure gene/cluster/avg_log2FC cols exist)
            if (!"gene"       %in% colnames(markers)) markers$gene       <- rownames(markers)
            if (!"avg_log2FC" %in% colnames(markers)) markers$avg_log2FC <- markers$avg_logFC %||% 0
            if (!"p_val_adj"  %in% colnames(markers)) markers$p_val_adj  <- 1
            if (!"cluster"    %in% colnames(markers)) markers$cluster    <- "Unknown"
            if (!"pct.1"      %in% colnames(markers)) markers$pct.1      <- NA_real_
            if (!"pct.2"      %in% colnames(markers)) markers$pct.2      <- NA_real_
            shared_rv$markers_data <- markers
            log_sc(sprintf("\u2713 %d marqueurs trouv\u00e9s", nrow(markers)))

            # Optional: Pathway ORA on top markers
            if (isTRUE(input$sc_ap_pathway)) {
              log_sc("Pathway ORA (top 100 marqueurs)...")
              top_g <- head(markers$gene[order(markers$p_val_adj)], 100)
              pw <- tryCatch(
                run_pathway_enrichment(top_g, organism=input$sc_ap_pathway_org,
                                       database=input$sc_ap_pathway_db, pval_cutoff=0.05),
                error=function(e) { log_sc(paste("\u26a0\ufe0f Pathway:", e$message)); NULL }
              )
              if (!is.null(pw) && nrow(pw) > 0) {
                shared_rv$pathway_results <- pw
                shared_rv$pathway_db      <- input$sc_ap_pathway_db
                log_sc(sprintf("\u2713 %d pathways enrichis", nrow(pw)))
              }
            }
          }
        }

        global_data$sc_obj   <- obj
        shared_rv$active_tab <- "tab_viz"
        showNotification(sprintf("\u2713 Pipeline SC : %d cellules, %d clusters", ncol(obj), n_cl),
                         type="message", duration=6)

      }, error=function(e) {
        log_sc(paste("\u274c Erreur:", e$message))
        showNotification(paste("Erreur pipeline SC:", e$message), type="error", duration=10)
      })
    })

    # ── Report ─────────────────────────────────────────────────────────────
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) "Importez et traitez un objet SC."
      else "Pr\u00eat \u2014 s\u00e9lectionnez les sections."
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html="html", pdf="pdf", both="zip")
        paste0("rapport_singlecell_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(global_data$sc_obj)
        template_path <- file.path("modules", "sc", "sc_report_template.Rmd")
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite=TRUE)
        render_params <- list(
          sc_obj=global_data$sc_obj, markers_data=shared_rv$markers_data,
          pathway_results=shared_rv$pathway_results, pathway_db=shared_rv$pathway_db,
          correlated_genes=shared_rv$correlated_genes, corr_target_gene=shared_rv$corr_target_gene,
          sections=input$report_sections %||% character(0),
          reduction="umap", group_by="seurat_clusters",
          report_title=input$report_title %||% "Analyse Single-Cell",
          report_subtitle=input$report_subtitle %||% "",
          report_notes=input$report_notes %||% "",
          interactive=isTRUE(input$report_interactive) && input$report_format != "pdf"
        )
        withProgress(message="G\u00e9n\u00e9ration du rapport...", value=0.2, {
          formats_needed <- switch(input$report_format,
            html="html_document", pdf="pdf_document", both=c("html_document","pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail=paste("Rendu", fmt))
            out_path <- file.path(tempdir(), paste0("sc_report_", as.integer(Sys.time())))
            res <- tryCatch(
              rmarkdown::render(input=tmp_rmd, output_format=fmt, output_file=out_path,
                                params=render_params, envir=new.env(parent=globalenv()), quiet=TRUE),
              error=function(e) { showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)),
                                                   type="error", duration=12); NULL })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (!length(out_files)) stop("Aucun format g\u00e9n\u00e9r\u00e9.")
          else if (length(out_files)==1) file.copy(out_files[1], file, overwrite=TRUE)
          else zip::zip(file, files=out_files, mode="cherry-pick")
        })
      }
    )

  }) # /moduleServer
}
