# mod_sc.R  —  Parent Router Module
# Step-3.6 fixes:
#   - dl_report: tempfile(fileext=) so output is .html not .htm; timestamped filenames
#   - render_params: NULL guard for correlated_genes/corr_target_gene (was crashing PDF)
#   - Pipeline status bar (mirrors mod_bulk pipeline_status_bar)
#   - "Résumé Pipeline" nav_panel (mirrors mod_bulk "Résumé Up/Down")
#   - SC reproducible R script export (dl_sc_r_script)

mod_sc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 420, title = "Single-Cell Workflow",
      div(class="alert alert-info", style="font-size:0.8rem;padding:5px;",
          bsicons::bs_icon("info-circle"), "Etapes sequentielles recommandees."),
      actionButton(ns("btn_auto_pipeline_sc"), "\u25b6 Lancer Pipeline Complet (SC)",
                   icon=icon("play-circle"), class="btn-outline-success w-100 mb-1"),
      verbatimTextOutput(ns("sc_auto_log")),
      # ── Step-3.6: pipeline status bar ──────────────────────────────────
      uiOutput(ns("sc_pipeline_status_bar")),
      accordion(
        id=ns("acc_workflow"), open="1. Pipeline",
        accordion_panel("1. Pipeline",          icon=icon("cogs"),                  mod_sc_pipeline_ui(ns("pipeline"))),
        accordion_panel("2. Annotation",        icon=icon("user-tag"),              mod_sc_annotation_ui(ns("annotation"))),
        accordion_panel("3. Visualisation",     icon=icon("chart-area"),            mod_sc_viz_ui(ns("viz"))),
        accordion_panel("4. Marqueurs",         icon=icon("magnifying-glass-chart"),mod_sc_markers_ui(ns("markers"))),
        accordion_panel("5. Gene Correlation",  icon=icon("project-diagram"),       mod_sc_corr_ui(ns("corr"))),
        accordion_panel("6. Pathway Enrichment",icon=icon("sitemap"),               mod_sc_pathways_ui(ns("pathways"))),
        accordion_panel("7. Trajectory Analysis",icon=icon("route"),                mod_sc_trajectory_ui(ns("trajectory"))),
        accordion_panel(
          "8. Rapport Complet", icon=icon("file-export"),
          div(class="alert alert-light",style="font-size:0.85em;border-left:3px solid #2C3E50;",
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
          hr(),
          div(class="alert alert-light",style="font-size:0.82em;border-left:3px solid #18BC9C;",
              bsicons::bs_icon("code-slash"),
              " Script R reproductible (.zip) + objet Seurat trait\u00e9."),
          downloadButton(ns("dl_sc_r_script"), "\U0001f9fe Export Script R (.zip)", class="btn-outline-secondary w-100"),
          div(class="small text-muted mt-1", textOutput(ns("report_status")))
        )
      )
    ),
    navset_card_underline(
      id=ns("main_tabs"), title="Resultats",
      nav_panel("Graphiques",      value="tab_viz",         mod_sc_viz_output_ui(ns("viz"))),
      nav_panel("Table Marqueurs", value="tab_table",       mod_sc_markers_output_ui(ns("markers"))),
      nav_panel("Annotation",      value="tab_annotation",  mod_sc_annotation_output_ui(ns("annotation"))),
      nav_panel("Genes Correles",  value="tab_correlation", mod_sc_corr_output_ui(ns("corr"))),
      nav_panel("Pathways",        value="tab_pathway",     mod_sc_pathways_output_ui(ns("pathways"))),
      nav_panel("Trajectory",      value="tab_trajectory",  mod_sc_trajectory_output_ui(ns("trajectory"))),
      nav_panel("QC", value="tab_qc",
        card(max_height=750,
          div(class="card-header bg-light", h5("Controle Qualite", class="card-title mb-0")),
          plotOutput(ns("plot_qc"), height="650px"))),
      # ── Step-3.6: pipeline summary tab ─────────────────────────────────
      nav_panel("R\u00e9sum\u00e9 Pipeline", value="tab_summary",
        card(card_header("R\u00e9sum\u00e9 du Pipeline Single-Cell"),
             uiOutput(ns("pipeline_summary_panel"))))
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

    # ── Step-3.6: pipeline status bar ──────────────────────────────────────
    output$sc_pipeline_status_bar <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      meta <- obj@meta.data
      s_qc      <- if ("percent.mt"      %in% colnames(meta))          "\u2705" else "\u26aa"
      s_norm    <- if (length(tryCatch(VariableFeatures(obj), error=function(e) character(0))) > 0) "\u2705" else "\u26aa"
      s_cluster <- if ("seurat_clusters" %in% colnames(meta))          "\u2705" else "\u26aa"
      s_umap    <- if ("umap"            %in% names(obj@reductions))   "\u2705" else "\u26aa"
      s_annot   <- if (any(grepl("^SingleR_", colnames(meta))))        "\u2705" else "\u26aa"
      s_markers <- if (!is.null(shared_rv$markers_data))               "\u2705" else "\u26aa"
      div(style=paste0("display:flex;justify-content:space-around;font-size:0.72em;",
                       "background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;",
                       "padding:4px 2px;margin-bottom:8px;"),
          tags$span(style="padding:2px 4px;", s_qc,      " QC"),
          tags$span(style="padding:2px 4px;", s_norm,    " Norm"),
          tags$span(style="padding:2px 4px;", s_cluster, " Cluster"),
          tags$span(style="padding:2px 4px;", s_umap,    " UMAP"),
          tags$span(style="padding:2px 4px;", s_annot,   " Annot"),
          tags$span(style="padding:2px 4px;", s_markers, " Marqueurs"))
    })

    # ── Step-3.6: pipeline summary panel ───────────────────────────────────
    output$pipeline_summary_panel <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj))
        return(div(class="alert alert-info m-3", "Aucun objet Single-Cell charg\u00e9."))
      meta <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      reductions   <- names(obj@reductions)
      n_clusters   <- if ("seurat_clusters" %in% colnames(meta))
                        length(levels(factor(meta$seurat_clusters))) else NA

      rows <- list(
        c("Cellules",              format(ncol(obj), big.mark=",")),
        c("G\u00e8nes",            format(nrow(obj), big.mark=",")),
        c("R\u00e9ductions",       if (length(reductions)>0) paste(reductions,collapse=", ") else "\u2014"),
        c("Clusters",              if (!is.na(n_clusters)) as.character(n_clusters) else "Non calcul\u00e9"),
        c("Annotation SingleR",    if (length(singler_cols)>0) paste(singler_cols,collapse=", ") else "Non effectu\u00e9e"),
        c("G\u00e8nes variables",  if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0)))>0)
                                     format(length(VariableFeatures(obj)),big.mark=",") else "\u2014"),
        c("Marqueurs calcul\u00e9s",if (!is.null(shared_rv$markers_data))
                                     paste(nrow(shared_rv$markers_data),"marqueurs") else "Non calcul\u00e9s"),
        c("Pathways",              if (!is.null(shared_rv$pathway_results))
                                     paste(nrow(shared_rv$pathway_results),"pathways") else "Non calcul\u00e9s"),
        c("Pseudotemps",           if ("pseudotime" %in% colnames(meta)) "Calcul\u00e9" else "Non calcul\u00e9")
      )
      tagList(
        div(class="m-3",
          tags$table(class="table table-sm table-bordered",
            tags$tbody(lapply(rows, function(r) {
              tags$tr(tags$th(style="width:40%;",r[1]), tags$td(r[2]))
            }))
          ),
          div(class="small text-muted", paste("Mis \u00e0 jour :", format(Sys.time(),"%H:%M:%S")))
        )
      )
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
      ns_m <- session$ns
      showModal(modalDialog(
        title="\u25b6 Pipeline SC \u2014 Param\u00e8tres", size="m", easyClose=TRUE,
        fluidRow(
          column(6,
            h6("QC", style="font-weight:bold;"),
            numericInput(ns_m("sc_ap_min_gene"), "Min g\u00e8nes/cellule", 100, min=0),
            numericInput(ns_m("sc_ap_max_gene"), "Max g\u00e8nes/cellule", 8000, min=0),
            sliderInput(ns_m("sc_ap_mt"), "% Mito max", 0, 50, 20, step=1)
          ),
          column(6,
            h6("Normalisation & R\u00e9duction", style="font-weight:bold;"),
            radioButtons(ns_m("sc_ap_norm"), "Normalisation", c("LogNormalize"="log","SCTransform"="sct")),
            sliderInput(ns_m("sc_ap_pca_dim"), "Dims PCA", 5, 50, 20),
            numericInput(ns_m("sc_ap_res"), "R\u00e9solution clustering", 0.5, min=0.1, step=0.1)
          )
        ),
        h6("Options suppl\u00e9mentaires", style="font-weight:bold;"),
        checkboxInput(ns_m("sc_ap_singler"), "\U0001f9ec Annotation SingleR", value=FALSE),
        conditionalPanel(condition=sprintf("input['%s'] == true", ns_m("sc_ap_singler")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_singler_ref"), "R\u00e9f\u00e9rence",
                       c("Human Primary Cell Atlas"="hpca","Blueprint Encode"="blueprint",
                         "ImmGen (Souris)"="immgen","DICE Immune"="dice"))),
            column(6, radioButtons(ns_m("sc_ap_singler_level"), "Niveau",
                       c("Main"="main","Fine"="fine"), inline=TRUE))
          )
        ),
        checkboxInput(ns_m("sc_ap_markers"), "\U0001f9ec FindAllMarkers apr\u00e8s clustering", value=FALSE),
        checkboxInput(ns_m("sc_ap_pathway"), "\U0001f9ec Pathway ORA sur top marqueurs", value=FALSE),
        conditionalPanel(condition=sprintf("input['%s'] == true", ns_m("sc_ap_pathway")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_pathway_db"), "Base",
                       c("GO BP"="GOBP","KEGG"="KEGG","Reactome"="Reactome"))),
            column(6, selectInput(ns_m("sc_ap_pathway_org"), "Organisme",
                       c("Humain"="human","Souris"="mouse")))
          )
        ),
        footer=tagList(modalButton("Annuler"),
                       actionButton(ns_m("sc_ap_confirm"), "\u25b6 Lancer", class="btn-success"))
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
        p$set(0.05,"QC..."); log_sc("QC...")
        mt_pat <- if (any(grepl("^MT-",rownames(obj)))) "^MT-"
                  else if (any(grepl("^mt-",rownames(obj)))) "^mt-" else NULL
        obj[["percent.mt"]] <- if (!is.null(mt_pat)) PercentageFeatureSet(obj,pattern=mt_pat) else 0
        obj <- subset(obj, subset=nFeature_RNA > input$sc_ap_min_gene &
                        nFeature_RNA < input$sc_ap_max_gene & percent.mt < input$sc_ap_mt)
        if (ncol(obj) < 10) stop("Moins de 10 cellules apr\u00e8s QC.")
        log_sc(sprintf("\u2713 QC : %d cellules", ncol(obj)))
        p$set(0.2,"Normalisation..."); log_sc("Normalisation...")
        if (input$sc_ap_norm=="sct") {
          obj <- SCTransform(obj,verbose=FALSE,vst.flavor="v2")
        } else {
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj,verbose=FALSE)
          obj <- FindVariableFeatures(obj,nfeatures=2000,verbose=FALSE)
          obj <- ScaleData(obj,verbose=FALSE)
        }
        log_sc("\u2713 Normalisation OK")
        p$set(0.40,"PCA..."); obj <- RunPCA(obj,verbose=FALSE,npcs=input$sc_ap_pca_dim)
        log_sc(sprintf("\u2713 PCA (%d dims)",input$sc_ap_pca_dim))
        p$set(0.55,"Clustering...")
        obj <- FindNeighbors(obj,dims=1:input$sc_ap_pca_dim,verbose=FALSE)
        obj <- FindClusters(obj,resolution=input$sc_ap_res,verbose=FALSE)
        n_cl <- length(levels(obj$seurat_clusters))
        log_sc(sprintf("\u2713 %d clusters (res %.1f)",n_cl,input$sc_ap_res))
        p$set(0.70,"UMAP..."); obj <- RunUMAP(obj,dims=1:input$sc_ap_pca_dim,verbose=FALSE)
        log_sc("\u2713 UMAP OK")
        if (isTRUE(input$sc_ap_singler)) {
          if (!requireNamespace("SingleR",quietly=TRUE)||!requireNamespace("celldex",quietly=TRUE)) {
            log_sc("\u26a0\ufe0f SingleR/celldex non install\u00e9s.")
          } else {
            p$set(0.76,"Annotation SingleR...")
            result <- tryCatch(
              withCallingHandlers(.run_singler_safe(obj,input$sc_ap_singler_ref,input$sc_ap_singler_level),
                warning=function(w){ log_sc(paste("\u26a0\ufe0f",conditionMessage(w))); invokeRestart("muffleWarning") }),
              error=function(e){ log_sc(paste("\u26a0\ufe0f SingleR:",e$message)); NULL }
            )
            if (!is.null(result)) {
              col_name <- paste0("SingleR_",input$sc_ap_singler_ref,"_",input$sc_ap_singler_level)
              obj[[col_name]] <- result$labels
              log_sc(sprintf("\u2713 Annot\u00e9 [%s] \u2014 %d types",result$method,length(unique(result$labels))))
            }
          }
        }
        if (isTRUE(input$sc_ap_markers)) {
          p$set(0.82,"FindAllMarkers...")
          log_sc("FindAllMarkers...")
          markers <- tryCatch({
            Idents(obj) <- obj$seurat_clusters
            FindAllMarkers(obj,only.pos=TRUE,min.pct=0.1,logfc.threshold=0.25,verbose=FALSE)
          }, error=function(e){ log_sc(paste("\u26a0\ufe0f Markers:",e$message)); NULL })
          if (!is.null(markers) && nrow(markers)>0) {
            markers <- as.data.frame(markers); rownames(markers) <- NULL
            if (!"gene"       %in% colnames(markers)) markers$gene       <- rownames(markers)
            if (!"avg_log2FC" %in% colnames(markers)) markers$avg_log2FC <- markers$avg_logFC %||% 0
            if (!"p_val_adj"  %in% colnames(markers)) markers$p_val_adj  <- 1
            if (!"cluster"    %in% colnames(markers)) markers$cluster    <- "Unknown"
            if (!"pct.1"      %in% colnames(markers)) markers$pct.1      <- NA_real_
            if (!"pct.2"      %in% colnames(markers)) markers$pct.2      <- NA_real_
            shared_rv$markers_data <- markers
            log_sc(sprintf("\u2713 %d marqueurs",nrow(markers)))
            if (isTRUE(input$sc_ap_pathway)) {
              log_sc("Pathway ORA...")
              top_g <- head(markers$gene[order(markers$p_val_adj)],100)
              pw <- tryCatch(run_pathway_enrichment(top_g,organism=input$sc_ap_pathway_org,
                                                     database=input$sc_ap_pathway_db,pval_cutoff=0.05),
                             error=function(e){ log_sc(paste("\u26a0\ufe0f Pathway:",e$message)); NULL })
              if (!is.null(pw) && nrow(pw)>0) {
                shared_rv$pathway_results <- pw; shared_rv$pathway_db <- input$sc_ap_pathway_db
                log_sc(sprintf("\u2713 %d pathways",nrow(pw)))
              }
            }
          }
        }
        global_data$sc_obj <- obj; shared_rv$active_tab <- "tab_viz"
        showNotification(sprintf("\u2713 Pipeline SC : %d cellules, %d clusters",ncol(obj),n_cl),
                         type="message",duration=6)
      }, error=function(e){
        log_sc(paste("\u274c Erreur:",e$message))
        showNotification(paste("Erreur pipeline SC:",e$message),type="error",duration=10)
      })
    })

    # ── Report status ──────────────────────────────────────────────────────
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) "Importez et traitez un objet SC."
      else "Pr\u00eat \u2014 s\u00e9lectionnez les sections."
    })

    # ── HTML / PDF Report — Step-3.6: fixed .htm→.html, unique timestamps ─
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html="html", pdf="pdf", both="zip")
        # Unique per-second filename — prevents overwriting on same day
        paste0("rapport_singlecell_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(global_data$sc_obj)
        template_path <- file.path("modules","sc","sc_report_template.Rmd")
        if (!file.exists(template_path))
          stop("Template introuvable : modules/sc/sc_report_template.Rmd")
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite=TRUE)

        # NULL-guard corr params: empty df would crash plot_gene_correlation_network
        corr_genes <- if (!is.null(shared_rv$correlated_genes) &&
                          is.data.frame(shared_rv$correlated_genes) &&
                          nrow(shared_rv$correlated_genes) > 0)
                        shared_rv$correlated_genes else NULL
        corr_target <- if (!is.null(shared_rv$corr_target_gene) &&
                           nchar(shared_rv$corr_target_gene %||% "") > 0)
                         shared_rv$corr_target_gene else NULL

        render_params <- list(
          sc_obj          = global_data$sc_obj,
          markers_data    = shared_rv$markers_data,
          pathway_results = shared_rv$pathway_results,
          pathway_db      = shared_rv$pathway_db,
          correlated_genes= corr_genes,           # NULL if never run or empty
          corr_target_gene= corr_target,          # NULL if never run
          sections        = input$report_sections %||% character(0),
          reduction       = "umap",
          group_by        = "seurat_clusters",
          report_title    = input$report_title    %||% "Analyse Single-Cell",
          report_subtitle = input$report_subtitle %||% "",
          report_notes    = input$report_notes    %||% "",
          interactive     = isTRUE(input$report_interactive) && input$report_format != "pdf"
        )

        withProgress(message="G\u00e9n\u00e9ration du rapport...", value=0.2, {
          formats_needed <- switch(input$report_format,
            html="html_document", pdf="pdf_document", both=c("html_document","pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail=paste("Rendu", fmt))
            # FIX: explicit fileext prevents .htm (rmarkdown dot-parsing bug)
            ext_i    <- if (fmt=="html_document") "html" else "pdf"
            out_path <- tempfile(pattern=paste0("sc_report_",ext_i,"_"), fileext=paste0(".",ext_i))
            res <- tryCatch(
              rmarkdown::render(input=tmp_rmd, output_format=fmt, output_file=out_path,
                                params=render_params, envir=new.env(parent=globalenv()), quiet=TRUE),
              error=function(e){
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)),
                                 type="error", duration=12); NULL })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (!length(out_files)) stop("Aucun format g\u00e9n\u00e9r\u00e9.")
          else if (length(out_files)==1) file.copy(out_files[1], file, overwrite=TRUE)
          else zip::zip(file, files=out_files, mode="cherry-pick")
        })
      }
    )

    # ── SC Reproducible R Script ───────────────────────────────────────────
    output$dl_sc_r_script <- downloadHandler(
      filename = function() paste0("analyse_sc_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$sc_obj)
        obj     <- global_data$sc_obj
        tmp_dir <- tempfile("sc_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive=TRUE), add=TRUE)
        stamp       <- format(Sys.time(),"%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_sc_",stamp,".R"))
        rds_path    <- file.path(tmp_dir, "sc_obj.rds")
        writeLines(.sc_r_script_text(obj, shared_rv), script_path)
        saveRDS(obj, rds_path)
        zip::zip(file, files=c(script_path, rds_path), mode="cherry-pick")
        showNotification("\u2713 Script R g\u00e9n\u00e9r\u00e9.", type="message", duration=4)
      }
    )

  }) # /moduleServer
}

# =============================================================================
# .sc_r_script_text — reproducible SC analysis script (introspects Seurat obj)
# =============================================================================
.sc_r_script_text <- function(obj, shared_rv = NULL) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  meta         <- obj@meta.data
  n_cells      <- ncol(obj)
  n_genes      <- nrow(obj)
  date         <- format(Sys.Date(), "%Y-%m-%d")
  has_umap     <- "umap"            %in% names(obj@reductions)
  has_pca      <- "pca"             %in% names(obj@reductions)
  has_mt       <- "percent.mt"      %in% colnames(meta)
  has_clusters <- "seurat_clusters" %in% colnames(meta)
  singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
  has_singler  <- length(singler_cols) > 0
  has_markers  <- !is.null(shared_rv) && !is.null(shared_rv$markers_data) && nrow(shared_rv$markers_data) > 0
  pca_dims     <- if (has_pca) min(ncol(Embeddings(obj,"pca")), 50) else 20
  n_clusters   <- if (has_clusters) length(levels(factor(meta$seurat_clusters))) else "?"

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny (Single-Cell)
# G\u00e9n\u00e9r\u00e9 le : ', date, '
# Dataset    : ', n_genes, ' g\u00e8nes \u00d7 ', n_cells, ' cellules
# Pipeline d\u00e9tect\u00e9 : PCA=', if(has_pca)"oui" else "non",
  ', UMAP=', if(has_umap)"oui" else "non",
  ', clusters=', if(has_clusters) n_clusters else "non",
  ', SingleR=', if(has_singler) paste(singler_cols,collapse=",") else "non", '
# =============================================================================
# Usage : setwd("<dossier_du_zip>") puis source("', paste0("analyse_sc_",date,".R"), '")
# =============================================================================

library(Seurat); library(ggplot2); library(patchwork)

# \u2500\u2500 0. Charger l\'objet \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
obj <- readRDS("sc_obj.rds")
cat(sprintf("Objet : %d cellules, %d g\u00e8nes\\n", ncol(obj), nrow(obj)))

# \u2500\u2500 1. QC \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
',
if (!has_mt) '
mt_pat <- if (any(grepl("^MT-",rownames(obj)))) "^MT-" else if (any(grepl("^mt-",rownames(obj)))) "^mt-" else NULL
if (!is.null(mt_pat)) obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern=mt_pat)
' else '# percent.mt d\u00e9j\u00e0 calcul\u00e9.',
'
VlnPlot(obj, features=c("nFeature_RNA","nCount_RNA"',
if(has_mt) ',"percent.mt"' else '',
'), ncol=', if(has_mt) 3 else 2, ', pt.size=0)

# Ajustez ces seuils selon votre dataset :
MIN_GENES <- 100; MAX_GENES <- 8000; MAX_MT <- 20
# obj <- subset(obj, subset=nFeature_RNA>MIN_GENES & nFeature_RNA<MAX_GENES & percent.mt<MAX_MT)

# \u2500\u2500 2. Normalisation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
DefaultAssay(obj) <- "RNA"
obj <- NormalizeData(obj); obj <- FindVariableFeatures(obj, nfeatures=2000); obj <- ScaleData(obj)
# Alternative : obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")

# \u2500\u2500 3. PCA + Clustering + UMAP \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
PCA_DIMS  <- ', pca_dims, '
CLUST_RES <- 0.5

obj <- RunPCA(obj, npcs=PCA_DIMS, verbose=FALSE)
ElbowPlot(obj, ndims=PCA_DIMS)
obj <- FindNeighbors(obj, dims=1:PCA_DIMS); obj <- FindClusters(obj, resolution=CLUST_RES)
obj <- RunUMAP(obj, dims=1:PCA_DIMS, verbose=FALSE)
p_umap <- DimPlot(obj, reduction="umap", label=TRUE, pt.size=0.5)
print(p_umap)
ggsave(paste0("umap_clusters_',date,'.png"), p_umap, width=8, height=6, dpi=300)

# \u2500\u2500 4. Marqueurs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
',
if (has_markers) paste0('# ', nrow(shared_rv$markers_data), ' marqueurs export\u00e9s depuis l\'app (recalcul\u00e9s ci-dessous) :') else '',
'
Idents(obj) <- obj$seurat_clusters
markers <- FindAllMarkers(obj, only.pos=TRUE, min.pct=0.1, logfc.threshold=0.25, verbose=FALSE)
write.csv(markers, paste0("markers_', date, '.csv"), row.names=FALSE)
top5 <- markers |> dplyr::group_by(cluster) |> dplyr::slice_min(p_val_adj, n=5)
print(DotPlot(obj, features=unique(top5$gene)) + theme(axis.text.x=element_text(angle=45,hjust=1)))

# \u2500\u2500 5. Annotation SingleR (optionnel) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# BiocManager::install(c("SingleR","celldex"))
',
if (has_singler) paste0(
'# D\u00e9j\u00e0 annot\u00e9 (colonne : ', tail(singler_cols,1), ') :
print(DimPlot(obj, group.by="', tail(singler_cols,1), '", label=TRUE, repel=TRUE))'
) else
'# if (requireNamespace("SingleR",quietly=TRUE)) {
#   ref  <- celldex::HumanPrimaryCellAtlasData()
#   sce  <- as.SingleCellExperiment(obj)
#   pred <- SingleR::SingleR(test=sce, ref=ref, labels=ref$label.main)
#   obj[["SingleR"]] <- pred$labels
# }',
'

# \u2500\u2500 6. Sauvegarde \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
saveRDS(obj, paste0("sc_obj_processed_', date, '.rds"))
cat("Objet sauvegard\u00e9.\\n")
'
  )
}
