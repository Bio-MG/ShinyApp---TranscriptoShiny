# =============================================================================
# mod_bulk.R  —  Bulk RNA-seq Parent Router Module
# Step-2.5b : auto-pipeline | Step-3.0 : multi-méthodes option + ns() fix
# =============================================================================

mod_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(
        width = 420,
        title = "RNA Bulk — Analyse",
        div(class = "alert alert-info", style = "font-size:0.8rem;padding:5px;",
            bsicons::bs_icon("info-circle"),
            "Importez d'abord vos données dans l'onglet 'Import Données > RNA Bulk'."),
        uiOutput(ns("pipeline_status_bar")),
        actionButton(ns("btn_auto_pipeline"), "\u25b6 Lancer Pipeline Complet",
                     icon = icon("play-circle"), class = "btn-outline-success w-100 mb-1"),
        verbatimTextOutput(ns("auto_pipeline_log")),
        accordion(
          id = ns("acc_bulk"), open = "1. Filtrage & VST",
          accordion_panel("0. Mapping IDs (Optionnel)", icon = icon("arrows-rotate"),
                          mod_bulk_mapping_ui(ns("mapping"))),
          accordion_panel("1. Filtrage & VST", icon = icon("filter"),
                          mod_bulk_filter_ui(ns("filter"))),
          accordion_panel("2. Design & Contrastes", icon = icon("sliders"),
                          mod_bulk_de_ui(ns("de"))),
          accordion_panel("3. Pathway Enrichment", icon = icon("dna"),
                          mod_bulk_pathways_ui(ns("pathways"))),
          accordion_panel("4. Rapport Complet", icon = icon("file-export"),
                          mod_bulk_report_ui(ns("report")))
        )
      ),
      navset_card_underline(
        id = ns("main_tabs"), title = "R\u00e9sultats Bulk RNA-seq",
        nav_panel("PCA",             value = "tab_pca",         mod_bulk_filter_pca_ui(ns("filter"))),
        nav_panel("QC \u00c9chantillons", value = "tab_qc",     mod_bulk_filter_qc_ui(ns("filter"))),
        nav_panel("Volcano Plot",    value = "tab_volcano",     mod_bulk_de_volcano_ui(ns("de"))),
        nav_panel("MA-Plot",         value = "tab_ma",          mod_bulk_de_ma_ui(ns("de"))),
        nav_panel("Heatmap",         value = "tab_heatmap",     mod_bulk_de_heatmap_ui(ns("de"))),
        nav_panel("Table DE",        value = "tab_table",       mod_bulk_de_table_ui(ns("de"))),
        nav_panel("R\u00e9sum\u00e9 Up/Down",  value = "tab_updown",  mod_bulk_de_summary_ui(ns("de"))),
        nav_panel("Multi-m\u00e9thodes",  value = "tab_multimethod", mod_bulk_de_multimethod_ui(ns("de"))),
        nav_panel("Venn / UpSet",    value = "tab_venn",        mod_bulk_de_venn_ui(ns("de"))),
        nav_panel("Pathway",         value = "tab_pathway",     mod_bulk_pathways_output_ui(ns("pathways")))
      )
    )
  )
}

mod_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    shared_rv <- reactiveValues(
      counts_mapped = NULL, counts_original = NULL,
      mapping_applied = FALSE, mapping_summary = NULL,
      filtered_counts = NULL, dds_blind = NULL, vst_mat = NULL,
      dds_full = NULL, contrasts = list(), active_contrast = NULL,
      pathway_results = NULL, active_tab = NULL,
      pca_color_by = NULL, pca_shape_by = NULL,
      pca_manual_colors = NULL,    # Bug A
      volcano_role_colors = NULL,  # Bug A / Step-3.0
      active_condition_col = NULL, # Step-3.0 (used by R script export)
      multimethod_de = NULL,       # Step-3.0 (auto-pipeline multi-m\u00e9thodes)
      lfc_thresh = 1, padj_thresh = 0.05,
      heatmap_top_n = 30, heatmap_annot = NULL,
      pathway_db = "GOBP", bulk_palette = "default"
    )

    auto_log_rv <- reactiveVal("")
    output$auto_pipeline_log <- renderText({ auto_log_rv() })

    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(id = "main_tabs", selected = shared_rv$active_tab, session = session)
    })

    output$pipeline_status_bar <- renderUI({
      s0 <- if (isTRUE(shared_rv$mapping_applied))       "\u2705" else "\u26aa"
      s1 <- if (!is.null(shared_rv$filtered_counts))     "\u2705" else "\u26aa"
      s2 <- if (length(shared_rv$contrasts) > 0)         "\u2705"
             else if (is.null(shared_rv$filtered_counts)) "\U0001f512" else "\u26aa"
      s3 <- if (!is.null(shared_rv$pathway_results))     "\u2705"
             else if (length(shared_rv$contrasts) == 0)   "\U0001f512" else "\u26aa"
      s4 <- if (!is.null(shared_rv$vst_mat)) "\u26aa" else "\U0001f512"
      div(style="display:flex;justify-content:space-around;font-size:0.72em;background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;padding:4px 2px;margin-bottom:8px;",
          tags$span(style="padding:2px 4px;", s0, " Map"),
          tags$span(style="padding:2px 4px;", s1, " Filtre"),
          tags$span(style="padding:2px 4px;", s2, " DE"),
          tags$span(style="padding:2px 4px;", s3, " Pathway"),
          tags$span(style="padding:2px 4px;", s4, " Rapport"))
    })

    # ── Auto-pipeline modal (ns() fix: session$ns used for all modal input IDs)
    observeEvent(input$btn_auto_pipeline, {
      req(global_data$bulk_obj)
      ns_m   <- session$ns   # ensures modal input IDs are module-scoped
      meta   <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      if (!length(cat_cols)) cat_cols <- names(meta)

      showModal(modalDialog(
        title = "\u25b6 Pipeline Complet \u2014 Param\u00e8tres", size = "l", easyClose = TRUE,
        fluidRow(
          column(6,
            h6("Filtrage", style="font-weight:bold;"),
            numericInput(ns_m("ap_min_count"),   "Counts min / g\u00e8ne", 10, min=0),
            numericInput(ns_m("ap_min_samples"), "Nb \u00e9chantillons min", 1, min=1)
          ),
          column(6,
            h6("DE", style="font-weight:bold;"),
            selectInput(ns_m("ap_condition"), "Colonne condition", choices = cat_cols),
            selectInput(ns_m("ap_engine"), "Moteur",
                        c("DESeq2"="deseq2","edgeR"="edger","limma-voom"="limma")),
            numericInput(ns_m("ap_lfc"),  "|Log2FC| seuil", 1,    min=0, step=0.1),
            numericInput(ns_m("ap_padj"), "P-adj seuil",    0.05, min=0, max=1, step=0.01)
          )
        ),
        fluidRow(
          column(12,
            h6("Options suppl\u00e9mentaires", style="font-weight:bold;"),
            checkboxInput(ns_m("ap_multimethod"),
                          "\U0001f52c Multi-m\u00e9thodes (DESeq2 + edgeR + limma) apr\u00e8s contraste principal",
                          value = FALSE),
            checkboxInput(ns_m("ap_run_pathway"), "Pathway ORA apr\u00e8s DE", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns_m("ap_run_pathway")),
              fluidRow(
                column(6, selectInput(ns_m("ap_pathway_db"), "Base",
                           c("GO BP"="GOBP","KEGG"="KEGG","Reactome"="Reactome"))),
                column(6, selectInput(ns_m("ap_pathway_org"), "Organisme",
                           c("Humain"="human","Souris"="mouse")))
              )
            )
          )
        ),
        helpText("S\u00e9lectionne les 2 groupes les plus repr\u00e9sent\u00e9s comme contraste principal."),
        footer = tagList(
          modalButton("Annuler"),
          actionButton(ns_m("ap_confirm"), "\u25b6 Lancer", class = "btn-success")
        )
      ))
    })

    observeEvent(input$ap_confirm, {
      removeModal()
      req(global_data$bulk_obj, input$ap_condition)
      counts   <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
      meta     <- global_data$bulk_obj$metadata
      cond_col <- input$ap_condition
      tab      <- sort(table(as.character(meta[[cond_col]])), decreasing = TRUE)
      if (length(tab) < 2) { showNotification("\u274c Au moins 2 groupes requis.", type="error"); return() }
      grp_target <- names(tab)[1]; grp_ref <- names(tab)[2]

      p <- shiny::Progress$new(); on.exit(p$close())
      ll <- character(0)
      log <- function(msg) {
        ll <<- c(ll, paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
        auto_log_rv(paste(ll, collapse = "\n"))
      }

      tryCatch({
        # 1. Filter + VST
        p$set(0.05, "Filtrage..."); log("Filtrage & VST...")
        filtered  <- filter_bulk_counts(counts, min_count=input$ap_min_count,
                                         min_samples=input$ap_min_samples, min_count_per_sample=1)
        dds_b     <- build_dds(filtered, meta, "~1", run_deseq=FALSE)
        dds_b     <- DESeq2::estimateSizeFactors(dds_b)
        vst_m     <- get_vst_matrix(dds_b)
        shared_rv$filtered_counts <- filtered; shared_rv$dds_blind <- dds_b
        shared_rv$vst_mat         <- vst_m
        shared_rv$contrasts <- list(); shared_rv$active_contrast <- NULL
        log(sprintf("\u2713 %d g\u00e8nes \u00d7 %d \u00e9chantillons", nrow(filtered), ncol(filtered)))

        # 2. DE
        p$set(0.4, "DE..."); log(paste("DE:", grp_target, "vs", grp_ref, "/", input$ap_engine))
        design_str <- paste0("~ ", cond_col)
        res <- if (input$ap_engine == "deseq2") {
          dds_full <- build_dds(filtered, meta, design_str, run_deseq=TRUE)
          shared_rv$dds_full <- dds_full
          run_bulk_de_dispatch("deseq2", filtered, meta, cond_col, grp_target, grp_ref,
                               dds=dds_full, shrink=TRUE)
        } else {
          run_bulk_de_dispatch(input$ap_engine, filtered, meta, cond_col, grp_target, grp_ref)
        }
        res   <- .normalize_de_cols(res, counts_for_basemean=filtered)
        cname <- paste0(grp_target, "_vs_", grp_ref)
        cur_c <- shared_rv$contrasts; cur_c[[cname]] <- res; shared_rv$contrasts <- cur_c
        shared_rv$active_contrast <- cname
        shared_rv$lfc_thresh <- input$ap_lfc; shared_rv$padj_thresh <- input$ap_padj
        shared_rv$active_condition_col <- cond_col
        log(sprintf("\u2713 %d sig. (%s vs %s)",
            sum(res$padj < input$ap_padj & abs(res$log2FoldChange) > input$ap_lfc, na.rm=TRUE),
            grp_target, grp_ref))

        # 2b. Multi-méthodes (optional)
        if (isTRUE(input$ap_multimethod) && !is.null(shared_rv$dds_full)) {
          p$set(0.6, "Multi-m\u00e9thodes...")
          log("Multi-m\u00e9thodes (DESeq2 + edgeR + limma)...")
          dl <- tryCatch(
            getAllDE(filtered, meta, cond_col, grp_target, grp_ref,
                     dds_full=shared_rv$dds_full, shrink=TRUE),
            error=function(e) { log(paste("\u26a0\ufe0f Multi-m\u00e9thodes:", e$message)); NULL }
          )
          if (!is.null(dl) && length(dl) >= 2) {
            shared_rv$multimethod_de <- dl
            log(sprintf("\u2713 %d m\u00e9thodes (%s)", length(dl), paste(names(dl), collapse=", ")))
          }
        }

        # 3. Pathway (optional)
        if (isTRUE(input$ap_run_pathway)) {
          p$set(0.85, "Pathway ORA..."); log("Pathway ORA...")
          sig_g <- res$gene[!is.na(res$padj) & res$padj < input$ap_padj &
                              abs(res$log2FoldChange) > input$ap_lfc]
          if (length(sig_g) >= 10) {
            pw <- tryCatch(
              run_pathway_enrichment(sig_g, organism=input$ap_pathway_org,
                                     database=input$ap_pathway_db, pval_cutoff=0.05),
              error=function(e) { log(paste("\u26a0\ufe0f Pathway:", e$message)); NULL }
            )
            if (!is.null(pw) && nrow(pw) > 0) {
              shared_rv$pathway_results <- pw; shared_rv$pathway_db <- input$ap_pathway_db
              log(sprintf("\u2713 %d pathways enrichis", nrow(pw)))
            }
          } else log(paste("\u26a0\ufe0f Pathway ignor\u00e9:", length(sig_g), "g\u00e8nes"))
        }

        shared_rv$active_tab <- "tab_pca"
        showNotification("\u2713 Pipeline termin\u00e9 \u2014 PCA disponible.", type="message", duration=6)

      }, error=function(e) {
        log(paste("\u274c Erreur:", e$message))
        showNotification(paste("Erreur pipeline:", e$message), type="error", duration=10)
      })
    })

    # ── Child servers ──────────────────────────────────────────────────────
    mod_bulk_mapping_server( "mapping",  global_data, shared_rv)
    mod_bulk_filter_server(  "filter",   global_data, shared_rv)
    mod_bulk_de_server(      "de",       global_data, shared_rv)
    mod_bulk_pathways_server("pathways", global_data, shared_rv)
    mod_bulk_report_server(  "report",   global_data, shared_rv)

  }) # /moduleServer
}
