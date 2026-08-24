# mod_bulk.R — Bulk RNA-seq Parent Router (i18n Phase 3.1)
# i18n: static labels via i18n$t(); accordion panels get explicit value= so
# `open=` still matches once titles become JS-shim tags. Dynamic strings via
# global_data$i18n + global_data$language trigger.

mod_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(
        width = 420,
        title = i18n$t("RNA Bulk \u2014 Analyse"),
        div(class = "alert alert-info", style = "font-size:0.8rem;padding:5px;",
            bsicons::bs_icon("info-circle"),
            " ", i18n$t("Importez d'abord vos donn\u00e9es dans l'onglet 'Import Donn\u00e9es > RNA Bulk'.")),
        uiOutput(ns("pipeline_status_bar")),
        actionButton(ns("btn_auto_pipeline"),
                     tagList(icon("play-circle"), i18n$t("Lancer Pipeline Complet")),
                     class = "btn-outline-success w-100 mb-1"),
        verbatimTextOutput(ns("auto_pipeline_log")),
        accordion(
          id = ns("acc_bulk"), open = "panel_filter",
          accordion_panel(i18n$t("0. Mapping IDs (Optionnel)"), value = "panel_mapping",
                          icon = icon("arrows-rotate"), mod_bulk_mapping_ui(ns("mapping"))),
          accordion_panel(i18n$t("1. Filtrage & VST"), value = "panel_filter",
                          icon = icon("filter"), mod_bulk_filter_ui(ns("filter"))),
          accordion_panel(i18n$t("2. Design & Contrastes"), value = "panel_de",
                          icon = icon("sliders"), mod_bulk_de_ui(ns("de"))),
          accordion_panel(i18n$t("3. Pathway Enrichment"), value = "panel_pathways",
                          icon = icon("dna"), mod_bulk_pathways_ui(ns("pathways"))),
          accordion_panel(i18n$t("4. Rapport Complet"), value = "panel_report",
                          icon = icon("file-export"), mod_bulk_report_ui(ns("report")))
        )
      ),
      navset_card_underline(
        id = ns("main_tabs"), title = "R\u00e9sultats Bulk RNA-seq",
        nav_panel("PCA",             value = "tab_pca",         mod_bulk_filter_pca_ui(ns("filter"))),
        nav_panel(i18n$t("QC \u00c9chantillons"), value = "tab_qc",  mod_bulk_filter_qc_ui(ns("filter"))),
        nav_panel("Volcano Plot",    value = "tab_volcano",     mod_bulk_de_volcano_ui(ns("de"))),
        nav_panel("MA-Plot",         value = "tab_ma",          mod_bulk_de_ma_ui(ns("de"))),
        nav_panel("Heatmap",         value = "tab_heatmap",     mod_bulk_de_heatmap_ui(ns("de"))),
        nav_panel(i18n$t("Table DE"), value = "tab_table",      mod_bulk_de_table_ui(ns("de"))),
        nav_panel(i18n$t("R\u00e9sum\u00e9 Up/Down"), value = "tab_updown", mod_bulk_de_summary_ui(ns("de"))),
        nav_panel(i18n$t("Multi-m\u00e9thodes"), value = "tab_multimethod", mod_bulk_de_multimethod_ui(ns("de"))),
        nav_panel("Venn / UpSet",    value = "tab_venn",        mod_bulk_de_venn_ui(ns("de"))),
        nav_panel("Pathway",         value = "tab_pathway",     mod_bulk_pathways_output_ui(ns("pathways")))
      )
    )
  )
}

mod_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    # Session-scoped translation proxy (Phase-2 pattern).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    shared_rv <- reactiveValues(
      counts_mapped = NULL, counts_original = NULL,
      mapping_applied = FALSE, mapping_summary = NULL,
      filtered_counts = NULL, dds_blind = NULL, vst_mat = NULL,
      dds_full = NULL, contrasts = list(), active_contrast = NULL,
      pathway_results = NULL, active_tab = NULL,
      pca_color_by = NULL, pca_shape_by = NULL,
      pca_manual_colors = NULL,
      volcano_role_colors = NULL,
      active_condition_col = NULL,
      multimethod_de = NULL,
      lfc_thresh = 1, padj_thresh = 0.05,
      heatmap_top_n = 30, heatmap_annot = NULL,
      pathway_db = "GOBP", pathway_mode = "ora", bulk_palette = "default"
    )

    auto_log_rv <- reactiveVal("")
    output$auto_pipeline_log <- renderText({ auto_log_rv() })

    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(id = "main_tabs", selected = shared_rv$active_tab, session = session)
    })

    # Pipeline status bar — language-aware.
    output$pipeline_status_bar <- renderUI({
      global_data$language
      s0 <- if (isTRUE(shared_rv$mapping_applied))   "\u2705" else "\u26aa"
      s1 <- if (!is.null(shared_rv$filtered_counts)) "\u2705" else "\u26aa"
      s2 <- if (length(shared_rv$contrasts) > 0)     "\u2705"
            else if (is.null(shared_rv$filtered_counts)) "\U0001f512" else "\u26aa"
      s3 <- if (!is.null(shared_rv$pathway_results)) "\u2705"
            else if (length(shared_rv$contrasts) == 0) "\U0001f512" else "\u26aa"
      s4 <- if (!is.null(shared_rv$vst_mat)) "\u26aa" else "\U0001f512"
      div(style="display:flex;justify-content:space-around;font-size:0.72em;background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;padding:4px 2px;margin-bottom:8px;",
          tags$span(style="padding:2px 4px;", s0, " ", .tr("Map")),
          tags$span(style="padding:2px 4px;", s1, " ", .tr("Filtre")),
          tags$span(style="padding:2px 4px;", s2, " ", .tr("DE")),
          tags$span(style="padding:2px 4px;", s3, " ", .tr("Pathway")),
          tags$span(style="padding:2px 4px;", s4, " ", .tr("Rapport")))
    })

    # ── Auto-pipeline modal ──────────────────────────────────────────────
    observeEvent(input$btn_auto_pipeline, {
      req(global_data$bulk_obj)
      ns_m   <- session$ns
      meta   <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      if (!length(cat_cols)) cat_cols <- names(meta)

      showModal(modalDialog(
        title = tagList("\u25b6", .tr("Pipeline Complet \u2014 Param\u00e8tres")), size = "l", easyClose = TRUE,
        fluidRow(
          column(6,
            h6(.tr("0. Mapping IDs (auto)"), style="font-weight:bold;"),
            checkboxInput(ns_m("ap_map_ids"),
                          .tr("D\u00e9tecter + convertir Ensembl/Entrez/Affy \u2192 Symbol"), value = TRUE),
            selectInput(ns_m("ap_map_organism"), .tr("Organisme (mapping)"),
                        stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris")))),
            helpText(style="font-size:0.78em;",
                     .tr("Ignor\u00e9 automatiquement si vos identifiants sont d\u00e9j\u00e0 des symboles."))
          ),
          column(6,
            h6(.tr("1. Filtrage"), style="font-weight:bold;"),
            numericInput(ns_m("ap_min_count"),   .tr("Counts min / g\u00e8ne"), 10, min=0),
            numericInput(ns_m("ap_min_samples"), .tr("Nb \u00e9chantillons min"), 1, min=1)
          )
        ),
        fluidRow(
          column(12,
            h6(.tr("2. DE"), style="font-weight:bold;"),
            selectInput(ns_m("ap_condition"), .tr("Colonne condition"), choices = cat_cols),
            checkboxInput(ns_m("ap_pairwise"),
                          .tr("Calculer TOUTES les paires (pairwise) si > 2 groupes"), value = FALSE),
            helpText(style="font-size:0.78em;",
                     .tr("Sinon, seuls les 2 groupes les plus repr\u00e9sent\u00e9s sont compar\u00e9s.")),
            selectInput(ns_m("ap_engine"), .tr("Moteur"),
                        c("DESeq2"="deseq2","edgeR"="edger","limma-voom"="limma")),
            numericInput(ns_m("ap_lfc"),  .tr("|Log2FC| seuil"), 1,    min=0, step=0.1),
            numericInput(ns_m("ap_padj"), .tr("P-adj seuil"),    0.05, min=0, max=1, step=0.01)
          )
        ),
        fluidRow(
          column(12,
            h6(.tr("Options suppl\u00e9mentaires"), style="font-weight:bold;"),
            checkboxInput(ns_m("ap_multimethod"),
                          .tr("Multi-m\u00e9thodes (DESeq2 + edgeR + limma) apr\u00e8s contraste principal (ignor\u00e9 si pairwise)"),
                          value = FALSE),
            checkboxInput(ns_m("ap_run_pathway"), .tr("Pathway Enrichment apr\u00e8s DE"), value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns_m("ap_run_pathway")),
              radioButtons(ns_m("ap_pathway_mode"), .tr("M\u00e9thode"),
                           stats::setNames(c("ora","gsea"),
                             c(.tr("ORA \u2014 g\u00e8nes significatifs (seuil)"),
                               .tr("GSEA \u2014 tous les g\u00e8nes class\u00e9s (sans seuil)"))),
                           selected = "ora"),
              fluidRow(
                column(6, selectInput(ns_m("ap_pathway_db"), .tr("Base"),
                           c("GO BP"="GOBP","KEGG"="KEGG","Reactome"="Reactome"))),
                column(6, selectInput(ns_m("ap_pathway_org"), .tr("Organisme"),
                           stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris")))))
              )
            )
          )
        ),
        helpText(.tr("Par d\u00e9faut, seuls les 2 groupes les plus repr\u00e9sent\u00e9s sont compar\u00e9s (case pairwise ci-dessus d\u00e9coch\u00e9e). Le Venn/UpSet multi-contrastes reste disponible manuellement (onglet d\u00e9di\u00e9) d\u00e8s que \u2265 2 contrastes existent.")),
        footer = tagList(
          modalButton(.tr("Annuler")),
          actionButton(ns_m("ap_confirm"), tagList("\u25b6", .tr("Lancer")), class = "btn-success")
        )
      ))
    })

    # Auto-pipeline execution — logic unchanged; all user-facing strings via .tr()/.t_fmt().
    observeEvent(input$ap_confirm, {
      removeModal()
      req(global_data$bulk_obj, input$ap_condition)
      counts   <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
      meta     <- global_data$bulk_obj$metadata
      cond_col <- input$ap_condition
      tab      <- sort(table(as.character(meta[[cond_col]])), decreasing = TRUE)
      if (length(tab) < 2) { showNotification(.tr("Au moins 2 groupes requis."), type="error"); return() }
      grp_target <- names(tab)[1]; grp_ref <- names(tab)[2]

      p <- shiny::Progress$new(); on.exit(p$close())
      ll <- character(0)
      log <- function(msg) {
        ll <<- c(ll, paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
        auto_log_rv(paste(ll, collapse = "\n"))
      }

      tryCatch({
        if (isTRUE(input$ap_map_ids) && is.null(shared_rv$counts_mapped)) {
          detected <- tryCatch(detect_gene_id_type(rownames(counts)), error = function(e) "unknown")
          if (detected %in% c("ensembl", "entrez", "affy_probe")) {
            p$set(0.02, .tr("Mapping IDs..."))
            log(.t_fmt(.tr("Mapping IDs auto d\u00e9tect\u00e9 ({type})..."), type = detected))
            ids <- rownames(counts)
            if (detected == "ensembl") ids <- gsub("\\.[0-9]+$", "", ids)
            counts_work <- counts
            rownames(counts_work) <- ids
            map_res <- tryCatch(
              remap_gene_ids_to_symbol(counts_work, from_type = detected,
                                       organism = input$ap_map_organism %||% "human",
                                       collapse_method = "sum"),
              error = function(e) { log(paste("\u26a0\ufe0f", .tr("Mapping ignor\u00e9 :"), e$message)); NULL }
            )
            if (!is.null(map_res)) {
              shared_rv$counts_mapped   <- map_res$matrix
              shared_rv$mapping_applied <- TRUE
              shared_rv$mapping_summary <- sprintf(
                "\u2713 %d mapped, %d unmapped \u2192 %d final genes (auto pipeline).",
                map_res$n_mapped, map_res$n_unmapped, nrow(map_res$matrix))
              counts <- map_res$matrix
              log(.t_fmt(.tr("\u2713 Mapping : {n} g\u00e8nes finaux"), n = nrow(counts)))
            }
          } else {
            log(.tr("Mapping IDs : symboles d\u00e9j\u00e0 d\u00e9tect\u00e9s (ou type inconnu) \u2014 \u00e9tape ignor\u00e9e."))
          }
        }

        p$set(0.05, .tr("Filtrage...")); log(.tr("Filtrage & VST..."))
        filtered  <- filter_bulk_counts(counts, min_count=input$ap_min_count,
                                         min_samples=input$ap_min_samples, min_count_per_sample=1)
        dds_b     <- build_dds(filtered, meta, "~1", run_deseq=FALSE)
        dds_b     <- DESeq2::estimateSizeFactors(dds_b)
        vst_m     <- get_vst_matrix(dds_b)
        shared_rv$filtered_counts <- filtered; shared_rv$dds_blind <- dds_b
        shared_rv$vst_mat         <- vst_m
        shared_rv$contrasts <- list(); shared_rv$active_contrast <- NULL
        log(.t_fmt(.tr("\u2713 {n} g\u00e8nes \u00d7 {m} \u00e9chantillons"), n = nrow(filtered), m = ncol(filtered)))

        lvls <- names(tab)
        pairwise_mode <- isTRUE(input$ap_pairwise) && length(lvls) > 2
        design_str <- paste0("~ ", cond_col)

        if (pairwise_mode) {
          p$set(0.4, .tr("DE pairwise..."))
          log(.t_fmt(.tr("Pairwise : {g} groupes \u2192 {p} paires..."),
                     g = length(lvls), p = choose(length(lvls), 2)))
          dds_full <- NULL
          if (input$ap_engine == "deseq2") {
            dds_full <- build_dds(filtered, meta, design_str, run_deseq = TRUE)
            shared_rv$dds_full <- dds_full
          }
          pairs <- utils::combn(lvls, 2, simplify = FALSE)
          ok <- 0; failed <- character(0)
          for (i in seq_along(pairs)) {
            ref_i <- pairs[[i]][1]; target_i <- pairs[[i]][2]
            name_i <- sprintf("%s_vs_%s", target_i, ref_i)
            p$set(0.4 + 0.2 * i / length(pairs),
                  .t_fmt(.tr("Pairwise {i}/{n}..."), i = i, n = length(pairs)))
            res_i <- tryCatch({
              r <- if (input$ap_engine == "deseq2") {
                run_bulk_de_dispatch("deseq2", filtered, meta, cond_col, target_i, ref_i,
                                     dds = dds_full, shrink = TRUE)
              } else {
                run_bulk_de_dispatch(input$ap_engine, filtered, meta, cond_col, target_i, ref_i)
              }
              .normalize_de_cols(r, counts_for_basemean = filtered)
            }, error = function(e) { failed <<- c(failed, name_i); NULL })
            if (!is.null(res_i)) {
              cur_c <- shared_rv$contrasts; cur_c[[name_i]] <- res_i; shared_rv$contrasts <- cur_c
              ok <- ok + 1
            }
          }
          if (ok == 0) stop(.tr("Aucune paire n'a pu \u00eatre calcul\u00e9e (voir \u00e9checs)."))
          shared_rv$active_contrast <- names(shared_rv$contrasts)[1]
          shared_rv$lfc_thresh  <- input$ap_lfc; shared_rv$padj_thresh <- input$ap_padj
          shared_rv$active_condition_col <- cond_col
          log(.t_fmt(.tr("\u2713 {ok}/{n} paires calcul\u00e9es \u2014 actif: {c}"),
                     ok = ok, n = length(pairs), c = shared_rv$active_contrast))
        } else {
          p$set(0.4, .tr("DE..."))
          log(paste(.tr("DE:"), grp_target, "vs", grp_ref, "/", input$ap_engine))
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
          log(.t_fmt(.tr("\u2713 {n} sig. ({a} vs {b})"),
                     n = sum(res$padj < input$ap_padj & abs(res$log2FoldChange) > input$ap_lfc, na.rm=TRUE),
                     a = grp_target, b = grp_ref))
        }

        if (isTRUE(input$ap_multimethod) && pairwise_mode) {
          log(.tr("Multi-m\u00e9thodes ignor\u00e9 en mode pairwise (n\u00e9cessite un contraste unique \u2014 utilisez l'onglet d\u00e9di\u00e9 manuellement)."))
        } else if (isTRUE(input$ap_multimethod) && !is.null(shared_rv$dds_full)) {
          p$set(0.6, .tr("Multi-m\u00e9thodes..."))
          log(.tr("Multi-m\u00e9thodes (DESeq2 + edgeR + limma)..."))
          dl <- tryCatch(
            getAllDE(filtered, meta, cond_col, grp_target, grp_ref,
                     dds_full=shared_rv$dds_full, shrink=TRUE),
            error=function(e) { log(paste("\u26a0\ufe0f", .tr("Multi-m\u00e9thodes:"), e$message)); NULL }
          )
          if (!is.null(dl) && length(dl) >= 2) {
            shared_rv$multimethod_de <- dl
            log(.t_fmt(.tr("\u2713 {n} m\u00e9thodes ({m})"), n = length(dl), m = paste(names(dl), collapse=", ")))
          }
        }

        if (isTRUE(input$ap_run_pathway) && identical(input$ap_pathway_mode, "gsea")) {
          p$set(0.85, .tr("GSEA...")); log(.tr("GSEA (tous les g\u00e8nes class\u00e9s)..."))
          pw <- tryCatch(
            run_gsea_enrichment(res, organism=input$ap_pathway_org,
                                database=input$ap_pathway_db, pval_cutoff=0.05),
            error=function(e) { log(paste("\u26a0\ufe0f GSEA:", e$message)); NULL }
          )
          if (!is.null(pw) && nrow(pw) > 0) {
            shared_rv$pathway_results <- pw; shared_rv$pathway_db <- input$ap_pathway_db
            shared_rv$pathway_mode    <- "gsea"
            log(.t_fmt(.tr("\u2713 {n} pathways enrichis (GSEA)"), n = nrow(pw)))
          } else log(.tr("\u26a0\ufe0f GSEA : aucun pathway enrichi."))
        } else if (isTRUE(input$ap_run_pathway)) {
          p$set(0.85, .tr("Pathway ORA...")); log(.tr("Pathway ORA..."))
          sig_g <- res$gene[!is.na(res$padj) & res$padj < input$ap_padj &
                              abs(res$log2FoldChange) > input$ap_lfc]
          if (length(sig_g) >= 10) {
            pw <- tryCatch(
              run_pathway_enrichment(sig_g, organism=input$ap_pathway_org,
                                     database=input$ap_pathway_db, pval_cutoff=0.05,
                                     universe = rownames(filtered)),
              error=function(e) { log(paste("\u26a0\ufe0f", .tr("Pathway:"), e$message)); NULL }
            )
            if (!is.null(pw) && nrow(pw) > 0) {
              shared_rv$pathway_results <- pw; shared_rv$pathway_db <- input$ap_pathway_db
              shared_rv$pathway_mode    <- "ora"
              log(.t_fmt(.tr("\u2713 {n} pathways enrichis"), n = nrow(pw)))
            }
          } else log(paste("\u26a0\ufe0f", .tr("Pathway ignor\u00e9:"), length(sig_g), .tr("g\u00e8nes")))
        }

        shared_rv$active_tab <- if (pairwise_mode) "tab_updown" else "tab_pca"
        showNotification(
          if (pairwise_mode)
            .t_fmt(.tr("\u2713 Pipeline termin\u00e9 \u2014 {n} contraste(s) pairwise calcul\u00e9(s)."),
                   n = length(shared_rv$contrasts))
          else .tr("\u2713 Pipeline termin\u00e9 \u2014 PCA disponible."),
          type="message", duration=6)

      }, error=function(e) {
        log(paste("\u274c", .tr("Erreur:"), e$message))
        showNotification(paste(.tr("Erreur pipeline:"), e$message), type="error", duration=10)
      })
    })

    # ── Child servers ────────────────────────────────────────────────────
    mod_bulk_mapping_server( "mapping",  global_data, shared_rv)
    mod_bulk_filter_server(  "filter",   global_data, shared_rv)
    mod_bulk_de_server(      "de",       global_data, shared_rv)
    mod_bulk_pathways_server("pathways", global_data, shared_rv)
    mod_bulk_report_server(  "report",   global_data, shared_rv)

  }) # /moduleServer
}
