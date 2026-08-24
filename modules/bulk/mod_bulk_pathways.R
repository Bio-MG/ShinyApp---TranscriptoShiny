# mod_bulk_pathways.R — Bulk Child 3 (i18n Phase 3.1)

mod_bulk_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("enrich_mode"), i18n$t("M\u00e9thode d'enrichissement"),
                choices = stats::setNames(c("ora","gsea"),
                  c(i18n$t("ORA \u2014 sur g\u00e8nes significatifs (classique)"),
                    i18n$t("GSEA \u2014 sur tous les g\u00e8nes class\u00e9s (sans seuil)"))),
                selected = "ora"),
    div(class = "small text-muted mb-2",
        i18n$t("GSEA n'a pas besoin de seuil de significativit\u00e9 arbitraire \u2014 elle classe tous les g\u00e8nes par Log2FC et teste l'enrichissement cumul\u00e9. Plus robuste statistiquement, recommand\u00e9e si peu de g\u00e8nes passent vos seuils.")),

    conditionalPanel(
      condition = "input.enrich_mode == 'ora'", ns = ns,
      selectInput(ns("pathway_source"), i18n$t("Source de g\u00e8nes"),
                  choices = stats::setNames(c("up","down","all_sig","manual"),
                    c(i18n$t("G\u00e8nes Up (significatifs)"), i18n$t("G\u00e8nes Down (significatifs)"),
                      i18n$t("Tous g\u00e8nes significatifs"), i18n$t("S\u00e9lection manuelle")))),
      conditionalPanel(
        condition = "input.pathway_source == 'manual'", ns = ns,
        selectizeInput(ns("pathway_genes"), i18n$t("G\u00e8nes"), choices = NULL, multiple = TRUE)
      )
    ),

    fluidRow(
      column(6, selectInput(ns("pathway_db"), i18n$t("Base de donn\u00e9es"),
                            choices = c("GO Biological Process" = "GOBP",
                                        "KEGG Pathways"         = "KEGG",
                                        "Reactome"              = "Reactome"))),
      column(6, selectInput(ns("pathway_org"), i18n$t("Organisme"),
                            choices = stats::setNames(c("human","mouse"),
                              c(i18n$t("Humain"), i18n$t("Souris")))))
    ),
    numericInput(ns("pathway_pval"), i18n$t("P-value cutoff"), value = 0.05, min = 0.001, max = 0.1, step = 0.01),

    actionButton(ns("run_pathway"), i18n$t("Lancer Enrichissement"),
                 class = "btn-warning w-100", icon = icon("dna")),

    downloadButton(ns("dl_pathway"), i18n$t("Export CSV"), class = "btn-sm btn-info w-100 mt-2"),

    div(class = "small text-muted mt-1", textOutput(ns("pathway_status")))
  )
}

mod_bulk_pathways_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header("Pathway Enrichment"),
    navset_tab(
      nav_panel(i18n$t("Barplot Top 15"), plotOutput(ns("pathway_barplot"), height = "580px")),
      nav_panel(i18n$t("Dotplot"),        plotOutput(ns("pathway_dotplot"), height = "580px")),
      nav_panel(i18n$t("Table"),          DTOutput(ns("pathway_table"))),
      nav_panel(i18n$t("Courbe GSEA"),    uiOutput(ns("gsea_curve_ui")))
    )
  )
}

mod_bulk_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateRadioButtons(session, "enrich_mode", label = .tr("M\u00e9thode d'enrichissement"),
        choices = stats::setNames(c("ora","gsea"),
          c(.tr("ORA \u2014 sur g\u00e8nes significatifs (classique)"), .tr("GSEA \u2014 sur tous les g\u00e8nes class\u00e9s (sans seuil)"))))
      updateSelectInput(session, "pathway_source", label = .tr("Source de g\u00e8nes"),
        choices = stats::setNames(c("up","down","all_sig","manual"),
          c(.tr("G\u00e8nes Up (significatifs)"), .tr("G\u00e8nes Down (significatifs)"),
            .tr("Tous g\u00e8nes significatifs"), .tr("S\u00e9lection manuelle"))))
      updateSelectizeInput(session, "pathway_genes", label = .tr("G\u00e8nes"))
      updateSelectInput(session, "pathway_db",  label = .tr("Base de donn\u00e9es"))
      updateSelectInput(session, "pathway_org", label = .tr("Organisme"),
        choices = stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris"))))
      updateNumericInput(session, "pathway_pval", label = .tr("P-value cutoff"))
      updateActionButton(session, "run_pathway", label = .tr("Lancer Enrichissement"))
    }, ignoreInit = TRUE)

    # (unchanged) manual gene picker refresh + mirroring
    observeEvent(shared_rv$filtered_counts, {
      req(shared_rv$filtered_counts)
      updateSelectizeInput(session, "pathway_genes",
                           choices = rownames(shared_rv$filtered_counts), server = TRUE)
    })
    observe({
      shared_rv$pathway_db   <- input$pathway_db
      shared_rv$pathway_mode <- input$enrich_mode
    })

    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    observe({
      shinyjs::toggleState("run_pathway", condition = !is.null(shared_rv$filtered_counts))
      shinyjs::toggleState("dl_pathway",  condition = !is.null(shared_rv$pathway_results))
    })

    # ── Enrichment (ORA + GSEA) — strings translated ─────────────────────
    observeEvent(input$run_pathway, {
      req(shared_rv$filtered_counts)
      p <- shiny::Progress$new(); on.exit(p$close())

      if (input$enrich_mode == "gsea") {
        res_de <- .active_de_results()
        if (is.null(res_de)) { showNotification(.tr("\u26a0\ufe0f Lancez d'abord l'\u00e9tape 2 (Analyse Diff\u00e9rentielle)."), type = "warning"); return() }
        p$set(message = .tr("GSEA en cours..."), value = 0.3)
        tryCatch({
          res <- run_gsea_enrichment(res_de, organism = input$pathway_org,
                                     database = input$pathway_db, pval_cutoff = input$pathway_pval)
          if (nrow(res) == 0) {
            showNotification(.tr("\u2139\ufe0f Aucun pathway enrichi trouv\u00e9 (GSEA)."), type = "warning")
            shared_rv$pathway_results <- NULL; return()
          }
          shared_rv$pathway_results <- res
          showNotification(.t_fmt(.tr("\u2705 {n} pathways enrichis (GSEA)"), n = nrow(res)), type = "message")
          shared_rv$active_tab <- "tab_pathway"
        }, error = function(e) {
          showNotification(.t_fmt(.tr("\u274c Erreur GSEA: {msg}"), msg = as.character(e$message)[1]),
                           type = "error", duration = 8)
          shared_rv$pathway_results <- NULL
        })
        return()
      }

      genes_to_test <- NULL
      if (input$pathway_source %in% c("up","down","all_sig")) {
        res <- .active_de_results()
        if (is.null(res)) { showNotification(.tr("\u26a0\ufe0f Lancez d'abord l'\u00e9tape 2 (Analyse Diff\u00e9rentielle)."), type = "warning"); return() }
        sig <- res$padj < (shared_rv$padj_thresh %||% 0.05) &
               abs(res$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
        sig[is.na(sig)] <- FALSE
        genes_to_test <- switch(input$pathway_source,
          up = res$gene[sig & res$log2FoldChange > 0],
          down = res$gene[sig & res$log2FoldChange < 0],
          all_sig = res$gene[sig])
      } else {
        req(input$pathway_genes); genes_to_test <- input$pathway_genes
      }
      genes_to_test <- unique(trimws(genes_to_test)); genes_to_test <- genes_to_test[nchar(genes_to_test) > 0]

      if (length(genes_to_test) < 10) {
        showNotification(.t_fmt(.tr("\u26a0\ufe0f Trop peu de g\u00e8nes ({n}). Minimum 10 requis."), n = length(genes_to_test)),
                         type = "warning", duration = 5)
        return()
      }

      p$set(message = .tr("Enrichissement..."), value = 0.3)
      tryCatch({
        res <- run_pathway_enrichment(genes = genes_to_test, organism = input$pathway_org,
                                      database = input$pathway_db, pval_cutoff = input$pathway_pval)
        if (nrow(res) == 0) {
          showNotification(.tr("\u2139\ufe0f Aucun pathway enrichi trouv\u00e9."), type = "warning")
          shared_rv$pathway_results <- NULL; return()
        }
        shared_rv$pathway_results <- res
        showNotification(.t_fmt(.tr("\u2705 {n} pathways enrichis"), n = nrow(res)), type = "message")
        shared_rv$active_tab <- "tab_pathway"
      }, error = function(e) {
        showNotification(.t_fmt(.tr("\u274c Erreur pathway: {msg}"), msg = as.character(e$message)[1]),
                         type = "error", duration = 6)
        shared_rv$pathway_results <- NULL
      })
    })

    output$pathway_status <- renderText({
      global_data$language
      if (is.null(shared_rv$pathway_results)) .tr("Aucune analyse en cours")
      else .t_fmt(.tr("\u2713 {n} pathways trouv\u00e9s [ {db} ]"),
                  n = nrow(shared_rv$pathway_results), db = input$pathway_db)
    })

    output$pathway_barplot <- renderPlot({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      plot_pathway_barplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 15,
                           tr = .tr_fn(global_data))
    })
    output$pathway_dotplot <- renderPlot({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      plot_pathway_dotplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 20,
                           tr = .tr_fn(global_data))
    })
    output$pathway_table <- renderDT({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      build_pathway_dt(shared_rv$pathway_results, tr = .tr_fn(global_data))
    })
    output$dl_pathway <- downloadHandler(
      filename = function() paste0("pathways_bulk_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(shared_rv$pathway_results); write.csv(shared_rv$pathway_results, file, row.names = FALSE) }
    )

    # ── GSEA curve (strings translated) ──────────────────────────────────
    output$gsea_curve_ui <- renderUI({
      global_data$language
      gsea_obj <- attr(shared_rv$pathway_results, "gsea_obj")
      if (is.null(gsea_obj)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ", .tr("Disponible uniquement pour les r\u00e9sultats GSEA \u2014 relancez l'enrichissement en mode GSEA (panneau de gauche).")))
      }
      df <- shared_rv$pathway_results
      choices <- setNames(df$ID, sprintf("%s (NES=%.2f, p.adj=%.1e)", df$Description, df$NES, df$p.adjust))
      tagList(
        fluidRow(
          column(8, selectizeInput(ns("gsea_curve_pathway"), .tr("Pathway (tapez pour rechercher)"),
                                   choices = choices, width = "100%",
                                   options = list(placeholder = .tr("Pathway (tapez pour rechercher)")))),
          column(4, div(style = "margin-top:25px;",
                       downloadButton(ns("dl_gsea_curve_png"), .tr("Export PNG"), class = "btn-sm btn-secondary w-100")))
        ),
        checkboxInput(ns("gsea_curve_pvalue_table"), .tr("Afficher la table p-value sur le graphique"), value = TRUE),
        plotOutput(ns("gsea_curve_plot"), height = "500px")
      )
    })

    .gsea_curve_plot_fn <- function() {
      gsea_obj <- attr(shared_rv$pathway_results, "gsea_obj")
      req(gsea_obj, input$gsea_curve_pathway)
      if (!requireNamespace("enrichplot", quietly = TRUE)) {
        stop("Package 'enrichplot' requis (BiocManager::install('enrichplot')).")
      }
      enrichplot::gseaplot2(gsea_obj, geneSetID = input$gsea_curve_pathway,
                            title = input$gsea_curve_pathway,
                            pvalue_table = isTRUE(input$gsea_curve_pvalue_table))
    }

    output$gsea_curve_plot <- renderPlot({
      tryCatch(
        .gsea_curve_plot_fn(),
        error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste("Erreur:", conditionMessage(e)), color = "red") +
            theme_void()
        }
      )
    })

    output$dl_gsea_curve_png <- downloadHandler(
      filename = function() paste0("gsea_curve_", input$gsea_curve_pathway, "_", Sys.Date(), ".png"),
      content  = function(file) {
        png(file, width = 9, height = 7, units = "in", res = 300)
        print(.gsea_curve_plot_fn())
        dev.off()
      }
    )

  }) # /moduleServer
}
