# =============================================================================
# mod_bulk_pathways.R  —  Bulk Child 3: Pathway Enrichment (ORA + GSEA)
# =============================================================================
# Depends on global.R:
#   run_pathway_enrichment(genes, organism, database, pval_cutoff)
#   run_gsea_enrichment(de_results, organism, database, pval_cutoff)
#   plot_pathway_barplot(), plot_pathway_dotplot(), build_pathway_dt()
#
# State contract (shared_rv):
#   READ  : shared_rv$filtered_counts  — written by mod_bulk_filter (Step 1);
#                                         used as the gene universe for the
#                                         manual-selection picker
#           shared_rv$contrasts, shared_rv$active_contrast
#                                       — written by mod_bulk_de (Step 2);
#                                         source of "up"/"down"/"all_sig" gene
#                                         sets (ORA) and the full ranked table
#                                         (GSEA)
#   WRITE : shared_rv$pathway_results  — consumed by mod_bulk_report
#           shared_rv$active_tab       — "tab_pathway" after a successful run
#           shared_rv$pathway_db       — mirrored so mod_bulk_report can read
#                                         it without crossing module namespaces
#
# UI split:
#   mod_bulk_pathways_ui(id)         -> sidebar accordion body (Step 3 controls)
#   mod_bulk_pathways_output_ui(id)  -> main panel "Pathway" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("enrich_mode"), "Méthode d'enrichissement",
                choices = c("ORA — sur gènes significatifs (classique)" = "ora",
                            "GSEA — sur tous les gènes classés (sans seuil)" = "gsea"),
                selected = "ora"),
    div(class = "small text-muted mb-2",
        "GSEA n'a pas besoin de seuil de significativité arbitraire — elle classe tous les gènes par Log2FC et teste l'enrichissement cumulé. Plus robuste statistiquement, recommandée si peu de gènes passent vos seuils."),

    conditionalPanel(
      condition = "input.enrich_mode == 'ora'", ns = ns,
      selectInput(ns("pathway_source"), "Source de gènes",
                  choices = c("Gènes Up (significatifs)"   = "up",
                              "Gènes Down (significatifs)" = "down",
                              "Tous gènes significatifs"   = "all_sig",
                              "Sélection manuelle"         = "manual")),

      conditionalPanel(
        condition = "input.pathway_source == 'manual'", ns = ns,
        selectizeInput(ns("pathway_genes"), "Gènes", choices = NULL, multiple = TRUE)
      )
    ),

    fluidRow(
      column(6, selectInput(ns("pathway_db"), "Base de données",
                            choices = c("GO Biological Process" = "GOBP",
                                        "KEGG Pathways"         = "KEGG",
                                        "Reactome"              = "Reactome"))),
      column(6, selectInput(ns("pathway_org"), "Organisme",
                            choices = c("Humain" = "human", "Souris" = "mouse")))
    ),
    numericInput(ns("pathway_pval"), "P-value cutoff", value = 0.05, min = 0.001, max = 0.1, step = 0.01),

    actionButton(ns("run_pathway"), "Lancer Enrichissement",
                 class = "btn-warning w-100", icon = icon("dna")),

    downloadButton(ns("dl_pathway"), "Export CSV", class = "btn-sm btn-info w-100 mt-2"),

    div(class = "small text-muted mt-1", textOutput(ns("pathway_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_bulk_pathways_output_ui <- function(id) {
  ns <- NS(id)
  navset_tab(
    nav_panel("Barplot Top 15", plotOutput(ns("pathway_barplot"), height = "500px")),
    nav_panel("Dotplot",        plotOutput(ns("pathway_dotplot"), height = "500px")),
    nav_panel("Table",          DTOutput(ns("pathway_table"))),
    nav_panel("Courbe GSEA",    uiOutput(ns("gsea_curve_ui")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Refresh manual gene picker when filtered_counts changes ─────────────
    observeEvent(shared_rv$filtered_counts, {
      req(shared_rv$filtered_counts)
      updateSelectizeInput(session, "pathway_genes",
                          choices = rownames(shared_rv$filtered_counts), server = TRUE)
    })

    # ── Mirror pathway_db to shared_rv (read by mod_bulk_report) ────────────
    observe({
      shared_rv$pathway_db <- input$pathway_db
    })

    # ── Helper: currently active DE result (plain list lookup, no req()) ────
    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    # ── Polish UI: disable enrichment / export buttons until prerequisites
    #    are actually met ───────────────────────────────────────────────────
    observe({
      shinyjs::toggleState("run_pathway", condition = !is.null(shared_rv$filtered_counts))
      shinyjs::toggleState("dl_pathway",  condition = !is.null(shared_rv$pathway_results))
    })

    # =========================================================================
    # PATHWAY ENRICHMENT (réutilise run_pathway_enrichment() + helpers partagés)
    # =========================================================================
    observeEvent(input$run_pathway, {
      req(shared_rv$filtered_counts)

      p <- shiny::Progress$new(); on.exit(p$close())

      # ── GSEA branch: works on the FULL ranked DE table, no gene-list
      #    extraction needed (no significance threshold required) ──────────
      if (input$enrich_mode == "gsea") {
        res_de <- .active_de_results()
        if (is.null(res_de)) {
          showNotification("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle).", type = "warning")
          return()
        }
        p$set(message = "GSEA en cours...", value = 0.3)
        tryCatch({
          res <- run_gsea_enrichment(
            res_de, organism = input$pathway_org,
            database = input$pathway_db, pval_cutoff = input$pathway_pval
          )
          if (nrow(res) == 0) {
            showNotification("ℹ️ Aucun pathway enrichi trouvé (GSEA).", type = "warning")
            shared_rv$pathway_results <- NULL; return()
          }
          shared_rv$pathway_results <- res
          showNotification(paste("✅", nrow(res), "pathways enrichis (GSEA)"), type = "message")
          shared_rv$active_tab <- "tab_pathway"
        }, error = function(e) {
          showNotification(paste0("❌ Erreur GSEA: ", as.character(e$message)[1]),
                           type = "error", duration = 8)
          shared_rv$pathway_results <- NULL
        })
        return()
      }

      # ── ORA branch (existing logic, unchanged) ──────────────────────────
      genes_to_test <- NULL
      if (input$pathway_source %in% c("up", "down", "all_sig")) {
        res <- .active_de_results()
        if (is.null(res)) {
          showNotification("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle).", type = "warning")
          return()
        }
        sig <- res$padj < (shared_rv$padj_thresh %||% 0.05) &
               abs(res$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
        sig[is.na(sig)] <- FALSE
        genes_to_test <- switch(input$pathway_source,
          up      = res$gene[sig & res$log2FoldChange > 0],
          down    = res$gene[sig & res$log2FoldChange < 0],
          all_sig = res$gene[sig]
        )
      } else {
        req(input$pathway_genes)
        genes_to_test <- input$pathway_genes
      }

      genes_to_test <- unique(trimws(genes_to_test))
      genes_to_test <- genes_to_test[nchar(genes_to_test) > 0]

      if (length(genes_to_test) < 10) {
        showNotification(sprintf("⚠️ Trop peu de gènes (%d). Minimum 10 requis.", length(genes_to_test)),
                         type = "warning", duration = 5)
        return()
      }

      p$set(message = "Enrichissement...", value = 0.3)

      tryCatch({
        res <- run_pathway_enrichment(
          genes = genes_to_test, organism = input$pathway_org,
          database = input$pathway_db, pval_cutoff = input$pathway_pval
        )
        if (nrow(res) == 0) {
          showNotification("ℹ️ Aucun pathway enrichi trouvé.", type = "warning")
          shared_rv$pathway_results <- NULL; return()
        }
        shared_rv$pathway_results <- res
        showNotification(paste("✅", nrow(res), "pathways enrichis"), type = "message")
        shared_rv$active_tab <- "tab_pathway"
      }, error = function(e) {
        showNotification(paste0("❌ Erreur pathway: ", as.character(e$message)[1]),
                         type = "error", duration = 6)
        shared_rv$pathway_results <- NULL
      })
    })

    output$pathway_status <- renderText({
      if (is.null(shared_rv$pathway_results)) "Aucune analyse en cours"
      else paste("✓", nrow(shared_rv$pathway_results), "pathways trouvés [", input$pathway_db, "]")
    })

    output$pathway_barplot <- renderPlot({
      req(shared_rv$pathway_results)
      plot_pathway_barplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 15)
    })
    output$pathway_dotplot <- renderPlot({
      req(shared_rv$pathway_results)
      plot_pathway_dotplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 20)
    })
    output$pathway_table <- renderDT({
      req(shared_rv$pathway_results)
      build_pathway_dt(shared_rv$pathway_results)
    })
    output$dl_pathway <- downloadHandler(
      filename = function() paste0("pathways_bulk_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(shared_rv$pathway_results); write.csv(shared_rv$pathway_results, file, row.names = FALSE) }
    )

    # =========================================================================
    # GSEA CURVE — auto-detects ORA vs GSEA results (a GSEA run attaches the
    # raw gseaResult S4 object via attr(df, "gsea_obj"); ORA results never do).
    # enrichplot::gseaplot2() needs that raw object — NOT the flattened
    # data.frame — to draw the running enrichment-score curve.
    # =========================================================================
    output$gsea_curve_ui <- renderUI({
      gsea_obj <- attr(shared_rv$pathway_results, "gsea_obj")
      if (is.null(gsea_obj)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"),
                   " Disponible uniquement pour les résultats GSEA — relancez l'enrichissement en mode ",
                   tags$strong("GSEA"), " (panneau de gauche)."))
      }
      df <- shared_rv$pathway_results
      choices <- setNames(df$ID, sprintf("%s (NES=%.2f, p.adj=%.1e)", df$Description, df$NES, df$p.adjust))
      tagList(
        fluidRow(
          column(8, selectizeInput(ns("gsea_curve_pathway"), "Pathway (tapez pour rechercher)",
                                   choices = choices, width = "100%",
                                   options = list(placeholder = "Rechercher un pathway..."))),
          column(4, div(style = "margin-top:25px;",
                       downloadButton(ns("dl_gsea_curve_png"), "Export PNG", class = "btn-sm btn-secondary w-100")))
        ),
        checkboxInput(ns("gsea_curve_pvalue_table"), "Afficher la table p-value sur le graphique", value = TRUE),
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
