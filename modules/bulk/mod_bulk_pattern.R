# =============================================================================
# modules/bulk/mod_bulk_pattern.R — Clustering de profils d'expression (STAT-S3)
# =============================================================================
# UI + orchestration UNIQUEMENT : tout le calcul vit dans R/bulk/bulk_pattern.R
# (logique pure, contract-first — docs/contracts/BULK_PATTERN_CONTRACT.md).
#
# Sources de gènes : résultat DE du contraste actif (up / down / all_sig),
# même pattern que mod_bulk_pathways. Entrée matrice : shared_rv$vst_mat
# (étape 1), groupes : colonne DECLAREE de global_data$bulk_obj$metadata.
#
# MVP kmeans (zéro dépendance nouvelle) ; k et la graine sont déclarés par
# l'utilisateur. Résultat descriptif : aucune p-value produite.
# =============================================================================

mod_bulk_pattern_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.9em;border-left:3px solid #F39C12;",
        i18n$t("Regroupe les gènes par forme de profil d'expression entre groupes (moyenne VST par groupe, z-score par gène, kmeans).")),
    radioButtons(ns("pattern_source"), i18n$t("Source de gènes"),
                 choices = setNames(c("up", "down", "all_sig"),
                                    c(.tr_plain("Surexprimés (up)"), .tr_plain("Sous-exprimés (down)"), .tr_plain("Tous les significatifs")))),
    selectInput(ns("pattern_group"), i18n$t("Colonne de groupe (profils)"), choices = NULL),
    fluidRow(
      column(6, numericInput(ns("pattern_k"), i18n$t("Nombre de clusters (k)"),
                             value = 4, min = 2, max = 12, step = 1)),
      column(6, numericInput(ns("pattern_seed"), i18n$t("Graine (reproductibilité)"),
                             value = 15, min = 1, step = 1))
    ),
    actionButton(ns("run_pattern"), i18n$t("Lancer le clustering"), class = "btn-warning w-100", icon = icon("chart-line")),
    hr(),
    div(class = "small text-muted", textOutput(ns("pattern_status")))
  )
}

mod_bulk_pattern_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Clustering de profils (descriptif)"), class = "mb-0"),
                    downloadButton(ns("dl_pattern"), i18n$t("Export CSV"), class = "btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Profils"), plotOutput(ns("pattern_plot"), height = "560px")),
      nav_panel(i18n$t("Gènes par cluster"), DTOutput(ns("pattern_table")))
    )
  )
}

mod_bulk_pattern_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateRadioButtons(session, "pattern_source", label = .tr("Source de gènes"),
        choices = setNames(c("up", "down", "all_sig"),
                           c(.tr("Surexprimés (up)"), .tr("Sous-exprimés (down)"), .tr("Tous les significatifs"))),
        selected = isolate(input$pattern_source) %||% "up")
      updateSelectInput(session, "pattern_group", label = .tr("Colonne de groupe (profils)"))
      updateNumericInput(session, "pattern_k", label = .tr("Nombre de clusters (k)"))
      updateNumericInput(session, "pattern_seed", label = .tr("Graine (reproductibilité)"))
      updateActionButton(session, "run_pattern", label = .tr("Lancer le clustering"))
    }, ignoreInit = TRUE)

    # Colonnes de groupe candidates : facteur/character avec au moins 2 niveaux
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      ok <- vapply(names(meta), function(cl) {
        x <- meta[[cl]]
        (is.factor(x) || is.character(x)) &&
          length(unique(stats::na.omit(as.character(x)))) >= 2L
      }, logical(1))
      updateSelectInput(session, "pattern_group", choices = names(meta)[ok])
    }, ignoreNULL = TRUE)

    observe({
      shinyjs::toggleState("run_pattern", condition = !is.null(shared_rv$vst_mat))
    })

    # Contraste DE actif — même convention que mod_bulk_pathways
    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    output$pattern_status <- renderText({
      global_data$language
      res <- shared_rv$pattern_result
      if (is.null(res)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1) et l'analyse différentielle (étape 2).")
      else .t_fmt(.tr("\u2713 {g} g\u00e8nes regroup\u00e9s en {k} clusters ({c} groupes, graine {s})."),
                  g = format(res$summary$n_genes_used, big.mark = ","),
                  k = res$k, c = res$summary$n_groups, s = res$seed)
    })

    observeEvent(input$run_pattern, {
      req(shared_rv$vst_mat)
      res_de <- .active_de_results()
      if (is.null(res_de)) {
        showNotification(.tr("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle)."), type = "warning")
        return()
      }
      sig <- res_de$padj < (shared_rv$padj_thresh %||% 0.05) &
        abs(res_de$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
      sig[is.na(sig)] <- FALSE
      genes <- switch(input$pattern_source,
                      up      = res_de$gene[sig & res_de$log2FoldChange > 0],
                      down    = res_de$gene[sig & res_de$log2FoldChange < 0],
                      all_sig = res_de$gene[sig])
      genes <- unique(trimws(genes[nchar(trimws(as.character(genes))) > 0]))
      if (length(genes) < 10) {
        showNotification(.t_fmt(.tr("⚠️ Trop peu de gènes ({n}) — élargissez la liste (source ou seuils)."),
                                n = length(genes)), type = "warning", duration = 6)
        return()
      }
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Clustering des profils (kmeans)..."), value = 0.3)
      tryCatch({
        res <- run_pattern_clustering(
          vst_mat      = shared_rv$vst_mat,
          metadata     = global_data$bulk_obj$metadata,
          group_column = input$pattern_group,
          genes        = genes,
          k            = input$pattern_k,
          seed         = input$pattern_seed
        )
        assert_bulk_pattern_result(res, context = "module clustering de profils")
        shared_rv$pattern_result <- res
        showNotification(.t_fmt(.tr("\u2713 {g} g\u00e8nes regroup\u00e9s en {k} clusters."),
                                 g = format(res$summary$n_genes_used, big.mark = ","),
                                 k = res$k), type = "message")
        shared_rv$active_tab <- "tab_pattern"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur clustering:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$pattern_result <- NULL
      })
    })

    output$pattern_plot <- renderPlot({
      global_data$language
      req(shared_rv$pattern_result)
      plot_pattern_profiles(shared_rv$pattern_result, tr = .tr,
                            palette = "default", manual_colors = NULL)
    })

    output$pattern_table <- renderDT({
      req(shared_rv$pattern_result)
      tab <- build_pattern_table_export(shared_rv$pattern_result)
      ts_datatable(tab, page_length = 15L, filename_base = "pattern_clusters")
    })

    output$dl_pattern <- downloadHandler(
      filename = function() {
        req(shared_rv$pattern_result)
        paste0("pattern_clusters_k", shared_rv$pattern_result$k, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(shared_rv$pattern_result)
        utils::write.csv(build_pattern_table_export(shared_rv$pattern_result),
                         file, row.names = FALSE)
      }
    )
  })
}
