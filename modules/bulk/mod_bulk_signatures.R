# =============================================================================
# mod_bulk_signatures.R — Bulk V2 M3 : scores de SIGNATURES CELLULAIRES
# (MSigDB Hallmark / PROGENy / DoRothEA / RDS local) — UI + orchestration.
# =============================================================================
# Logique pure : R/bulk/bulk_signatures.R (contrat gelé). Le moteur de scoring
# est réutilisé du M2 (compute_pathway_scores) ou decoupleR (ulm).
# GARDE MISSION §M3 : l'avertissement « scores relatifs » est AFFICHÉ en
# PERMANENCE dans l'UI et porté par le résultat + chaque export.
# =============================================================================

mod_bulk_signatures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # GARDE §M3 — avertissement permanent, non conditionnel.
    div(class = "alert alert-warning", style = "font-size:0.82em;",
        icon("triangle-exclamation"), " ",
        i18n$t("Scores de signatures relatifs : estimation d'abondance relative ne rempla\u00e7ant pas une quantification cytom\u00e9trique.")),
    h6(i18n$t("Ressource de signatures"), style = "font-weight:bold;"),
    selectInput(ns("sig_resource"), i18n$t("Source"),
                choices = stats::setNames(
                  c("hallmark", "progeny", "dorothea", "rds_local"),
                  c(.tr_plain("MSigDB Hallmark (50 voies)"), "PROGENy", "DoRothEA",
                    .tr_plain("Fichier RDS local"))),
                selected = "hallmark"),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'rds_local'", ns("sig_resource")), ns = ns,
      fileInput(ns("sig_rds"), i18n$t("Fichier .rds (liste nommée ou data.frame signature/gene)"),
                accept = c(".rds", ".RDS")),
      div(class = "small text-muted",
          i18n$t("Liste nomm\u00e9e de vecteurs de g\u00e8nes, ou data.frame avec colonnes 'signature' et 'gene'."))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'rds_local'", ns("sig_resource")), ns = ns,
      selectInput(ns("sig_organism"), i18n$t("Organisme"),
                  choices = stats::setNames(c("human", "mouse"),
                                            c(.tr_plain("Humain"), .tr_plain("Souris"))))
    ),
    selectInput(ns("sig_method"), i18n$t("M\u00e9thode de scoring"),
                choices = stats::setNames(
                  c("ssgsea", "gsva", "zscore", "ulm_decoupleR"),
                  c("ssGSEA", "GSVA", "zscore",
                    .tr_plain("ULM (decoupleR)"))),
                selected = "ssgsea"),
    fluidRow(
      column(6, numericInput(ns("sig_min_size"), i18n$t("Taille min signature"),
                             value = TS_BULK_GSVA_MIN_SIZE, min = 1, step = 1)),
      column(6, numericInput(ns("sig_max_size"), i18n$t("Taille max signature"),
                             value = TS_BULK_GSVA_MAX_SIZE, min = 2, step = 1))
    ),
    actionButton(ns("run_signatures"), i18n$t("Lancer Scores de signatures"),
                 class = "btn-warning w-100", icon = icon("fingerprint")),
    div(class = "small text-muted mt-1", textOutput(ns("sig_status")))
  )
}

mod_bulk_signatures_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header(i18n$t("Signatures cellulaires")),
    # GARDE §M3 — l'avertissement voyage aussi avec les vues.
    div(class = "alert alert-light", style = "font-size:0.78em;margin-bottom:4px;",
        icon("info-circle"), " ", i18n$t("Scores de signatures relatifs : estimation d'abondance relative ne rempla\u00e7ant pas une quantification cytom\u00e9trique.")),
    navset_tab(
      id = ns("sig_tabs"),
      nav_panel(i18n$t("Heatmap"), plotOutput(ns("sig_heatmap"), height = "640px")),
      nav_panel("PCA",              plotOutput(ns("sig_pca"), height = "420px")),
      nav_panel(i18n$t("Table"),    DTOutput(ns("sig_table"))),
      nav_panel(i18n$t("Rejets"),   uiOutput(ns("sig_dropped_ui")))
    ),
    fluidRow(
      column(6, downloadButton(ns("dl_sig_csv"), i18n$t("Export CSV"),
                               class = "btn-sm btn-info w-100 mt-2")),
      column(6, downloadButton(ns("dl_sig_rds"), i18n$t("Export RDS (résultat complet)"),
                               class = "btn-sm btn-secondary w-100 mt-2"))
    )
  )
}

mod_bulk_signatures_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "sig_resource", label = .tr("Source"),
        choices = stats::setNames(c("hallmark", "progeny", "dorothea", "rds_local"),
          c(.tr("MSigDB Hallmark (50 voies)"), "PROGENy", "DoRothEA",
            .tr("Fichier RDS local"))))
      updateSelectInput(session, "sig_organism", label = .tr("Organisme"),
        choices = stats::setNames(c("human", "mouse"), c(.tr("Humain"), .tr("Souris"))))
      updateSelectInput(session, "sig_method", label = .tr("Méthode de scoring"),
        choices = stats::setNames(c("ssgsea", "gsva", "zscore", "ulm_decoupleR"),
          c("ssGSEA", "GSVA", "zscore", .tr("ULM (decoupleR)"))))
      updateNumericInput(session, "sig_min_size", label = .tr("Taille min signature"))
      updateNumericInput(session, "sig_max_size", label = .tr("Taille max signature"))
      updateActionButton(session, "run_signatures", label = .tr("Lancer Scores de signatures"))
      updateActionButton(session, "dl_sig_csv", label = .tr("Export CSV"))
      updateActionButton(session, "dl_sig_rds", label = .tr("Export RDS (résultat complet)"))
    }, ignoreInit = TRUE)

    observe({
      shinyjs::toggleState("run_signatures", condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_sig_csv", condition = !is.null(shared_rv$signature_scores))
      shinyjs::toggleState("dl_sig_rds", condition = !is.null(shared_rv$signature_scores))
    })

    output$sig_status <- renderText({
      global_data$language
      sc <- shared_rv$signature_scores
      if (is.null(sc)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      else .t_fmt(.tr("\u2713 {n} signatures scor\u00e9es x {m} \u00e9chantillons [ {meth} ]"),
                  n = nrow(sc$scores), m = ncol(sc$scores), meth = sc$method)
    })

    observeEvent(input$run_signatures, {
      req(shared_rv$vst_mat)
      resource <- input$sig_resource %||% "hallmark"
      sets <- NULL
      if (identical(resource, "rds_local")) {
        if (is.null(input$sig_rds) || is.null(input$sig_rds$datapath)) {
          showNotification(.tr("\u26a0\ufe0f Fournissez un fichier .rds de signatures."),
                           type = "warning", duration = 5)
          return()
        }
        sets <- tryCatch(
          bulk_load_signatures("rds_local", rds_path = input$sig_rds$datapath),
          error = function(e) e)
      } else {
        p <- shiny::Progress$new(); on.exit(p$close())
        p$set(message = .tr("Chargement de la ressource..."), value = 0.1)
        sets <- tryCatch(
          bulk_load_signatures(resource, organism = input$sig_organism %||% "human"),
          error = function(e) e)
      }
      if (inherits(sets, "error")) {
        showNotification(paste(.tr("Erreur signatures:"), conditionMessage(sets)),
                         type = "error", duration = 8)
        return()
      }

      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Scores de signatures..."), value = 0.4)
      tryCatch({
        res <- bulk_score_signatures(
          shared_rv$vst_mat, sets,
          method   = input$sig_method %||% "ssgsea",
          min_size = input$sig_min_size %||% TS_BULK_GSVA_MIN_SIZE,
          max_size = input$sig_max_size %||% TS_BULK_GSVA_MAX_SIZE
        )
        res$resource <- resource
        shared_rv$signature_scores <- res
        # Stockage contractuel bulk_obj$pathways$signatures (réaffectation
        # complète pour l'invalidation propre de global_data).
        bo <- global_data$bulk_obj
        if (!is.list(bo$pathways)) bo$pathways <- list()
        bo$pathways$signatures <- res$scores
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        showNotification(.t_fmt(.tr("\u2713 {n} signatures scor\u00e9es sur {m} \u00e9chantillons."),
                                 n = nrow(res$scores), m = ncol(res$scores)), type = "message")
        nav_select(id = "sig_tabs", selected = "sig_heatmap_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur signatures:"), conditionMessage(e)),
                         type = "error", duration = 8)
        shared_rv$signature_scores <- NULL
      })
    })

    output$sig_heatmap <- renderPlot({
      global_data$language
      req(shared_rv$signature_scores)
      h <- plot_pathway_scores_heatmap(shared_rv$signature_scores, top_n = 60,
                                       tr = .tr_fn(global_data))
      print(h)
    })
    output$sig_pca <- renderPlot({
      global_data$language
      req(shared_rv$signature_scores)
      tryCatch(
        plot_pathway_scores_pca(shared_rv$signature_scores,
                                metadata = global_data$bulk_obj$metadata,
                                color_by = NULL, tr = .tr_fn(global_data)),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            ggplot2::theme_void()
        }
      )
    })
    output$sig_table <- renderDT({
      global_data$language
      req(shared_rv$signature_scores)
      ts_datatable(as.data.frame(round(shared_rv$signature_scores$scores, 4)),
                   page_length = 15L, filename_base = "bulk_signature_scores",
                   filter = "none", rownames = TRUE)
    })
    output$sig_dropped_ui <- renderUI({
      global_data$language
      sc <- shared_rv$signature_scores
      if (is.null(sc) || is.null(sc$qc$dropped) || nrow(sc$qc$dropped) == 0L) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;",
                   icon("check-circle"), " ", .tr("Aucune signature rejet\u00e9e.")))
      }
      div(class = "alert alert-warning", style = "font-size:0.82em;",
          icon("triangle-exclamation"), " ",
          .t_fmt(.tr("{n} signature(s) rejet\u00e9e(s) :"), n = nrow(sc$qc$dropped)),
          ts_datatable(sc$qc$dropped, page_length = 5, buttons = FALSE,
                       filter = "none", scroll_x = FALSE))
    })

    output$dl_sig_csv <- downloadHandler(
      filename = function() paste0("signature_scores_", shared_rv$signature_scores$method,
                                   "_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(shared_rv$signature_scores)
        # GARDE §M3 : la colonne disclaimer voyage avec l'export.
        write.csv(build_signature_scores_export(shared_rv$signature_scores),
                  file, row.names = FALSE)
      }
    )
    output$dl_sig_rds <- downloadHandler(
      filename = function() paste0("signature_scores_", shared_rv$signature_scores$method,
                                   "_", Sys.Date(), ".rds"),
      content  = function(file) {
        req(shared_rv$signature_scores)
        saveRDS(shared_rv$signature_scores, file)
      }
    )

  }) # /moduleServer
}
