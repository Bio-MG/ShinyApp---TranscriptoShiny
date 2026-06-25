# =============================================================================
# mod_bulk_filter.R  —  Bulk Child 1: Filtering + VST, PCA & Sample-QC
# =============================================================================
# Entry point of the bulk pipeline: raw counts -> filtered counts ->
# DESeqDataSet(design = ~1) -> VST matrix. Everything downstream (DE, heatmap,
# pathways, report) reads shared_rv$vst_mat / shared_rv$filtered_counts.
#
# Depends on global.R:
#   filter_bulk_counts(), build_dds(), get_vst_matrix(),
#   plot_bulk_pca(), plot_sample_correlation_heatmap()
#
# State contract (shared_rv):
#   READ  : shared_rv$counts_mapped    — written by mod_bulk_mapping (Step 0,
#                                         optional); used instead of the raw
#                                         import when present (see %||% below)
#   WRITE : shared_rv$filtered_counts  — matrix, post-filter counts
#           shared_rv$dds_blind        — DESeqDataSet (design ~1), exploration only
#           shared_rv$vst_mat          — matrix, VST-transformed counts
#           shared_rv$contrasts        — RESET to list() if user re-filters
#                                         (stale gene set would no longer match
#                                         existing DE results / heatmap)
#           shared_rv$active_contrast  — RESET to NULL alongside contrasts
#           shared_rv$pca_color_by, shared_rv$pca_shape_by
#                                       — mirrored so mod_bulk_report can read
#                                         them without crossing module namespaces
#
# UI split:
#   mod_bulk_filter_ui(id)      -> sidebar accordion body (Step 1 controls)
#   mod_bulk_filter_pca_ui(id)  -> main panel "PCA" tab
#   mod_bulk_filter_qc_ui(id)   -> main panel "QC Échantillons" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display:flex;align-items:center;gap:6px;",
        tags$label("Counts totaux minimum / gène", class = "control-label", style = "margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
               "Un gène doit avoir au moins ce nombre de reads, cumulés sur tous les échantillons, pour être conservé. Élimine le bruit de fond sans biaiser l'analyse différentielle.")),
    numericInput(ns("min_count"), NULL, 10, min = 0, step = 1),
    numericInput(ns("min_samples"), "Nb échantillons min. au-dessus du seuil", 1, min = 1, step = 1),
    numericInput(ns("min_count_per_sample"), "Seuil par échantillon", 1, min = 0, step = 1),

    helpText("La transformation VST (variance-stabilizing) est utilisée pour la PCA et la heatmap. ",
             "Si < 4 échantillons : repli automatique sur log2(counts normalisés + 1)."),

    actionButton(ns("run_filter_norm"), "🚀 Lancer Filtrage & VST",
                 class = "btn-danger w-100", icon = icon("play")),

    div(class = "small text-muted mt-2", textOutput(ns("filter_status")))
  )
}


# ── UI: PCA tab ────────────────────────────────────────────────────────────────

mod_bulk_filter_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("pca_color_by"), "Colorer par", choices = NULL)),
      column(4, selectInput(ns("pca_shape_by"), "Forme par (optionnel)", choices = NULL))
    ),
    checkboxInput(ns("pca_interactive"), "📊 Interactif (Plotly — survol pour identifier l'échantillon)", value = FALSE),
    uiOutput(ns("pca_container")),
    downloadButton(ns("dl_pca_png"), "Export PNG (statique)", class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: QC Échantillons tab ────────────────────────────────────────────────────

mod_bulk_filter_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;",
        bsicons::bs_icon("info-circle"),
        " Détecte les échantillons mal étiquetés, les outliers ou doublons inattendus ",
        "AVANT de lancer l'analyse différentielle. Des échantillons d'un même groupe ",
        "devraient corréler fortement entre eux (cellules sombres groupées)."),
    fluidRow(
      column(4, selectInput(ns("qc_corr_annot"), "Annotation", choices = NULL)),
      column(4, selectInput(ns("qc_corr_method"), "Méthode",
                            choices = c("Pearson" = "pearson", "Spearman" = "spearman")))
    ),
    plotOutput(ns("plot_sample_corr"), height = "550px"),
    downloadButton(ns("dl_sample_corr_png"), "Export PNG", class = "btn-sm btn-secondary mt-2")
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_filter_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Refresh metadata-driven choices when bulk_obj changes ────────────────
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      cat_cols <- if (length(cat_cols) == 0) names(meta) else cat_cols

      updateSelectInput(session, "pca_color_by",  choices = c("Aucun" = "", cat_cols))
      updateSelectInput(session, "pca_shape_by",  choices = c("Aucun" = "", cat_cols))
      updateSelectInput(session, "qc_corr_annot", choices = c("Aucun" = "", cat_cols))
    }, ignoreNULL = TRUE)

    # ── Mirror PCA inputs to shared_rv (read by mod_bulk_report) ─────────────
    observe({
      shared_rv$pca_color_by <- input$pca_color_by
      shared_rv$pca_shape_by <- input$pca_shape_by
    })

    # ── Polish UI: disable the run button until an import actually exists ───
    observe({
      shinyjs::toggleState("run_filter_norm", condition = !is.null(global_data$bulk_obj))
    })

    # =========================================================================
    # STEP 1 — Filtering + VST
    # =========================================================================
    observeEvent(input$run_filter_norm, {
      req(global_data$bulk_obj)
      # Picks up the Step-0 gene-ID-mapped matrix if one was applied
      # (mod_bulk_mapping.R), otherwise falls back to the raw import.
      counts <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
      meta   <- global_data$bulk_obj$metadata

      if (ncol(counts) > 500 || nrow(counts) > 60000) {
        showNotification(
          "⚠️ Matrice volumineuse — le filtrage va réduire la taille avant VST/PCA.",
          type = "warning", duration = 5
        )
      }

      # SAFETY: re-filtering invalidates any previously computed contrast,
      # since gene sets between vst_mat and contrasts would otherwise diverge
      # (this caused a heatmap crash when filtering was re-run after a DE pass).
      if (length(shared_rv$contrasts) > 0) {
        showNotification(
          "⚠️ Les contrastes calculés précédemment seront invalidés par ce nouveau filtrage.",
          type = "warning", duration = 6
        )
        shared_rv$contrasts       <- list()
        shared_rv$active_contrast <- NULL
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Filtrage & VST...", value = 0.2)

      tryCatch({
        filtered <- filter_bulk_counts(
          counts, min_count = input$min_count, min_samples = input$min_samples,
          min_count_per_sample = input$min_count_per_sample
        )
        p$set(0.5, "Construction DESeqDataSet (design ~1)...")
        dds_blind <- build_dds(filtered, meta, design_formula = "~1", run_deseq = FALSE)
        dds_blind <- DESeq2::estimateSizeFactors(dds_blind)

        p$set(0.8, "Transformation VST...")
        vst_mat <- get_vst_matrix(dds_blind)

        shared_rv$filtered_counts <- filtered
        shared_rv$dds_blind       <- dds_blind
        shared_rv$vst_mat         <- vst_mat

        showNotification(sprintf("✓ %d gènes conservés sur %d échantillons",
                                  nrow(filtered), ncol(filtered)),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Erreur filtrage/VST:", e$message), type = "error", duration = 8)
      })
    })

    output$filter_status <- renderText({
      if (is.null(shared_rv$filtered_counts)) "En attente du filtrage..."
      else sprintf("✓ %d gènes × %d échantillons", nrow(shared_rv$filtered_counts), ncol(shared_rv$filtered_counts))
    })

    # =========================================================================
    # PCA
    # =========================================================================
    pca_plot <- reactive({
      req(shared_rv$vst_mat)
      plot_bulk_pca(shared_rv$vst_mat, global_data$bulk_obj$metadata,
                    color_by = if (nzchar(input$pca_color_by %||% "")) input$pca_color_by else NULL,
                    shape_by = if (nzchar(input$pca_shape_by %||% "")) input$pca_shape_by else NULL)
    })
    output$plot_pca <- renderPlot({ pca_plot() })

    output$pca_container <- renderUI({
      if (isTRUE(input$pca_interactive)) plotlyOutput(ns("plot_pca_ly"), height = "550px")
      else plotOutput(ns("plot_pca"), height = "550px")
    })

    output$plot_pca_ly <- renderPlotly({
      req(pca_plot())
      tryCatch(
        suppressWarnings(ggplotly(pca_plot(), tooltip = c("x", "y", "colour", "shape", "label"))),
        error = function(e) plotly_empty()
      )
    })

    output$dl_pca_png <- downloadHandler(
      filename = function() paste0("pca_bulk_", Sys.Date(), ".png"),
      content  = function(file) ggsave(file, plot = pca_plot(), width = 8, height = 6, dpi = 300)
    )

    # =========================================================================
    # QC: Sample correlation heatmap (reuses shared_rv$vst_mat — zero extra
    # heavy computation)
    # =========================================================================
    sample_corr_plot_fn <- function() {
      req(shared_rv$vst_mat)
      annot <- if (nzchar(input$qc_corr_annot %||% "")) input$qc_corr_annot else NULL
      plot_sample_correlation_heatmap(
        shared_rv$vst_mat, global_data$bulk_obj$metadata,
        annotation_col = annot, method = input$qc_corr_method %||% "pearson"
      )
    }
    output$plot_sample_corr <- renderPlot({
      req(shared_rv$vst_mat)
      print(sample_corr_plot_fn())
    })
    output$dl_sample_corr_png <- downloadHandler(
      filename = function() paste0("sample_correlation_qc_", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 9, height = 8, units = "in", res = 300)
        print(sample_corr_plot_fn())
        dev.off()
      }
    )

  }) # /moduleServer
}
