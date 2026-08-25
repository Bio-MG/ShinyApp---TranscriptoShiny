# =============================================================================
# mod_bulk_filter.R  —  Bulk Child 1: Filtering + VST, PCA & Sample-QC
# i18n Phase 3.1 — static labels via i18n$t()/.tr_plain() + server push via update*Input
# =============================================================================
# Entry point of the bulk pipeline: raw counts -> filtered counts ->
# DESeqDataSet(design = ~1) -> VST matrix. Everything downstream (DE, heatmap,
# pathways, report) reads shared_rv$vst_mat / shared_rv$filtered_counts.
#
# Depends on helpers_bulk.R (sourced by global.R, not defined there):
#   filter_bulk_counts(), build_dds(), get_vst_matrix(),
#   plot_bulk_pca(), plot_sample_correlation_heatmap(),
#   bulk_color_scale(), manual_color_picker_ui(), plot_scree_bulk()
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
        tags$label(i18n$t("Counts totaux minimum / g\u00e8ne"), class = "control-label", style = "margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
                i18n$t("Un g\u00e8ne doit avoir au moins ce nombre de reads, cumul\u00e9s sur tous les \u00e9chantillons, pour \u00eatre conserv\u00e9. \u00c9limine le bruit de fond sans biaiser l'analyse diff\u00e9rentielle."))),
    numericInput(ns("min_count"), NULL, 10, min = 0, step = 1),
    numericInput(ns("min_samples"), i18n$t("Nb \u00e9chantillons min. au-dessus du seuil"), 1, min = 1, step = 1),
    numericInput(ns("min_count_per_sample"), i18n$t("Seuil par \u00e9chantillon"), 1, min = 0, step = 1),

    helpText(i18n$t("La transformation VST (variance-stabilizing) est utilis\u00e9e pour la PCA et la heatmap. Si < 4 \u00e9chantillons : repli automatique sur log2(counts normalis\u00e9s + 1).")),

    actionButton(ns("run_filter_norm"),
                 tagList(icon("play"), i18n$t("Lancer Filtrage & VST")),
                 class = "btn-danger w-100"),

    div(class = "small text-muted mt-2", textOutput(ns("filter_status"))),

    hr(),
    div(style = "display:flex;align-items:center;gap:6px;",
        tags$label(tagList("\U0001f3a8", i18n$t("Palette de couleurs (PCA)")), class = "control-label", style = "margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
                i18n$t("Couleurs utilis\u00e9es pour les groupes sur la PCA. Okabe-Ito = s\u00fbre pour daltoniens."))),
    selectInput(ns("palette_choice"), NULL,
               choices = stats::setNames(
                 c("default","okabeito","viridis","set2","manual"),
                 c(.tr_plain("D\u00e9faut (ggplot)"), .tr_plain("Okabe-Ito (daltonien)"), "Viridis",
                   .tr_plain("Set2 (ColorBrewer)"), .tr_plain("Manuel (choisir chaque couleur)"))))
  )
}


# ── UI: PCA tab ────────────────────────────────────────────────────────────────

mod_bulk_filter_pca_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("PCA"),
    fluidRow(
      column(4, selectizeInput(ns("pca_color_by"), i18n$t("Colorer par"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, selectizeInput(ns("pca_shape_by"), i18n$t("Forme par (optionnel)"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE)))
    ),
    uiOutput(ns("manual_palette_ui")),
    checkboxInput(ns("pca_interactive"), i18n$t("Interactif (Plotly \u2014 survol pour identifier l'\u00e9chantillon)"), value = FALSE),
    div(style = "height:620px;overflow-y:auto;", uiOutput(ns("pca_container"))),
    downloadButton(ns("dl_pca_png"), i18n$t("Export PNG (statique)"), class = "btn-sm btn-secondary mt-2"),

    hr(),
    h6(i18n$t("Scree Plot \u2014 Variance Expliqu\u00e9e"), style = "font-weight:bold;"),
    helpText(i18n$t("Combien de composantes principales faut-il regarder ? Une chute nette (\"coude\") indique o\u00f9 le signal biologique s'arr\u00eate et o\u00f9 le bruit commence.")),
    div(style = "height:400px;overflow-y:auto;", plotOutput(ns("plot_scree"), height = "380px")),
    downloadButton(ns("dl_scree_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: QC Échantillons tab ────────────────────────────────────────────────────

mod_bulk_filter_qc_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("QC \u00c9chantillons")),
    div(class = "alert alert-light", style = "font-size:0.85em;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("D\u00e9tecte les \u00e9chantillons mal \u00e9tiquet\u00e9s, les outliers ou doublons inattendus AVANT de lancer l'analyse diff\u00e9rentielle. Des \u00e9chantillons d'un m\u00eame groupe devraient corr\u00e9ler fortement entre eux (cellules sombres group\u00e9es).")),
    fluidRow(
      column(4, selectizeInput(ns("qc_corr_annot"), i18n$t("Annotation"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, selectInput(ns("qc_corr_method"), i18n$t("M\u00e9thode"),
                            choices = c("Pearson" = "pearson", "Spearman" = "spearman")))
    ),
    uiOutput(ns("qc_manual_palette_ui")),
    div(style = "height:640px;overflow-y:auto;", plotOutput(ns("plot_sample_corr"), height = "620px")),
    downloadButton(ns("dl_sample_corr_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2")
  )
}

# tiny helper for selectize placeholders (returns French by default; JS shim
# does not touch placeholders, so keep them neutral/short).
.tr_placeholder <- function() "\u2014"


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_filter_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n: push translated labels/choices on language switch ──────────
    observeEvent(global_data$language, {
      updateNumericInput(session, "min_samples",         label = .tr("Nb \u00e9chantillons min. au-dessus du seuil"))
      updateNumericInput(session, "min_count_per_sample",label = .tr("Seuil par \u00e9chantillon"))
      updateSelectInput(session, "palette_choice",
        choices = stats::setNames(c("default","okabeito","viridis","set2","manual"),
          c(.tr("D\u00e9faut (ggplot)"), .tr("Okabe-Ito (daltonien)"), "Viridis",
            .tr("Set2 (ColorBrewer)"), .tr("Manuel (choisir chaque couleur)"))))
      updateSelectizeInput(session, "pca_color_by",  label = .tr("Colorer par"))
      updateSelectizeInput(session, "pca_shape_by",  label = .tr("Forme par (optionnel)"))
      updateCheckboxInput(session, "pca_interactive", label = .tr("Interactif (Plotly \u2014 survol pour identifier l'\u00e9chantillon)"))
      updateSelectizeInput(session, "qc_corr_annot", label = .tr("Annotation"))
      updateSelectInput(session, "qc_corr_method", label = .tr("M\u00e9thode"))
      updateActionButton(session, "run_filter_norm",
                         label = paste0("\U0001f680 ", .tr("Lancer Filtrage & VST")))
    }, ignoreInit = TRUE)

    # ── Refresh metadata-driven choices when bulk_obj changes ────────────────
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      cat_cols <- if (length(cat_cols) == 0) names(meta) else cat_cols

      updateSelectizeInput(session, "pca_color_by",  choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "pca_shape_by",  choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "qc_corr_annot", choices = cat_cols, server = FALSE)
    }, ignoreNULL = TRUE)

    # ── Mirror PCA inputs + palette choice to shared_rv (read by mod_bulk_report) ─
    observe({
      shared_rv$pca_color_by    <- input$pca_color_by
      shared_rv$pca_shape_by    <- input$pca_shape_by
      shared_rv$bulk_palette    <- input$palette_choice
      shared_rv$pca_manual_colors <- if (identical(input$palette_choice, "manual")) manual_palette_vec() else NULL
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
          .tr("Matrice volumineuse \u2014 le filtrage va r\u00e9duire la taille avant VST/PCA."),
          type = "warning", duration = 5
        )
      }

      # SAFETY: re-filtering invalidates any previously computed contrast,
      # since gene sets between vst_mat and contrasts would otherwise diverge
      # (this caused a heatmap crash when filtering was re-run after a DE pass).
      if (length(shared_rv$contrasts) > 0) {
        showNotification(
          .tr("Les contrastes calcul\u00e9s pr\u00e9c\u00e9demment seront invalid\u00e9s par ce nouveau filtrage."),
          type = "warning", duration = 6
        )
        shared_rv$contrasts       <- list()
        shared_rv$active_contrast <- NULL
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Filtrage & VST..."), value = 0.2)

      tryCatch({
        filtered <- filter_bulk_counts(
          counts, min_count = input$min_count, min_samples = input$min_samples,
          min_count_per_sample = input$min_count_per_sample
        )
        p$set(0.5, .tr("Construction DESeqDataSet (design ~1)..."))
        dds_blind <- build_dds(filtered, meta, design_formula = "~1", run_deseq = FALSE)
        dds_blind <- DESeq2::estimateSizeFactors(dds_blind)

        p$set(0.8, .tr("Transformation VST..."))
        vst_mat <- get_vst_matrix(dds_blind)

        shared_rv$filtered_counts <- filtered
        shared_rv$dds_blind       <- dds_blind
        shared_rv$vst_mat         <- vst_mat

        showNotification(.t_fmt(.tr("\u2713 {n} g\u00e8nes conserv\u00e9s sur {m} \u00e9chantillons"),
                                n = nrow(filtered), m = ncol(filtered)),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur filtrage/VST:"), e$message), type = "error", duration = 8)
      })
    })

    output$filter_status <- renderText({
      global_data$language
      if (is.null(shared_rv$filtered_counts)) .tr("En attente du filtrage...")
      else .t_fmt(.tr("\u2713 {n} g\u00e8nes \u00d7 {m} \u00e9chantillons"),
                  n = nrow(shared_rv$filtered_counts), m = ncol(shared_rv$filtered_counts))
    })

    # =========================================================================
    # PCA
    # =========================================================================
    # ── Manual palette: dynamic color pickers for the active PCA grouping ───
    # Re-evaluates whenever pca_color_by changes (new set of levels) — the
    # picker UI itself is rebuilt by output$manual_palette_ui below.
    manual_pca_levels <- reactive({
      req(global_data$bulk_obj$metadata, input$pca_color_by)
      req(nzchar(input$pca_color_by))
      lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$pca_color_by]]))))
      req(length(lvls) > 0)
      lvls
    })

    output$manual_palette_ui <- renderUI({
      global_data$language  # i18n
      if (!identical(input$palette_choice, "manual")) return(NULL)
      if (!nzchar(input$pca_color_by %||% "")) {
        return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                   .tr("S\u00e9lectionnez d'abord une variable \"Colorer par\" pour personnaliser ses couleurs.")))
      }
      lvls <- tryCatch(manual_pca_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      ids <- paste0("manual_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.t_fmt(.tr("Couleurs manuelles \u2014 {var}"), var = input$pca_color_by),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    manual_palette_vec <- reactive({
      if (!identical(input$palette_choice, "manual")) return(NULL)
      lvls <- tryCatch(manual_pca_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("manual_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    pca_plot <- reactive({
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      pal <- input$palette_choice %||% "default"
      plot_bulk_pca(shared_rv$vst_mat, global_data$bulk_obj$metadata,
                    color_by = if (nzchar(input$pca_color_by %||% "")) input$pca_color_by else NULL,
                    shape_by = if (nzchar(input$pca_shape_by %||% "")) input$pca_shape_by else NULL,
                    palette  = pal,
                    manual_colors = if (identical(pal, "manual")) manual_palette_vec() else NULL,
                    tr = .tr_fn(global_data))
    })
    output$plot_pca <- renderPlot({
      .safe_plot_render(session, "plot_pca", function() pca_plot())
    })

    output$pca_container <- renderUI({
      if (isTRUE(input$pca_interactive)) plotlyOutput(ns("plot_pca_ly"), height = "620px")
      else plotOutput(ns("plot_pca"), height = "620px")
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
    # SCREE PLOT — PCA companion, reuses shared_rv$vst_mat (no extra heavy
    # computation; same ntop=500 variable-gene selection as plot_bulk_pca()).
    # =========================================================================
    scree_plot <- reactive({
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      plot_scree_bulk(shared_rv$vst_mat, tr = .tr_fn(global_data))
    })
    output$plot_scree <- renderPlot({
      .safe_plot_render(session, "plot_scree", function() scree_plot())
    })
    output$dl_scree_png <- downloadHandler(
      filename = function() paste0("scree_plot_bulk_", Sys.Date(), ".png"),
      content  = function(file) ggsave(file, plot = scree_plot(), width = 7, height = 5, dpi = 300)
    )

    # =========================================================================
    # QC: Sample correlation heatmap (reuses shared_rv$vst_mat — zero extra
    # heavy computation)
    # =========================================================================
    # Manual palette: own picker, keyed to qc_corr_annot's levels — kept
    # SEPARATE from the PCA picker above because qc_corr_annot may point to
    # a different metadata column (e.g. "batch" for QC vs "treatment" for PCA).
    manual_qc_levels <- reactive({
      req(global_data$bulk_obj$metadata, input$qc_corr_annot)
      req(nzchar(input$qc_corr_annot))
      lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$qc_corr_annot]]))))
      req(length(lvls) > 0)
      lvls
    })

    output$qc_manual_palette_ui <- renderUI({
      global_data$language  # i18n
      if (!identical(input$palette_choice, "manual")) return(NULL)
      if (!nzchar(input$qc_corr_annot %||% "")) {
        return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                   .tr("S\u00e9lectionnez d'abord une \"Annotation\" pour personnaliser ses couleurs.")))
      }
      lvls <- tryCatch(manual_qc_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      ids <- paste0("qc_manual_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.t_fmt(.tr("Couleurs manuelles \u2014 {var}"), var = input$qc_corr_annot),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    qc_manual_colors <- reactive({
      if (!identical(input$palette_choice, "manual")) return(NULL)
      lvls <- tryCatch(manual_qc_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("qc_manual_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    sample_corr_plot_fn <- function() {
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      annot <- if (nzchar(input$qc_corr_annot %||% "")) input$qc_corr_annot else NULL
      pal   <- input$palette_choice %||% "default"
      plot_sample_correlation_heatmap(
        shared_rv$vst_mat, global_data$bulk_obj$metadata,
        annotation_col = annot, method = input$qc_corr_method %||% "pearson",
        palette = pal, manual_colors = if (identical(pal, "manual")) qc_manual_colors() else NULL,
        tr = .tr_fn(global_data))
    }
    output$plot_sample_corr <- renderPlot({
      req(shared_rv$vst_mat)
      .safe_plot_render(session, "plot_sample_corr", function() print(sample_corr_plot_fn()))
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
