# =============================================================================
# mod_bulk_de.R  —  Bulk Child 2: Design & Contrastes (Differential Expression)
# =============================================================================
# Builds the design formula, runs DESeq2/edgeR/limma-voom, stores one DE
# result data.frame per named "contrast", and owns the 4 tabs that visualise
# the currently active contrast (Volcano, MA-Plot, Heatmap, Table DE).
#
# Also owns:
#   - Pairwise auto: when condition_col has > 2 levels, computes every
#     pairwise contrast in one click (single DESeq2 fit reused across all
#     pairs; edgeR/limma refit per pair since those helpers are 2-group only).
#   - sync_warning: desync banner shown on BOTH Volcano and Heatmap tabs,
#     detecting when the active contrast's gene set no longer matches the
#     CURRENT filtered_counts (e.g. Step 1 was re-run after this DE pass).
#   - Dedicated heatmap export (PNG/PDF) — ComplexHeatmap objects aren't
#     ggplot, so ggsave() doesn't work; uses an explicit graphics device +
#     print() instead (same pattern as mod_bulk_filter's QC-corr export).
#   - Optional interactive Volcano AND MA-Plot (plotly, native plot_ly for
#     full gene/log2FC/baseMean/padj tooltips) — toggled via a checkbox, PNG
#     export always uses the static ggplot version regardless of the toggle.
#
# Depends on global.R:
#   has_limma, check_design_confounding(), validate_bulk_design(), build_dds(),
#   run_bulk_de_dispatch(), .normalize_de_cols(), plot_volcano_bulk(),
#   plot_ma_bulk(), plot_heatmap_bulk(), build_de_results_dt()
#
# State contract (shared_rv):
#   READ  : shared_rv$filtered_counts  — written by mod_bulk_filter (Step 1)
#           shared_rv$vst_mat          — written by mod_bulk_filter (for heatmap)
#   WRITE : shared_rv$dds_full         — DESeqDataSet (real design)
#           shared_rv$contrasts        — named list of DE result data.frames
#           shared_rv$active_contrast  — name of the currently displayed contrast
#           shared_rv$lfc_thresh, shared_rv$padj_thresh,
#           shared_rv$heatmap_top_n, shared_rv$heatmap_annot
#                                       — mirrored so mod_bulk_report can read
#                                         them without crossing module namespaces
#
# UI split:
#   mod_bulk_de_ui(id)          -> sidebar accordion body (Step 2 controls)
#   mod_bulk_de_volcano_ui(id)  -> main panel "Volcano Plot" tab
#   mod_bulk_de_ma_ui(id)       -> main panel "MA-Plot" tab
#   mod_bulk_de_heatmap_ui(id)  -> main panel "Heatmap" tab
#   mod_bulk_de_table_ui(id)    -> main panel "Table DE" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("condition_col"), "Variable de Condition (principale)", choices = NULL),
    selectizeInput(ns("covariates"), "Covariables additionnelles (optionnel)",
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Ex: batch, sex")),

    verbatimTextOutput(ns("design_formula_preview")),

    uiOutput(ns("de_readiness_check")),

    fluidRow(
      column(6, selectInput(ns("group_ref"),    "Groupe Référence", choices = NULL)),
      column(6, selectInput(ns("group_target"), "Groupe Cible",     choices = NULL))
    ),

    selectInput(ns("de_engine"), "Moteur Statistique", choices = NULL),
    div(style = "display:flex;align-items:center;gap:6px;",
        checkboxInput(ns("shrink_lfc"), "Shrinkage LFC (apeglm) — DESeq2 uniquement", value = TRUE),
        tooltip(bsicons::bs_icon("info-circle"),
               "Réduit les Log2FC artificiellement élevés sur les gènes à faible expression (haute variance d'échantillonnage). Recommandé pour le classement/visualisation ; laissez activé sauf besoin spécifique.")),

    fluidRow(
      column(6, numericInput(ns("lfc_thresh"),  "Seuil |Log2FC|", value = 1,    min = 0, step = 0.1)),
      column(6, numericInput(ns("padj_thresh"), "Seuil p-adj",    value = 0.05, min = 0, max = 1, step = 0.01))
    ),

    textInput(ns("contrast_name"), "Nom du contraste (auto si vide)", placeholder = "Ex: KO_vs_WT"),

    actionButton(ns("run_de"), "🚀 Lancer l'Analyse Différentielle",
                 class = "btn-success w-100", icon = icon("play")),

    uiOutput(ns("pairwise_btn_ui")),

    hr(),

    h6("Contrastes calculés:", style = "font-weight:bold;"),
    selectInput(ns("active_contrast_view"), NULL, choices = NULL),

    div(class = "small text-muted mt-1", textOutput(ns("de_status")))
  )
}


# ── UI: Volcano Plot tab ──────────────────────────────────────────────────────

mod_bulk_de_volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sync_warning_banner")),
    checkboxInput(ns("volcano_interactive"), "📊 Interactif (Plotly — survol pour détails gène)", value = FALSE),
    uiOutput(ns("volcano_container")),
    downloadButton(ns("dl_volcano_png"), "Export PNG (statique)", class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: MA-Plot tab ────────────────────────────────────────────────────────────

mod_bulk_de_ma_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("ma_interactive"), "📊 Interactif (Plotly — survol pour détails gène)", value = FALSE),
    uiOutput(ns("ma_container")),
    downloadButton(ns("dl_ma_png"), "Export PNG (statique)", class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: Heatmap tab ────────────────────────────────────────────────────────────

mod_bulk_de_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sync_warning_banner_heatmap")),
    fluidRow(
      column(4, numericInput(ns("heatmap_top_n"), "Top N gènes (par p-adj)", value = 30, min = 2, max = 200)),
      column(4, selectInput(ns("heatmap_annot"), "Annotation colonnes", choices = NULL))
    ),
    plotOutput(ns("plot_heatmap"), height = "600px"),
    fluidRow(
      column(6, selectInput(ns("heatmap_export_fmt"), "Format export", choices = c("PNG" = "png", "PDF" = "pdf"))),
      column(6, div(style = "margin-top:25px;", downloadButton(ns("dl_heatmap"), "📥 Export Heatmap", class = "btn-sm btn-secondary w-100")))
    )
  )
}


# ── UI: Table DE tab ───────────────────────────────────────────────────────────

mod_bulk_de_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display:flex;justify-content:flex-end;gap:5px;margin-bottom:5px;",
      downloadButton(ns("dl_de_csv"),   "CSV",   class = "btn-sm btn-primary"),
      downloadButton(ns("dl_de_excel"), "Excel", class = "btn-sm btn-success")
    ),
    DTOutput(ns("table_de"))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_de_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Engine availability (graceful degradation) ───────────────────────────
    available_engines <- reactive({
      c(
        if (requireNamespace("DESeq2", quietly = TRUE)) c("DESeq2 (recommandé)" = "deseq2"),
        if (requireNamespace("edgeR",  quietly = TRUE)) c("edgeR"              = "edger"),
        if (has_limma)                                  c("limma-voom"         = "limma")
      )
    })

    observe({
      eng <- available_engines()
      validate(need(length(eng) > 0,
                    "Aucun moteur DE disponible (installez DESeq2, edgeR ou limma)."))
      updateSelectInput(session, "de_engine", choices = eng, selected = eng[1])
    })

    # ── Refresh metadata-driven choices when bulk_obj changes ────────────────
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      cat_cols <- if (length(cat_cols) == 0) names(meta) else cat_cols

      updateSelectInput(session, "condition_col", choices = cat_cols)
      updateSelectizeInput(session, "covariates",  choices = cat_cols, server = TRUE)
      updateSelectInput(session, "heatmap_annot",  choices = c("Aucun" = "", cat_cols))
    }, ignoreNULL = TRUE)

    # ── Update group_ref/group_target when condition_col changes ────────────
    observeEvent(input$condition_col, {
      req(global_data$bulk_obj, input$condition_col)
      meta <- global_data$bulk_obj$metadata
      lvls <- unique(na.omit(as.character(meta[[input$condition_col]])))
      validate(need(length(lvls) >= 2, "La colonne de condition doit avoir au moins 2 niveaux."))
      updateSelectInput(session, "group_ref",    choices = lvls, selected = lvls[1])
      updateSelectInput(session, "group_target", choices = lvls, selected = lvls[min(2, length(lvls))])
    })

    # ── Mirror report-relevant scalars to shared_rv ─────────────────────────
    observe({
      shared_rv$lfc_thresh    <- input$lfc_thresh
      shared_rv$padj_thresh   <- input$padj_thresh
      shared_rv$heatmap_top_n <- input$heatmap_top_n
      shared_rv$heatmap_annot <- input$heatmap_annot
    })

    # ── Design formula preview ───────────────────────────────────────────────
    output$design_formula_preview <- renderText({
      req(input$condition_col)
      terms <- c(input$covariates, input$condition_col)
      paste0("Design: ~ ", paste(unique(terms), collapse = " + "))
    })

    # ── Proactive validation banner (didactic — shown BEFORE the user clicks
    #    "Lancer l'Analyse Différentielle", not after a cryptic crash) ───────
    output$de_readiness_check <- renderUI({
      req(global_data$bulk_obj, input$condition_col)
      meta <- global_data$bulk_obj$metadata

      issues <- character(0)
      if (is.null(shared_rv$filtered_counts)) {
        issues <- c(issues, "Étape 1 (Filtrage & VST) non lancée — obligatoire avant le calcul DE.")
      }
      if (input$condition_col %in% colnames(meta)) {
        issues <- c(issues, validate_bulk_design(meta, input$condition_col, input$covariates %||% character(0)))
      }

      if (length(issues) == 0) return(NULL)
      div(class = "alert alert-warning", style = "font-size:0.82em;",
          icon("triangle-exclamation"), tags$strong(" Avant de lancer :"),
          tags$ul(lapply(issues, tags$li)))
    })

    # ── Helper shared by single-pair + pairwise: build the design string ────
    .design_str <- function() {
      terms <- unique(c(input$covariates, input$condition_col))
      paste0("~ ", paste(terms, collapse = " + "))
    }

    # ── Helper: register a freshly computed contrast (shared by single +
    #    pairwise paths) ──────────────────────────────────────────────────────
    .register_contrast <- function(name, res) {
      current <- shared_rv$contrasts
      current[[name]] <- res
      shared_rv$contrasts <- current
    }

    # ── Polish UI: disable the DE button until Step 1 has actually run ──────
    observe({
      shinyjs::toggleState("run_de", condition = !is.null(shared_rv$filtered_counts))
    })

    # =========================================================================
    # STEP 2 — Differential Expression (single pair)
    # =========================================================================
    observeEvent(input$run_de, {
      req(shared_rv$filtered_counts, input$condition_col, input$group_ref, input$group_target,
          input$de_engine)

      if (input$group_ref == input$group_target) {
        showNotification("⚠️ Le groupe Référence et le groupe Cible doivent être différents.",
                         type = "warning"); return()
      }

      meta <- global_data$bulk_obj$metadata
      grp_n <- table(meta[[input$condition_col]])
      if (any(grp_n[c(input$group_ref, input$group_target)] < 2)) {
        showNotification("⚠️ Au moins un groupe a < 2 réplicats — résultats peu fiables.",
                         type = "warning", duration = 6)
      }

      # HARD BLOCK: confounded covariate would make DESeq2's design matrix
      # lose full rank, producing a cryptic linear-algebra error deep inside
      # DESeq(). Catch it here with an actionable message instead.
      covariates_in_use <- input$covariates %||% character(0)
      confounded <- Filter(
        function(cov) check_design_confounding(meta, input$condition_col, cov),
        covariates_in_use
      )
      if (length(confounded) > 0) {
        showNotification(
          sprintf("❌ Covariable(s) confondue(s) avec '%s' : %s. Retirez-la(les) du design ou revoyez votre plan d'expérience.",
                  input$condition_col, paste(confounded, collapse = ", ")),
          type = "error", duration = 10
        )
        return()
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Analyse différentielle...", value = 0.2)

      tryCatch({
        design_str <- .design_str()

        res <- NULL
        if (input$de_engine == "deseq2") {
          p$set(0.4, "Ajustement DESeq2...")
          dds_full <- build_dds(shared_rv$filtered_counts, meta, design_formula = design_str, run_deseq = TRUE)
          shared_rv$dds_full <- dds_full
          res <- run_bulk_de_dispatch("deseq2", shared_rv$filtered_counts, meta, input$condition_col,
                                      input$group_target, input$group_ref,
                                      dds = dds_full, shrink = input$shrink_lfc)
        } else {
          p$set(0.5, paste("Ajustement", input$de_engine, "..."))
          res <- run_bulk_de_dispatch(input$de_engine, shared_rv$filtered_counts, meta,
                                      input$condition_col, input$group_target, input$group_ref)
        }

        res <- .normalize_de_cols(res, counts_for_basemean = shared_rv$filtered_counts)

        contrast_name <- if (nchar(trimws(input$contrast_name)) > 0) {
          trimws(input$contrast_name)
        } else {
          paste0(input$group_target, "_vs_", input$group_ref)
        }

        .register_contrast(contrast_name, res)
        shared_rv$active_contrast <- contrast_name

        updateSelectInput(session, "active_contrast_view",
                          choices = names(shared_rv$contrasts), selected = contrast_name)

        n_sig <- sum(res$padj < input$padj_thresh & abs(res$log2FoldChange) > input$lfc_thresh, na.rm = TRUE)
        showNotification(sprintf("✓ Contraste '%s': %d gènes significatifs", contrast_name, n_sig),
                         type = "message", duration = 6)

      }, error = function(e) {
        showNotification(paste("Erreur DE:", e$message), type = "error", duration = 10)
      })
    })

    # =========================================================================
    # PAIRWISE AUTO — visible only when condition_col has > 2 levels
    # =========================================================================
    output$pairwise_btn_ui <- renderUI({
      req(global_data$bulk_obj, input$condition_col)
      meta <- global_data$bulk_obj$metadata
      req(input$condition_col %in% colnames(meta))
      lvls <- unique(na.omit(as.character(meta[[input$condition_col]])))
      if (length(lvls) > 2) {
        n_pairs <- choose(length(lvls), 2)
        actionButton(ns("run_pairwise"),
                    sprintf("⚡ Calculer les %d paires possibles", n_pairs),
                    class = "btn-outline-success w-100 mt-1", icon = icon("layer-group"))
      } else NULL
    })

    # Stashes the "proceed" closure between the confirmation modal and the
    # actual run, so the heavy logic is written exactly once.
    pairwise_proceed_fn <- reactiveVal(NULL)

    .run_pairwise_now <- function(pairs, meta) {
      n_pairs <- length(pairs)
      withProgress(message = "Calcul des contrastes pairwise...", value = 0, {
        dds_full <- NULL
        if (input$de_engine == "deseq2") {
          dds_full <- tryCatch(
            build_dds(shared_rv$filtered_counts, meta, design_formula = .design_str(), run_deseq = TRUE),
            error = function(e) {
              showNotification(paste("Erreur ajustement DESeq2:", conditionMessage(e)), type = "error", duration = 10)
              NULL
            }
          )
          if (is.null(dds_full)) return(invisible(NULL))
          shared_rv$dds_full <- dds_full
        }

        ok <- 0; failed <- character(0)
        for (i in seq_along(pairs)) {
          ref <- pairs[[i]][1]; target <- pairs[[i]][2]
          incProgress(1 / n_pairs, detail = sprintf("%s vs %s (%d/%d)", target, ref, i, n_pairs))
          name <- sprintf("%s_vs_%s", target, ref)

          res <- tryCatch({
            r <- if (input$de_engine == "deseq2") {
              run_bulk_de_dispatch("deseq2", shared_rv$filtered_counts, meta, input$condition_col,
                                   target, ref, dds = dds_full, shrink = input$shrink_lfc)
            } else {
              run_bulk_de_dispatch(input$de_engine, shared_rv$filtered_counts, meta,
                                   input$condition_col, target, ref)
            }
            .normalize_de_cols(r, counts_for_basemean = shared_rv$filtered_counts)
          }, error = function(e) { failed <<- c(failed, name); NULL })

          if (!is.null(res)) { .register_contrast(name, res); ok <- ok + 1 }
        }

        if (ok > 0 && is.null(shared_rv$active_contrast)) {
          shared_rv$active_contrast <- names(shared_rv$contrasts)[1]
        }
        updateSelectInput(session, "active_contrast_view",
                          choices = names(shared_rv$contrasts), selected = shared_rv$active_contrast)

        msg <- sprintf("✓ %d/%d contrastes calculés.", ok, n_pairs)
        if (length(failed) > 0) msg <- paste0(msg, " Échecs: ", paste(failed, collapse = ", "))
        showNotification(msg, type = if (length(failed) == 0) "message" else "warning", duration = 8)
      })
    }

    observeEvent(input$run_pairwise, {
      req(shared_rv$filtered_counts, input$condition_col, input$de_engine, global_data$bulk_obj)
      meta <- global_data$bulk_obj$metadata
      lvls <- unique(na.omit(as.character(meta[[input$condition_col]])))
      validate(need(length(lvls) > 2, "Au moins 3 niveaux requis pour le mode pairwise."))
      pairs   <- utils::combn(lvls, 2, simplify = FALSE)
      n_pairs <- length(pairs)

      if (n_pairs > 10) {
        pairwise_proceed_fn(function() .run_pairwise_now(pairs, meta))
        showModal(modalDialog(
          title = "Confirmation — beaucoup de contrastes",
          sprintf("Cela va lancer %d analyses différentielles (une par paire de '%s'). ",
                  n_pairs, input$condition_col),
          "Cela peut prendre du temps selon le moteur statistique choisi. Continuer ?",
          footer = tagList(
            modalButton("Annuler"),
            actionButton(ns("confirm_pairwise"), "Oui, lancer", class = "btn-success")
          )
        ))
      } else {
        .run_pairwise_now(pairs, meta)
      }
    })

    observeEvent(input$confirm_pairwise, {
      removeModal()
      fn <- pairwise_proceed_fn()
      if (!is.null(fn)) fn()
      pairwise_proceed_fn(NULL)
    })

    # Sync active_contrast when user switches the dropdown
    observeEvent(input$active_contrast_view, {
      req(input$active_contrast_view %in% names(shared_rv$contrasts))
      shared_rv$active_contrast <- input$active_contrast_view
    })

    # Reactive accessor for the currently displayed DE result (local to this
    # module — sibling modules read shared_rv$contrasts[[shared_rv$active_contrast]]
    # directly since they don't need req()-based reactive semantics here)
    active_de_results <- reactive({
      req(shared_rv$active_contrast, shared_rv$contrasts[[shared_rv$active_contrast]])
      shared_rv$contrasts[[shared_rv$active_contrast]]
    })

    output$de_status <- renderText({
      if (length(shared_rv$contrasts) == 0) "Aucun contraste calculé."
      else paste("Contraste actif:", shared_rv$active_contrast %||% "-")
    })

    # =========================================================================
    # SYNC WARNING — shared by Volcano + Heatmap tabs. Detects when the active
    # contrast's genes no longer match the CURRENT filtered_counts (Step 1 was
    # re-run after this DE pass). Defense-in-depth: mod_bulk_filter already
    # wipes shared_rv$contrasts on re-filter, but this catches any remaining
    # edge case (e.g. that safety being bypassed by a future code change)
    # instead of failing silently downstream.
    # =========================================================================
    sync_warning <- reactive({
      res <- tryCatch(active_de_results(), error = function(e) NULL)
      if (is.null(res) || is.null(shared_rv$filtered_counts)) return(NULL)
      total   <- nrow(res)
      present <- sum(res$gene %in% rownames(shared_rv$filtered_counts))
      pct_missing <- if (total > 0) 100 * (total - present) / total else 0
      if (pct_missing > 1) {
        sprintf(
          "⚠️ %.0f%% des gènes du contraste '%s' ne sont plus présents dans les données filtrées actuelles. Le filtrage (Étape 1) a probablement été relancé après ce calcul — relancez l'Étape 2 pour resynchroniser.",
          pct_missing, shared_rv$active_contrast
        )
      } else NULL
    })

    .sync_banner <- function() {
      msg <- sync_warning()
      if (is.null(msg)) return(NULL)
      div(class = "alert alert-danger", style = "font-size:0.82em;", icon("triangle-exclamation"), " ", msg)
    }
    output$sync_warning_banner         <- renderUI({ .sync_banner() })
    output$sync_warning_banner_heatmap <- renderUI({ .sync_banner() })

    # =========================================================================
    # VOLCANO — static (export) + optional interactive (plotly, native tooltip)
    # =========================================================================
    volcano_plot <- reactive({
      req(active_de_results())
      plot_volcano_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh)
    })
    output$plot_volcano <- renderPlot({ volcano_plot() })

    output$volcano_container <- renderUI({
      if (isTRUE(input$volcano_interactive)) plotlyOutput(ns("plot_volcano_ly"), height = "600px")
      else plotOutput(ns("plot_volcano"), height = "600px")
    })

    output$plot_volcano_ly <- renderPlotly({
      req(active_de_results())
      res <- active_de_results()
      res <- res[!is.na(res$padj), ]
      lfc <- input$lfc_thresh; pval <- input$padj_thresh
      res$status <- dplyr::case_when(
        res$padj < pval & res$log2FoldChange >  lfc ~ "Up",
        res$padj < pval & res$log2FoldChange < -lfc ~ "Down",
        TRUE ~ "NS"
      )
      color_map <- c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")

      plot_ly(
        data = res, x = ~log2FoldChange, y = ~-log10(padj + 1e-300),
        type = "scatter", mode = "markers",
        marker = list(color = ~color_map[status], size = 6, opacity = 0.75, line = list(width = 0)),
        hovertext = ~paste0("<b>", gene, "</b><br>Log2FC: ", round(log2FoldChange, 3),
                            "<br>-log10(padj): ", round(-log10(padj + 1e-300), 2),
                            "<br>Statut: ", status),
        hoverinfo = "text"
      ) |>
        layout(
          title  = paste("Volcano —", shared_rv$active_contrast %||% ""),
          xaxis  = list(title = "Log2 Fold Change", zeroline = TRUE),
          yaxis  = list(title = "-log10(P-adj)"),
          shapes = list(
            list(type = "line", x0 = lfc, x1 = lfc, y0 = 0, y1 = 1, yref = "paper",
                line = list(dash = "dot", color = "#E74C3C", width = 1)),
            list(type = "line", x0 = -lfc, x1 = -lfc, y0 = 0, y1 = 1, yref = "paper",
                line = list(dash = "dot", color = "#2980B9", width = 1)),
            list(type = "line", x0 = min(res$log2FoldChange, na.rm = TRUE),
                x1 = max(res$log2FoldChange, na.rm = TRUE),
                y0 = -log10(pval), y1 = -log10(pval),
                line = list(dash = "dot", color = "#7F8C8D", width = 1))
          ),
          showlegend = FALSE
        )
    })

    output$dl_volcano_png <- downloadHandler(
      filename = function() paste0("volcano_", shared_rv$active_contrast, "_", Sys.Date(), ".png"),
      content  = function(file) ggsave(file, plot = volcano_plot(), width = 8, height = 6, dpi = 300)
    )

    # =========================================================================
    # MA-PLOT — static (export) + optional interactive (plotly, native tooltip)
    # =========================================================================
    ma_plot <- reactive({
      req(active_de_results())
      plot_ma_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh)
    })
    output$plot_ma <- renderPlot({ ma_plot() })

    output$ma_container <- renderUI({
      if (isTRUE(input$ma_interactive)) plotlyOutput(ns("plot_ma_ly"), height = "550px")
      else plotOutput(ns("plot_ma"), height = "550px")
    })

    output$plot_ma_ly <- renderPlotly({
      req(active_de_results())
      res <- active_de_results()
      res <- res[!is.na(res$padj) & !is.na(res$baseMean), ]
      lfc <- input$lfc_thresh; pval <- input$padj_thresh
      res$sig <- res$padj < pval & abs(res$log2FoldChange) > lfc

      plot_ly(
        data = res, x = ~log10(baseMean + 1), y = ~log2FoldChange,
        type = "scatter", mode = "markers",
        marker = list(color = ~ifelse(sig, "#E74C3C", "#BDC3C7"), size = 6, opacity = 0.7, line = list(width = 0)),
        hovertext = ~paste0("<b>", gene, "</b><br>BaseMean: ", round(baseMean, 1),
                            "<br>Log2FC: ", round(log2FoldChange, 3),
                            "<br>padj: ", format(padj, scientific = TRUE, digits = 2)),
        hoverinfo = "text"
      ) |>
        layout(
          title = paste("MA-Plot —", shared_rv$active_contrast %||% ""),
          xaxis = list(title = "Log10(Expression Moyenne + 1)"),
          yaxis = list(title = "Log2 Fold Change"),
          shapes = list(list(type = "line", x0 = 0, x1 = max(log10(res$baseMean + 1)), y0 = 0, y1 = 0,
                             line = list(color = "grey30", width = 1))),
          showlegend = FALSE
        )
    })

    output$dl_ma_png <- downloadHandler(
      filename = function() paste0("ma_plot_", shared_rv$active_contrast, "_", Sys.Date(), ".png"),
      content  = function(file) ggsave(file, plot = ma_plot(), width = 8, height = 6, dpi = 300)
    )

    # =========================================================================
    # HEATMAP — render + dedicated PNG/PDF export (no ggsave: ComplexHeatmap
    # objects are grid grobs, not ggplot — need an explicit device + print()).
    # =========================================================================
    heatmap_genes <- reactive({
      req(shared_rv$vst_mat, active_de_results())
      res <- active_de_results()
      res <- res[!is.na(res$padj), ]
      validate(need(nrow(res) > 0,
                    "Aucun gène avec p-adj valide (DESeq2 a peut-être filtré tous les outliers)."))
      ranked_genes <- res$gene[order(res$padj)]
      valid_genes  <- intersect(ranked_genes, rownames(shared_rv$vst_mat))
      genes        <- head(valid_genes, input$heatmap_top_n)
      validate(need(
        length(genes) >= 2,
        paste0("Pas assez de gènes communs entre le contraste actif et la matrice VST actuelle (",
               length(valid_genes), " trouvés). Relancez l'étape 2 (DE) après tout changement de filtrage.")
      ))
      genes
    })

    .heatmap_obj <- function() {
      annot <- if (nzchar(input$heatmap_annot %||% "")) input$heatmap_annot else NULL
      plot_heatmap_bulk(shared_rv$vst_mat, heatmap_genes(), global_data$bulk_obj$metadata, annotation_col = annot)
    }

    output$plot_heatmap <- renderPlot({ .heatmap_obj() })

    output$dl_heatmap <- downloadHandler(
      filename = function() paste0("heatmap_", shared_rv$active_contrast, "_", Sys.Date(), ".", input$heatmap_export_fmt),
      content  = function(file) {
        ht <- .heatmap_obj()
        if (input$heatmap_export_fmt == "pdf") {
          pdf(file, width = 9, height = 8)
        } else {
          png(file, width = 9, height = 8, units = "in", res = 300)
        }
        print(ht)
        dev.off()
      }
    )

    # ── DE Table ──────────────────────────────────────────────────────────────
    output$table_de <- renderDT({
      req(active_de_results())
      build_de_results_dt(active_de_results())
    })

    output$dl_de_csv <- downloadHandler(
      filename = function() paste0("DE_", shared_rv$active_contrast, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(active_de_results()); write.csv(active_de_results(), file, row.names = FALSE) }
    )
    output$dl_de_excel <- downloadHandler(
      filename = function() paste0("DE_", shared_rv$active_contrast, "_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(active_de_results())
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(active_de_results(), file)
        } else {
          write.csv(active_de_results(), file, row.names = FALSE)
        }
      }
    )

  }) # /moduleServer
}
