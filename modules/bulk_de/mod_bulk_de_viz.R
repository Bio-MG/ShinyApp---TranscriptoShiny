# =============================================================================
# mod_bulk_de_viz.R — Bulk Child 2: Volcano / MA-Plot / Heatmap / Table DE
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Owns the 4 tabs that visualise the CURRENTLY ACTIVE contrast:
#   - active_de_results(): reactive accessor for shared_rv$contrasts[[active]]
#   - sync_warning: desync banner shown on BOTH Volcano and Heatmap tabs,
#     detecting when the active contrast's gene set no longer matches the
#     CURRENT filtered_counts (e.g. Step 1 was re-run after this DE pass)
#   - Volcano (static ggplot + optional interactive plotly)
#   - MA-Plot (static ggplot + optional interactive plotly)
#   - Heatmap (ComplexHeatmap, dedicated PNG/PDF export — not ggsave-able)
#   - Table DE (DT + CSV/Excel export)
#
# Also mirrors shared_rv$volcano_role_colors (Step-3.0), read by
# mod_bulk_report.R / the exported R script, since it owns volcano_role_colors().
#
# Depends on helpers_bulk.R: plot_volcano_bulk(), plot_ma_bulk(),
#   plot_heatmap_bulk(), build_de_results_dt(), bulk_role_colors(),
#   manual_color_picker_ui(), .default_manual_colors().
# =============================================================================

.de_viz_server <- function(input, output, session, ns, global_data, shared_rv) {

  # Reactive accessor for the currently displayed DE result (local to this
  # function — sibling functions read shared_rv$contrasts[[shared_rv$active_contrast]]
  # directly since they don't need req()-based reactive semantics here)
  active_de_results <- reactive({
    req(shared_rv$active_contrast, shared_rv$contrasts[[shared_rv$active_contrast]])
    shared_rv$contrasts[[shared_rv$active_contrast]]
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
  # Up/Down/NS are FIXED semantic roles (not an arbitrary N-level grouping),
  # so this reuses bulk_role_colors() rather than the per-level picker used
  # for PCA/Heatmap/QC — same "Manuel" MODE (shared_rv$bulk_palette, set in
  # the Step 1 sidebar), but only 3 swatches regardless of dataset.
  output$volcano_manual_palette_ui <- renderUI({
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    div(
      class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
      h6("Couleurs manuelles — Up / Down / Non-significatif",
         style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
      manual_color_picker_ui(ns, c("role_color_up", "role_color_down", "role_color_ns"),
                             c("Up-régulé", "Down-régulé", "Non-significatif"),
                             c("#E74C3C", "#2980B9", "#BDC3C7"))
    )
  })

  volcano_role_colors <- reactive({
    pal <- shared_rv$bulk_palette %||% "default"
    manual_vec <- if (identical(pal, "manual")) {
      c(Up = input$role_color_up %||% "#E74C3C",
        Down = input$role_color_down %||% "#2980B9",
        NS = input$role_color_ns %||% "#BDC3C7")
    } else NULL
    bulk_role_colors(pal, manual_vec)
  })

  # Step-3.0: mirror resolved role colors → used by report Rmd and R script
  observe({
    shared_rv$volcano_role_colors <- tryCatch(volcano_role_colors(), error = function(e) NULL)
  })

  volcano_plot <- reactive({
    req(active_de_results())
    rc <- volcano_role_colors()
    plot_volcano_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh,
                      up_color = rc[["Up"]], down_color = rc[["Down"]], ns_color = rc[["NS"]])
  })
  output$plot_volcano <- renderPlot({ volcano_plot() })

  output$volcano_container <- renderUI({
    if (isTRUE(input$volcano_interactive)) plotlyOutput(ns("plot_volcano_ly"), height = "650px")
    else plotOutput(ns("plot_volcano"), height = "650px")
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
    color_map <- volcano_role_colors()

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
              line = list(dash = "dot", color = unname(color_map[["Up"]]), width = 1)),
          list(type = "line", x0 = -lfc, x1 = -lfc, y0 = 0, y1 = 1, yref = "paper",
              line = list(dash = "dot", color = unname(color_map[["Down"]]), width = 1)),
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
  # Reuses the SAME role-color picker as Volcano (Up = "significant" here,
  # NS unchanged) — one fewer control to keep in sync, see helpText on the
  # MA-Plot tab pointing back to Volcano.
  ma_plot <- reactive({
    req(active_de_results())
    rc <- volcano_role_colors()
    plot_ma_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh,
                sig_color = rc[["Up"]], ns_color = rc[["NS"]])
  })
  output$plot_ma <- renderPlot({ ma_plot() })

  output$ma_container <- renderUI({
    if (isTRUE(input$ma_interactive)) plotlyOutput(ns("plot_ma_ly"), height = "650px")
    else plotOutput(ns("plot_ma"), height = "650px")
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

    # ── Directional subset (BingleSeq-style: Tous/Sig/Up/Down/Non-sig) ────
    # Applied BEFORE ranking by p-adj, on the CURRENT lfc/padj thresholds —
    # consistent with Volcano/MA-plot which already read the same inputs.
    dir_choice <- input$heatmap_direction %||% "all"
    lfc <- input$lfc_thresh; pval <- input$padj_thresh
    is_sig <- !is.na(res$padj) & res$padj < pval & abs(res$log2FoldChange) > lfc
    res <- switch(dir_choice,
      sig  = res[is_sig, , drop = FALSE],
      up   = res[is_sig & res$log2FoldChange > 0, , drop = FALSE],
      down = res[is_sig & res$log2FoldChange < 0, , drop = FALSE],
      ns   = res[!is_sig, , drop = FALSE],
      res  # "all" — unchanged, original behaviour
    )
    validate(need(nrow(res) > 0,
                  "Aucun gène dans ce sous-ensemble (Up/Down/Sig/Non-sig) avec les seuils actuels."))

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

  # ── Manual palette: own picker, keyed to heatmap_annot's levels — kept
  #    SEPARATE from PCA/QC pickers since heatmap_annot may point to yet
  #    another metadata column. Same shared_rv$bulk_palette MODE (Step 1
  #    sidebar) decides whether "Manuel" is active app-wide.
  manual_heatmap_levels <- reactive({
    req(global_data$bulk_obj$metadata, input$heatmap_annot)
    req(nzchar(input$heatmap_annot))
    lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$heatmap_annot]]))))
    req(length(lvls) > 0)
    lvls
  })

  output$heatmap_manual_palette_ui <- renderUI({
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    if (!nzchar(input$heatmap_annot %||% "")) {
      return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                 "Sélectionnez d'abord une \"Annotation colonnes\" pour personnaliser ses couleurs."))
    }
    lvls <- tryCatch(manual_heatmap_levels(), error = function(e) character(0))
    if (length(lvls) == 0) return(NULL)
    ids <- paste0("heatmap_manual_color_", seq_along(lvls))
    div(
      class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
      h6(paste("Couleurs manuelles —", input$heatmap_annot),
         style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
      manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
    )
  })

  heatmap_manual_colors <- reactive({
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    lvls <- tryCatch(manual_heatmap_levels(), error = function(e) character(0))
    if (length(lvls) == 0) return(NULL)
    defaults <- .default_manual_colors(length(lvls))
    vals <- vapply(seq_along(lvls), function(i) {
      v <- input[[paste0("heatmap_manual_color_", i)]]
      if (is.null(v) || !nzchar(v)) defaults[i] else v
    }, character(1))
    setNames(vals, lvls)
  })

  .heatmap_obj <- function() {
    annot <- if (nzchar(input$heatmap_annot %||% "")) input$heatmap_annot else NULL
    pal   <- shared_rv$bulk_palette %||% "default"
    plot_heatmap_bulk(shared_rv$vst_mat, heatmap_genes(), global_data$bulk_obj$metadata,
                      annotation_col = annot, palette = pal,
                      manual_colors = if (identical(pal, "manual")) heatmap_manual_colors() else NULL)
  }

  output$plot_heatmap <- renderPlot({ .heatmap_obj() })

  output$dl_heatmap <- downloadHandler(
    filename = function() paste0("heatmap_", shared_rv$active_contrast, "_", Sys.Date(), ".", input$heatmap_export_fmt),
    content  = function(file) {
      if (input$heatmap_export_fmt == "pdf") {
        pdf(file, width = 9, height = 8)
      } else {
        png(file, width = 9, height = 8, units = "in", res = 300)
      }
      .heatmap_obj()  # draws as a side effect on the device just opened above
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
}
