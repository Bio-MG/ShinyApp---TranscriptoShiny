# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv_outputs.R — Result outputs
# =============================================================================
# Owns: deconv_bar_plot, deconv_table (DT), deconv_coloc_plot.
# All read from shared_rv$deconv_props (written by the deconv task handler
# in the orchestrator).
#
# Plain function called from the orchestrator's moduleServer().
# =============================================================================

.deconv_outputs_server <- function(input, output, session, ns, shared_rv) {

  # discrete_palette_colors_fallback() supprimee (morte) : les plots ci-dessous
  # passent desormais par spatial_discrete_colors()/spatial_diverging_scale()
  # (R/palettes.R), qui suivent la palette globale de l'onglet Visualisation.

  # ── Bar plot (proportions per spot) ─────────────────────────────────────
  output$deconv_bar_plot <- renderPlot({
    req(shared_rv$deconv_props)
    df <- shared_rv$deconv_props
    long <- reshape2::melt(df, id.vars = "id", variable.name = "cell_type", value.name = "proportion")
    ids_show <- utils::head(unique(long$id), 60)
    ggplot2::ggplot(long[long$id %in% ids_show, ],
                     ggplot2::aes(x = id, y = proportion, fill = cell_type)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = spatial_discrete_colors(unique(long$cell_type), shared_rv, kind = "celltype")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 8)) +
      ggplot2::labs(x = "Spots/cellules (echantillon)", y = "Proportion", fill = "Type")
  })

  # ── DT table ────────────────────────────────────────────────────────────
  output$deconv_table <- DT::renderDT({
    req(shared_rv$deconv_props)
    DT::datatable(shared_rv$deconv_props, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      DT::formatRound(setdiff(colnames(shared_rv$deconv_props), "id"), 3)
  })

  # ── Colocalisation heatmap ──────────────────────────────────────────────
  output$deconv_coloc_plot <- renderPlot({
    req(shared_rv$deconv_props)
    df <- shared_rv$deconv_props
    cts <- setdiff(colnames(df), "id")
    validate(need(length(cts) >= 2, "Au moins 2 types cellulaires necessaires."))
    cor_mat <- stats::cor(df[, cts, drop = FALSE], use = "pairwise.complete.obs", method = "pearson")
    long <- reshape2::melt(cor_mat, varnames = c("Type1", "Type2"), value.name = "correlation")
    ggplot2::ggplot(long, ggplot2::aes(x = Type1, y = Type2, fill = correlation)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 3) +
      spatial_diverging_scale(shared_rv, aesthetic = "fill", limits = c(-1, 1)) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(x = NULL, y = NULL, fill = "Correlation\n(Pearson)",
                    title = "Colocalisation des types cellulaires (proportions par spot)")
  })
}