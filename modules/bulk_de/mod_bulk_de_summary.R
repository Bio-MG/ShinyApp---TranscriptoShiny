# =============================================================================
# mod_bulk_de_summary.R — Bulk Child 2: Resume Up/Down tab
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Barchart + table of significant Up/Down genes across ALL computed
# contrasts. Reuses the same helper as the HTML/PDF report
# (summarize_contrasts_updown(), helpers_bulk.R) — single source of truth —
# and recomputes LIVE on input$lfc_thresh/padj_thresh, with no DE recalculation.
# =============================================================================

.de_summary_server <- function(input, output, session, ns, global_data, shared_rv) {

  updown_summary <- reactive({
    req(length(shared_rv$contrasts) > 0)
    summarize_contrasts_updown(shared_rv$contrasts, lfc_thresh = input$lfc_thresh,
                               padj_thresh = input$padj_thresh,
                               active_contrast = shared_rv$active_contrast)
  })

  updown_plot <- reactive({ plot_updown_barchart(updown_summary()) })

  output$plot_updown <- renderPlot({
    validate(need(length(shared_rv$contrasts) > 0,
                  "Aucun contraste calculé — lancez l'Étape 2 (DE) d'abord."))
    updown_plot()
  })

  output$dl_updown_png <- downloadHandler(
    filename = function() paste0("updown_summary_", Sys.Date(), ".png"),
    content  = function(file) ggsave(file, plot = updown_plot(), width = 8, height = 5.5, dpi = 300)
  )

  output$table_updown <- renderDT({
    req(updown_summary())
    df <- updown_summary()
    df$Actif <- ifelse(df$actif, "→ actif", "")
    df$actif <- NULL
    colnames(df) <- c("Contraste", "Gènes testés", "Significatifs", "Up", "Down", "Actif")
    datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$dl_updown_csv <- downloadHandler(
    filename = function() paste0("updown_summary_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(updown_summary(), file, row.names = FALSE)
  )
}
