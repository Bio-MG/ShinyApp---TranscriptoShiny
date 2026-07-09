# =============================================================================
# mod_bulk_de_venn.R — Bulk Child 2: Venn/UpSet ACROSS CONTRASTS tab
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Compares significant-gene sets BETWEEN contrasts (e.g. after a pairwise-auto
# run). Distinct from mod_bulk_de_multimethod.R, which compares gene sets
# ACROSS STATISTICAL METHODS for the SAME contrast.
#
# Gene sets are recomputed LIVE whenever input$lfc_thresh/padj_thresh change,
# even with no new DE calculation, so the comparison always reflects the
# CURRENT thresholds rather than whatever was active when each contrast was
# originally fitted.
#
# Depends on helpers_bulk.R: build_contrast_gene_sets(), plot_upset_contrasts(),
#   plot_venn_contrasts(), build_contrast_intersection_dt().
# =============================================================================

.de_venn_server <- function(input, output, session, ns, global_data, shared_rv) {

  # Populate the contrast picker whenever shared_rv$contrasts changes
  # (new single-pair run, pairwise-auto batch, etc.) — default selection:
  # all of them, capped at 6 for readability.
  observeEvent(shared_rv$contrasts, {
    nm <- names(shared_rv$contrasts)
    updateSelectizeInput(session, "venn_contrasts", choices = nm, selected = head(nm, 6), server = TRUE)
  })

  output$venn_gate_message <- renderUI({
    n <- length(input$venn_contrasts)
    if (length(shared_rv$contrasts) < 2) {
      div(class = "alert alert-warning", style = "font-size:0.85em;",
          "Lancez au moins 2 contrastes (Step 2 simple répété, ou ", tags$em("Pairwise auto"),
          ") pour pouvoir les comparer ici.")
    } else if (n < 2) {
      div(class = "alert alert-info", style = "font-size:0.85em;", "Sélectionnez au moins 2 contrastes ci-dessus.")
    } else if (input$venn_type == "venn" && (n < 2 || n > 4)) {
      div(class = "alert alert-warning", style = "font-size:0.85em;",
          "Le diagramme de Venn n'est lisible que pour 2 à 4 contrastes (vous en avez ", n,
          ") — passez en UpSet ou réduisez la sélection.")
    } else {
      NULL
    }
  })

  # LIVE recompute: re-runs whenever input$lfc_thresh / input$padj_thresh
  # change, even with NO new DE calculation — sets must reflect the CURRENT
  # threshold, not whatever it was when each contrast was originally fitted.
  venn_gene_sets <- reactive({
    req(length(input$venn_contrasts) >= 2)
    contrasts_sel <- shared_rv$contrasts[input$venn_contrasts]
    contrasts_sel <- contrasts_sel[!vapply(contrasts_sel, is.null, logical(1))]
    req(length(contrasts_sel) >= 2)
    build_contrast_gene_sets(
      contrasts_sel,
      lfc_thresh      = input$lfc_thresh,
      padj_thresh     = input$padj_thresh,
      direction_aware = isTRUE(input$venn_direction_aware)
    )
  })

  output$venn_plot <- renderPlot({
    # Soft pre-check only — if the clientData width/height ever resolves to
    # something genuinely tiny, show the friendly message proactively
    # instead of attempting to plot. isTRUE() makes this safe: if the key
    # doesn't resolve the way we expect (NULL), the comparison is simply
    # not TRUE and we fall through to the normal flow below — this can
    # never block rendering, only skip straight to a message early.
    w <- session$clientData[[paste0("output_", "venn_plot", "_width")]]
    h <- session$clientData[[paste0("output_", "venn_plot", "_height")]]
    if (isTRUE(w < 30) || isTRUE(h < 30)) {
      grid::grid.newpage()
      grid::grid.text("Conteneur trop petit pour afficher le diagramme.\nAgrandissez la fenêtre ou l'onglet.",
                      gp = grid::gpar(col = "grey40", fontsize = 12))
      return(invisible(NULL))
    }

    sets <- tryCatch(venn_gene_sets(), error = function(e) NULL)
    validate(need(!is.null(sets), "Sélectionnez au moins 2 contrastes."))
    tryCatch({
      if (input$venn_type == "venn") {
        plot_venn_contrasts(sets)
      } else {
        plot_upset_contrasts(sets)
      }
    }, error = function(e) {
      # grid-based, NOT plot.new() — survives even a near-zero device, so
      # this fallback itself can no longer fail the way it used to (was
      # the actual source of the duplicate "figure margins too large").
      grid::grid.newpage()
      grid::grid.text(paste0("Erreur : ", conditionMessage(e), "\n(ou conteneur trop petit — agrandissez la fenêtre/onglet)"),
                      gp = grid::gpar(col = "firebrick", fontsize = 11))
    })
  })

  output$venn_intersection_table <- renderDT({
    sets <- tryCatch(venn_gene_sets(), error = function(e) NULL)
    validate(need(!is.null(sets), "Sélectionnez au moins 2 contrastes avec des gènes significatifs."))
    dt <- tryCatch(build_contrast_intersection_dt(sets), error = function(e) NULL)
    validate(need(!is.null(dt) && nrow(dt) > 0,
                  "Aucun gène dans les intersections avec les seuils actuels."))
    datatable(dt, filter = "top", rownames = FALSE,
             options = list(pageLength = 15, scrollX = TRUE))
  })

  output$dl_venn_png <- downloadHandler(
    filename = function() paste0("venn_upset_", Sys.Date(), ".png"),
    content  = function(file) {
      sets <- venn_gene_sets()
      png(file, width = 9, height = 7, units = "in", res = 300)
      if (input$venn_type == "venn") plot_venn_contrasts(sets) else plot_upset_contrasts(sets)
      dev.off()
    }
  )

  output$dl_venn_genes_csv <- downloadHandler(
    filename = function() paste0("genes_par_intersection_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(build_contrast_intersection_dt(venn_gene_sets()), file, row.names = FALSE)
    }
  )
}
