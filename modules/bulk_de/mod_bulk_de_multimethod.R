# =============================================================================
# mod_bulk_de_multimethod.R — Bulk Child 2: multi-method comparison
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Runs the SAME contrast (Cible/Référence from the Step 2 sidebar) through
# DESeq2 + edgeR + limma-voom via getAllDE(), then computes a rank consensus
# via rankConsensus() — both in helpers_bulk.R. Also owns the Venn/UpSet
# comparison ACROSS METHODS (distinct from mod_bulk_de_venn.R, which compares
# ACROSS CONTRASTS for a single method).
#
# Step-3.6 BUG FIX: the pre-refactor version stored results in local
# `reactiveVal()`s (multi_de_rv/consensus_rv) that were NEVER copied to
# shared_rv, so mod_bulk_report.R always received multimethod_de=NULL —
# the report's "Multi-methodes" section could never show real data even
# after a successful comparison. A later, incomplete patch then wrote BOTH
# the local reactiveVal()s AND shared_rv (redundant, with a confusing
# dead-code branch in the rankConsensus error handler). This version keeps
# state in EXACTLY ONE place — shared_rv$multimethod_de /
# shared_rv$multimethod_consensus — read directly by every output below AND
# by mod_bulk_report.R across the module boundary. multimethod_status_rv
# stays as a local reactiveVal since it is pure UI text, not report state.
#
# Depends on helpers_bulk.R: check_design_confounding(), build_dds(),
#   getAllDE(), rankConsensus(), build_contrast_gene_sets(),
#   plot_upset_contrasts(), plot_venn_contrasts().
# Depends on: helpers$design_str() — see mod_bulk_de_engine.R::.de_make_helpers().
#
# State contract (shared_rv):
#   WRITE : shared_rv$multimethod_de         — named list (deseq2/edger/limma)
#           shared_rv$multimethod_consensus  — rankConsensus() data.frame or NULL
#           shared_rv$dds_full               — reused/refit as needed
# =============================================================================

.de_multimethod_server <- function(input, output, session, ns, global_data, shared_rv, helpers) {

  multimethod_status_rv <- reactiveVal(NULL)

  observeEvent(input$run_multimethod, {
    req(shared_rv$filtered_counts, input$condition_col, input$group_ref, input$group_target)

    if (input$group_ref == input$group_target) {
      showNotification("⚠️ Le groupe Référence et le groupe Cible doivent être différents.",
                       type = "warning"); return()
    }
    meta <- global_data$bulk_obj$metadata

    # Same hard blocks as the single-engine run (confounding / single-level
    # covariate) — duplicated check, not factored out, to avoid touching the
    # already-tested single-engine path while adding this new one.
    covariates_in_use <- input$covariates %||% character(0)
    confounded <- Filter(function(cov) check_design_confounding(meta, input$condition_col, cov),
                         covariates_in_use)
    if (length(confounded) > 0) {
      showNotification(sprintf("❌ Covariable(s) confondue(s) avec '%s' : %s.",
                               input$condition_col, paste(confounded, collapse = ", ")),
                       type = "error", duration = 10); return()
    }

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = "Comparaison multi-méthodes...", value = 0.15)

    tryCatch({
      design_str <- helpers$design_str()

      # Reuse the existing dds_full ONLY if it already matches this exact
      # design (cheap heuristic: same design formula string) — otherwise
      # refit. Avoids re-running DESeq() needlessly when the user just
      # clicked "Comparer" right after "Lancer l'Analyse Différentielle"
      # with the deseq2 engine already selected (the common case).
      dds_full <- shared_rv$dds_full
      needs_fit <- is.null(dds_full) ||
        !isTRUE(identical(attr(dds_full, "design_str_cache"), design_str))
      if (needs_fit) {
        p$set(0.3, "Ajustement DESeq2 (requis pour le consensus)...")
        dds_full <- build_dds(shared_rv$filtered_counts, meta, design_formula = design_str, run_deseq = TRUE)
        attr(dds_full, "design_str_cache") <- design_str
        shared_rv$dds_full <- dds_full
      }

      p$set(0.5, "DESeq2 + edgeR + limma-voom...")
      de_list <- getAllDE(shared_rv$filtered_counts, meta, input$condition_col,
                          input$group_target, input$group_ref,
                          dds_full = dds_full, shrink = input$shrink_lfc)

      if (length(de_list) < 2) {
        stop("Au moins 2 méthodes doivent réussir pour comparer (", length(de_list),
            " a réussi). Vérifiez que edgeR/limma sont installés.")
      }

      p$set(0.85, "Consensus de rang...")
      cons <- tryCatch(
        rankConsensus(de_list, input$lfc_thresh, input$padj_thresh),
        error = function(e) { warning(conditionMessage(e)); NULL }
      )

      # Single source of truth — read directly by this module's own outputs
      # below AND by mod_bulk_report.R (across the module boundary).
      shared_rv$multimethod_de        <- de_list
      shared_rv$multimethod_consensus <- cons

      multimethod_status_rv(sprintf(
        "✓ %d méthode(s) comparée(s) : %s (%s vs %s)",
        length(de_list), paste(names(de_list), collapse = ", "),
        input$group_target, input$group_ref
      ))
      showNotification(sprintf("✓ Comparaison multi-méthodes terminée (%s)",
                               paste(names(de_list), collapse = ", ")),
                       type = "message", duration = 6)
      shared_rv$active_tab <- "tab_multimethod"

    }, error = function(e) {
      multimethod_status_rv(NULL)
      shared_rv$multimethod_de        <- NULL
      shared_rv$multimethod_consensus <- NULL
      showNotification(paste("Erreur comparaison multi-méthodes:", e$message),
                       type = "error", duration = 10)
    })
  })

  output$multimethod_status_ui <- renderUI({
    if (is.null(multimethod_status_rv())) {
      div(class = "alert alert-info", style = "font-size:0.85em;",
          "Cliquez \"🔬 Comparer DESeq2 / edgeR / limma-voom\" dans le panneau Step 2.")
    } else {
      div(class = "alert alert-success", style = "font-size:0.85em;", multimethod_status_rv())
    }
  })

  # LIVE recompute on threshold change, same pattern as venn_gene_sets in
  # mod_bulk_de_venn.R.
  mm_gene_sets <- reactive({
    de_list <- shared_rv$multimethod_de
    req(length(de_list) >= 2)
    build_contrast_gene_sets(de_list, lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh)
  })

  output$mm_venn_plot <- renderPlot({
    w <- session$clientData[[paste0("output_", "mm_venn_plot", "_width")]]
    h <- session$clientData[[paste0("output_", "mm_venn_plot", "_height")]]
    if (isTRUE(w < 30) || isTRUE(h < 30)) {
      grid::grid.newpage()
      grid::grid.text("Conteneur trop petit pour afficher le diagramme.",
                      gp = grid::gpar(col = "grey40", fontsize = 12))
      return(invisible(NULL))
    }
    sets <- tryCatch(mm_gene_sets(), error = function(e) NULL)
    validate(need(!is.null(sets), "Lancez d'abord la comparaison multi-méthodes."))
    tryCatch({
      if (input$mm_venn_type == "venn") plot_venn_contrasts(sets) else plot_upset_contrasts(sets)
    }, error = function(e) {
      grid::grid.newpage()
      grid::grid.text(paste0("Erreur : ", conditionMessage(e), "\n(ou conteneur trop petit — agrandissez la fenêtre/onglet)"),
                      gp = grid::gpar(col = "firebrick", fontsize = 11))
    })
  })

  output$dl_mm_venn_png <- downloadHandler(
    filename = function() paste0("venn_methodes_", Sys.Date(), ".png"),
    content  = function(file) {
      sets <- mm_gene_sets()
      png(file, width = 9, height = 7, units = "in", res = 300)
      if (input$mm_venn_type == "venn") plot_venn_contrasts(sets) else plot_upset_contrasts(sets)
      dev.off()
    }
  )

  output$mm_consensus_table <- renderDT({
    df <- shared_rv$multimethod_consensus
    req(df)
    df_display <- df
    num_cols <- setdiff(colnames(df_display), c("gene", "consistent_sign"))
    for (cl in num_cols) df_display[[cl]] <- round(df_display[[cl]], 4)
    datatable(df_display, filter = "top", rownames = FALSE,
             options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("n_methods_sig",
                  background = styleColorBar(range(df_display$n_methods_sig), "#F39C12"),
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$dl_mm_consensus_csv <- downloadHandler(
    filename = function() paste0("consensus_rang_", Sys.Date(), ".csv"),
    content  = function(file) { req(shared_rv$multimethod_consensus); write.csv(shared_rv$multimethod_consensus, file, row.names = FALSE) }
  )
}
