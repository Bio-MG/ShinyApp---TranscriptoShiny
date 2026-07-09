# =============================================================================
# mod_bulk_de_run.R — Bulk Child 2: single-pair DE + ad-hoc contrast
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Owns:
#   - Step 2 "Lancer l'Analyse Différentielle" (single Cible vs Référence pair)
#   - Contraste Ad-hoc (BingleSeq pattern): manual Group A/B sample selection,
#     bypasses condition_col entirely with a synthetic 2-level metadata.
#
# Both hard-block confounded/single-level covariates BEFORE fitting (see
# helpers_bulk.R: check_design_confounding(), validate_bulk_design()) —
# DESeq2 would otherwise fail deep inside DESeq() with a cryptic linear-
# algebra error, or silently ignore a single-level covariate without warning.
#
# Depends on helpers_bulk.R: check_design_confounding(), build_dds(),
#   run_bulk_de_dispatch(), .normalize_de_cols().
# Depends on: helpers$design_str(), helpers$register_contrast() —
#   see mod_bulk_de_engine.R::.de_make_helpers().
# =============================================================================

.de_run_server <- function(input, output, session, ns, global_data, shared_rv, helpers) {

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

    # HARD BLOCK: a single-level covariate contributes nothing to the
    # model — R's contrast coding produces zero columns for it, so DESeq2
    # would silently fit ~ condition_col alone while the user believes
    # they are also correcting for this covariate. No crash, no warning
    # from DESeq2 itself — catch it explicitly instead of letting the
    # analysis "succeed" on the wrong design.
    single_level <- Filter(
      function(cov) length(unique(na.omit(meta[[cov]]))) < 2,
      covariates_in_use
    )
    if (length(single_level) > 0) {
      showNotification(
        sprintf("❌ Covariable(s) à une seule modalité : %s. Elle(s) n'apporterai(en)t aucune information — retirez-la(les) du design.",
                paste(single_level, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = "Analyse différentielle...", value = 0.2)

    tryCatch({
      design_str <- helpers$design_str()

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

      helpers$register_contrast(contrast_name, res)
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
  # CONTRASTE AD-HOC (BingleSeq pattern) — manual Group A/B sample selection
  # =========================================================================
  observeEvent(shared_rv$filtered_counts, {
    req(shared_rv$filtered_counts)
    samples <- colnames(shared_rv$filtered_counts)
    updateCheckboxGroupInput(session, "adhoc_group_a", choices = samples, selected = character(0))
    updateCheckboxGroupInput(session, "adhoc_group_b", choices = samples, selected = character(0))
  })

  output$adhoc_readiness <- renderUI({
    a <- input$adhoc_group_a %||% character(0)
    b <- input$adhoc_group_b %||% character(0)
    issues <- character(0)
    if (length(intersect(a, b)) > 0) issues <- c(issues, "Échantillon(s) présent(s) dans les 2 groupes.")
    if (length(a) == 0 || length(b) == 0) issues <- c(issues, "Sélectionnez au moins 1 échantillon / groupe.")
    else if (length(a) < 2 || length(b) < 2) issues <- c(issues, "Un groupe a < 2 réplicats — résultats peu fiables.")
    if (length(issues) == 0) return(NULL)
    div(class = "alert alert-warning", style = "font-size:0.78em;padding:4px 8px;",
        lapply(issues, tags$div))
  })

  observeEvent(input$run_de_adhoc, {
    req(shared_rv$filtered_counts, input$de_engine)
    a <- input$adhoc_group_a %||% character(0)
    b <- input$adhoc_group_b %||% character(0)
    if (length(intersect(a, b)) > 0) { showNotification("❌ Même échantillon dans les 2 groupes.", type = "error", duration = 6); return() }
    if (length(a) == 0 || length(b) == 0) { showNotification("❌ Sélectionnez au moins 1 échantillon / groupe.", type = "error", duration = 6); return() }

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = "Analyse ad-hoc...", value = 0.2)
    tryCatch({
      counts_sub <- shared_rv$filtered_counts[, c(a, b), drop = FALSE]
      meta_adhoc <- data.frame(
        condition = factor(c(rep("GroupA", length(a)), rep("GroupB", length(b))), levels = c("GroupB", "GroupA")),
        row.names = c(a, b)
      )
      res <- if (input$de_engine == "deseq2") {
        dds_a <- build_dds(counts_sub, meta_adhoc, "~condition", run_deseq = TRUE)
        run_bulk_de_dispatch("deseq2", counts_sub, meta_adhoc, "condition", "GroupA", "GroupB", dds = dds_a, shrink = input$shrink_lfc)
      } else {
        run_bulk_de_dispatch(input$de_engine, counts_sub, meta_adhoc, "condition", "GroupA", "GroupB")
      }
      res <- .normalize_de_cols(res, counts_for_basemean = counts_sub)
      cname <- if (nchar(trimws(input$adhoc_contrast_name %||% "")) > 0) trimws(input$adhoc_contrast_name) else "GroupA_vs_GroupB_adhoc"
      helpers$register_contrast(cname, res)
      shared_rv$active_contrast <- cname
      updateSelectInput(session, "active_contrast_view", choices = names(shared_rv$contrasts), selected = cname)
      n_sig <- sum(res$padj < input$padj_thresh & abs(res$log2FoldChange) > input$lfc_thresh, na.rm = TRUE)
      showNotification(sprintf("✓ Ad-hoc '%s': %d gènes sig.", cname, n_sig), type = "message", duration = 6)
    }, error = function(e) showNotification(paste("Erreur DE ad-hoc:", e$message), type = "error", duration = 10))
  })
}
