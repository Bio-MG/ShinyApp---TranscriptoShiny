# =============================================================================
# mod_bulk_de_pairwise.R — Bulk Child 2: pairwise-auto DE (>2 levels)
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# When condition_col has more than 2 levels, computes EVERY pairwise contrast
# in one click: a single DESeq2 fit is reused across all pairs (results()/
# lfcShrink() only, no refit per pair); edgeR/limma-voom refit per pair since
# run_bulk_de_dispatch()'s edgeR/limma helpers are 2-group only.
#
# Confirmation modal gate (>10 pairs) to avoid an accidental very long run.
#
# Depends on helpers_bulk.R: check_design_confounding(), build_dds(),
#   run_bulk_de_dispatch(), .normalize_de_cols().
# Depends on: helpers$design_str(), helpers$register_contrast() —
#   see mod_bulk_de_engine.R::.de_make_helpers().
# =============================================================================

.de_pairwise_server <- function(input, output, session, ns, global_data, shared_rv, helpers) {

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
          build_dds(shared_rv$filtered_counts, meta, design_formula = helpers$design_str(), run_deseq = TRUE),
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

        if (!is.null(res)) { helpers$register_contrast(name, res); ok <- ok + 1 }
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

    # Same two hard blocks as the single-pair path (confounding, single-
    # level covariate) — the pairwise path fits ONE shared dds_full reused
    # for every pair, so a bad design here silently corrupts ALL contrasts
    # at once rather than just one. Was previously missing here entirely.
    covariates_in_use <- input$covariates %||% character(0)
    confounded <- Filter(function(cov) check_design_confounding(meta, input$condition_col, cov),
                         covariates_in_use)
    if (length(confounded) > 0) {
      showNotification(
        sprintf("❌ Covariable(s) confondue(s) avec '%s' : %s. Retirez-la(les) du design ou revoyez votre plan d'expérience.",
                input$condition_col, paste(confounded, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }
    single_level <- Filter(function(cov) length(unique(na.omit(meta[[cov]]))) < 2,
                           covariates_in_use)
    if (length(single_level) > 0) {
      showNotification(
        sprintf("❌ Covariable(s) à une seule modalité : %s. Elle(s) n'apporterai(en)t aucune information — retirez-la(les) du design.",
                paste(single_level, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }

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
}
