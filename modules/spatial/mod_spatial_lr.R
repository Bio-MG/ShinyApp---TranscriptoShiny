# =============================================================================
# modules/spatial/mod_spatial_lr.R — Ligand-Receptor paires (B5 core)
# =============================================================================
# v1 (vague 8 — backlog, separate session): module inspire de mod_spatial_niche.R
#    mais pour les paires ligand-recepteur (CellTalkDB/CellPhoneDB). Pas de
#    telechargement auto dans ce premier temps (fichier statique plus tard) :
#    l'utilisateur fournit les gènes LR via lr_pairs_db.
#
# Pattern ExtendedTask/mirai identique aux sous-onglets B1/B6/B4 de
# mod_spatial_niche.R et au pipeline de deconvolution/mod_spatial_cluster.R.
#
# Input: a curated LR pairs file (ligand, receptor, pathway/context).
# Output: per-spot mean LR score + pair-level z-scores/significance (optionnel).
#
# Sidebar: expression source = global_data$spatial_obj$sketch; k_neighbors; n_perm.
# =============================================================================

mod_spatial_lr_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("Ligand-Recepteur (CellTalkDB-lite)"), width = 380,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
           bsicons::bs_icon("info-circle"),
           i18n$t(" Evalue la co-occurrence spatiale de paires ligand-récepteur basée sur l'expression gène dans les voisinages (BANKSY-lite). ")),

      div(class = "alert alert-warning", style = "font-size:0.7rem;",
           bsicons::bs_icon("exclamation-triangle"),
           i18n$t(" Les paires LR doivent etre fournies via lr_pairs_db (fichier CSV ). Le telechargement auto de CellTalkDB/CellPhoneDB n'est pas supporte dans ce module.")),

      numericInput(ns("k_neighbors"), i18n$t("Voisins spatiaux (neighbors.k)"), 30, min = 5, max = 200, step = 5),
      numericInput(ns("n_perm"), i18n$t("Permutations (enveloppe nulle)"), 100, min = 10, max = 500, step = 10),

      bslib::input_task_button(ns("btn_lr"), i18n$t("Calculer les scores LR"),
                                icon = icon("dna")),
      verbatimTextOutput(ns("lr_progress_text"), placeholder = TRUE),

      hr(),
      h6(i18n$t("Top paires par spot"), style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
           bsicons::bs_icon("list-ul"),
           i18n$t("Tableau des top-20 paires ligand-récepteur par spot, trie par |z_score|.")),

      hr(),
      h6(i18n$t("Carte spatiale de signification"), style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
           bsicons::bs_icon("map-marker-alt"),
           i18n$t("Visualisez la distribution des paires selectionnees (Hotspot/Coldspot/NS) sur le fond histologique.")),

      uiOutput(ns("slot_spot_selector")),
      uiOutput(ns("slot_pair_selector"))
    ),

    navset_card_underline(
      nav_panel(i18n$t("Top paires par spot"),
                div(class = "alert alert-light small mb-2", style = "font-size:0.65rem;",
                     i18n$t("Les paires sont triees par |z_score|. Les lignes avec NS ne sont pas considerees ")),
                card(full_screen = TRUE, plotOutput(ns("lr_spot_map"), height = "500px")),
                DT::DTOutput(ns("lr_pairs_table"))),

      nav_panel(i18n$t("Repartition des paires par spot"),
                div(class = "alert alert-light small mb-2", style = "font-size:0.68rem;",
                     i18n$t("Distribution des Hotspot/Coldspot/NS par spot.")),
                card(full_screen = TRUE, plotOutput(ns("lr_signif_hist"), height = "450px")))
    )
  )
}

mod_spatial_lr_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Session-scoped scalar translation (plain strings, never HTML spans).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # i18n: push translated labels for build-time-frozen inputs on every
    # language change (values NEVER change; selection is preserved).
    observeEvent(global_data$language, {
      updateNumericInput(session, "k_neighbors", label = .tr("Voisins spatiaux (neighbors.k)"))
      updateNumericInput(session, "n_perm", label = .tr("Permutations (enveloppe nulle)"))
    }, ignoreInit = TRUE)

    log_file <- spatial_log_path(session, "lr")
    tracker  <- create_reactive_tracker(session, log_file)

    # ── UI slots for spot/pair selection (post-calculation) ────────────────
    output$slot_spot_selector <- renderUI({
      global_data$language  # re-render on language switch
      req(shared_rv$lr_result)
      spots <- unique(shared_rv$lr_result$spot_scores$from_spot)
      if (!length(spots)) return(NULL)
      tags$span(.tr("Selectionnez un spot (pour la carte en dessous, si disponible):"))
    })

    output$slot_pair_selector <- renderUI({
      global_data$language  # re-render on language switch
      req(shared_rv$lr_result)
      # Unique LR pairs sorted by |z_score| globally
      if (!is.null(shared_rv$lr_result$pair_scores) && nrow(shared_rv$lr_result$pair_scores) > 0) {
        top_pairs <- shared_rv$lr_result$pair_scores[order(-abs(shared_rv$lr_result$pair_scores$z_score)), ]
        top_pairs <- head(top_pairs, 100L)
        selectInput(ns("slot_lr_pair"), .tr("Selecter une paire ligand-récepteur"),
                    choices = paste0(shared_rv$lr_result$pair_scores$ligand, "-",
                                     shared_rv$lr_result$pair_scores$receptor),
                    selected = NULL)
      }
    })

    # ── ExtendedTask for spatial_lr_score ───────────────────────────────────
    lr_task <- ExtendedTask$new(function(coords, expr_mat, lr_pairs, k_neighbors, n_perm, log_file) {
      mirai::mirai(
        {
          spatial_lr_score(
            coords = coords,
            expr_mat = expr_mat,
            ligands = lr_pairs$ligand,
            receptors = lr_pairs$receptor,
            k_neighbors = k_neighbors,
            n_perm = n_perm,
            log_file = log_file
          )
        },
        coords = coords, expr_mat = expr_mat, lr_pairs = lr_pairs,
        k_neighbors = k_neighbors, n_perm = n_perm, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(lr_task, "btn_lr")

    observeEvent(input$btn_lr, {
      req(global_data$spatial_obj$coords, lr_pairs_db)

      # Filter genes to those in the sketch
      genes <- unique(c(lr_pairs_db$ligand, lr_pairs_db$receptor))
      sk <- global_data$spatial_obj$sketch
      keep <- genes[genes %in% rownames(sk)]
      expr_mat <- SeuratObject::LayerData(sk, layer = "data")[keep, , drop = FALSE]

      # Handle case where no overlap (return early)
      if (!length(keep)) {
        shared_rv$lr_result <- NULL
        shared_rv$lr_params <- list(expr_source = "none_found", genes_used = keep,
                                     k_neighbors = input$k_neighbors, n_perm = input$n_perm)
        showNotification(.tr("Aucun des gènes ligand/récepteur trouvés dans l'échantillon."),
                          type = "warning", duration = 6)
        return()
      }

      reset_log(log_file)
      # v1: store params as mirror of what was used (not for auto-pipeline yet)
      shared_rv$lr_params <- list(expr_source = "sketch_data", genes_used = keep,
                                   k_neighbors = input$k_neighbors, n_perm = input$n_perm)

      lr_task$invoke(
        coords       = global_data$spatial_obj$coords,
        expr_mat     = expr_mat,
        lr_pairs     = lr_pairs_db,
        k_neighbors  = input$k_neighbors,
        n_perm       = input$n_perm,
        log_file     = log_file
      )
    })

    observeEvent(lr_task$status(), {
      if (lr_task$status() == "success") {
        res <- lr_task$result()
        shared_rv$lr_result <- res
        showNotification(.t_fmt(.tr("Scores LR calcules : {pairs} lignes de scores pairs; {spots} spots avec voisins."),
                                pairs = nrow(res$pair_scores),
                                spots = nrow(res$spot_scores) - ifelse(is.null(res$skipped), 0L, as.integer(res$skipped))))
      } else if (lr_task$status() == "error") {
        showNotification(
          .tr("Erreur pendant le calcul des scores LR — voir le log. Essayez 'Reinitialiser les daemons' puis relancez."),
          type = "error", duration = 10)
      }
    })

    output$lr_progress_text <- renderText({
      global_data$language  # re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })

    # ── Results UI: Top pairs table (by |z_score|), spot map, hist ──────────
    output$lr_pairs_table <- DT::renderDT({
      req(shared_rv$lr_result)
      df <- shared_rv$lr_result$pair_scores
      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(
          from_spot = character(), to_spot = character(), ligand = character(), receptor = character(),
          score = numeric(0), z_score = numeric(0), p_value = numeric(0), signif = character()
        ), rownames = FALSE, options = list(pageLength = 20)))
      }

      # Add composite pair name (ligand-receptor) and sort by |z_score|
      df$pair_name <- paste0(df$ligand, "-", df$receptor)
      df_sorted <- df[order(-abs(df$z_score)), ]
      head_tbl <- head(df_sorted[, c("from_spot", "to_spot", "pair_name", "score", "z_score", "p_value", "signif")], 50L)

      DT::datatable(head_tbl, rownames = FALSE,
                    options = list(pageLength = 20, scrollX = TRUE)) |>
        DT::formatColumn("z_score", inline = "cell", format = function(v) sprintf("%.2f", v),
                         class = ifelse(abs(v) > 1.96, "text-danger fw-bold",
                                    ifelse(abs(v) < -1.96, "text-info fw-bold", "muted"))) |>
        DT::formatColumn("signif", list(list(targets = 7), bsicons::bs_icon(x, class = paste("align-middle", x))))
    })

    output$lr_spot_map <- renderPlot({
      global_data$language  # re-render on language switch
      req(shared_rv$lr_result)
      # Only if spot_scores has valid data from a selected pair
      req(input$slot_lr_pair %in% shared_rv$lr_result$pair_scores[, "pair_name"])

      sp <- shared_rv$lr_result$spot_scores
      # Filter to only the selected pair (if possible - for simplicity, plot overall signif)
      # For now, map the mean score or classify by z_score threshold from spot summary
      if (!is.null(sp) && nrow(sp) > 0) {
        # Use 'mean_lr_score' as primary; add 'signif' column in table above
        df_plot <- sp[, c("spot_id", "mean_lr_score")]
        long_df <- reshape2::melt(df_plot, id.vars = "spot_id", variable.name = NULL, value.name = "score")
        # For mapping: use score directly or map by z if we had per-spot z
        ggplot2::ggplot(long_df, ggplot2::aes(x = score)) +
          ggplot2::geom_histogram(bins = 30, fill = shared_rv$color_palette %||% "default", color = "white") +
          spatial_continuous_scale(shared_rv, fill) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::labs(title = .tr("Repartition des scores LR moyens par spot"), x = .tr("Moyenne score LR"), y = .tr("Count")) +
          ggplot2::labs(subtitle = sprintf(.tr("k=%d, %d permutations: shared_rv$lr_result$spot_scores"),
                                          input$k_neighbors, input$n_perm))
      } else {
        ggplot2::ggplot() +
          ggplot2::labs(title = .tr("Aucune donnée de scores LR disponible"), subtitle = .tr("Calculez d'abord les scores (onglet gauche)")) +
          ggplot2::theme_minimal()
      }
    })

    output$lr_signif_hist <- renderPlot({
      global_data$language  # re-render on language switch
      req(shared_rv$lr_result)
      req(!is.null(shared_rv$lr_result$pair_scores) && nrow(shared_rv$lr_result$pair_scores) > 0)

      df <- shared_rv$lr_result$pair_scores
      signif_counts <- table(df$signif)
      if (!length(signif_counts)) {
        return(ggplot2::ggplot() + ggplot2::labs(title = .tr("Aucune donnée")) + ggplot2::theme_minimal())
      }

      ggplot2::ggplot(as.data.frame(signif_counts), ggplot2::aes(x = reorder(name, count), y = count, fill = name)) +
        ggplot2::geom_bar(stat = "identity", width = 0.7) +
        ggplot2::scale_fill_identity(
          labels = c("NS" = .tr("NS"), "Hotspot" = .tr("Hotspot (chaud)"), "Coldspot" = .tr("Coldspot (froid)")),
          names = c("#999999", setNames(shared_rv$manual_gradient[["high"]], ifelse("Hotspot" %in% names(signif_counts), 1L, NA)),
                            setNames(shared_rv$manual_gradient[["low"]], ifelse("Coldspot" %in% names(signif_counts), 1L, NA)))) +
        spatial_continuous_scale(shared_rv, fill) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = .tr("Repartition Hotspot/Coldspot/NS des paires LR"), x = .tr("Signification"), y = .tr("Nombre"))
    })
  })
}
