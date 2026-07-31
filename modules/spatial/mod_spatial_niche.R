# =============================================================================
# modules/spatial/mod_spatial_niche.R — Niches spatiales (BuildNicheAssay-lite)
# =============================================================================
# NEW (Phase 5 — seurat5_spatial_vignette_2.Rmd parity, section "Niche
# analysis" / BuildNicheAssay()). See R/utils_spatial_niche.R for the full
# rationale and the compute_spatial_niches() implementation this module
# wires up.
#
# Input: a categorical group label the user already computed elsewhere in
# the app — either shared_rv$cluster_labels (tab 2, BANKSY-lite) or a
# dominant-cell-type call derived from shared_rv$deconv_props (tab 3,
# RCTD/Label Transfer/STdeconvolve, via dominant_group_labels()). No new
# clustering/deconvolution logic lives here — this tab is purely "what
# surrounds what", one layer on top of results the app already produces.
#
# Output contract: shared_rv$niche_labels is a named character vector
# (id -> "N1".."N<k>"), DELIBERATELY the same shape as shared_rv$cluster_labels
# — this lets tab 4 (mod_spatial_viz.R) treat "niche" as just another
# color_by option, reusing 100% of its existing map/PNG/CSV/ROI machinery
# with a two-line addition there (dropdown entry + plot_df() branch) rather
# than duplicating any plotting code here. This module owns only the
# computation + the composition summary (interpretation aid).
#
# Cheap, async anyway for consistency: only coords + a small named vector
# travel into the mirai daemon (no BPCells/Seurat object needed at all, see
# utils_spatial_niche.R header) — kept on the same ExtendedTask/mirai
# pattern as every other spatial computation in this app (progress log,
# task button, "Reinitialiser les daemons" guidance on error) purely for UI
# consistency, not because it is slow.
# =============================================================================

mod_spatial_niche_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Niches spatiales (BuildNicheAssay-lite)", width = 360,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " Regroupe les spots/cellules selon la COMPOSITION de leur voisinage spatial ",
          "(quels clusters ou types cellulaires sont a proximite), pas selon leur propre ",
          "expression — revele des regions definies par la coexistence de plusieurs ",
          "populations (ex: interface tumeur/stroma, bordure d'un centre germinatif). ",
          "Equivalent de Seurat::BuildNicheAssay()."),

      uiOutput(ns("group_by_ui")),

      numericInput(ns("k_neighbors"), "Voisins spatiaux (neighbors.k)", 30, min = 5, max = 200, step = 5),
      numericInput(ns("n_niches"), "Nombre de niches (niches.k)", 5, min = 2, max = 20, step = 1),

      bslib::input_task_button(ns("btn_niches"), "Calculer les niches",
                                icon = icon("diagram-project")),
      verbatimTextOutput(ns("niche_progress_text"), placeholder = TRUE),

      hr(),
      div(class = "alert alert-light", style = "font-size:0.72rem;",
          bsicons::bs_icon("eye"),
          " Une fois calculees, les niches sont disponibles comme option de coloration ",
          "(\"Niche spatiale\") dans l'onglet \"4. Visualisation\" — fond histologique, ",
          "export PNG/CSV et ROI fonctionnent avec, exactement comme pour un cluster.")
    ),

    navset_card_underline(
      nav_panel("Composition par niche",
                div(class = "alert alert-light small mb-2",
                    "Composition moyenne (proportion de chaque cluster/type cellulaire) au ",
                    "sein du voisinage de chaque niche — utilisez ceci pour interpreter ",
                    "biologiquement chaque niche (ex: \"Niche 2 = frontiere tumeur/immun\")."),
                card(full_screen = TRUE, plotOutput(ns("niche_composition_plot"), height = "420px")),
                DT::DTOutput(ns("niche_composition_table"))),

      nav_panel("Effectifs par niche",
                DT::DTOutput(ns("niche_sizes_table")))
    )
  )
}

mod_spatial_niche_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_file <- spatial_log_path(session, "niche")
    tracker  <- create_reactive_tracker(session, log_file)

    # ── Which categorical grouping can we build niches from? ──────────────
    output$group_by_ui <- renderUI({
      choices <- c()
      if (!is.null(shared_rv$cluster_labels)) choices["Cluster spatial (BANKSY-lite, onglet 2)"] <- "cluster"
      if (!is.null(shared_rv$deconv_props))    choices["Type cellulaire dominant (deconvolution, onglet 3)"] <- "deconv"
      if (length(choices) == 0) {
        return(div(class = "alert alert-warning", style = "font-size:0.8rem;",
                    "Calculez d'abord un clustering (onglet 2) ou une deconvolution (onglet 3) ",
                    "pour disposer d'un regroupement categoriel utilisable comme base des niches."))
      }
      selectInput(ns("group_by"), "Regroupement de base", choices = choices)
    })

    niche_task <- ExtendedTask$new(function(coords, group_labels, k_neighbors, n_niches, log_file) {
      mirai::mirai(
        {
          compute_spatial_niches(coords = coords, group_labels = group_labels,
                                  k_neighbors = k_neighbors, n_niches = n_niches,
                                  log_file = log_file)
        },
        coords = coords, group_labels = group_labels, k_neighbors = k_neighbors,
        n_niches = n_niches, log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(niche_task, "btn_niches")

    observeEvent(input$btn_niches, {
      req(global_data$spatial_obj$coords, input$group_by)

      group_labels <- if (identical(input$group_by, "cluster")) {
        req(shared_rv$cluster_labels)
        shared_rv$cluster_labels
      } else {
        req(shared_rv$deconv_props)
        dominant_group_labels(shared_rv$deconv_props)
      }

      reset_log(log_file)
      niche_task$invoke(
        coords       = global_data$spatial_obj$coords,
        group_labels = group_labels,
        k_neighbors  = input$k_neighbors,
        n_niches     = input$n_niches,
        log_file     = log_file
      )
    })

    observeEvent(niche_task$status(), {
      if (niche_task$status() == "success") {
        res <- niche_task$result()
        shared_rv$niche_labels      <- stats::setNames(res$assignments$niche, res$assignments$id)
        shared_rv$niche_composition <- res$niche_composition
        showNotification(sprintf("Niches calculees : %d niches sur %d elements.",
                                  length(unique(shared_rv$niche_labels)), length(shared_rv$niche_labels)),
                          type = "message", duration = 5)
      } else if (niche_task$status() == "error") {
        showNotification(
          "Erreur pendant le calcul des niches — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez.",
          type = "error", duration = 10)
      }
    })

    output$niche_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    # ── Composition heatmap (niche x groupe, proportion moyenne) ──────────
    output$niche_composition_plot <- renderPlot({
      req(shared_rv$niche_composition)
      df <- shared_rv$niche_composition
      long <- reshape2::melt(df, id.vars = "niche", variable.name = "groupe", value.name = "proportion")
      ggplot2::ggplot(long, ggplot2::aes(x = groupe, y = niche, fill = proportion)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_viridis_c(option = "magma", limits = c(0, 1)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = NULL, y = NULL, fill = "Proportion\nmoyenne")
    })

    output$niche_composition_table <- DT::renderDT({
      req(shared_rv$niche_composition)
      DT::datatable(shared_rv$niche_composition, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound(setdiff(colnames(shared_rv$niche_composition), "niche"), 3)
    })

    output$niche_sizes_table <- DT::renderDT({
      req(shared_rv$niche_labels)
      tab <- as.data.frame(table(niche = shared_rv$niche_labels), stringsAsFactors = FALSE)
      colnames(tab) <- c("Niche", "Effectif")
      DT::datatable(tab, options = list(pageLength = 15), rownames = FALSE)
    })
  })
}
