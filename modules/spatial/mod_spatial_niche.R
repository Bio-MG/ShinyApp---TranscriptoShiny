# =============================================================================
# modules/spatial/mod_spatial_niche.R — Niches spatiales (BuildNicheAssay-lite)
# =============================================================================
# v3 (vague 5 — Phase 6 stats, CORRIGE) : deux nouveaux sous-onglets,
#    additifs, zero changement de comportement pour "Composition par niche"/
#    "Effectifs par niche" ci-dessous :
#      - "Enrichissement (co-occurrence)" (B1) — squidpy nhood_enrichment()-
#        like : z-score de co-occurrence spatiale par paire de labels
#        (cluster/type cellulaire dominant/niche), via
#        R/utils_spatial_stats.R::spatial_neighborhood_enrichment().
#      - "Ripley's K (etiquetage aleatoire)" (B6) — pour UN label cible,
#        compare l'agregation spatiale observee a une enveloppe nulle
#        construite par randomisation des labels, via
#        R/utils_spatial_stats.R::ripley_k_random_labeling(). Le flag
#        `subsampled` est toujours affiche dans le titre du graphe (voir
#        handoff — important sur un cluster tres majoritaire).
#    Les DEUX fonctions sont deja pures et preloadees dans les daemons (voir
#    R/utils_spatial_async.R) -- ce module se contente de les appeler
#    directement depuis l'interieur du mirai::mirai(), meme pattern que ce
#    module utilise deja pour `compute_spatial_niches()`.
#    CORRECTIF (cette session) : la 1ere version livree de ce module
#    appelait des noms de fonction INVENTES (spatial_hotspots_gi,
#    spatial_ripley_k, arguments n_perms/target_label...) qui ne
#    correspondaient PAS a R/utils_spatial_stats.R -- fichier confirme par
#    l'utilisateur comme etant le bon (noms reels : n_perm, target_level,
#    ripley_k_random_labeling(), retour $curve/$enrichment/$matrix, etc.).
#    Corrige ici pour appeler EXACTEMENT ces noms/signatures.
#
# v2 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    shared_rv$niche_params ecrit au moment du clic sur "Calculer les
#    niches" (miroir des parametres UI utilises) -- purement additif, lu
#    uniquement par mod_spatial_export.R (script R reproductible) ; zero
#    changement de comportement pour cet onglet.
#
# NEW (Phase 5 — seurat5_spatial_vignette_2.Rmd parity, section "Niche
# analysis", BuildNicheAssay()). See R/utils_spatial_niche.R for the full
# rationale and the compute_spatial_niches() implementation this module
# wires up.
#
# Input: a categorical group label the user already computed elsewhere in
# the app — either shared_rv$cluster_labels (tab 2, BANKSY-lite), a
# dominant-cell-type call derived from shared_rv$deconv_props (tab 3,
# RCTD/Label Transfer/STdeconvolve), or (for the 2 new sub-tabs only)
# shared_rv$niche_labels itself once computed.
#
# Output contract: shared_rv$niche_labels is a named character vector
# (id -> "N1".."N<k>"), DELIBERATELY the same shape as shared_rv$cluster_labels
# — this lets tab 4 (mod_spatial_viz.R) treat "niche" as just another
# color_by option, reusing 100% of its existing map/PNG/CSV/ROI machinery
# with a two-line addition there (dropdown entry + plot_df() branch) rather
# than duplicating any plotting code here. This module owns only the
# computation + the composition summary (interpretation aid).
# =============================================================================

mod_spatial_niche_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Niches spatiales (BuildNicheAssay-lite)", width = 380,

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
          "export PNG/CSV et ROI fonctionnent avec, exactement comme pour un cluster."),

      hr(),
      h6("Enrichissement de voisinage (co-occurrence)", style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          bsicons::bs_icon("hash"),
          " Pour chaque paire de labels (cluster, type cellulaire dominant ou niche), ",
          "compare le nombre de voisinages observes a une enveloppe nulle obtenue en ",
          "melangeant les labels au hasard -- un z-score positif signale une co-occurrence ",
          "spatiale (ex: interface tumeur/stroma), negatif une exclusion mutuelle."),
      uiOutput(ns("group_by_enrich_ui")),
      numericInput(ns("k_neighbors_enrich"), "Voisins spatiaux (k)", 30, min = 5, max = 200, step = 5),
      numericInput(ns("n_perm_enrich"), "Permutations (enveloppe nulle)", 200, min = 50, max = 1000, step = 50),
      bslib::input_task_button(ns("btn_enrichment"), "Calculer l'enrichissement",
                                icon = icon("hashtag")),
      verbatimTextOutput(ns("enrichment_progress_text"), placeholder = TRUE),

      hr(),
      h6("Ripley's K (etiquetage aleatoire)", style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          bsicons::bs_icon("bullseye"),
          " Pour UN label cible, compare son agregation spatiale observee a une enveloppe ",
          "nulle obtenue en reshufflant QUI porte ce label parmi les memes positions ",
          "(etiquetage aleatoire) -- adaptation usuelle en transcriptomique spatiale (le ",
          "tissu entier est la fenetre, pas un processus de Poisson) plutot qu'un vrai ",
          "test CSR/spatstat."),
      uiOutput(ns("group_by_ripley_ui")),
      uiOutput(ns("ripley_target_ui")),
      tags$details(
        tags$summary(style = "cursor:pointer; font-size:0.72rem; color:#666;",
                     "Options avancees (permutations / sous-echantillonnage RAM)"),
        div(class = "mt-2",
            numericInput(ns("n_perm_ripley"), "Permutations (enveloppe nulle)", 199, min = 49, max = 499, step = 10),
            numericInput(ns("max_total_ripley"), "Max elements consideres (RAM)", 6000, min = 1000, max = 20000, step = 500),
            numericInput(ns("max_target_ripley"), "Max points cible testes", 2000, min = 500, max = 10000, step = 500),
            div(class = "text-muted", style = "font-size:0.68rem;",
                "Le calcul des distances par paires est en O(n^2) -- ces plafonds evitent une ",
                "explosion RAM/temps sur un cluster tres majoritaire ou un gros jeu de donnees. ",
                "Le titre du graphe indique si un sous-echantillonnage a ete applique.")
        )
      ),
      bslib::input_task_button(ns("btn_ripley"), "Calculer Ripley's K",
                                icon = icon("chart-line")),
      verbatimTextOutput(ns("ripley_progress_text"), placeholder = TRUE)
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
                DT::DTOutput(ns("niche_sizes_table"))),

      nav_panel("Enrichissement (co-occurrence)",
                div(class = "alert alert-light small mb-2",
                    "z-score de co-occurrence spatiale par paire de labels -- positif = ",
                    "co-occurrence (voisinage frequent), negatif = exclusion mutuelle."),
                card(full_screen = TRUE, plotOutput(ns("enrichment_heatmap"), height = "500px")),
                DT::DTOutput(ns("enrichment_table"))),

      nav_panel("Ripley's K (agregation spatiale)",
                div(class = "alert alert-light small mb-2",
                    "Ligne + points = K(r) observe pour le label cible (couleur = ",
                    "significativite). Bande grise = enveloppe a 95% sous etiquetage ",
                    "aleatoire. Au-dessus = agregation spatiale ; en-dessous = dispersion."),
                card(full_screen = TRUE, plotOutput(ns("ripley_plot"), height = "500px")))
    )
  )
}

mod_spatial_niche_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_file <- spatial_log_path(session, "niche")
    tracker  <- create_reactive_tracker(session, log_file)

    # ── Which categorical grouping can we build niches from? (unchanged --
    # niches themselves are never grouped by an existing niche) ───────────
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
      # v2 (export/script reproductible) : miroir des parametres UTILISES,
      # lu uniquement par mod_spatial_export.R -- purement additif.
      shared_rv$niche_params <- list(group_by = input$group_by, k_neighbors = input$k_neighbors,
                                     n_niches = input$n_niches)
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

    # =========================================================================
    # B1/B6 (vague 5) — shared "which categorical grouping is available"
    # helper, extended to ALSO offer niches themselves (unlike group_by_ui
    # above, on purpose -- testing enrichment/aggregation OF the niches you
    # just computed is a natural next question once they exist).
    # =========================================================================
    .group_choices <- function() {
      choices <- c()
      if (!is.null(shared_rv$cluster_labels)) choices["Cluster spatial (BANKSY-lite, onglet 2)"] <- "cluster"
      if (!is.null(shared_rv$deconv_props))    choices["Type cellulaire dominant (deconvolution, onglet 3)"] <- "deconv"
      if (!is.null(shared_rv$niche_labels))    choices["Niche spatiale (ci-dessus)"] <- "niche"
      choices
    }
    .resolve_group_labels <- function(key) {
      switch(key,
        "cluster" = shared_rv$cluster_labels,
        "deconv"  = if (!is.null(shared_rv$deconv_props)) dominant_group_labels(shared_rv$deconv_props) else NULL,
        "niche"   = shared_rv$niche_labels,
        NULL)
    }

    # -------------------------------------------------------------------------
    # B1. Neighborhood enrichment (co-occurrence) —
    # spatial_neighborhood_enrichment(coords, group_labels, k_neighbors, n_perm,
    # log_file) -> list(enrichment = data.frame(from, to, observed, z_score),
    # matrix =, levels =, k_neighbors =, n_perm =)
    # -------------------------------------------------------------------------
    output$group_by_enrich_ui <- renderUI({
      choices <- .group_choices()
      if (length(choices) == 0) {
        return(div(class = "alert alert-warning", style = "font-size:0.75rem;",
                    "Calculez d'abord un clustering, une deconvolution ou des niches pour ",
                    "disposer d'un regroupement categoriel."))
      }
      selectInput(ns("group_by_enrich"), "Regroupement", choices = choices)
    })

    enrich_log_file <- spatial_log_path(session, "enrichment")
    enrich_tracker  <- create_reactive_tracker(session, enrich_log_file)

    enrichment_task <- ExtendedTask$new(function(coords, group_labels, k_neighbors, n_perm, log_file) {
      mirai::mirai(
        {
          spatial_neighborhood_enrichment(coords = coords, group_labels = group_labels,
                                           k_neighbors = k_neighbors, n_perm = n_perm,
                                           log_file = log_file)
        },
        coords = coords, group_labels = group_labels, k_neighbors = k_neighbors,
        n_perm = n_perm, log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(enrichment_task, "btn_enrichment")

    observeEvent(input$btn_enrichment, {
      req(global_data$spatial_obj$coords, input$group_by_enrich)
      group_labels <- .resolve_group_labels(input$group_by_enrich)
      req(group_labels)

      reset_log(enrich_log_file)
      shared_rv$enrichment_params <- list(group_by = input$group_by_enrich,
                                          k_neighbors = input$k_neighbors_enrich,
                                          n_perm = input$n_perm_enrich)
      enrichment_task$invoke(
        coords = global_data$spatial_obj$coords, group_labels = group_labels,
        k_neighbors = input$k_neighbors_enrich, n_perm = input$n_perm_enrich,
        log_file = enrich_log_file
      )
    })

    observeEvent(enrichment_task$status(), {
      if (enrichment_task$status() == "success") {
        shared_rv$enrichment_result <- enrichment_task$result()
        showNotification("Enrichissement de voisinage calcule.", type = "message", duration = 4)
      } else if (enrichment_task$status() == "error") {
        showNotification(
          "Erreur pendant le calcul de l'enrichissement — voir le log. Essayez 'Reinitialiser les daemons' puis relancez.",
          type = "error", duration = 10)
      }
    })

    output$enrichment_progress_text <- renderText({
      lines <- enrich_tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    output$enrichment_heatmap <- renderPlot({
      req(shared_rv$enrichment_result)
      df <- shared_rv$enrichment_result$enrichment
      ggplot2::ggplot(df, ggplot2::aes(x = to, y = from, fill = z_score)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(z_score), "", sprintf("%.1f", z_score))), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                      midpoint = 0, na.value = "grey85") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = "Voisin", y = "Origine", fill = "z-score",
                      title = sprintf("Enrichissement de voisinage (k=%d, %d permutations)",
                                       shared_rv$enrichment_result$k_neighbors,
                                       shared_rv$enrichment_result$n_perm))
    })

    output$enrichment_table <- DT::renderDT({
      req(shared_rv$enrichment_result)
      df <- shared_rv$enrichment_result$enrichment
      df <- df[order(-abs(df$z_score)), ]
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
        DT::formatRound("z_score", 2)
    })

    # -------------------------------------------------------------------------
    # B6. Ripley's K (etiquetage aleatoire) —
    # ripley_k_random_labeling(coords, group_labels, target_level, n_perm,
    # max_total, max_target, log_file) -> list(curve = data.frame(r,
    # k_observed, k_perm_mean, k_perm_lo, k_perm_hi, signif), target_level =,
    # n_target =, n_total =, n_perm =, subsampled =)
    # -------------------------------------------------------------------------
    output$group_by_ripley_ui <- renderUI({
      choices <- .group_choices()
      if (length(choices) == 0) {
        return(div(class = "alert alert-warning", style = "font-size:0.75rem;",
                    "Calculez d'abord un clustering, une deconvolution ou des niches pour ",
                    "disposer d'un regroupement categoriel."))
      }
      selectInput(ns("group_by_ripley"), "Regroupement", choices = choices)
    })

    output$ripley_target_ui <- renderUI({
      req(input$group_by_ripley)
      labs <- .resolve_group_labels(input$group_by_ripley)
      req(labs)
      lv <- sort(unique(as.character(labs)))
      validate(need(length(lv) > 0, "Aucun niveau disponible pour ce regroupement."))
      selectInput(ns("ripley_target"), "Cluster / type cible", choices = lv)
    })

    ripley_log_file <- spatial_log_path(session, "ripley")
    ripley_tracker  <- create_reactive_tracker(session, ripley_log_file)

    ripley_task <- ExtendedTask$new(function(coords, group_labels, target_level, n_perm,
                                              max_total, max_target, log_file) {
      mirai::mirai(
        {
          ripley_k_random_labeling(coords = coords, group_labels = group_labels,
                                   target_level = target_level, n_perm = n_perm,
                                   max_total = max_total, max_target = max_target,
                                   log_file = log_file)
        },
        coords = coords, group_labels = group_labels, target_level = target_level,
        n_perm = n_perm, max_total = max_total, max_target = max_target,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(ripley_task, "btn_ripley")

    observeEvent(input$btn_ripley, {
      req(global_data$spatial_obj$coords, input$group_by_ripley, input$ripley_target)
      group_labels <- .resolve_group_labels(input$group_by_ripley)
      req(group_labels)

      reset_log(ripley_log_file)
      shared_rv$ripley_params <- list(group_by = input$group_by_ripley, target = input$ripley_target,
                                      n_perm = input$n_perm_ripley, max_total = input$max_total_ripley,
                                      max_target = input$max_target_ripley)
      ripley_task$invoke(
        coords = global_data$spatial_obj$coords, group_labels = group_labels,
        target_level = input$ripley_target, n_perm = input$n_perm_ripley,
        max_total = as.integer(input$max_total_ripley), max_target = as.integer(input$max_target_ripley),
        log_file = ripley_log_file
      )
    })

    observeEvent(ripley_task$status(), {
      if (ripley_task$status() == "success") {
        shared_rv$ripley_result <- ripley_task$result()
        showNotification("Ripley's K calcule.", type = "message", duration = 4)
      } else if (ripley_task$status() == "error") {
        showNotification(
          "Erreur pendant le calcul de Ripley's K — voir le log. Essayez 'Reinitialiser les daemons' puis relancez.",
          type = "error", duration = 10)
      }
    })

    output$ripley_progress_text <- renderText({
      lines <- ripley_tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    output$ripley_plot <- renderPlot({
      req(shared_rv$ripley_result)
      res <- shared_rv$ripley_result
      df  <- res$curve
      title_txt <- sprintf("Ripley's K -- '%s' (%s/%s elements)%s",
                           res$target_level, format(res$n_target, big.mark = ","),
                           format(res$n_total, big.mark = ","),
                           if (isTRUE(res$subsampled)) " -- SOUS-ECHANTILLONNE" else "")
      ggplot2::ggplot(df, ggplot2::aes(x = r)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = k_perm_lo, ymax = k_perm_hi), fill = "grey80", alpha = 0.6) +
        ggplot2::geom_line(ggplot2::aes(y = k_perm_mean), color = "grey40", linetype = "dashed") +
        ggplot2::geom_line(ggplot2::aes(y = k_observed), color = "#D55E00", linewidth = 1) +
        ggplot2::geom_point(ggplot2::aes(y = k_observed, color = signif), size = 2.8) +
        ggplot2::scale_color_manual(values = c("Agregation" = "#D55E00", "Dispersion" = "#0072B2", "NS" = "grey40")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = "Rayon r", y = "K(r)", color = "Significativite", title = title_txt,
                      subtitle = "Bande grise = enveloppe 95% (etiquetage aleatoire)")
    })
  })
}
