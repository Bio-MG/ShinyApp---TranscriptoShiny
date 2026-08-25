# =============================================================================
# modules/spatial/mod_spatial_multi.R — Phase 4: Multi-echantillons
# ("Working with multiple slices in Seurat", vignette parity)
# =============================================================================
# v3 (vague 5 — Phase 6 stats, B3) : nouvel onglet "Composition differentielle"
#    -- test du Chi2 d'independance (dataset x cluster) sur l'integration
#    conjointe (ci-dessus), via
#    R/utils_spatial_stats.R::compute_composition_differential() (deja pure,
#    synchrone, meme convention que mod_spatial_deconv.R's "Colocalisation").
#    FIX layout (retour terrain -- "les graphiques Residus standardises et
#    Proportions par echantillon restent dans un petit cadre malgre le bouton
#    plein ecran") : les deux plots sont maintenant dans des card(height=)
#    EXPLICITES (520px) + card_body(class="p-0") + plotOutput(height="100%"),
#    copie a l'identique du pattern DEJA UTILISE (et DEJA CONFIRME correct
#    par l'utilisateur) par l'onglet "UMAP conjoint" un peu plus bas dans ce
#    meme fichier -- un plotOutput/plotlyOutput sans hauteur explicite a
#    l'interieur d'un card() sans hauteur explicite retombe sur la hauteur
#    par defaut du navigateur (souvent ~400px), ce que full_screen=TRUE
#    n'affecte QUE lorsqu'on clique effectivement sur l'icone d'agrandissement
#    -- d'ou "le bouton plein ecran marche mais la vue normale reste petite".
#
# v2 (feedback biologiste — support rapport multi-echantillons,
# mod_spatial_report.R) : integration_result() (heretofore un reactiveVal
# LOCAL a ce module, invisible en dehors) est maintenant AUSSI ecrit dans
# global_data$spatial_multi_integration (plain list, pas reactif) des qu'un
# calcul reussit -- purement additif, permet a mod_spatial_report.R (et a
# tout futur module) de lire le dernier resultat d'integration conjointe
# sans dependre de l'etat interne de ce module (meme pattern que
# global_data$spatial_results_cache pour le cache par-echantillon).
#
# v1.1 (audit step 3.9b — multi-sample bugfix): output$dataset_picker_ui's
# sprintf() call had its format string accidentally split into TWO separate
# character arguments (missing string concatenation) instead of one
# continuous "%d ...(onglet Import > Spatial)..." string. Fixed.
#
# Operates on global_data$spatial_datasets (NEW top-level container, see
# app.R / mod_import_spatial.R): a named list of the SAME spatial_obj-shaped
# list every other spatial module already reads ($sketch, $bpcells_dir,
# $coords, ...), one entry per imported dataset. global_data$spatial_obj
# keeps pointing at whichever ONE dataset is "active" (see
# mod_import_spatial.R) — so this module is purely ADDITIVE: QC/Clustering/
# Deconvolution/Visualisation (tabs 1-4) are completely unaware multi-sample
# support exists and needed ZERO changes.
#
# Sketch-only integration (see R/utils_spatial_multi.R header for the full
# RAM-safety rationale) — the heavy step (merge + normalize + PCA +
# Harmony + UMAP + clustering) runs in its own mirai daemon, reopening each
# dataset's sketch from a tempfile (same pattern as the existing
# single-dataset sketch UMAP task, mod_spatial_viz.R) rather than shipping
# live Shiny state into the daemon. The per-section spatial maps below the
# integration button are cheap (sketch + coords, already in RAM) and render
# synchronously, exactly like every other "instant preview" in this app.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

mod_spatial_multi_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Multi-echantillons", width = 360,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " Comparez et integrez conjointement plusieurs echantillons spatiaux deja importes ",
          "(onglet Import > Spatial). L'integration s'execute sur les \"sketches\" en RAM ",
          "(echantillon <= max_sketch cellules par jeu de donnees, jamais les matrices ",
          "pleine resolution) pour rester leger sur une machine CPU/32 Go."),

      uiOutput(ns("dataset_picker_ui")),

      numericInput(ns("npcs"), "Composantes PCA", 30, min = 5, max = 50, step = 5),
      numericInput(ns("resolution"), "Resolution (Leiden)", 0.8, min = 0.1, max = 3, step = 0.1),
      checkboxInput(ns("use_harmony"), "Correction de batch (Harmony, groupe = echantillon)", value = TRUE),
      div(class = "alert alert-light", style = "font-size:0.72rem;",
          "Recommande pour comparer des echantillons/conditions differents. Desactivez si vos ",
          "coupes proviennent du meme tissu/bloc (ex: 2 moities d'un meme cerveau) — comportement ",
          "de la vignette Seurat par defaut, sans correction."),

      bslib::input_task_button(ns("btn_integrate"), "Lancer l'integration",
                                icon = icon("layer-group")),
      verbatimTextOutput(ns("integrate_progress_text"), placeholder = TRUE),

      hr(),
      radioButtons(ns("section_color_by"), "Colorer les cartes par section",
                   choices = c("nCount (avant integration)" = "ncount",
                               "Cluster integre (apres integration)" = "cluster"),
                   selected = "ncount"),

      hr(),
      div(class = "alert alert-light", style = "font-size:0.72rem;",
          bsicons::bs_icon("bar-chart-steps"),
          " Le test de composition differentielle (onglet \"Composition differentielle\") se ",
          "recalcule automatiquement des qu'une integration conjointe est disponible ci-dessus ",
          "— aucun bouton separe.")
    ),

    navset_card_underline(
      nav_panel(
        "Cartes par section",
        div(class = "alert alert-light small mt-2 mb-2",
            "Equivalent de SpatialDimPlot(brain.merge) : une carte par echantillon selectionne, ",
            "cote a cote, meme metrique/coloration pour comparaison directe."),
        uiOutput(ns("section_maps_ui"))
      ),
      nav_panel(
        "UMAP conjoint",
        div(class = "alert alert-light small mt-2 mb-2",
            "Equivalent de DimPlot(brain.merge, group.by=c(\"ident\",\"orig.ident\")) : le panneau de ",
            "gauche verifie l'effet de lot (les echantillons doivent bien se melanger si Harmony est ",
            "actif) ; celui de droite montre les clusters biologiques resultants."),
        layout_columns(
          col_widths = c(6, 6),
          row_heights = "460px",
          card(full_screen = TRUE, height = "460px",
               card_header("Colore par echantillon (effet de lot)"),
               card_body(class = "p-0", plotly::plotlyOutput(ns("umap_by_dataset"), height = "100%"))),
          card(full_screen = TRUE, height = "460px",
               card_header("Colore par cluster integre"),
               card_body(class = "p-0", plotly::plotlyOutput(ns("umap_by_cluster"), height = "100%")))
        )
      ),
      nav_panel(
        "Composition differentielle",
        div(class = "alert alert-light small mt-2 mb-2",
            "Test du Chi2 d'independance (table de contingence dataset x cluster) sur ",
            "l'integration conjointe -- repli automatique sur un p simule (2000 replicats) si ",
            "des effectifs attendus sont trop faibles (< 5). Les residus standardises indiquent ",
            "QUELS clusters/echantillons s'ecartent le plus de l'independance (|residu| > ~2 = ",
            "ecart notable)."),
        uiOutput(ns("diffcomp_summary_ui")),
        layout_columns(
          col_widths = c(6, 6),
          row_heights = "520px",
          card(full_screen = TRUE, height = "520px",
               card_header("Residus standardises (dataset x cluster)"),
               card_body(class = "p-0", plotOutput(ns("diffcomp_resid_plot"), height = "100%"))),
          card(full_screen = TRUE, height = "520px",
               card_header("Proportions par echantillon"),
               card_body(class = "p-0", plotOutput(ns("diffcomp_prop_plot"), height = "100%")))
        ),
        DT::DTOutput(ns("diffcomp_contingency_table"))
      ),
      nav_panel(
        "Resume",
        uiOutput(ns("summary_ui")),
        DT::DTOutput(ns("n_per_dataset_table"))
      )
    )
  )
}

mod_spatial_multi_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Dataset picker (dynamic: reflects whatever has been imported) ────
    output$dataset_picker_ui <- renderUI({
      ds_names <- names(global_data$spatial_datasets)
      if (length(ds_names) < 2) {
        return(div(class = "alert alert-warning", style = "font-size:0.8rem;",
                    sprintf(paste0("%d echantillon(s) importe(s) — importez-en au moins 2 ",
                                   "(onglet Import > Spatial) pour utiliser cette page."),
                            length(ds_names))))
      }
      checkboxGroupInput(ns("selected_datasets"), "Echantillons a comparer",
                         choices = ds_names, selected = ds_names)
    })

    selected_ds <- reactive({
      req(global_data$spatial_datasets)
      sel <- input$selected_datasets %||% names(global_data$spatial_datasets)
      intersect(sel, names(global_data$spatial_datasets))
    })

    # ── Async integration task (mirai) ───────────────────────────────────
    log_file <- spatial_log_path(session, "multi_integrate")
    tracker  <- create_reactive_tracker(session, log_file)

    integrate_task <- ExtendedTask$new(function(sketch_paths, npcs, resolution, use_harmony, log_file) {
      mirai::mirai(
        {
          integrate_spatial_sketches(
            sketch_paths = sketch_paths, npcs = npcs, resolution = resolution,
            use_harmony = use_harmony, log_file = log_file
          )
        },
        sketch_paths = sketch_paths, npcs = npcs, resolution = resolution,
        use_harmony = use_harmony, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(integrate_task, "btn_integrate")

    observeEvent(input$btn_integrate, {
      ds <- selected_ds()
      if (length(ds) < 2) {
        showNotification("Selectionnez au moins 2 echantillons.", type = "warning", duration = 6)
        return()
      }
      reset_log(log_file)
      sketch_paths <- stats::setNames(
        lapply(ds, function(nm) {
          tmp <- tempfile(fileext = ".rds")
          saveRDS(global_data$spatial_datasets[[nm]]$sketch, tmp)
          tmp
        }),
        ds
      )
      integrate_task$invoke(
        sketch_paths = sketch_paths, npcs = input$npcs, resolution = input$resolution,
        use_harmony = isTRUE(input$use_harmony), log_file = log_file
      )
    })

    integration_result <- reactiveVal(NULL)
    observeEvent(integrate_task$status(), {
      if (integrate_task$status() == "success") {
        res <- integrate_task$result()
        integration_result(res)
        global_data$spatial_multi_integration <- list(
          embeddings = res$embeddings, n_per_dataset = res$n_per_dataset,
          reduction_used = res$reduction_used, datasets = selected_ds(),
          computed_at = Sys.time()
        )
        updateRadioButtons(session, "section_color_by", selected = "cluster")
        showNotification("Integration terminee.", type = "message", duration = 5)
      } else if (integrate_task$status() == "error") {
        showNotification(
          "Erreur (ou depassement du delai) pendant l'integration multi-echantillons — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez.",
          type = "error", duration = 12)
      }
    })

    output$integrate_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    # ── Per-section spatial maps (cheap, synchronous — sketch + coords) ──
    # Palette partagee (R/palettes.R::spatial_discrete_colors) : suit le
    # selecteur global de l'onglet Visualisation ; repli Dark 3 inchange
    # pour la palette "Defaut".
    cluster_palette <- function(v, kind = "cluster") spatial_discrete_colors(v, shared_rv, kind = kind)

    section_df <- function(dataset_name) {
      obj <- global_data$spatial_datasets[[dataset_name]]
      req(obj$sketch, obj$coords)
      sk_ids <- colnames(obj$sketch)
      df <- obj$coords[match(sk_ids, obj$coords$id), c("id", "x", "y")]

      if (identical(input$section_color_by, "cluster") && !is.null(integration_result())) {
        emb <- integration_result()$embeddings
        emb_ds <- emb[emb$dataset == dataset_name, ]
        m <- match(df$id, emb_ds$original_id)
        df$value <- emb_ds$cluster[m]
      } else {
        n_count <- Matrix::colSums(SeuratObject::LayerData(obj$sketch, layer = "counts"))
        df$value <- as.numeric(n_count[df$id])
      }
      df[stats::complete.cases(df[, c("x", "y")]), ]
    }

    output$section_maps_ui <- renderUI({
      ds <- selected_ds()
      req(length(ds) > 0)
      div(
        style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); gap: 1rem;",
        lapply(ds, function(nm) {
          card(height = "380px",
               card_header(nm),
               card_body(class = "p-0", plotly::plotlyOutput(ns(paste0("map_", nm)), height = "100%")))
        })
      )
    })

    observe({
      ds <- selected_ds()
      req(length(ds) > 0)
      for (nm in ds) {
        local({
          dataset_name <- nm
          output_id <- paste0("map_", dataset_name)
          output[[output_id]] <- plotly::renderPlotly({
            df <- section_df(dataset_name)
            req(nrow(df) > 0)

            if (is.numeric(df$value)) {
              pal <- leaflet::colorNumeric(spatial_color_ramp_hex(shared_rv), domain = range(df$value, na.rm = TRUE), na.color = "#CCCCCC")
              cols <- pal(df$value)
            } else {
              pal <- cluster_palette(df$value)
              cols <- if (!is.null(pal)) pal[as.character(df$value)] else "#CCCCCC"
              cols[is.na(cols)] <- "#CCCCCC"
            }

            plotly::plot_ly(
              data = df, x = ~x, y = ~-y, type = "scattergl", mode = "markers",
              marker = list(color = cols, size = 5, opacity = 0.85),
              text = ~paste0("ID: ", id, "<br>Valeur: ", value), hoverinfo = "text"
            ) |>
              plotly::layout(
                margin = list(l = 5, r = 5, t = 5, b = 5),
                xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE),
                yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, scaleanchor = "x", scaleratio = 1)
              )
          })
        })
      }
    })

    # ── Joint UMAP (post-integration) ────────────────────────────────────
    output$umap_by_dataset <- plotly::renderPlotly({
      req(integration_result())
      emb <- integration_result()$embeddings
      pal <- cluster_palette(emb$dataset, kind = "dataset")
      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~dataset, colors = pal,
                      type = "scattergl", mode = "markers",
                      marker = list(size = 5, opacity = 0.7)) |>
        plotly::layout(margin = list(l = 20, r = 20, t = 20, b = 20))
    })

    output$umap_by_cluster <- plotly::renderPlotly({
      req(integration_result())
      emb <- integration_result()$embeddings
      pal <- cluster_palette(emb$cluster)
      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cluster, colors = pal,
                      type = "scattergl", mode = "markers",
                      marker = list(size = 5, opacity = 0.7)) |>
        plotly::layout(margin = list(l = 20, r = 20, t = 20, b = 20))
    })

    # =========================================================================
    # B3 (vague 5) — Composition differentielle inter-echantillons
    # compute_composition_differential(embeddings) -> list(contingency=table,
    # chisq=list(statistic,p_value,method), residuals=data.frame(dataset,
    # cluster,std_resid), proportions=data.frame(dataset,cluster,proportion,n))
    # Purely reactive off integration_result() -- no extra button, same
    # convention as mod_spatial_deconv.R's colocalisation heatmap (B2).
    # =========================================================================
    composition_diff_result <- reactive({
      req(integration_result())
      tryCatch(
        compute_composition_differential(integration_result()$embeddings),
        error = function(e) {
          showNotification(paste("Erreur test de composition differentielle :", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    output$diffcomp_summary_ui <- renderUI({
      res <- composition_diff_result()
      if (is.null(res)) {
        return(div(class = "alert alert-light",
                    "Calculez d'abord une integration conjointe (bouton \"Lancer l'integration\" ",
                    "dans le panneau lateral)."))
      }
      sig <- if (res$chisq$p_value < 0.05) "SIGNIFICATIVE" else "non significative"
      div(class = if (res$chisq$p_value < 0.05) "alert alert-success" else "alert alert-light",
          sprintf("Chi2 = %.1f, p = %.4g (%s) — composition %s entre echantillons (seuil 0.05).",
                  res$chisq$statistic, res$chisq$p_value, res$chisq$method, sig))
    })

    output$diffcomp_resid_plot <- renderPlot({
      res <- req(composition_diff_result())
      ggplot2::ggplot(res$residuals, ggplot2::aes(x = cluster, y = dataset, fill = std_resid)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", std_resid)), size = 3.2) +
        spatial_diverging_scale(shared_rv, aesthetic = "fill") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = "Cluster", y = "Echantillon", fill = "Residu\nstandardise",
                      title = "Ecarts a l'independance (dataset x cluster)")
    })

    output$diffcomp_prop_plot <- renderPlot({
      res <- req(composition_diff_result())
      lv  <- sort(unique(as.character(res$proportions$cluster)))
      ggplot2::ggplot(res$proportions, ggplot2::aes(x = dataset, y = proportion, fill = cluster)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = spatial_discrete_colors(lv, shared_rv, kind = "cluster")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
        ggplot2::labs(x = NULL, y = "Proportion", fill = "Cluster",
                      title = "Composition en clusters par echantillon")
    })

    output$diffcomp_contingency_table <- DT::renderDT({
      res <- req(composition_diff_result())
      tab <- as.data.frame.matrix(res$contingency)
      tab <- cbind(Echantillon = rownames(tab), tab)
      DT::datatable(tab, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    # ── Summary ───────────────────────────────────────────────────────────
    output$summary_ui <- renderUI({
      res <- integration_result()
      if (is.null(res)) {
        return(div(class = "alert alert-light",
                    "Aucune integration calculee pour le moment. Selectionnez des echantillons et ",
                    "cliquez \"Lancer l'integration\"."))
      }
      div(class = "alert alert-success",
          sprintf("Integration terminee — reduction utilisee : %s ; %d elements au total sur %d echantillon(s).",
                   res$reduction_used, nrow(res$embeddings), length(unique(res$embeddings$dataset))))
    })

    output$n_per_dataset_table <- DT::renderDT({
      res <- integration_result()
      req(res)
      tab <- as.data.frame(res$n_per_dataset)
      colnames(tab) <- c("Echantillon", "Elements (sketch)")
      DT::datatable(tab, options = list(pageLength = 10), rownames = FALSE)
    })
  })
}
