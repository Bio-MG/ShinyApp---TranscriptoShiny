mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Visualisation", width = 320,
      
      uiOutput(ns("sketch_norm_status_ui")),
      
      selectInput(
        ns("color_by"), "Colorer par",
        choices = c(
          "Metrique QC" = "qc",
          "Cluster spatial" = "cluster",
          "Type cellulaire (deconvolution)" = "deconv",
          "Gene" = "gene"
        ),
        selected = "qc"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'qc'", ns("color_by")),
        selectInput(
          ns("qc_metric"), NULL,
          choices = c("nCount", "nFeature", "pct_mt", "pct_ribo")
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        selectizeInput(
          ns("gene"), NULL, choices = NULL,
          options = list(
            maxOptions = 3000,
            placeholder = "Rechercher un gene..."
          )
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        checkboxInput(
          ns("scale_alpha_by_expr"),
          "Opacite proportionnelle a l'expression (SpatialFeaturePlot)",
          value = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("scale_alpha_by_expr")),
          sliderInput(
            ns("alpha_range"), "Plage d'opacite (min-max)",
            0, 1, c(0.15, 1), step = 0.05
          )
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'deconv'", ns("color_by")),
        uiOutput(ns("deconv_celltype_ui"))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'cluster'", ns("color_by")),
        checkboxInput(
          ns("show_cluster_labels"),
          "Afficher les labels de cluster sur la carte (vignette: DimPlot label=TRUE)",
          value = FALSE
        )
      ),
      
      sliderInput(ns("pt_radius"), "Taille des points", 1, 20, 6, step = 1),
      sliderInput(ns("pt_opacity"), "Opacite des points (hors mode Gene)", 0.1, 1, 0.85, step = 0.05),
      
      hr(),
      checkboxInput(ns("show_histology"), "Afficher l'image histologique (fond de coupe)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("show_histology")),
        sliderInput(ns("histology_opacity"), "Opacite de l'image", 0, 1, 0.7, step = 0.05)
      ),
      uiOutput(ns("histology_status_ui")),
      
      div(
        class = "border-top pt-2 mt-2",
        downloadButton(ns("dl_png"), "Export PNG", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_csv"), "Export CSV (donnees affichees)", class = "btn-sm btn-outline-secondary w-100")
      ),
      
      hr(),
      bslib::input_task_button(
        ns("btn_compute_umap"),
        "Calculer PCA + UMAP (sketch)",
        icon = icon("chart-line")
      ),
      verbatimTextOutput(ns("umap_progress_text"), placeholder = TRUE)
    ),
    
    navset_card_underline(
      nav_panel(
        "Carte spatiale",
        card(
          full_screen = TRUE,
          style = "min-height: 78vh;",
          plotly::plotlyOutput(
            ns("spatial_preview_plot"),
            height = "calc(100vh - 220px)"
          )
        )
      ),
      
      nav_panel(
        "UMAP (non-spatial, sketch)",
        card(
          full_screen = TRUE,
          style = "min-height: 78vh;",
          plotly::plotlyOutput(
            ns("umap_plot"),
            height = "calc(100vh - 220px)"
          )
        )
      ),
      
      nav_panel(
        "Vue combinee (mode expert)",
        div(
          class = "alert alert-light mb-2",
          style = "font-size:0.82rem;",
          uiOutput(ns("combined_help_ui"))
        ),
        div(
          class = "d-flex flex-wrap gap-2 align-items-end mb-3",
          style = "row-gap: 0.5rem;",
          div(
            style = "min-width: 320px; flex: 1 1 320px;",
            selectizeInput(
              ns("highlight_clusters"),
              "Isoler cluster(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Tous les clusters",
                plugins = list("remove_button")
              )
            )
          ),
          div(
            style = "min-width: 220px;",
            actionButton(
              ns("btn_clear_selection"),
              "Effacer la selection lasso",
              class = "btn btn-outline-secondary"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header("Carte spatiale liee"),
            plotly::plotlyOutput(
              ns("combined_spatial_plot"),
              height = "72vh"
            )
          ),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header("UMAP lie"),
            plotly::plotlyOutput(
              ns("combined_umap_plot"),
              height = "72vh"
            )
          )
        )
      )
    )
    
    
  )
}

mod_spatial_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sketch_norm_status_ui <- renderUI({
      req(global_data$spatial_obj$sketch)
      norm_used <- tryCatch(Seurat::DefaultAssay(global_data$spatial_obj$sketch), error = function(e) NA)
      label <- if (identical(norm_used, "SCT")) "SCTransform" else "LogNormalize"
      div(class = "alert alert-light", style = "font-size:0.7rem;padding:2px 8px;",
          sprintf("Normalisation du sketch : %s", label))
    })
    
    # ── Histology background (Visium only) ────────────────────────────────
    histology_overlay <- reactive({
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data) || is.null(hist_data$raster)) return(NULL)
      
      sf_lowres <- hist_data$scale_factors$lowres %||% 1
      bounds <- list(x = c(0, hist_data$dim[2] / sf_lowres),
                     y = c(0, hist_data$dim[1] / sf_lowres))
      
      data_uri <- tryCatch({
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          warning("Package 'jsonlite' absent : fond histologique indisponible en previsualisation interactive.")
          return(NULL)
        }
        # FIX: rasterizing to PNG was leaving the graphics device OPEN if
        # grid.raster() errored (dev.off() below it never ran) -- an R
        # process has ONE global device stack, so a dangling device here
        # would silently corrupt every OTHER plot in the session (QC
        # histograms, ggplot exports, etc.), not just this one. Scoping
        # on.exit() inside its own function guarantees the device closes
        # no matter what happens. Also: bg="transparent" is not reliably
        # supported by every graphics backend (notably Windows' default
        # device) -- bg="white" is universally safe; Plotly's own
        # `opacity` on the image layer still gives the desired see-through
        # effect regardless of whether the PNG itself has alpha.
        render_png <- function() {
          tmp <- tempfile(fileext = ".png")
          grDevices::png(tmp, width = hist_data$dim[2], height = hist_data$dim[1], bg = "white")
          on.exit(grDevices::dev.off(), add = TRUE)
          grid::grid.raster(hist_data$raster, width = 1, height = 1)
          tmp
        }
        tmp_png <- render_png()
        raw_png <- readBin(tmp_png, "raw", file.info(tmp_png)$size)
        unlink(tmp_png)
        paste0("data:image/png;base64,", jsonlite::base64_enc(raw_png))
      }, error = function(e) {
        warning("Rendu PNG/base64 de l'image histologique echoue : ", conditionMessage(e))
        NULL
      })
      
      list(raster_obj = hist_data$raster, data_uri = data_uri, bounds = bounds)
    })
    
    observe({
      req(global_data$spatial_obj)
      
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data) || is.null(hist_data$raster)) {
        return()
      }
      
      ov <- histology_overlay()
      if (is.null(ov)) {
        message("[spatial_viz] histology_overlay encore NULL apres disponibilite histology")
        return()
      }
      
      message("[spatial_viz] ", build_histology_debug_text(ov))
    })
    
    
    # ── Helpers Plotly / Histology ───────────────────────────────────────
    
    make_plotly_histology_image <- function(hist_ov, opacity = 0.7) {
      if (is.null(hist_ov) || is.null(hist_ov$data_uri) || is.null(hist_ov$bounds)) {
        return(NULL)
      }
      
      b <- hist_ov$bounds
      xmin <- b$x[1]
      xmax <- b$x[2]
      ymin <- b$y[1]
      ymax <- b$y[2]
      
      list(list(
        source = hist_ov$data_uri,
        xref = "x",
        yref = "y",
        
        # On affiche les points dans le repère (x, -y).
        # L'image doit donc être ancrée dans ce même repère.
        x = xmin,
        y = -ymax,
        
        sizex = xmax - xmin,
        sizey = ymax - ymin,
        
        xanchor = "left",
        yanchor = "bottom",
        sizing = "stretch",
        opacity = opacity,
        layer = "below"
      ))
    }
    
    compute_spatial_ranges <- function(df_all, hist_ov = NULL, show_hist = FALSE) {
      x_vals <- df_all$x
      y_vals <- -df_all$y
      
      if (isTRUE(show_hist) && !is.null(hist_ov) && !is.null(hist_ov$bounds)) {
        x_vals <- c(x_vals, hist_ov$bounds$x)
        y_vals <- c(y_vals, -hist_ov$bounds$y[1], -hist_ov$bounds$y[2])
      }
      
      list(
        x = range(x_vals, na.rm = TRUE),
        y = range(y_vals, na.rm = TRUE)
      )
    }
    
    build_histology_debug_text <- function(hist_ov) {
      if (is.null(hist_ov)) return("Histology overlay = NULL")
      paste0(
        "Histology debug | data_uri: ", !is.null(hist_ov$data_uri),
        " | raster dims: ", paste(dim(hist_ov$raster_obj), collapse = " x "),
        " | x=[", paste(round(hist_ov$bounds$x, 2), collapse = ", "), "]",
        " | y=[", paste(round(hist_ov$bounds$y, 2), collapse = ", "), "]"
      )
    } 
    
    
    output$histology_status_ui <- renderUI({
      req(global_data$spatial_obj)
      
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data) || is.null(hist_data$raster)) {
        return(
          div(
            class = "alert alert-light",
            style = "font-size:0.7rem;padding:2px 6px;",
            "Image histologique indisponible (technologie non-Visium, ou image absente/non extraite pour ce jeu de donnees)."
          )
        )
      }
      
      ov <- histology_overlay()
      
      if (is.null(ov) || is.null(ov$data_uri)) {
        return(
          div(
            class = "alert alert-warning",
            style = "font-size:0.7rem;padding:2px 6px;",
            "Image presente mais non affichable en previsualisation interactive (encodage PNG/base64 echoue — voir avertissements console R). L'export PNG reste disponible."
          )
        )
      }
      
      div(
        class = "text-muted",
        style = "font-size:0.65rem;padding:2px 6px;",
        paste0(
          "Fond histologique OK — ",
          round(nchar(ov$data_uri) / 1024), " Ko ; ",
          "x=[", round(ov$bounds$x[1]), ", ", round(ov$bounds$x[2]), "] ; ",
          "y=[", round(ov$bounds$y[1]), ", ", round(ov$bounds$y[2]), "]"
        )
      )
    })
    
    output$combined_help_ui <- renderUI({
      has_clusters <- !is.null(shared_rv$cluster_labels)
      has_umap <- !is.null(umap_df())
      
      if (!has_clusters && !has_umap) {
        tagList(
          strong("Mode expert prêt, mais incomplet. "),
          "Lancez d'abord un clustering spatial puis calculez le PCA+UMAP du sketch. ",
          "Ensuite, vous pourrez sélectionner des cellules au lasso dans un panneau et voir la même sélection dans l'autre."
        )
      } else if (!has_clusters) {
        tagList(
          strong("UMAP disponible. "),
          "Il manque encore les clusters spatiaux pour activer l'exploration croisée complète."
        )
      } else if (!has_umap) {
        tagList(
          strong("Clusters disponibles. "),
          "Calculez maintenant le PCA+UMAP du sketch pour activer la sélection liée spatiale/UMAP."
        )
      } else {
        tagList(
          strong("Mode expert actif. "),
          "Utilisez le lasso ou le rectangle dans l'un des deux panneaux pour suivre une population dans l'autre; ",
          "vous pouvez aussi isoler un ou plusieurs clusters sans perdre l'affichage couplé."
        )
      }
    })
    
    # ── Populate gene / cell-type selectors ─────────────────────────────
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$sketch)
      updateSelectizeInput(session, "gene", choices = rownames(global_data$spatial_obj$sketch), server = TRUE)
    }, ignoreInit = TRUE)
    
    output$deconv_celltype_ui <- renderUI({
      req(shared_rv$deconv_props)
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      selectInput(ns("deconv_celltype"), NULL, choices = cts)
    })
    
    # ── Build per-sketch-cell plotting data.frame ────────────────────────
    plot_df <- reactive({
      req(global_data$spatial_obj$sketch, global_data$spatial_obj$coords)
      sk_ids <- colnames(global_data$spatial_obj$sketch)
      coords <- global_data$spatial_obj$coords
      df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]
      
      df$value <- tryCatch(switch(input$color_by,
                                  "cluster" = {
                                    req(shared_rv$cluster_labels)
                                    as.character(shared_rv$cluster_labels[df$id])
                                  },
                                  "deconv" = {
                                    req(shared_rv$deconv_props, input$deconv_celltype)
                                    m <- match(df$id, shared_rv$deconv_props$id)
                                    shared_rv$deconv_props[[input$deconv_celltype]][m]
                                  },
                                  "qc" = {
                                    req(shared_rv$qc_metrics)
                                    m <- match(df$id, shared_rv$qc_metrics$id)
                                    shared_rv$qc_metrics[[input$qc_metric]][m]
                                  },
                                  "gene" = {
                                    req(input$gene)
                                    sk <- global_data$spatial_obj$sketch
                                    if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
                                    as.numeric(SeuratObject::LayerData(sk, layer = "data")[input$gene, df$id])
                                  }
      ), error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        warning("plot_df(): echec du calcul de couleur (", conditionMessage(e), ") — points affiches en gris.")
        rep(NA_real_, nrow(df))
      })
      df[stats::complete.cases(df[, c("x", "y")]), ]
    })
    
    scale_alpha_by_value <- function(v, alpha_range = c(0.15, 1)) {
      rng <- suppressWarnings(range(v[is.finite(v)]))
      if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(alpha_range[2], length(v)))
      norm <- (v - rng[1]) / diff(rng)
      norm[!is.finite(norm)] <- 0
      alpha_range[1] + norm * diff(alpha_range)
    }
    
    # ── Unlimited discrete palette for plotly's `color=~cluster` mapping ──
    # plotly::plot_ly()'s DEFAULT discrete palette is RColorBrewer::Set2,
    # capped at 8 colors — BANKSY-lite routinely produces more clusters than
    # that, which throws "n too large" warnings and silently recycles colors
    # (two different clusters end up the SAME color). grDevices::hcl.colors()
    # has no such cap. Pass this explicitly to every plot_ly(color=~cluster).
    cluster_palette <- function(cluster_vec) {
      lv <- sort(unique(stats::na.omit(as.character(cluster_vec))))
      if (length(lv) == 0) return(NULL)
      stats::setNames(grDevices::hcl.colors(length(lv), palette = "Dark 3"), lv)
    }
    
    # ── Robust Color Mapping ─────────────────────────────────────────────
    color_values <- function(df) {
      n <- nrow(df)
      if (n == 0L) return(character(0))
      
      add_alpha <- function(cols, alpha) {
        cols <- as.character(cols)
        alpha <- as.numeric(alpha)
        
        if (length(alpha) != length(cols)) {
          alpha <- rep(1, length(cols))
        }
        
        cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        alpha[!is.finite(alpha)] <- 1
        alpha <- pmax(0, pmin(1, alpha))
        
        rgb <- grDevices::col2rgb(cols, alpha = FALSE)
        rgb_hex <- grDevices::rgb(
          red = rgb[1, ],
          green = rgb[2, ],
          blue = rgb[3, ],
          alpha = round(alpha * 255),
          maxColorValue = 255
        )
        
        as.character(rgb_hex)
      }
      
      if (is.numeric(df$value)) {
        valid <- is.finite(df$value)
        
        if (!any(valid)) {
          return(rep("#CCCCCC", n))
        }
        
        domain <- range(df$value[valid])
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = domain,
          na.color = "#CCCCCC"
        )
        
        cols <- as.character(pal(df$value))
        cols[!valid | is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        
        if (identical(input$color_by, "gene") && isTRUE(input$scale_alpha_by_expr)) {
          alpha <- scale_alpha_by_value(df$value, input$alpha_range %||% c(0.15, 1))
          return(add_alpha(cols, alpha))
        }
        
        return(cols)
      }
      
      levels <- sort(unique(stats::na.omit(as.character(df$value))))
      
      if (length(levels) == 0L) {
        return(rep("#CCCCCC", n))
      }
      
      pal <- leaflet::colorFactor(
        palette = grDevices::hcl.colors(length(levels), palette = "Dark 3"),
        domain = levels,
        na.color = "#CCCCCC"
      )
      
      cols <- as.character(pal(as.character(df$value)))
      cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
      cols
    }
    
    # ── Plotly Spatial Preview (Fallback / Interactive) ──────────────────
    output$spatial_preview_plot <- plotly::renderPlotly({
      df <- plot_df()
      req(nrow(df) > 0)
      
      df_plot <- df
      df_plot$y_display <- -df_plot$y
      df_plot$colour <- color_values(df_plot)
      
      max_preview_cells <- 150000L
      if (nrow(df_plot) > max_preview_cells) {
        set.seed(1)
        df_plot <- df_plot[sample.int(nrow(df_plot), max_preview_cells), , drop = FALSE]
      }
      
      hist_ov <- histology_overlay()
      show_hist <- !is.null(hist_ov) &&
        !is.null(hist_ov$data_uri) &&
        isTRUE(input$show_histology)
      
      plot_images <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      p <- plotly::plot_ly(
        data = df_plot,
        x = ~x,
        y = ~y_display,
        key = ~id,
        type = "scattergl",
        mode = "markers",
        marker = list(
          color = ~colour,
          size = input$pt_radius,
          opacity = input$pt_opacity %||% 0.85
        ),
        text = ~paste0(
          "ID: ", id,
          "<br>x: ", round(x, 1),
          "<br>y: ", round(-y_display, 1),
          "<br>Valeur: ", ifelse(is.na(value), "NA", as.character(round(as.numeric(value), 3)))
        ),
        hoverinfo = "text",
        source = ns("spatial_preview_src")
      ) |>
        plotly::layout(
          dragmode = "pan",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = spatial_ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = spatial_ranges$y
          )
        )
      
      if (
        identical(input$color_by, "cluster") &&
        isTRUE(input$show_cluster_labels) &&
        is.character(df_plot$value)
      ) {
        centroids <- stats::aggregate(
          cbind(x, y_display) ~ value,
          data = df_plot,
          FUN = mean
        )
        
        p <- p |>
          plotly::add_annotations(
            x = centroids$x,
            y = centroids$y_display,
            text = centroids$value,
            showarrow = FALSE,
            font = list(size = 13, color = "#1a1a1a"),
            bgcolor = "rgba(255,255,255,0.65)",
            bordercolor = "#666",
            borderwidth = 0.5
          )
      }
      
      plotly::event_register(p, "plotly_selected")
    })
    
    # ── Scattermore Fallback ─────────────────────────────────────────────
    build_raster_plot <- function(df) {
      use_alpha_scale <- identical(input$color_by, "gene") && isTRUE(input$scale_alpha_by_expr) && is.numeric(df$value)
      p <- if (use_alpha_scale) {
        ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = value, alpha = value))
      } else {
        ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = value))
      }
      
      hist_ov <- histology_overlay()
      if (!is.null(hist_ov) && isTRUE(input$show_histology)) {
        b <- hist_ov$bounds
        p <- p + ggplot2::annotation_raster(hist_ov$raster_obj,
                                            xmin = b$x[1], xmax = b$x[2],
                                            ymin = -b$y[2], ymax = -b$y[1],
                                            interpolate = TRUE)
      }
      
      pt_alpha <- input$pt_opacity %||% 0.85
      if (!requireNamespace("scattermore", quietly = TRUE)) {
        p <- if (use_alpha_scale) p + ggplot2::geom_point(size = input$pt_radius / 4)
        else p + ggplot2::geom_point(size = input$pt_radius / 4, alpha = pt_alpha)
      } else {
        p <- if (use_alpha_scale) p + scattermore::geom_scattermore(pointsize = input$pt_radius)
        else p + scattermore::geom_scattermore(pointsize = input$pt_radius, alpha = pt_alpha)
      }
      if (use_alpha_scale) {
        p <- p + ggplot2::scale_alpha_continuous(range = input$alpha_range %||% c(0.15, 1), guide = "none")
      }
      
      p + ggplot2::coord_fixed() + ggplot2::theme_void() +
        (if (is.numeric(df$value)) ggplot2::scale_color_viridis_c(na.value = "#CCCCCC")
         else ggplot2::scale_color_manual(values = stats::setNames(
           grDevices::hcl.colors(length(unique(stats::na.omit(df$value))), "Dark 3"),
           sort(unique(stats::na.omit(df$value)))), na.value = "#CCCCCC")) +
        ggplot2::labs(color = input$color_by)
    }
    
    # ── Exports ────────────────────────────────────────────────────────
    output$dl_png <- downloadHandler(
      filename = function() paste0("carte_spatiale_", input$color_by, "_", Sys.Date(), ".png"),
      content = function(file) {
        df <- plot_df()
        validate(need(nrow(df) > 0, "Aucune donnee a exporter."))
        ggplot2::ggsave(file, plot = build_raster_plot(df), width = 8, height = 8, dpi = 200, bg = "white")
      }
    )
    output$dl_csv <- downloadHandler(
      filename = function() paste0("carte_spatiale_", input$color_by, "_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- plot_df()
        validate(need(nrow(df) > 0, "Aucune donnee a exporter."))
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # ── Sketch PCA + UMAP — ASYNC (ExtendedTask + mirai) ─────────────────
    log_file <- spatial_log_path(session, "sketch_umap")
    tracker  <- create_reactive_tracker(session, log_file)
    
    umap_task <- ExtendedTask$new(function(sketch_path, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement du sketch...", 1, 5)
          sk <- readRDS(sketch_path)
          
          # Check if already normalized via SCTransform at import time
          already_sct <- identical(Seurat::DefaultAssay(sk), "SCT")
          
          if (already_sct) {
            write_mirai_log(log_file, "Sketch deja normalise (SCTransform) — HVG/scale.data reutilises.", 2, 5)
          } else {
            write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
            if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
            sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
            sk <- Seurat::ScaleData(sk, verbose = FALSE)
          }
          
          write_mirai_log(log_file, "PCA...", 3, 5)
          sk <- Seurat::RunPCA(sk, npcs = 30, verbose = FALSE)
          
          write_mirai_log(log_file, "UMAP...", 4, 5)
          sk <- Seurat::RunUMAP(sk, dims = 1:30, verbose = FALSE)
          
          write_mirai_log(log_file, "Termine.", 5, 5)
          emb <- as.data.frame(Seurat::Embeddings(sk, "umap"))
          colnames(emb)[1:2] <- c("dim1", "dim2")
          emb$id <- rownames(emb)
          emb
        },
        sketch_path = sketch_path, log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(umap_task, "btn_compute_umap")
    
    observeEvent(input$btn_compute_umap, {
      req(global_data$spatial_obj$sketch)
      reset_log(log_file)
      tmp <- tempfile(fileext = ".rds")
      saveRDS(global_data$spatial_obj$sketch, tmp)
      umap_task$invoke(sketch_path = tmp, log_file = log_file)
    })
    
    umap_df <- reactiveVal(NULL)
    observeEvent(umap_task$status(), {
      if (umap_task$status() == "success") {
        umap_df(umap_task$result())
        showNotification("UMAP calcule.", type = "message", duration = 3)
      } else if (umap_task$status() == "error") {
        showNotification(
          "Erreur (ou depassement du delai) pendant le calcul UMAP — voir le log.",
          type = "error", duration = 10)
      }
    })
    
    output$umap_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })
    
    output$umap_plot <- plotly::renderPlotly({
      req(umap_df())
      emb <- umap_df()
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else "sketch"
      
      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cluster, colors = cluster_palette(emb$cluster),
                      type = "scattergl", mode = "markers",
                      marker = list(size = 5, opacity = 0.7)) |>
        plotly::layout(margin = list(l = 20, r = 20, t = 20, b = 20),
                       autosize = TRUE)
    })
    
    # ── "Vue combinee" — Linked UMAP + Spatial Scatter (Plotly) ──────────
    # ── "Vue combinee" — Linked UMAP + Spatial Scatter (Plotly) ──────────
    
    observeEvent(shared_rv$cluster_labels, {
      req(shared_rv$cluster_labels)
      updateSelectizeInput(
        session,
        "highlight_clusters",
        choices = sort(unique(shared_rv$cluster_labels)),
        server = FALSE
      )
    }, ignoreInit = FALSE)
    
    linked_selection <- reactiveVal(NULL)
    
    observe({
      req(shared_rv$cluster_labels)
      ed <- plotly::event_data(
        event = "plotly_selected",
        source = ns("spatial_src"),
        session = session,
        priority = "event"
      )
      
      if (is.null(ed) || nrow(ed) == 0) {
        return()
      }
      
      linked_selection(as.character(ed$key))
    })
    
    observe({
      req(umap_df())
      ed <- plotly::event_data(
        event = "plotly_selected",
        source = ns("umap_src"),
        session = session,
        priority = "event"
      )
      
      if (is.null(ed) || nrow(ed) == 0) {
        return()
      }
      
      linked_selection(as.character(ed$key))
    })
    
    observeEvent(input$btn_clear_selection, {
      linked_selection(NULL)
    })
    
    observeEvent(input$highlight_clusters, {
      if (length(input$highlight_clusters) > 0) {
        linked_selection(NULL)
      }
    })
    
    highlighted_ids <- reactive({
      req(shared_rv$cluster_labels)
      
      if (length(input$highlight_clusters) > 0) {
        names(shared_rv$cluster_labels)[shared_rv$cluster_labels %in% input$highlight_clusters]
      } else {
        linked_selection()
      }
    })
    
    apply_highlight_alpha <- function(df) {
      hl <- highlighted_ids()
      if (is.null(hl) || length(hl) == 0) {
        return(rep(0.85, nrow(df)))
      }
      ifelse(df$id %in% hl, 0.95, 0.08)
    }
    
    combined_spatial_df <- reactive({
      req(global_data$spatial_obj$sketch, global_data$spatial_obj$coords, shared_rv$cluster_labels)
      
      sk_ids <- colnames(global_data$spatial_obj$sketch)
      coords <- global_data$spatial_obj$coords
      
      df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]
      df$cluster <- as.character(shared_rv$cluster_labels[df$id])
      
      df[stats::complete.cases(df[, c("x", "y")]) & !is.na(df$cluster), ]
    })
    
    output$combined_spatial_plot <- plotly::renderPlotly({
      df <- combined_spatial_df()
      req(nrow(df) > 0)
      
      hist_ov <- histology_overlay()
      show_hist <- !is.null(hist_ov) &&
        !is.null(hist_ov$data_uri) &&
        isTRUE(input$show_histology)
      
      plot_images <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      p <- plotly::plot_ly(
        data = df,
        x = ~x,
        y = ~-y,
        color = ~cluster,
        colors = cluster_palette(df$cluster),
        key = ~id,
        type = "scattergl",
        mode = "markers",
        marker = list(
          size = 5,
          opacity = apply_highlight_alpha(df)
        ),
        source = ns("spatial_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          images = plot_images,
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = spatial_ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = spatial_ranges$y
          )
        )
      
      plotly::event_register(p, "plotly_selected")
    })
    
    output$combined_umap_plot <- plotly::renderPlotly({
      req(umap_df())
      
      emb <- umap_df()
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else {
        "sketch"
      }
      
      p <- plotly::plot_ly(
        data = emb,
        x = ~dim1,
        y = ~dim2,
        color = ~cluster,
        colors = cluster_palette(emb$cluster),
        key = ~id,
        type = "scattergl",
        mode = "markers",
        marker = list(
          size = 5,
          opacity = apply_highlight_alpha(emb)
        ),
        source = ns("umap_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE)
        )
      
      plotly::event_register(p, "plotly_selected")
    })
    
  })
}