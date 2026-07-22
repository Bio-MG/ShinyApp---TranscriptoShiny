# =============================================================================
# modules/spatial/mod_spatial_viz.R — Rendering engine (WebGL + raster)
# =============================================================================
# v3 (post-test-4):
#   - FIX: leaflet::colorNumeric() throws ("Wasn't able to determine range of
#     domain") when the color column is entirely NA (id mismatch between
#     sketch and cluster/deconv results) — min()/max() on an empty vector
#     return +-Inf, which colorNumeric rejects. Now falls back to solid grey
#     instead of crashing (which used to cascade and blank out the whole tab
#     — same shared-reactive-error pattern fixed for plot_df() previously).
#   - FIX: leaflet::colorFactor("Set2", ...) only supports 8 colors; BANKSY
#     clustering can easily produce more domains. Switched to
#     grDevices::hcl.colors(), which generates exactly as many distinct
#     colors as needed.
#   - NEW: PNG (static, scattermore/ggplot) + CSV export of the current view
#     — was entirely missing.
#
# Renders ONLY global_data$spatial_obj$sketch (<= 50k elements, in-RAM) plus
# whatever shared_rv$cluster_labels / shared_rv$deconv_props have been
# computed for those same ids — never touches bpcells_dir directly.
# =============================================================================

mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Visualisation", width = 320,

      uiOutput(ns("engine_status_ui")),

      selectInput(ns("color_by"), "Colorer par",
                  choices = c("Metrique QC" = "qc",
                              "Cluster spatial" = "cluster",
                              "Type cellulaire (deconvolution)" = "deconv",
                              "Gene" = "gene"),
                  selected = "qc"),
      conditionalPanel(condition = sprintf("input['%s'] == 'qc'", ns("color_by")),
                        selectInput(ns("qc_metric"), NULL,
                                    choices = c("nCount", "nFeature", "pct_mt", "pct_ribo"))),
      conditionalPanel(condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
                        selectizeInput(ns("gene"), NULL, choices = NULL,
                                       options = list(maxOptions = 3000, placeholder = "Rechercher un gene..."))),
      conditionalPanel(condition = sprintf("input['%s'] == 'deconv'", ns("color_by")),
                        uiOutput(ns("deconv_celltype_ui"))),

      sliderInput(ns("pt_radius"), "Taille des points", 1, 20, 6, step = 1),
      checkboxInput(ns("show_polygons"), "Afficher les limites cellulaires au zoom (Xenium/CosMx)", value = TRUE),

      div(class = "border-top pt-2 mt-2",
          downloadButton(ns("dl_png"), "Export PNG", class = "btn-sm btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_csv"), "Export CSV (donnees affichees)", class = "btn-sm btn-outline-secondary w-100")),

      hr(),
      bslib::input_task_button(ns("btn_compute_umap"), "Calculer PCA + UMAP (sketch)",
                                icon = icon("chart-line")),
      verbatimTextOutput(ns("umap_progress_text"), placeholder = TRUE)
    ),

    navset_card_underline(
      nav_panel("Carte spatiale (WebGL)",
                uiOutput(ns("map_ui"))),
      nav_panel("UMAP (non-spatial, sketch)",
                plotly::plotlyOutput(ns("umap_plot"), height = "650px"))
    )
  )
}

mod_spatial_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    use_leafgl <- requireNamespace("leaflet", quietly = TRUE) && requireNamespace("leafgl", quietly = TRUE) &&
      requireNamespace("sf", quietly = TRUE)

    output$engine_status_ui <- renderUI({
      if (use_leafgl) {
        div(class = "alert alert-success", style = "font-size:0.72rem;padding:4px 8px;",
            "Moteur : leaflet + leafgl (WebGL)")
      } else {
        missing_pkgs <- c("leaflet", "leafgl", "sf")[!c(
          requireNamespace("leaflet", quietly = TRUE),
          requireNamespace("leafgl", quietly = TRUE),
          requireNamespace("sf", quietly = TRUE))]
        div(class = "alert alert-warning", style = "font-size:0.72rem;padding:4px 8px;",
            sprintf("Moteur : repli scattermore (package(s) manquant(s) : %s)",
                    paste(missing_pkgs, collapse = ", ")))
      }
    })

    output$map_ui <- renderUI({
      if (use_leafgl) leaflet::leafletOutput(ns("leaf_map"), height = "650px")
      else plotOutput(ns("raster_map"), height = "650px")
    })

    # ── Populate gene / cell-type selectors from the sketch ─────────────
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$sketch)
      updateSelectizeInput(session, "gene", choices = rownames(global_data$spatial_obj$sketch), server = TRUE)
    }, ignoreInit = TRUE)

    output$deconv_celltype_ui <- renderUI({
      req(shared_rv$deconv_props)
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      selectInput(ns("deconv_celltype"), NULL, choices = cts)
    })

    # ── Build the per-sketch-cell plotting data.frame (id, x, y, value) ──
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
        if (inherits(e, "shiny.silent.error")) stop(e)  # req() not-ready-yet: let it behave normally
        warning("plot_df(): echec du calcul de couleur (", conditionMessage(e), ") — points affiches en gris.")
        rep(NA_real_, nrow(df))
      })
      df[stats::complete.cases(df[, c("x", "y")]), ]
    })

    # ── Robust color mapping: never let a bad/empty domain crash the map ──
    # (FIX: id mismatches -> all-NA `value` used to throw inside
    # colorNumeric ; >8 cluster levels used to silently degrade with Set2.)
    color_values <- function(df) {
      if (is.numeric(df$value)) {
        vals <- df$value[is.finite(df$value)]
        if (length(vals) == 0) return(rep("#CCCCCC", nrow(df)))
        pal <- leaflet::colorNumeric("viridis", domain = range(vals), na.color = "#CCCCCC")
        pal(df$value)
      } else {
        lv <- sort(unique(stats::na.omit(df$value)))
        if (length(lv) == 0) return(rep("#CCCCCC", nrow(df)))
        pal <- leaflet::colorFactor(grDevices::hcl.colors(length(lv), palette = "Dark 3"),
                                     domain = lv, na.color = "#CCCCCC")
        pal(df$value)
      }
    }

    # ── leafgl / WebGL primary map ───────────────────────────────────────
    output$leaf_map <- leaflet::renderLeaflet({
      req(use_leafgl)
      leaflet::leaflet(options = leaflet::leafletOptions(
        crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple"), minZoom = -5, maxZoom = 8
      ))
    })

    observe({
      req(use_leafgl)
      df <- plot_df()
      req(nrow(df) > 0)

      pts  <- sf::st_as_sf(df, coords = c("x", "y"))
      cols <- color_values(df)

      leaflet::leafletProxy("leaf_map") |>
        leafgl::clearGlLayers() |>
        leafgl::addGlPoints(data = pts, fillColor = cols, radius = input$pt_radius,
                             popup = TRUE, group = "spatial") |>
        leaflet::fitBounds(lng1 = min(df$x), lat1 = min(df$y),
                            lng2 = max(df$x), lat2 = max(df$y))
    })

    observeEvent(input$leaf_map_zoom, {
      req(use_leafgl, isTRUE(input$show_polygons))
      tech <- global_data$spatial_obj$technology
      if (is.null(tech) || tech == "visium") return(invisible(NULL))
      if (is.null(input$leaf_map_zoom) || input$leaf_map_zoom < 3) return(invisible(NULL))
      b <- input$leaf_map_bounds
      req(b)
      shared_rv$current_fov_crop <- list(x = c(b$west, b$east), y = c(b$south, b$north))
      # Polygon rendering itself (crop_fov_bbox() -> sf -> addGlPolygons) is
      # left as a follow-up — see README evolutivite notes.
    })

    # ── scattermore fallback (also reused for PNG export below) ──────────
    build_raster_plot <- function(df) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = value))
      if (!requireNamespace("scattermore", quietly = TRUE)) {
        p <- p + ggplot2::geom_point(size = input$pt_radius / 4)
      } else {
        p <- p + scattermore::geom_scattermore(pointsize = input$pt_radius)
      }
      p + ggplot2::coord_fixed() + ggplot2::theme_void() +
        (if (is.numeric(df$value)) ggplot2::scale_color_viridis_c(na.value = "#CCCCCC")
         else ggplot2::scale_color_manual(values = stats::setNames(
           grDevices::hcl.colors(length(unique(stats::na.omit(df$value))), "Dark 3"),
           sort(unique(stats::na.omit(df$value)))), na.value = "#CCCCCC")) +
        ggplot2::labs(color = input$color_by)
    }

    output$raster_map <- renderPlot({
      req(!use_leafgl)
      df <- plot_df()
      req(nrow(df) > 0)
      build_raster_plot(df)
    })

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

          write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
          if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
          sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
          sk <- Seurat::ScaleData(sk, verbose = FALSE)

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

      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cluster,
                       type = "scattergl", mode = "markers",
                       marker = list(size = 5, opacity = 0.7))
    })
  })
}
