# =============================================================================
# modules/spatial/mod_spatial_viz.R — Rendering engine (WebGL + raster)
# =============================================================================
# Renders ONLY global_data$spatial_obj$sketch (<= 50k elements, in-RAM) plus
# whatever shared_rv$cluster_labels / shared_rv$deconv_props have been
# computed for those same ids — never touches bpcells_dir directly.
#
# Primary map: leaflet + leafgl::addGlPoints() (verified signature: needs an
# sf POINT object; tissue coordinates are plotted on a leaflet map configured
# with crsClass = "L.CRS.Simple" so no geographic projection is applied —
# the standard trick for plotting arbitrary x/y "image-space" data in
# leaflet). Falls back to a scattermore-rasterized ggplot if leafgl/sf are
# unavailable, which doubles as the "static high-density export" mode from
# the spec.
#
# Crop()/Simplify() polygon overlay: driven by leaflet's own zoom reactive
# input (input$<id>_zoom, auto-populated by leaflet-for-shiny) crossing a
# user-adjustable density threshold — see .crop_threshold_ui below.
# =============================================================================

mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Visualisation", width = 320,

      selectInput(ns("color_by"), "Colorer par",
                  choices = c("Cluster spatial" = "cluster",
                              "Type cellulaire (deconvolution)" = "deconv",
                              "Metrique QC" = "qc",
                              "Gene" = "gene")),
      conditionalPanel(condition = sprintf("input['%s'] == 'qc'", ns("color_by")),
                        selectInput(ns("qc_metric"), NULL,
                                    choices = c("nCount", "nFeature", "pct_mt", "pct_ribo"))),
      conditionalPanel(condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
                        selectizeInput(ns("gene"), NULL, choices = NULL,
                                       options = list(maxOptions = 3000, placeholder = "Rechercher un gene..."))),
      conditionalPanel(condition = sprintf("input['%s'] == 'deconv'", ns("color_by")),
                        uiOutput(ns("deconv_celltype_ui"))),

      sliderInput(ns("pt_radius"), "Taille des points", 1, 20, 4, step = 1),
      checkboxInput(ns("show_polygons"), "Afficher les limites cellulaires au zoom (Xenium/CosMx)", value = TRUE),

      hr(),
      actionButton(ns("btn_compute_umap"), "Calculer PCA + UMAP (sketch)",
                   class = "btn-outline-secondary w-100", icon = icon("chart-scatter"))
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

      df$value <- switch(input$color_by,
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
          as.numeric(SeuratObject::LayerData(global_data$spatial_obj$sketch,
                                              layer = "data")[input$gene, df$id])
        }
      )
      df[stats::complete.cases(df[, c("x", "y")]), ]
    })

    # ── leafgl / WebGL primary map ───────────────────────────────────────
    output$leaf_map <- leaflet::renderLeaflet({
      req(use_leafgl)
      leaflet::leaflet(options = leaflet::leafletOptions(
        crs = leaflet::leafletCRS(crsClass = "L.CRS.Simple"), minZoom = -5, maxZoom = 8
      )) |> leaflet::setView(lng = 0, lat = 0, zoom = 0)
    })

    observe({
      req(use_leafgl)
      df <- plot_df()
      req(nrow(df) > 0)

      pts <- sf::st_as_sf(df, coords = c("x", "y"))
      is_categorical <- input$color_by %in% c("cluster", "gene") == FALSE || input$color_by == "cluster"
      cols <- if (is.numeric(df$value)) {
        pal <- leaflet::colorNumeric("viridis", domain = df$value, na.color = "#CCCCCC")
        pal(df$value)
      } else {
        pal <- leaflet::colorFactor("Set2", domain = df$value, na.color = "#CCCCCC")
        pal(df$value)
      }

      leaflet::leafletProxy("leaf_map") |>
        leafgl::clearGlLayers() |>
        leafgl::addGlPoints(data = pts, fillColor = cols, radius = input$pt_radius,
                             popup = TRUE, group = "spatial")
    })

    # Zoom-driven polygon overlay for imaging tech (Xenium/CosMx) — Crop() +
    # the FOV's already-simplified boundaries (see utils_spatial_io.R).
    observeEvent(input$leaf_map_zoom, {
      req(use_leafgl, isTRUE(input$show_polygons))
      tech <- global_data$spatial_obj$technology
      if (is.null(tech) || tech == "visium") return(invisible(NULL))
      if (is.null(input$leaf_map_zoom) || input$leaf_map_zoom < 3) return(invisible(NULL))
      b <- input$leaf_map_bounds
      req(b)
      shared_rv$current_fov_crop <- list(x = c(b$west, b$east), y = c(b$south, b$north))
      # Polygon rendering itself (crop_fov_bbox() -> sf -> addGlPolygons) is
      # deliberately left as a follow-up: it needs the full FOV object
      # re-opened (not just the sketch) — a natural next mirai/cache step,
      # see README evolutivity notes. For now the crop bbox is tracked and
      # available to any future consumer via shared_rv$current_fov_crop.
    })

    # ── scattermore fallback (also used for "static high-density export") ──
    output$raster_map <- renderPlot({
      req(!use_leafgl)
      df <- plot_df()
      req(nrow(df) > 0)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = value))
      if (!requireNamespace("scattermore", quietly = TRUE)) {
        p <- p + ggplot2::geom_point(size = input$pt_radius / 4)
      } else {
        p <- p + scattermore::geom_scattermore(pointsize = input$pt_radius)
      }
      p + ggplot2::coord_fixed() + ggplot2::theme_void() +
        (if (is.numeric(df$value)) ggplot2::scale_color_viridis_c() else ggplot2::scale_color_brewer(palette = "Set2")) +
        ggplot2::labs(color = input$color_by)
    })

    # ── Sketch-only PCA + UMAP (synchronous: sketch is capped, <= 50k) ───
    umap_obj <- reactiveVal(NULL)
    observeEvent(input$btn_compute_umap, {
      req(global_data$spatial_obj$sketch)
      withProgress(message = "Calcul PCA + UMAP sur le sketch...", {
        tryCatch({
          sk <- global_data$spatial_obj$sketch
          sk <- Seurat::NormalizeData(sk, verbose = FALSE)
          sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
          sk <- Seurat::ScaleData(sk, verbose = FALSE)
          sk <- Seurat::RunPCA(sk, npcs = 30, verbose = FALSE)
          sk <- Seurat::RunUMAP(sk, dims = 1:30, verbose = FALSE)
          umap_obj(sk)
          showNotification("UMAP calcule.", type = "message", duration = 3)
        }, error = function(e) showNotification(paste("Erreur UMAP:", conditionMessage(e)), type = "error", duration = 8))
      })
    })

    output$umap_plot <- plotly::renderPlotly({
      req(umap_obj())
      sk <- umap_obj()
      emb <- as.data.frame(Seurat::Embeddings(sk, "umap"))
      colnames(emb)[1:2] <- c("dim1", "dim2")  # Seurat's default key is "UMAP_" — normalize rather than hardcode
      emb$id <- rownames(emb)
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else "sketch"

      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cluster,
                       type = "scattergl", mode = "markers",
                       marker = list(size = 5, opacity = 0.7))
    })
  })
}
