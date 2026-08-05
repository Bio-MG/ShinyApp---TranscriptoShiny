# =============================================================================
# modules/spatial/mod_spatial_qc.R — QC & Spatial Autocorrelation (Moran's I)
# =============================================================================
# v3 (UX feedback): new "Apercu du jeu de donnees" tab, first in the strip —
# absorbs the dataset banner that used to sit above ALL tabs in mod_spatial.R
# (project/technology/counts/sketch size), now shown where it's contextually
# relevant instead of permanently pinned, plus a metadata table (sketch
# meta.data) that didn't exist anywhere before. Also added: %ribo histogram
# (was computed by compute_qc_metrics_fast() already but never plotted) and
# an nCount-vs-nFeature scatter colored by %MT (the classic QC diagnostic,
# complements the 4 histograms with the actual joint relationship).
#
# v2 (vignette coverage — Phase 2): added the "Top SVGs" small-multiples grid
# (facet_wrap of the top-N Moran's-I genes over the spatial sketch) — purely
# a visualization of results already computed by moran_task below, so it
# runs synchronously off global_data$spatial_obj$sketch (<=50k cells, no
# BPCells/disk access, no mirai needed — consistent with this project's
# convention of reserving async for genuinely heavy compute only).
#
# Two very different cost profiles, per spec:
#   1. QC metrics (nCount/nFeature/%MT/%ribo) — cheap, streamed straight off
#      the on-disk BPCells matrix (R/utils_spatial_io.R::compute_qc_metrics_fast()),
#      runs synchronously on the main thread (spec explicitly reserves async
#      only for Moran's I).
#   2. Moran's I spatial autocorrelation on the top ~1000 HVGs — genuinely
#      heavy, so it goes through ExtendedTask + mirai, isolated in a daemon
#      that reopens the BPCells matrix from disk (never receives the Seurat
#      object itself).
#
# Reuses Seurat's own FindSpatiallyVariableFeatures(selection.method="moransi")
# / SVFInfo() (verified against SeuratObject/Seurat source — see comments
# inline) rather than reimplementing Moran's I.
# =============================================================================

mod_spatial_qc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "QC & filtres", width = 350,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " Les seuils ci-dessous ne modifient pas les donnees sur disque : ils ",
          "definissent quels spots/cellules sont inclus dans le clustering et la ",
          "deconvolution. Ajustables a tout moment."),

      numericInput(ns("min_features"), "nFeature minimum", 200, min = 0, step = 10),
      numericInput(ns("min_count"), "nCount minimum", 100, min = 0, step = 10),
      sliderInput(ns("max_pct_mt"), "% Mitochondrial max", 0, 100, 20, step = 1),

      actionButton(ns("btn_apply_qc"), "Appliquer les seuils",
                   class = "btn-danger w-100 mt-2", icon = icon("filter")),
      uiOutput(ns("qc_pass_summary")),

      hr(),
      h6("Autocorrelation spatiale (Indice de Moran)", style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("cpu"),
          " Calcul asynchrone (mirai) sur les 1000 genes les plus variables — ",
          "n'interrompt pas votre session."),
      numericInput(ns("n_hvg_moran"), "Nombre de genes (HVG)", 1000, min = 100, max = 5000, step = 100),

      bslib::input_task_button(ns("btn_moran"), "Lancer l'autocorrelation spatiale",
                                icon = icon("wave-square")),
      verbatimTextOutput(ns("moran_progress_text"), placeholder = TRUE)
    ),

    navset_card_underline(
      nav_panel("Apercu du jeu de donnees",
                uiOutput(ns("dataset_overview_ui")),
                hr(),
                h6("Metadata (sketch)", style = "font-weight:bold;"),
                div(class = "text-muted small mb-2",
                    "Colonnes disponibles dans les metadonnees de l'objet Seurat ",
                    "(orig.ident, annotations importees, etc.) pour les elements du sketch en RAM."),
                DT::DTOutput(ns("metadata_table"))),

      nav_panel("Distributions QC",
                card(full_screen = TRUE,
                     plotOutput(ns("qc_hist_plot"), height = "480px")),
                card(full_screen = TRUE,
                     card_header("nCount vs nFeature (couleur = %MT)"),
                     plotOutput(ns("qc_scatter_plot"), height = "480px"))),

      nav_panel("Genes spatialement variables (Moran's I)",
                div(class = "alert alert-light", style = "font-size:0.78rem;",
                    "Grille des genes les plus spatialement structures (rang Moran's I) — ",
                    "necessite l'autocorrelation ci-contre (calculee au moins une fois)."),
                fluidRow(
                  column(6, numericInput(ns("n_top_svg"), "Nombre de genes (grille)", 9, min = 4, max = 20, step = 1)),
                  column(6, actionButton(ns("btn_svg_grid"), "Afficher la grille des top SVGs",
                                          class = "btn-sm btn-outline-primary mt-4", icon = icon("table-cells")))
                ),
                card(full_screen = TRUE, plotOutput(ns("svg_grid_plot"), height = "600px")),
                hr(),
                DT::DTOutput(ns("moran_table")))
    )
  )
}

mod_spatial_qc_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Apercu du jeu de donnees (ex-bandeau vert, deplace ici) ───────────
    output$dataset_overview_ui <- renderUI({
      if (is.null(global_data$spatial_obj)) {
        return(div(class = "alert alert-danger",
                    bsicons::bs_icon("exclamation-triangle"),
                    " Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'."))
      }
      obj <- global_data$spatial_obj
      disk_ok <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)
      norm_used  <- tryCatch(Seurat::DefaultAssay(obj$sketch), error = function(e) NA)
      norm_label <- if (identical(norm_used, "SCT")) "SCTransform" else "LogNormalize"

      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(title = "Echantillon", value = obj$project %||% "-",
                     showcase = bsicons::bs_icon("bookmark"), theme = "primary"),
          value_box(title = "Technologie", value = obj$technology,
                     showcase = bsicons::bs_icon("diagram-3"), theme = "secondary"),
          value_box(title = "Elements (total disque)", value = format(obj$n_total, big.mark = ","),
                     showcase = bsicons::bs_icon("grid-3x3"), theme = "info"),
          value_box(title = "Sketch (RAM)",
                     value = sprintf("%s (%s)", format(ncol(obj$sketch), big.mark = ","), norm_label),
                     showcase = bsicons::bs_icon("cpu"), theme = "light")
        ),
        if (!disk_ok) {
          div(class = "alert alert-warning mt-2",
              bsicons::bs_icon("exclamation-triangle"),
              " Donnees BPCells introuvables sur disque — le sketch reste utilisable pour la ",
              "visualisation, mais reimportez pour relancer clustering/deconvolution/Moran.")
        }
      )
    })

    output$metadata_table <- DT::renderDT({
      req(global_data$spatial_obj$sketch)
      meta <- global_data$spatial_obj$sketch@meta.data
      validate(need(ncol(meta) > 0, "Aucune metadata disponible pour ce jeu de donnees."))
      DT::datatable(meta, options = list(pageLength = 10, scrollX = TRUE), rownames = TRUE)
    })

    # ── Fast, synchronous QC metrics (recomputed whenever the object changes) ──
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$bpcells_dir)
      shared_rv$qc_metrics <- tryCatch(
        compute_qc_metrics_fast(global_data$spatial_obj$bpcells_dir),
        error = function(e) {
          showNotification(paste("Erreur calcul QC :", conditionMessage(e)), type = "error")
          NULL
        }
      )
    }, ignoreInit = TRUE)

    output$qc_hist_plot <- renderPlot({
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x = nCount)) +
        ggplot2::geom_histogram(bins = 50, fill = "#2C3E50") +
        ggplot2::geom_vline(xintercept = input$min_count, color = "red", linetype = "dashed") +
        ggplot2::labs(title = "nCount") + ggplot2::theme_minimal()
      p2 <- ggplot2::ggplot(df, ggplot2::aes(x = nFeature)) +
        ggplot2::geom_histogram(bins = 50, fill = "#18BC9C") +
        ggplot2::geom_vline(xintercept = input$min_features, color = "red", linetype = "dashed") +
        ggplot2::labs(title = "nFeature") + ggplot2::theme_minimal()
      p3 <- ggplot2::ggplot(df, ggplot2::aes(x = pct_mt)) +
        ggplot2::geom_histogram(bins = 50, fill = "#E74C3C") +
        ggplot2::geom_vline(xintercept = input$max_pct_mt, color = "red", linetype = "dashed") +
        ggplot2::labs(title = "% Mitochondrial") + ggplot2::theme_minimal()
      # NEW: was already computed by compute_qc_metrics_fast() but never
      # plotted anywhere -- completes the 4-metric QC picture.
      p4 <- ggplot2::ggplot(df, ggplot2::aes(x = pct_ribo)) +
        ggplot2::geom_histogram(bins = 50, fill = "#8E44AD") +
        ggplot2::labs(title = "% Ribosomal") + ggplot2::theme_minimal()
      patchwork::wrap_plots(p1, p2, p3, p4, ncol = 4)
    })

    # NEW: classic joint QC diagnostic (nCount vs nFeature, colored by %MT) —
    # the 4 histograms show marginal distributions only; this shows whether
    # low-nFeature spots are ALSO high-%MT (a real dying-cell/empty-spot
    # signature) or not (could just be genuinely low-complexity tissue).
    output$qc_scatter_plot <- renderPlot({
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      ggplot2::ggplot(df, ggplot2::aes(x = nCount, y = nFeature, color = pct_mt)) +
        ggplot2::geom_point(alpha = 0.6, size = 1.3) +
        ggplot2::geom_vline(xintercept = input$min_count, color = "red", linetype = "dashed") +
        ggplot2::geom_hline(yintercept = input$min_features, color = "red", linetype = "dashed") +
        ggplot2::scale_color_viridis_c(option = "inferno", direction = -1, na.value = "grey70") +
        ggplot2::labs(x = "nCount", y = "nFeature", color = "% MT") +
        ggplot2::theme_minimal(base_size = 12)
    })

    observeEvent(input$btn_apply_qc, {
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      pass <- with(df, nCount >= input$min_count & nFeature >= input$min_features &
                     (is.na(pct_mt) | pct_mt <= input$max_pct_mt))
      idx <- which(pass)
      attr(idx, "dataset") <- global_data$active_spatial_dataset
      attr(idx, "n_total") <- nrow(df)
      shared_rv$qc_pass_idx <- idx
      showNotification(sprintf("Seuils appliques : %d/%d elements conserves.",
                               sum(pass), length(pass)), type = "message", duration = 4)
    })

    output$qc_pass_summary <- renderUI({
      req(shared_rv$qc_pass_idx, shared_rv$qc_metrics)
      div(class = "alert alert-success", style = "font-size:0.8rem;",
          sprintf("%d / %d elements passent les seuils actuels.",
                   length(shared_rv$qc_pass_idx), nrow(shared_rv$qc_metrics)))
    })

    # ── Async: Moran's I on top HVGs (ExtendedTask + mirai) ────────────────
    log_file <- spatial_log_path(session, "moran")
    tracker  <- create_reactive_tracker(session, log_file)

    moran_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, n_hvg, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, nfeatures = n_hvg, verbose = FALSE)
          hvgs <- Seurat::VariableFeatures(obj)

          write_mirai_log(log_file, "Alignement des coordonnees spatiales...", 3, 5)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          coords_df <- coords_df[keep, , drop = FALSE]
          obj <- obj[, rownames(coords_df)]

          write_mirai_log(log_file, sprintf("Calcul de l'indice de Moran sur %d genes...", length(hvgs)), 4, 5)
          # Verified against Seurat source: FindSpatiallyVariableFeatures.Assay()
          # / .StdAssay() takes spatial.location directly (no FOV needed), and
          # SVFInfo() retrieves the per-gene statistics table afterwards.
          assay_res <- Seurat::FindSpatiallyVariableFeatures(
            object = obj[["RNA"]], layer = "data", features = hvgs,
            spatial.location = coords_df, selection.method = "moransi",
            nfeatures = length(hvgs), verbose = FALSE
          )
          info <- SeuratObject::SVFInfo(assay_res, method = "moransi")

          write_mirai_log(log_file, "Termine.", 5, 5)
          obs_col <- grep("observed$", colnames(info), value = TRUE)[1]
          pv_col  <- grep("p\\.value$|pvalue$", colnames(info), value = TRUE)[1]
          data.frame(
            gene     = rownames(info),
            moran_i  = if (!is.na(obs_col)) info[[obs_col]] else NA_real_,
            p_value  = if (!is.na(pv_col))  info[[pv_col]]  else NA_real_,
            row.names = NULL, stringsAsFactors = FALSE
          )
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        n_hvg = n_hvg, log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(moran_task, "btn_moran")

    observeEvent(input$btn_moran, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      pass_idx <- safe_pass_idx(shared_rv$qc_pass_idx, global_data$active_spatial_dataset,
                                global_data$spatial_obj$n_total)
      if (is.null(pass_idx) && !is.null(shared_rv$qc_pass_idx)) {
        showNotification("Seuils QC obsoletes pour cet echantillon -- indice de Moran calcule sans filtre QC.",
                         type = "warning", duration = 8)
      }
      reset_log(log_file)
      moran_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = pass_idx,
        coords      = global_data$spatial_obj$coords,
        n_hvg       = input$n_hvg_moran,
        log_file    = log_file
      )
    })
    observeEvent(moran_task$status(), {
      if (moran_task$status() == "success") {
        shared_rv$moran_results <- moran_task$result()
        showNotification("Autocorrelation spatiale terminee.", type = "message", duration = 4)
      } else if (moran_task$status() == "error") {
        showNotification(
          "Erreur (ou depassement du delai) pendant le calcul de Moran — voir le log. Essayez 'Reinitialiser les daemons' puis relancez.",
          type = "error", duration = 10)
      }
    })

    output$moran_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    output$moran_table <- DT::renderDT({
      req(shared_rv$moran_results)
      DT::datatable(shared_rv$moran_results, options = list(pageLength = 15), rownames = FALSE) |>
        DT::formatRound(c("moran_i", "p_value"), 4)
    })

    # ── Top SVGs grid (vignette parity: small-multiples of the top-N most
    # spatially structured genes). Sync — sketch is capped (<=50k) and N is
    # small (<=20 genes), so no mirai needed here (same convention as
    # everywhere else in this module: async is reserved for genuinely heavy
    # compute, not for visualizing results already computed).
    svg_grid_long <- eventReactive(input$btn_svg_grid, {
      req(shared_rv$moran_results, global_data$spatial_obj$sketch, global_data$spatial_obj$coords)

      ord <- order(-shared_rv$moran_results$moran_i)
      top_genes <- utils::head(shared_rv$moran_results$gene[ord], input$n_top_svg)
      top_genes <- intersect(top_genes, rownames(global_data$spatial_obj$sketch))
      validate(need(length(top_genes) > 0,
                    paste("Aucun des genes les mieux classes (Moran's I) n'est present dans le sketch (RAM).",
                          "Relancez l'autocorrelation ou reduisez N.")))

      sk <- global_data$spatial_obj$sketch
      if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
      expr_mat <- as.matrix(SeuratObject::LayerData(sk, layer = "data")[top_genes, , drop = FALSE])

      coords  <- global_data$spatial_obj$coords
      base_df <- coords[match(colnames(sk), coords$id), c("id", "x", "y")]

      long <- do.call(rbind, lapply(top_genes, function(g) {
        data.frame(base_df, gene = g, expr = as.numeric(expr_mat[g, base_df$id]))
      }))
      long <- long[stats::complete.cases(long[, c("x", "y")]), ]
      long$gene <- factor(long$gene, levels = top_genes)  # preserve Moran's I rank order across facets
      long
    })

    output$svg_grid_plot <- renderPlot({
      long <- svg_grid_long()
      p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = -y, color = expr))
      if (requireNamespace("scattermore", quietly = TRUE)) {
        p <- p + scattermore::geom_scattermore(pointsize = 2.5)
      } else {
        p <- p + ggplot2::geom_point(size = 0.4)
      }
      p + ggplot2::facet_wrap(~gene, ncol = 3) +
        ggplot2::scale_color_viridis_c(option = "plasma") +
        ggplot2::coord_fixed() + ggplot2::theme_void() +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 10)) +
        ggplot2::labs(color = "Expression",
                      title = sprintf("Top %d genes spatialement variables (indice de Moran)",
                                       length(unique(long$gene))))
    })
  })
}
