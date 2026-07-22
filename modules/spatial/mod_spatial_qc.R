# =============================================================================
# modules/spatial/mod_spatial_qc.R — QC & Spatial Autocorrelation (Moran's I)
# =============================================================================
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
      nav_panel("Distributions QC",
                plotOutput(ns("qc_hist_plot"), height = "500px")),
      nav_panel("Genes spatialement variables (Moran's I)",
                DT::DTOutput(ns("moran_table")))
    )
  )
}

mod_spatial_qc_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      patchwork::wrap_plots(p1, p2, p3, ncol = 3)
    })

    observeEvent(input$btn_apply_qc, {
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      pass <- with(df, nCount >= input$min_count & nFeature >= input$min_features &
                     (is.na(pct_mt) | pct_mt <= input$max_pct_mt))
      shared_rv$qc_pass_idx <- which(pass)
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
      reset_log(log_file)
      moran_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = shared_rv$qc_pass_idx,
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
  })
}
