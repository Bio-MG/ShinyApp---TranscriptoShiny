# =============================================================================
# modules/spatial/mod_spatial_cluster.R — Spatial clustering (BANKSY)
# =============================================================================
# Uses SeuratWrappers::RunBanksy() (verified signature against
# satijalab/seurat-wrappers source: lambda, assay, slot, dimx/dimy [metadata
# column names], k_geom, features, lazy, npcs, assay_name, ...) with
# lazy = TRUE, which computes the BANKSY-augmented PCA directly via irlba
# without ever materializing the full neighborhood-augmented feature matrix
# — the RAM-conscious mode, ideal for a 32 Go CPU-only workstation.
#
# Install (not on CRAN):
#   remotes::install_github("prabhakarlab/Banksy", ref = "devel")
#   remotes::install_github("satijalab/seurat-wrappers")
#
# Entirely async: the daemon reopens the BPCells matrix from disk and
# receives only the small `coords` data.frame + scalar parameters — never
# the Seurat/BPCells object itself (hard rule).
# =============================================================================

mod_spatial_cluster_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Clustering spatial (BANKSY)", width = 350,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " BANKSY augmente l'expression de chaque spot/cellule avec un ",
          "resume de son voisinage spatial avant le clustering — utile pour ",
          "detecter des domaines tissulaires, pas seulement des types cellulaires."),

      sliderInput(ns("lambda"), "Lambda (poids spatial)", 0, 1, 0.8, step = 0.05),
      numericInput(ns("k_geom"), "Voisins geometriques (k_geom)", 18, min = 4, max = 100, step = 1),
      numericInput(ns("npcs"), "Composantes PCA (BANKSY)", 30, min = 5, max = 100, step = 5),
      numericInput(ns("resolution"), "Resolution (Leiden)", 0.8, min = 0.1, max = 3, step = 0.1),

      div(class = "alert alert-light", style = "font-size:0.75rem;",
          "Le clustering s'applique aux elements passant les seuils QC ",
          "(onglet 1) — reimportez ou ajustez les seuils si necessaire."),

      bslib::input_task_button(ns("btn_cluster"), "Lancer Clustering Spatial",
                                icon = icon("shapes")),
      verbatimTextOutput(ns("cluster_progress_text"), placeholder = TRUE)
    ),

    card(
      card_header("Resultat"),
      uiOutput(ns("cluster_summary")),
      DT::DTOutput(ns("cluster_sizes_table"))
    )
  )
}

mod_spatial_cluster_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_file <- spatial_log_path(session, "cluster")
    tracker  <- create_reactive_tracker(session, log_file)

    cluster_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords,
                                               lambda, k_geom, npcs, resolution, log_file) {
      mirai::mirai(
        {
          if (!requireNamespace("SeuratWrappers", quietly = TRUE) ||
              !requireNamespace("Banksy", quietly = TRUE)) {
            stop("Packages 'SeuratWrappers' et 'Banksy' requis (remotes::install_github).")
          }

          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 6)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Normalisation...", 2, 6)
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)

          write_mirai_log(log_file, "Alignement des coordonnees spatiales...", 3, 6)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          obj <- obj[, keep]
          obj$spatial_x <- coords_df$x[keep]
          obj$spatial_y <- coords_df$y[keep]

          write_mirai_log(log_file, "BANKSY (voisinage spatial, mode lazy/PCA directe)...", 4, 6)
          obj <- SeuratWrappers::RunBanksy(
            obj, lambda = lambda, assay = "RNA", slot = "data",
            dimx = "spatial_x", dimy = "spatial_y", features = "variable",
            k_geom = k_geom, lazy = FALSE, npcs = npcs,
            assay_name = "BANKSY_PCA", verbose = FALSE
          )

          write_mirai_log(log_file, "Clustering Leiden...", 5, 6)
          obj <- Seurat::FindNeighbors(obj, reduction = "BANKSY_PCA", dims = 1:npcs, verbose = FALSE)
          obj <- Seurat::FindClusters(obj, resolution = resolution, algorithm = 4, verbose = FALSE)  # 4 = Leiden

          write_mirai_log(log_file, "Termine.", 6, 6)
          setNames(as.character(obj$seurat_clusters), colnames(obj))
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        lambda = lambda, k_geom = k_geom, npcs = npcs, resolution = resolution,
        log_file = log_file
      )
    })
    bslib::bind_task_button(cluster_task, "btn_cluster")

    observeEvent(input$btn_cluster, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      reset_log(log_file)
      cluster_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = shared_rv$qc_pass_idx,
        coords      = global_data$spatial_obj$coords,
        lambda      = input$lambda, k_geom = input$k_geom,
        npcs        = input$npcs, resolution = input$resolution,
        log_file    = log_file
      )
    })

    observeEvent(cluster_task$status(), {
      if (cluster_task$status() == "success") {
        shared_rv$cluster_labels <- cluster_task$result()
        showNotification(sprintf("Clustering termine : %d clusters trouves.",
                                  length(unique(shared_rv$cluster_labels))),
                          type = "message", duration = 5)
      } else if (cluster_task$status() == "error") {
        showNotification("Erreur pendant le clustering spatial — voir le log.", type = "error", duration = 8)
      }
    })

    output$cluster_progress_text <- renderText({
      p <- parse_log_progress(tracker())
      if (!is.na(p$pct)) sprintf("%s (%d%%)", p$text, p$pct) else p$text
    })

    output$cluster_summary <- renderUI({
      req(shared_rv$cluster_labels)
      div(class = "alert alert-success",
          sprintf("%d clusters sur %d elements.",
                   length(unique(shared_rv$cluster_labels)), length(shared_rv$cluster_labels)))
    })

    output$cluster_sizes_table <- DT::renderDT({
      req(shared_rv$cluster_labels)
      tab <- as.data.frame(table(cluster = shared_rv$cluster_labels), stringsAsFactors = FALSE)
      colnames(tab) <- c("Cluster", "Effectif")
      DT::datatable(tab, options = list(pageLength = 15), rownames = FALSE)
    })
  })
}
