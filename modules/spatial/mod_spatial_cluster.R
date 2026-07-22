# =============================================================================
# modules/spatial/mod_spatial_cluster.R — Spatial clustering ("BANKSY-lite")
# =============================================================================
# REWRITE (post-test-3): SeuratWrappers::RunBanksy() / Banksy::computeHarmonics()
# tries to spawn nested parallel worker processes from inside the mirai
# daemon (parallel::makeCluster-style) -- fragile in general, and observed to
# hang indefinitely on a Windows project path containing spaces/brackets.
# Rather than keep chasing Banksy/SeuratWrappers version compatibility, this
# reimplements BANKSY's core idea directly, with zero nested parallelism:
#   1. k-NN in PHYSICAL (x,y) space (RANN::nn2 — single-threaded C++, no
#      cluster spawning).
#   2. For each spot, average its neighbors' (log-normalized) expression —
#      done as ONE sparse-matrix multiply (same "indicator matrix" trick
#      already used elsewhere in this project, e.g. helpers_sc.R's
#      remap_seurat_ids_to_symbol()), not a per-row loop.
#   3. Augmented feature matrix = [sqrt(1-lambda)*scale(own),
#      sqrt(lambda)*scale(neighbor_mean)] — BANKSY's own augmentation
#      formula (own expression vs. neighborhood mean, weighted by lambda),
#      minus the optional azimuthal-Gabor-filter term (a refinement, not the
#      core mechanism).
#   4. PCA (irlba, truncated/fast) on the augmented matrix, then Seurat's
#      OWN FindNeighbors()/FindClusters(algorithm=4, Leiden) — mature,
#      already used successfully elsewhere in this app (mod_sc_pipeline.R),
#      no external nested-parallel dependency.
#
# Install: install.packages(c("RANN", "irlba")) — both lightweight, no
# GitHub-only / version-fragile dependency chain anymore.
#
# Entirely async: the daemon reopens the BPCells matrix from disk and
# receives only the small `coords` data.frame + scalar parameters — never
# the Seurat/BPCells object itself (hard rule). A MIRAI_TASK_TIMEOUT_MS
# ceiling (R/utils_spatial_async.R) guarantees this never hangs forever.
# =============================================================================

mod_spatial_cluster_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Clustering spatial (BANKSY-lite)", width = 350,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " Augmente l'expression de chaque spot/cellule avec la moyenne de ",
          "son voisinage spatial avant le clustering — utile pour detecter ",
          "des domaines tissulaires, pas seulement des types cellulaires."),

      sliderInput(ns("lambda"), "Lambda (poids spatial)", 0, 1, 0.8, step = 0.05),
      numericInput(ns("k_geom"), "Voisins geometriques (k_geom)", 18, min = 4, max = 100, step = 1),
      numericInput(ns("npcs"), "Composantes PCA", 30, min = 5, max = 100, step = 5),
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
          if (!requireNamespace("RANN", quietly = TRUE)) {
            stop("Package 'RANN' requis (install.packages('RANN')).")
          }

          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 8)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 8)
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
          var_feat <- Seurat::VariableFeatures(obj)

          write_mirai_log(log_file, "Alignement des coordonnees spatiales...", 3, 8)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          obj <- obj[, keep]
          coords_mat <- as.matrix(coords_df[keep, , drop = FALSE])

          write_mirai_log(log_file, sprintf("Voisinage spatial (k=%d, RANN)...", k_geom), 4, 8)
          nn <- RANN::nn2(coords_mat, k = min(k_geom + 1, nrow(coords_mat)))
          neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]  # drop self (1st column, distance 0)

          write_mirai_log(log_file, "Augmentation BANKSY-lite (moyenne du voisinage)...", 5, 8)
          own_mat <- t(as.matrix(SeuratObject::LayerData(obj, layer = "data")[var_feat, , drop = FALSE]))
          n  <- nrow(own_mat)
          kk <- ncol(neighbor_idx)
          # ONE sparse-matrix multiply instead of a per-row loop (same trick
          # as helpers_sc.R::remap_seurat_ids_to_symbol()'s duplicate collapse).
          W <- Matrix::sparseMatrix(
            i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)),
            x = 1 / kk, dims = c(n, n)
          )
          nbr_mat <- as.matrix(W %*% own_mat)
          dimnames(nbr_mat) <- dimnames(own_mat)

          own_scaled <- scale(own_mat); own_scaled[!is.finite(own_scaled)] <- 0
          nbr_scaled <- scale(nbr_mat); nbr_scaled[!is.finite(nbr_scaled)] <- 0
          augmented  <- cbind(sqrt(1 - lambda) * own_scaled, sqrt(lambda) * nbr_scaled)

          write_mirai_log(log_file, "PCA sur la matrice augmentee...", 6, 8)
          n_pc <- max(2, min(npcs, ncol(augmented) - 1, nrow(augmented) - 1))
          pca <- if (requireNamespace("irlba", quietly = TRUE)) {
            irlba::prcomp_irlba(augmented, n = n_pc, center = FALSE, scale. = FALSE)
          } else {
            stats::prcomp(augmented, rank. = n_pc, center = FALSE, scale. = FALSE)
          }
          emb <- pca$x
          rownames(emb) <- colnames(obj)
          colnames(emb) <- paste0("BANKSYPCA_", seq_len(ncol(emb)))
          obj[["BANKSY_PCA"]] <- Seurat::CreateDimReducObject(
            embeddings = emb, key = "BANKSYPCA_", assay = Seurat::DefaultAssay(obj)
          )

          write_mirai_log(log_file, "Recherche des voisins (graphe)...", 7, 8)
          obj <- Seurat::FindNeighbors(obj, reduction = "BANKSY_PCA", dims = seq_len(n_pc), verbose = FALSE)

          write_mirai_log(log_file, "Clustering Leiden...", 8, 8)
          obj <- tryCatch(
            Seurat::FindClusters(obj, resolution = resolution, algorithm = 4, verbose = FALSE),
            error = function(e) {
              write_mirai_log(log_file, paste("Leiden indisponible (", conditionMessage(e),
                                               ") - repli sur Louvain."), 8, 8)
              Seurat::FindClusters(obj, resolution = resolution, algorithm = 1, verbose = FALSE)
            }
          )

          write_mirai_log(log_file, "Termine.", 8, 8)
          setNames(as.character(obj$seurat_clusters), colnames(obj))
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        lambda = lambda, k_geom = k_geom, npcs = npcs, resolution = resolution,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
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
        showNotification(
          "Erreur (ou depassement du delai de 20 min) pendant le clustering — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez.",
          type = "error", duration = 12)
      }
    })

    # Full log history (not just the last line) — fast tasks on small
    # datasets can otherwise complete between two 1000ms polls, hiding every
    # intermediate step.
    output$cluster_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
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
