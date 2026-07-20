# mod_sc_pipeline.R  —  Child 1: QC → Norm → PCA → Harmony → Clusters → UMAP
# Step-3.6: relaxed QC defaults for test datasets; actionable QC error messages.
# Step-3.7:
#   - Secondary t-SNE always computed (capped) alongside PCA/UMAP so it's
#     available in "3. Visualisation -> Réduction à visualiser" without an
#     extra manual run, mirroring PCA/UMAP's "always on" availability.
#   - "Max cellules/cluster pour analyses lourdes" slider -> shared_rv$max_cells_heavy,
#     read by mod_sc_markers.R / mod_sc_corr.R (and the auto-pipeline in mod_sc.R)
#     to cap FindAllMarkers / find_correlated_genes on large datasets.

.get_mt_pattern <- function(obj) {
  genes <- rownames(obj)
  if (any(grepl("^MT-",  genes, ignore.case=FALSE))) return("^MT-")
  if (any(grepl("^mt-",  genes, ignore.case=FALSE))) return("^mt-")
  if (any(grepl("^MT-",  genes, ignore.case=TRUE)))  return("^MT-")
  NULL
}

.with_sequential_future <- function(expr) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add=TRUE)
  future::plan("sequential")
  force(expr)
}

.is_big_dataset <- function(obj) ncol(obj) > 100000

# t-SNE is quadratic-ish in cost vs UMAP's approximate NN — cap the automatic
# "secondary" run so it never becomes the bottleneck of a routine pipeline run
# on a 32Go CPU-only machine. Users who explicitly pick "t-SNE" as their PRIMARY
# reduction method below are not capped (informed, deliberate choice).
.AUTO_TSNE_MAX_CELLS <- 30000L

# Step-3.7A: default cell-count threshold above which "Auto" mode switches the
# pipeline to a disk-backed (BPCells) counts layer instead of RAM. Below this,
# a sparse dgCMatrix comfortably fits in 32Go even with generous headroom for
# ScaleData()/RunPCA() intermediates -- BPCells' overhead (slower per-access,
# disk I/O) only pays off once RAM would otherwise be the bottleneck.
.BPCELLS_AUTO_THRESHOLD <- 150000L

# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_pipeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # QC filters — relaxed defaults for broader compatibility
    layout_columns(
      numericInput(ns("qc_min_gene"), "Min Gènes",  100,  step=50),   # was 200
      numericInput(ns("qc_max_gene"), "Max Gènes",  8000, step=500)   # was 6000
    ),
    sliderInput(ns("qc_mt"), "% Mito Max", 0, 50, 20, step=1),        # was 5
    div(class="alert alert-light", style="font-size:0.82em;padding:6px;",
        bsicons::bs_icon("info-circle"),
        " Valeurs conseillées pour jeux de test réduits :\n",
        "Min gènes = 50, Max gènes = 10 000, % Mito = 40."),
    # --- UI: sketch size preset selector ---
    selectInput(
      ns("sketch_preset"),
      "Taille du sketch (vitesse vs precision)",
      choices = list(
        "Rapide (test, 5 000 cellules)"   = "fast",
        "Leger (10 000 cellules)"         = "light",
        "Moyen (25 000 cellules)"         = "medium",
        "Standard (50 000 cellules)"      = "standard",
        "Eleve (100 000 cellules)"        = "high",
        "Max (dataset complet)"           = "max",
        "Personnalise"                    = "custom"
      ),
      selected = "standard"
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("sketch_preset")),
      numericInput(ns("sketch_ncells_custom"), "Nombre de cellules (sketch)",
                   value = 20000, min = 1000, max = 500000, step = 1000)
    ),
    hr(),
    h6("Normalisation"),
    radioButtons(ns("norm_method"), NULL,
      choices=c("LogNormalize (Standard)"="log","SCTransform (Avancé)"="sct")),
    hr(),
    h6("Réduction Dimensionnelle"),
    selectInput(ns("reduction_method"), "Méthode de Réduction (principale)",
      choices=c("UMAP"="umap","PCA"="pca","t-SNE"="tsne",
                "Diffusion Maps"="dm","Harmony"="harmony"), selected="umap"),
    div(class="small text-muted mb-2",
        "t-SNE est en plus toujours calculé automatiquement en secondaire (si < ",
        format(.AUTO_TSNE_MAX_CELLS, big.mark=" "),
        " cellules) pour être disponible dans l'onglet Visualisation."),
    layout_columns(
      sliderInput(ns("pca_dim"),    "Dims PCA",   5, 50, 20),
      numericInput(ns("clust_res"), "Résolution", 0.5, step=0.1)
    ),
    selectInput(ns("cluster_algo"), "Algorithme de clustering",
                choices = c("Louvain (standard)"               = "1",
                           "Louvain (multilevel refinement)"   = "2",
                           "SLM (Smart Local Moving)"          = "3",
                           "Leiden (nécessite reticulate + leidenalg)" = "4"),
                selected = "1"),
    div(class="small text-muted mb-2",
        "Leiden est souvent plus rapide/qualitatif sur les gros datasets, mais",
        " nécessite un environnement Python (reticulate + leidenalg/igraph)",
        " installé séparément — repli automatique sur Louvain si indisponible."),
    actionButton(ns("run_pipeline"), "Lancer Pipeline",
                 class="btn-danger w-100", icon=icon("play")),
    hr(),
    h6("Gros Dataset — Backend Disque (BPCells)", style="font-weight:bold;"),
    selectInput(ns("bpcells_mode"), "Mode",
                choices = c("Auto (recommandé)" = "auto",
                           "Toujours (forcer disque)" = "always",
                           "Jamais (toujours RAM)" = "never"),
                selected = "auto"),
    conditionalPanel(
      condition = "input.bpcells_mode == 'auto'", ns = ns,
      numericInput(ns("bpcells_threshold"), "Seuil auto (cellules)",
                   value = .BPCELLS_AUTO_THRESHOLD, min = 10000, step = 10000)
    ),
    div(class="small text-muted mb-2",
        "Au-delà du seuil, les counts bruts sont écrits sur disque (BPCells) avant",
        " Normalisation — la RAM n'est plus le facteur limitant. Nécessite le",
        " package 'BPCells' (optionnel) ; sans lui, le pipeline continue en RAM",
        " normalement avec un avertissement."),
    hr(),
    h6("Performance / RAM", style="font-weight:bold;"),
    numericInput(ns("max_cells_heavy"),
                 "Max cellules/cluster pour analyses lourdes (Marqueurs, Corrélation)",
                 value = 5000, min = 0, step = 500),
    div(class="small text-muted",
        "0 = pas de sous-échantillonnage (toutes les cellules). Réduit uniquement",
        " les calculs de FindAllMarkers/Corrélation — l'objet complet reste intact",
        " pour la visualisation et l'export.")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pipeline_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    if (is.null(getOption("future.rng.onMisuse")))
      options(future.rng.onMisuse = "ignore")
    if (is.null(getOption("future.globals.maxSize")))
      options(future.globals.maxSize = 2 * 1024^3)

    # ── Mirror the RAM-safety cap to shared_rv so mod_sc_markers.R / mod_sc_corr.R
    #    (and the auto-pipeline in mod_sc.R) can read a single source of truth.
    #    Fires once at startup too (default ignoreInit=FALSE) so the cap is set
    #    even if the user never opens this accordion panel. ──────────────────────
    observeEvent(input$max_cells_heavy, {
      shared_rv$max_cells_heavy <- if (is.na(input$max_cells_heavy) || input$max_cells_heavy <= 0)
        Inf else input$max_cells_heavy
    })

    observeEvent(input$run_pipeline, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Pipeline...", value=0)

      tryCatch({
        # ── Step 1 : QC ───────────────────────────────────────────────────
        p$set(0.10, "QC — détection mitochondriale")
        mt_pattern <- .get_mt_pattern(obj)
        if (!is.null(mt_pattern)) {
          obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern=mt_pattern)
        } else {
          obj[["percent.mt"]] <- 0
          showNotification("Aucun gène mitochondrial détecté. Filtre MT ignoré.",
                           type="warning", duration=3)
        }

        # Capture pre-filter counts for diagnostic message
        n_before     <- ncol(obj)
        meta_pre     <- obj@meta.data
        n_ok_min     <- sum(meta_pre$nFeature_RNA >  input$qc_min_gene, na.rm=TRUE)
        n_ok_max     <- sum(meta_pre$nFeature_RNA <  input$qc_max_gene, na.rm=TRUE)
        n_ok_mt      <- sum(meta_pre$percent.mt   <  input$qc_mt,       na.rm=TRUE)
        n_ok_all     <- sum(meta_pre$nFeature_RNA >  input$qc_min_gene &
                            meta_pre$nFeature_RNA <  input$qc_max_gene &
                            meta_pre$percent.mt   <  input$qc_mt,       na.rm=TRUE)

        obj <- subset(obj,
          subset = nFeature_RNA > input$qc_min_gene &
                   nFeature_RNA < input$qc_max_gene &
                   percent.mt   < input$qc_mt)

        # Actionable QC error with per-filter diagnostics
        if (ncol(obj) < 10) {
          stop(sprintf(
            paste0(
              "Seulement %d cellule(s) après QC (départ : %d). Diagnostique par filtre :\n",
              "  • Min gènes > %d  : %d cellules passent (%d%%)\n",
              "  • Max gènes < %d  : %d cellules passent (%d%%)\n",
              "  • %% Mito < %d%%  : %d cellules passent (%d%%)\n",
              "  • Les 3 combinés  : %d cellules passent\n\n",
              "Suggestions : réduire Min gènes (essayez %d), augmenter Max gènes (%d) ou %% Mito (%d%%)."
            ),
            ncol(obj), n_before,
            input$qc_min_gene, n_ok_min, round(100*n_ok_min/n_before),
            input$qc_max_gene, n_ok_max, round(100*n_ok_max/n_before),
            input$qc_mt,       n_ok_mt,  round(100*n_ok_mt /n_before),
            n_ok_all,
            max(10,  input$qc_min_gene - 100),
            min(50000, input$qc_max_gene + 2000),
            min(50,  input$qc_mt + 15)
          ))
        }

        # Step-3.7A.2: per-sample QC snapshot (before/after counts + %mito
        # median) -- MUST be captured here, meta_pre is the only place the
        # "before filter" metadata still exists. Doublet rate is NOT captured
        # here (scDblFinder runs after QC) -- build_qc_summary_table() joins
        # it in live from the current object at display time instead.
        samples    <- union(names(table(meta_pre$orig.ident)), unique(as.character(obj$orig.ident)))
        before_tab <- table(meta_pre$orig.ident)
        after_tab  <- table(obj$orig.ident)
        mt_med     <- tapply(meta_pre$percent.mt, meta_pre$orig.ident, median, na.rm = TRUE)
        qc_snap <- data.frame(
          Echantillon     = samples,
          Cellules_avant  = as.integer(ifelse(is.na(before_tab[samples]), 0L, before_tab[samples])),
          Cellules_apres  = as.integer(ifelse(is.na(after_tab[samples]),  0L, after_tab[samples])),
          Pct_Mito_Median = round(as.numeric(mt_med[samples]), 1),
          stringsAsFactors = FALSE
        )
        qc_snap$Pct_Conserve <- round(100 * qc_snap$Cellules_apres / pmax(qc_snap$Cellules_avant, 1), 1)
        shared_rv$qc_snapshot <- qc_snap

        # ── Step 1b : Backend disque (BPCells) — Step-3.7A ─────────────────
        p$set(0.22, "Backend stockage...")
        bpcells_mode <- input$bpcells_mode %||% "auto"
        threshold    <- input$bpcells_threshold %||% .BPCELLS_AUTO_THRESHOLD
        should_convert <- switch(bpcells_mode,
          "always" = TRUE,
          "never"  = FALSE,
          ncol(obj) > threshold   # "auto"
        )
        if (should_convert && sc_backend_status(obj) == "memory") {
          if (!.bpcells_available()) {
            showNotification(
              "Package 'BPCells' non installé — pipeline exécuté en RAM comme d'habitude. Installez-le pour les très gros datasets (remotes::install_github('bnprks/BPCells/r')).",
              type = "warning", duration = 8)
          } else {
            conv <- tryCatch(convert_seurat_to_bpcells(obj), error = function(e) {
              showNotification(paste("BPCells: conversion échouée, poursuite en RAM —", e$message),
                               type = "warning", duration = 8)
              NULL
            })
            if (!is.null(conv)) {
              obj <- conv$object
              if (!isTRUE(conv$already_disk)) {
                # One folder per conversion accumulates in tempdir() for the
                # session's lifetime — clean it up when the session closes.
                session$onSessionEnded(function() unlink(conv$dir, recursive = TRUE))
                showNotification(
                  sprintf("💽 Backend disque activé (BPCells) — %s cellules, RAM préservée pour la suite.",
                          format(conv$n_cells, big.mark=" ")),
                  type = "message", duration = 6)
              }
            }
          }
        }

        # ── Steps 2-4: on disk-backed (BPCells) objects, force sequential
        #    future -- Seurat's parallel dispatch for some of these steps can
        #    try to export a huge closure environment as a "global" to
        #    background worker processes (observed on a 1.3M-cell dataset: a
        #    ~5GB function closure captured, blowing past
        #    future.globals.maxSize and crashing right after PCA). Same fix
        #    already used for Harmony below — restored via on.exit() when this
        #    handler returns, so it never leaks into the rest of the app. ────
        if (sc_backend_status(obj) == "disk") {
          .pipeline_old_plan <- future::plan()
          on.exit(future::plan(.pipeline_old_plan), add = TRUE)
          future::plan("sequential")
        }

        # ── Step 2 : Normalisation ────────────────────────────────────────
        p$set(0.30, "Normalisation")
        if (input$norm_method == "sct") {
          obj <- SCTransform(obj, verbose=FALSE, variable.features.n=3000, vst.flavor="v2")
        } else {
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj)
          obj <- FindVariableFeatures(obj, selection.method="vst", nfeatures=2000)
          obj <- smart_scale_data(obj)   # Step-3.7A: RAM-safe (VariableFeatures only, not all genes)
        }

        # ── Step 3 : PCA ──────────────────────────────────────────────────
        p$set(0.50, "PCA")
        obj <- RunPCA(obj, verbose=FALSE, npcs=input$pca_dim)

        # Step-3.7A.2: scale.data (dense, VariableFeatures x cells -- e.g.
        # ~20 Go for 2000 genes x 1.3M cells) is only needed by RunPCA().
        # Freeing it immediately gives FindNeighbors/FindClusters (next, the
        # actual RAM/CPU bottleneck on huge datasets) more headroom. Heatmap
        # re-derives scale.data on demand for whatever genes it needs via
        # ensure_genes_scaled() (helpers_sc_bpcells.R) -- nothing downstream
        # depends on scale.data surviving past this point.
        obj <- tryCatch({
          da <- DefaultAssay(obj)
          if (isTRUE("scale.data" %in% SeuratObject::Layers(obj[[da]]))) obj[[da]]$scale.data <- NULL
          obj
        }, error = function(e) obj)
        clean_mem()

        # ── Step 4 : Clustering ───────────────────────────────────────────
        p$set(0.60, "Clustering")
        obj <- FindNeighbors(obj, dims=1:input$pca_dim)
        algo <- suppressWarnings(as.integer(input$cluster_algo %||% "1"))
        obj <- tryCatch(
          FindClusters(obj, resolution=input$clust_res, algorithm=algo),
          error = function(e) {
            if (algo != 1L) {
              # Step-3.8: algorithms 2 (Louvain multilevel refinement) and 3
              # (SLM) were observed to crash on BPCells-backed objects
              # (igraph/leiden C++ backends expecting a dense/standard sparse
              # adjacency representation) -- same defensive fallback pattern
              # already used for Leiden (algorithm=4) below, generalized.
              algo_name <- c("2"="Louvain multilevel","3"="SLM","4"="Leiden")[as.character(algo)]
              showNotification(
                paste0(algo_name %||% "Algorithme", " indisponible/a échoué sur cet objet — repli sur Louvain standard. ",
                      e$message),
                type = "warning", duration = 10)
              FindClusters(obj, resolution=input$clust_res, algorithm=1)
            } else stop(e)
          }
        )

        # ── Step 5 : Réduction dimensionnelle (méthode principale) ────────
        p$set(0.70, paste("Réduction:", input$reduction_method))

        if (input$reduction_method == "harmony") {
          if (requireNamespace("harmony", quietly=TRUE)) {
            library(harmony)
            if ("orig.ident" %in% colnames(obj@meta.data)) {
              n_batches <- length(unique(obj$orig.ident))
              if (n_batches >= 2) {
                .with_sequential_future({
                  if (.is_big_dataset(obj)) {
                    showNotification("Large dataset: paramètres Harmony réduits.", type="info", duration=3)
                    obj <- RunHarmony(obj, group.by.vars="orig.ident",
                                      dims.use=1:min(30,input$pca_dim), max.iter.harmony=10, verbose=FALSE)
                  } else {
                    obj <- RunHarmony(obj, group.by.vars="orig.ident", dims.use=1:input$pca_dim, verbose=FALSE)
                  }
                })
                obj <- RunUMAP(obj, reduction="harmony", dims=1:input$pca_dim,
                               verbose=FALSE, reduction.name="umap_harmony")
              } else {
                obj <- RunUMAP(obj, dims=1:input$pca_dim, verbose=FALSE)
              }
            }
          } else {
            obj <- RunUMAP(obj, dims=1:input$pca_dim, verbose=FALSE)
          }
        } else if (input$reduction_method == "umap") {
          obj <- RunUMAP(obj, dims=1:input$pca_dim, verbose=FALSE)
        } else if (input$reduction_method == "tsne") {
          obj <- RunTSNE(obj, dims=1:input$pca_dim, verbose=FALSE)
        } else if (input$reduction_method == "dm") {
          if (requireNamespace("destiny", quietly=TRUE)) {
            library(destiny)
            dm <- DiffusionMap(t(Embeddings(obj,"pca")[, 1:input$pca_dim]))
            obj[["dm"]] <- CreateDimReducObject(embeddings=dm@eigenvectors[,1:2],
                                                key="DM_", assay=DefaultAssay(obj))
          } else {
            showNotification("Package 'destiny' introuvable. UMAP utilisé.", type="warning", duration=4)
            obj <- RunUMAP(obj, dims=1:input$pca_dim, verbose=FALSE)
          }
        }
        # pca: already computed in Step 3 — nothing extra

        # ── Step 5b : t-SNE secondaire (Step-3.7) ──────────────────────────
        # Always computed too (unless it's already the primary method chosen
        # above, or the dataset is too big) so PCA/UMAP/t-SNE are ALL
        # available in the Viz "Réduction à visualiser" picker without a
        # second manual pipeline run.
        if (!"tsne" %in% names(obj@reductions) && input$reduction_method != "tsne") {
          if (ncol(obj) > .AUTO_TSNE_MAX_CELLS) {
            showNotification(
              sprintf("t-SNE secondaire ignoré (%s cellules > %s max). Choisissez 't-SNE' comme méthode principale si nécessaire.",
                      format(ncol(obj), big.mark=" "), format(.AUTO_TSNE_MAX_CELLS, big.mark=" ")),
              type = "info", duration = 5)
          } else {
            p$set(0.85, "t-SNE (secondaire)...")
            obj <- tryCatch(RunTSNE(obj, dims = 1:input$pca_dim, verbose = FALSE),
                            error = function(e) {
                              showNotification(paste("t-SNE secondaire ignoré:", e$message),
                                               type = "warning", duration = 5)
                              obj
                            })
          }
        }

        global_data$sc_obj <- obj
        showNotification("✓ Pipeline terminé", type = "message")

      }, error = function(e) {
        showNotification(paste("Erreur pipeline:", e$message), type="error", duration=10)
      })
    })
  })
}
