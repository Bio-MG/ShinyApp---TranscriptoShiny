# =============================================================================
# mod_sc_pipeline.R  —  Child 1: QC → Norm → PCA → Harmony → Clusters → UMAP
# =============================================================================
# Inputs  (from parent via arguments):
#   global_data  : reactiveValues(sc_obj = NULL)
#   shared_rv    : reactiveValues()  — writes shared_rv$active_tab = "tab_viz"
#                                      after a successful run
#
# Writes  :  global_data$sc_obj  (updated Seurat object)
#
# UI split:  mod_sc_pipeline_ui()   — sidebar controls (accordion body)
#            No dedicated output panel; QC plot is in the parent's "QC" tab.
# =============================================================================


# ── Private helpers ───────────────────────────────────────────────────────────

# Detect mitochondrial gene prefix (human ^MT- or mouse ^mt-)
.get_mt_pattern <- function(obj) {
  genes <- rownames(obj)
  if (any(grepl("^MT-", genes, ignore.case = FALSE))) return("^MT-")
  if (any(grepl("^mt-", genes, ignore.case = FALSE))) return("^mt-")
  if (any(grepl("^MT-", genes, ignore.case = TRUE)))  return("^MT-")
  NULL
}

# Swap future plan to sequential for the duration of `expr`
# (required for Harmony to avoid memory blow-up with multisession)
.with_sequential_future <- function(expr) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("sequential")
  force(expr)
}

# TRUE when ncol(obj) > 100k
.is_big_dataset <- function(obj) ncol(obj) > 100000


# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_pipeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # QC filters
    layout_columns(
      numericInput(ns("qc_min_gene"), "Min Gènes", 200, step = 50),
      numericInput(ns("qc_max_gene"), "Max Gènes", 6000, step = 500)
    ),
    sliderInput(ns("qc_mt"), "% Mito Max", 0, 50, 5, step = 1),
    helpText("Filtre pour retirer les cellules de mauvaise qualité."),
    hr(),

    # Normalisation
    h6("Normalisation"),
    radioButtons(
      ns("norm_method"), NULL,
      choices = c(
        "LogNormalize (Standard)" = "log",
        "SCTransform (Avancé)"   = "sct"
      )
    ),
    hr(),

    # Dimensionality reduction
    h6("Réduction Dimensionnelle"),
    selectInput(
      ns("reduction_method"), "Méthode de Réduction",
      choices = c(
        "UMAP"          = "umap",
        "PCA"           = "pca",
        "t-SNE"         = "tsne",
        "Diffusion Maps"= "dm",
        "Harmony"       = "harmony"
      ),
      selected = "umap"
    ),
    layout_columns(
      sliderInput(ns("pca_dim"),   "Dims PCA",   5, 50, 20),
      numericInput(ns("clust_res"), "Résolution", 0.5, step = 0.1)
    ),

    actionButton(
      ns("run_pipeline"), "Lancer Pipeline",
      class = "btn-danger w-100", icon = icon("play")
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pipeline_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # ── future / RNG options (set once per session) ───────────────────────────
    if (is.null(getOption("future.rng.onMisuse")))
      options(future.rng.onMisuse = "ignore")
    if (is.null(getOption("future.globals.maxSize")))
      options(future.globals.maxSize = 2 * 1024^3)  # 2 GB

    # ── Helper: refresh all downstream UI choices after pipeline ─────────────
    # Exported via shared_rv so mod_sc_viz / mod_sc_markers etc. can call it
    # indirectly through observing global_data$sc_obj changes.
    # (Each child module owns its own updateSelectize* calls triggered by
    #  observeEvent(global_data$sc_obj, ...) in that child.)

    # ── Main pipeline observer ────────────────────────────────────────────────
    observeEvent(input$run_pipeline, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj

      p <- shiny::Progress$new()
      on.exit(p$close())
      p$set(message = "Pipeline...", value = 0)

      tryCatch({
        # ── Step 1 : QC ───────────────────────────────────────────────────────
        p$set(0.10, "QC — détection mitochondriale")
        mt_pattern <- .get_mt_pattern(obj)
        if (!is.null(mt_pattern)) {
          obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = mt_pattern)
        } else {
          obj[["percent.mt"]] <- 0
          showNotification(
            "Aucun gène mitochondrial détecté. Filtre MT ignoré.",
            type = "warning", duration = 3
          )
        }

        obj <- subset(
          obj,
          subset = nFeature_RNA > input$qc_min_gene &
                   nFeature_RNA < input$qc_max_gene &
                   percent.mt   < input$qc_mt
        )
        if (ncol(obj) < 10) stop("Moins de 10 cellules après filtrage QC.")

        # ── Step 2 : Normalisation ────────────────────────────────────────────
        p$set(0.30, "Normalisation")
        if (input$norm_method == "sct") {
          obj <- SCTransform(
            obj,
            verbose            = FALSE,
            variable.features.n = 3000,
            vst.flavor         = "v2"
          )
        } else {
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj)
          obj <- FindVariableFeatures(obj, selection.method = "vst", nfeatures = 2000)
          obj <- ScaleData(obj)
        }

        # ── Step 3 : PCA ──────────────────────────────────────────────────────
        p$set(0.50, "PCA")
        obj <- RunPCA(obj, verbose = FALSE, npcs = input$pca_dim)

        # ── Step 4 : Clustering ───────────────────────────────────────────────
        p$set(0.60, "Clustering")
        obj <- FindNeighbors(obj, dims = 1:input$pca_dim)
        obj <- FindClusters(obj,  resolution = input$clust_res)

        # ── Step 5 : Dimensionality reduction ─────────────────────────────────
        p$set(0.70, paste("Réduction:", input$reduction_method))

        if (input$reduction_method == "harmony") {
          # ── Harmony (sequential future to cap memory) ──────────────────────
          if (requireNamespace("harmony", quietly = TRUE)) {
            library(harmony)
            if ("orig.ident" %in% colnames(obj@meta.data)) {
              n_batches <- length(unique(obj$orig.ident))
              if (n_batches >= 2) {
                .with_sequential_future({
                  if (.is_big_dataset(obj)) {
                    showNotification(
                      "Large dataset: paramètres Harmony réduits.",
                      type = "info", duration = 3
                    )
                    obj <- RunHarmony(
                      obj,
                      group.by.vars    = "orig.ident",
                      dims.use         = 1:min(30, input$pca_dim),
                      max.iter.harmony = 10,
                      verbose          = FALSE
                    )
                  } else {
                    obj <- RunHarmony(
                      obj,
                      group.by.vars = "orig.ident",
                      dims.use      = 1:input$pca_dim,
                      verbose       = FALSE
                    )
                  }
                })
                obj <- RunUMAP(
                  obj,
                  reduction      = "harmony",
                  dims           = 1:input$pca_dim,
                  verbose        = FALSE,
                  reduction.name = "umap_harmony"
                )
              } else {
                # Single batch — fall back to standard UMAP
                obj <- RunUMAP(obj, dims = 1:input$pca_dim, verbose = FALSE)
              }
            }
          } else {
            # harmony package not installed
            obj <- RunUMAP(obj, dims = 1:input$pca_dim, verbose = FALSE)
          }

        } else if (input$reduction_method == "umap") {
          obj <- RunUMAP(obj, dims = 1:input$pca_dim, verbose = FALSE)

        } else if (input$reduction_method == "tsne") {
          obj <- RunTSNE(obj, dims = 1:input$pca_dim, verbose = FALSE)

        } else if (input$reduction_method == "dm") {
          # Diffusion Maps via destiny package
          if (requireNamespace("destiny", quietly = TRUE)) {
            library(destiny)
            dm <- DiffusionMap(t(Embeddings(obj, "pca")[, 1:input$pca_dim]))
            obj[["dm"]] <- CreateDimReducObject(
              embeddings = dm@eigenvectors[, 1:2],
              key        = "DM_",
              assay      = DefaultAssay(obj)
            )
          } else {
            showNotification(
              "Package 'destiny' introuvable. UMAP utilisé à la place.",
              type = "warning", duration = 4
            )
            obj <- RunUMAP(obj, dims = 1:input$pca_dim, verbose = FALSE)
          }

        } else if (input$reduction_method == "pca") {
          # PCA already computed in Step 3 — nothing extra to do
        }

        # ── Commit updated object ──────────────────────────────────────────────
        global_data$sc_obj <- obj
        showNotification("✓ Pipeline terminé", type = "message")

        # Signal downstream children that new data is available
        # (they react via observeEvent(global_data$sc_obj, ...) in their own servers)

      }, error = function(e) {
        showNotification(paste("Erreur pipeline:", e$message), type = "error")
      })
    }) # /observeEvent run_pipeline

  }) # /moduleServer
}
