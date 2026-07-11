# mod_sc_pipeline.R  —  Child 1: QC → Norm → PCA → Harmony → Clusters → UMAP
# Step-3.6: relaxed QC defaults for test datasets; actionable QC error messages.

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
    hr(),
    h6("Normalisation"),
    radioButtons(ns("norm_method"), NULL,
      choices=c("LogNormalize (Standard)"="log","SCTransform (Avancé)"="sct")),
    hr(),
    h6("Réduction Dimensionnelle"),
    selectInput(ns("reduction_method"), "Méthode de Réduction",
      choices=c("UMAP"="umap","PCA"="pca","t-SNE"="tsne",
                "Diffusion Maps"="dm","Harmony"="harmony"), selected="umap"),
    layout_columns(
      sliderInput(ns("pca_dim"),    "Dims PCA",   5, 50, 20),
      numericInput(ns("clust_res"), "Résolution", 0.5, step=0.1)
    ),
    actionButton(ns("run_pipeline"), "Lancer Pipeline",
                 class="btn-danger w-100", icon=icon("play"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pipeline_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    if (is.null(getOption("future.rng.onMisuse")))
      options(future.rng.onMisuse = "ignore")
    if (is.null(getOption("future.globals.maxSize")))
      options(future.globals.maxSize = 2 * 1024^3)

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

        # ── Step 2 : Normalisation ────────────────────────────────────────
        p$set(0.30, "Normalisation")
        if (input$norm_method == "sct") {
          obj <- SCTransform(obj, verbose=FALSE, variable.features.n=3000, vst.flavor="v2")
        } else {
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj)
          obj <- FindVariableFeatures(obj, selection.method="vst", nfeatures=2000)
          obj <- ScaleData(obj)
        }

        # ── Step 3 : PCA ──────────────────────────────────────────────────
        p$set(0.50, "PCA")
        obj <- RunPCA(obj, verbose=FALSE, npcs=input$pca_dim)

        # ── Step 4 : Clustering ───────────────────────────────────────────
        p$set(0.60, "Clustering")
        obj <- FindNeighbors(obj, dims=1:input$pca_dim)
        obj <- FindClusters(obj, resolution=input$clust_res)

        # ── Step 5 : Réduction dimensionnelle ─────────────────────────────
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

        global_data$sc_obj <- obj
        showNotification("✓ Pipeline terminé", type="message")

      }, error = function(e) {
        showNotification(paste("Erreur pipeline:", e$message), type="error", duration=10)
      })
    })
  })
}
