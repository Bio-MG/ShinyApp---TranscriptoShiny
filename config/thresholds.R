# =============================================================================
# config/thresholds.R — Scientific thresholds and hard limits
# =============================================================================
# Hard caps and scientific thresholds. Modules read these; do not hardcode.
# =============================================================================

# --- Hard cell-count ceilings -----------------------------------------------
TS_MAX_TRAJECTORY_CELLS    <- 100000L  # trajectory disabled above this
TS_AUTO_TSNE_MAX_CELLS     <- 30000L   # auto t-SNE disabled above this
TS_BPCELLS_AUTO_THRESHOLD  <- 150000L  # auto disk-backend above this
TS_PREVIEW_MAX_CELLS       <- 50000L   # viz preview subsample threshold
TS_DENSITY_MAX_CELLS       <- 50000L   # 2D density subsample cap
TS_3D_MAX_CELLS            <- 50000L   # 3D plot subsample cap

# --- Bulk RNA-seq ------------------------------------------------------------
TS_BULK_MIN_COUNT_DEFAULT  <- 10L      # default min total counts per gene
TS_BULK_MIN_SAMPLES_DEFAULT <- 1L      # default min samples above threshold

# --- Bulk V2 — provenance & diagnostics batch --------------------------------
TS_BULK_VARPART_MAX_GENES  <- 2000L    # cap gènes pour la décomposition de variance (repli R pur)

# --- Bulk V2 — scores par échantillon (GSVA / ssGSEA) -------------------------
TS_BULK_GSVA_MIN_SIZE      <- 10L      # taille min d'un jeu de gènes
TS_BULK_GSVA_MAX_SIZE      <- 500L     # taille max d'un jeu de gènes

# --- Bulk V2 — WGCNA safe-mode ------------------------------------------------
TS_BULK_WGCNA_MIN_SAMPLES  <- 15L      # arrêt dur en dessous (N < 15)
TS_BULK_WGCNA_MIN_GENES    <- 2000L    # pré-filtrage HVG : plancher
TS_BULK_WGCNA_MAX_GENES    <- 5000L    # pré-filtrage HVG : plafond
TS_BULK_WGCNA_R2_MIN       <- 0.80     # fit scale-free cible
TS_BULK_WGCNA_MAX_BLOCKSIZE<- 5000L    # maxBlockSize (calibré RAM 32 Go)
TS_BULK_WGCNA_MIN_MODULE   <- 30L      # minModuleSize blockwiseModules

# --- Bulk V2 — parallélisme (garde-fou mémoire 32 Go) -------------------------
TS_BULK_MAX_WORKERS        <- 4L       # min(detectCores()-1, 4) — jamais au-delà

# --- Bulk V2 — survie & association clinique ----------------------------------
TS_BULK_SURV_MIN_EVENTS    <- 10L      # événements observés minimum (statut=1)

# --- Spatial deconvolution ---------------------------------------------------
TS_DECONV_MAX_CELLS_PER_TYPE <- 500L   # per-type subsample cap in artifact

# --- Trajectory / velocity ---------------------------------------------------
TS_TRAJECTORY_K_DEFAULT    <- 15L      # default kNN k for graph pseudotime
TS_VELOCITY_OVERLAP_MIN    <- 0.80     # min cell overlap fraction (0-1)
