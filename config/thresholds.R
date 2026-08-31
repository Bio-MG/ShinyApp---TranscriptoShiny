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

# --- Spatial deconvolution ---------------------------------------------------
TS_DECONV_MAX_CELLS_PER_TYPE <- 500L   # per-type subsample cap in artifact

# --- Trajectory / velocity ---------------------------------------------------
TS_TRAJECTORY_K_DEFAULT    <- 15L      # default kNN k for graph pseudotime
TS_VELOCITY_OVERLAP_MIN    <- 0.80     # min cell overlap fraction (0-1)
