# =============================================================================
# config/defaults.R — Default parameter values (sourced by app.R)
# =============================================================================
# Single source of truth for default values used across modules.
# Modules must read these values; do NOT hardcode them in module code.
# =============================================================================

# --- Parallelism / async ---------------------------------------------------
TS_MIRAI_TIMEOUT_MS        <- 20 * 60 * 1000L   # 20 min generic task ceiling
TS_RCTD_TIMEOUT_MS         <- 40 * 60 * 1000L   # RCTD-specific ceiling
TS_LABEL_TRANSFER_TIMEOUT  <- 45 * 60 * 1000L   # Label Transfer ceiling
TS_MIRAI_N_DAEMONS         <- 6L

# --- Single-Cell sketch presets (cells) ------------------------------------
TS_SKETCH_FAST             <- 5000L
TS_SKETCH_LIGHT            <- 10000L
TS_SKETCH_MEDIUM           <- 25000L
TS_SKETCH_STANDARD         <- 50000L
TS_SKETCH_HIGH             <- 100000L

# --- Single-Cell analysis caps ---------------------------------------------
TS_MAX_PER_CLUSTER_MARKERS <- 1000L   # subsample cap for FindAllMarkers
TS_MAX_PER_SAMPLE_CORR     <- 5000L   # subsample cap for correlation
TS_MAX_HEATMAP_CELLS       <- 5000L   # max cells before heatmap aggregation
TS_MAX_HEATMAP_FEATURES    <- 50L
TS_MAX_CORR_FEATURES       <- 50L
TS_MAX_CORR_GENES          <- 200L    # genes in correlation matrix
TS_MAX_CORRELATED_TOP_N    <- 50L     # top correlated genes returned

# --- Spatial ----------------------------------------------------------------
TS_RCTD_MIN_CELLS_PER_TYPE <- 25L     # RCTD minimum per cell type
TS_DECONV_DEFAULT_N_HVG    <- 2000L   # HVG cap before deconvolution
TS_MAX_SVG_HEATMAP         <- 60L     # max SVGs in grid display

# --- Velocity ----------------------------------------------------------------
TS_VELOCITY_MAX_PORTRAIT_CELLS <- 50000L  # phase portrait subsample cap
TS_VELOCITY_MAX_EMBED_CELLS    <- 5000L   # embedding plot subsample cap
