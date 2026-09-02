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

# --- Differential abundance design (4E-0) ------------------------------------
# Principe central : les cellules ne sont PAS des replicats biologiques
# (pas de pseudoreplication) — le blocage est hard en dessous du plancher.
TS_DA_MIN_REPLICATES_PER_CONDITION  <- 2L   # blocage en dessous (par condition)
TS_DA_MIN_CELLS_PER_SAMPLE          <- 10L  # avertissement en dessous (echantillon faible)
TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE <- 5L   # avertissement en dessous (identite sous-representee)
TS_DA_CELLS_IMBALANCE_RATIO         <- 3    # avertissement au-dessus (ratio max/min cellules par echantillon)

# --- Milo (4E-1) --------------------------------------------------------------
# Construction des voisinages + modele DA par voisinage (miloR). La graine
# est enregistree pour la reproductibilite declaree (le graphe kNN exact est
# deterministe ; la graine couvre les eventuels liens/empiriques).
TS_DA_MILO_K                     <- 30L        # k du graphe kNN et des voisinages
TS_DA_MILO_PROP                  <- 0.1        # proportion de voisinages conserves (makeNhoods)
TS_DA_MILO_D                     <- 30L        # dimensionnalite latente (plafonnee a ncol de la reduction)
TS_DA_MILO_REFINEMENT_SCHEME     <- "graph"    # makeNhoods refinement_scheme
TS_DA_MILO_FDR_WEIGHTING         <- "k-distance" # graphSpatialFDR (ponderation k-distance)
TS_DA_MILO_MIN_MEAN              <- 0          # seuil d'expression minimale testNhoods (defaut miloR)
TS_DA_MILO_ROBUST                <- TRUE       # glmQLFit robust (defaut miloR)
TS_DA_MILO_IDENTITY_FRACTION_MIN <- 0.7        # fraction min d'identite pour annoter un voisinage (convention miloR)
TS_DA_MILO_DISPLAY_ALPHA         <- 0.1        # seuil SpatialFDR d'affichage des voisinages significatifs (vues)
TS_DA_MILO_SEED                  <- 14L        # graine enregistree dans la provenance

# --- scCODA (4E-2) ------------------------------------------------------------
# DA compositionnelle au niveau ECHANTILLON via l'environnement Python sccoda
# (detection explicite — jamais de repli silencieux). Defauts MCMC = defauts
# scCODA (resultats/burnin/leapfrog/step size) ; la convergence est evaluee
# en pure R : ESS < ESS_FAIL = ECHEC (posteriorite inutilisable) ; ESS <
# ESS_MIN = avertissement (le spike-and-slab deprime structurellement l'ESS du
# HMC tensorflow — calibre empiriquement : ESS ~11 a 20000 tirages sur le
# fixture) ; r_hat/divergences bloquent quand ils sont disponibles (NA = note).
TS_DA_SCCODA_NUM_RESULTS    <- 20000L  # echantillons MCMC conserves (defaut scCODA)
TS_DA_SCCODA_NUM_BURNIN     <- 5000L   # burnin MCMC (defaut scCODA)
TS_DA_SCCODA_NUM_LEAPFROG   <- 10L     # pas de leapfrog HMC (defaut scCODA)
TS_DA_SCCODA_STEP_SIZE      <- 0.01    # step size initial HMC (defaut scCODA)
TS_DA_SCCODA_FDR_TARGET     <- 0.05    # seuil FDR des effets credibles
TS_DA_SCCODA_RHAT_MAX       <- 1.01    # echec de convergence au-dela (NA = note)
TS_DA_SCCODA_ESS_FAIL       <- 10      # echec de convergence en dessous
TS_DA_SCCODA_ESS_MIN        <- 100     # avertissement de convergence en dessous
TS_DA_SCCODA_MAX_DIVERGENCES<- 0       # divergences NUTS tolerees (NA = note)
TS_DA_SCCODA_SEED           <- 15L     # graine tensorflow enregistree