# =============================================================================
# global.R — Packages, global options, theme (slim, post-refactor)
# =============================================================================
# Domain helper functions used to live here (3251 lines, "fourre-tout") — they
# have been split out into:
#   helpers_io.R       — multi-format loading, gene-ID / GEO metadata mapping
#   helpers_sc.R       — Seurat: scatter/violin/correlation/trajectory plots
#   helpers_bulk.R     — DESeq2/edgeR/limma engine + bulk plots
#   helpers_pathway.R  — ORA + GSEA, shared by mod_sc_pathways.R and
#                         mod_bulk_pathways.R
#   R/utils_spatial_io.R / R/utils_spatial_async.R — BPCells conversion +
#                         mirai daemon pool for the Spatial module (v3)
# global.R itself now ONLY holds: package loading, global options, future
# parallel plan, and the bslib theme. source() order in app.R: this file
# first (defines packages/options other helpers may rely on at call time),
# then the 4 helpers_*.R (order among them does not matter — R resolves
# function-to-function calls at call time, not at source time), then the
# R/utils_spatial_*.R pair (spatial daemons init), then modules.
# =============================================================================

# global.R v3.txt

# Charge les librairies et définit les options globales



# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)



# --- 2. PACKAGES (CRAN / Bioconductor — installables via install.packages()/BiocManager) ---
# new package install.packages(c("RANN", "irlba", "topicmodels", "slam"))

required_packages <- c(
  
  "shiny", "bslib", "Seurat", "SeuratObject", "ggplot2", "dplyr", "DT", "patchwork", "viridis",
  
  "plotly", "bsicons", "future", "shinyFiles", "SingleR", "celldex",
  
  "SingleCellExperiment", "harmony", "destiny", "fs", "igraph", "Matrix", "reshape2",
  
  "shinyjs", "circlize", "rmarkdown", "zip",
  
  # --- Spatial v3 (BPCells + mirai async) ---
  "mirai",       # daemon pool for clustering/deconvolution/Moran's I (R/utils_spatial_async.R)
  "sf",          # required by SeuratObject::Simplify()/Crop() (imaging FOV polygons)
  "leaflet",     # WebGL spatial map (mod_spatial_viz.R), CRS.Simple mode
  "scattermore", # rasterized fallback / high-density static export
  "leiden",      # Seurat::FindClusters(algorithm = 4) — Leiden
  "ape",         # RunMoransI() fallback if Rfast2 is absent (install Rfast2 for speed)
  "RANN",        # spatial k-NN for the manual "BANKSY-lite" augmentation (mod_spatial_cluster.R)
  "irlba"        # fast truncated PCA on the augmented feature matrix (falls back to stats::prcomp)
  
) 



bioc_packages <- c("DESeq2", "edgeR", "limma", "ComplexHeatmap")



for (pkg in required_packages) {
  
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    
    warning(paste("Paquet manquant :", pkg))
    
  }
  
}



has_deseq2 <- requireNamespace("DESeq2", quietly = TRUE)

has_edger <- requireNamespace("edgeR", quietly = TRUE)

has_limma <- requireNamespace("limma", quietly = TRUE)



# --- Spatial v3 : dependances non-CRAN / optionnelles ---------------------
# Le clustering spatial ("BANKSY-lite", mod_spatial_cluster.R) et la
# deconvolution reference-free (mod_spatial_deconv.R) sont maintenant
# implementes SANS dependre de Banksy/SeuratWrappers ni de
# STdeconvolve::fitLDA() -- ces derniers spawnaient des sous-processus
# paralleles internes qui se bloquaient depuis un daemon mirai. RCTD
# (spacexr) reste utilise mais force en mono-coeur (max_cores=1).
#
#   remotes::install_github("bnprks/BPCells/r")     # backend disque (obligatoire)
#   remotes::install_github("dmcable/spacexr")      # RCTD (deconvolution avec reference)
#   install.packages(c("STdeconvolve", "topicmodels", "slam"))  # LDA (deconvolution sans reference)
#
# Optionnels, non requis par le pipeline par defaut :
#   remotes::install_github("prabhakarlab/Banksy", ref = "devel")
#   remotes::install_github("satijalab/seurat-wrappers")
#
has_bpcells      <- requireNamespace("BPCells", quietly = TRUE)
has_spacexr      <- requireNamespace("spacexr", quietly = TRUE)
has_stdeconvolve <- requireNamespace("STdeconvolve", quietly = TRUE) &&
  requireNamespace("topicmodels", quietly = TRUE) &&
  requireNamespace("slam", quietly = TRUE)
has_leafgl       <- requireNamespace("leafgl", quietly = TRUE)
has_mirai        <- requireNamespace("mirai", quietly = TRUE)
has_rann         <- requireNamespace("RANN", quietly = TRUE)

for (dep in c("bpcells", "spacexr", "stdeconvolve", "leafgl", "mirai", "rann")) {
  if (!get(paste0("has_", dep))) {
    message(sprintf("[spatial] Package(s) pour '%s' non installe(s) — fonctionnalite associee indisponible tant que non installee (voir commentaire ci-dessus pour la commande d'installation).", dep))
  }
}



if (require("future", quietly = TRUE)) {
  
  plan(multisession, workers = max(1, parallel::detectCores() - 2))
  
}

# OPTIMISATION PARALLÉLISATION 

if(require("future", quietly = TRUE)) {
  
  plan("multisession", workers = max(1, parallel::detectCores() - 2))
  
  options(future.globals.maxSize = 10000 * 1024^2)  # 10GB
  
}





my_theme <- bs_theme(
  
  version = 5,
  
  bootswatch = "flatly",
  
  primary = "#2C3E50",
  
  secondary = "#18BC9C",
  
  "enable-gradients" = TRUE
  
)



clean_mem <- function() { gc() }
