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

# global.R v2.3.txt

# --- RETICULATE / CONDA : opt-in uniquement ---
# Mettre USE_RETICULATE = TRUE pour activer Python/CUDA (configuration requise)
USE_RETICULATE <- FALSE  # <-- passer à TRUE quand env conda configuré

if (USE_RETICULATE) {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    reticulate::use_condaenv("transcriptoshiny", required = FALSE)
  }
} else {
  # Désactive le chargement automatique de reticulate par les dépendances
  options(reticulate.autoconfig = FALSE)
  Sys.setenv(RETICULATE_PYTHON = "")
}


# Charge les librairies et définit les options globales



# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)



# --- 2. PACKAGES (CRAN / Bioconductor — installables via install.packages()/BiocManager) ---

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
  "leiden",      # Seurat::FindClusters(algorithm = 4) — Leiden, used by mod_spatial_cluster.R
  "ape"          # RunMoransI() fallback if Rfast2 is absent (install Rfast2 for speed)

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



# --- Spatial v3 : dependances non-CRAN (GitHub) — verifiees mais jamais
# require()-ees au chargement (elles ne bloquent aucune autre fonctionnalite
# de l'app si absentes ; chaque module spatial degrade proprement avec un
# message clair — voir mod_spatial_cluster.R / mod_spatial_deconv.R).
#
#   remotes::install_github("bnprks/BPCells/r")
#   remotes::install_github("prabhakarlab/Banksy", ref = "devel")
#   remotes::install_github("satijalab/seurat-wrappers")   # fournit RunBanksy()
#   remotes::install_github("dmcable/spacexr")              # RCTD
#   install.packages("STdeconvolve")                        # si disponible sur le CRAN/Bioc
#   remotes::install_github("r-spatial/leafgl")              # si absent du CRAN
#
has_bpcells      <- requireNamespace("BPCells", quietly = TRUE)
has_banksy       <- requireNamespace("Banksy", quietly = TRUE) && requireNamespace("SeuratWrappers", quietly = TRUE)
has_spacexr      <- requireNamespace("spacexr", quietly = TRUE)
has_stdeconvolve <- requireNamespace("STdeconvolve", quietly = TRUE)
has_leafgl       <- requireNamespace("leafgl", quietly = TRUE)
has_mirai        <- requireNamespace("mirai", quietly = TRUE)

for (dep in c("bpcells", "banksy", "spacexr", "stdeconvolve", "leafgl", "mirai")) {
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
