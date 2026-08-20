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
#
# AUDIT FIX (quick win): R/utils_spatial_io.R used to be source()'d HERE,
# BEFORE any package is loaded, AND AGAIN in app.R (after packages are
# loaded, alongside the rest of the spatial infrastructure). Two independent
# audits flagged this double-sourcing as a latent divergence risk (harmless
# today since source() just redefines the same functions identically, but a
# future edit to one call site and not the other would silently diverge).
# Single source of truth now lives in app.R, where it belongs alongside
# R/utils_spatial_async.R / _multi.R / _niche.R / _reference.R and AFTER
# every package is guaranteed loaded.
# =============================================================================

# global.R v3.txt

# Charge les librairies et définit les options globales

# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)



# --- 2. PACKAGES (CRAN / Bioconductor — installables via install.packages()/BiocManager) ---
# new package install.packages(c("RANN", "irlba", "topicmodels", "slam")) , deconvonvolution label transfer : BiocManager::install('glmGamPoi')

required_packages <- c(
  "shiny", "bslib", "Seurat", "SeuratObject",
  "ggplot2", "dplyr", "DT", "patchwork", "viridis",
  "plotly", "bsicons", "future", "shinyFiles",
  "SingleR", "celldex", "SingleCellExperiment",
  "harmony", "destiny", "fs", "igraph", "Matrix", "reshape2",
  "shinyjs", "circlize", "rmarkdown", "zip",
  "mirai", "sf", "leaflet", "scattermore",
  "ape", "RANN", "irlba", "png", "RColorBrewer", "shinyWidgets", "shinycssloaders"
)

optional_packages <- c(
  "leaflet.extras2",
  "leiden",
  "topicmodels",
  "slam",
  "schard"
)

bioc_packages <- c(
  "DESeq2", "edgeR", "limma", "ComplexHeatmap"
)


for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    warning(sprintf("Paquet requis manquant : %s", pkg))
  }
}

for (pkg in optional_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Paquet optionnel absent : %s", pkg))
  }
}

for (pkg in bioc_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(sprintf("Paquet Bioconductor manquant : %s", pkg))
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
#   remotes::install_github("cellgeni/schard")      # lecture .h5ad robuste (reference
#                                                    # de deconvolution + import Single-Cell,
#                                                    # voir helpers_io.R::load_single_cell_data())
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
# Phase 5 : lecteur .h5ad robuste (reference de deconvolution RCTD/Label
# Transfer + import Single-Cell) — voir helpers_io.R::load_single_cell_data().
# Optionnel : a defaut, l'app retombe sur SeuratDisk (moins fiable pour
# AnnData) si celui-ci est installe, sinon .h5ad est simplement indisponible.
has_schard       <- requireNamespace("schard", quietly = TRUE)
# Phase 5 : requis pour lire les .parquet des exports Visium HD "binned"
# (tissue_positions.parquet sous binned_outputs/square_0XXum/spatial/) —
# voir helpers_io.R::load_spatial_visium_hd(), qui verifie aussi ce point
# lui-meme avant d'importer (message clair plutot que l'erreur Seurat brute
# "Please install arrow to read parquet files"). Souvent difficile a
# installer depuis les sources sous Windows sans toolchain complet — si
# aucun binaire CRAN n'est disponible pour votre version de R, essayez :
# install.packages("arrow", repos = c("https://apache.r-universe.dev", getOption("repos")))
has_arrow        <- requireNamespace("arrow", quietly = TRUE)

for (dep in c("bpcells", "spacexr", "stdeconvolve", "leafgl", "mirai", "rann", "schard", "arrow")) {
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
