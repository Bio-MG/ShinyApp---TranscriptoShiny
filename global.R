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
# global.R itself now ONLY holds: package loading, global options, future
# parallel plan, and the bslib theme. source() order in app.R: this file
# first (defines packages/options other helpers may rely on at call time),
# then the 4 helpers_*.R (order among them does not matter — R resolves
# function-to-function calls at call time, not at source time), then modules.
# =============================================================================

# global.R v2.2.txt

# Charge les librairies et définit les options globales



# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)



# --- 2. PACKAGES ---

required_packages <- c(

  "shiny", "bslib", "Seurat", "ggplot2", "dplyr", "DT", "patchwork", "viridis",

  "plotly", "bsicons", "future", "shinyFiles", "SingleR", "celldex", 

  "SingleCellExperiment", "harmony", "destiny", "fs", "igraph", "Matrix", "reshape2",

  "shinyjs", "circlize", "rmarkdown", "zip", "leaflet", "mirai", "BPCells"

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
