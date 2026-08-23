# =============================================================================
# scripts/renv_bootstrap.R — one-time renv setup for TranscriptoShiny
# =============================================================================
# Run ONCE, interactively, from the project root (same folder as app.R):
#
#   Rscript scripts/renv_bootstrap.R
#
# What this does:
#   1. Installs 'renv' if missing (from CRAN — needs normal internet access;
#      this script is meant for a real dev/deploy machine, not a locked-down
#      sandbox).
#   2. Points R at BOTH CRAN and the CORRECT Bioconductor release for the
#      currently installed R version, via BiocManager::repositories() — see
#      the "Bioconductor version pinning" note below for why this is done
#      via detection rather than a hardcoded version string.
#   3. renv::init() — scans the codebase (source()-based dependency
#      detection works fine for a non-package Shiny app like this one; renv
#      does not require a DESCRIPTION file) and creates renv.lock +
#      .Rprofile + renv/ (activation script).
#   4. Prints the resolved Bioconductor release so it can be pasted into the
#      project README (see RENV_SETUP.md for the exact paragraph to use).
#
# WHAT THIS DOES NOT DO:
#   - Install the GitHub-only packages (BPCells, spacexr, schard, and
#     optionally Banksy/SeuratWrappers) — see RENV_SETUP.md for the explicit
#     renv::install() calls for each, to run AFTER this bootstrap and BEFORE
#     the final renv::snapshot().
# =============================================================================

if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installation de 'renv' (CRAN)...")
  install.packages("renv")
}

# --- Bioconductor version pinning: detected, not hand-maintained ----------
# Bioconductor releases are tightly coupled to the R minor version (e.g. R
# 4.4.x <-> Bioc 3.19/3.20 depending on exact point release) — hardcoding a
# specific Bioc version number here would silently break BiocManager on any
# machine running a different R version than whoever last edited this file.
# BiocManager::repositories() auto-resolves the CORRECT release for the R
# version actually running this script; renv then records the resolved
# Bioconductor package sources (repository + exact version) in renv.lock
# during the snapshot below — the lockfile itself becomes the single source
# of truth for "which exact Bioc release this project was locked against",
# which is both more correct and lower-maintenance than a manual comment.
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  message("Installation de 'BiocManager' (CRAN)...")
  install.packages("BiocManager")
}
options(repos = BiocManager::repositories())

message(sprintf(
  "R %s -> Bioconductor release detecte : %s",
  getRversion(), as.character(BiocManager::version())
))

# --- renv init (non-package project: source()-based scan works fine) ------
renv::init(bare = FALSE)

message(
  "\nrenv initialise. PROCHAINES ETAPES (voir RENV_SETUP.md) :\n",
  "  1. Installez les paquets GitHub-only (BPCells, spacexr, ...) via\n",
  "     renv::install('owner/repo') — PAS install.packages().\n",
  "  2. Une fois TOUT installe et l'app testee, figez l'etat exact :\n",
  "       renv::snapshot()\n",
  "  3. Committez renv.lock + .Rprofile + renv/activate.R (PAS renv/library/,\n",
  "     deja exclu par le .gitignore que renv::init() vient de generer).\n"
)

