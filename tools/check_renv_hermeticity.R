# =============================================================================
# check_renv_hermeticity.R — garde D'HERMETICITE du lockfile renv
# =============================================================================
# Pourquoi ce garde existe (mesure du 2026-09-15) : `renv.lock` peut annoncer
# 438 paquets alors que certains se reselvent HORS de la bibliotheque du
# projet — typiquement depuis la bibliotheque SYSTEME. C'est un trou de
# reproductibilite SILENCIEUX :
#
#     ca marche sur cette machine  ->  renv::restore() sur une machine propre
#                                     ne le posera pas  ->  casse a l'arrivee
#
# Constate sur `ggpubr` : present dans renv.lock (ajoute avec CellChat, CC-1)
# mais resolu depuis R-4.4.2/library, jamais installe cote projet.
#
# Ce garde ne CHARGE aucun paquet (find.package() seulement) : il sort donc en 0,
# contrairement a tout processus R qui charge dplyr/ggplot2/igraph (segfault de
# teardown 139 sur cette machine).
#
# Usage : Rscript tools/check_renv_hermeticity.R [--strict]
#         --strict : liste aussi les paquets conformes.
# Sortie : 0 si hermetique, 1 sinon.
# =============================================================================

.pkg_norm <- function(p) {
  if (is.na(p) || !nzchar(p)) return(NA_character_)
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE),
           error = function(e) NA_character_)
}

run_hermeticity_check <- function(root = getwd(), verbose = FALSE) {
  lock_path <- file.path(root, "renv.lock")
  if (!file.exists(lock_path)) {
    cat("ERREUR : renv.lock introuvable :", lock_path, "\n")
    return(1L)
  }

  lock <- tryCatch(jsonlite::fromJSON(lock_path, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(lock) || is.null(lock$Packages)) {
    cat("ERREUR : renv.lock illisible ou sans section Packages.\n")
    return(1L)
  }

  libpaths <- .libPaths()
  proj_lib <- .pkg_norm(libpaths[1L])

  # Les paquets de base ne sont PAS geres par renv : jamais des contrevenants.
  base_pkgs <- c("base", "boot", "class", "cluster", "codetools", "compiler",
                 "datasets", "foreign", "graphics", "grDevices", "grid",
                 "KernSmooth", "lattice", "MASS", "Matrix", "methods", "mgcv",
                 "nlme", "nnet", "parallel", "rpart", "spatial", "splines",
                 "stats", "stats4", "survival", "tcltk", "tools", "utils")

  pkgs <- sort(names(lock$Packages))
  pkgs <- setdiff(pkgs, base_pkgs)

  cat("== 1) Bibliotheque du projet ==\n")
  cat("  ", proj_lib, "\n")
  cat("  (", length(libpaths), " chemin(s) dans .libPaths() )\n\n")

  outside <- character(0)
  missing <- character(0)
  for (p in pkgs) {
    path <- tryCatch(find.package(p, quiet = TRUE)[1L],
                     error = function(e) NA_character_)
    path <- .pkg_norm(path)
    if (is.na(path)) { missing <- c(missing, p); next }
    # Un paquet est hermetique si sa racine d'installation EST la bibliotheque
    # du projet (comparaison sur le chemin normalise, insensible a la casse).
    # startsWith() et non une regex : le chemin du projet contient des
    # parenthes/es et Windows n'est pas sensible a la casse.
    if (is.na(proj_lib) ||
        !startsWith(toupper(path), toupper(proj_lib))) {
      outside <- c(outside, p)
      src <- lock$Packages[[p]]$Source
      cat("  HORS PROJET : ", p, "  [", if (is.null(src)) "?" else src,
          "]\n      -> ", path, "\n")
    } else if (verbose) {
      cat("  ok : ", p, "\n")
    }
  }

  cat("\n== 2) Bilan ==\n")
  cat("  ", length(pkgs), " paquets du lock examines (base exclus).\n")
  cat("  hermetiques (bibliotheque projet) : ",
      length(pkgs) - length(outside) - length(missing), "\n")
  cat("  resolus HORS bibliotheque projet  : ", length(outside), "\n")
  cat("  introuvables                      : ", length(missing), "\n")
  if (length(missing)) {
    cat("\n  MANQUANTS :\n")
    for (p in missing) cat("    - ", p, "\n")
  }

  n_err <- length(outside) + length(missing)
  cat("\n---- Resume : ", n_err, " erreur(s) d'hermeticite ----\n")
  if (n_err > 0L) {
    cat("Un paquet Hors Projet ou Manquant rend renv::restore()\n")
    cat("non reproductible sur une machine propre.\n")
  }
  if (n_err > 0L) 1L else 0L
}

if (sys.nframe() == 0L) {
  a <- commandArgs(trailingOnly = TRUE)
  status <- run_hermeticity_check(getwd(), verbose = "--strict" %in% a)
  quit(status = status, save = "no")
}
