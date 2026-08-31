# =============================================================================
# helper-source.R — auto-sourced by testthat before any test-*.R file runs
# =============================================================================
# TranscriptoShiny has no package structure (no DESCRIPTION/NAMESPACE).
# This walks UP from the current working directory until it finds "app.R"
# to locate the project root.
# =============================================================================

.ts_find_project_root <- function(start = getwd(), marker = "app.R", max_up = 8) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop(
    "Impossible de localiser la racine du projet (fichier '", marker,
    "' introuvable en remontant depuis '", start, "').",
    call. = FALSE
  )
}

ts_project_root <- local({
  root <- NULL
  function() {
    if (is.null(root)) root <<- .ts_find_project_root()
    root
  }
})

#' Source a project file (relative to project root)
#' Handles both old-style top-level files and new R/ subdirectory files.
source_project_file <- function(relpath) {
  full <- file.path(ts_project_root(), relpath)
  if (!file.exists(full)) {
    stop("Fichier projet introuvable : ", full, call. = FALSE)
  }
  sys.source(full, envir = globalenv())
  invisible(full)
}

# Source config files first (if they exist — tests may run without them)
.local_source_if_exists <- function(relpath) {
  full <- file.path(ts_project_root(), relpath)
  if (file.exists(full)) sys.source(full, envir = globalenv())
}
.local_source_if_exists("config/defaults.R")
.local_source_if_exists("config/thresholds.R")
