# =============================================================================
# helper-source.R — auto-sourced by testthat before any test-*.R file runs
# (testthat convention: any tests/testthat/helper*.R is loaded first).
# =============================================================================
# TranscriptoShiny has no package structure (no DESCRIPTION/NAMESPACE) — it's
# a flat Shiny app where app.R source()s every helper/module file directly.
# testthat::test_path()/test_dir() therefore can't rely on package-install
# conventions to locate the project root; this walks UP from the current
# working directory (wherever `Rscript -e 'testthat::test_dir("tests/testthat")'`
# or `testthat::test_file()` was invoked from) until it finds "app.R" — the
# same root marker used throughout this project's own tooling.
#
# Deliberately does NOT source app.R/global.R themselves: those have real
# side effects (future::plan(multisession, ...), options(shiny.maxRequestSize),
# package-availability warnings) that a unit test run should never trigger.
# Each helper file under test is self-contained (function definitions only,
# no top-level side effects) — see each file's own header.
# =============================================================================

.ts_find_project_root <- function(start = getwd(), marker = "app.R", max_up = 8) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break  # reached filesystem root
    d <- parent
  }
  stop(
    "Impossible de localiser la racine du projet (fichier '", marker, "' introuvable ",
    "en remontant depuis '", start, "'). Lancez les tests depuis la racine du projet, ",
    "ex: `Rscript -e \"testthat::test_dir('tests/testthat')\"` depuis le dossier ",
    "contenant app.R.", call. = FALSE
  )
}

# Cached once per test session — every test-*.R file needing the project
# root calls this instead of re-walking the filesystem each time.
ts_project_root <- local({
  root <- NULL
  function() {
    if (is.null(root)) root <<- .ts_find_project_root()
    root
  }
})

#' Source a project file (relative to the project root) into the global env
#'
#' Safe to call multiple times with the same path across different test
#' files (idempotent — R just re-defines the same functions).
#'
#' @param relpath Character, path relative to the project root, e.g.
#'   "helpers_bulk.R" or "R/utils_spatial_stats.R".
source_project_file <- function(relpath) {
  full <- file.path(ts_project_root(), relpath)
  if (!file.exists(full)) {
    stop("Fichier projet introuvable : ", full, call. = FALSE)
  }
  sys.source(full, envir = globalenv())
  invisible(full)
}
