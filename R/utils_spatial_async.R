# =============================================================================
# R/utils_spatial_async.R — mirai daemon pool + reactivePoll progress tracking
# =============================================================================
# Pure utilities, no Shiny module code. Called from:
#   - app.R (once, at startup)         -> init_spatial_daemons()
#   - modules/spatial/mod_spatial_*.R  -> spatial_log_path(), reset_log(),
#                                          create_reactive_tracker(), parse_log_progress()
#   - *inside* mirai daemons            -> write_mirai_log()
#
# Daemons are process-level (shared by every Shiny session running in this R
# process), so init_spatial_daemons() is idempotent — it only spawns workers
# the first time it is called, and pre-loads Seurat/BPCells + this project's
# own helper files on every daemon via mirai::everywhere() so individual
# mirai() calls stay small (no need to re-serialize function bodies/library()
# calls on every single task).
# =============================================================================

.spatial_async_env <- new.env(parent = emptyenv())
.spatial_async_env$daemons_ready <- FALSE

#' Initialize the mirai daemon pool used by all spatial async tasks
#'
#' Idempotent: safe to call multiple times (e.g. defensively from a module
#' server) — daemons are only spawned once per R process. Pre-loads Seurat +
#' BPCells and sources this project's helper files on every daemon so heavy
#' packages/functions do not need to be re-shipped on every mirai() call.
#'
#' @param n_daemons Integer, number of persistent background R processes.
#'   Kept small (default 6) to preserve OS resources on a local workstation
#'   (CPU-only, 32 Go RAM) — see project hard rules.
#' @param source_files Character vector of project file paths to source()
#'   inside every daemon (relative to the app's working directory).
#' @return invisible(TRUE) on success, invisible(FALSE) if mirai is missing.
init_spatial_daemons <- function(n_daemons = 6,
                                  source_files = c("R/utils_spatial_async.R",
                                                    "R/utils_spatial_io.R")) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    warning("Package 'mirai' manquant : les calculs spatiaux asynchrones (clustering, ",
            "deconvolution, indice de Moran) seront indisponibles. Installez-le via ",
            "install.packages('mirai').")
    return(invisible(FALSE))
  }

  if (isTRUE(.spatial_async_env$daemons_ready)) return(invisible(TRUE))

  mirai::daemons(n_daemons)

  # Pre-load on every current + future daemon (mirai::everywhere runs once
  # per worker, not once per task) — keeps individual mirai() bodies short.
  tryCatch({
    mirai::everywhere({
      suppressPackageStartupMessages({
        if (requireNamespace("Seurat", quietly = TRUE))  library(Seurat)
        if (requireNamespace("BPCells", quietly = TRUE)) library(BPCells)
      })
      for (.f in .source_files) if (file.exists(.f)) source(.f)
    }, .source_files = source_files)
  }, error = function(e) {
    warning("mirai::everywhere() a echoue lors du preload des daemons : ", conditionMessage(e))
  })

  .spatial_async_env$daemons_ready <- TRUE
  message(sprintf("[spatial] %d daemon(s) mirai initialises.", n_daemons))
  invisible(TRUE)
}

#' Are the spatial mirai daemons ready?
spatial_daemons_ready <- function() isTRUE(.spatial_async_env$daemons_ready)

#' Stop and free the daemon pool (e.g. on app shutdown / testing teardown)
stop_spatial_daemons <- function() {
  if (isTRUE(.spatial_async_env$daemons_ready) && requireNamespace("mirai", quietly = TRUE)) {
    tryCatch(mirai::daemons(0), error = function(e) NULL)
  }
  .spatial_async_env$daemons_ready <- FALSE
  invisible(NULL)
}

#' Build a stable per-session, per-task log file path
#'
#' One fixed path per (session, task) pair — reused/truncated across runs
#' (see reset_log()) rather than a new tempfile per click, so a single
#' create_reactive_tracker() can be set up once per module.
#'
#' @param session Shiny session object (used for session$token uniqueness).
#' @param task_name Short slug, e.g. "cluster", "deconv", "moran".
#' @return Character path inside tempdir().
spatial_log_path <- function(session, task_name) {
  file.path(tempdir(), sprintf("spatial_%s_%s.log", task_name, session$token))
}

#' Truncate/create a log file before starting a new async run
#' @param file Character path.
#' @return invisible(file)
reset_log <- function(file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  cat("", file = file)
  invisible(file)
}

#' Write a timestamped progress line — call this *inside* a mirai daemon
#'
#' Self-contained on purpose (only base R): it runs in a separate R process
#' with no access to the caller's environment beyond what mirai() explicitly
#' passes in, which is exactly `file` (and optionally step/total).
#'
#' @param file Character path to the log file (same one the tracker reads).
#' @param message Character, user-facing progress message (French).
#' @param step,total Optional integers rendered as a "[step/total]" prefix,
#'   consumed by parse_log_progress() to drive a numeric progress bar.
write_mirai_log <- function(file, message, step = NULL, total = NULL) {
  prefix <- if (!is.null(step) && !is.null(total)) sprintf("[%d/%d] ", step, total) else ""
  line <- sprintf("%s | %s%s\n", format(Sys.time(), "%H:%M:%S"), prefix, message)
  cat(line, file = file, append = TRUE)
}

#' reactivePoll-based tracker for a task's log file
#'
#' Polls file.mtime() every `interval_ms` (spec: 1000ms) and re-reads the
#' file only when it actually changed — cheap even for long-running tasks.
#'
#' @param session Shiny session.
#' @param log_file Character path (stable for the module's lifetime — see
#'   spatial_log_path()).
#' @param interval_ms Poll interval in milliseconds.
#' @return A reactive expression returning the log's lines (character vector).
create_reactive_tracker <- function(session, log_file, interval_ms = 1000) {
  shiny::reactivePoll(
    intervalMillis = interval_ms,
    session        = session,
    checkFunc = function() {
      if (file.exists(log_file)) file.info(log_file)$mtime else NA
    },
    valueFunc = function() {
      if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
    }
  )
}

#' Turn tracked log lines into a small textual/numeric progress summary
#'
#' @param log_lines Character vector, as returned by create_reactive_tracker().
#' @return list(text = <last line or placeholder>, pct = <0-100 or NA>).
parse_log_progress <- function(log_lines) {
  if (length(log_lines) == 0) return(list(text = "En attente...", pct = NA_real_))
  last <- utils::tail(log_lines, 1)
  m <- regmatches(last, regexec("\\[(\\d+)/(\\d+)\\]", last))[[1]]
  pct <- if (length(m) == 3) round(100 * as.numeric(m[2]) / as.numeric(m[3])) else NA_real_
  list(text = last, pct = pct)
}
