# R/utils_spatial_async.R

#' Initialize Mirai Daemons for Spatial Processing
#'
#' Sets up a pool of 6 mirai daemons for handling heavy spatial computations
#' asynchronously without blocking the main Shiny session.
#' @param n_daemons Integer. Number of daemons to spawn (default: 6).
#' @importFrom mirai daemons
initialize_spatial_mirai <- function(n_daemons = 6) {
  # Check if daemons are already running to avoid duplication
  if (mirai::status()$daemons == 0) {
    mirai::daemons(n = n_daemons, seed = TRUE)
  }
}

#' Write Log Message from Within a Mirai Daemon
#'
#' Appends a timestamped message to a log file. This function is designed
#' to be called inside the mirai expression to track progress.
#' @param file Character. Path to the log file.
#' @param message Character. The message to log.
write_mirai_log <- function(file, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "] ", message, "\n")
  cat(log_line, file = file, append = TRUE)
}

#' Create Reactive Progress Tracker
#'
#' Sets up a reactivePoll that monitors a log file for changes and updates
#' a Shiny progress bar accordingly.
#' @param session Shiny session object.
#' @param log_file Character. Path to the log file to monitor.
#' @param progress_id Character. ID for the progress bar (optional).
#' @return A reactive expression that returns the latest log content.
#' @importFrom shiny reactivePoll invalidateLater
#' @importFrom bslib ExtendedTask
create_reactive_tracker <- function(session, log_file, progress_id = NULL) {
  
  # Store last modification time to detect changes
  last_mtime <- reactiveVal(0)
  
  # Reactive poll to check file modification time
  file_changed <- reactivePoll(
    intervalMillis = 1000,
    session = session,
    checkFunc = function() {
      if (file.exists(log_file)) {
        mtime <- file.mtime(log_file)
        if (!identical(mtime, last_mtime())) {
          last_mtime(mtime)
          return(mtime)
        }
      }
      return(NULL)
    },
    valueFunc = function() {
      if (file.exists(log_file)) {
        return(readLines(log_file, warn = FALSE))
      }
      return(character(0))
    }
  )
  
  # Observe changes and update UI (e.g., show toast or update text output)
  observeEvent(file_changed(), {
    logs <- file_changed()
    if (length(logs) > 0) {
      # Optional: Send notification or update a textOutput with the latest log
      # For now, we just ensure the reactive chain is active
      last_log <- tail(logs, 1)
      # You can integrate this with a bslib::update_progress_bar if needed
    }
  }, ignoreInit = TRUE)
  
  return(file_changed)
}
