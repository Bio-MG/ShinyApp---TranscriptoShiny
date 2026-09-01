# =============================================================================
# R/core/jobs.R — Wrapper fin d'execution sync/async (CHRYSALIS 2D)
# =============================================================================
# REGLE 9 (AGENTS.md) : run_job() est un wrapper FIN. Aucun second framework
# asynchrone : le chemin async=TRUE imite le pattern mirai EXISTANT de
# R/spatial/spatial_async.R (mirai::mirai(...)[], collect bloquant, cf.
# .verify_spatial_daemons()) et exige un pool DEJA initialise par le module
# spatial (init_spatial_daemons(), differe a l'ouverture de l'onglet Spatial).
# R/core/jobs.R ne demarre JAMAIS de daemons lui-meme et ne depend d'aucun
# fichier de domaine : sans pool actif (ou sans 'mirai'), repli synchrone
# transparent avec un warning.
#
# Chemin sync : execution inline de fn(...) — le comportement de tous les
# modules actuels, preserve a l'identique.
#
# fn doit etre AUTONOME sur le chemin async (serialise vers le daemon) :
# une fonction pure ou une closure qui n'appelle que des fonctions preloadees
# dans le pool (cf. source_files de init_spatial_daemons()).
# =============================================================================

#' Extraire un message lisible d'une erreur R ou d'une errorValue mirai
.err_job_message <- function(e) {
  if (inherits(e, "condition")) {
    conditionMessage(e)
  } else {
    tryCatch(paste(as.character(e)[1], collapse = " "), error = function(e2) "Erreur mirai inconnue")
  }
}

#' Executer un calcul long via le contrat commun sync/async
#'
#' @param fn Fonction a executer (autonome si async=TRUE, voir en-tete).
#' @param ... Arguments passes a fn().
#' @param async Si TRUE et qu'un pool mirai est actif, delegation au pattern
#'   spatial (collect BLOQUANT — l'integration non-bloquante ExtendedTask des
#'   modules n'est pas du ressort de ce wrapper). Sans pool : warning + repli
#'   synchrone. Si FALSE (defaut) : execution inline, comportement historique.
#' @param on_progress Callback optionnel function(message character) appele
#'   aux etapes "demarrage"/"termine" (+"soumission mirai" en async). Les
#'   erreurs du callback sont ignorees (jamais fatales au job).
#' @param on_error Callback optionnel function(condition) : si fourni, une
#'   erreur de fn() est transmise au callback et run_job() retourne NULL
#'   (invisible) — le module garde la main sur l'UI. Si NULL (defaut),
#'   l'erreur est relancee (comportement historique des tryCatch modules).
#' @param timeout_ms Plafond du job mirai en ms (argument .timeout natif de
#'   mirai ; NULL = pas de plafond propre au wrapper, les plafonds spatiaux
#'   MIRAI_TASK_TIMEOUT_MS etc. restent geres par les modules).
#' @return Le resultat de fn(...), ou NULL (invisible) si on_error a absorbe
#'   une erreur.
run_job <- function(fn, ..., async = FALSE, on_progress = NULL, on_error = NULL,
                    timeout_ms = NULL) {
  if (!is.function(fn)) {
    stop("run_job() : 'fn' doit etre une fonction.", call. = FALSE)
  }
  .progress <- function(msg) {
    if (is.function(on_progress)) {
      tryCatch(on_progress(msg), error = function(e) NULL)
    }
  }
  .handle_error <- function(e) {
    if (is.function(on_error)) {
      tryCatch(on_error(e), error = function(e2) NULL)
      invisible(NULL)
    } else {
      stop(e)
    }
  }
  .run_sync <- function() {
    .progress("demarrage")
    tryCatch(
      {
        res <- fn(...)
        .progress("termine")
        res
      },
      error = function(e) .handle_error(e)
    )
  }

  if (!isTRUE(async)) return(.run_sync())

  # ── Chemin async — delegation au pattern mirai existant ───────────────────
  if (!requireNamespace("mirai", quietly = TRUE)) {
    warning("Package 'mirai' absent : execution synchrone de secours pour ce job.",
            call. = FALSE)
    return(.run_sync())
  }
  if (!mirai::daemons_set()) {
    warning("Aucun pool mirai actif (init_spatial_daemons() non appele) : ",
            "execution synchrone de secours pour ce job.", call. = FALSE)
    return(.run_sync())
  }

  .progress("soumission mirai")
  job_args <- list(...)
  res <- tryCatch(
    mirai::mirai(do.call(fn, job_args), fn = fn, job_args = job_args,
                 .timeout = timeout_ms)[],
    error = function(e) e
  )

  err <- NULL
  if (inherits(res, "error")) {
    err <- res
  } else if (requireNamespace("mirai", quietly = TRUE) &&
             isTRUE(mirai::is_error_value(res))) {
    # Timeout mirai ou echec daemon : errorValue (pas une condition R native)
    err <- simpleError(.err_job_message(res))
  }
  if (!is.null(err)) return(.handle_error(err))

  .progress("termine")
  res
}
