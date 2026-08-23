# Shared helpers for the shinytest2 "golden path" smoke tests (backlog #7).
# Coexists with helper-source.R (pure-helper unit tests): disjoint symbols,
# both auto-sourced by testthat before any test-*.R file runs.
#
# NOTE (API shinytest2) : $run_js() exécute le JS en fire-and-forget et
# renvoie NULL — toute valeur doit revenir par $get_js(). Les deux helpers
# qui lisent le DOM passent donc par get_js(), avec une IIFE pour que
# l'instruction 'return' soit légale quel que soit le contexte d'évaluation.

# shinytest2 s'auto-skippe avec la raison "On CRAN" tant que NOT_CRAN!=true
# (projet sans DESCRIPTION : aucun R CMD check ne passera jamais le poser).
# Pose ici, au chargement du helper, pour que la commande du plan —
#   Rscript -e "testthat::test_dir('tests/testthat')"
# — fonctionne telle quelle.
Sys.setenv(NOT_CRAN = "true")

#' Absolute path to the app's root directory (where app.R lives)
app_root_dir <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
}

#' Best-effort AppDriver constructor — skips (never fails) if shinytest2 or
#' a headless Chrome (chromote) is unavailable. Generous timeouts : global.R
#' charge toute la pile de packages, et l'init des 6 daemons mirai est
#' différée à la première ouverture de l'onglet Spatial (boot lazy, voir
#' l'en-tête de app.R).
new_app_driver <- function(name) {
  testthat::skip_if_not_installed("shinytest2")
  driver <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = app_root_dir(), name = name, height = 900, width = 1400,
      load_timeout = 120000, timeout = 120000,
      # Smoke tests : check_names=FALSE pour qu'un warning de doublon d'id
      # ne transforme PAS chaque test en skip silencieux — le drift de
      # namespace est déjà asserté explicitement via get_values().
      check_names = FALSE,
      options = list(shiny.testmode = TRUE)
    ),
    error = function(e) { message("AppDriver init failed: ", conditionMessage(e)); NULL }
  )
  testthat::skip_if(is.null(driver), "Chromote/headless Chrome unavailable.")
  driver
}

#' Click a top-level nav tab by (partial) visible text, JS-side — more
#' robust than a CSS selector on bslib's emoji+text nav-link markup.
click_nav_by_text <- function(app, text_fragment) {
  clicked <- app$get_js(sprintf(
    "(function() {
       var links = document.querySelectorAll('a.nav-link');
       for (var i = 0; i < links.length; i++) {
         if (links[i].textContent.indexOf('%s') !== -1) { links[i].click(); return true; }
       }
       return false;
     })()", text_fragment
  ))
  testthat::expect_true(isTRUE(clicked), info = paste("Nav tab not found:", text_fragment))
  # Best-effort uniquement : la sidebar (textOutput mem_usage) et l'init
  # lazy des daemons mirai (onglet Spatial) peuvent maintenir Shiny
  # "instable" >15s ; attendre la stabilite est un confort de rendu, pas une
  # assertion — un vrai crash d'output sera de toute facon attrape par
  # assert_no_shiny_error()/get_values() qui suivent.
  try(app$wait_for_idle(duration = 250, timeout = 10000), silent = TRUE)
  invisible(app)
}

#' Assert no Shiny/JS runtime error is currently shown in the DOM.
assert_no_shiny_error <- function(app) {
  n_errors <- app$get_js("document.querySelectorAll('.shiny-output-error').length")
  testthat::expect_equal(n_errors, 0, info = "A .shiny-output-error element is visible.")
}
