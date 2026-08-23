# =============================================================================
# tests/testthat.R — standard testthat entry point
# =============================================================================
# TranscriptoShiny has no DESCRIPTION/package structure, so this is invoked
# directly rather than via R CMD check / devtools::test():
#
#   Rscript tests/testthat.R
#
# ...run from the PROJECT ROOT (same convention as tests/testthat/helper-
# source.R's root-finder). Add this as a CI step (GitHub Actions / GitLab
# CI) to gate merges on the pure-helper regression suite.
# =============================================================================

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' requis pour lancer la suite de tests : install.packages('testthat').",
       call. = FALSE)
}

test_dir_path <- file.path("tests", "testthat")
if (!dir.exists(test_dir_path)) {
  stop(
    "Dossier 'tests/testthat' introuvable depuis le repertoire courant (",
    getwd(), "). Lancez ce script depuis la racine du projet, ex:\n",
    "  Rscript tests/testthat.R\n",
    "depuis le dossier contenant app.R.", call. = FALSE
  )
}

results <- testthat::test_dir(test_dir_path, reporter = "summary", stop_on_failure = FALSE)

# Explicit non-zero exit on any failure — testthat::test_dir() itself does
# not call quit(), so a CI step relying on this script's own exit code needs
# this final check (test_dir()'s return value carries per-test results we
# can summarize directly instead of parsing reporter text output).
df <- as.data.frame(results)
n_fail <- sum(df$failed > 0 | df$error, na.rm = TRUE)
if (n_fail > 0) {
  cat(sprintf("\n%d fichier(s) de test avec au moins un echec.\n", n_fail))
  quit(status = 1, save = "no")
}
cat("\nTous les tests sont passes.\n")
