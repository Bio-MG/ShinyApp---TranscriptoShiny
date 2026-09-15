# Reusable targeted test runner — Rscript tools/run_tests.R <filter> [<filter2> ...]
#
# Locale UTF-8 BEFORE any parse(): Git Bash exports LC_ALL=C.UTF-8, a name R
# does NOT recognise on Windows, so R silently falls back to "C", where it
# cannot parse some UTF-8 sources of the repo (STATUS.md, "Garde C13 +
# semantique testServer() MESUREE": "unexpected invalid token" on
# R/sc/sc_communication_perturbation.R). Not fatal if the locale is missing,
# but the warning must stay visible: a wrong locale yields FALSE failures.
.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}
args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) >= 1L) args else "bulk"
# testthat::test_dir(filter=) takes ONE regex, not a vector: given a character
# vector of length > 1 it silently uses only the FIRST element. Measured
# 2026-09-16: asking for c("core-jobs", "da-milo-async") ran only
# test-core-jobs.R and still printed a green TOTAL -- the requested file was
# silently not run. Collapse into an alternation so every filter is honoured.
filter <- paste(filter, collapse = "|")
options(testthat.progress.max_fails = 100)
res <- testthat::test_dir("tests/testthat", filter = filter, reporter = "silent",
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
cat("== filter:", paste(filter, collapse = ","), "==\n")
for (i in seq_len(nrow(df))) {
  cat(sprintf("  %-55s fail=%d pass=%d skip=%d warn=%d\n",
              df$file[i], df$failed[i], df$passed[i], df$skipped[i],
              if ("warning" %in% colnames(df)) df$warning[i] else 0))
}
cat(sprintf("TOTAL failed=%d passed=%d error=%d skipped=%d\n",
            sum(df$failed), sum(df$passed), sum(df$error), sum(df$skipped)))
if (sum(df$failed) > 0L || sum(df$error) > 0L) {
  # Re-run with a verbose reporter to show the actual failures.
  cat("\n---- FAILURES ----\n")
  testthat::test_dir("tests/testthat", filter = filter,
                     reporter = "summary", stop_on_failure = FALSE)
}
invisible(NULL)
