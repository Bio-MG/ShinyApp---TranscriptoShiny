# Reusable targeted test runner — Rscript tools/run_tests.R <filter> [<filter2> ...]
args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) >= 1L) args else "bulk"
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
