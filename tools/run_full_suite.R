# tools/run_full_suite.R — full test suite, crash-resilient per-file bilan.
# Rationale: R exits with SIGSEGV (139) on this environment when some
# package combos unload (STATUS.md §2l) — with file-redirected stdout the
# test_dir summary is lost. Running file-by-file inside ONE process keeps
# the shared helper state AND flushes a result line per file, so a crash
# is isolated to its file and everything before it survives.
files <- sort(list.files("tests/testthat", pattern = "^test-.*\\.R$",
                         full.names = TRUE))
cat(sprintf("files: %d\n", length(files)))
results_file <- "full_suite_results.txt"
con <- file(results_file, open = "wt")
writeLines(sprintf("start %s | %d files", format(Sys.time(), "%H:%M:%S"), length(files)), con)
flush(con)

total_failed <- 0L; total_passed <- 0L; total_error <- 0L; total_skipped <- 0L
crashed_after <- NA_character_
for (f in files) {
  bn <- basename(f)
  r <- tryCatch({
    res <- testthat::test_file(f, reporter = "silent", stop_on_failure = FALSE,
                               env = testthat::test_env(),
                               load_package = "none")
    df <- as.data.frame(res)
    sprintf("%-58s fail=%d pass=%d err=%d skip=%d",
            bn, sum(df$failed), sum(df$passed), sum(df$error), sum(df$skipped))
  }, error = function(e) sprintf("%-58s RUNNER-ERROR: %.120s", bn, conditionMessage(e)))
  writeLines(r, con); flush(con)
  if (grepl("RUNNER-ERROR", r)) next
  # parse numbers for the tally (best-effort)
  nums <- as.integer(regmatches(r, gregexpr("(-?\\d+)", r))[[1]])
  if (length(nums) >= 4L) {
    total_failed <- total_failed + nums[1]
    total_passed <- total_passed + nums[2]
    total_error  <- total_error + nums[3]
    total_skipped <- total_skipped + nums[4]
  }
  rm(r)
}
writeLines(sprintf("BILAN: failed=%d passed=%d error=%d skipped=%d | %s",
                   total_failed, total_passed, total_error, total_skipped,
                   format(Sys.time(), "%H:%M:%S")), con)
close(con)
