# =============================================================================
# test-helpers_pathway.R — tests for helpers_pathway.R's plot/table builders
# =============================================================================
# Scope: plot_pathway_barplot(), plot_pathway_dotplot(), build_pathway_dt() —
# the only functions in this file with no hard dependency on clusterProfiler/
# org.Hs.eg.db/org.Mm.eg.db/ReactomePA (those need a live Bioconductor
# annotation stack + network access to bitr()/enrichGO() and are integration-
# tested manually against a real dataset, not unit-tested here).
#
# run_pathway_enrichment() / run_gsea_enrichment() themselves are NOT
# covered: every code path needs clusterProfiler + an org.*.eg.db package
# before it can even validate its inputs (see each function's own early
# requireNamespace() guards) — there's no meaningful pure-logic slice to
# isolate without those.
# =============================================================================

source_project_file("R/plotting/palettes.R")
source_project_file("R/core/pathway_helpers.R")
# ggplot2 is required for plot builders but not auto-loaded by helper-source.R
if (requireNamespace("ggplot2", quietly = TRUE)) suppressPackageStartupMessages(library(ggplot2))

.toy_pathway_df <- function(n = 5) {
  data.frame(
    ID          = paste0("GO:", sprintf("%07d", seq_len(n))),
    Description = paste("Pathway", LETTERS[seq_len(n)]),
    p.adjust    = seq(0.001, by = 0.01, length.out = n),
    Count       = rev(seq_len(n)) * 3,
    GeneRatio   = paste0(rev(seq_len(n)) * 3, "/100"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# plot_pathway_barplot()
# ---------------------------------------------------------------------------
test_that("plot_pathway_barplot returns a ggplot object sized to top_n", {
  skip_if_not_installed("ggplot2")
  df <- .toy_pathway_df(10)
  p <- plot_pathway_barplot(df, db_label = "GOBP", top_n = 5)
  expect_s3_class(p, "gg")
  expect_equal(nrow(p$data), 5)
})

test_that("plot_pathway_barplot handles fewer rows than top_n gracefully", {
  skip_if_not_installed("ggplot2")
  df <- .toy_pathway_df(3)
  p <- plot_pathway_barplot(df, db_label = "GOBP", top_n = 15)
  expect_s3_class(p, "gg")
  expect_equal(nrow(p$data), 3)
})

# ---------------------------------------------------------------------------
# plot_pathway_dotplot()
# ---------------------------------------------------------------------------
test_that("plot_pathway_dotplot parses GeneRatio strings ('k/n') into a numeric ratio", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ID = "GO:1", Description = "Pathway A", p.adjust = 0.01,
                   Count = 3, GeneRatio = "3/50", stringsAsFactors = FALSE)
  p <- plot_pathway_dotplot(df, db_label = "GOBP", top_n = 5)
  expect_equal(p$data$GeneRatioNum, 3 / 50)
})

test_that("plot_pathway_dotplot falls back to Count/max(Count) when GeneRatio is absent", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ID = c("GO:1","GO:2"), Description = c("A","B"),
                   p.adjust = c(0.01, 0.02), Count = c(5, 10), stringsAsFactors = FALSE)
  p <- plot_pathway_dotplot(df, db_label = "GOBP", top_n = 5)
  expect_equal(p$data$GeneRatioNum, c(0.5, 1))
})

# ---------------------------------------------------------------------------
# build_pathway_dt()
# ---------------------------------------------------------------------------
test_that("build_pathway_dt renames the 5 standard columns to their French labels", {
  skip_if_not_installed("DT")
  df <- .toy_pathway_df(4)
  dt <- build_pathway_dt(df)
  expect_s3_class(dt, "datatables")
  # rownames = FALSE is passed explicitly -> no leading row-number column.
  expect_identical(colnames(dt$x$data), c("ID", "Description", "P-adj", "Nb G\u00e8nes", "Ratio"))
})

test_that("build_pathway_dt only keeps columns actually present in the input (defensive intersect)", {
  skip_if_not_installed("DT")
  # NOTE (finding, not asserted as a bug here): build_pathway_dt() renames
  # via a POSITIONAL vector after intersect()-ing the expected column names.
  # This is only correct when any missing columns are TRAILING in the
  # canonical order (ID, Description, p.adjust, Count, GeneRatio) — which
  # matches how this file's own callers (run_pathway_enrichment(),
  # run_gsea_enrichment()) always populate all 5. A future caller supplying
  # a partial data.frame with a MIDDLE column missing (e.g. Count without
  # GeneRatio) would get its trailing column mislabeled. Covered here only
  # for the trailing-omission case, which is what actually occurs today.
  df <- .toy_pathway_df(3)[, c("ID", "Description", "p.adjust")]  # trailing cols omitted
  dt <- build_pathway_dt(df)
  expect_identical(colnames(dt$x$data), c("ID", "Description", "P-adj"))
})
