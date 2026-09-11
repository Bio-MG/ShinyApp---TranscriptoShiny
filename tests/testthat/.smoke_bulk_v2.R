# Smoke test M1 — run with Rscript, deleted after validation
options(warn = 1)
library(ggplot2)
`%||%` <- function(a, b) if (is.null(a)) b else a
sys.source("R/core/validation.R", envir = globalenv())
sys.source("R/bulk/bulk_helpers.R", envir = globalenv())
sys.source("R/bulk/bulk_provenance.R", envir = globalenv())
sys.source("R/bulk/bulk_batch_qc.R", envir = globalenv())

set.seed(1)
m <- matrix(rnorm(20 * 12), 20, 12, dimnames = list(paste0("g", 1:20), paste0("s", 1:12)))
meta <- data.frame(row.names = paste0("s", 1:12),
                   batch = rep(c("b1", "b2"), 6),
                   condition = rep(c("A", "B"), each = 6))

# --- provenance (functional contract: caller assigns the returned object) ---
bo <- list(counts = matrix(1:24, 4, 6), metadata = meta)
bo <- bulk_ensure_provenance(bo)
bo <- bulk_update_provenance(bo, reference_genome = "GRCh38")
stopifnot(identical(bo$provenance$reference_genome, "GRCh38"),
          length(bo$provenance$history) == 1L,
          identical(bo$provenance$type, "bulk_provenance"))
cat("prov: genome =", bo$provenance$reference_genome,
    "| history =", length(bo$provenance$history),
    "| pkgs =", length(bo$provenance$key_packages), "\n")
cat("prov df rows:", nrow(bulk_provenance_dataframe(bo$provenance)), "\n")

# --- batch design check (balanced, not collinear) ---
chk <- bulk_batch_design_check(meta, "batch", "condition")
stopifnot(isFALSE(chk$fully_collinear), length(chk$warning_messages) == 0L)
cat("design check: collinear =", chk$fully_collinear, "| warns =", length(chk$warning_messages), "\n")

# --- fully collinear design ---
meta_conf <- data.frame(row.names = paste0("s", 1:12),
                        batch = rep(c("b1", "b2"), each = 6),
                        condition = rep(c("A", "B"), each = 6))
chk2 <- bulk_batch_design_check(meta_conf, "batch", "condition")
stopifnot(isTRUE(chk2$fully_collinear), length(chk2$warning_messages) >= 1L)
cat("confounded design detected:", chk2$fully_collinear, "\n")

# --- variance partition (variancePartition path) ---
vp <- bulk_variance_partition(m, meta, c("batch", "condition"))
stopifnot(nrow(vp$var_part) == 20L, all(c("batch", "condition") %in% names(vp$var_part)))
cat("varpart:", vp$method, "|", paste(dim(vp$var_part), collapse = "x"), "\n")

# --- single covariate path ---
vp2 <- bulk_variance_partition(m, meta, c("batch"))
stopifnot(nrow(vp2$var_part) == 20L)
cat("varpart single cov OK (method:", vp2$method, ")\n")

# --- raw counts rejected ---
raw_m <- matrix(rpois(600, 500), 50, 12)
err <- tryCatch({ bulk_assert_transformed_matrix(raw_m, "test"); NULL },
                error = function(e) e)
stopifnot(!is.null(err), inherits(err, "bulk_batch_qc_error"),
          identical(err$state, "raw_counts_rejected"))
cat("raw counts rejected:", err$state, "\n")

# --- plots build ---
stopifnot(inherits(plot_bulk_varpart(vp), "ggplot"))
stopifnot(inherits(plot_bulk_batch_scree(m), "ggplot"))
cat("plots OK\n")

cat("SMOKE-OK-M1\n")
