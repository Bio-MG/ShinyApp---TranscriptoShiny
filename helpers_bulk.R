# =============================================================================
# helpers_bulk.R — Bulk RNA-seq differential expression engine + plots
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure functions
# used by the mod_bulk_* family (mod_bulk_filter.R, mod_bulk_de.R,
# mod_bulk_report.R). No Shiny reactivity here.
#
# Contents:
#   - Filtering        : filter_bulk_counts()
#   - DESeq2 engine     : build_dds(), check_design_confounding(),
#                         validate_bulk_design(), extract_deseq2_contrast()
#   - Fallback engines  : run_edger_de(), run_limma_voom_de(),
#                         run_bulk_de_dispatch(), .normalize_de_cols()
#   - Plots             : get_vst_matrix(), plot_bulk_pca(), plot_volcano_bulk(),
#                         plot_ma_bulk(), plot_heatmap_bulk(),
#                         plot_sample_correlation_heatmap()
#   - DT tables         : build_de_results_dt()
#
# Pathway enrichment (ORA/GSEA) is shared with single-cell — see
# helpers_pathway.R, NOT duplicated here even though it's used by
# mod_bulk_pathways.R too.
# Depends on: DESeq2/edgeR/limma (one of, checked via has_deseq2/has_edger/
# has_limma flags set in global.R), ComplexHeatmap (optional, ggplot fallback),
# ggplot2, dplyr.
# =============================================================================







#' Filter low-count genes from a bulk counts matrix

#'

#' @param counts_matrix Numeric matrix, genes in rows, samples in columns.

#' @param min_count Minimum total count across all samples to keep a gene.

#' @param min_samples Minimum number of samples reaching `min_count_per_sample`.

#' @param min_count_per_sample Per-sample count threshold used with `min_samples`.

#' @return Filtered numeric matrix (same column order, fewer rows).

filter_bulk_counts <- function(counts_matrix, min_count = 10,

                                min_samples = 1, min_count_per_sample = 1) {

  keep_total <- rowSums(counts_matrix, na.rm = TRUE) >= min_count

  keep_n     <- rowSums(counts_matrix >= min_count_per_sample, na.rm = TRUE) >= min_samples

  out <- counts_matrix[keep_total & keep_n, , drop = FALSE]

  if (nrow(out) == 0) stop("Aucun gène ne passe le filtre — seuils trop stricts.")

  out

}



#' Build a DESeqDataSet and (optionally) fit DESeq()

#'

#' @param counts_matrix Integer-like counts matrix (genes x samples).

#' @param metadata Sample metadata; rownames must match colnames(counts_matrix).

#' @param design_formula Character, e.g. "~ condition" or "~ batch + condition".

#' @param run_deseq Logical — if FALSE, returns the unfit DDS (used for blind VST).

#' @return A DESeqDataSet (fit or unfit depending on `run_deseq`).

build_dds <- function(counts_matrix, metadata, design_formula = "~ condition",

                       run_deseq = TRUE) {

  if (!requireNamespace("DESeq2", quietly = TRUE)) {

    stop("Le package 'DESeq2' est requis pour cette analyse.")

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  if (length(common) < 2) {

    stop("Moins de 2 échantillons communs entre la matrice de counts et les métadonnées.")

  }

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  # Convert character design columns to factor explicitly, BEFORE

  # DESeqDataSetFromMatrix — silences the "converting to factors" warning

  # and gives us control over level ordering (first level = reference).

  design_terms <- all.vars(stats::as.formula(design_formula))

  for (term in design_terms) {

    if (term %in% colnames(metadata) && is.character(metadata[[term]])) {

      metadata[[term]] <- factor(metadata[[term]])

    }

  }



  if (any(counts_matrix < 0, na.rm = TRUE)) {

    stop("La matrice de counts contient des valeurs négatives — DESeq2 requiert des counts bruts.")

  }

  if (any(counts_matrix != round(counts_matrix), na.rm = TRUE)) {

    warning("Valeurs non-entières détectées — arrondi automatique (DESeq2 requiert des counts entiers).")

    counts_matrix <- round(counts_matrix)

  }



  dds <- DESeq2::DESeqDataSetFromMatrix(

    countData = counts_matrix,

    colData   = metadata,

    design    = stats::as.formula(design_formula)

  )

  if (run_deseq) dds <- DESeq2::DESeq(dds, quiet = TRUE)

  dds

}



#' Detect if a covariate is fully confounded with the main condition

#'

#' DESeq2's design matrix loses full rank when a covariate level maps to

#' exactly one condition level (and vice-versa) — this produces a cryptic

#' "model matrix is not full rank" error deep inside DESeq(). This check

#' runs BEFORE fitting, so the UI can show a clear, actionable message

#' instead of a low-level matrix algebra error.

#'

#' @param metadata Sample metadata data.frame.

#' @param condition_col Character, name of the main condition column.

#' @param covariate_col Character, name of the covariate column to check.

#' @return Logical — TRUE if covariate_col is fully confounded with condition_col.

check_design_confounding <- function(metadata, condition_col, covariate_col) {

  if (!condition_col %in% colnames(metadata) || !covariate_col %in% colnames(metadata)) {

    return(FALSE)

  }

  tbl <- table(

    factor(metadata[[condition_col]]),

    factor(metadata[[covariate_col]])

  )

  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(FALSE)



  # Confounded when every covariate level is observed in only one condition

  # level (cross-tab has exactly one non-zero cell per covariate column).

  nonzero_per_covariate_level <- colSums(tbl > 0)

  all(nonzero_per_covariate_level == 1)

}



#' Validate a full design (condition + optional covariates) before fitting
#'
#' @param metadata Sample metadata data.frame.
#' @param condition_col Character, main condition column.
#' @param covariates Character vector of additional covariate columns (may be empty).
#' @return Character vector of human-readable problem descriptions (empty if none).
validate_bulk_design <- function(metadata, condition_col, covariates = character(0)) {
  problems <- character(0)

  if (!condition_col %in% colnames(metadata)) return(problems)

  n_na <- sum(is.na(metadata[[condition_col]]))
  if (n_na > 0) {
    problems <- c(problems, sprintf(
      "%d échantillon(s) ont une valeur manquante (NA) dans '%s'.", n_na, condition_col
    ))
  }

  grp_counts <- table(metadata[[condition_col]])
  if (any(grp_counts < 2)) {
    problems <- c(problems, sprintf(
      "Groupe(s) avec un seul réplicat dans '%s' (sur '%s' seule) : %s — résultats statistiquement peu fiables.",
      condition_col, condition_col, paste(names(grp_counts)[grp_counts < 2], collapse = ", ")
    ))
  }

  # -- Effective (complete-case) replicate check -------------------------
  # A covariate's own NAs can silently shrink a group to n=1 even though
  # the naive check above (condition_col alone) looked fine -- common with
  # badly-parsed GEO coldata (e.g. a "batch" column with blanks for some
  # samples). DESeq2 drops incomplete rows from the design matrix, so the
  # EFFECTIVE n actually used for testing can be smaller than what the
  # condition column alone suggests.
  covariates <- intersect(covariates, colnames(metadata))
  if (length(covariates) > 0) {
    design_cols <- unique(c(condition_col, covariates))
    complete    <- stats::complete.cases(metadata[, design_cols, drop = FALSE])
    n_dropped   <- sum(!complete)

    if (n_dropped > 0) {
      grp_counts_cc <- table(metadata[[condition_col]][complete])
      if (any(grp_counts_cc < 2)) {
        problems <- c(problems, sprintf(
          paste0("Groupe(s) avec un seul réplicat APRÈS exclusion de %d échantillon(s) ayant un NA ",
                 "dans une covariable (%s) : %s. Vérifiez vos métadonnées (coldata GEO mal parsé ?)."),
          n_dropped, paste(covariates, collapse = ", "),
          paste(names(grp_counts_cc)[grp_counts_cc < 2], collapse = ", ")
        ))
      }
    }
  }

  # -- Single-level covariate ----------------------------------------------
  # A covariate with only one observed level contributes nothing to the
  # model -- R's contrast coding produces zero columns for it, so DESeq2
  # silently fits ~ condition_col alone while the user believes they are
  # also correcting for this covariate. No crash, no warning: just a
  # design that quietly doesn't do what was asked.
  for (cov in covariates) {
    n_levels <- length(unique(metadata[[cov]][!is.na(metadata[[cov]])]))
    if (n_levels < 2) {
      only_val <- if (n_levels == 1) as.character(unique(na.omit(metadata[[cov]]))) else "aucune valeur exploitable"
      problems <- c(problems, sprintf(
        paste0("La covariable '%s' n'a qu'une seule modalité observée (%s) - elle n'apporte aucune ",
               "information, sera ignorée silencieusement par le design (~ %s), et ne corrige donc PAS ",
               "pour cet effet comme attendu. Retirez-la du design."),
        cov, only_val, paste(unique(c(covariates, condition_col)), collapse = " + ")
      ))
    }
  }

  for (cov in covariates) {
    if (check_design_confounding(metadata, condition_col, cov)) {
      problems <- c(problems, sprintf(
        "La covariable '%s' est entièrement confondue avec '%s' - son effet ne peut pas être estimé séparément (design non plein rang). Retirez-la ou vérifiez votre plan d'expérience.",
        cov, condition_col
      ))
    }
  }

  problems
}



#' Extract a (shrunk) contrast result table from a fitted DESeqDataSet

#'

#' @param dds Fitted DESeqDataSet (post-DESeq()).

#' @param condition_col Column name used for the contrast.

#' @param group_target Numerator level ("treatment").

#' @param group_ref Denominator level ("reference").

#' @param shrink Apply apeglm/normal LFC shrinkage when possible.

#' @param alpha Significance threshold passed to results().

#' @return data.frame: gene, baseMean, log2FoldChange, pvalue, padj, ...

extract_deseq2_contrast <- function(dds, condition_col, group_target, group_ref,

                                     shrink = TRUE, alpha = 0.05) {

  contrast_vec <- c(condition_col, group_target, group_ref)

  res <- DESeq2::results(dds, contrast = contrast_vec, alpha = alpha)



  if (shrink) {

    coef_name    <- paste0(condition_col, "_", group_target, "_vs_", group_ref)

    avail_coefs  <- DESeq2::resultsNames(dds)

    res <- tryCatch({

      if (coef_name %in% avail_coefs) {

        DESeq2::lfcShrink(dds, coef = coef_name, res = res, type = "apeglm", quiet = TRUE)

      } else {

        DESeq2::lfcShrink(dds, contrast = contrast_vec, res = res, type = "normal", quiet = TRUE)

      }

    }, error = function(e) res)  # fallback silencieux sur le résultat non-shrunk

  }



  out <- as.data.frame(res)

  out$gene <- rownames(out)

  out <- out[order(out$padj), ]

  rownames(out) <- NULL

  out

}



#' edgeR fallback differential expression (2-group comparison)

run_edger_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref) {

  if (!requireNamespace("edgeR", quietly = TRUE)) stop("Package 'edgeR' requis.")

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  grp  <- factor(metadata[[condition_col]], levels = c(group_ref, group_target))

  keep <- !is.na(grp)

  if (sum(keep) < 4) stop("Trop peu d'échantillons valides pour edgeR (minimum 4 recommandé).")



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]), group = grp[keep])

  y      <- edgeR::calcNormFactors(y)

  design <- stats::model.matrix(~grp[keep])

  y      <- edgeR::estimateDisp(y, design)

  fit    <- edgeR::glmQLFit(y, design)

  qlf    <- edgeR::glmQLFTest(fit, coef = 2)



  res <- edgeR::topTags(qlf, n = Inf)$table

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]  <- "log2FoldChange"

  colnames(res)[colnames(res) == "PValue"] <- "pvalue"

  colnames(res)[colnames(res) == "FDR"]    <- "padj"

  res[order(res$padj), ]

}



#' limma-voom fallback differential expression (2-group comparison)

run_limma_voom_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref) {

  missing_pkgs <- c(

    if (!requireNamespace("limma", quietly = TRUE)) "limma",

    if (!requireNamespace("edgeR", quietly = TRUE)) "edgeR"

  )

  if (length(missing_pkgs) > 0) {

    stop("Package(s) manquant(s) pour limma-voom : ", paste(missing_pkgs, collapse = ", "),

         ". Vérifiez .libPaths() — installé mais peut-être dans une autre librairie R.")

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  grp  <- factor(metadata[[condition_col]], levels = c(group_ref, group_target))

  keep <- !is.na(grp)

  if (sum(keep) < 4) stop("Trop peu d'échantillons valides pour limma-voom (minimum 4 recommandé).")



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]))

  y      <- edgeR::calcNormFactors(y)

  design <- stats::model.matrix(~grp[keep])

  v      <- limma::voom(y, design)

  fit    <- limma::eBayes(limma::lmFit(v, design))



  res <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "P")

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]     <- "log2FoldChange"

  colnames(res)[colnames(res) == "P.Value"]   <- "pvalue"

  colnames(res)[colnames(res) == "adj.P.Val"] <- "padj"

  res[order(res$padj), ]

}



#' Dispatch DE engine call

run_bulk_de_dispatch <- function(engine, counts_matrix, metadata, condition_col,

                                  group_target, group_ref, dds = NULL, shrink = TRUE) {

  switch(engine,

    deseq2 = {

      if (is.null(dds)) stop("DESeqDataSet manquant pour le moteur DESeq2.")

      extract_deseq2_contrast(dds, condition_col, group_target, group_ref, shrink = shrink)

    },

    edger = run_edger_de(counts_matrix, metadata, condition_col, group_target, group_ref),

    limma = run_limma_voom_de(counts_matrix, metadata, condition_col, group_target, group_ref),

    stop("Moteur DE non supporté : ", engine)

  )

}



#' Normalize DE result column names + fill baseMean if missing (edgeR/limma)

.normalize_de_cols <- function(df, counts_for_basemean = NULL) {

  if (is.null(df) || nrow(df) == 0) return(df)

  if (!"gene" %in% colnames(df))           df$gene <- rownames(df)

  if (!"log2FoldChange" %in% colnames(df)) df$log2FoldChange <- NA_real_

  if (!"pvalue" %in% colnames(df))         df$pvalue <- NA_real_

  if (!"padj" %in% colnames(df))           df$padj   <- NA_real_

  if (!"baseMean" %in% colnames(df)) {

    df$baseMean <- if (!is.null(counts_for_basemean)) {

      rowMeans(counts_for_basemean[df$gene, , drop = FALSE], na.rm = TRUE)

    } else NA_real_

  }

  df

}



#' Run the SAME contrast through DESeq2 + edgeR + limma-voom (when available)
#'
#' Multi-method comparison helper: dispatches the same group_target vs
#' group_ref contrast across all available engines and returns normalized
#' results per method, so downstream code can compute rank consensus or
#' compare significant gene sets via Venn/UpSet across METHODS (as opposed
#' to across CONTRASTS — see `build_contrast_gene_sets()` for that). Reuses
#' the exact same `run_bulk_de_dispatch()` + `.normalize_de_cols()` already
#' used by the single-engine path, so results are byte-for-byte identical
#' to running each engine individually — no parallel code path to drift.
#' limma-voom is skipped if `has_limma` is FALSE (flag set in global.R), or
#' silently dropped if its fit errors (e.g. too few replicates).
#'
#' @param counts_matrix Filtered counts matrix (genes x samples).
#' @param metadata Sample metadata data.frame.
#' @param condition_col Name of the grouping column.
#' @param group_target,group_ref Levels of `condition_col` being compared.
#' @param dds_full Pre-built DESeqDataSet (real design) — reused as-is.
#' @param shrink Passed to the DESeq2 engine only (apeglm LFC shrinkage).
#' @return Named list of normalized data.frames: any of `deseq2`/`edger`/
#'   `limma` that succeeded (length >= 1; methods that errored are omitted,
#'   with a `warning()` raised for each — caller decides whether to surface it).
getAllDE <- function(counts_matrix, metadata, condition_col, group_target, group_ref,
                     dds_full, shrink = TRUE) {
  out <- list()

  out$deseq2 <- tryCatch({
    res <- run_bulk_de_dispatch("deseq2", counts_matrix, metadata, condition_col,
                                group_target, group_ref, dds = dds_full, shrink = shrink)
    .normalize_de_cols(res, counts_for_basemean = counts_matrix)
  }, error = function(e) { warning("DESeq2 a échoué : ", conditionMessage(e)); NULL })

  out$edger <- tryCatch({
    res <- run_bulk_de_dispatch("edger", counts_matrix, metadata, condition_col,
                                group_target, group_ref)
    .normalize_de_cols(res, counts_for_basemean = counts_matrix)
  }, error = function(e) { warning("edgeR a échoué : ", conditionMessage(e)); NULL })

  if (isTRUE(has_limma)) {
    out$limma <- tryCatch({
      res <- run_bulk_de_dispatch("limma", counts_matrix, metadata, condition_col,
                                  group_target, group_ref)
      .normalize_de_cols(res, counts_for_basemean = counts_matrix)
    }, error = function(e) { warning("limma-voom a échoué : ", conditionMessage(e)); NULL })
  }

  out[!vapply(out, is.null, logical(1))]
}

#' Rank-aggregation consensus across DE methods (mean-rank, dependency-free)
#'
#' For each gene tested by ALL supplied methods, ranks genes by p-value
#' WITHIN each method (rank 1 = most significant), then averages the
#' per-method ranks — a simple, transparent, well-understood aggregation
#' scheme (equivalent to a Borda count), deliberately NOT using an extra
#' package (e.g. RobustRankAggreg) per the project's RAM/CPU-only /
#' minimal-dependency guidance. Genes a given method didn't report at all
#' (NA padj — can happen with DESeq2's automatic outlier filtering) are
#' excluded from the common gene set across methods.
#'
#' @param de_list Named list of normalized DE data.frames — typically the
#'   output of `getAllDE()`. Must contain >= 2 methods.
#' @param lfc_thresh,padj_thresh Significance thresholds, used only to
#'   compute `n_methods_sig` / `consistent_sign` — does NOT filter rows out.
#' @return data.frame ordered by `mean_rank` ascending (most consistently
#'   significant first), with columns: gene, mean_rank, n_methods_sig,
#'   mean_log2FC, consistent_sign, plus per-method log2FC_<m>/padj_<m>/rank_<m>.
rankConsensus <- function(de_list, lfc_thresh = 1, padj_thresh = 0.05) {
  de_list <- de_list[!vapply(de_list, is.null, logical(1))]
  if (length(de_list) < 2) stop("Au moins 2 méthodes requises pour un consensus de rang.")

  common_genes <- Reduce(intersect, lapply(de_list, function(d) d$gene[!is.na(d$padj)]))
  if (length(common_genes) == 0) {
    stop("Aucun gène commun (avec p-adj valide) entre les méthodes sélectionnées.")
  }

  per_method <- lapply(names(de_list), function(m) {
    d <- de_list[[m]]
    d <- d[match(common_genes, d$gene), c("gene", "log2FoldChange", "padj")]
    d$rank <- rank(d$padj, ties.method = "average", na.last = "keep")
    d$sig  <- !is.na(d$padj) & d$padj < padj_thresh & abs(d$log2FoldChange) > lfc_thresh
    setNames(d, c("gene", paste0("log2FC_", m), paste0("padj_", m), paste0("rank_", m), paste0("sig_", m)))
  })

  merged <- Reduce(function(a, b) merge(a, b, by = "gene"), per_method)

  rank_cols <- grep("^rank_",   colnames(merged))
  sig_cols  <- grep("^sig_",    colnames(merged))
  lfc_cols  <- grep("^log2FC_", colnames(merged))

  merged$mean_rank     <- rowMeans(merged[, rank_cols, drop = FALSE], na.rm = TRUE)
  merged$n_methods_sig <- rowSums(merged[, sig_cols, drop = FALSE], na.rm = TRUE)
  merged$mean_log2FC   <- rowMeans(merged[, lfc_cols, drop = FALSE], na.rm = TRUE)
  merged$consistent_sign <- apply(merged[, lfc_cols, drop = FALSE], 1, function(x) {
    x <- x[!is.na(x)]
    length(x) > 0 && (all(x > 0) || all(x < 0))
  })

  merged[order(merged$mean_rank), ]
}


#' Variance-stabilizing matrix for PCA/heatmap (falls back to log2 for small n)

get_vst_matrix <- function(dds) {

  # Guard 1: too few samples for blind dispersion estimation.

  if (ncol(dds) < 4) {

    return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

  }

  # Guard 2: vst() requires >= nsub genes (default 1000) for the

  # subsampling-based dispersion fit. Small gene panels (e.g. targeted

  # capture panels, custom panels < 1000 genes) must use the direct

  # variance-stabilizing transformation instead (slower, but correct

  # regardless of gene count — no subsampling involved).

  nsub <- min(1000, nrow(dds))

  if (nrow(dds) < 1000) {

    vsd <- tryCatch(

      DESeq2::varianceStabilizingTransformation(dds, blind = TRUE),

      error = function(e) {

        warning("VST direct a échoué (", conditionMessage(e),

                "), repli sur log2(counts normalisés + 1).")

        NULL

      }

    )

    if (is.null(vsd)) return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

    return(SummarizedExperiment::assay(vsd))

  }

  vsd <- DESeq2::vst(dds, blind = TRUE, nsub = nsub)

  SummarizedExperiment::assay(vsd)

}



#' Resolve a categorical ggplot color scale from a palette name
#'
#' Centralises palette choice so it can be reused by any bulk plot that
#' colors by a metadata grouping variable. "manual" takes priority over
#' `palette` when a non-empty `manual_colors` named vector is supplied —
#' this lets `palette = "manual"` mean "use whatever the user picked in the
#' color-picker UI for each level", falling back to the Okabe-Ito sequence
#' if a level is missing a manual entry (defensive, should not happen in
#' normal use since the UI always renders one picker per detected level).
#'
#' @param palette Character: "default" | "okabeito" | "viridis" | "set2" | "manual".
#' @param manual_colors Optional named character vector (level -> hex color),
#'   used only when `palette == "manual"`.
#' @return A ggplot2 discrete color scale layer, or NULL.
bulk_color_scale <- function(palette = "default", manual_colors = NULL) {
  if (identical(palette, "manual") && !is.null(manual_colors) && length(manual_colors) > 0) {
    return(scale_color_manual(values = manual_colors))
  }
  switch(palette %||% "default",
    okabeito = scale_color_manual(values = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )),
    viridis  = scale_color_viridis_d(),
    set2     = scale_color_brewer(palette = "Set2"),
    NULL
  )
}

#' Okabe-Ito sequence, recycled — starting point for the manual color pickers
#' so the user tweaks from a colorblind-safe baseline instead of pure black.
.default_manual_colors <- function(n) {
  base <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#999999")
  rep_len(base, n)
}

#' Native HTML5 color-picker row, zero extra package dependency
#'
#' Renders one `<input type="color">` per id/label/default triple, wired
#' straight to Shiny via an inline `onchange` -> `Shiny.setInputValue()`
#' (no `colourpicker` package needed — keeps the dependency footprint flat,
#' per project guidance on RAM/CPU-only constraints). Reusable by ANY module
#' that wants a "Manuel" palette mode — currently wired into PCA; queued for
#' Heatmap annotation / Volcano / MA in a follow-up.
#'
#' @param ns Namespacing function for the CURRENT module server (`session$ns`).
#' @param ids Character vector of un-namespaced input ids, one per swatch.
#' @param labels Character vector of display labels (e.g. group level names).
#' @param defaults Character vector of starting hex colors, same length.
#' @return A `tagList` — one inline color swatch + label per id.
manual_color_picker_ui <- function(ns, ids, labels, defaults) {
  tagList(
    div(
      style = "display:flex;flex-wrap:wrap;gap:14px;align-items:center;padding:8px 0;",
      lapply(seq_along(ids), function(i) {
        full_id <- ns(ids[i])
        div(
          style = "display:flex;align-items:center;gap:6px;",
          tags$input(
            type     = "color",
            id       = full_id,
            value    = defaults[i],
            style    = "width:34px;height:28px;border:1px solid #ccc;border-radius:4px;padding:0;cursor:pointer;",
            onchange = sprintf("Shiny.setInputValue('%s', this.value)", full_id)
          ),
          tags$span(labels[i], style = "font-size:0.85em;")
        )
      })
    )
  )
}


#' Named color vector for categorical levels — ComplexHeatmap annotation flavor
#'
#' Companion to `bulk_color_scale()` (which returns a ggplot scale object).
#' `HeatmapAnnotation(col = list(...))` wants a plain named character vector
#' instead, hence this separate variant. Returns NULL for "default" so the
#' caller skips `col=` entirely and ComplexHeatmap keeps its own built-in
#' auto-assigned colors — fully backward-compatible unless the user
#' explicitly picks a non-default palette.
#'
#' @param levels Character vector of the metadata levels needing a color
#'   (duplicates allowed — only unique values are used).
#' @param palette "default" | "okabeito" | "viridis" | "set2" | "manual".
#' @param manual_colors Optional named vector (level -> hex), used only
#'   when `palette == "manual"`; missing levels fall back to Okabe-Ito.
#' @return Named character vector (one color per unique level), or NULL.
bulk_annotation_colors <- function(levels, palette = "default", manual_colors = NULL) {
  palette <- palette %||% "default"
  if (identical(palette, "default")) return(NULL)
  levels <- unique(as.character(levels))
  n <- length(levels)
  if (n == 0) return(NULL)

  if (identical(palette, "manual")) {
    defaults <- .default_manual_colors(n)
    vals <- vapply(seq_along(levels), function(i) {
      v <- if (levels[i] %in% names(manual_colors)) manual_colors[[levels[i]]] else NA_character_
      if (is.null(v) || is.na(v) || !nzchar(v)) defaults[i] else v
    }, character(1))
    return(setNames(vals, levels))
  }

  cols <- switch(palette,
    okabeito = .default_manual_colors(n),
    viridis  = viridisLite::viridis(n),
    set2     = RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)],
    .default_manual_colors(n)
  )
  setNames(cols, levels)
}

#' Semantic role colors (Up / Down / NS) for Volcano + MA-Plot
#'
#' Volcano/MA don't color by an arbitrary metadata grouping — they color by
#' a FIXED 2-3 way significance role. This keeps that role-based scheme
#' visually in sync with whichever app-wide palette is active, without
#' forcing the N-level categorical machinery onto a fixed role scheme.
#'
#' @param palette "default" | "okabeito" | "viridis" | "set2" | "manual".
#' @param manual_colors Optional named vector with any of Up/Down/NS keys,
#'   used only when `palette == "manual"` — overrides just the supplied keys,
#'   any role not present keeps its current-palette default.
#' @return Named character vector with keys Up, Down, NS (always all three —
#'   MA-Plot just uses Up (as "significant") + NS and ignores Down).
bulk_role_colors <- function(palette = "default", manual_colors = NULL) {
  presets <- list(
    default  = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7"),
    okabeito = c(Up = "#D55E00", Down = "#0072B2", NS = "#999999"),
    viridis  = { v <- viridisLite::viridis(3); c(Up = v[3], Down = v[1], NS = "#BDC3C7") },
    set2     = { s <- RColorBrewer::brewer.pal(3, "Set2"); c(Up = s[1], Down = s[2], NS = "#BDC3C7") }
  )
  base <- presets[[palette %||% "default"]]
  if (is.null(base)) base <- presets$default
  if (identical(palette, "manual") && !is.null(manual_colors)) {
    for (role in intersect(names(manual_colors), names(base))) {
      v <- manual_colors[[role]]
      if (!is.null(v) && nzchar(v)) base[[role]] <- v
    }
  }
  base
}


#' Safe plot render guard — "figure margins too large" (Bug C)
.safe_plot_render <- function(session, output_id, plot_fn, min_px = 30) {
  w <- session$clientData[[paste0("output_", output_id, "_width")]]
  h <- session$clientData[[paste0("output_", output_id, "_height")]]
  if (isTRUE(w < min_px) || isTRUE(h < min_px)) {
    grid::grid.newpage()
    grid::grid.text("Redimensionnement en cours...", gp = grid::gpar(col = "grey40", fontsize = 11))
    return(invisible(NULL))
  }
  tryCatch(plot_fn(), error = function(e) {
    if (grepl("figure margins too large", conditionMessage(e))) {
      grid::grid.newpage()
      grid::grid.text("Conteneur trop petit.", gp = grid::gpar(col = "grey40", fontsize = 11))
      return(invisible(NULL))
    }
    stop(e)
  })
}


#' Bulk PCA plot colored/shaped by metadata

plot_bulk_pca <- function(vst_matrix, metadata, color_by = NULL, shape_by = NULL, ntop = 500,
                          palette = "default", manual_colors = NULL) {

  rv   <- apply(vst_matrix, 1, var)

  ntop <- min(ntop, nrow(vst_matrix))

  sel  <- order(rv, decreasing = TRUE)[seq_len(ntop)]

  pca  <- prcomp(t(vst_matrix[sel, , drop = FALSE]), scale. = FALSE)

  pct  <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)



  df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], sample = colnames(vst_matrix))



  # Defensive normalisation: treat NULL/NA/""/length!=1 all the same way, and

  # never let an ambiguous condition reach an `if`/`&&` — R >= 4.3 errors hard

  # ("missing value where TRUE/FALSE needed" or "length(0) in coercion to

  # 'logical(1)'") on exactly this class of bug, e.g. `shape_by != color_by`

  # when one side is NULL, or a stray length-0/NA value slips through from

  # an upstream selectInput.

  .clean_scalar <- function(x) {

    if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) NULL else x

  }

  color_by <- .clean_scalar(color_by)

  shape_by <- .clean_scalar(shape_by)



  has_color <- !is.null(color_by) && isTRUE(color_by %in% colnames(metadata))

  has_shape <- !is.null(shape_by) && isTRUE(shape_by %in% colnames(metadata)) &&

               isTRUE(is.null(color_by) || shape_by != color_by)



  if (has_color) df$color <- as.character(metadata[df$sample, color_by])

  if (has_shape) df$shape <- as.character(metadata[df$sample, shape_by])



  p <- ggplot(df, aes(x = PC1, y = PC2, label = sample))

  if (has_color && has_shape) {

    p <- p + geom_point(aes(color = color, shape = shape), size = 3.5, alpha = 0.85)

  } else if (has_color) {

    p <- p + geom_point(aes(color = color), size = 3.5, alpha = 0.85)

  } else {

    p <- p + geom_point(size = 3.5, alpha = 0.85, color = "#2C3E50")

  }

  if (has_color) {
    sc <- bulk_color_scale(palette, manual_colors)
    if (!is.null(sc)) p <- p + sc
  }

  p + geom_text(size = 3, vjust = -1, check_overlap = TRUE) +

    labs(title = "PCA — Échantillons Bulk RNA-seq",

         x = paste0("PC1 (", pct[1], "%)"), y = paste0("PC2 (", pct[2], "%)"),

         color = if (has_color) color_by else NULL,

         shape = if (has_shape) shape_by else NULL) +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' Volcano plot for bulk DE results

plot_volcano_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05, top_label = 10,
                              up_color = "#E74C3C", down_color = "#2980B9", ns_color = "#BDC3C7") {

  res_df <- res_df[!is.na(res_df$padj), ]

  res_df$status <- dplyr::case_when(

    res_df$padj < padj_thresh & res_df$log2FoldChange >  lfc_thresh ~ "Up",

    res_df$padj < padj_thresh & res_df$log2FoldChange < -lfc_thresh ~ "Down",

    TRUE ~ "NS"

  )

  ord      <- order(res_df$padj)

  up_lbl   <- head(res_df$gene[ord][res_df$status[ord] == "Up"],   top_label)

  down_lbl <- head(res_df$gene[ord][res_df$status[ord] == "Down"], top_label)

  res_df$label <- ifelse(res_df$gene %in% c(up_lbl, down_lbl), res_df$gene, NA)



  ggplot(res_df, aes(x = log2FoldChange, y = -log10(padj), color = status)) +

    geom_point(alpha = 0.7, size = 1.6) +

    scale_color_manual(values = c(Up = up_color, Down = down_color, NS = ns_color)) +

    geom_vline(xintercept = c(-lfc_thresh, lfc_thresh), linetype = "dashed", color = "grey40") +

    geom_hline(yintercept = -log10(padj_thresh), linetype = "dashed", color = "grey40") +

    geom_text(aes(label = label), size = 2.8, na.rm = TRUE, vjust = -0.6, show.legend = FALSE) +

    labs(title = "Volcano Plot — Analyse Différentielle",

         x = "Log2 Fold Change", y = "-Log10(P-adj)", color = "Statut") +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' MA-plot for bulk DE results

plot_ma_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05,
                         sig_color = "#E74C3C", ns_color = "#BDC3C7") {

  res_df <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]

  res_df$sig <- res_df$padj < padj_thresh & abs(res_df$log2FoldChange) > lfc_thresh

  ggplot(res_df, aes(x = log10(baseMean + 1), y = log2FoldChange, color = sig)) +

    geom_point(alpha = 0.6, size = 1.4) +

    scale_color_manual(values = c(`TRUE` = sig_color, `FALSE` = ns_color), guide = "none") +

    geom_hline(yintercept = 0, color = "grey30") +

    labs(title = "MA-Plot", x = "Log10(Expression Moyenne + 1)", y = "Log2 Fold Change") +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' Heatmap of selected genes (ComplexHeatmap if available, ggplot fallback otherwise)

plot_heatmap_bulk <- function(vst_matrix, genes, metadata, annotation_col = NULL, scale_rows = TRUE,

                              palette = "default", manual_colors = NULL) {

  genes <- intersect(genes, rownames(vst_matrix))

  if (length(genes) < 2) stop("Au moins 2 gènes requis pour la heatmap.")

  mat <- vst_matrix[genes, , drop = FALSE]

  if (scale_rows) mat <- t(scale(t(mat)))



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    ann <- NULL

    if (!is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      grp_vals   <- metadata[colnames(mat), annotation_col]

      ann_colors <- bulk_annotation_colors(grp_vals, palette, manual_colors)

      ann <- if (!is.null(ann_colors)) {

        ComplexHeatmap::HeatmapAnnotation(group = grp_vals, col = list(group = ann_colors))

      } else {

        ComplexHeatmap::HeatmapAnnotation(group = grp_vals)

      }

    }

    ht <- ComplexHeatmap::Heatmap(mat, name = "Z-score", top_annotation = ann,

                                  show_row_names = nrow(mat) <= 60,

                                  column_title = "Heatmap — Gènes Différentiels")

    # Draw explicitly here (with the same margin safety as
    # plot_upset_contrasts()/plot_venn_contrasts()) rather than returning
    # the raw Heatmap object for implicit auto-print elsewhere — auto-print
    # called draw() with whatever base-graphics mar happened to be active
    # at TWO different call sites (renderPlot's auto-print AND the
    # download handler's explicit print()), neither margin-safe, which is
    # the same class of "figure margins too large" bug as Venn/UpSet.
    old_mar <- graphics::par("mar")

    on.exit(graphics::par(mar = old_mar), add = TRUE)

    graphics::par(mar = c(1, 1, 1, 1))

    return(invisible(ComplexHeatmap::draw(ht)))

  }

  melted <- reshape2::melt(mat, varnames = c("gene", "sample"))

  ggplot(melted, aes(x = sample, y = gene, fill = value)) +

    geom_tile() +

    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, name = "Z-score") +

    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

}



#' Sample-to-sample correlation heatmap (QC diagnostic, computed on VST matrix)

#'

#' High diagnostic value, near-zero extra cost: detects mislabeled samples,

#' batch outliers, or unexpected duplicates BEFORE the user spends time

#' interpreting a differential expression result built on bad input.

#' Reuses the already-computed VST matrix — no extra heavy computation.

#'

#' @param vst_matrix Matrix, VST-transformed expression (genes x samples).

#' @param metadata Sample metadata, for optional column annotation.

#' @param annotation_col Character, metadata column used to annotate samples (optional).

#' @param method Correlation method, "pearson" (default, standard for VST data) or "spearman".

#' @return A ComplexHeatmap object if available, otherwise a ggplot fallback.

plot_sample_correlation_heatmap <- function(vst_matrix, metadata = NULL,

                                             annotation_col = NULL, method = "pearson",

                                             palette = "default", manual_colors = NULL) {

  if (ncol(vst_matrix) < 2) stop("Au moins 2 échantillons requis pour la corrélation.")

  cor_mat <- cor(vst_matrix, method = method, use = "pairwise.complete.obs")



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    ann <- NULL

    if (!is.null(metadata) && !is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      grp_vals   <- metadata[colnames(cor_mat), annotation_col]

      ann_colors <- bulk_annotation_colors(grp_vals, palette, manual_colors)

      ann <- if (!is.null(ann_colors)) {

        ComplexHeatmap::HeatmapAnnotation(group = grp_vals, col = list(group = ann_colors))

      } else {

        ComplexHeatmap::HeatmapAnnotation(group = grp_vals)

      }

    }

    return(ComplexHeatmap::Heatmap(

      cor_mat, name = paste0(toupper(substring(method, 1, 1)), substring(method, 2)),

      top_annotation     = ann,

      col                = circlize::colorRamp2(c(min(cor_mat), 1), c("white", "#2C3E50")),

      show_row_names     = TRUE,

      show_column_names  = TRUE,

      column_title       = "Corrélation Inter-Échantillons (QC)",

      cell_fun = function(j, i, x, y, width, height, fill) {

        grid::grid.text(sprintf("%.2f", cor_mat[i, j]), x, y, gp = grid::gpar(fontsize = 8))

      }

    ))

  }



  melted <- reshape2::melt(cor_mat, varnames = c("sample1", "sample2"))

  ggplot(melted, aes(x = sample1, y = sample2, fill = value)) +

    geom_tile() +

    geom_text(aes(label = sprintf("%.2f", value)), size = 2.8, color = "black") +

    scale_fill_gradient(low = "white", high = "#2C3E50", name = toupper(method)) +

    labs(title = "Corrélation Inter-Échantillons (QC)", x = NULL, y = NULL) +

    theme_minimal() +

    theme(axis.text.x = element_text(angle = 45, hjust = 1),

          plot.title  = element_text(face = "bold", size = 14))

}



#' Standardized DE results DT table (shared by mod_bulk.R Shiny render AND the

#' bulk HTML/PDF report). Centralizing this fixes a real bug: the report's

#' previous inline call to DT::datatable() omitted rownames = FALSE, so the

#' first column displayed auto-numbered row indices (1..n) instead of gene

#' symbols (which live in the "gene" column, since extract_deseq2_contrast()

#' resets rownames to NULL). One function, used everywhere, prevents this

#' divergence from reappearing.

#'

#' @param df DE results data.frame (must have gene, baseMean, log2FoldChange, pvalue, padj).

#' @return A DT::datatable object.

build_de_results_dt <- function(df) {

  df_display <- data.frame(

    Gene     = df$gene,

    BaseMean = round(df$baseMean, 1),

    Log2FC   = round(df$log2FoldChange, 3),

    PValue   = format(df$pvalue, scientific = TRUE, digits = 3),

    Padj     = format(df$padj,   scientific = TRUE, digits = 3),

    stringsAsFactors = FALSE

  )

  DT::datatable(df_display, filter = "top", rownames = FALSE,

                options = list(pageLength = 15, scrollX = TRUE)) %>%

    DT::formatStyle("Padj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                       c("darkgreen", "green", "orange", "red")))

}


# =============================================================================
# Multi-contrast comparison: Venn / UpSet (added post-v1.0, roadmap item #3)
# =============================================================================
# Compares the SIGNIFICANT-gene sets across 2+ contrasts already stored in
# shared_rv$contrasts (single-pair runs and/or pairwise-auto). Deliberately
# takes lfc_thresh/padj_thresh as PLAIN ARGUMENTS rather than reading
# shared_rv directly: the caller (mod_bulk_de.R) wraps this in a reactive()
# keyed on the live input$lfc_thresh/input$padj_thresh, so the sets are
# always recomputed against the CURRENT threshold — not frozen at whatever
# the threshold happened to be when each contrast was originally computed.
# Uses ComplexHeatmap::make_comb_mat()/UpSet() (already a hard dependency —
# NOT the separate "ComplexUpset" package, which adds nothing here and isn't
# in any install list). Venn uses the optional 'VennDiagram' package, only
# for 2-4 sets (unreadable beyond that — UpSet is the right tool there).

#' Build significant-gene sets from a named list of DE result data.frames
#'
#' @param contrasts Named list of data.frames (each needs log2FoldChange + padj,
#'   and either a "gene" column or rownames as gene identifiers).
#' @param lfc_thresh,padj_thresh Significance thresholds, evaluated NOW.
#' @param direction_aware If TRUE, splits each contrast into "<name> (Up)" /
#'   "<name> (Down)" sets instead of one set per contrast.
#' @return Named list of character vectors (gene IDs per set).
build_contrast_gene_sets <- function(contrasts, lfc_thresh = 1, padj_thresh = 0.05,
                                     direction_aware = FALSE) {
  stopifnot(length(contrasts) >= 2)
  sets <- list()
  for (nm in names(contrasts)) {
    df    <- contrasts[[nm]]
    genes <- if ("gene" %in% colnames(df)) df$gene else rownames(df)
    sig   <- !is.na(df$padj) & df$padj < padj_thresh & abs(df$log2FoldChange) > lfc_thresh
    if (direction_aware) {
      sets[[paste0(nm, " (Up)")]]   <- genes[sig & df$log2FoldChange > 0]
      sets[[paste0(nm, " (Down)")]] <- genes[sig & df$log2FoldChange < 0]
    } else {
      sets[[nm]] <- genes[sig]
    }
  }
  sets
}

#' UpSet plot from gene sets — works for any number of sets >= 2.
#' @return A drawn ComplexHeatmap object (call inside renderPlot, no extra draw() needed by caller besides this).
plot_upset_contrasts <- function(gene_sets, min_comb_size = 1) {
  if (length(unlist(gene_sets)) == 0) {
    stop("Aucun gène significatif dans les contrastes sélectionnés avec ces seuils.")
  }
  # Tighter margins lower the minimum device size plot.new()-based layout
  # needs before throwing "figure margins too large" (verified: default
  # ~200px floor vs ~50px with mar=c(1,1,1,1)) — restored on exit regardless
  # of success/failure so it never leaks into other plots on the same device.
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 1, 1))

  m <- ComplexHeatmap::make_comb_mat(gene_sets)
  m <- m[ComplexHeatmap::comb_size(m) >= min_comb_size]
  ComplexHeatmap::draw(ComplexHeatmap::UpSet(
    m,
    comb_order       = order(-ComplexHeatmap::comb_size(m)),
    top_annotation   = ComplexHeatmap::upset_top_annotation(m, add_numbers = TRUE),
    right_annotation = ComplexHeatmap::upset_right_annotation(m, add_numbers = TRUE)
  ))
}

#' Venn diagram from gene sets — readable for 2 to 4 sets only.
#' @return invisible(NULL); draws directly onto the current grid device (call inside renderPlot).
plot_venn_contrasts <- function(gene_sets) {
  n <- length(gene_sets)
  if (n < 2 || n > 4) {
    stop("Le diagramme de Venn n'est lisible que pour 2 à 4 contrastes (vous en avez ",
        n, ") -- utilisez UpSet au-delà.")
  }
  venn_load_error <- tryCatch({
    loadNamespace("VennDiagram")
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(venn_load_error)) {
    stop(sprintf(
      paste0("Impossible de charger le package 'VennDiagram' : %s\n",
            "(VennDiagram dépend de 'futile.logger' — si VennDiagram est bien installe mais ",
            "cette erreur persiste, c'est probablement futile.logger/futile.options/lambda.r ",
            "qui manque, pas VennDiagram lui-même. Vérifiez aussi que .libPaths() dans cette ",
            "session R correspond bien à là où vous avez installé les packages.)"),
      venn_load_error
    ))
  }
  if (length(unlist(gene_sets)) == 0) {
    stop("Aucun gène significatif dans les contrastes sélectionnés avec ces seuils.")
  }
  # Same margin safety as plot_upset_contrasts() — NOTE: VennDiagram has no
  # "margin" parameter on venn.diagram()/draw.*.venn() (checked against the
  # actual installed package: passing one is silently swallowed by ... and
  # does nothing). par(mar=) on the device is the real, verified lever here.
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 1, 1))

  # VennDiagram writes a stray "VennDiagram*.log" file to the working
  # directory on every call unless this logger is silenced first — long
  # documented quirk of the package, not specific to this app.
  if (requireNamespace("futile.logger", quietly = TRUE)) {
    futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  }
  grid::grid.newpage()
  v <- VennDiagram::venn.diagram(
    x = gene_sets, filename = NULL,
    fill = grDevices::rainbow(n, alpha = 0.5),
    main = "", cex = 1, cat.cex = 0.9
  )
  grid::grid.draw(v)
  invisible(NULL)
}

#' Build a downloadable per-gene/per-intersection table from gene sets.
#' One row per gene, listing every set it belongs to and the intersection
#' code — lets the user actually pull "give me the genes in A AND B but not C".
#' @return data.frame(gene, sets, n_sets, comb_code)
build_contrast_intersection_dt <- function(gene_sets) {
  m <- ComplexHeatmap::make_comb_mat(gene_sets)
  combs <- ComplexHeatmap::comb_name(m)
  set_names <- names(gene_sets)

  rows <- lapply(combs, function(code) {
    genes <- ComplexHeatmap::extract_comb(m, code)
    if (length(genes) == 0) return(NULL)
    bits <- as.integer(strsplit(code, "")[[1]])
    sets_here <- paste(set_names[bits == 1], collapse = " & ")
    data.frame(gene = genes, sets = sets_here, n_sets = sum(bits),
              comb_code = code, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) {
    return(data.frame(gene = character(0), sets = character(0),
                      n_sets = integer(0), comb_code = character(0)))
  }
  out[order(-out$n_sets, out$sets), ]
}


# =============================================================================
# Up/Down summary + Scree plot (BingleSeq-inspired additions, Step-2 session)
# =============================================================================

#' Per-contrast Up/Down/Significant summary table (active thresholds, live)
#'
#' Single source of truth for the "n_sig/n_up/n_down per contrast" summary,
#' shared by the HTML/PDF report (mod_bulk_report.R) and the "Resume Up/Down"
#' tab (mod_bulk_de.R) — avoids the two diverging the way the report-only
#' inline version used to, before this was extracted.
#'
#' @param contrasts Named list of DE result data.frames (log2FoldChange, padj).
#' @param lfc_thresh,padj_thresh Significance thresholds, evaluated NOW.
#' @param active_contrast Optional name flagged TRUE in the `actif` column.
#' @return data.frame(Contraste, n_testes, n_sig, n_up, n_down, actif)
summarize_contrasts_updown <- function(contrasts, lfc_thresh = 1, padj_thresh = 0.05,
                                       active_contrast = NULL) {
  contrasts <- contrasts %||% list()
  if (length(contrasts) == 0) {
    return(data.frame(Contraste = character(0), n_testes = integer(0),
                      n_sig = integer(0), n_up = integer(0), n_down = integer(0),
                      actif = logical(0)))
  }
  do.call(rbind, lapply(names(contrasts), function(nm) {
    r   <- contrasts[[nm]]
    sig <- !is.na(r$padj) & r$padj < padj_thresh & abs(r$log2FoldChange) > lfc_thresh
    data.frame(
      Contraste = nm,
      n_testes  = nrow(r),
      n_sig     = sum(sig),
      n_up      = sum(sig & r$log2FoldChange > 0),
      n_down    = sum(sig & r$log2FoldChange < 0),
      actif     = identical(nm, active_contrast),
      stringsAsFactors = FALSE
    )
  }))
}

#' Up/Down summary barchart — one or several contrasts side by side
#' @param summary_df Output of `summarize_contrasts_updown()`.
#' @return ggplot object (grouped bar chart, counts labelled).
plot_updown_barchart <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    stop("Aucun contraste calculé.")
  }
  long_df <- data.frame(
    Contraste = rep(summary_df$Contraste, 2),
    Direction = rep(c("Up", "Down"), each = nrow(summary_df)),
    Count     = c(summary_df$n_up, summary_df$n_down)
  )
  long_df$Contraste <- factor(long_df$Contraste, levels = summary_df$Contraste)
  long_df$Direction <- factor(long_df$Direction, levels = c("Up", "Down"))

  ggplot(long_df, aes(x = Contraste, y = Count, fill = Direction)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = Count), position = position_dodge(width = 0.7),
              vjust = -0.3, size = 3.4) +
    scale_fill_manual(values = c(Up = "#E74C3C", Down = "#2980B9")) +
    labs(title = "Gènes significatifs Up / Down par contraste",
         x = NULL, y = "Nombre de gènes", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14),
          axis.text.x = element_text(angle = if (nrow(summary_df) > 4) 30 else 0, hjust = 1))
}

#' Scree plot (variance expliquée par composante) — PCA companion
#'
#' Reuses the exact same top-variable-gene selection as `plot_bulk_pca()`
#' (ntop = 500, scale. = FALSE) so the % variance shown here matches what
#' PC1/PC2 actually represent on the PCA tab.
#'
#' @param vst_matrix Matrix, VST-transformed expression (genes x samples).
#' @param ntop Number of most-variable genes used (must match plot_bulk_pca()).
#' @param max_pc Maximum number of components to display.
#' @return ggplot object (bars = % variance per PC, line = cumulative %).
plot_scree_bulk <- function(vst_matrix, ntop = 500, max_pc = 10) {
  rv   <- apply(vst_matrix, 1, var)
  ntop <- min(ntop, nrow(vst_matrix))
  sel  <- order(rv, decreasing = TRUE)[seq_len(ntop)]
  pca  <- prcomp(t(vst_matrix[sel, , drop = FALSE]), scale. = FALSE)

  n_pc <- min(max_pc, length(pca$sdev))
  pct  <- 100 * pca$sdev[seq_len(n_pc)]^2 / sum(pca$sdev^2)
  df <- data.frame(PC = factor(paste0("PC", seq_len(n_pc)), levels = paste0("PC", seq_len(n_pc))),
                   pct = pct, cum_pct = cumsum(pct))

  ggplot(df, aes(x = PC)) +
    geom_col(aes(y = pct), fill = "#18BC9C", width = 0.6) +
    geom_line(aes(y = cum_pct, group = 1), color = "#2C3E50", linewidth = 0.8) +
    geom_point(aes(y = cum_pct), color = "#2C3E50", size = 2) +
    geom_text(aes(y = pct, label = paste0(round(pct, 1), "%")), vjust = -0.6, size = 3) +
    labs(title = "Scree Plot — Variance Expliquée", x = NULL,
         y = "% Variance (barres) / % Cumulée (ligne)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13))
}
