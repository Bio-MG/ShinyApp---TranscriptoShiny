# =============================================================================
# test-stats-quickwins.R — STAT-Q1..Q4 (ROADMAP_presentation_stats.md §4)
# =============================================================================
# Quick wins statistiques de la session STAT-Q :
#   STAT-Q1 — méthode de correction pour tests multiples (DE + pathways)
#   STAT-Q2 — exposition de lfcSE (table DE + tooltip volcano)
#   STAT-Q3 — note « outliers distance de Cook »
#   STAT-Q4 — export Excel des pathways
#
# Portée : logique PURE et vérifiable hors application Shiny. Les moteurs
# statistiques (DESeq2/edgeR/limma) sont réels mais sur un jeu jouet minuscule
# (120 gènes × 6 échantillons) — c'est le seul moyen de prouver que la méthode
# choisie atteint bien results()/topTags()/topTable() ET que padj est
# recalculable SANS réajuster le modèle (exigence STAT-Q1).
# =============================================================================

source_project_file("R/core/io_helpers.R")     # %||%
source_project_file("R/core/validation.R")     # guards canoniques (avant bulk_helpers)
source_project_file("R/bulk/bulk_helpers.R")
source_project_file("R/bulk/bulk_report_engine.R")
# STAT-Q2 : .de_volcano_hover() vit dans le module de visualisation (c'est du
# formatage de texte de survol plotly, donc de la présentation — pas de la
# logique de domaine). On source le fichier uniquement pour définir la fonction
# pure : il n'a aucun effet de bord au chargement (que des définitions).
source_project_file("modules/bulk_de/mod_bulk_de_viz.R")

#' Jeu jouet : 120 gènes × 6 échantillons, effet biologique réel sur 30 gènes.
#' Sans effet, tous les padj valent 1 et « BH vs bonferroni » ne se distingue pas.
.toy_bulk <- function(seed = 11L, n_genes = 120L, n_per_group = 3L) {
  set.seed(seed)
  n_samp <- 2L * n_per_group
  counts <- matrix(stats::rpois(n_genes * n_samp, lambda = 80),
                   nrow = n_genes, ncol = n_samp)
  rownames(counts) <- paste0("gene", seq_len(n_genes))
  colnames(counts) <- paste0("s", seq_len(n_samp))
  counts[seq_len(30), seq_len(n_per_group)] <-
    counts[seq_len(30), seq_len(n_per_group)] * 4L
  meta <- data.frame(
    condition = factor(rep(c("WT", "KO"), each = n_per_group), levels = c("WT", "KO")),
    row.names = colnames(counts)
  )
  list(counts = counts, meta = meta)
}

.toy_dds <- function(...) {
  skip_if_not_installed("DESeq2")
  d <- .toy_bulk(...)
  suppressWarnings(
    build_dds(d$counts, d$meta, design_formula = "~ condition", run_deseq = TRUE)
  )
}

# Comparaison de deux résultats DE : l'ORDRE des lignes dépend de padj (donc de
# la méthode) — toute comparaison de vecteurs doit passer par un réalignement
# sur le nom de gène, sinon on compare des lignes différentes.
.by_gene <- function(df) df[order(df$gene), , drop = FALSE]

# =============================================================================
# STAT-Q1 — contrat de configuration
# =============================================================================

test_that("TS_PADJ_METHODS exposes 4 distinct methods, WITHOUT the 'fdr' alias", {
  expect_true(exists("TS_PADJ_METHODS"))
  expect_identical(TS_PADJ_METHODS, c("BH", "BY", "bonferroni", "holm"))
  # "fdr" est un ALIAS de "BH" dans stats::p.adjust.methods — l'exposer en plus
  # de "BH" serait redondant et trompeur pour un biologiste (décision figée).
  expect_false("fdr" %in% TS_PADJ_METHODS)
  expect_true(all(TS_PADJ_METHODS %in% stats::p.adjust.methods))
})

test_that("the default correction stays BH (zero behaviour change, rule 1)", {
  expect_identical(TS_PADJ_METHOD_DEFAULT, "BH")
  expect_identical(TS_PADJ_METHOD_DEFAULT, TS_PADJ_METHODS[1])
})

# =============================================================================
# STAT-Q1 — DESeq2 : padj recalculable depuis le dds DÉJÀ ajusté
# =============================================================================

test_that("extract_deseq2_contrast: default is identical to an explicit 'BH'", {
  dds <- .toy_dds()
  a <- extract_deseq2_contrast(dds, "condition", "KO", "WT")
  b <- extract_deseq2_contrast(dds, "condition", "KO", "WT", p_adjust_method = "BH")
  expect_equal(a$padj, b$padj)
  expect_equal(a$log2FoldChange, b$log2FoldChange)
})

test_that("extract_deseq2_contrast: padj == p.adjust(pvalue, method) — the method reaches results()", {
  dds <- .toy_dds()
  bo <- extract_deseq2_contrast(dds, "condition", "KO", "WT", p_adjust_method = "bonferroni")
  keep <- !is.na(bo$padj)
  expect_gt(sum(keep), 0)   # sinon l'assertion suivante serait vide de sens
  expect_equal(bo$padj[keep], stats::p.adjust(bo$pvalue[keep], method = "bonferroni"))
})

test_that("extract_deseq2_contrast: switching the method changes padj but NOT pvalue/LFC (no refit)", {
  dds <- .toy_dds()
  bh <- .by_gene(extract_deseq2_contrast(dds, "condition", "KO", "WT", p_adjust_method = "BH"))
  bo <- .by_gene(extract_deseq2_contrast(dds, "condition", "KO", "WT", p_adjust_method = "bonferroni"))
  expect_false(isTRUE(all.equal(bh$padj, bo$padj)))
  # pvalue et log2FoldChange sont inchangés => le modèle n'a PAS été réajusté,
  # seule la correction pour tests multiples a été recalculée.
  expect_equal(bh$pvalue, bo$pvalue)
  expect_equal(bh$log2FoldChange, bo$log2FoldChange)
  expect_equal(bh$lfcSE, bo$lfcSE)
})

test_that("extract_deseq2_contrast rejects an unknown correction method", {
  dds <- .toy_dds()
  expect_error(
    extract_deseq2_contrast(dds, "condition", "KO", "WT", p_adjust_method = "pas-une-methode")
  )
})

# =============================================================================
# STAT-Q1 — edgeR / limma : la méthode atteint bien l'ajustement
# =============================================================================

test_that("run_edger_de: a non-BH method still produces a real padj column (FDR/FWER rename)", {
  skip_if_not_installed("edgeR")
  d <- .toy_bulk()
  # edgeR nomme la colonne "FDR" pour BH/BY mais "FWER" pour
  # bonferroni/holm : sans le renommage des DEUX, il n'y aurait aucune colonne
  # padj (et .normalize_de_cols() en ajouterait une entièrement NA).
  bo <- run_edger_de(d$counts, d$meta, "condition", "KO", "WT", p_adjust_method = "bonferroni")
  expect_true("padj" %in% colnames(bo))
  expect_true(any(!is.na(bo$padj)))
  expect_false("FWER" %in% colnames(bo))
  expect_false("FDR"  %in% colnames(bo))

  bh <- run_edger_de(d$counts, d$meta, "condition", "KO", "WT", p_adjust_method = "BH")
  expect_true("padj" %in% colnames(bh))
  expect_false(isTRUE(all.equal(.by_gene(bh)$padj, .by_gene(bo)$padj)))
})

test_that("run_limma_voom_de: a non-BH method yields a usable padj column", {
  skip_if_not_installed("limma")
  skip_if_not_installed("edgeR")
  d <- .toy_bulk()
  bo <- run_limma_voom_de(d$counts, d$meta, "condition", "KO", "WT", p_adjust_method = "bonferroni")
  expect_true("padj" %in% colnames(bo))
  expect_true(any(!is.na(bo$padj)))
  bh <- run_limma_voom_de(d$counts, d$meta, "condition", "KO", "WT", p_adjust_method = "BH")
  expect_false(isTRUE(all.equal(.by_gene(bh)$padj, .by_gene(bo)$padj)))
})

test_that("run_bulk_de_dispatch forwards p_adjust_method to the selected engine", {
  skip_if_not_installed("edgeR")
  d <- .toy_bulk()
  direct <- run_edger_de(d$counts, d$meta, "condition", "KO", "WT", p_adjust_method = "holm")
  via    <- run_bulk_de_dispatch("edger", d$counts, d$meta, "condition", "KO", "WT",
                                 p_adjust_method = "holm")
  expect_equal(direct$padj, via$padj)
})

# =============================================================================
# STAT-Q1 — script R reproductible exporté
# =============================================================================

test_that("bulk_r_script_text embeds the chosen methods and stays parseable R", {
  txt <- bulk_r_script_text(
    n_genes = 100, n_samp = 6, lfc = 1, padj = 0.05,
    contrast_name = "KO_vs_WT", condition_col = "condition",
    group_target = "KO", group_ref = "WT",
    palette_colors = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7"),
    pathway_mode = "ora",
    padj_method = "holm", pathway_padj_method = "BY"
  )
  expect_true(grepl('PADJ_METHOD   <- "holm"', txt, fixed = TRUE))
  expect_true(grepl('PADJ_METHOD_PW <- "BY"', txt, fixed = TRUE))
  expect_true(grepl("pAdjustMethod=PADJ_METHOD)", txt, fixed = TRUE))
  expect_true(grepl("pAdjustMethod=PADJ_METHOD_PW", txt, fixed = TRUE))
  # Plus aucune méthode figée en dur : le script suit l'application.
  expect_false(grepl('pAdjustMethod="BH"', txt, fixed = TRUE))
  # Le script généré est du code que l'UTILISATEUR exécute : il doit parser.
  expect_silent(parse(text = txt))
})

test_that("bulk_r_script_text defaults to BH and rejects an injected/unknown method", {
  args <- list(
    n_genes = 100, n_samp = 6, lfc = 1, padj = 0.05,
    contrast_name = "KO_vs_WT", condition_col = "condition",
    group_target = "KO", group_ref = "WT",
    palette_colors = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
  )
  # Défaut = BH (comportement historique).
  txt <- do.call(bulk_r_script_text, args)
  expect_true(grepl('PADJ_METHOD   <- "BH"', txt, fixed = TRUE))
  expect_true(grepl('PADJ_METHOD_PW <- "BH"', txt, fixed = TRUE))

  # La méthode est INTERPOLÉE dans du code : une valeur non validée serait une
  # injection. match.arg() doit la refuser AVANT toute interpolation.
  expect_error(do.call(bulk_r_script_text,
                       c(args, list(padj_method = 'BH"); system("echo pwned"); #'))))
  expect_error(do.call(bulk_r_script_text,
                       c(args, list(pathway_padj_method = "pas-une-methode"))))
})

# =============================================================================
# STAT-Q2 — lfcSE : présent pour DESeq2, absent (et non-affiché) pour edgeR/limma
# =============================================================================

test_that("extract_deseq2_contrast really carries a finite lfcSE (not a NULL no-op)", {
  dds <- .toy_dds()
  res <- extract_deseq2_contrast(dds, "condition", "KO", "WT")
  # Sans cette assertion, le expect_equal(bh$lfcSE, bo$lfcSE) du test Q1
  # ci-dessus passerait trivialement sur deux NULL.
  expect_true("lfcSE" %in% colnames(res))
  expect_false(is.null(res$lfcSE))
  expect_gt(sum(is.finite(res$lfcSE)), 0)
})

test_that("build_de_results_dt shows lfcSE between Log2FC and PValue when present", {
  skip_if_not_installed("DESeq2")
  skip_if_not_installed("DT")
  dds <- .toy_dds()
  res <- extract_deseq2_contrast(dds, "condition", "KO", "WT")
  dt  <- build_de_results_dt(res)
  expect_identical(colnames(dt$x$data),
                   c("Gene", "BaseMean", "Log2FC", "lfcSE", "PValue", "Padj"))
  # Arrondi à 3 décimales, aligné sur Log2FC (sinon 0.1234567 s'affiche).
  expect_equal(dt$x$data$lfcSE, round(res$lfcSE, 3))
})

test_that("build_de_results_dt drops lfcSE gracefully when the engine never produced it", {
  skip_if_not_installed("DT")
  # edgeR::topTags() et limma::topTable() n'ont PAS de colonne lfcSE : la table
  # doit rester valide, sans colonne entièrement NA (qui laisserait croire à un
  # échec de calcul).
  df <- data.frame(
    gene = c("g1", "g2"), baseMean = c(10, 20),
    log2FoldChange = c(1.5, -2), pvalue = c(1e-4, 0.02), padj = c(5e-4, 0.04),
    stringsAsFactors = FALSE
  )
  dt <- build_de_results_dt(df)
  expect_identical(colnames(dt$x$data),
                   c("Gene", "BaseMean", "Log2FC", "PValue", "Padj"))
})

test_that("a real edgeR result renders without an lfcSE column", {
  skip_if_not_installed("edgeR")
  skip_if_not_installed("DT")
  d <- .toy_bulk()
  # On reproduit le VRAI chemin de l'application : run_edger_de() rend une table
  # edgeR brute (logCPM/F, pas de baseMean), puis .normalize_de_cols() ajoute
  # baseMean depuis la matrice de comptages — c'est l'appel fait par
  # mod_bulk_de_run.R. Aucune des deux étapes ne crée de lfcSE.
  res <- run_edger_de(d$counts, d$meta, "condition", "KO", "WT")
  expect_false("lfcSE" %in% colnames(res))
  res <- .normalize_de_cols(res, counts_for_basemean = d$counts)
  expect_true("baseMean" %in% colnames(res))
  expect_false("lfcSE" %in% colnames(res))
  expect_false("lfcSE" %in% colnames(build_de_results_dt(res)$x$data))
  expect_identical(colnames(build_de_results_dt(res)$x$data),
                   c("Gene", "BaseMean", "Log2FC", "PValue", "Padj"))
})

test_that(".de_volcano_hover appends 'SE:' only when lfcSE is available", {
  base <- data.frame(
    gene = c("g1", "g2"), log2FoldChange = c(1.5, -2),
    padj = c(5e-4, 0.04), status = c("Up", "Down"),
    stringsAsFactors = FALSE
  )

  without <- .de_volcano_hover(base)
  expect_true("hover" %in% colnames(without))
  expect_false(any(grepl("SE:", without$hover, fixed = TRUE)))
  # Le reste du survol est inchangé (pas de régression sur l'existant).
  expect_true(all(grepl("Log2FC: 1.5", without$hover[1], fixed = TRUE)))

  with_se <- .de_volcano_hover(cbind(base, lfcSE = c(0.25, 0.5)))
  expect_true(grepl("SE: 0.25", with_se$hover[1], fixed = TRUE))
  expect_true(grepl("SE: 0.5",  with_se$hover[2], fixed = TRUE))

  # NA (gène filtré) => pas de ligne "SE:" pour cette ligne, sans casser le reste.
  na_se <- .de_volcano_hover(cbind(base, lfcSE = c(NA_real_, 0.5)))
  expect_false(grepl("SE:", na_se$hover[1], fixed = TRUE))
  expect_true(grepl("SE: 0.5", na_se$hover[2], fixed = TRUE))
  expect_true(grepl("Statut: Up", na_se$hover[1], fixed = TRUE))
})
