# =============================================================================
# test-sc-milo-views.R — Stage 14 : vues pures du resultat Milo (4E-1)
# =============================================================================
# Les vues sont des CONSOMMATRICES PURES (assert_milo_result) : elles ne
# creent, n'inferent et ne reparient jamais un resultat. Le niveau (voisinage,
# PAS type cellulaire), le contraste et la methode de correction sont visibles
# sur chaque figure.
# =============================================================================

test_that("embedding view plots the descriptive per-cell signal", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  p <- plot_milo_da_embedding(res, obj, "pca")
  expect_s3_class(p, "ggplot")
  # Les cellules couvertes portent un score ; les autres restent NA (gris).
  expect_true(all(c("mean_logFC", "in_nhood") %in% colnames(p$data)))
  expect_identical(nrow(p$data), nrow(res$nhood_assignment))
  expect_identical(sum(p$data$in_nhood),
                   sum(res$nhood_assignment$n_neighbourhoods > 0L))
  # Le contraste et le niveau sont affiches (sous-titre).
  expect_match(p$labels$subtitle, "conditionB", fixed = TRUE)
  expect_match(p$labels$subtitle, "PAR VOISINAGE", fixed = TRUE)
  expect_match(p$labels$title, "Milo", fixed = TRUE)
})

test_that("distribution view separates significant neighbourhoods and counts NA", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  p <- plot_milo_da_distribution(res)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "SpatialFDR", fixed = TRUE)
  expect_match(p$labels$x, "logFC", fixed = TRUE)
  # Resultat sans table DA : message explicite (graphe annoté, pas vide).
  empty <- res; empty$DA_table <- res$DA_table[0, , drop = FALSE]
  p_empty <- plot_milo_da_distribution(empty)
  expect_s3_class(p_empty, "ggplot")
  expect_true(length(p_empty$layers) > 0L)
})

test_that("sample composition view stays at the sample level", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  p <- plot_milo_sample_composition(res)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "ECHANTILLON", fixed = TRUE)
  expect_match(p$labels$caption, "replicats", fixed = TRUE)
  # Une ligne par echantillon (niveau ECHANTILLON — jamais par cellule).
  expect_setequal(unique(as.character(p$data$sample)),
                  as.character(res$sample_composition$sample))
})

test_that("views refuse non-canonical or stale inputs (pure consumers)", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  for (bad in list(NULL, list(), "resultat")) {
    expect_error(plot_milo_da_embedding(bad, obj, "pca"), class = "milo_error")
    expect_error(plot_milo_da_distribution(bad), class = "milo_error")
    expect_error(plot_milo_sample_composition(bad), class = "milo_error")
  }
  expect_error(plot_milo_da_embedding(res, other, "pca"), class = "milo_error")
  # Reduction absente : le garde transversal assert_reduction parle francais
  # (erreur simple, meme contrat que les autres domaines).
  expect_error(plot_milo_da_embedding(res, obj, "inexistante"), "introuvable")
})
