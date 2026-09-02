# =============================================================================
# test-sc-sccoda-views.R — Stage 15 : vues pures du resultat scCODA (4E-2)
# =============================================================================
# Les vues sont des CONSOMMATRICES PURES (assert_sccoda_result) : elles ne
# creent, n'inferent et ne reparient jamais un resultat. L'unite (ECHANTILLON),
# la reference (identite + condition) et la nature bayesienne des effets
# (intervalles de credibilite, pas des p-values) sont affichees.
# =============================================================================

scc_views_canonical <- suppressMessages(.sccoda_run())

test_that("composition view stays at the sample level and shows the reference", {
  p <- plot_sccoda_composition(scc_views_canonical)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "ECHANTILLON", fixed = TRUE)
  expect_match(p$labels$subtitle, scc_views_canonical$reference_identity,
               fixed = TRUE)
  # Une ligne par (echantillon x identite) — jamais par cellule.
  expect_setequal(unique(as.character(p$data$sample)),
                  as.character(scc_views_canonical$composition_table$sample))
})

test_that("effects view displays credible effects with the scientific framing", {
  p <- plot_sccoda_effects(scc_views_canonical)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "CREDIBILITE", fixed = TRUE)
  expect_match(p$labels$subtitle, scc_views_canonical$reference_identity,
               fixed = TRUE)
  expect_match(p$labels$caption, "PAS un test", fixed = TRUE)
  # Par defaut, seuls les effets de condition sont affiches.
  expect_false(any(grepl("^batch\\[", p$data$covariate)))
  # Variante avec batch en contexte secondaire.
  p_batch <- plot_sccoda_effects(scc_views_canonical, include_batch = TRUE)
  expect_true(any(grepl("^batch\\[", p_batch$data$covariate)))
  # L'effet credible du fixture (B) est present et colore comme credible.
  expect_true("B" %in% p$data$identity)
})

test_that("uncertainty view shows HDI widths", {
  p <- plot_sccoda_uncertainty(scc_views_canonical)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$x, "Largeur", fixed = TRUE)
  expect_true(all(p$data$hdi_width > 0))
})

test_that("views refuse non-canonical or stale inputs (pure consumers)", {
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  for (bad in list(NULL, list(), "resultat")) {
    expect_error(plot_sccoda_composition(bad), class = "sccoda_error")
    expect_error(plot_sccoda_effects(bad), class = "sccoda_error")
    expect_error(plot_sccoda_uncertainty(bad), class = "sccoda_error")
  }
  expect_invisible(assert_sccoda_result(scc_views_canonical))
  expect_error(
    assert_sccoda_result(scc_views_canonical, seurat_obj = other),
    class = "sccoda_error"
  )
})
