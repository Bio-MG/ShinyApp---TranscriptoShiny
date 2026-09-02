# =============================================================================
# test-sc-sccoda-contract.R — Stage 15 : contrat du domaine scCODA (4E-2)
# =============================================================================
# Portes : design Stage 13 OBLIGATOIRE (absent/bloque/perime refuses), colonne
# identite requise, environnement Python explicite (resolveur injectable),
# convergence MCMC (echec => aucun resultat), forme du resultat canonique,
# direction du contraste (reorientation cible vs reference), determinisme
# graine, exports traces par analysis_id.
# Le resultat canonique est calcule UNE SEULE fois en tete de fichier (MCMC
# ~30 s avec 10000/2500 — ESS bas => avertissement, jamais un echec).
# =============================================================================

scc_canonical <- suppressMessages(.sccoda_run())

# ── Resultat canonique ──────────────────────────────────────────────────────
test_that("run_sccoda_da produces the canonical contract on the valid fixture", {
  expect_identical(setdiff(sccoda_contract_fields(), names(scc_canonical)),
                   character(0))
  expect_identical(scc_canonical$type, "sccoda_da")
  expect_true(scc_canonical$status %in% c("valid", "valid_with_warnings"))
  expect_identical(scc_canonical$compositional_unit, "sample")
  expect_identical(scc_canonical$analysis_id, "sc-da-sccoda")
  # Table d'effets : colonnes figees, reference epinglee absente
  expect_identical(
    colnames(scc_canonical$effect_table),
    c("covariate", "identity", "effect", "hdi_low", "hdi_high", "sd",
      "inclusion_probability", "log2_fold_change", "credible",
      "effect_sign_flipped")
  )
  expect_false(scc_canonical$reference_identity %in%
                 scc_canonical$effect_table$identity)
  # Composition : la matrice ANALYSEE (paire de conditions, 4 echantillons)
  expect_identical(nrow(scc_canonical$composition_table), 4L)
  expect_identical(sum(scc_canonical$composition_table$condition %in%
                         c("A", "B")), 4L)
  expect_true(all(c("condition", "batch") %in%
                    colnames(scc_canonical$composition_table)))
  # Spec du modele et diagnostics enregistres
  expect_match(scc_canonical$model_specification$formula, "condition")
  expect_identical(scc_canonical$model_specification$reference_policy,
                   "most_abundant")
  expect_identical(scc_canonical$model_specification$seed,
                   TS_DA_SCCODA_SEED)
  expect_true(is.finite(scc_canonical$convergence_diagnostics$ess_min))
  # r_hat indisponible (chaine unique) => note explicite, jamais silencieuse
  expect_false(is.finite(scc_canonical$convergence_diagnostics$rhat_max))
  expect_true(length(scc_canonical$convergence_diagnostics$notes) > 0L)
  # Versions Python enregistrees
  expect_match(scc_canonical$package_versions$sccoda, "^\\d+\\.\\d+")
  expect_match(scc_canonical$package_versions$python, "^\\d+")
  # Provenance produite a l'analyse (regle 7)
  expect_identical(scc_canonical$provenance$analysis_type, "sccoda_da")
  expect_identical(scc_canonical$provenance$analysis_id, "sc-da-sccoda")
  expect_identical(scc_canonical$provenance$parameters$composition_unit,
                   "sample")
  expect_match(scc_canonical$object_identity$fingerprint, "^v2::")
  expect_match(scc_canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("effects are oriented target vs reference and credible at fdr target", {
  et <- scc_canonical$effect_table
  cond <- et[grepl("^condition\\[", et$covariate), , drop = FALSE]
  b_row <- cond[cond$identity == "B", , drop = FALSE]
  # La composition du fixture enrichit B en condition B : effet crediblement
  # positif (reorientation cible vs reference).
  expect_true(isTRUE(b_row$credible))
  expect_gt(b_row$effect, 0)
  expect_gt(b_row$inclusion_probability, 0.9)
  expect_false(b_row$effect_sign_flipped)
  expect_identical(scc_canonical$credible_effects, "B")
  # Les effets "batch" sont secondaires mais conserves (contexte).
  expect_true(any(grepl("^batch\\[", et$covariate)))
})

test_that("scCODA is reproducible for fixed input and seed", {
  res2 <- suppressMessages(.sccoda_run())
  expect_equal(scc_canonical$effect_table$effect, res2$effect_table$effect,
               tolerance = 1e-6)
  expect_identical(scc_canonical$credible_effects, res2$credible_effects)
})

# ── Portes (design / entrees / environnement / convergence) ─────────────────
test_that("scCODA refuses to run without a canonical Stage 13 design", {
  obj <- .sccoda_seurat_obj()
  expect_error(
    run_sccoda_da(obj, da_design_result = NULL, "B", "A"),
    class = "da_design_error"
  )
  expect_error(
    run_sccoda_da(obj, da_design_result = "pas un design", "B", "A"),
    class = "da_design_error"
  )
})

test_that("scCODA refuses a blocked (pseudoreplicated) design", {
  blk <- .sccoda_seurat_obj()
  des_blk <- .milo_design(.milo_seurat_obj(
    meta = .da_meta(samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50),
                    conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B"))
  ))
  expect_identical(des_blk$status, "invalid_design")
  e <- tryCatch(run_sccoda_da(blk, des_blk, "B", "A"), error = function(e) e)
  expect_identical(class(e), c("da_design_error", "error", "condition"))
  expect_match(conditionMessage(e), "non eligible", fixed = TRUE)
})

test_that("scCODA refuses a stale design", {
  des <- .milo_design(.sccoda_seurat_obj())
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  e <- tryCatch(run_sccoda_da(other, des, "B", "A"), error = function(e) e)
  expect_identical(da_design_error_state(e), "stale_against_current_seurat_object")
})

test_that("scCODA requires an identity column in the design", {
  obj <- .sccoda_seurat_obj()
  des_noid <- finalize_da_design_result(
    validate_da_design(obj@meta.data, "sample_id", "condition",
                       "replicate_id", "batch", identity = NULL),
    seurat_obj = obj
  )
  e <- tryCatch(run_sccoda_da(obj, des_noid, "B", "A"), error = function(e) e)
  expect_identical(class(e), c("sccoda_error", "error", "condition"))
  expect_identical(sccoda_error_state(e), "invalid_input")
  expect_match(conditionMessage(e), "colonne d'identite", fixed = TRUE)
})

test_that("scCODA validates the contrast conditions and the reference identity", {
  obj <- .sccoda_seurat_obj()
  des <- .milo_design(obj)
  e1 <- tryCatch(run_sccoda_da(obj, des, "C", "A"), error = function(e) e)
  expect_identical(sccoda_error_state(e1), "invalid_input")
  e2 <- tryCatch(run_sccoda_da(obj, des, "A", "A"), error = function(e) e)
  expect_identical(sccoda_error_state(e2), "invalid_input")
  e3 <- tryCatch(
    run_sccoda_da(obj, des, "B", "A", reference_identity = "Toto"),
    error = function(e) e
  )
  expect_identical(sccoda_error_state(e3), "invalid_input")
})

test_that("a missing Python environment is an explicit error (never a fallback)", {
  obj <- .sccoda_seurat_obj()
  des <- .milo_design(obj)
  e <- tryCatch(
    run_sccoda_da(obj, des, "B", "A",
                  resolver = function() list(available = FALSE,
                                             error = "aucun python detecte (test)")),
    error = function(e) e
  )
  expect_identical(class(e), c("sccoda_error", "error", "condition"))
  expect_identical(sccoda_error_state(e), "environment_missing")
  expect_match(conditionMessage(e), "aucun python detecte (test)", fixed = TRUE)
})

test_that("a non-converged MCMC produces NO result (convergence gate)", {
  obj <- .sccoda_seurat_obj()
  des <- .milo_design(obj)
  # MCMC ridicule : ESS < TS_DA_SCCODA_ESS_FAIL => refus, aucun resultat.
  e <- tryCatch(
    run_sccoda_da(obj, des, "B", "A", num_results = 50, num_burnin = 10),
    error = function(e) e
  )
  expect_identical(class(e), c("sccoda_error", "error", "condition"))
  expect_identical(sccoda_error_state(e), "convergence_failure")
  expect_match(conditionMessage(e), "ESS", fixed = TRUE)
})

# ── Fonctions pures (sans Python) ───────────────────────────────────────────
test_that("composition matrix counts cells per sample and surfaces missingness", {
  meta <- .da_meta(samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50),
                   na_sample_id = 5L)
  # Cellules sans identite : comptees a part (colonne technique) — lignes
  # DISJOINTES des exclusions sample_id pour des compteurs independants.
  meta$cell_type[20:26] <- NA
  prep <- sccoda_composition_matrix(meta, "sample_id", "condition",
                                    batch = "batch", identity = "cell_type")
  expect_identical(nrow(prep$counts), 4L)
  expect_identical(sum(prep$counts), 195L - 7L)
  expect_identical(prep$n_cells_excluded, 5L)
  expect_identical(prep$n_cells_missing_identity, 7L)
  expect_false("sans_identite" %in% colnames(prep$counts))
  expect_identical(colnames(prep$covariates), c("sample", "condition", "batch"))
  # Colonne requise absente : erreur structurelle classee.
  expect_error(
    sccoda_composition_matrix(meta, "sample_id", "condition",
                              identity = "inexistante"),
    class = "sccoda_error"
  )
})

test_that("reference identity choice is explicit and validated", {
  counts <- data.frame(A = c(30L, 30L), B = c(10L, 10L), NK = c(10L, 10L),
                       row.names = c("s1", "s2"))
  expect_identical(sccoda_reference_identity(counts), "A")
  expect_identical(sccoda_reference_identity(counts, identity_value = "B"), "B")
  expect_error(sccoda_reference_identity(counts, identity_value = "Toto"),
               class = "sccoda_error")
  expect_error(sccoda_reference_identity(counts, policy = "inventee"),
               class = "sccoda_error")
  # Moins de deux identites : modele compositionnel impossible.
  expect_error(sccoda_reference_identity(counts[, "A", drop = FALSE]),
               class = "sccoda_error")
})

test_that("convergence assessment gates on available diagnostics only", {
  # rhat hors seuil => echec ; divergences NUTS hors seuil => echec.
  a <- sccoda_convergence_assessment(list(rhat_max = 1.05, ess_min = 500,
                                          n_divergences = 0))
  expect_false(a$converged)
  b <- sccoda_convergence_assessment(list(rhat_max = NA_real_, ess_min = NA_real_,
                                          n_divergences = 3))
  expect_false(b$converged)
  # ESS sous le plancher => echec ; ESS bas mais au-dessus => avertissement.
  c1 <- sccoda_convergence_assessment(list(rhat_max = NA, ess_min = 5,
                                           n_divergences = NA))
  expect_false(c1$converged)
  c2 <- sccoda_convergence_assessment(list(rhat_max = NA, ess_min = 60,
                                           n_divergences = NA))
  expect_true(c2$converged)
  expect_true(length(c2$warnings_suggested) > 0L)
  # Tout NA => notes explicites (jamais silencieuses), pas d'echec artificiel.
  d <- sccoda_convergence_assessment(list())
  expect_true(d$converged)
  expect_true(length(d$notes) >= 3L)
  expect_error(sccoda_convergence_assessment("pas une liste"), class = "sccoda_error")
})

# ── Consommation (assert) ───────────────────────────────────────────────────
test_that("assert_sccoda_result guards consumers (canonical, staleness)", {
  obj <- .sccoda_seurat_obj()
  expect_invisible(assert_sccoda_result(scc_canonical, seurat_obj = obj))
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  e <- tryCatch(assert_sccoda_result(scc_canonical, seurat_obj = other),
                error = function(e) e)
  expect_identical(class(e), c("sccoda_error", "error", "condition"))
  expect_identical(sccoda_error_state(e), "stale_against_current_seurat_object")
  for (bad in list(NULL, list(), list(type = "sccoda_da"), "resultat")) {
    expect_error(assert_sccoda_result(bad), class = "sccoda_error")
  }
  bad_status <- scc_canonical; bad_status$status <- "compute_failed"
  expect_error(assert_sccoda_result(bad_status), class = "sccoda_error")
})

# ── Exports ─────────────────────────────────────────────────────────────────
test_that("scCODA exports are traced by analysis_id and stable", {
  s <- build_sccoda_summary(scc_canonical)
  expect_identical(nrow(s), 1L)
  expect_true(all(c("analysis_id", "status", "compositional_unit",
                    "target_condition", "reference_identity", "model_formula",
                    "credible_effects", "sccoda_version", "python_version",
                    "object_fingerprint", "warnings") %in% colnames(s)))
  eff <- build_sccoda_effect_table_export(scc_canonical)
  expect_identical(nrow(eff), nrow(scc_canonical$effect_table))
  expect_true(all(eff$analysis_id == "sc-da-sccoda"))
  comp <- build_sccoda_composition_export(scc_canonical)
  expect_identical(nrow(comp), nrow(scc_canonical$composition_table))
  expect_true(all(comp$analysis_id == "sc-da-sccoda"))
  expect_match(sccoda_export_filename(scc_canonical, "sccoda_effects", "csv"),
               "^sccoda_effects_sc-da-sccoda_\\d{4}-\\d{2}-\\d{2}\\.csv$")
  expect_error(build_sccoda_summary(list()), class = "sccoda_error")
})
