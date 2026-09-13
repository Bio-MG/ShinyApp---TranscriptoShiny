# =============================================================================
# test-sc-population-rarity.R — Rarete par population annotee (CCC 9, Q1)
# =============================================================================
# Tests fonctionnels du domaine R/sc/sc_population_rarity.R : decompte
# descriptif par population, regles declarees (absolue / relative), etats,
# cas limites et erreurs classees. AUCUN test statistique, AUCUN graphe : le
# jalon est descriptif et mono-condition (cf. POPULATION_RARITY_CONTRACT.md).
# =============================================================================

# ── Cas nominal ─────────────────────────────────────────────────────────────
test_that("le resultat canonique expose exactement les champs du contrat", {
  canon <- .pr_run()
  expect_identical(names(canon), population_rarity_contract_fields())
  expect_identical(canon$type, "sc_population_rarity")
  expect_identical(canon$analysis_id, "sc-population-rarity")
  expect_match(canon$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("le decompte nominal est correct (tailles connues, seuil declare 10)", {
  canon <- .pr_run()
  expect_identical(canon$status, "valid")
  expect_identical(canon$summary$n_populations, 4L)
  expect_identical(canon$summary$n_rare, 2L)
  expect_identical(canon$qc$n_cells_total, 91L)
  expect_identical(canon$qc$n_cells_counted, 91L)
  expect_identical(canon$summary$declared_rule_label, "n_cells < 10")
  expect_identical(canon$rarity_rule$direction, "below")
  expect_identical(canon$rarity_rule$threshold, 10L)

  tab <- canon$population_table
  expect_identical(colnames(tab), c("population", "n_cells", "fraction", "is_rare"))
  expect_identical(tab$population, c("A", "B", "C", "D"))
  expect_identical(tab$n_cells, c(60L, 20L, 8L, 3L))
  expect_equal(sum(tab$fraction), 1)
  expect_identical(tab$is_rare, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("la regle relative qualifie la rarete par fraction", {
  canon <- .pr_run(rule_type = "relative_fraction", threshold = 0.1)
  expect_identical(canon$status, "valid")
  expect_identical(canon$summary$declared_rule_label, "fraction < 0.1")
  # 8/91 = 0.088 et 3/91 = 0.033 passent sous 0.1 ; 20/91 = 0.22 non.
  expect_identical(canon$population_table$is_rare, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(canon$summary$n_rare, 2L)
})

test_that("une seule population ne produit AUCUNE rarete (etat explicite, pas une erreur)", {
  canon <- .pr_run(meta = .pr_meta(counts = c(A = 60L)))
  expect_identical(canon$status, "unavailable_single_population")
  expect_identical(nrow(canon$population_table), 1L)
  # NA et non FALSE : « non rare » serait une affirmation, pas une abstention.
  expect_true(all(is.na(canon$population_table$is_rare)))
  expect_true(is.na(canon$summary$n_rare))
})

# ── Qualite de donnees : exclusion + comptabilisation (aucune imputation) ───
test_that("les libelles NA sont exclus du decompte ET comptabilises", {
  canon <- .pr_run(meta = .pr_meta(na_labels = 5L))
  expect_identical(canon$status, "valid_with_warnings")
  expect_identical(canon$qc$n_labels_na, 5L)
  expect_identical(canon$qc$n_cells_total, 96L)
  expect_identical(canon$qc$n_cells_counted, 91L)
  expect_identical(canon$identity_summary$n_labels_na, 5L)
  expect_true(any(grepl("sans etiquette", canon$warnings)))
  # La fraction se rapporte au jeu COMPTE, jamais au total.
  expect_equal(sum(canon$population_table$fraction), 1)
})

test_that("les niveaux de facteur sans cellule sont exclus et comptabilises", {
  canon <- .pr_run(meta = .pr_meta(empty_levels = c("E", "F")))
  expect_identical(canon$status, "valid_with_warnings")
  expect_identical(canon$qc$n_levels_empty, 2L)
  expect_identical(canon$qc$empty_levels, c("E", "F"))
  expect_false(any(c("E", "F") %in% canon$population_table$population))
})

test_that("une colonne d'echantillon ajoute n_samples_present (descriptif)", {
  canon <- .pr_run(sample_column = "sample_id")
  tab <- canon$population_table
  expect_true("n_samples_present" %in% colnames(tab))
  expect_identical(tab$n_samples_present, c(4L, 4L, 4L, 3L))
  # La colonne n'existe PAS si aucun echantillon n'est declare.
  expect_false("n_samples_present" %in% colnames(.pr_run()$population_table))
})

# ── Erreurs classees (aucun repli silencieux) ───────────────────────────────
test_that("chaque erreur structurelle est classee population_rarity_error", {
  expect_pr_error <- function(expr) {
    err <- tryCatch(expr, error = function(e) e)
    expect_s3_class(err, "population_rarity_error")
    expect_identical(population_rarity_error_state(err), "invalid_input")
    expect_false(is.null(conditionMessage(err)))
    err
  }

  expect_pr_error(.pr_run(meta = "pas un data.frame"))
  expect_pr_error(.pr_run(meta = NULL))
  expect_pr_error(.pr_run(identity_column = "colonne_absente"))
  expect_pr_error(.pr_run(rule_type = "regle_inconnue"))
  expect_pr_error(.pr_run(sample_column = "echantillon_absent"))
  # Total 40 < plancher declare TS_POPULATION_RARITY_MIN_CELLS_TOTAL (50).
  expect_pr_error(.pr_run(meta = .pr_meta(counts = c(A = 20L, B = 20L))))
})

test_that("l'absence de regle ou de seuil est REFUSEE (aucun defaut implicite)", {
  expect_error(.pr_run(rule_type = NULL), class = "population_rarity_error")
  expect_error(.pr_run(rule_type = NA_character_), class = "population_rarity_error")
  expect_error(.pr_run(threshold = NULL), class = "population_rarity_error")
  expect_error(.pr_run(threshold = NA), class = "population_rarity_error")
  # Seuil absolu : entier >= 1.
  expect_error(.pr_run(threshold = 0L), class = "population_rarity_error")
  expect_error(.pr_run(threshold = 2.5), class = "population_rarity_error")
  expect_error(.pr_run(threshold = "abc"), class = "population_rarity_error")
  # Seuil relatif : fraction strictement dans ]0, 1[.
  expect_error(.pr_run(rule_type = "relative_fraction", threshold = 0),
               class = "population_rarity_error")
  expect_error(.pr_run(rule_type = "relative_fraction", threshold = 1),
               class = "population_rarity_error")
  expect_error(.pr_run(rule_type = "relative_fraction", threshold = 1.5),
               class = "population_rarity_error")
})

test_that("un jeu vide de toute population exploitable est refuse", {
  err <- tryCatch(.pr_run(meta = .pr_meta(counts = c(A = 0L), na_labels = 60L)),
                  error = function(e) e)
  expect_s3_class(err, "population_rarity_error")
  expect_identical(population_rarity_error_state(err), "invalid_input")
})

# ── assert / staleness / exports ────────────────────────────────────────────
test_that("assert_population_rarity_result accepte le canonique, refuse le reste", {
  canon <- .pr_run()
  expect_invisible(assert_population_rarity_result(canon))
  expect_identical(assert_population_rarity_result(canon), canon)
  # unavailable_single_population est un resultat VALIDE (sans rarete).
  single <- .pr_run(meta = .pr_meta(counts = c(A = 60L)))
  expect_invisible(assert_population_rarity_result(single))

  expect_error(assert_population_rarity_result(NULL),
               class = "population_rarity_error")
  expect_error(assert_population_rarity_result(list(type = "autre")),
               class = "population_rarity_error")
  bad_status <- canon
  bad_status$status <- "statut_inconnu"
  expect_error(assert_population_rarity_result(bad_status),
               class = "population_rarity_error")
})

test_that("la peremption suit l'empreinte v2 de l'objet", {
  meta <- .pr_meta()
  obj <- .pr_stub_obj(meta)
  canon <- .pr_run(meta = meta, seurat_obj = obj)
  expect_match(canon$object_identity$fingerprint, "^v2::")
  expect_identical(canon$object_identity$method, "velocity_object_fingerprint")
  expect_false(population_rarity_is_stale(canon, obj))
  expect_true(population_rarity_is_stale(canon, .pr_stub_obj(.pr_meta(na_labels = 1L))))
  # Sans objet : indeterminable, jamais une affirmation.
  expect_true(is.na(population_rarity_is_stale(canon, NULL)))
  expect_true(is.na(population_rarity_is_stale(.pr_run(), obj)))
})

test_that("le resume et l'export suivent les conventions du depot", {
  canon <- .pr_run()
  smry <- build_population_rarity_summary(canon)
  expect_identical(nrow(smry), 1L)
  expect_identical(smry$analysis_id, "sc-population-rarity")
  expect_identical(smry$analysis_type, "sc_population_rarity")
  expect_identical(smry$rule_type, "absolute_n_cells")
  expect_identical(smry$threshold, "10")
  expect_identical(smry$n_rare, "2")
  expect_identical(smry$descriptive_only, "TRUE")

  tab <- build_population_rarity_table_export(canon)
  expect_identical(tab, canon$population_table)

  fn <- population_rarity_export_filename(canon)
  expect_identical(fn, sprintf("population_rarity_sc-population-rarity_%s.csv",
                               format(Sys.Date(), "%Y-%m-%d")))
})

# ── Garde-fou transversal : la sortie est DESCRIPTIVE ───────────────────────
test_that("la provenance marque explicitement la sortie comme descriptive", {
  canon <- .pr_run()
  expect_identical(canon$provenance$descriptive_only, TRUE)
  expect_identical(canon$provenance$analysis_type, "population_rarity")
  expect_identical(canon$provenance$parameters$descriptive_only, TRUE)
  expect_identical(canon$provenance$parameters$rule_type, "absolute_n_cells")
  expect_identical(canon$provenance$parameters$threshold, 10L)
  expect_identical(canon$provenance$n_rare, 2L)
})

test_that("aucun champ d'affirmation differentielle n'existe dans le resultat", {
  canon <- .pr_run()
  # Ni p-value, ni FDR, ni logFC : le resultat ne porte AUCUNE affirmation
  # differentielle (contrat §1.1 — la porte Stage 13 est inapplicable).
  expect_false(any(grepl("p_value|p_adjusted|fdr|logFC|pval",
                         names(canon), ignore.case = TRUE)))
  expect_false(any(grepl("p_value|p_adjusted|fdr|logFC",
                         colnames(canon$population_table), ignore.case = TRUE)))
})
