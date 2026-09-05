# =============================================================================
# test-sc-report-template-structure.R — Post-V1.0 polish (mandat utilisateur)
# =============================================================================
# Le rapport Rmd SC expose désormais les résultats canoniques des domaines
# velocity / communication / DA (sections TABLES pures — esprit compilateur,
# aucune re-exécution). Le rendu réel exige pandoc (indisponible dans
# l'environnement portable de référence, voir KNOWN_LIMITATIONS §4) : ce test
# vérifie donc la STRUCTURE figée du template et son alimentation par
# mod_sc.R. Note: le YAML du template reste ordonné et les clés i18n des
# libellés sont couvertes par test-i18n-integrity.R (aucun doublon).
# =============================================================================

test_that("SC Rmd template declares the new canonical-result params and sections", {
  tpl <- paste(readLines(file.path(ts_project_root(), "reports",
                                   "sc_report_template.Rmd"), warn = FALSE),
               collapse = "\n")
  # Params canoniques ajoutés (bloc YAML).
  for (p in c("velocity_result", "communication_result", "da_design_result",
              "da_milo_result", "da_sccoda_result")) {
    expect_match(tpl, paste0("\n  ", p, ": NULL"), fixed = TRUE,
                 info = paste("param manquant :", p))
  }
  # Sections conditionnées par les clés de choix du panneau 9.
  for (s in c('"velocity" %in% sections', '"communication" %in% sections',
              '"da" %in% sections')) {
    expect_match(tpl, s, fixed = TRUE, info = paste("section manquante :", s))
  }
  # Esprit compilateur : AUCUNE re-exécution d'analyse dans le template.
  expect_false(grepl("run_milo_da\\(|run_sccoda_da\\(|FindAllMarkers\\(|testNhoods\\(|run_bulk_de_dispatch\\(",
                     tpl))
  # Disclaimers scientifiques figés présents.
  expect_match(tpl, "pas une annotation par type cellulaire", fixed = TRUE)
  expect_match(tpl, "pas des p-values", fixed = TRUE)
})

test_that("mod_sc.R feeds the new template params and exposes the new section choices", {
  mod <- paste(readLines(file.path(ts_project_root(), "modules", "sc", "mod_sc.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(mod, 'velocity_result      = state_get(shared_rv, "velocity_result")',
               fixed = TRUE)
  expect_match(mod, 'communication_result = state_get(shared_rv, "communication_result")',
               fixed = TRUE)
  expect_match(mod, 'da_sccoda_result     = state_get(shared_rv, "da_sccoda_result")',
               fixed = TRUE)
  # Choix de sections (UI + update i18n) — synchronisés sur les mêmes clés.
  expect_match(mod, '"velocity", "communication", "da"', fixed = TRUE)
})

test_that("pseudobulk result is additively exposed to the consolidated report", {
  src <- paste(readLines(file.path(ts_project_root(), "modules", "sc",
                                   "mod_sc_pseudobulk.R"), warn = FALSE),
               collapse = "\n")
  expect_match(src, 'shared_rv$pseudobulk_result', fixed = TRUE)
  expect_match(src, 'type = "sc_pseudobulk_de"', fixed = TRUE)
})
