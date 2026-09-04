# =============================================================================
# test-i18n-integrity.R — Gate packaging (Stage 19)
# =============================================================================
# shiny.i18n indexe les traductions par clé FR : un DOUBLON fait crasher le
# démarrage (row.names non uniques — incident réel corrigé en Stage 17,
# clé « Verdict »). Ce test est la non-régression promise au Stage 18.
# =============================================================================

test_that("translation.json is valid, complete, duplicate-free and covers SC panels", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  expect_true(file.exists(path))
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(is.list(j$translation) && length(j$translation) > 0)
  expect_setequal(unlist(j$languages), c("fr", "en"))
  expect_true(nzchar(j$cultural_date_format %||% ""))

  fr <- vapply(j$translation, function(x) x$fr %||% "", character(1))
  en <- vapply(j$translation, function(x) x$en %||% "", character(1))
  expect_true(all(nzchar(fr)), info = "clé FR vide dans translation.json")
  expect_true(all(nzchar(en)), info = "traduction EN manquante dans translation.json")
  expect_length(unique(fr), length(fr))  # le crash startup : doublon de clé FR

  # Libellés des panneaux du workflow SC (8b → 9b) : présents et uniques.
  panel_keys <- c(
    "8b. Communication (import)",
    "8c. Abondance différentielle (design)",
    "8d. Abondance différentielle — Milo (voisinages)",
    "8e. Abondance différentielle — scCODA (composition)",
    "8f. Abondance différentielle — vues croisées (Milo × scCODA)",
    "9. Rapport Complet",
    "9b. Rapport consolidé (4F)",
    "Rapport consolidé"
  )
  for (k in panel_keys) {
    expect_true(k %in% fr, info = paste("clé de panneau manquante :", k))
  }
})
