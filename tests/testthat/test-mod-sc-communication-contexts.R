# =============================================================================
# test-mod-sc-communication-contexts.R — les 3 modules « contextes » CCC
# (mod_sc_communication_velocity.R / _trajectory.R / _perturbation.R) + la
# normalisation des filtres score/rang du parent.
# =============================================================================
# Item 3F du packet round-7 (O28 batch B2, 2026-10-31 — spéc §2.18).
#
# Constat 3F : gardes RÉACTIVES existantes (bandeau vel_info :58-61, stop :84)
# mais AUCUNE garde PRÉVENTIVE (disable/bandeau avant clic) et 0 withProgress
# dans les trois modules (git grep mesuré à HEAD 7550e39, packet row 3F).
#
# ⚠️ Limite documentée (même motif que test-mod-sc-communication-ui.R) : les
# gardes vivent DANS les moduleServer — le rendu headless complet des
# sous-modules n'est pas réalisable à coût raisonnable (comm_state, objet
# Seurat, collection LIANA). La preuve est donc un verrou source structurel :
# chaque garde est verrouillée par sa signature de code, et la sémantique
# (états utilisables du contrat velocity) est citée depuis la source de
# vérité (sc_velocity.R:141-151).
# =============================================================================

.VEL_FILE  <- "modules/sc/mod_sc_communication_velocity.R"
.TRJ_FILE  <- "modules/sc/mod_sc_communication_trajectory.R"
.PERT_FILE <- "modules/sc/mod_sc_communication_perturbation.R"
.PAR_FILE  <- "modules/sc/mod_sc_communication.R"

.read <- function(rel) {
  paste(readLines(file.path(ts_project_root(), rel),
                  warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
.n_hits <- function(src, pattern) {
  lengths(regmatches(src, gregexpr(pattern, src, fixed = TRUE)))[1]
}

# ── Vélocité : garde préventive (spéc §2.18 point 1) ─────────────────────────
test_that("3F vélocité : bouton désactivé préventivement + bandeau — états du contrat consommés", {
  src <- .read(.VEL_FILE)
  # Le bouton est piloté par updateActionButton(disabled=) sur le résultat
  # velocity (le stop post-clic :84 reste en seconde ligne) :
  expect_match(src, 'updateActionButton(session, "vel_compute", disabled',
               fixed = TRUE)
  # États utilisables = ceux du CONTRAT (source de vérité sc_velocity.R) —
  # valid_no_vectors et les invalid_* restent bloqués :
  expect_match(src, 'st %in% c("valid", "valid_partial_embedding")', fixed = TRUE)
  # Bandeau préventif dédié (l'infobulle vel_info seule ne désactive rien) :
  expect_match(src, 'output$vel_guard <- renderUI', fixed = TRUE)
  expect_match(src, 'uiOutput(ns("vel_guard"))', fixed = TRUE)
  # Texte de la spéc (valid_no_vectors / absent) :
  expect_match(src,
    "Le calcul de contexte n\u00e9cessite des vecteurs de v\u00e9locit\u00e9 pr\u00e9calcul\u00e9s complets (absents dans l'import actuel).",
    fixed = TRUE)
})

test_that("3F vélocité : jauge withProgress autour du calcul de contexte", {
  src <- .read(.VEL_FILE)
  expect_match(src, 'withProgress(message = .tr("Calcul du contexte v\u00e9locit\u00e9...")',
               fixed = TRUE)
  # Le calcul est BIEN encapsulé (build dans le bloc withProgress) :
  expect_match(src, "build_communication_velocity_context(", fixed = TRUE)
})

# ── Trajectoire : garde préventive (spéc §2.18 point 2) ──────────────────────
test_that("3F trajectoire : bascule auto vers global sans slingshot + bouton désactivé sans pseudo-temps", {
  src <- .read(.TRJ_FILE)
  # Détecteur slingshot + bascule automatique (option documentée de la spéc) :
  expect_match(src, 'grepl("^pseudotime_slingshot_", meta_cols)', fixed = TRUE)
  expect_match(src, 'updateRadioButtons(session, "trj_lineage_mode", selected = "global")',
               fixed = TRUE)
  # Bouton désactivé tant qu'aucune colonne de pseudo-temps n'existe :
  expect_match(src, 'updateActionButton(session, "trj_compute", disabled',
               fixed = TRUE)
  # Bandeau préventif dédié :
  expect_match(src, 'output$trj_guard <- renderUI', fixed = TRUE)
  expect_match(src, 'uiOutput(ns("trj_guard"))', fixed = TRUE)
  expect_match(src,
    "Aucune colonne de pseudo-temps dans l'objet courant : lancez d'abord Trajectoire (8.4)",
    fixed = TRUE)
})

test_that("3F trajectoire : jauge withProgress autour de l'extraction + du calcul", {
  src <- .read(.TRJ_FILE)
  expect_match(src, 'withProgress(message = .tr("Calcul du contexte trajectoire...")',
               fixed = TRUE)
  expect_match(src, "communication_fetch_expression_matrix(", fixed = TRUE)
  expect_match(src, "build_communication_trajectory_context(", fixed = TRUE)
})

# ── Perturbation : jauge (spéc §2.18 point 3) + garde préventive ─────────────
test_that("3F perturbation : simulation encapsulée dans withProgress + bouton gardé sans résultat", {
  src <- .read(.PERT_FILE)
  # Texte exact de la spéc :
  expect_match(src, 'withProgress(message = .tr("Simulation de perturbation in silico...")',
               fixed = TRUE)
  expect_match(src, "build_communication_perturbation(", fixed = TRUE)
  # Garde préventive : sans résultat importé, la simulation n'a AUCUNE table :
  expect_match(src, 'updateActionButton(session, "pert_compute"', fixed = TRUE)
  expect_match(src, "disabled = is.null(comm_state$result)", fixed = TRUE)
})

# ── Parent : normalisation contextuelle des filtres score / rang (point 4) ───
test_that("3F filtres : intitulés et bornes suivent la source (un rang n'est pas un score)", {
  src <- .read(.PAR_FILE)
  # L'adaptation réagit au résultat déposé :
  expect_match(src, "observeEvent(comm_state$result, {\n      r <- comm_state$result",
               fixed = TRUE)
  # CellChat — probabilité [0.0 - 1.0] (spéc) :
  expect_match(src, "Probabilit\u00e9 de communication [0.0 - 1.0]", fixed = TRUE)
  # CellPhoneDB — moyenne d'expression, borne haute = max de la TABLE :
  expect_match(src, "Moyenne d'expression [0.0 - Max]", fixed = TRUE)
  # LIANA — rang agrégé (1 = plus significatif), score sans objet :
  expect_match(src, "Rang agr\u00e9g\u00e9 [1 - N] (1 = plus significatif)", fixed = TRUE)
  expect_match(src,
    "Score minimum \u2014 sans objet pour LIANA (un rang n'est pas un score : utiliser Rang maximum)",
    fixed = TRUE)
  # Aucune borne fabriquée : les max viennent de la table canonique :
  expect_match(src, 'max(t$score, na.rm = TRUE)', fixed = TRUE)
  expect_match(src, 'max(t$rank, na.rm = TRUE)', fixed = TRUE)
})
