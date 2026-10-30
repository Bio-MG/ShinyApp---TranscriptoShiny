# =============================================================================
# test-sc-identity-mapping.R — tools/pkg_skeleton/R/sc_communication.R (3E)
# =============================================================================
# Item 3E du packet round-7 (O28 batch B5) : l'harmonisation des identites CCC
# reposait sur l'exact match seul (harmonize_communication_identities,
# sc_communication.R:1104 a HEAD 7ed6100) — toute divergence mineure de
# casse/syntaxe ("CD4+ T cells" vs "CD4_T_Cells") laissait le label sans
# correspondance (*_mapped a NA, warning de liste). Correctif (spec §2.17 de
# docs/proposals/V1X_SC_QA_AUDIT_FIXES_PROPOSAL.md) :
#   1. propositions de rapprochement (normalisation documentee + Levenshtein
#      sur formes normalisees) avec PREUVE par ligne (rule/distance/detail) ;
#   2. application exclusivement via un mapping explicitement VALIDE par
#      l'utilisateur (apply_communication_identity_mapping / parametre
#      approved_mapping de harmonize) — JAMAIS d'application automatique ;
#   3. voie historique intacte : harmonize sans approved_mapping = exact
#      match, memes messages, meme garde zero-correspondance.
#
# Pur moteur (aucun Shiny) : les regles, cas limites, rejets et le contrat de
# retour sont epingles ici ; la surface de validation du module est testee
# dans son propre fichier (pattern B1/B3 : requetes de mise a jour capturees).
# =============================================================================

Sys.setenv(OMNIPATHR_LOGFILE = "none")

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/sc_communication.R")

# ── Fixtures ────────────────────────────────────────────────────────────────

.harm_tab <- function(sender, receiver) {
  data.frame(
    sender = sender, receiver = receiver,
    ligand = "L1", receptor = "R1", interaction = "L1-R1",
    pathway = NA_character_, score = 0.5,
    p_value = NA_real_, p_adjusted = NA_real_,
    source_method = "cellphonedb", source_file = "means.txt",
    stringsAsFactors = FALSE
  )
}

.capture_warnings <- function(code) {
  w <- character(0)
  res <- withCallingHandlers(
    code,
    warning = function(wm) {
      w <<- c(w, conditionMessage(wm))
      invokeRestart("muffleWarning")
    }
  )
  list(res = res, warnings = w)
}

# ── 1. Regles de normalisation documentees ──────────────────────────────────

test_that("documented normalisation rules are exposed, named, ordered", {
  r <- communication_identity_normalisation_rules()
  expect_identical(names(r), c("trim", "casefold", "strip_punct", "collapse_ws"))
  expect_true(all(nzchar(r)))
})

test_that("normalisation: spec example converges, accents kept, NA passthrough", {
  n <- .communication_identity_normalise
  # Exemple de la spec §2.17 : les deux écritures convergent.
  expect_identical(n("CD4+ T cells"), "cd4 t cells")
  expect_identical(n("CD4_T_Cells"), "cd4 t cells")
  # trim + collapse des espaces internes.
  expect_identical(n("  T   Cells  "), "t cells")
  # Les alphanumeriques (dont accents) subsistent ; la ponctuation ASCII part.
  # (Le repli de casse est garanti ASCII ; les accents restent inchangés.)
  expect_identical(n("Cellules NK"), "cellules nk")
  expect_identical(n("mémoire T"), "mémoire t")
  expect_identical(n("Mémoire T"), "mémoire t")
  expect_identical(n(c("A+", "b_")), c("a", "b"))
  expect_true(is.na(n(NA_character_)))
})

test_that("levenshtein helper: identity, insertion, substitution, empty", {
  lev <- .communication_identity_levenshtein
  expect_identical(lev("abc", "abc"), 0L)
  expect_identical(lev("b cel", "b cels"), 1L)      # insertion
  expect_identical(lev("celss", "cells"), 1L)       # substitution
  expect_identical(lev("cd4 t cel", "cd4 t cells"), 2L)
  expect_identical(lev("", "abc"), 3L)
  expect_identical(lev("abc", ""), 3L)
})

# ── 2. Propositions : statuts, preuves, cas limites ────────────────────────

test_that("propose: exact match kept, normalised equality proposed with evidence", {
  p <- propose_communication_identity_mapping(
    c("CD4 T cells", "CD4_T_Cells", "B cells"),
    c("CD4 T cells", "CD8 T cells", "B cells", "NK cells")
  )
  expect_identical(p$status[1], "matched_exact")
  expect_identical(p$candidate_identity[1], "CD4 T cells")
  expect_identical(p$rule[1], "exact")
  expect_identical(p$distance[1], 0L)
  # Spec §2.17 : divergence casse + ponctuation -> proposition normalisee.
  expect_identical(p$status[2], "proposed")
  expect_identical(p$candidate_identity[2], "CD4 T cells")
  expect_identical(p$rule[2], "normalise")
  expect_identical(p$distance[2], 0L)
  expect_match(p$detail[2], "normalisation")
  expect_true(grepl("trim", p$detail[2], fixed = TRUE))
  expect_identical(p$status[3], "matched_exact")
})

test_that("propose: levenshtein typo proposal, unique nearest, distance cap", {
  p <- propose_communication_identity_mapping(
    c("CD4 T cel", "B celss", "Totally Other"),
    c("CD4 T cells", "CD8 T cells", "B cells", "NK cells"),
    max_edit_distance = 2
  )
  expect_identical(p$status[1], "proposed")
  expect_identical(p$rule[1], "levenshtein")
  expect_identical(p$distance[1], 2L)
  expect_identical(p$candidate_identity[1], "CD4 T cells")
  expect_match(p$detail[1], "distance d'edition 2")
  expect_identical(p$candidate_identity[2], "B cells")
  expect_identical(p$distance[2], 1L)
  # Hors portee : ni candidat ni invention.
  expect_identical(p$status[3], "unmatched")
  expect_true(is.na(p$candidate_identity[3]))
  expect_match(p$detail[3], "max_edit_distance")
})

test_that("propose: normalised-key collisions and distance ties stay ambiguous", {
  p <- propose_communication_identity_mapping(
    c("T CELLS", "B cell"),
    c("T cells", "T Cells", "B cells", "B cellz")
  )
  # "t cells" : cle partagee par 2 identites (casse) -> aucune proposition.
  expect_identical(p$status[1], "ambiguous")
  expect_true(is.na(p$candidate_identity[1]))
  expect_match(p$detail[1], "partagee par 2 identites")
  # "b cell" : ex-aequo a distance 1 ("b cells" / "b cellz") -> aucune.
  expect_identical(p$status[2], "ambiguous")
  expect_true(is.na(p$candidate_identity[2]))
  expect_match(p$detail[2], "ex-aequo")
})

test_that("propose: exact match wins over normalised ambiguity, flagged in detail", {
  p <- propose_communication_identity_mapping("T cells", c("T cells", "T Cells"))
  expect_identical(p$status, "matched_exact")
  expect_identical(p$candidate_identity, "T cells")
  expect_match(p$detail, "ambiguite")
})

test_that("propose: roles passthrough and max_edit_distance = 0 disables fuzzy", {
  p <- propose_communication_identity_mapping(
    "B cel", "B cells", roles = "sender", max_edit_distance = 0
  )
  expect_identical(p$roles, "sender")
  expect_identical(p$status, "unmatched")
  expect_identical(p$rule, NA_character_)
})

test_that("propose: rejects blank/duplicate labels, empty identities, bad cap", {
  expect_error(
    propose_communication_identity_mapping(c("A", ""), c("A")),
    class = "communication_import_error"
  )
  expect_error(
    propose_communication_identity_mapping(c("A", "A"), c("A")),
    class = "communication_import_error"
  )
  expect_error(
    propose_communication_identity_mapping("A", character(0)),
    class = "communication_import_error"
  )
  expect_error(
    propose_communication_identity_mapping("A", NA_character_),
    class = "communication_import_error"
  )
  expect_error(
    propose_communication_identity_mapping("A", "A", roles = c("x", "y")),
    class = "communication_import_error"
  )
  expect_error(
    propose_communication_identity_mapping("A", "A", max_edit_distance = -1),
    class = "communication_import_error"
  )
  e <- tryCatch(
    propose_communication_identity_mapping(c("A", ""), c("A")),
    error = function(e) e
  )
  expect_s3_class(e, "communication_import_error")
  expect_identical(communication_error_state(e), "invalid_input")
})

# ── 3. AUCUNE application automatique (voie historique intacte) ────────────

test_that("NO auto-apply: historical path ignores proposals, inputs untouched", {
  tab <- .harm_tab("CD4+ T cells", "B cells")
  ids <- c("CD4 T cells", "B cells", "NK cells")
  tab_before <- tab
  p <- propose_communication_identity_mapping(
    unique(c(tab$sender, tab$receiver)), ids
  )
  # Une proposition EXISTE pour le label divergent...
  expect_identical(p$status[p$label == "CD4+ T cells"], "proposed")
  # ...mais l'harmonisation historique ne l'applique PAS.
  h <- harmonize_communication_identities(tab, ids, "cell_type")
  expect_true(is.na(h$table$sender_mapped[h$table$sender == "CD4+ T cells"]))
  expect_false(h$mapping$matched[h$mapping$label == "CD4+ T cells"])
  expect_identical(h$summary$n_user_mapped, 0L)
  expect_null(h$summary$user_mapping)
  # La table d'entree n'est jamais mutee (copie-on-modify).
  expect_identical(tab, tab_before)
})

test_that("historical path: same warnings, same zero-match guard, additive columns", {
  h <- harmonize_communication_identities(
    .harm_tab("Macro", "B cells"), c("CD4 T cells", "B cells"), "cell_type"
  )
  # Message d'avertissement HISTORIQUE, inchangé.
  expect_identical(
    h$warnings[1],
    paste0(
      "Harmonisation : 1 label(s) sans correspondance exacte dans 'cell_type' ",
      "(conserves tels quels, colonnes *_mapped a NA) : Macro."
    )
  )
  # Colonnes additives de provenance, valeurs par defaut de la voie historique.
  expect_identical(h$mapping$source, c("unmatched", "exact"))
  expect_identical(h$mapping$rule, c(NA_character_, "exact"))
  expect_identical(h$mapping$distance, c(NA_integer_, 0L))
  # Contrat de retour inchangé.
  expect_identical(names(h), c("table", "mapping", "summary", "warnings"))
  expect_true(all(c(
    "identity_column", "n_identities", "n_labels", "n_labels_matched",
    "n_labels_unmatched", "unmatched_labels", "ambiguous_identities",
    "ambiguous_labels", "n_rows", "n_rows_with_unmatched_role"
  ) %in% names(h$summary)))
  # Garde zero-correspondance historique (etat et message).
  e <- tryCatch(
    harmonize_communication_identities(
      .harm_tab("Macro", "Macro"), c("CD4 T cells", "B cells"), "cell_type"
    ),
    error = function(e) e
  )
  expect_s3_class(e, "communication_import_error")
  expect_identical(communication_error_state(e), "invalid_identity_mapping")
  expect_match(conditionMessage(e), "aucun label sender/receiver ne correspond")
})

# ── 4. Application d'un mapping VALIDE : rejets ────────────────────────────

test_that("apply: refuses NULL mapping (no auto-apply) and unknown identities", {
  tab <- .harm_tab("CD4+ T cells", "B cells")
  ids <- c("CD4 T cells", "B cells")
  expect_error(
    apply_communication_identity_mapping(tab, ids, "cell_type", NULL),
    class = "communication_import_error"
  )
  # Identité validée absente de la colonne choisie : rejet (jamais inventée).
  e <- tryCatch(
    apply_communication_identity_mapping(
      tab, ids, "cell_type",
      data.frame(label = "CD4+ T cells", identity = "NK cells")
    ),
    error = function(e) e
  )
  expect_s3_class(e, "communication_import_error")
  expect_identical(communication_error_state(e), "invalid_identity_mapping")
  expect_match(conditionMessage(e), "absente")
  # Structure invalide : rejet.
  expect_error(
    apply_communication_identity_mapping(tab, ids, "cell_type",
                                         data.frame(label = "CD4+ T cells")),
    class = "communication_import_error"
  )
  # Doublons contradictoires sur un meme label : rejet.
  expect_error(
    apply_communication_identity_mapping(
      tab, c("CD4 T cells", "NK cells", "B cells"), "cell_type",
      data.frame(label = c("CD4+ T cells", "CD4+ T cells"),
                 identity = c("CD4 T cells", "NK cells"))
    ),
    class = "communication_import_error"
  )
})

test_that("apply: nothing resolvable keeps the historical zero-match stop", {
  expect_error(
    apply_communication_identity_mapping(
      .harm_tab("Macro", "Macro"), c("CD4 T cells", "B cells"), "cell_type",
      data.frame(label = character(0), identity = character(0))
    ),
    class = "communication_import_error"
  )
})

# ── 5. Application d'un mapping VALIDE : effets attendus ───────────────────

test_that("apply: validated pairs applied with provenance, roles and warnings", {
  tab <- .harm_tab(sender = c("CD4+ T cells", "CD4+ T cells"),
                   receiver = c("B cells", "NK cells"))
  ids <- c("CD4 T cells", "B cells", "NK cells")
  am <- data.frame(
    label = "CD4+ T cells", identity = "CD4 T cells",
    rule = "normalise", distance = 0L   # colonnes de preuve tolerees/ignorees
  )
  res <- apply_communication_identity_mapping(tab, ids, "cell_type", am)
  # Table : les deux roles du label mappe portent l'identite validee.
  expect_identical(res$table$sender_mapped, c("CD4 T cells", "CD4 T cells"))
  expect_identical(res$table$receiver_mapped, c("B cells", "NK cells"))
  expect_true(all(res$table$source_cell_identity_level == "cell_type"))
  # Mapping : preuve de provenance par ligne.
  expect_identical(res$mapping$label, c("CD4+ T cells", "B cells", "NK cells"))
  expect_identical(res$mapping$source, c("user_approved", "exact", "exact"))
  expect_identical(res$mapping$rule, c("user_validated", "exact", "exact"))
  expect_identical(res$mapping$distance, c(NA_integer_, 0L, 0L))
  expect_true(all(res$mapping$matched))
  expect_identical(res$mapping$roles, c("sender", "receiver", "receiver"))
  # Summary : compteurs et paires appliquees.
  expect_identical(res$summary$n_user_mapped, 1L)
  expect_identical(res$summary$n_labels_matched, 3L)
  expect_identical(res$summary$n_labels_unmatched, 0L)
  expect_identical(res$summary$user_mapping$label, "CD4+ T cells")
  expect_identical(res$summary$user_mapping$identity, "CD4 T cells")
  # La validation laisse une TRACE visible (jamais silencieuse).
  expect_true(any(grepl("Mapping valide", res$warnings, fixed = TRUE)))
})

test_that("apply: blank identity means explicit leave-unmapped (dropped, no error)", {
  res <- apply_communication_identity_mapping(
    .harm_tab("CD4+ T cells", "B cells"), c("B cells"), "cell_type",
    data.frame(label = "CD4+ T cells", identity = NA_character_)
  )
  expect_true(is.na(res$table$sender_mapped))
  expect_identical(res$summary$n_user_mapped, 0L)
  expect_identical(
    res$mapping$source[res$mapping$label == "CD4+ T cells"], "unmatched"
  )
})

test_that("apply: explicit override of an exact match is applied and warned", {
  tab <- .harm_tab("T cells", "B cells")
  ids <- c("T cells", "T Cells", "B cells")   # colonne a ambiguite de casse
  res <- apply_communication_identity_mapping(
    tab, ids, "cell_type",
    data.frame(label = "T cells", identity = "T Cells")
  )
  expect_identical(res$table$sender_mapped, "T Cells")
  expect_identical(res$summary$n_user_over_exact, 1L)
  expect_identical(
    res$mapping$source[res$mapping$label == "T cells"], "user_approved"
  )
  expect_true(any(grepl("re-mapés explicitement", res$warnings, fixed = TRUE)))
  # L'ambiguite de casse reste signalee (message historique conserve).
  expect_true(any(grepl("ambiguës", res$warnings, fixed = TRUE)))
})

test_that("apply: unknown labels in the validated mapping are ignored with a warning", {
  out <- .capture_warnings(
    apply_communication_identity_mapping(
      .harm_tab("CD4+ T cells", "B cells"), c("CD4 T cells", "B cells"),
      "cell_type",
      data.frame(label = "Pas dans la table", identity = "B cells")
    )
  )
  # harmonize RETOURNE ses warnings (il n'appelle jamais warning()) :
  # le trace du mapping ignore est dans le champ warnings du resultat.
  expect_true(any(grepl("ignoree", out$res$warnings)))
  expect_identical(out$res$summary$n_user_mapped, 0L)
  expect_identical(out$res$mapping$source, c("unmatched", "exact"))
})

# ── 6. Consommation en aval : contrat du resultat canonique ────────────────

test_that("apply output feeds finalize_communication_result (contract-safe)", {
  tab <- .harm_tab(sender = c("CD4+ T cells", "CD4+ T cells"),
                   receiver = c("B cells", "NK cells"))
  res <- apply_communication_identity_mapping(
    tab, c("CD4 T cells", "B cells", "NK cells"), "cell_type",
    data.frame(label = "CD4+ T cells", identity = "CD4 T cells")
  )
  fin <- finalize_communication_result(
    canonical_table = res$table,
    source_method = "cellphonedb",
    source_files = list(means = "means.txt"),
    identity_column = "cell_type",
    identity_mapping = res$mapping,
    identity_summary = res$summary,
    column_mapping = list(),
    qc = list(),
    n_input_rows = 2L,
    seurat_obj = NULL
  )
  expect_true(is.data.frame(fin$identity_mapping))
  expect_identical(fin$identity_mapping$source,
                   c("user_approved", "exact", "exact"))
  expect_identical(fin$identity_summary$n_user_mapped, 1L)
  # L'export CSV historique ne selectionne que les 4 colonnes d'origine :
  # aucun changement de contrat d'export.
  ex <- build_communication_identity_mapping_export(fin)
  expect_identical(
    colnames(ex),
    c("label", "roles", "matched_identity", "matched", "identity_column",
      "analysis_id", "timestamp_utc")
  )
})
