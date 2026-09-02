# =============================================================================
# test-communication-contract-freeze.R — Stage 11 : gel du contrat de resultat
# communication (import et validation uniquement)
# =============================================================================
# Ce fichier REFUSE toute evolution incompatible du contrat :
#   - champs du resultat canonique (schema fige, absence = NULL/NA) ;
#   - etats de validite + libelles ;
#   - empreinte objet (v2, REUTILISEE de velocity — jamais dupliquee) ;
#   - surface publique de R/sc/sc_communication.R
#     (communication_public_api()) ;
#   - orchestration module (3 fonctions top-level uniquement) ;
#   - isolation des consommateurs rapport/export (aucune reimplementation
#     de l'import) ;
#   - synchronisation code <-> docs/contracts/COMMUNICATION_RESULT_CONTRACT.md.
# Toute modification du contrat doit passer simultanement par le code, ce
# test et la documentation. Fixtures partages : helper-communication-fixtures.R.
#
# NB : l'extraction des noms top-level est reecrite ici via parse data
# (implantation VOLONTAIREMENT differente du test velocity, pour ne pas
# dupliquer un helper de test ligne a ligne).
# =============================================================================

.comm_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    # Affectation top-level "<-" dont le cote gauche est un symbole simple
    # (les affectations internes aux fonctions ne sont PAS des expressions
    # top-level — parse() ne retourne que le premier niveau).
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# ── Resultat canonique complet (avec colonnes d'origine + pvalues) ──────────
.comm_freeze_canonical_full <- function() {
  parsed <- parse_cellphonedb_import(
    .comm_cellphonedb_means(), .comm_cellphonedb_pvalues(),
    source_file = "means.txt"
  )
  .comm_import_and_finalize(
    parsed, source_files = list(means = "means.txt", pvalues = "pvalues.txt")
  )
}

# ── Resultat canonique minimal (CellChat, sans p-values) ────────────────────
.comm_freeze_canonical_min <- function() {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  .comm_import_and_finalize(parsed)
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical result exposes every frozen contract field", {
  canonical <- .comm_freeze_canonical_full()
  contract <- c(
    "type", "status", "source_method", "canonical_table", "identity_column",
    "identity_mapping", "identity_summary", "column_mapping", "qc",
    "input_summary", "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc"
  )
  expect_identical(setdiff(contract, names(canonical)), character(0))
})

test_that("canonical table carries the 12 documented fields on both sources", {
  for (canon in list(.comm_freeze_canonical_full(), .comm_freeze_canonical_min())) {
    expect_identical(
      setdiff(communication_contract_fields(),
              colnames(canon$canonical_table)),
      character(0)
    )
    expect_true(all(c("sender_mapped", "receiver_mapped",
                      "duplicate_interaction") %in%
                      colnames(canon$canonical_table)))
  }
})

test_that("frozen per-field shapes on the canonical result", {
  canonical <- .comm_freeze_canonical_full()
  expect_identical(canonical$type, "cell_cell_communication")
  expect_identical(canonical$status, "valid")
  expect_identical(canonical$source_method, "cellphonedb")
  expect_identical(canonical$provenance$analysis_type,
                   "cell_cell_communication")
  expect_true(isTRUE(canonical$provenance$import_only))
  expect_identical(canonical$provenance$analysis_id, "sc-communication-import")
  expect_match(canonical$object_identity$fingerprint, "^v2::")
  expect_match(canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_true(is.data.frame(canonical$identity_mapping))
  expect_true(is.list(canonical$column_mapping))
  # input_summary : noms de fichiers ORIGINAUX uniquement.
  expect_identical(canonical$input_summary$files$means, "means.txt")
  expect_identical(canonical$input_summary$files$pvalues, "pvalues.txt")
})

test_that("absent components stay NULL/NA (no fabrication policy)", {
  canonical <- .comm_freeze_canonical_min()
  # CellChat sans pval/padj : NA, jamais fabriques.
  expect_true(all(is.na(canonical$canonical_table$p_value)))
  expect_true(all(is.na(canonical$canonical_table$p_adjusted)))
  # Les colonnes d'origine presentes sont conservees telles quelles.
  expect_identical(canonical$canonical_table$pathway,
                   c("IL7 signaling", "CD40 signaling", "CCL signaling"))
})

# ── Etats de validite figes ─────────────────────────────────────────────────
test_that("validity states and labels are frozen", {
  expect_setequal(communication_validity_states(), c(
    "valid", "invalid_input", "invalid_schema",
    "invalid_identity_mapping", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(communication_status_labels()),
                  communication_validity_states())
})

test_that("object identity is pinned to the v2 fingerprint reused from velocity", {
  expect_match(velocity_object_fingerprint(.comm_obj_stub()), "^v2::")
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  canonical <- .comm_import_and_finalize(parsed)
  expect_identical(canonical$object_identity$fingerprint,
                   velocity_object_fingerprint(.comm_obj_stub()))
  expect_match(canonical$object_identity$method, "velocity_object_fingerprint",
               fixed = TRUE)
})

# ── Surface publique figee ──────────────────────────────────────────────────
test_that("public API surface of sc_communication.R is frozen", {
  defined <- .comm_top_level_assignments("R/sc/sc_communication.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, communication_public_api())
  # Helpers internes prefixes d'un point, jamais publics.
  expect_true(all(c(".COMMUNICATION_STATUS_STATES", ".communication_stop",
                    ".COMMUNICATION_FIELD_ALIASES", ".communication_pick_column",
                    ".communication_is_blank", ".communication_coerce_numeric",
                    ".communication_object_fingerprint") %in%
                    setdiff(defined, public)))
})

test_that("communication module defines exactly its three orchestration functions", {
  expect_setequal(
    .comm_top_level_assignments("modules/sc/mod_sc_communication.R"),
    c("mod_sc_communication_ui", "mod_sc_communication_output_ui",
      "mod_sc_communication_server")
  )
  # Le module consomme les accesseurs documentes (contrat Stage 11) —
  # aucune reimplementation locale de la logique d'import.
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_communication.R")),
               collapse = "\n")
  expect_match(src, "parse_cellchat_import(", fixed = TRUE)
  expect_match(src, "parse_cellphonedb_import(", fixed = TRUE)
  expect_match(src, "harmonize_communication_identities(", fixed = TRUE)
  expect_match(src, "communication_import_qc(", fixed = TRUE)
  expect_match(src, "finalize_communication_result(", fixed = TRUE)
  expect_match(src, "communication_status_labels(", fixed = TRUE)
  expect_match(src, "communication_export_filename(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  # Import uniquement : aucun appel CellChat/CellPhoneDB (aucun calcul).
  expect_false(grepl("library\\(|CellChatDB|computeCommunProb|::run",
                     src))
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement communication import", {
  # mod_sc.R monte le module (aucune logique de resultat).
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_communication_server", fixed = TRUE)
  expect_false(grepl("finalize_communication_result|parse_cellchat_import|harmonize_communication_identities",
                     mod_sc_src))

  # Le script reproducible et les templates de rapport ignorent encore la
  # communication (branche rapport reservee au Stage 17).
  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("communication", export_src, ignore.case = TRUE))
  for (tpl in list.files(file.path(ts_project_root(), "reports"),
                         full.names = TRUE)) {
    tpl_src <- paste(readLines(tpl), collapse = "\n")
    expect_false(grepl("communication", tpl_src, ignore.case = TRUE),
                 info = basename(tpl))
  }
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "COMMUNICATION_RESULT_CONTRACT.md")
  expect_true(file.exists(doc_path))
  doc <- paste(readLines(doc_path), collapse = "\n")
  for (f in communication_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in communication_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  expect_match(doc, "finalize_communication_result", fixed = TRUE)
  expect_match(doc, "assert_communication_result", fixed = TRUE)
  expect_match(doc, "communication_public_api", fixed = TRUE)
})
