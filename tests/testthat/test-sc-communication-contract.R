# =============================================================================
# test-sc-communication-contract.R — Stage 11 (4D-1) : contrat de resultat
# communication (import et validation uniquement)
# =============================================================================
# Fixtures deterministes : tables CellChat / CellPhoneDB synthetiques + echecs
# de schema, d'identite et de QC. Aucune donnee biologique reelle. Les
# fixtures partages vivent dans helper-communication-fixtures.R.
# Invariants testes : conversion canonique sans invention de valeurs, mapping
# de colonnes explicite, harmonisation exact match uniquement (jamais de
# renommage), QC compte avant/apres, provenance produite a l'import,
# peremption derivee de l'empreinte v2 (reutilisee de velocity).
# =============================================================================

# ── Contrat : champs, sources, etats ────────────────────────────────────────
test_that("communication contract exposes documented fields, sources and states", {
  expect_setequal(communication_contract_fields(), c(
    "sender", "receiver", "ligand", "receptor", "interaction", "pathway",
    "score", "p_value", "p_adjusted",
    "source_method", "source_file", "source_cell_identity_level"
  ))
  expect_setequal(communication_supported_sources(), c("cellchat", "cellphonedb"))
  expect_setequal(communication_validity_states(), c(
    "valid", "invalid_input", "invalid_schema",
    "invalid_identity_mapping", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(communication_status_labels()), communication_validity_states())
  expect_true(communication_status_is_valid("valid"))
  expect_false(communication_status_is_valid("invalid_input"))
  expect_false(communication_status_is_valid("stale_against_current_seurat_object"))
  # Validite technique != validite biologique : explicite dans les libelles.
  expect_match(communication_status_labels()[["valid"]], "technique")
})

# ── Fixture 1 : CellChat valide ─────────────────────────────────────────────
test_that("Fixture 1 - valid CellChat table maps to the canonical fields", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab(),
                                  source_file = "cellchat_export.csv")
  tab <- parsed$table

  expect_true(all(communication_contract_fields() %in% colnames(tab)))
  expect_identical(nrow(tab), 3L)
  expect_identical(parsed$n_input_rows, 3L)
  expect_identical(tab$sender, c("CD4 T", "B", "CD4 T"))
  expect_identical(tab$receiver, c("B", "CD8 T", "CD8 T"))
  # interaction derivee, direction explicite.
  expect_identical(tab$interaction[1], "IL7 -> IL7R")
  expect_identical(tab$source_method, rep("cellchat", 3L))
  expect_identical(tab$source_file, rep("cellchat_export.csv", 3L))
  expect_true(is.numeric(tab$score))
  # Champs absents de la source : NA, jamais fabriques.
  expect_true(all(is.na(tab$p_value)))
  expect_true(all(is.na(tab$p_adjusted)))
  expect_true(all(is.na(tab$source_cell_identity_level)))
  # Mapping de colonnes resolu de facon deterministe et enregistre.
  expect_identical(parsed$column_mapping$sender, "source")
  expect_identical(parsed$column_mapping$receiver, "target")
  expect_identical(parsed$column_mapping$score, "prob")
  expect_identical(parsed$column_mapping$pathway, "pathway")
})

test_that("Fixture 1b - CellChat extra origin columns are retained and mapped", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab_groups())
  expect_true(all(c("ligand_group", "receptor_group") %in% colnames(parsed$table)))
  expect_identical(parsed$column_mapping$ligand_group, "ligand.group")
  expect_identical(parsed$column_mapping$receptor_group, "receptor.group")
})

test_that("Fixture 1c - CellChat missing required column is rejected with state", {
  tab <- .comm_cellchat_tab()
  tab$receptor <- NULL
  e <- tryCatch(parse_cellchat_import(tab), error = function(e) e)
  expect_s3_class(e, "communication_import_error")
  expect_identical(communication_error_state(e), "invalid_schema")
  expect_match(conditionMessage(e), "CellChat")
  # La colonne requise manquante et la table d'origine sont citees
  # (message actionnable, pas d'echec silencieux).
  expect_match(conditionMessage(e), "receptor")
  expect_match(conditionMessage(e), "prob")
})

test_that("Fixture 1d - CellChat NULL/empty input rejected as invalid_input", {
  e1 <- tryCatch(parse_cellchat_import(NULL), error = function(e) e)
  expect_identical(communication_error_state(e1), "invalid_input")
  e2 <- tryCatch(parse_cellchat_import(data.frame()), error = function(e) e)
  expect_identical(communication_error_state(e2), "invalid_input")
})

test_that("Fixture 1e - non-numeric CellChat scores become NA with a warning", {
  tab <- .comm_cellchat_tab()
  tab$prob[2] <- "n/a"
  parsed <- parse_cellchat_import(tab)
  expect_true(is.na(parsed$table$score[2]))
  expect_identical(parsed$table$score[1], 0.3)
  expect_match(parsed$warnings, "score")
})

# ── Fixture 2 : CellPhoneDB valide (means + pvalues) ────────────────────────
test_that("Fixture 2 - valid CellPhoneDB means+pvalues reshape to canonical long table", {
  parsed <- parse_cellphonedb_import(
    .comm_cellphonedb_means(), .comm_cellphonedb_pvalues(),
    source_file = "means.txt"
  )
  tab <- parsed$table
  # 3 interactions x 3 paires de populations = 9 lignes canoniques.
  expect_identical(nrow(tab), 9L)
  expect_identical(parsed$n_input_rows, 3L)
  expect_identical(tab$source_method, rep("cellphonedb", 9L))
  # sender/receiver issus du nom de colonne de paire (partner_a|partner_b).
  expect_true(all(c("CD4 T", "B", "CD8 T") %in% tab$sender))
  expect_true(all(c("CD4 T", "B", "CD8 T") %in% tab$receiver))
  # ligand/receptor issus de la separation d'interacting_pair.
  expect_identical(sort(unique(tab$ligand)), c("CCL5", "CD40", "IL7"))
  expect_identical(sort(unique(tab$receptor)), c("CCR5", "CD40", "IL7R"))
  expect_identical(tab$interaction[1], "IL7 -> IL7R")
  # p-values rapprochees par (interacting_pair, colonne de paire).
  sel <- tab[tab$ligand == "IL7" & tab$sender == "CD4 T", , drop = FALSE]
  expect_identical(nrow(sel), 1L)
  expect_identical(sel$score, 0.3)
  expect_identical(sel$p_value, 0.01)
  # p_adjusted n'existe pas dans ce format : NA + avertissement explicite.
  expect_true(all(is.na(tab$p_adjusted)))
  expect_true(any(grepl("p_adjusted", parsed$warnings, fixed = TRUE)))
  # pathway absent du format : NA, jamais fabrique.
  expect_true(all(is.na(tab$pathway)))
  expect_identical(tab$source_file, rep("means.txt", 9L))
})

test_that("Fixture 2b - CellPhoneDB without pvalues keeps p_value NA (no fabrication)", {
  parsed <- parse_cellphonedb_import(.comm_cellphonedb_means())
  expect_true(all(is.na(parsed$table$p_value)))
  expect_false(any(grepl("table pvalues", parsed$warnings)))
})

test_that("Fixture 2c - CellPhoneDB pvalues pair missing from means stays NA + warning", {
  pv <- .comm_cellphonedb_pvalues()
  pv$"CD4 T|B" <- NULL
  parsed <- parse_cellphonedb_import(.comm_cellphonedb_means(), pv)
  sel <- parsed$table[parsed$table$ligand == "IL7" & parsed$table$sender == "CD4 T", ]
  expect_true(is.na(sel$p_value))
  # Les autres paires restent rapprochees (jamais de drop global).
  sel2 <- parsed$table[parsed$table$ligand == "CD40" & parsed$table$sender == "B", ]
  expect_identical(sel2$p_value, 0.03)
})

test_that("Fixture 2d - CellPhoneDB interacting_pair without '|' is rejected (invalid_schema)", {
  means <- .comm_cellphonedb_means()
  means$interacting_pair[2] <- "CD40-CD40"
  e <- tryCatch(
    parse_cellphonedb_import(means),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_schema")
  expect_match(conditionMessage(e), "CD40-CD40")
})

test_that("Fixture 2e - CellPhoneDB means without pair columns is rejected", {
  means <- .comm_cellphonedb_means()
  means <- means[, !grepl("|", colnames(means), fixed = TRUE), drop = FALSE]
  e <- tryCatch(parse_cellphonedb_import(means), error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_schema")
})

test_that("Fixture 2f - CellPhoneDB NULL/empty means rejected as invalid_input", {
  e1 <- tryCatch(parse_cellphonedb_import(NULL), error = function(e) e)
  expect_identical(communication_error_state(e1), "invalid_input")
  e2 <- tryCatch(parse_cellphonedb_import(data.frame()), error = function(e) e)
  expect_identical(communication_error_state(e2), "invalid_input")
})

# ── Harmonisation des identites : exact match uniquement ────────────────────
test_that("Fixture 3 - full identity match maps sender/receiver by exact match", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  harm <- harmonize_communication_identities(
    parsed$table, .comm_identities, "cell_type"
  )
  tab <- harm$table
  expect_identical(tab$sender_mapped, tab$sender)
  expect_identical(tab$receiver_mapped, tab$receiver)
  expect_identical(tab$source_cell_identity_level, rep("cell_type", 3L))
  expect_identical(harm$summary$n_labels_matched, 3L)
  expect_identical(harm$summary$n_labels_unmatched, 0L)
  expect_identical(harm$summary$identity_column, "cell_type")
  expect_true(is.data.frame(harm$mapping))
  expect_true(all(harm$mapping$matched))
  expect_identical(harm$summary$n_rows_with_unmatched_role, 0L)
})

test_that("Fixture 3b - unmatched labels are reported, kept as-is (no silent renaming)", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  harm <- harmonize_communication_identities(
    parsed$table, c("CD4 T", "B", "NK"), "cell_type"  # "CD8 T" absent
  )
  tab <- harm$table
  expect_identical(tab$sender_mapped, c("CD4 T", "B", "CD4 T"))
  expect_true(all(is.na(tab$receiver_mapped[2:3])))
  expect_setequal(harm$summary$unmatched_labels, "CD8 T")
  expect_identical(harm$summary$n_rows_with_unmatched_role, 2L)
  expect_true(any(grepl("CD8 T", harm$warnings, fixed = TRUE)))
  # Le label original est conserve tel quel dans sender/receiver.
  expect_true("CD8 T" %in% tab$receiver)
})

test_that("Fixture 3c - zero exact match blocks the import (invalid_identity_mapping)", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  e <- tryCatch(
    harmonize_communication_identities(parsed$table, c("Tumeur", "Stroma"), "tissue"),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_identity_mapping")
  expect_match(conditionMessage(e), "tissue")
})

test_that("Fixture 3d - case-only collisions are reported as ambiguous (mapping stays exact)", {
  tab <- .comm_cellchat_tab()
  tab$target[1] <- "b"  # vs identite "B" : uniquement la casse differe
  parsed <- parse_cellchat_import(tab)
  harm <- harmonize_communication_identities(
    parsed$table, c("CD4 T", "CD8 T", "B", "b", "NK"), "cell_type"
  )
  # Les identites B/b (distinguees uniquement par la casse) sont signalees
  # ambiguës ; l'exact match reste deterministe ("b" correspond a "b").
  expect_setequal(harm$summary$ambiguous_identities, c("B", "b"))
  expect_true(any(grepl("casse", harm$warnings)))
  expect_identical(harm$table$receiver_mapped[1], "b")
})

test_that("Fixture 3e - empty identity vector blocks with actionable error", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  e <- tryCatch(
    harmonize_communication_identities(parsed$table, character(0), "cell_type"),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_identity_mapping")
})

# ── QC d'import ──────────────────────────────────────────────────────────────
test_that("Fixture 4 - QC counts blanks, self-interactions, duplicates and p-values", {
  # Partir de la TABLE CANONIQUE (parse d'abord), puis alterer les champs
  # contractuels — la fixture brute porte les noms CellChat (source/target).
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  tab <- parsed$table
  tab$sender[2] <- ""                      # champ requis vide -> droppee
  tab$receiver[3] <- tab$sender[3]         # auto-interaction
  tab$p_value <- c(0.01, 0.5, 1.5)         # 1.5 hors [0,1]
  tab$ligand[3] <- "CD40"                  # ligand[3] = ligand[2] -> duplication
  tab <- rbind(tab, tab[3, ])              # cle dupliquee (2 lignes identiques)

  qcr <- communication_import_qc(tab)
  expect_identical(qcr$counts$n_rows_before, 4L)
  expect_identical(qcr$counts$n_rows_after, 3L)
  expect_identical(qcr$counts$n_dropped_required_fields, 1L)
  # La ligne 3 ET sa copie sont des auto-interactions (sender = receiver).
  expect_identical(qcr$counts$n_self_interactions, 2L)
  expect_identical(qcr$counts$n_duplicate_interactions, 2L)
  expect_identical(qcr$counts$n_p_value_out_of_range, 2L)
  expect_identical(qcr$counts$n_pathway_missing, 0L)
  # Duplications conservees et flagees, jamais fusionnees.
  expect_identical(sum(qcr$table$duplicate_interaction), 2L)
  expect_true(all(qcr$table$duplicate_interaction[qcr$table$ligand == "CD40" &
                                                    qcr$table$sender == "CD4 T"]))
  expect_match(qcr$warnings[1], "1 ligne")
})

test_that("Fixture 4b - QC on empty-after-filter table stays consistent (no crash)", {
  tab <- .comm_cellchat_tab()
  tab$ligand <- ""
  parsed <- parse_cellchat_import(tab)
  qcr <- communication_import_qc(parsed$table)
  expect_identical(qcr$counts$n_rows_after, 0L)
  expect_identical(nrow(qcr$table), 0L)
  expect_identical(length(qcr$table$duplicate_interaction), 0L)
})

# ── Finalisation : contrat canonique ────────────────────────────────────────
test_that("Fixture 5 - canonical result exposes every contract field with provenance", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab_groups(),
                                  source_file = "cellchat_export.csv")
  canonical <- .comm_import_and_finalize(
    parsed, source_files = list(table = "cellchat_export.csv")
  )
  contract <- c(
    "type", "status", "source_method", "canonical_table", "identity_column",
    "identity_mapping", "identity_summary", "column_mapping", "qc",
    "input_summary", "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc"
  )
  expect_identical(setdiff(contract, names(canonical)), character(0))
  expect_identical(canonical$type, "cell_cell_communication")
  expect_identical(canonical$status, "valid")
  expect_identical(canonical$source_method, "cellchat")
  expect_identical(canonical$identity_column, "cell_type")
  expect_identical(canonical$input_summary$files$table, "cellchat_export.csv")
  expect_identical(canonical$input_summary$n_rows_canonical, 3L)
  expect_match(canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  # Provenance PRODUITE a l'import (regle 7) : entree complete.
  expect_identical(canonical$provenance$analysis_id, "sc-communication-import")
  expect_identical(canonical$provenance$method, "import_cellchat")
  expect_identical(canonical$provenance$analysis_type, "cell_cell_communication")
  expect_true(isTRUE(canonical$provenance$import_only))
  expect_match(canonical$provenance$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_identical(canonical$provenance$parameters$source_files$table,
                   "cellchat_export.csv")
  # Empreinte v2 REUTILISEE de velocity (pas de deuxieme formule).
  expect_identical(canonical$object_identity$fingerprint,
                   velocity_object_fingerprint(.comm_obj_stub()))
})

test_that("Fixture 5b - warnings from parse/harmonization/QC are merged without duplicates", {
  tab <- .comm_cellchat_tab()
  tab$target[3] <- "CD8 T cell"   # label sans correspondance
  parsed <- parse_cellchat_import(tab)
  canonical <- .comm_import_and_finalize(parsed)
  expect_true(any(grepl("CD8 T cell", canonical$warnings, fixed = TRUE)))
  expect_false(any(duplicated(canonical$warnings)))
})

test_that("Fixture 5c - a table mixing two source methods is rejected", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  mixed <- parsed$table
  mixed$source_method[2] <- "cellphonedb"
  harm <- harmonize_communication_identities(mixed, .comm_identities, "cell_type")
  qcr <- communication_import_qc(harm$table)
  e <- tryCatch(
    finalize_communication_result(
      canonical_table = qcr$table, source_method = "cellchat",
      qc = qcr$counts, extra_warnings = c(parsed$warnings, harm$warnings, qcr$warnings)
    ),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_input")
  expect_match(conditionMessage(e), "sources")
})

test_that("Fixture 5d - missing canonical fields in the table is rejected", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  broken <- parsed$table[, !colnames(parsed$table) %in% "p_adjusted"]
  e <- tryCatch(
    finalize_communication_result(
      canonical_table = broken, source_method = "cellchat"
    ),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_input")
  expect_match(conditionMessage(e), "p_adjusted")
})

# ── Peremption et garde de contrat ──────────────────────────────────────────
test_that("stale detection follows the v2 fingerprint (match/mismatch/NA)", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  canonical <- .comm_import_and_finalize(parsed)
  expect_false(communication_result_is_stale(canonical, .comm_obj_stub()))
  other <- matrix(0, nrow = 3, ncol = 2, dimnames = list(c("g1", "g2", "g3"), c("x", "y")))
  expect_true(communication_result_is_stale(canonical, other))
  expect_true(is.na(communication_result_is_stale(canonical, NULL)))
  expect_true(is.na(communication_result_is_stale(NULL, .comm_obj_stub())))
})

test_that("assert_communication_result guards consumers (no repair, no inference)", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  canonical <- .comm_import_and_finalize(parsed)

  expect_error(assert_communication_result(NULL), class = "communication_import_error")
  expect_error(assert_communication_result(list(type = "rna_velocity")),
               class = "communication_import_error")
  bad <- canonical; bad$status <- "etat_inconnu"
  expect_error(assert_communication_result(bad), class = "communication_import_error")
  # Perime : refuse quand l'objet courant a change.
  other <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("x", "y")))
  e <- tryCatch(assert_communication_result(canonical, seurat_obj = other),
                error = function(e) e)
  expect_identical(communication_error_state(e), "stale_against_current_seurat_object")
  # Valide : retourne le resultat, invisible (pipable).
  res <- assert_communication_result(canonical, seurat_obj = .comm_obj_stub(),
                                     context = "vue test")
  expect_identical(res, canonical)
})

# ── Exports ──────────────────────────────────────────────────────────────────
test_that("import summary export is a one-row traceable data.frame", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab(),
                                  source_file = "cellchat_export.csv")
  canonical <- .comm_import_and_finalize(parsed, source_files = list(table = "cellchat_export.csv"))
  df <- build_communication_import_summary(canonical)
  expect_identical(nrow(df), 1L)
  expect_identical(df$analysis_id, "sc-communication-import")
  expect_identical(df$source_method, "cellchat")
  expect_match(df$source_files, "cellchat_export.csv", fixed = TRUE)
  expect_identical(df$identity_column, "cell_type")
  expect_identical(df$import_only, "TRUE")
  expect_match(df$object_fingerprint, "^v2::")
  expect_true(is.character(df$warnings))
})

test_that("identity mapping export reproduces the mapping produced at import", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  canonical <- .comm_import_and_finalize(parsed)
  df <- build_communication_identity_mapping_export(canonical)
  expect_identical(nrow(df), nrow(canonical$identity_mapping))
  expect_true(all(c("label", "matched_identity", "matched", "identity_column",
                    "analysis_id") %in% colnames(df)))
  expect_true(all(df$analysis_id == "sc-communication-import"))
  # Sans mapping (resultat etranger), erreur classee explicite.
  e <- tryCatch(
    build_communication_identity_mapping_export(list(type = "cell_cell_communication", status = "valid")),
    error = function(e) e
  )
  expect_identical(communication_error_state(e), "invalid_input")
})

test_that("export filenames are traced by analysis_id", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  canonical <- .comm_import_and_finalize(parsed)
  fn <- communication_export_filename(canonical, "communication_import_table", "csv")
  expect_match(fn, "^communication_import_table_sc-communication-import_\\d{4}-\\d{2}-\\d{2}\\.csv$")
  expect_error(communication_export_filename(NULL, "k", "csv"),
               class = "communication_import_error")
})
