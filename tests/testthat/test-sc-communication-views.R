# =============================================================================
# test-sc-communication-views.R — Stage 12 (4D-2) : vues exploratoires des
# imports communication valides
# =============================================================================
# Route objet CellChat (extraction, PAS de calcul), filtres d'affichage
# (jamais destructifs), DotPlot/heatmap/reseau circulaire en ggplot pur,
# centralite descriptive, provenance des filtres, exports traces.
# Aucune donnee biologique reelle ; fixtures deterministes.
# =============================================================================

# ── Route objet CellChat : extraction sans recalcul ─────────────────────────
test_that("Fixture O1 - CellChat object stub extracts non-zero interactions only", {
  parsed <- parse_cellchat_object(.comm_cellchat_object_stub(),
                                  source_file = "cellchat_obj.rds")
  tab <- parsed$table
  # 8 valeurs dont 3 nulles -> 5 lignes extraites (fidele, sans agregation).
  expect_identical(nrow(tab), 5L)
  expect_identical(parsed$n_input_rows, 2L)   # 2 paires
  expect_identical(sort(unique(tab$sender)), c("B", "CD4 T"))
  expect_identical(sort(unique(tab$receiver)), c("B", "CD4 T"))
  expect_identical(tab$source_method, rep("cellchat", 5L))
  # p-values extraites de net$pval (meme forme) par indices.
  sel <- tab[tab$sender == "CD4 T" & tab$ligand == "IL7", ]
  expect_identical(sel$score, 0.5)
  expect_identical(sel$p_value, 0.01)
  sel2 <- tab[tab$sender == "B" & tab$ligand == "CCL5" & tab$receptor == "CCR5", ]
  expect_identical(nrow(sel2), 1L)
  expect_identical(sel2$score, 0.1)
  expect_identical(sel2$p_value, 0.01)
  # Route objet : pathway jamais reconstitue, avertissement explicite.
  expect_true(all(is.na(tab$pathway)))
  expect_true(any(grepl("pathway", parsed$warnings)))
  expect_true(all(is.na(tab$p_adjusted)))
  expect_identical(parsed$column_mapping$ligand, "dimnames(net$prob)[[1]]")
})

test_that("Fixture O2 - CellChat object route works through an .rds file", {
  path <- tempfile(fileext = ".rds")
  saveRDS(.comm_cellchat_object_stub(), path)
  parsed <- parse_cellchat_object(path, source_file = basename(path))
  expect_identical(nrow(parsed$table), 5L)
  expect_true(all(parsed$table$source_file == basename(path)))
})

test_that("Fixture O3 - CellChat object without pval (or wrong pval shape) keeps p NA", {
  parsed <- parse_cellchat_object(.comm_cellchat_object_stub(with_pval = FALSE))
  expect_true(all(is.na(parsed$table$p_value)))
  # Forme incompatible : p NA + avertissement, jamais de fabrication.
  bad <- .comm_cellchat_object_stub()
  bad$net$pval <- array(1, dim = c(2, 2))
  parsed2 <- parse_cellchat_object(bad)
  expect_true(all(is.na(parsed2$table$p_value)))
  expect_true(any(grepl("forme differente", parsed2$warnings)))
})

test_that("Fixture O4 - CellChat object failure modes are explicit and classed", {
  # net$prob absent.
  e1 <- tryCatch(parse_cellchat_object(list(net = list())), error = function(e) e)
  expect_identical(communication_error_state(e1), "invalid_schema")
  # Array sans dimnames.
  bad <- list(net = list(prob = array(1, dim = c(2, 2, 2))))
  e2 <- tryCatch(parse_cellchat_object(bad), error = function(e) e)
  expect_identical(communication_error_state(e2), "invalid_schema")
  # Paire sans separateur unique.
  bad2 <- .comm_cellchat_object_stub()
  dimnames(bad2$net$prob)[[3]] <- c("CD4 T-B", "B|CD4 T")
  e3 <- tryCatch(parse_cellchat_object(bad2), error = function(e) e)
  expect_identical(communication_error_state(e3), "invalid_schema")
  expect_match(conditionMessage(e3), "CD4 T-B")
  # Tout nul : rien a importer.
  zero <- .comm_cellchat_object_stub()
  zero$net$prob[] <- 0
  e4 <- tryCatch(parse_cellchat_object(zero), error = function(e) e)
  expect_identical(communication_error_state(e4), "invalid_input")
  # Objet d'un type inattendu.
  e5 <- tryCatch(parse_cellchat_object(42), error = function(e) e)
  expect_identical(communication_error_state(e5), "invalid_input")
  # Chemin inexistant.
  e6 <- tryCatch(parse_cellchat_object("inexistant.rds"), error = function(e) e)
  expect_identical(communication_error_state(e6), "invalid_input")
  # S4 sans slot 'net' (classe de test definie localement — pas CellChat).
  if (!"CommTestNoNet" %in% methods::getClasses(where = globalenv())) {
    methods::setClass("CommTestNoNet", slots = list(x = "numeric"),
                      where = globalenv())
  }
  s4 <- methods::new("CommTestNoNet", x = 1)
  e7 <- tryCatch(parse_cellchat_object(s4), error = function(e) e)
  expect_identical(communication_error_state(e7), "invalid_schema")
})

# ── Filtres d'affichage : jamais destructifs, comptages explicites ──────────
test_that("Fixture F1 - default filters return the full canonical table", {
  r <- .comm_result_big()
  before <- r$canonical_table
  fs <- communication_apply_filters(r)
  expect_identical(nrow(fs$table), nrow(before))
  expect_identical(fs$summary$n_before, fs$summary$n_after)
  expect_identical(before, r$canonical_table)   # non modifiee
})

test_that("Fixture F2 - score and p-value filters drop unverifiable rows explicitly", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.5))
  expect_true(all(fs$table$score >= 0.5))
  expect_identical(fs$summary$dropped_score, sum(r$canonical_table$score < 0.5))
  # p_value_max : les lignes sans p-value sont retirees (filtre non
  # verifiable ne passe pas) et comptabilisees.
  fs2 <- communication_apply_filters(r, list(p_value_max = 0.1))
  expect_true(all(fs2$table$p_value <= 0.1))
  expect_identical(fs2$summary$dropped_p_value,
                   sum(is.na(r$canonical_table$p_value) |
                         r$canonical_table$p_value > 0.1))
  expect_match(fs$description, "score_min=0.5", fixed = TRUE)
})

test_that("Fixture F3 - pathway, sender, receiver and self filters", {
  r <- .comm_result_big()
  fs_pw <- communication_apply_filters(r, list(pathways = "IL7 signaling"))
  expect_true(all(fs_pw$table$pathway == "IL7 signaling"))
  expect_identical(fs_pw$summary$dropped_pathway,
                   sum(is.na(r$canonical_table$pathway) |
                         r$canonical_table$pathway != "IL7 signaling"))

  fs_s <- communication_apply_filters(r, list(senders = "CD4 T"))
  expect_true(all(fs_s$table$sender_node == "CD4 T"))

  # Filtre sur un label SANS correspondance : le noeud coalescent est le
  # label brut (Mono) — la selection reste possible et explicite.
  fs_m <- communication_apply_filters(r, list(senders = "Mono"))
  expect_true(all(fs_m$table$sender == "Mono"))
  expect_identical(nrow(fs_m$table), 1L)

  fs_self <- communication_apply_filters(r, list(include_self = FALSE))
  expect_identical(fs_self$summary$dropped_self, 1L)
  expect_false(any(fs_self$table$sender_node == fs_self$table$receiver_node))
})

test_that("Fixture F4 - combining filters can yield an empty (handled) table", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.99, p_value_max = 0.0001))
  expect_identical(nrow(fs$table), 0L)
  expect_identical(fs$summary$n_after, 0L)
  # Les vues sur une selection vide restent des ggplot avec message.
  p <- plot_communication_dotplot(r, fs$table)
  expect_s3_class(p, "ggplot")
})

# ── Vues : consommatrices pures, gardes de peremption ───────────────────────
test_that("Fixture V1 - dotplot aggregates per node pair and shows score guardrails", {
  r <- .comm_result_big()
  tbl <- r$canonical_table
  p <- plot_communication_dotplot(r, seurat_obj = .comm_obj_stub())
  expect_s3_class(p, "ggplot")
  expect_identical(r$canonical_table, tbl)   # la vue ne mute pas le resultat
  # Sous-titre : methode source + garde-fou d'echelle + non-causalite.
  sub <- paste(p$labels$subtitle, collapse = " ")
  expect_match(sub, "cellchat")
  expect_match(sub, "non comparable")
  expect_match(sub, "aucune causalite", ignore.case = TRUE)
})

test_that("Fixture V2b - heatmap on a source without any pathway shows explicit message", {
  # Source sans aucun pathway (CellPhoneDB) : message explicite, pas un
  # graphe vide — le message est porte dans le sous-titre.
  parsed_cpdb <- parse_cellphonedb_import(.comm_cellphonedb_means())
  r_cpdb <- .comm_import_and_finalize(parsed_cpdb,
                                      identities = c("CD4 T", "B", "CD8 T"),
                                      source_files = list(means = "means.txt"))
  p2 <- plot_communication_pathway_heatmap(r_cpdb)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$title, "Heatmap pathways", fixed = TRUE)
  expect_match(p2$labels$subtitle, "Aucun pathway renseigne", fixed = TRUE)
})

test_that("Fixture V2 - pathway heatmap excludes NA pathways with explicit counts", {
  r <- .comm_result_big()
  p <- plot_communication_pathway_heatmap(r)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "sans pathway", fixed = TRUE)
  expect_match(p$labels$subtitle, "2 sans pathway (exclues, jamais imputees)", fixed = TRUE)
  expect_match(p$labels$subtitle, "7 interaction(s) avec pathway", fixed = TRUE)
})

test_that("Fixture V3 - circle network draws edges minus self, message when nothing drawable", {
  r <- .comm_result_big()
  p <- plot_communication_circle(r, seurat_obj = .comm_obj_stub())
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "1 auto-interaction", fixed = TRUE)
  # Uniquement des auto-interactions : message, pas de crash.
  parsed_self <- parse_cellchat_import(.comm_cellchat_tab())
  r_self <- .comm_import_and_finalize(parsed_self)
  only_self <- r_self$canonical_table
  only_self$receiver <- only_self$sender
  only_self$receiver_mapped <- only_self$sender_mapped
  r_self$canonical_table <- only_self
  p2 <- plot_communication_circle(r_self)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$subtitle, "auto-interaction", fixed = TRUE)
})

test_that("Fixture V4 - stale result is refused by every view", {
  r <- .comm_result_big()
  other <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("x", "y")))
  for (fn in list(plot_communication_dotplot, plot_communication_pathway_heatmap,
                  plot_communication_circle)) {
    e <- tryCatch(fn(r, seurat_obj = other), error = function(e) e)
    expect_identical(communication_error_state(e), "stale_against_current_seurat_object")
  }
})

# ── Centralite : descriptive, niveau reseau ─────────────────────────────────
test_that("Fixture C1 - centrality sums and degrees match the table", {
  r <- .comm_result_big()
  cen <- build_communication_centrality(r)
  tbl <- r$canonical_table
  cd4 <- cen[cen$node == "CD4 T", ]
  expect_identical(cd4$n_out_interactions, sum(tbl$sender == "CD4 T"))
  expect_identical(cd4$n_in_interactions, sum(tbl$receiver == "CD4 T"))
  expect_identical(cd4$out_score_total,
                   sum(tbl$score[tbl$sender == "CD4 T"]))
  expect_true(all(c("analysis_id", "source_method", "identity_column") %in% colnames(cen)))
  expect_identical(cen$analysis_id[1], "sc-communication-import")
  # Trie par poids total decroissant.
  expect_true(all(diff(cen$total_interactions) <= 0))
})

# ── Provenance des filtres + export filtre ──────────────────────────────────
test_that("Fixture P1 - filter provenance entry is produced with frozen parameters", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.5))
  entry <- build_communication_filter_provenance(r, fs)
  expect_identical(entry$analysis_id, "sc-communication-explore")
  expect_identical(entry$method, "explore_cellchat")
  expect_identical(entry$analysis_type, "cell_cell_communication")
  expect_false(isTRUE(entry$import_only))
  expect_match(as.character(entry$parameters$applied_filters), "score_min=0.5", fixed = TRUE)
  expect_identical(entry$parameters$n_rows_before, fs$summary$n_before)
  expect_identical(entry$parameters$n_rows_after, fs$summary$n_after)
  expect_identical(entry$parameters$base_analysis_id, "sc-communication-import")
  # Appending a un etat d'analyse (chemin module reel).
  state <- create_analysis_state("sc")
  provenance_append(state, entry)
  entries <- state_get(state, "provenance")
  expect_identical(length(entries), 1L)
  # Mauvais argument : erreur classee.
  e <- tryCatch(build_communication_filter_provenance(r, list()), error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_input")
})

test_that("Fixture P2 - filtered export carries filters and analysis_id on every row", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(senders = "CD4 T"))
  df <- build_communication_filtered_export(r, fs$table, fs$description)
  expect_identical(nrow(df), nrow(fs$table))
  expect_true(all(df$analysis_id == "sc-communication-import"))
  expect_true(all(df$applied_filters == fs$description))
  expect_match(fs$description, "senders=[CD4 T]", fixed = TRUE)
})
