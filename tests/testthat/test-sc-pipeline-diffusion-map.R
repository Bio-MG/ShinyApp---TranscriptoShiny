# =============================================================================
# test-sc-pipeline-diffusion-map.R — F03 (revue externe 2026-10-28)
# =============================================================================
# `mod_sc_pipeline.R` (pipeline MANUEL) appelait :
#     DiffusionMap(t(Seurat::Embeddings(obj, "pca")[, 1:pca_dim]))
#
# `Embeddings()` rend deja une matrice CELLULES x DIMENSIONS. `destiny`, lui,
# attend les ECHANTILLONS EN LIGNES et les variables en COLONNES (son aide :
# « vars: Variables (columns) of the data »). Le `t()` inversait donc les deux
# axes : la carte etait calculee sur `pca_dim` « echantillons » (= 20 colonnes
# de PCA) au lieu des n cellules.
#
# 🔴 L'orientation n'est PAS une convention qu'on peut deviner : elle est
# MESUREE ci-dessous sur le paquet installe (entree 6x3 => 6 lignes
# d'eigenvectors). C'est ce qui a tranche le « I » de la revue.
#
# Effet en cascade : `CreateDimReducObject()` recevait 20 lignes pour un objet
# de n cellules => erreur => l'observer mourait AVANT `global_data$sc_obj <- obj`
# (unique site d'ecriture, fin de fichier) => le run ENTIER etait perdu.
# =============================================================================

source_project_file("modules/sc/mod_sc_pipeline.R")

.DM_FILE <- "modules/sc/mod_sc_pipeline.R"

test_that("F03 : la carte de diffusion garde les CELLULES en lignes", {
  skip_if_not_installed("destiny")

  set.seed(1)
  # 40 cellules x 6 composantes : l'orientation reelle de `Embeddings()`.
  emb <- matrix(stats::rnorm(40 * 6), nrow = 40, ncol = 6)
  rownames(emb) <- paste0("cell_", seq_len(40))

  dm <- .dm_from_pca(emb, npcs = 6)

  # Le temoin d'orientation : destiny rend une ligne par ligne d'entree.
  # Si le helper transposait, on obtiendrait 6 lignes au lieu de 40.
  expect_identical(nrow(dm), nrow(emb))
  expect_identical(rownames(dm), rownames(emb))
})

test_that("F03 : `npcs` borne les colonnes utilisees, sans transposer", {
  skip_if_not_installed("destiny")

  set.seed(2)
  emb <- matrix(stats::rnorm(30 * 10), nrow = 30, ncol = 10)
  rownames(emb) <- paste0("cell_", seq_len(30))

  # npcs superieur au nombre de colonnes disponibles : borne, pas d'erreur.
  dm <- .dm_from_pca(emb, npcs = 50)
  expect_identical(nrow(dm), 30L)
})

test_that("F03 : le chemin 'dm' ne transpose plus les embeddings (verrou source)", {
  p     <- ts_ast_parse(.DM_FILE)
  calls <- ts_ast_find_all(p, function(x) TRUE)

  # Anti-vacuite : un garde qui ne visite rien passerait au vert a vide.
  expect_true(length(calls) > 0L)

  d <- vapply(calls, ts_ast_deparse, character(1))

  # La forme fautive exacte : DiffusionMap(t(...)).
  expect_identical(sum(grepl("DiffusionMap(t(", d, fixed = TRUE)), 0L)

  # Le chemin passe bien par le helper unique (pas un appel nu ailleurs).
  expect_true(sum(grepl(".dm_from_pca(", d, fixed = TRUE)) >= 2L)
})
