# =============================================================================
# helper-communication-fixtures.R — fixtures deterministes communication
# (Stage 11, 4D-1)
# =============================================================================
# Auto-source par testthat avant les test-*.R. NB : ce fichier est source
# AVANT helper-source.R (ordre alphabetique : c < s) — si source_project_file()
# n'existe pas encore, on source helper-source.R soi-meme apres avoir remonte
# a la racine du projet (marqueur app.R). Noms prefixes .comm_* pour eviter
# toute collision entre fichiers de test. Sourcing des contrats requis
# (io_helpers fournit %||%) + sc_velocity.R (velocity_object_fingerprint est
# REUTILISE, jamais duplique) + sc_communication.R.
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .comm_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.comm_root, "app.R"))) break
    .parent <- dirname(.comm_root)
    if (identical(.parent, .comm_root)) break
    .comm_root <- .parent
  }
  sys.source(file.path(.comm_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("R/core/io_helpers.R")   # %||%
source_project_file("R/core/rdata_io.R")     # parse_cellchat_object() accepte .rda
source_project_file("R/core/state.R")
source_project_file("R/core/provenance.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_communication_views.R")

# ── Fixture CellChat : table exportee (subsetCommunication-like) ────────────
.comm_cellchat_tab <- function() {
  data.frame(
    source   = c("CD4 T", "B", "CD4 T"),
    target   = c("B", "CD8 T", "CD8 T"),
    ligand   = c("IL7", "CD40", "CCL5"),
    receptor = c("IL7R", "CD40", "CCR5"),
    prob     = c(0.3, 0.5, 0.2),
    pathway  = c("IL7 signaling", "CD40 signaling", "CCL signaling"),
    stringsAsFactors = FALSE
  )
}

# Table avec colonnes d'origine utiles en plus (groupes ligand/receptor).
.comm_cellchat_tab_groups <- function() {
  tab <- .comm_cellchat_tab()
  tab$ligand.group <- c("Cytokine", "TNF", "Chemokine")
  tab$receptor.group <- c("Cytokine receptor", "TNF receptor", "Chemokine receptor")
  tab
}

# ── Fixture CellPhoneDB : means.txt + pvalues.txt (format v2 large) ─────────
.comm_cellphonedb_means <- function() {
  data.frame(
    id_cp_interaction = c("CII-1", "CII-2", "CII-3"),
    interacting_pair = c("IL7|IL7R", "CD40|CD40", "CCL5|CCR5"),
    partner_a = c("IL7", "CD40", "CCL5"),
    partner_b = c("IL7R", "CD40", "CCR5"),
    gene_a = c("IL7", "CD40", "CCL5"),
    gene_b = c("IL7R", "CD40", "CCR5"),
    receptor_a = c(FALSE, FALSE, FALSE),
    receptor_b = c(TRUE, TRUE, TRUE),
    "CD4 T|B" = c(0.3, 0.0, 0.1),
    "B|CD4 T" = c(0.1, 0.5, 0.0),
    "CD8 T|CD8 T" = c(0.0, 0.0, 0.2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.comm_cellphonedb_pvalues <- function() {
  data.frame(
    id_cp_interaction = c("CII-1", "CII-2", "CII-3"),
    interacting_pair = c("IL7|IL7R", "CD40|CD40", "CCL5|CCR5"),
    partner_a = c("IL7", "CD40", "CCL5"),
    partner_b = c("IL7R", "CD40", "CCR5"),
    gene_a = c("IL7", "CD40", "CCL5"),
    gene_b = c("IL7R", "CD40", "CCR5"),
    "CD4 T|B" = c(0.01, 0.90, 0.20),
    "B|CD4 T" = c(0.20, 0.03, 0.80),
    "CD8 T|CD8 T" = c(0.70, 0.70, 0.02),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Identites Seurat de la colonne choisie (extraites par le module depuis
# obj@meta.data) — le domaine consomme un vecteur, pas un objet Seurat.
.comm_identities <- c("CD4 T", "CD8 T", "B", "NK")

# Objet a dimnames (stub) : seule l'identite (velocity_object_fingerprint)
# est extraite — testabilite hors Shiny, comme les fixtures velocity.
.comm_obj_stub <- function() {
  m <- matrix(0, nrow = 6, ncol = 4,
              dimnames = list(paste0("gene", 1:6),
                              c("c1", "c2", "c3", "c4")))
  m
}

# Table riche pour les vues d'aggregation (Stage 12) : 3 senders x 3
# receivers, pathways partiellement renseignes, auto-interaction, label sans
# correspondance ("Mono").
.comm_cellchat_tab_big <- function() {
  data.frame(
    source   = c("CD4 T", "CD4 T", "CD4 T", "B", "B", "NK", "NK", "NK", "Mono"),
    target   = c("B", "B", "NK", "CD4 T", "NK", "CD4 T", "B", "NK", "B"),
    ligand   = c("IL7", "CCL5", "IL7", "CD40", "TNF", "GZMB", "CCL5", "PTGDS", "S100A8"),
    receptor = c("IL7R", "CCR5", "IL7R", "CD40", "TNFRSF1B", "NKG7", "CCR5", "PTGDR", "TLR4"),
    prob     = c(0.3, 0.5, 0.2, 0.7, 0.4, 0.1, 0.6, 0.8, 0.9),
    p_value  = c(0.01, 0.50, 0.02, 0.30, 0.90, 0.40, 0.05, 0.01, 0.20),
    pathway  = c("IL7 signaling", NA, "IL7 signaling", "CD40 signaling", NA,
                 "Cytotoxicity", "CCL signaling", "PTGDS signaling", "TLR signaling"),
    stringsAsFactors = FALSE
  )
}

# ── Stub objet CellChat (contrat documente : liste avec net$prob/net$pval) ──
# net$prob : array 3D ligands x recepteurs x paires "sender|receiver". Les
# valeurs nulles ne donnent pas de ligne (extraction fidele des non-nuls).
.comm_cellchat_object_stub <- function(with_pval = TRUE) {
  lig <- c("IL7", "CCL5")
  rec <- c("IL7R", "CCR5")
  prs <- c("CD4 T|B", "B|CD4 T")
  prob <- array(c(0.5, 0.0, 0.0, 0.3,   # paire CD4 T|B
                  0.2, 0.4, 0.0, 0.1),  # paire B|CD4 T
                dim = c(2, 2, 2),
                dimnames = list(lig, rec, prs))
  net <- list(prob = prob)
  if (with_pval) {
    net$pval <- array(c(0.01, 0.5, 0.9, 0.02,
                        0.03, 0.2, 0.8, 0.01),
                      dim = c(2, 2, 2),
                      dimnames = list(lig, rec, prs))
  }
  list(net = net)
}

# ── Resultat canonique riche (vues Stage 12) ────────────────────────────────
.comm_result_big <- function() {
  parsed <- parse_cellchat_import(.comm_cellchat_tab_big(),
                                  source_file = "cellchat_big.csv")
  .comm_import_and_finalize(parsed, source_files = list(table = "cellchat_big.csv"))
}

# ── Orchestration miroir du module (sequence d'import Stage 11) ─────────────
# reproduce EXACTEMENT la sequence du module : harmonisation -> QC ->
# finalisation, avertissements fusionnes via extra_warnings.
.comm_import_and_finalize <- function(parsed,
                                      identities = .comm_identities,
                                      identity_column = "cell_type",
                                      source_files = list(table = "import.csv"),
                                      seurat_obj = .comm_obj_stub()) {
  harm <- harmonize_communication_identities(
    parsed$table, identities, identity_column,
    context = "communication import"
  )
  qcr <- communication_import_qc(harm$table)
  warnings_all <- c(parsed$warnings, harm$warnings, qcr$warnings)
  finalize_communication_result(
    canonical_table = qcr$table,
    source_method   = unique(parsed$table$source_method)[1L],
    source_files    = source_files,
    identity_column = identity_column,
    identity_mapping = harm$mapping,
    identity_summary = harm$summary,
    column_mapping  = parsed$column_mapping,
    qc              = qcr$counts,
    n_input_rows    = parsed$n_input_rows,
    seurat_obj      = seurat_obj,
    extra_warnings  = warnings_all
  )
}
