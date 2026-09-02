# =============================================================================
# test-sc-report-contract.R — Rapport consolidé 4F (Stage 17)
# =============================================================================
# Contrat : docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md (figé).
# Le rapport est un COMPILATEUR : ces tests vérifient que le collecteur ne
# fait que LIRE l'état, que le validateur refuse les sections sans
# provenance, que les analyses absentes sont gracieuses et que le bundle
# exporte des copies fidèles — jamais une re-exécution.
# =============================================================================

source_project_file("R/sc/sc_velocity.R")            # empreinte v2 + staleness velocity
source_project_file("R/sc/sc_communication.R")       # staleness communication
source_project_file("R/sc/sc_abundance_design.R")    # design DA (finalizer)
source_project_file("R/sc/sc_abundance_milo.R")      # staleness milo
source_project_file("R/sc/sc_abundance_sccoda.R")    # staleness scCODA
source_project_file("R/reports/report_collector.R")
source_project_file("R/reports/report_validator.R")
source_project_file("R/reports/report_render.R")
source_project_file("R/reports/report_bundle.R")

# ── Fixtures locaux (stub objet commun : 5 genes x 160 cellules) ────────────
.rep_stub_obj <- function() .da_stub_obj()

.rep_velocity_canonical <- function(seurat_obj, ...) {
  velocity_input <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L)
  validated <- .vel_validate_and_enrich(velocity_input)
  finalize_velocity_result(validated = validated, input_mode = "rds",
                           input_files = list(rds = "velocity.rds"),
                           seurat_obj = seurat_obj, requested_reduction = "umap",
                           analysis_id = "sc-velocity", ...)
}

.rep_communication_canonical <- function(seurat_obj) {
  parsed <- parse_cellchat_import(.comm_cellchat_tab(),
                                  source_file = "cellchat_export.csv")
  .comm_import_and_finalize(parsed, seurat_obj = seurat_obj)
}

.rep_design_canonical <- function(seurat_obj) {
  validated <- validate_da_design(metadata = .da_meta(),
                                  sample_id = "sample_id", condition = "condition",
                                  replicate_id = "replicate_id", batch = "batch",
                                  identity = "cell_type")
  finalize_da_design_result(validated = validated, seurat_obj = seurat_obj)
}

# Resultats Milo/scCODA synthetiques — schéma canonique minimal requis par le
# collecteur (champs figés des contrats MILO/SCCODA). Le CALCUL Milo/scCODA
# reel est couvert par les suites 4E ; ici on ne teste que la compilation.
.rep_milo_canonical <- function(seurat_obj) {
  list(
    type = "milo_da", status = "valid", analysis_id = "sc-milo-test",
    tested_contrast = list(target = "B", reference = "A",
                           formula = "~0 + condition",
                           contrast = "conditionB - conditionA",
                           interpretation = "logFC > 0 = enrichi cible"),
    parameters = list(reduction = "umap", seed = 14L),
    neighbourhood_summary = data.frame(
      n_neighbourhoods = 3L, n_cells_in_nhoods = 100L,
      fraction_cells_in_nhoods = 0.6, median_nhood_size = 30,
      min_nhood_size = 20, max_nhood_size = 50),
    DA_table = data.frame(
      Nhood = 1:3, n_cells = c(20L, 30L, 50L), logFC = c(0.5, -0.3, 0.1),
      logCPM = 1, F = 1, PValue = 0.1, FDR = 0.2,
      SpatialFDR = c(0.05, 0.5, 0.8), identity = c("B", "A", NA),
      identity_fraction = 0.9),
    nhood_assignment = NULL,
    sample_composition = data.frame(
      sample = "s1", condition = "A", batch = "b1", n_cells_total = 40L,
      n_cells_in_nhoods = 30L),
    package_versions = list(miloR = "2.2.0"),
    object_identity = list(fingerprint = velocity_object_fingerprint(seurat_obj),
                           method = "v2", seurat_dims = c(5L, 160L)),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "sc-milo-test",
                                      method = "milo", dataset = seurat_obj)
  )
}

.rep_sccoda_canonical <- function(seurat_obj) {
  list(
    type = "sccoda_da", status = "valid", analysis_id = "sc-sccoda-test",
    compositional_unit = "sample", reference_identity = "A",
    credible_effects = c("B"),
    convergence_diagnostics = list(rhat_max = NA, ess_min = 50,
                                   n_divergences = NA, acc_rate = 0.8,
                                   num_results = 20000L, num_burnin = 5000L,
                                   notes = "chaine unique"),
    composition_table = data.frame(sample = "s1", condition = "A",
                                   batch = "b1", A = 30L, B = 10L),
    effect_table = data.frame(
      covariate = "conditionB", identity = "B", effect = 0.4,
      hdi_low = 0.1, hdi_high = 0.7, sd = 0.1, inclusion_probability = 0.99,
      log2_fold_change = 0.5, credible = TRUE, effect_sign_flipped = FALSE),
    parameters = list(fdr_target = 0.05, reference_policy = "explicit"),
    model_specification = list(formula = "~condition", reference_identity = "A"),
    package_versions = list(sccoda = "0.1.9"),
    object_identity = list(fingerprint = velocity_object_fingerprint(seurat_obj),
                           method = "v2", seurat_dims = c(5L, 160L)),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "sc-sccoda-test",
                                      method = "sccoda", dataset = seurat_obj)
  )
}

.rep_full_shared <- function(seurat_obj) {
  shared_rv <- create_sc_shared_state()
  shared_rv$markers_data <- data.frame(
    gene = c("g1", "g2"), cluster = c("1", "2"), avg_log2FC = c(1.1, 0.8))
  shared_rv$pathway_results <- data.frame(
    pathway = c("PATH1", "PATH2"), pval = c(0.01, 0.04))
  shared_rv$pathway_db <- "GOBP"
  shared_rv$traj_method <- "slingshot"
  shared_rv$velocity_result <- .rep_velocity_canonical(seurat_obj)
  shared_rv$communication_result <- .rep_communication_canonical(seurat_obj)
  shared_rv$da_design_result <- .rep_design_canonical(seurat_obj)
  shared_rv$da_milo_result <- .rep_milo_canonical(seurat_obj)
  shared_rv$da_sccoda_result <- .rep_sccoda_canonical(seurat_obj)
  provenance_append(shared_rv,
    new_provenance_entry(analysis_id = "sc-markers-test", method = "FindAllMarkers",
                         dataset = seurat_obj))
  shared_rv
}

.rep_html_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ── 1. Gardes du collecteur ──────────────────────────────────────────────────
test_that("collector refuses a NULL / dim-less object with a classed French error", {
  e <- tryCatch(collect_consolidated_report_input(NULL), error = function(e) e)
  expect_s3_class(e, "report_error")
  expect_match(conditionMessage(e), "aucun objet Single-Cell", ignore.case = TRUE)

  e2 <- tryCatch(collect_consolidated_report_input("pas_un_objet"), error = function(e) e)
  expect_s3_class(e2, "report_error")
})

test_that("collector produces the frozen canonical input shape", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj)
  expect_identical(ri$type, "consolidated_report_input")
  expect_identical(ri$version, "1.0")
  expect_identical(ri$analysis_id, "sc-report-consolide")
  expect_setequal(names(ri$analyses), consolidated_report_analyses())
  expect_identical(ri$dataset$n_cells, 160L)
  expect_identical(ri$dataset$n_genes, 5L)
  # Empreinte v2 REUTILISEE (jamais re-implementee)
  expect_identical(ri$dataset$fingerprint$fingerprint,
                   velocity_object_fingerprint(obj))
  # Provenance vide mais bien typée
  expect_s3_class(ri$provenance_df, "data.frame")
  expect_identical(nrow(ri$provenance_df), 0L)
  expect_true(is.data.frame(ri$config_snapshot))
  expect_true(all(c("TS_DA_MILO_DISPLAY_ALPHA", "TS_REPORT_MAX_TABLE_ROWS")
                  %in% ri$config_snapshot$constante))
})

# ── 2. Projet vide : gracieux, jamais d'erreur technique ────────────────────
test_that("empty project renders every section as gracefully absent", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, create_sc_shared_state())
  val <- validate_consolidated_report_input(ri)
  expect_true(all(vapply(val$verdicts, function(v) v$state == "absent", logical(1))))
  expect_true(val$ok_overall)
  expect_identical(nrow(val$counts), 1L)
  expect_identical(val$counts$etat, "absent")
  expect_identical(val$counts$n_sections, 9L)

  html_path <- file.path(tempdir(), "rep_empty.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  # Message gracieux fige — pas d'erreur technique, pas de contenu fabrique.
  expect_match(txt, "non exécutée pour ce projet", fixed = TRUE)
  expect_match(txt, "sc-report-consolide", fixed = TRUE)
  unlink(html_path)
})

# ── 3. Projet complet : verdicts valides + tracabilite analysis_id ──────────
test_that("full project collects valid verdicts with traceable analysis ids", {
  obj <- .rep_stub_obj()
  shared_rv <- .rep_full_shared(obj)
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)

  states <- vapply(val$verdicts, function(v) v$state, character(1))
  # Legacy presents
  expect_identical(states[["markers"]], "valid_legacy")
  expect_identical(states[["pathways"]], "valid_legacy")
  expect_identical(states[["trajectory"]], "valid_legacy")
  # Domaines a contrat, empreinte fraiche
  expect_identical(states[["velocity"]], "valid")
  expect_identical(states[["communication"]], "valid")
  expect_identical(states[["da_design"]], "valid")
  expect_identical(states[["da_milo"]], "valid")
  expect_identical(states[["da_sccoda"]], "valid")
  # Vues croisées : la vue pure peut échouer sur des résultats synthétiques —
  # le collecteur doit rester gracieux (absent), jamais planter.
  expect_true(states[["da_cross"]] %in% c("absent", "valid"))
  expect_true(val$ok_overall)

  html_path <- file.path(tempdir(), "rep_full.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  expect_match(txt, "sc-velocity", fixed = TRUE)
  expect_match(txt, "sc-communication-import", fixed = TRUE)
  expect_match(txt, "sc-milo-test", fixed = TRUE)
  expect_match(txt, "aucune analyse ré-exécutée", ignore.case = TRUE)
  unlink(html_path)
})

# ── 4. Obsolete : l'objet courant a change depuis le calcul ─────────────────
test_that("stale identity after object change produces an explicit stale banner", {
  old_obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  shared_rv$velocity_result <- .rep_velocity_canonical(.rep_stub_obj())  # empreinte identique
  # L'objet courant CHANGE (une cellule de plus) -> empreinte divergente.
  new_obj <- matrix(0, nrow = 5, ncol = 161,
                    dimnames = list(paste0("gene", 1:5),
                                    c(colnames(.rep_stub_obj()), "cell161")))
  ri <- collect_consolidated_report_input(new_obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$velocity$state, "stale")

  html_path <- file.path(tempdir(), "rep_stale.html")
  write_consolidated_report_html(ri, val, html_path)
  expect_match(.rep_html_text(html_path), "obsolète", fixed = TRUE)
  unlink(html_path)
})

test_that("unverifiable identity (no fingerprint) maps to the unknown state", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  vel <- .rep_velocity_canonical(obj)
  vel$object_identity <- list(fingerprint = NULL, method = "v2")
  shared_rv$velocity_result <- vel
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$velocity$state, "unknown")
})

# ── 5. Provenance absente : section REFUSEE (regle 7) ───────────────────────
test_that("missing provenance blocks the section and is stated in the HTML", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  comm <- .rep_communication_canonical(obj)
  comm$provenance <- NULL
  shared_rv$communication_result <- comm
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$communication$state, "blocked")
  expect_identical(val$blocked_sections, "communication")
  expect_false(val$ok_overall)

  html_path <- file.path(tempdir(), "rep_blocked.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  expect_match(txt, "Section refusée : provenance absente", fixed = TRUE)
  unlink(html_path)
})

test_that("invalid DA design status is flagged invalid, not valid", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  des <- .rep_design_canonical(obj)
  des$status <- "invalid_design"
  shared_rv$da_design_result <- des
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$da_design$state, "invalid")
})

# ── 6. Bundle : copies fideles + manifeste, aucune donnee brute ──────────────
test_that("bundle writes report, manifest, provenance, tables and README", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, .rep_full_shared(obj))
  val <- validate_consolidated_report_input(ri)
  bundle_dir <- file.path(tempdir(), paste0("bundle_", as.integer(Sys.time())))
  on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)
  bundle <- build_report_bundle(bundle_dir, ri, val)

  expect_true(file.exists(bundle$html_path))
  expect_true(file.exists(file.path(bundle_dir, "manifest_sections.csv")))
  expect_true(file.exists(file.path(bundle_dir, "provenance.csv")))
  expect_true(file.exists(file.path(bundle_dir, "README.txt")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "marqueurs.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "communication_canonical.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "milo_da_table.csv")))
  # Tables = copies fideles (meme nombre de lignes que le canonique)
  milo_csv <- utils::read.csv(file.path(bundle_dir, "tables", "milo_da_table.csv"))
  expect_identical(nrow(milo_csv), 3L)
  # Manifeste : une ligne par verdict + fichiers exportes
  manifest <- utils::read.csv(file.path(bundle_dir, "manifest_sections.csv"))
  expect_true(all(consolidated_report_analyses() %in% manifest$section))
  # Aucune matrice brute n'est exportee : nhood_assignment per-cellule exclu.
  expect_false(file.exists(file.path(bundle_dir, "tables",
                                     "milo_nhood_assignment.csv")))
})

# ── 7. Nommage d'export + recap ──────────────────────────────────────────────
test_that("export filename follows the domain pattern", {
  fn <- consolidated_report_export_filename("rapport_consolide", "html")
  expect_match(fn, "^rapport_consolide_sc-report-consolide_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.html$")
})

test_that("recap summarises the collected input without recomputing", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, .rep_full_shared(obj))
  recap <- consolidated_report_input_recap(ri)
  expect_match(recap, "sc-report-consolide")
  expect_match(recap, "velocity")
  expect_match(recap, "provenance", ignore.case = TRUE)
})
