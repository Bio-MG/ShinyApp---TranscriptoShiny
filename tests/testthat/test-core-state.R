# =============================================================================
# test-core-state.R — contrat d'etat R/core/state.R (CHRYSALIS 2A)
# =============================================================================
# Point cle couvert ici : les accesseurs state_get()/state_set()/state_has()
# doivent fonctionner HORS contexte reactif (scripts/tests) SANS casser la
# trace de dependance DANS l'app (test d'invalidation via compteur).

source_project_file("R/core/state.R")

# ---------------------------------------------------------------------------
# Fabriques de schema domaine (consolidation existante)
# ---------------------------------------------------------------------------
test_that("create_sc_shared_state exposes the documented SC schema", {
  s <- create_sc_shared_state()
  expect_s3_class(s, "reactivevalues")
  expect_null(state_get(s, "markers_data"))
  expect_null(state_get(s, "correlated_genes"))
  expect_null(state_get(s, "pathway_results"))
  expect_null(state_get(s, "traj_reduction"))
  expect_identical(state_get(s, "selected_genes"), character(0))
  expect_identical(state_get(s, "traj_genes"), character(0))
  expect_identical(state_get(s, "report_viz_list"), list())
  expect_identical(state_get(s, "max_cells_heavy"), Inf)
  expect_identical(state_get(s, "sc_palette"), "default")
})

test_that("create_spatial_shared_state exposes the spatial cache schema", {
  s <- create_spatial_shared_state()
  expect_s3_class(s, "reactivevalues")
  for (f in c("qc_metrics", "qc_params", "cluster_labels", "cluster_params",
              "deconv_props", "moran_results", "niche_labels", "umap_df")) {
    expect_null(state_get(s, f), info = f)
  }
  expect_identical(state_get(s, "saved_viz_list"), list())
})

# ---------------------------------------------------------------------------
# new_analysis_state() — contrat 5 emplacements (2A)
# ---------------------------------------------------------------------------
test_that("new_analysis_state creates the 5-slot contract for every domain", {
  for (d in c("sc", "bulk", "spatial")) {
    s <- new_analysis_state(d)
    expect_s3_class(s, "reactivevalues")
    expect_identical(attr(s, "domain"), d)
    for (slot in c("input", "preprocessing", "results", "visualization")) {
      expect_null(state_get(s, slot), info = paste(d, slot))
    }
    expect_identical(state_get(s, "provenance"), list())
  }
})

test_that("new_analysis_state defaults to the 'sc' domain and rejects unknown ones", {
  expect_identical(attr(new_analysis_state(), "domain"), "sc")
  expect_error(new_analysis_state("virology"), "domaine")
})

# ---------------------------------------------------------------------------
# Accesseurs state_get()/state_set()/state_has()
# ---------------------------------------------------------------------------
test_that("state_set/state_get round-trip works OUTSIDE a reactive context", {
  s <- new_analysis_state()
  expect_invisible(state_set(s, "results", "payload"))
  expect_identical(state_get(s, "results"), "payload")
})

test_that("state_get returns NULL for a field never set", {
  s <- new_analysis_state()
  expect_null(state_get(s, "no_such_field"))
  expect_false(state_has(s, "no_such_field"))
})

test_that("state_has distinguishes NULL-initialized from set fields", {
  s <- new_analysis_state()
  expect_false(state_has(s, "input"))          # schema NULL = pas encore calcule
  state_set(s, "input", list(a = 1))
  expect_true(state_has(s, "input"))
  # Un character(0)/list() vide compte comme present (valeur non-NULL)
  state_set(s, "results", character(0))
  expect_true(state_has(s, "results"))
})

test_that("state_set can add a field not pre-declared in the schema (dynamic add)", {
  s <- create_sc_shared_state()
  state_set(s, "qc_snapshot", list(n_cells = 10))
  expect_identical(state_get(s, "qc_snapshot"), list(n_cells = 10))
})

test_that("state_get PRESERVES reactive dependency tracking inside the app", {
  s <- create_sc_shared_state()
  counter <- new.env(parent = emptyenv())
  counter$evals <- 0L
  p <- shiny::reactive({
    counter$evals <- counter$evals + 1L
    state_get(s, "sc_palette")
  })
  expect_identical(shiny::isolate(p()), "default")
  shiny::isolate(p())                            # cache : pas de re-evaluation
  expect_identical(counter$evals, 1L)
  shiny::isolate(state_set(s, "sc_palette", "manual"))
  expect_identical(shiny::isolate(p()), "manual") # invalide puis re-evalue
  expect_identical(counter$evals, 2L)
})

# ---------------------------------------------------------------------------
# sc_state.R reste un re-export fin de la definition canonique
# ---------------------------------------------------------------------------
test_that("sourcing R/sc/sc_state.R standalone keeps the canonical factory", {
  source_project_file("R/sc/sc_state.R")   # no-op si deja charge (guard)
  s <- create_sc_shared_state()
  expect_s3_class(s, "reactivevalues")
  expect_identical(state_get(s, "sc_palette"), "default")
})
