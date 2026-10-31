# =============================================================================
# test-drive-allowlist-spatial.R — DSTV-2 B1 RED tests (allowlist M3b Spatial)
# =============================================================================
# Mandate: re-measure docs/agent/swarm/round7/DSTV1_REMEASURE_2026-10-31.md
# § DSTV-2·B1; fiche docs/proposals/
# DRIVE_SESSION_TRACKER_SPATIAL_WIRING_PROPOSAL.md:67-69 (B1) and criterion #3
# (:106-107). RED-first — these tests are written AGAINST that scope BEFORE any
# implementation; they MUST fail on the unmodified tree, then pin the B1 gate.
#
# Scope (re-measure B1 row, "Reste à faire" column):
#   1. an entry `spatial` in TS_MCP_SET_INPUT_MODULES (mcp_server.R:1827-1831 —
#      a computed setdiff over TS_DRIVE_MODULES that currently yields no
#      `spatial`, because TS_DRIVE_MODULES does not carry the parent module,
#      drive_allowlist.R:896-902 — while the refusal MODULE_NOT_ALLOWED fires
#      at mcp_server.R:1571-1575);
#   2. the five declared inputs on the APP allowlist table
#      (drive_allowlist.R TS_DRIVE_ALLOWLIST);
#   3. the vocabulary publication for `color_by` (no spatial key exists in
#      TS_DRIVE_VOCABULARY_KEYS, drive_allowlist.R:1106-1112 — measured);
#   4. the frozen refusals conserved (targeted tests + refus conservés).
#
# ── MEASURED IDS — the fiche names resolve to these DOM ids ──────────────────
# House doctrine (drive_allowlist.R:12-14): "MEASURED IDS (G0 discovery,
# mandatory) — Discovered by reading the module sources, never guessed". The
# five fiche names are logical labels; the app's ids are:
#
#   fiche "color_by"        -> `spatial-viz-color_by`   selectInput, STATIC
#                               choices qc/cluster/deconv/niche/gene
#                               (mod_spatial_viz.R:22-30; values re-pushed
#                               verbatim on language change :414-419)
#   fiche "deconv_celltype" -> `spatial-viz-deconv_celltype`  selectInput
#                               rendered by the uiOutput at mod_spatial_viz.R:78
#                               (:963-966), choices = deconv result columns —
#                               session-derived
#   fiche "gene"            -> `spatial-viz-gene`       selectizeInput with
#                               choices = NULL until data (mod_spatial_viz.R:47),
#                               updated server-side (:849) — session-derived
#   fiche "point_size"      -> `spatial-viz-pt_radius`  sliderInput 1-20 step 1
#                               (mod_spatial_viz.R:90; "Taille des points")
#   fiche "point_opacity"   -> `spatial-viz-pt_opacity` sliderInput 0.1-1 step
#                               0.05 (mod_spatial_viz.R:91; "Opacite des points")
#
# The viz module is NESTED (`mod_spatial_viz_ui(ns("viz"))`, mod_spatial.R:197;
# `mod_spatial_viz_server("viz", ...)` :434) inside the top-level
# `mod_spatial_ui("spatial")` (app.R:552), hence the `spatial-viz-` prefix —
# the same convention as `bulk-de-` (module prefix + child id). A grep over
# modules/, R/, scripts/ and tools/pkg_skeleton/R/ measured ZERO ids named
# `point_size`/`point_opacity` (2026-10-31) — declaring those strings would
# allowlist ids that exist nowhere in the DOM, the exact silent no-op the
# doctrine forbids.
#
# ── M3b EXPOSURE DESIGN (the Slice-3 doctrine) ───────────────────────────────
# mcp_server.R:1276-1292 ("NOTHING is 'not exposed' any more" :1759-1767): a
# select whose domain the server cannot statically verify is exposed BY INDEX
# against the app's PUBLISHED vocabulary, never by raw name. Therefore:
#   * `color_by` (STATIC domain, measured :22-30) is exposed as `enum`;
#   * `gene` / `deconv_celltype` (session-derived) are exposed as `index` and
#     enter TS_DRIVE_SESSION_INPUTS (their keys live in the module's published
#     vocabulary — the server consistency check mcp_server.R:2021-2049 refuses
#     any index-typed schema id that is not declared there);
#   * the two sliders are `number` with the MEASURED widget bounds (precedent:
#     `bulk-wgcna-wgcna_n_genes`, schema bounds = slider bounds,
#     mod_bulk_wgcna.R:20).
# The vocabulary publication (scope item 3) declares the `spatial` keep-set in
# TS_DRIVE_VOCABULARY_KEYS: `color_by` (the STATIC declared domain, readable on
# the wire — "les routes « voient » les nouveaux champs uniquement via
# snapshot/state", re-measure B1 row) plus the two session-derived keys.
#
# ── FROZEN PINS THE GREEN STEP MUST WIDEN (dated, visible — house convention) ─
# The re-measure's rail is "refus conservés / frozen tests still pass"; three
# frozen pins PIN THE OLD closed set and are widened by the SAME commit that
# lands B1 (each is the file's own documented widening move):
#   * drive_allowlist.R:1240-1252 — the `spatial-` namespace is a CLOSED set of
#     THREE buttons; its own comment says "Widening it is a visible edit here,
#     never a default". The five input ids join the declared set (inputs, never
#     buttons);
#   * test-drive-allowlist.R:186-190 — "Spatial drive exposure is exactly THREE
#     owned buttons" pins spatial_ids setequal the three buttons; grows to
#     buttons + the five inputs (the file's changelog convention, :127-140);
#   * test-drive-session-vocabulary.R:38-40 / :61-66 — pins
#     TS_DRIVE_SESSION_INPUTS length 6 and the three-module keep-set; grows by
#     the `spatial` entry (Slice-4 precedent :35-36).
# Everything else frozen (self-consistency at source time, the closed-namespace
# refusal for UNdeclared keys, the pipeline-module exclusions from set_inputs)
# must pass UNCHANGED — asserted below.
#
# Sandbox discipline (house style): every drive-rooted test boots its OWN temp
# root, honours TRANSCRIPTO_DRIVE_ROOT (set and restored on exit, precedent
# test-adversarial-m2.R:94-96) and never touches the live tools/_drive/ of the
# working tree; NO Shiny app is booted (no port); the arm fixture is the
# test-mcp-spatial-local.R:109-135 pattern (a WRITE names its session,
# mcp_server.R:1554-1556).
# =============================================================================

# --- Fixtures ----------------------------------------------------------------

.b1_al_source <- function() {
  source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
  # The vocabulary projection lives in the WATCHER (ts_drive_project_vocabulary,
  # drive_watcher.R:1420) — the vocabulary tests exercise it, so the watcher
  # loads beside the allowlist (same bootstrap as
  # test-drive-session-vocabulary.R:3-4).
  watcher_path <- if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
    file.path(ts_project_root(), "R", "core", "drive_watcher.R")
  } else {
    file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R")
  }
  sys.source(watcher_path, envir = globalenv())
}

.b1_mcp_env <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), envir = e)
  watcher_path <- if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
    file.path(ts_project_root(), "R", "core", "drive_watcher.R")
  } else {
    file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R")
  }
  sys.source(watcher_path, envir = e)
  expressions <- parse(path, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R internal definition boundaries were not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)
  e
}

# The five DECLARED inputs, measured (see header). Single source inside this
# file so the ids are pinned exactly once.
.B1_SPATIAL_INPUTS <- c(
  "spatial-viz-color_by",
  "spatial-viz-deconv_celltype",
  "spatial-viz-gene",
  "spatial-viz-pt_radius",
  "spatial-viz-pt_opacity"
)

# The measured color_by domain (mod_spatial_viz.R:24-28 — the VALUES, never the
# i18n labels, C13).
.B1_COLOR_BY_CHOICES <- c("qc", "cluster", "deconv", "niche", "gene")

#' One sandboxed, ARMED drive session for a set_inputs write (the
#' test-mcp-spatial-local.R:109-135 pattern; the live tools/_drive/ is only
#' ever READ to prove it was not touched).
.b1_armed_session <- function(e, token) {
  real_drive <- list.files(file.path(ts_project_root(), "tools", "_drive"), recursive = TRUE)
  root <- tempfile("ts-b1-spatial-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  # The sandbox survives the fixture's RETURN: the cleanup is deferred to the
  # CALLING TEST's frame (withr::defer on parent.frame()). Measured
  # 2026-10-31: an on.exit INSIDE this fixture unlinked the root between the
  # ready.json write and the first assertion, and every set_inputs assertion
  # then answered NO_SESSION — a session unlinked before its assertions is
  # not a sandbox, it is a self-inflicted refusal.
  withr::defer(
    {
      unlink(root, recursive = TRUE, force = TRUE)
      # The live drive dir must survive untouched (same guard as
      # test-mcp-spatial-local.R:118-122).
      expect_setequal(list.files(file.path(ts_project_root(), "tools", "_drive"),
                                 recursive = TRUE), real_drive)
    },
    envir = parent.frame())
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = TRUE, last_seq = 0L,
                         hb_n = 0L, started_at = started)
  e$ts_drive_write_json(
    list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
    e$ts_drive_path("arm.json")
  )
  session_id <- e$.ts_session_id(Sys.getpid(), started, token)
  list(root = root, token = token, started = started,
       expectation = list(session_id = session_id, pid = Sys.getpid(),
                          started_at = started))
}

# =============================================================================
# 1. Scope item 2 — the five declared inputs on the APP allowlist
# =============================================================================

test_that("the five declared Spatial inputs are allowlist entries owned by module 'spatial'", {
  .b1_al_source()
  kinds <- c("select", "select", "select", "numeric", "numeric")
  names(kinds) <- .B1_SPATIAL_INPUTS
  for (id in names(kinds)) {
    entry <- ts_drive_allowlist_get(id)
    expect_false(is.null(entry), info = id)
    expect_identical(entry$kind, unname(kinds[[id]]), info = id)
    expect_identical(entry$module, "spatial", info = id)
    # The scenario validator routes on entry$module — the declared module must
    # agree with the id's own prefix (frozen cross-check,
    # test-drive-allowlist.R:109-118).
    expect_identical(ts_drive_module_of(id), "spatial", info = id)
  }
  # The parent module becomes a member of TS_DRIVE_MODULES (the setdiff at
  # mcp_server.R:1827-1831 then admits it; the source-time consistency check
  # drive_allowlist.R:1169-1171 otherwise refuses every entry).
  expect_true("spatial" %in% TS_DRIVE_MODULES)
  # The fiche's two slider names are NOT ids anywhere in the app (grep
  # measured 0, 2026-10-31) — the declared entries use the MEASURED ids, so a
  # later reader cannot "correct" them back into silent no-ops.
  expect_false("spatial-viz-point_size" %in% names(TS_DRIVE_ALLOWLIST))
  expect_false("spatial-viz-point_opacity" %in% names(TS_DRIVE_ALLOWLIST))
})

# =============================================================================
# 2. Scope item 1 — `spatial` in TS_MCP_SET_INPUT_MODULES; the T1 refusal is
#    retired for the module (fiche criterion #3: the write aboutit)
# =============================================================================

test_that("MODULE_NOT_ALLOWED no longer fires for spatial — the criterion-#3 payload is accepted", {
  e <- .b1_mcp_env()
  # The refusal this test RETIRES, named: mcp_server.R:1571-1575 answers
  # MODULE_NOT_ALLOWED for any module outside TS_MCP_SET_INPUT_MODULES; the
  # re-measure measured that refusal live ×2 (fiche :40). Before B1 the enum
  # below cannot contain "spatial" (drive_allowlist.R:896-902 carries no parent
  # module), so BOTH assertions fail RED.
  expect_true("spatial" %in% e$TS_MCP_SET_INPUT_MODULES)

  s <- .b1_armed_session(e, "b1criterion3")
  # The EXACT fiche criterion-#3 payload (fiche :106-107), whose inputs are
  # keyed by ALLOWLIST ID like every scenario the tool writes (the fiche's
  # {color_by: ...} prose shorthand names the input; the wire key is the
  # measured id, the same convention as "bulk-de-de_engine" in the sc-local
  # tests):
  # set_inputs {module: "spatial", inputs: {"spatial-viz-color_by": "cluster"}}.
  response <- e$.ts_tool_set_inputs(1L, "spatial",
                                    list("spatial-viz-color_by" = "cluster"),
                                    TRUE, s$expectation)
  expect_false(response$isError)
  expect_true(isTRUE(response$structuredContent$accepted))
  expect_identical(response$structuredContent$module, "spatial")
  expect_identical(response$structuredContent$action, "set_inputs")
  expect_setequal(response$structuredContent$input_ids, "spatial-viz-color_by")
  # The write landed (the tool's whole honesty contract: an ACKNOWLEDGEMENT of
  # a write, `applied` stays FALSE until the app confirms).
  expect_false(isTRUE(response$structuredContent$applied))
  scenario <- jsonlite::fromJSON(file.path(s$root, "tools", "_drive", "scenario.json"),
                                 simplifyVector = FALSE)
  expect_identical(scenario$module, "spatial")
  expect_identical(scenario$action, "set_inputs")
})

test_that("set_inputs with no session assertion is still refused — expect.session_id unchanged", {
  e <- .b1_mcp_env()
  s <- .b1_armed_session(e, "b1noassert")
  # Fiche B1: "(+ expect.session_id inchangé)" — a WRITE still names its
  # session (mcp_server.R:1554-1556). B1 widens the module gate, never the
  # assertion discipline.
  response <- e$.ts_tool_set_inputs(1L, "spatial",
                                    list("spatial-viz-color_by" = "cluster"),
                                    TRUE, NULL)
  expect_true(response$isError)
  expect_identical(response$structuredContent$code, "SESSION_ASSERTION_REQUIRED")
})

# =============================================================================
# 3. The five declared inputs pass M3b per-field validation and ONE scenario
#    carries all five (the RED item: "set_inputs accepts module spatial with
#    the 5 declared inputs")
# =============================================================================

test_that("set_inputs accepts module spatial with the five declared inputs", {
  e <- .b1_mcp_env()
  s <- .b1_armed_session(e, "b1fiveinputs")
  # The app has PUBLISHED the session-derived domains (the vocabulary block the
  # index validation reads, mcp_server.R:1506-1546 — .ts_vocabulary_ok reads
  # result.json snapshot.modules.<module>.vocabulary). color_by's static
  # domain needs no publication on the wire; it is pinned by the enum schema.
  e$ts_drive_write_json(
    list(
      snapshot = list(
        modules = list(
          spatial = list(
            vocabulary = list(
              color_by        = .B1_COLOR_BY_CHOICES,
              gene            = c("GENE_A", "GENE_B"),
              deconv_celltype = c("T cell", "B cell"),
              vocab_rev       = 1L
            )
          )
        )
      )
    ),
    e$ts_drive_path("result.json")
  )
  inputs <- list(
    "spatial-viz-color_by"        = "cluster",                      # static enum, raw string
    "spatial-viz-gene"            = list(index = 1L, vocab_rev = 1L),   # session-derived, BY INDEX
    "spatial-viz-deconv_celltype" = list(index = 2L, vocab_rev = 1L),   # session-derived, BY INDEX
    "spatial-viz-pt_radius"       = 8,                              # number, slider 1-20
    "spatial-viz-pt_opacity"      = 0.5                             # number, slider 0.1-1
  )
  response <- e$.ts_tool_set_inputs(2L, "spatial", inputs, TRUE, s$expectation)
  expect_false(response$isError)
  expect_true(isTRUE(response$structuredContent$accepted))
  expect_setequal(response$structuredContent$input_ids, .B1_SPATIAL_INPUTS)
  expect_identical(response$structuredContent$inputs_accepted, 5L)
  scenario <- jsonlite::fromJSON(file.path(s$root, "tools", "_drive", "scenario.json"),
                                 simplifyVector = FALSE)
  # The write carries what was validated, in the shape it was validated
  # (mcp_server.R:1663-1675): a raw string for the enum, INDEX OBJECTS
  # normalised to integers for the session-derived pair.
  expect_identical(scenario$inputs[["spatial-viz-color_by"]], "cluster")
  expect_identical(as.integer(scenario$inputs[["spatial-viz-gene"]]$index), 1L)
  expect_identical(as.integer(scenario$inputs[["spatial-viz-gene"]]$vocab_rev), 1L)
  expect_identical(as.integer(scenario$inputs[["spatial-viz-deconv_celltype"]]$index), 2L)
})

test_that("an out-of-domain value is still refused — the M3b boundary travels with the widening", {
  e <- .b1_mcp_env()
  s <- .b1_armed_session(e, "b1baddomain")
  # color_by = "cluster" is a MEASURED choice (mod_spatial_viz.R:24-28);
  # anything outside the published domain is refused BEFORE the write (the
  # hole M3b exists to close, mcp_server.R:1276-1280).
  response <- e$.ts_tool_set_inputs(1L, "spatial",
                                    list("spatial-viz-color_by" = "rainbow"),
                                    TRUE, s$expectation)
  expect_true(response$isError)
  expect_identical(response$structuredContent$code, "VALUE_REFUSED")
})

# =============================================================================
# 4. Scope item 3 — the vocabulary publication for color_by (module side)
# =============================================================================

test_that("the Spatial vocabulary keep-set declares color_by and the projection keeps it", {
  .b1_al_source()
  # Today TS_DRIVE_VOCABULARY_KEYS carries bulk_de/bulk_pathways/bulk_wgcna and
  # NO spatial key (drive_allowlist.R:1106-1112, re-measured). B1 declares the
  # spatial keep-set: the STATIC color_by domain plus the two session-derived
  # keys the index surface validates against.
  expect_setequal(TS_DRIVE_VOCABULARY_KEYS$spatial,
                  c("color_by", "deconv_celltype", "gene"))
  # The projection (drive_watcher.R:1420-1443) keeps the declared keys VERBATIM
  # (decision D1) and drops everything else — the same closed-keep-set
  # behaviour the bulk modules are pinned under
  # (test-drive-session-vocabulary.R:72-100).
  proj <- ts_drive_project_vocabulary(
    c(list(color_by        = .B1_COLOR_BY_CHOICES,
           deconv_celltype = c("T cell", "B cell"),
           gene            = c("GENE_A", "GENE_B"),
           vocab_rev       = 2L),
      list(sneaky = "classified_data")),
    "spatial")
  expect_identical(as.character(proj$color_by), .B1_COLOR_BY_CHOICES)
  expect_identical(as.character(proj$deconv_celltype), c("T cell", "B cell"))
  expect_identical(as.character(proj$gene), c("GENE_A", "GENE_B"))
  expect_identical(as.integer(proj$vocab_rev), 2L)
  expect_false("sneaky" %in% names(proj))
  # The index members agree with the keep-set (the server's own consistency
  # mirror, mcp_server.R:2037-2049: every index-typed id's key must live in
  # TS_DRIVE_VOCABULARY_KEYS[[module]]).
  expect_true("spatial-viz-gene" %in% names(TS_DRIVE_SESSION_INPUTS))
  expect_true("spatial-viz-deconv_celltype" %in% names(TS_DRIVE_SESSION_INPUTS))
  expect_identical(TS_DRIVE_SESSION_INPUTS[["spatial-viz-gene"]]$module, "spatial")
  expect_identical(TS_DRIVE_SESSION_INPUTS[["spatial-viz-gene"]]$key, "gene")
  expect_identical(TS_DRIVE_SESSION_INPUTS[["spatial-viz-gene"]]$type, "index")
  expect_identical(TS_DRIVE_SESSION_INPUTS[["spatial-viz-deconv_celltype"]]$key,
                   "deconv_celltype")
  expect_identical(TS_DRIVE_SESSION_INPUTS[["spatial-viz-deconv_celltype"]]$type,
                   "index")
})

# =============================================================================
# 5. The M3b value schema — the five inputs exposed with their MEASURED domains
# =============================================================================

test_that("the M3b value schema exposes the five Spatial inputs with their measured domains", {
  e <- .b1_mcp_env()
  sch <- e$TS_MCP_INPUT_SCHEMA
  # color_by: STATIC domain, exposed as enum with the measured choices.
  expect_identical(sch[["spatial-viz-color_by"]]$type, "enum")
  expect_setequal(sch[["spatial-viz-color_by"]]$values, .B1_COLOR_BY_CHOICES)
  # gene / deconv_celltype: session-derived, exposed BY INDEX (Slice-3
  # doctrine, mcp_server.R:1276-1292) — an index entry carries NO static
  # values.
  expect_identical(sch[["spatial-viz-gene"]]$type, "index")
  expect_null(sch[["spatial-viz-gene"]]$values)
  expect_identical(sch[["spatial-viz-deconv_celltype"]]$type, "index")
  # The sliders: numbers with the MEASURED widget bounds (mod_spatial_viz.R:90
  # 1-20 step 1; :91 0.1-1), same convention as bulk-wgcna-wgcna_n_genes.
  expect_identical(sch[["spatial-viz-pt_radius"]]$type, "number")
  expect_identical(as.numeric(sch[["spatial-viz-pt_radius"]]$min), 1)
  expect_identical(as.numeric(sch[["spatial-viz-pt_radius"]]$max), 20)
  expect_true(isTRUE(sch[["spatial-viz-pt_radius"]]$integer))
  expect_identical(sch[["spatial-viz-pt_opacity"]]$type, "number")
  expect_identical(as.numeric(sch[["spatial-viz-pt_opacity"]]$min), 0.1)
  expect_identical(as.numeric(sch[["spatial-viz-pt_opacity"]]$max), 1)
  # The schema-vs-allowlist cross-check stays clean with the five new entries
  # (the drift detector, mcp_server.R:1352-1410) — this is the assertion that
  # forces the app table, the session-input table and the keep-set to land
  # TOGETHER.
  expect_length(e$.ts_mcp_schema_problems(), 0L)
  # The declared tool surface carries the widened module enum.
  tools <- stats::setNames(e$.ts_tools(), vapply(e$.ts_tools(), function(x) x$name, character(1)))
  set_modules <- unlist(tools$transcripto_drive_set_inputs$inputSchema$properties$module$enum)
  expect_true("spatial" %in% set_modules)
})

# =============================================================================
# 6. The frozen refusals conserved (the re-measure's "refus conservés")
# =============================================================================

test_that("the frozen refusals survive the Spatial widening", {
  .b1_al_source()
  # The source-time self-check still holds with the widened table (frozen
  # invariant, test-drive-allowlist.R:45-48).
  expect_true(isTRUE(ts_drive_allowlist_problems()))
  # The closed `spatial-` namespace still refuses every UNdeclared key: only
  # the three buttons and the five declared inputs are in scope (the widened
  # declared set of drive_allowlist.R:1240-1252). A viz sibling that B1 did
  # NOT declare stays out.
  undeclared <- list("spatial-viz-color_palette" = list(kind = "select", module = "spatial"))
  expect_true(any(grepl("out of scope",
                        ts_drive_allowlist_problems(undeclared, character(0)))))
  # The parent-module gate opens for `spatial` ONLY: the pipeline/QC action
  # modules stay OUT of set_inputs (frozen, test-mcp-spatial-local.R:67-68 and
  # the watcher-side refusal drive_watcher.R:2968-2981).
  e <- .b1_mcp_env()
  expect_false("spatial_pipeline" %in% e$TS_MCP_SET_INPUT_MODULES)
  expect_false("spatial_qc" %in% e$TS_MCP_SET_INPUT_MODULES)
  expect_true("spatial" %in% e$TS_MCP_SET_INPUT_MODULES)
})
