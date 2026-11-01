# =============================================================================
# test-session-checkpoint-restore.R — ASR-2 RED tests (O29, GO option A)
# =============================================================================
# Contract: docs/contracts/SESSION_CHECKPOINT_RESTORE_CONTRACT.md — §5
# (restauration :87-103), §6 (honnêteté), §7 (comportements REFUSÉS), §8 row
# ASR-2. These tests are written BEFORE the implementation (RED-first): they
# MUST fail on the ASR-1 tree because no restore primitive exists, then pin
# the ASR-2 milestone gate.
#
# Scope (ask ASR-2): the RESTORATION primitives appended to the PURE engine
# tools/pkg_skeleton/R/session_checkpoint.R ONLY —
#   * ts_session_checkpoint_restore_plan(): from a VALIDATED checkpoint, the
#     rebind PLAN (never a mutation) — layer-1 objects -> named payload
#     targets, layer-2 business state -> the field map, layer-3 assets ->
#     recoverable-from-fallback resolutions; carries per-target status and
#     the losses vector VERBATIM from ts_session_checkpoint_validate();
#   * ts_session_checkpoint_restore_apply(): executes the rebind into a
#     CALLER-SUPPLIED container (environment or named list — the engine
#     never touches a real `global_data`, C17), skipping the losses honestly,
#     returning what was restored / skipped / lost;
#   * the honest refusal: any validate() refusal (missing/corrupt manifest,
#     schema/env mismatch) returns refused = TRUE with the reasons — NEVER a
#     partial silent restore, NEVER deletion of the checkpoint (contract §6).
#   * the bannière payload: timestamp, dataset scope, restored/skipped/lost
#     counts — computed here, rendered by ASR-3 (contract §5.1).
# Restoration-adjacent UI is ASR-3, retention settings ASR-4, drive ASR-5
# (GIVRE, O29/O31) — none of it here.
#
# Purity rails (house rule, contract §1 portée): NO Shiny, NO reactive, NO
# Seurat load, NO state-container access (C17) — the caller passes the
# target container; apply() is a pure function of (plan, container).
#
# Sandbox discipline (house style, precedent test-session-checkpoint-engine.R):
# every test writes into its own tempfile() store — the project's real
# data_sc/ tree is never touched; no daemon pool, no env-var leakage (the
# store `dir` is passed explicitly, which wins over every TRANSCRIPTO_* knob).
#
# =============================================================================
# OPEN-POINT DECISIONS (contract §10.4/§10.5 — tranchés conservativement par
# ASR-2, each documented here and mirrored in the engine header):
# =============================================================================
# 1. Rebind target naming for the layer-1 objects (fiche couche-1 table :51):
#    the couche-1 names ARE the `global_data` field names, VERBATIM —
#    {sc_obj, bulk_obj, spatial_obj, spatial_datasets} (fiche :51; field
#    definitions app.R:572-590). The plan rebinds each layer-1 payload to the
#    container field of the SAME name (identity map). A layer-1 object whose
#    name is OUTSIDE this frozen vocabulary under an unchanged schema_version
#    means the checkpoint was written by a different engine generation
#    (foreign/corrupt producer — the ASR-1 layout policy bumps schema_version
#    exactly when the payload layout changes, test-session-checkpoint-engine.R
#    §3.3): the plan REFUSES the whole restore (no half-restore, no silent
#    drop), reason naming the target, checkpoint conserved.
# 2. Restore x epochs `global_data` (F08) — contract §5.6 open point:
#    default = BUMP. A restore rebinds sc_obj/bulk_obj: this is exactly the
#    destructive dataset replacement F08 guards (precedent app.R:1289-1291 —
#    "restaurer une session REMPLACE les jeux SC et Bulk. Sans bump d'epoch,
#    les résultats partagés du jeu précédent survivent et le rapport consolidé
#    mélange deux jeux"; .ts_bump_all_epochs() state.R:129-137, also called by
#    confirm_reset app.R:1599). Bumping wrongly merely purges already-stale
#    shared results; preserving wrongly lets results of the pre-restore game
#    survive into the restored session — the worse failure. Opt-out preserved:
#    epoch_action = "preserve" (caller-restores-into-fresh-container case).
#    The bump is EXECUTED at apply time from the container's CURRENT epoch
#    values (the plan cannot know them), semantics identical to
#    .ts_bump_all_epochs: NULL/NA/non-numeric -> 0L, then +1L on both
#    sc_obj_epoch and bulk_obj_epoch (cross-domain, never one without the
#    other, state.R:129-137).
# 3. Asset fallback resolution order (contract §5.3, per asset, first match
#    wins): (a) the recorded path exists NOW -> resolution "as-is" (nothing
#    to re-materialise); (b) path missing AND fallback_source recorded AND
#    the fallback exists NOW -> resolution "fallback": the plan carries
#    rematerialize_from = the fallback source, and the RE-MATERIALISATION
#    ITSELF (the import conversion, seam mod_import_sc.R:924) stays with the
#    module wiring — the pure engine plans it, it never runs the import
#    engine; (c) otherwise -> the asset is LOST: it enters the losses vector
#    (verbatim from validate(), the exact honest-refusal list of §5.3) and
#    apply() skips it. The re-staging of `spatial_reference` from a present
#    reference manifest (§5.4, precedent mod_spatial_deconv_refviz.R:34-45)
#    follows the same per-asset resolution: "as-is" when present, and the
#    staging itself is module wiring. Resolved assets are re-bound as a table
#    under the container field `session_checkpoint_assets`.
# 4. Layer-2 field map (couche 2 -> container): the six frozen top-level
#    names (test-session-checkpoint-engine.R §2, engine :39-45) rebind to
#    container fields of the SAME name — business_state and manual_pipeline
#    exist verbatim as `global_data` fields (DSTV-1 producer,
#    drive_watcher.R:3869-3896 reads global_data$business_state; §8.4
#    publishes global_data$manual_pipeline); pipeline_steps_completed,
#    deconv_props, derived_tables, banners rebind at the same names for the
#    module wiring (post F08/F02, O29). A layer-2 field whose checkpointed
#    payload is NULL (nothing was retained at checkpoint) is planned with
#    status "skipped" — honestly reported, never fabricated as an empty
#    restored value, never counted as a loss.
# 5. Counts convention (bannière payload): counts are computed over restore
#    ITEMS — one per layer-1 object, one per layer-2 field, one per layer-3
#    asset, plus one "obj_epochs" item when the plan bumps (absent under
#    "preserve"). c(restored =, skipped =, lost =) partitions the items;
#    apply() returns the same counts and the banner carries them verbatim —
#    ASR-3 renders, never recomputes (contract §5.1: computed here).
# =============================================================================

# The engine is sourced DIRECTLY (house precedent
# test-session-checkpoint-engine.R:83): single physical home
# tools/pkg_skeleton/R/, one file, no package boot needed for a pure engine.
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")

# -----------------------------------------------------------------------------
# Fixtures — a real ASR-1 checkpoint in a sandbox store, with the three
# layer-3 asset situations the contract §5.3 defines (present / recoverable
# from fallback / lost), and a NULL layer-2 field (nothing retained).
# -----------------------------------------------------------------------------

.scratch_store <- function() {
  d <- tempfile(pattern = "ts_asr2_store_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.restore_ckpt <- function(store, layer1 = NULL, deconv_props = NULL) {
  # present asset: an existing dir (bpcells_dir-like)
  bpc <- tempfile(pattern = "ts_asr2_bpc_"); dir.create(bpc, recursive = TRUE)
  # recoverable asset: missing path, but the original import source exists
  fb <- tempfile(pattern = "ts_asr2_src_"); writeLines("h5", fb)
  layers <- list(
    layer1 = if (is.null(layer1)) list(
      sc_obj      = data.frame(cell = 1:3, cluster = c("a", "a", "b")),
      bulk_obj    = list(counts = matrix(1:4, 2), metadata = data.frame(cond = c("x", "y"))),
      spatial_obj = list(bpcells_dir = bpc, coords = data.frame(x = c(1, 2), y = c(3, 4)))
    ) else layer1,
    layer2 = ts_session_checkpoint_layer2(
      pipeline_steps_completed = c("import" = TRUE, "qc" = TRUE),
      manual_pipeline = list(status = "done", step = "deconv"),
      business_state = list(has_data = TRUE, object_class = "SingleCellExperiment",
                            n_elements = 12, n_clusters = 4, deconv_done = TRUE),
      deconv_props = deconv_props,
      derived_tables = list(props_summary = data.frame(type = c("t1", "t2"), n = c(7, 5))),
      banners = list(welcome = TRUE)
    ),
    layer3 = ts_session_checkpoint_layer3(list(
      bpcells_dir        = list(path = bpc, kind = "dir"),
      reference_manifest = list(path = file.path(bpc, "absent_manifest.rds"),
                                kind = "file", fallback_source = fb),
      sketch_path        = list(path = file.path(bpc, "absent_sketch.rds"),
                                kind = "file")
    ))
  )
  w <- ts_session_checkpoint_write(
    layers, dir = store, trigger = "manual",
    dataset_ids = c("ds1", "ds2"),
    now = as.POSIXct("2026-10-31 12:00:00", tz = "UTC"))
  stopifnot(isTRUE(w$ok))  # the ASR-1 writer must work — failures here are
  # fixture bugs, not ASR-2 assertions
  w
}

.dir_inventory <- function(d) {
  sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE))
}

.fresh_container <- function(sc_epoch = 0L, bulk_epoch = 0L) {
  list(sc_obj = NULL, bulk_obj = NULL, spatial_obj = NULL,
       sc_obj_epoch = sc_epoch, bulk_obj_epoch = bulk_epoch)
}

# -----------------------------------------------------------------------------
# 1. Plan from a validated checkpoint (contract §5.2 — readRDS -> PLAN)
# -----------------------------------------------------------------------------

test_that("restore plan from a validated checkpoint carries the layer-1 rebinds, the layer-2 field map and the layer-3 resolutions — and is a PLAN, not a mutation", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  before <- .dir_inventory(store)
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)

  expect_true(isTRUE(plan$ok))
  expect_false(isTRUE(plan$refused))
  expect_identical(plan$id, ck$id)
  expect_identical(plan$schema_version, 1L)

  # layer-1: identity map, verbatim couche-1 names (decision 1), payloads read
  for (nm in c("sc_obj", "bulk_obj", "spatial_obj")) {
    it <- plan$items[[nm]]
    expect_identical(it$kind, "layer1")
    expect_identical(it$target, nm)
    expect_identical(it$status, "restored")
  }
  # (the payloads must be the ORIGINAL checkpointed objects)
  expect_identical(plan$items$sc_obj$payload,
                   data.frame(cell = 1:3, cluster = c("a", "a", "b")))
  expect_identical(plan$items$bulk_obj$payload$counts, matrix(1:4, 2))
  expect_identical(plan$items$spatial_obj$payload$coords,
                   data.frame(x = c(1, 2), y = c(3, 4)))

  # layer-2 field map: identity over the frozen six (decision 4)
  expect_identical(plan$items[["layer2:business_state"]]$target, "business_state")
  expect_identical(plan$items[["layer2:business_state"]]$payload$object_class,
                   "SingleCellExperiment")
  expect_identical(plan$items[["layer2:manual_pipeline"]]$status, "restored")
  expect_identical(plan$items[["layer2:manual_pipeline"]]$payload$status, "done")
  expect_identical(plan$items[["layer2:derived_tables"]]$status, "restored")
  # NULL payload -> honestly skipped, never fabricated (decision 4)
  expect_identical(plan$items[["layer2:deconv_props"]]$status, "skipped")
  expect_true(nzchar(plan$items[["layer2:deconv_props"]]$reason))

  # layer-3 resolutions in the contract §5.3 order (decision 3)
  a1 <- plan$items[["asset:bpcells_dir"]]
  expect_identical(a1$status, "restored")
  expect_identical(a1$resolution, "as-is")
  a2 <- plan$items[["asset:reference_manifest"]]
  expect_identical(a2$status, "restored")
  expect_identical(a2$resolution, "fallback")
  expect_true(file.exists(a2$rematerialize_from))
  a3 <- plan$items[["asset:sketch_path"]]
  expect_identical(a3$status, "lost")

  # losses vector VERBATIM from validate() (ask ASR-2)
  v <- ts_session_checkpoint_validate(ck$id, dir = store)
  expect_identical(plan$losses, v$losses)
  expect_length(plan$losses, 1L)
  expect_true(grepl("sketch_path", plan$losses, fixed = TRUE))

  # A PLAN, not a mutation: the checkpoint store on disk is untouched
  expect_identical(.dir_inventory(store), before)
})

test_that("restore plan refuses a layer-1 object outside the frozen couche-1 vocabulary (no half-restore, no silent drop)", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store, layer1 = list(
    sc_obj = data.frame(x = 1),
    mystery_obj = list(whatever = "born after schema 1")))
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  expect_false(isTRUE(plan$ok))
  expect_true(isTRUE(plan$refused))
  expect_true(any(grepl("mystery_obj", plan$reasons, fixed = TRUE)))
  expect_null(plan$items)
  expect_null(plan$banner)
})

# -----------------------------------------------------------------------------
# 2. apply() into a caller-supplied container — happy path (contract §5.2)
# -----------------------------------------------------------------------------

test_that("apply() rebinds a plan into a caller-supplied LIST container, skipping losses, and never mutates the caller's original list", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  expect_true(isTRUE(plan$ok))

  gd <- .fresh_container()
  gd_snapshot <- gd
  res <- ts_session_checkpoint_restore_apply(plan, gd)

  expect_true(isTRUE(res$ok))
  expect_false(isTRUE(res$refused))

  out <- res$container
  # layer-1 rebinds, identity names
  expect_identical(out$sc_obj, plan$items$sc_obj$payload)
  expect_identical(out$bulk_obj, plan$items$bulk_obj$payload)
  expect_identical(out$spatial_obj, plan$items$spatial_obj$payload)
  # layer-2 rebinds
  expect_identical(out$business_state$object_class, "SingleCellExperiment")
  expect_identical(out$manual_pipeline$status, "done")
  expect_identical(out$derived_tables$props_summary$n, c(7, 5))
  expect_null(out$deconv_props)  # skipped: never fabricated
  # layer-3: the resolved asset table — as-is + fallback, lost asset ABSENT
  tab <- out$session_checkpoint_assets
  expect_identical(tab$bpcells_dir$resolution, "as-is")
  expect_identical(tab$reference_manifest$resolution, "fallback")
  expect_true(file.exists(tab$reference_manifest$rematerialize_from))
  expect_false("sketch_path" %in% names(tab))
  # what was restored / skipped / lost
  expect_true(all(c("sc_obj", "layer2:business_state", "asset:bpcells_dir")
                  %in% res$restored))
  expect_true("layer2:deconv_props" %in% names(res$skipped))
  expect_identical(res$lost, plan$losses)
  # PURE: the caller's original list was not mutated — the rebinds live in
  # the RETURNED container
  expect_identical(gd, gd_snapshot)
})

test_that("apply() rebinds a plan into a caller-supplied ENVIRONMENT container", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  gd <- new.env(parent = emptyenv())
  gd$sc_obj_epoch <- 0L
  gd$bulk_obj_epoch <- 0L
  res <- ts_session_checkpoint_restore_apply(plan, gd)
  expect_true(isTRUE(res$ok))
  expect_identical(gd$sc_obj, plan$items$sc_obj$payload)
  expect_identical(gd$business_state$has_data, TRUE)
  expect_false("sketch_path" %in% names(gd$session_checkpoint_assets))
})

# -----------------------------------------------------------------------------
# 3. Losses skipped honestly (contract §5.3 refus honnête, §6 honesty table)
# -----------------------------------------------------------------------------

test_that("apply() skips the lost assets honestly: they enter no container field, they are reported verbatim, they are never silent", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  res <- ts_session_checkpoint_restore_apply(plan, .fresh_container())

  expect_identical(res$lost, ts_session_checkpoint_validate(ck$id, dir = store)$losses)
  expect_length(res$lost, 1L)
  expect_true(grepl("sketch_path", res$lost, fixed = TRUE))
  expect_identical(res$counts[["lost"]], 1L)
  expect_identical(res$counts[["skipped"]], 1L)
  # the lost asset is nowhere in the container (skipped, not silently present)
  expect_false("sketch_path" %in% names(res$container$session_checkpoint_assets))
  # the counts partition the items exactly
  expect_identical(as.integer(sum(res$counts)), length(plan$items))
})

# -----------------------------------------------------------------------------
# 4. Honest refusal — corrupt/missing manifest, schema/env mismatch, corrupt
#    layer-1 payload: refused = TRUE with reasons, NO partial restore, the
#    checkpoint is CONSERVED (contract §6.4, §3.3, §7.4)
# -----------------------------------------------------------------------------

test_that("a corrupt manifest is refused with reasons, restores NOTHING, and leaves the checkpoint conserved byte-for-byte", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  man <- file.path(ck$path, "manifest.rds")
  before <- .dir_inventory(store)
  writeBin(as.raw(c(0x58, 0x4f, 0x58, 0x4f)), man)  # corrupt the completeness marker

  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  expect_false(isTRUE(plan$ok))
  expect_true(isTRUE(plan$refused))
  expect_true(length(plan$reasons) >= 1L)
  expect_true(any(grepl("manifest", plan$reasons, ignore.case = TRUE)))
  expect_null(plan$items)
  expect_null(plan$banner)

  # apply() on a refused plan is a NO-OP — no partial silent restore
  gd <- .fresh_container(sc_epoch = 5L, bulk_epoch = 9L)
  gd_snapshot <- gd
  res <- ts_session_checkpoint_restore_apply(plan, gd)
  expect_false(isTRUE(res$ok))
  expect_true(isTRUE(res$refused))
  expect_identical(res$reasons, plan$reasons)
  expect_identical(res$container, gd_snapshot)  # container untouched
  expect_length(res$restored, 0L)

  # the checkpoint is CONSERVED — the engine has no delete path
  expect_identical(.dir_inventory(store), before)
  expect_equal(as.numeric(file.info(man)$size), 4)  # the corrupting payload
  # itself, not deleted/rewritten by the refused restore
})

test_that("schema and environment mismatches are refused, restore nothing, and conserve the checkpoint (contract §3.3, §7.4)", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  before <- .dir_inventory(store)
  gd <- .fresh_container()
  gd_snapshot <- gd

  p_schema <- ts_session_checkpoint_restore_plan(ck$id, dir = store,
                                                 expect_schema_version = 2L)
  expect_true(isTRUE(p_schema$refused))
  expect_true(any(grepl("schema_version", p_schema$reasons, fixed = TRUE)))
  expect_null(p_schema$banner)

  p_env <- ts_session_checkpoint_restore_plan(
    ck$id, dir = store,
    expect_env = list(r_version_major_minor = "9.9",
                      renv_lock_digest = strrep("ab", 32)))
  expect_true(isTRUE(p_env$refused))
  expect_true(any(grepl("environment mismatch", p_env$reasons, fixed = TRUE)))

  for (p in list(p_schema, p_env)) {
    res <- ts_session_checkpoint_restore_apply(p, gd)
    expect_false(isTRUE(res$ok))
    expect_identical(res$container, gd_snapshot)
  }
  expect_identical(.dir_inventory(store), before)  # conserved
})

test_that("a corrupt layer-1 payload is refused WHOLE (no partial restore) and a missing id is refused honestly", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  before <- .dir_inventory(store)
  writeBin(as.raw(c(0x00, 0x01)), file.path(ck$path, "layer1_bulk_obj.rds"))
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  expect_true(isTRUE(plan$refused))
  expect_true(any(grepl("bulk_obj", plan$reasons, fixed = TRUE)))
  expect_null(plan$items)
  expect_identical(.dir_inventory(store), before)

  plan2 <- ts_session_checkpoint_restore_plan("checkpoint_nope", dir = store)
  expect_true(isTRUE(plan2$refused))
  expect_true(any(grepl("manifest", plan2$reasons, ignore.case = TRUE)))
})

# -----------------------------------------------------------------------------
# 5. Bannière payload — computed here, rendered by ASR-3 (contract §5.1)
# -----------------------------------------------------------------------------

test_that("the plan carries the bannière payload: horodatage, dataset scope, restored/skipped/lost counts", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  b <- plan$banner

  # « Reprendre la session du <horodatage> (n jeux de données) »
  expect_identical(b$timestamp, plan$manifest$timestamp)
  expect_identical(b$dataset_ids, c("ds1", "ds2"))
  expect_identical(b$n_datasets, 2L)
  expect_identical(b$trigger, "manual")
  expect_identical(b$epoch_action, "bump")
  expect_setequal(names(b$counts), c("restored", "skipped", "lost"))
  # counts partition the items exactly (decision 5)
  statuses <- vapply(plan$items, function(it) it$status, character(1))
  expect_identical(b$counts[["restored"]], sum(statuses == "restored"))
  expect_identical(b$counts[["skipped"]], sum(statuses == "skipped"))
  expect_identical(b$counts[["lost"]], sum(statuses == "lost"))
  expect_identical(b$losses, plan$losses)
  # apply() reports the SAME counts — ASR-3 renders, never recomputes
  res <- ts_session_checkpoint_restore_apply(plan, .fresh_container())
  expect_identical(res$counts, b$counts)
})

# -----------------------------------------------------------------------------
# 6. Restore x epochs (F08) — the bump-vs-preserve decision, applied
#    (contract §5.6 open point, decision 2)
# -----------------------------------------------------------------------------

test_that("the epoch decision applies: default BUMP on both domains at apply time, opt-out preserve, F08 semantics on empty epochs", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)

  # default: bump — executed from the CONTAINER's current values
  plan <- ts_session_checkpoint_restore_plan(ck$id, dir = store)
  expect_identical(plan$epoch$action, "bump")
  expect_setequal(plan$epoch$targets, c("sc_obj_epoch", "bulk_obj_epoch"))
  res <- ts_session_checkpoint_restore_apply(
    plan, .fresh_container(sc_epoch = 2L, bulk_epoch = 7L))
  expect_identical(res$container$sc_obj_epoch, 3L)
  expect_identical(res$container$bulk_obj_epoch, 8L)

  # F08 semantics on empty/absent epochs: 0L then +1L (state.R:129-137)
  res0 <- ts_session_checkpoint_restore_apply(plan, .fresh_container())
  expect_identical(res0$container$sc_obj_epoch, 1L)
  expect_identical(res0$container$bulk_obj_epoch, 1L)

  # opt-out: preserve — no obj_epochs item, epochs untouched
  plan_p <- ts_session_checkpoint_restore_plan(ck$id, dir = store,
                                               epoch_action = "preserve")
  expect_identical(plan_p$epoch$action, "preserve")
  expect_false("obj_epochs" %in% names(plan_p$items))
  res_p <- ts_session_checkpoint_restore_apply(
    plan_p, .fresh_container(sc_epoch = 4L, bulk_epoch = 6L))
  expect_identical(res_p$container$sc_obj_epoch, 4L)
  expect_identical(res_p$container$bulk_obj_epoch, 6L)

  # the bump is CROSS-DOMAIN by construction (state.R:129-137): one item only
  expect_identical(plan$items$obj_epochs$kind, "epoch")
  expect_identical(plan$items$obj_epochs$status, "restored")
})

# -----------------------------------------------------------------------------
# 7. Conservation by construction + honest apply() refusals
# -----------------------------------------------------------------------------

test_that("the restore primitives never delete, never time, never write back and never touch a state container — ASR-1's grep guard extended to the new functions (contract §6, §6.2, §7)", {
  # Structural, not promised: the forbidden behaviours of contract §6/§7 are
  # made impossible BY CONSTRUCTION in the two restore primitives — same
  # grep-guard form as test-session-checkpoint-engine.R (whose whole-file
  # code-line guards ALSO cover these functions, the additions living in the
  # same physical engine file).
  for (fn in list(ts_session_checkpoint_restore_plan,
                  ts_session_checkpoint_restore_apply)) {
    code <- paste(deparse(fn), collapse = "\n")
    # §7 no-deletion: the restore side contains NO removal primitive at all —
    # conservation of the checkpoint is structural.
    expect_length(grep("unlink|file\\.remove|file\\.delete|dir\\.remove", code), 0L)
    # §6.2 no-timer: the ASR-1 banned token set, verbatim
    # (test-session-checkpoint-engine.R "event-driven only").
    expect_length(grep(paste0("invalidateLater|later::|\\blater\\(|reactivePoll|",
                              "setTimer|scheduleTask|Sys\\.sleep|eventReactive"),
                       code, perl = TRUE), 0L)
    # Purity rails (C17, house rule): no Shiny, no state container, no Seurat.
    expect_length(grep("global_data|shared_rv|shiny|reactive|Seurat",
                       code, ignore.case = TRUE), 0L)
    # No silent write-back: the restore side never calls the write/retention
    # machinery — plan() is read-only over the store, apply() a pure rebind.
    expect_length(grep("checkpoint_write|atomic_write|retention", code,
                       ignore.case = TRUE), 0L)
  }
})

test_that("apply() refuses honestly on a refused/invalid plan or an unusable container — the container is never half-touched", {
  store <- .scratch_store()
  ck <- .restore_ckpt(store)
  before <- .dir_inventory(store)
  gd <- .fresh_container(sc_epoch = 1L, bulk_epoch = 1L)
  gd_snapshot <- gd

  bad_plan <- list(ok = FALSE, refused = TRUE, reasons = "boom")
  res <- ts_session_checkpoint_restore_apply(bad_plan, gd)
  expect_false(isTRUE(res$ok))
  expect_true(isTRUE(res$refused))
  expect_identical(res$reasons, "boom")
  expect_identical(res$container, gd_snapshot)

  res2 <- ts_session_checkpoint_restore_apply(list(nothing = TRUE), gd)
  expect_true(isTRUE(res2$refused))
  expect_identical(res2$container, gd_snapshot)

  # never throws: an unusable container yields a structured honest refusal
  res3 <- ts_session_checkpoint_restore_apply(
    ts_session_checkpoint_restore_plan(ck$id, dir = store), "not-a-container")
  expect_true(isTRUE(res3$refused))

  # ...and none of these refusals touched the store
  expect_identical(.dir_inventory(store), before)
})
