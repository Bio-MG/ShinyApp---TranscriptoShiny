# =============================================================================
# test-session-checkpoint-engine.R — ASR-1 RED tests (O29, GO option A)
# =============================================================================
# Contract: docs/contracts/SESSION_CHECKPOINT_RESTORE_CONTRACT.md (RED-first —
# these tests are written AGAINST that contract BEFORE any implementation; they
# MUST fail on the unmodified tree because the engine file does not exist, then
# pin the ASR-1 milestone gate, contract §8 row ASR-1).
#
# Scope (contract §2-§3, ask ASR-1): the PURE checkpoint engine
# tools/pkg_skeleton/R/session_checkpoint.R ONLY — 3-layer payload assembly,
# manifest, atomic temp+rename+verify write, read/validate primitives,
# retention hooks (N per dataset + disk cap), event-driven trigger surface.
# Restoration itself is ASR-2, the UI toggle/banner is ASR-3, retention
# settings UI is ASR-4, the drive integration is ASR-5 (GIVRE, O29/O31).
#
# Purity rails (house rule, contract §1 portée): NO Shiny, NO reactive, NO
# Seurat load, NO direct state-container access (C17) — the engine receives
# its payloads as plain function arguments and never touches `global_data`.
#
# Sandbox discipline (house style, precedent test-drive-session-tracker.R:20-34):
#   * every test writes into its own tempdir() store — the project's real
#     data_sc/ tree is never touched (contract §2 only forbids tempdir() as
#     the PRODUCTION checkpoint store; using temp dirs in TESTS is the house
#     sandbox rule, contract §9 premise note);
#   * TRANSCRIPTO_* overrides are set with withr::local_envvar and restored
#     on exit;
#   * no daemon pool is booted: the mirai seam is probed via an injectable
#     pool_probe (deterministic headless), the real pool wiring belongs to
#     the import/pipeline points de pose (post F08/F02, O29).
#
# Open-point decisions encoded below (contract §10.5 + ask — tranchés
# conservativement, each documented; these are the implementer's targets):
#   §2 directory — ts_session_checkpoint_dir() resolution order:
#     (1) explicit `dir` argument (tests / programmatic callers);
#     (2) TRANSCRIPTO_CHECKPOINT_DIR env var (NEW knob, single dir);
#     (3) FIRST entry of the existing TRANSCRIPTO_DRIVE_DATA_DIR operator
#         data-dir knob (path-sep separated) + "/session_checkpoints";
#     (4) <project root>/data_sc/session_checkpoints (project root = app.R
#         walk-up, same marker as tests helper-source.R). Never tempdir()
#         alone (contract §2).
#   §2 file naming — one DIRECTORY per checkpoint:
#     checkpoint_<YYYYMMDDThhmmssZ>_<3 random bytes hex>/ containing
#     layer1_<name>.rds (ONE file per object, contract §2 table),
#     layer2_business_state.rds, layer3_assets.rds and manifest.rds written
#     LAST (completeness marker: a directory without a verified manifest is
#     an aborted write, never offered).
#   §3.3 schema_version — initial value = integer constant 1L
#     (TS_SESSION_CHECKPOINT_SCHEMA_VERSION). Bump policy: +1 only on an
#     incompatible payload-layout change; additive fields with honest
#     defaults do NOT bump (same philosophy as the DSTV-1 additive state
#     fields, test-drive-session-tracker.R §8.1).
#   §3.4 disk cap default — 4 GiB = 4294967296 bytes
#     (TS_SESSION_CHECKPOINT_DISK_CAP_BYTES), overridable per call. Large
#     enough for the N=3-per-dataset regime on typical GB-scale objects,
#     small enough to bound the disk. Purge is oldest-first and NEVER removes
#     the newest checkpoint of any dataset scope; when the cap is
#     unsatisfiable the newest is kept and cap_satisfied = FALSE is reported
#     honestly.
#   §2/fiche:56-57 layer-2 field list — frozen top-level names:
#     pipeline_steps_completed, manual_pipeline (status/step, DSTV-1
#     producer), business_state (the FIVE fiche-frozen scalars verbatim —
#     has_data, object_class, n_elements, n_clusters, deconv_done,
#     drive_watcher.R:3869-3896), deconv_props, derived_tables, banners.
#   §3.1 trigger vocabulary — frozen set {import_complete,
#     pipeline_step_complete, deconv_complete, manual}; an unknown trigger is
#     REFUSED (no silent vocabulary drift, DSTV precedent §6.5). The precise
#     per-module points de pose stay with the module wiring jalon (post
#     F08/F02, contract §3.1 open point, O29).
#   §6.2/§7.1 mirai seam — ts_session_checkpoint_write_async() is the
#     daemon entry point: it dispatches through a mirai daemon pool when one
#     is active and REFUSES HONESTLY (no silent main-thread fallback,
#     contract §7.1) when there is none. The synchronous
#     ts_session_checkpoint_write() is the pure primitive the daemon executes.
#     Pool boot stays with the app layer (spatial_async precedent); the seam
#     test below asserts the refusal path headlessly via an injectable probe.
# =============================================================================

# The engine is sourced DIRECTLY (house precedent
# test-drive-session-tracker.R:70-80): single physical home
# tools/pkg_skeleton/R/, one file, no package boot needed for a pure engine.
# RED: this source() fails while the engine file does not exist — every test
# below fails until the engine lands.
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")

.engine_file <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R",
                          "session_checkpoint.R")

.engine_code_lines <- function() {
  # Code lines only: comments are excluded so documentation can MENTION the
  # refused behaviours (the contract §6 table must be quotable in comments)
  # without weakening a code-level assertion (same rule as C17,
  # tools/check_conventions.R:1373-1375 "Les commentaires ne sont pas du code").
  lines <- readLines(.engine_file, warn = FALSE)
  lines[!grepl("^\\s*#", lines)]
}

.scratch_store <- function() {
  d <- tempfile(pattern = "ts_asr1_store_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.tiny_layers <- function(marker = "a") {
  list(
    layer1 = list(small_obj = data.frame(x = 1:3, y = c(marker, marker, marker))),
    layer2 = ts_session_checkpoint_layer2(
      pipeline_steps_completed = c("import" = TRUE),
      business_state = list(has_data = TRUE, object_class = "Seurat",
                            n_elements = 3, n_clusters = 1, deconv_done = FALSE)
    ),
    layer3 = ts_session_checkpoint_layer3(list())
  )
}

# -----------------------------------------------------------------------------
# Layer 2 / layer 3 packagers — the frozen payload shapes (contract §2)
# -----------------------------------------------------------------------------

test_that("layer-2 packager returns the frozen field list with the five fiche-frozen business_state scalars and honest defaults", {
  l2 <- ts_session_checkpoint_layer2()
  expect_setequal(
    names(l2),
    c("pipeline_steps_completed", "manual_pipeline", "business_state",
      "deconv_props", "derived_tables", "banners")
  )
  expect_setequal(
    names(l2$business_state),
    c("has_data", "object_class", "n_elements", "n_clusters", "deconv_done")
  )
  expect_false(l2$business_state$has_data)
  expect_false(l2$business_state$deconv_done)
  expect_null(l2$business_state$object_class)
  # Malformed input never throws — the honest-shape rule (DSTV-1 precedent,
  # ts_drive_feed_business_state drive_watcher.R:3930-3947).
  l2_bad <- ts_session_checkpoint_layer2(
    business_state = list(has_data = "yes", n_elements = NA, object_class = c())
  )
  expect_false(l2_bad$business_state$has_data)
  expect_null(l2_bad$business_state$n_elements)
})

test_that("layer-3 packager records each asset path WITH its existence at build time (fiche :66-67)", {
  existing <- tempfile(pattern = "ts_asr1_asset_")
  dir.create(existing)
  l3 <- ts_session_checkpoint_layer3(list(
    bpcells_dir = list(path = existing, kind = "dir"),
    ghost_dir   = list(path = file.path(existing, "nope"), kind = "dir")
  ))
  expect_true(l3$assets$bpcells_dir$existed_at_checkpoint)
  expect_false(l3$assets$ghost_dir$existed_at_checkpoint)
  expect_identical(l3$assets$bpcells_dir$kind, "dir")
})

# -----------------------------------------------------------------------------
# Write side — 3-layer assembly, manifest, atomic publish (contract §2-§3)
# -----------------------------------------------------------------------------

test_that("3-layer assembly writes the three layers TOGETHER, one RDS per heavy object, with a complete manifest", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(
    .tiny_layers(), dir = store, trigger = "import_complete",
    dataset_ids = "dsA", now = as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  )
  expect_true(res$ok)
  cp <- res$path
  expect_true(dir.exists(cp))
  # Layer 1: one file per object (contract §2 table).
  expect_setequal(
    list.files(cp, pattern = "^layer1_"),
    "layer1_small_obj.rds"
  )
  # Layers 2, 3 and the manifest are all present in the same checkpoint.
  expect_true(file.exists(file.path(cp, "layer2_business_state.rds")))
  expect_true(file.exists(file.path(cp, "layer3_assets.rds")))
  expect_true(file.exists(file.path(cp, "manifest.rds")))
  # Manifest completeness (contract §3.3 frozen list).
  man <- res$manifest
  expect_identical(man$schema_version, TS_SESSION_CHECKPOINT_SCHEMA_VERSION)
  expect_identical(man$trigger, "import_complete")
  expect_identical(man$dataset_ids, "dsA")
  expect_true(grepl("^2026-11-01T12:00:", man$timestamp))
  expect_true(is.numeric(man$layer1_sizes$small_obj) && man$layer1_sizes$small_obj > 0)
  expect_identical(man$total_bytes, res$bytes_total)
  expect_true(nzchar(man$r_version))
  # Read-back: the heavy object round-trips byte-perfect through RDS.
  back <- readRDS(file.path(cp, "layer1_small_obj.rds"))
  expect_identical(back, .tiny_layers()$layer1$small_obj)
  l2back <- readRDS(file.path(cp, "layer2_business_state.rds"))
  expect_true(l2back$business_state$has_data)
  expect_identical(l2back$pipeline_steps_completed, c("import" = TRUE))
})

test_that("hostile layer-1 object names are refused and publish nothing (path safety)", {
  store <- .scratch_store()
  layers <- .tiny_layers()
  layers$layer1[["../escape"]] <- data.frame(x = 1)
  res <- ts_session_checkpoint_write(layers, dir = store, trigger = "manual",
                                     now = as.POSIXct("2026-11-01 12:00:01", tz = "UTC"))
  expect_false(res$ok)
  expect_length(list.files(store), 0)  # nothing published outside the rules
})

test_that("a checkpoint missing any of the three layers is refused — couches 1+2+3 ensemble (contract §2)", {
  store <- .scratch_store()
  layers <- .tiny_layers()
  layers$layer3 <- NULL
  res <- ts_session_checkpoint_write(layers, dir = store, trigger = "manual")
  expect_false(res$ok)
  expect_true(length(res$reasons) >= 1)
  expect_length(list.files(store), 0)
})

test_that("the trigger vocabulary is frozen: an unknown trigger is refused (no silent drift)", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store,
                                     trigger = "every_60_seconds")
  expect_false(res$ok)
  expect_length(list.files(store), 0)
})

test_that("publish is all-or-nothing: an unusable store refuses and leaves NOTHING behind", {
  store_file <- tempfile(pattern = "ts_asr1_notadir_")
  writeLines("not a directory", store_file)
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store_file,
                                     trigger = "manual")
  expect_false(res$ok)
  expect_true(file.exists(store_file))  # untouched
})

test_that("checkpoint ids are unique even for two writes in the same second", {
  store <- .scratch_store()
  t0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  r1 <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual", now = t0)
  r2 <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual", now = t0)
  expect_true(r1$ok && r2$ok)
  expect_false(identical(r1$id, r2$id))
  expect_false(identical(r1$path, r2$path))
})

# -----------------------------------------------------------------------------
# Atomic write discipline — temp + rename + byte-level VERIFY (contract §3.2)
# -----------------------------------------------------------------------------

test_that("atomic byte write round-trips and its verify DETECTS on-disk corruption (leçon scenario.json)", {
  d <- tempfile(pattern = "ts_asr1_atomic_")
  dir.create(d)
  bytes <- serialize(list(x = 1:5, s = "payload"), connection = NULL)
  dest <- file.path(d, "payload.rds")
  res <- ts_session_atomic_write_bytes(bytes, dest)
  expect_true(res$ok)
  expect_identical(readBin(dest, "raw", n = length(bytes)), bytes)
  expect_true(ts_session_verify_bytes(dest, bytes))
  # Simulate external corruption: flip ONE byte on disk behind the engine's
  # back (bitrot / interrupted rename), then the verify must refuse it.
  tampered <- bytes
  tampered[10] <- as.raw(as.integer(tampered[10]) + 1)
  con <- file(dest, open = "wb"); writeBin(tampered, con); close(con)
  expect_false(ts_session_verify_bytes(dest, bytes))
})

test_that("a failed atomic write leaves no tmp behind and does not create the destination", {
  d <- tempfile(pattern = "ts_asr1_missing_parent_")
  dest <- file.path(d, "payload.rds")  # parent d does NOT exist
  bytes <- serialize(1:3, connection = NULL)
  res <- ts_session_atomic_write_bytes(bytes, dest)
  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_false(file.exists(dest))
  expect_false(dir.exists(d))
})

test_that("a corrupted manifest is refused honestly and the checkpoint is CONSERVED (contract §3.3)", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual")
  expect_true(res$ok)
  man_path <- file.path(res$path, "manifest.rds")
  con <- file(man_path, open = "wb"); writeBin(as.raw(c(0x00, 0x58, 0x0a)), con); close(con)
  v <- ts_session_checkpoint_validate(res$id, dir = store)
  expect_false(v$valid)
  expect_true(v$refused)
  expect_true(any(grepl("manifest", v$reasons)))
  expect_true(file.exists(man_path))  # conserved, never deleted
})

test_that("a directory without a manifest is an ABORTED write: never offered, refused at validate", {
  store <- .scratch_store()
  ghost <- file.path(store, "checkpoint_20261101T000000Z_deadbe")
  dir.create(ghost, recursive = TRUE)
  expect_false(any(ghost == file.path(store, ts_session_checkpoint_list(dir = store)$id)))
  v <- ts_session_checkpoint_validate("checkpoint_20261101T000000Z_deadbe", dir = store)
  expect_false(v$valid)
  expect_true(v$refused)
  expect_true(any(grepl("manifest", v$reasons)))
})

# -----------------------------------------------------------------------------
# Read/validate primitives — manifest refusal (contract §3.3, §6.4)
# -----------------------------------------------------------------------------

test_that("schema mismatch is REFUSED with an honest reason and the old checkpoint is conserved", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual")
  expect_true(res$ok)
  v <- ts_session_checkpoint_validate(res$id, dir = store, expect_schema_version = 99L)
  expect_false(v$valid)
  expect_true(v$refused)
  expect_true(any(grepl("schema", v$reasons)))
  expect_true(dir.exists(res$path))  # l'ancien checkpoint est conservé
})

test_that("environment mismatch (R version / renv lock digest) is REFUSED and the checkpoint conserved", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual")
  expect_true(res$ok)
  v_r <- ts_session_checkpoint_validate(res$id, dir = store, expect_env =
    list(r_version_major_minor = "9.9",
         renv_lock_digest = paste0(rep("0", 64), collapse = "")))
  expect_false(v_r$valid)
  expect_true(v_r$refused)
  expect_true(any(grepl("environment|R version", v_r$reasons)))
  v_lock <- ts_session_checkpoint_validate(res$id, dir = store, expect_env =
    list(r_version_major_minor = paste(R.version$major, sub("\\..*", "", R.version$minor), sep = "."),
         renv_lock_digest = paste0(rep("f", 64), collapse = "")))
  expect_false(v_lock$valid)
  expect_true(v_lock$refused)
  expect_true(dir.exists(res$path))
})

test_that("a compatible checkpoint validates clean: valid, not refused, zero reasons", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual")
  v <- ts_session_checkpoint_validate(res$id, dir = store)
  expect_true(v$valid)
  expect_false(v$refused)
  expect_length(v$reasons, 0)
  expect_identical(v$manifest$trigger, "manual")
})

# -----------------------------------------------------------------------------
# Layer-3 asset validation rules (contract §5.3 — report, never silent)
# -----------------------------------------------------------------------------

test_that("layer-3 validation reports present/missing assets and flags a lost asset only when its fallback source is gone too", {
  store <- .scratch_store()
  live_dir <- tempfile(pattern = "ts_asr1_bpcells_")
  dir.create(live_dir)
  src <- tempfile(pattern = "ts_asr1_src_")
  writeLines("h5", src)
  kept <- tempfile(pattern = "ts_asr1_kept_")
  writeLines("mtx", kept)
  layers <- .tiny_layers()
  layers$layer3 <- ts_session_checkpoint_layer3(list(
    bpcells_dir   = list(path = live_dir, kind = "dir"),
    import_source = list(path = src, kind = "file"),
    kept_source   = list(path = kept, kind = "file"),
    vanished      = list(path = tempfile(pattern = "ts_asr1_gone_"), kind = "dir",
                         fallback_source = tempfile(pattern = "ts_asr1_srvgone_"))
  ))
  res <- ts_session_checkpoint_write(layers, dir = store, trigger = "import_complete")
  expect_true(res$ok)
  unlink(live_dir, recursive = TRUE)  # bpcells_dir dies after the checkpoint
  unlink(src)                          # import source dies too
  v <- ts_session_checkpoint_validate(res$id, dir = store)
  expect_identical(v$assets$bpcells_dir$status, "missing")
  expect_false(v$assets$bpcells_dir$recoverable)  # source gone too
  expect_identical(v$assets$kept_source$status, "present")
  expect_true(v$assets$kept_source$recoverable)
  expect_identical(v$assets$import_source$status, "missing")
  # A missing bpcells_dir alone is NOT a refusal — re-materialisation is
  # ASR-2's job (contract §5.3); the validator reports the per-asset status
  # and hands over the exact loss list an honest restore refusal will present.
  expect_true(v$valid)
  expect_false(v$refused)
  expect_length(v$reasons, 0)
  expect_true(any(grepl("vanished", v$losses)))
  expect_true(any(grepl("bpcells_dir", v$losses)))
})

# -----------------------------------------------------------------------------
# Retention hooks — N=3 per dataset + disk cap (contract §3.4)
# -----------------------------------------------------------------------------

test_that("retention keeps the N=3 newest checkpoints per dataset (default), union across datasets", {
  store <- .scratch_store()
  now0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  ids <- character(0)
  for (i in 1:4) {
    r <- ts_session_checkpoint_write(.tiny_layers(), dir = store,
                                     trigger = "pipeline_step_complete",
                                     dataset_ids = "dsA",
                                     now = now0 + i)
    expect_true(r$ok)
    ids[i] <- r$id
  }
  r5 <- ts_session_checkpoint_write(.tiny_layers(), dir = store,
                                    trigger = "pipeline_step_complete",
                                    dataset_ids = "dsB", now = now0 + 5)
  expect_true(r5$ok)
  ids[5] <- r5$id
  # dsA: keep the 3 newest (ids[2:4]); dsB: keep its only checkpoint (ids[5]);
  # ids[1] is the retention casualty.
  lst <- ts_session_checkpoint_list(dir = store)
  expect_false(ids[1] %in% lst$id)
  expect_setequal(ids[2:5], lst$id)
})

test_that("retention N is honoured per DATASET, not globally: two datasets each keep their own 3", {
  store <- .scratch_store()
  now0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  for (i in 1:3) {
    expect_true(ts_session_checkpoint_write(.tiny_layers(), dir = store,
      trigger = "manual", dataset_ids = "dsA", now = now0 + i)$ok)
  }
  for (i in 4:6) {
    expect_true(ts_session_checkpoint_write(.tiny_layers(), dir = store,
      trigger = "manual", dataset_ids = "dsB", now = now0 + i)$ok)
  }
  lst <- ts_session_checkpoint_list(dir = store)
  expect_length(lst$id, 6)  # 3 for dsA + 3 for dsB, nothing purged
})

test_that("the disk cap purges oldest-first but NEVER starves the newest checkpoint of a dataset, and reports an unsatisfiable cap honestly", {
  store <- .scratch_store()
  now0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  big_layers <- list(
    # Random, non-ALTREP payload: 1:20000 would serialize as a compact range
    # (913 B measured) and defeat the purge margins.
    layer1 = list(small_obj = data.frame(x = sample.int(1e6, 20000, TRUE),
                                         y = runif(20000))),
    layer2 = ts_session_checkpoint_layer2(),
    layer3 = ts_session_checkpoint_layer3(list())
  )
  r1 <- ts_session_checkpoint_write(big_layers, dir = store, trigger = "manual",
                                    dataset_ids = "dsA", now = now0 + 1)
  expect_true(r1$ok)
  expect_gte(r1$bytes_total, 100000)  # big payload: deterministic purge margins
  r2 <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual",
                                    dataset_ids = "dsA", now = now0 + 2)
  r3 <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual",
                                    dataset_ids = "dsA", now = now0 + 3)
  expect_true(r2$ok && r3$ok)
  # Cap chosen so the purge removes EXACTLY the oldest (r1): total after the
  # 4th publish exceeds r1$bytes_total + 1000 (three tiny payloads > 1000 B),
  # and after r1 goes the rest is far below the cap.
  r4 <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual",
                                    dataset_ids = "dsA", now = now0 + 4,
                                    disk_cap_bytes = r1$bytes_total + 1000)
  expect_true(r4$ok)
  expect_true(r4$id %in% ts_session_checkpoint_list(dir = store)$id)
  expect_false(r1$id %in% ts_session_checkpoint_list(dir = store)$id)
  expect_true(isTRUE(r4$retention$cap_satisfied))
  expect_length(ts_session_checkpoint_list(dir = store)$id, 3)
  # Cap that even the newest alone exceeds: the newest SURVIVES (protected),
  # the rest are purged, and cap_satisfied = FALSE is reported honestly.
  r_starved <- ts_session_checkpoint_write(.tiny_layers(), dir = store,
                                           trigger = "manual", dataset_ids = "dsA",
                                           now = now0 + 5, disk_cap_bytes = 1)
  expect_true(r_starved$ok)
  expect_false(isTRUE(r_starved$retention$cap_satisfied))
  lst2 <- ts_session_checkpoint_list(dir = store)
  expect_length(lst2$id, 1)
  expect_true(r_starved$id %in% lst2$id)
})

test_that("the public retention hook re-applies N and cap deterministically (ASR-4 seam)", {
  store <- .scratch_store()
  now0 <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  ids <- character(0)
  for (i in 1:3) {
    r <- ts_session_checkpoint_write(.tiny_layers(), dir = store, trigger = "manual",
                                     dataset_ids = "dsA", now = now0 + i,
                                     disk_cap_bytes = Inf)
    expect_true(r$ok)
    ids[i] <- r$id
  }
  lst <- ts_session_checkpoint_list(dir = store)
  expect_length(lst$id, 3)
  total3 <- sum(lst$total_bytes)
  oldest <- lst$id[nrow(lst)]  # newest-first listing -> last row is the oldest
  s_oldest <- lst$total_bytes[nrow(lst)]
  # Exact cap: keep-set total minus the oldest -> exactly one removal, cap met.
  ret <- ts_session_checkpoint_retention(dir = store, n = 3,
                                         cap = total3 - s_oldest)
  expect_setequal(ret$removed, oldest)
  expect_setequal(ret$kept, setdiff(ids, oldest))
  expect_true(isTRUE(ret$cap_satisfied))
  expect_length(ts_session_checkpoint_list(dir = store)$id, 2)
})

# -----------------------------------------------------------------------------
# Trigger primitives — EVENT-DRIVEN ONLY, never a timer (contract §6.2, fiche :68-73)
# -----------------------------------------------------------------------------

test_that("the engine exposes NO timer, scheduler or poller — event-driven calls only (assert no side-effect API)", {
  code <- .engine_code_lines()
  banned <- "invalidateLater|later::|\\blater\\(|reactivePoll|setTimer|scheduleTask|Sys\\.sleep|eventReactive"
  expect_length(grep(banned, code, perl = TRUE, value = TRUE), 0)
  # No timer-ish function is defined by the engine.
  fns <- grep("^ts_session_checkpoint_[A-Za-z0-9_.]+\\s*<-\\s*function", code, perl = TRUE, value = TRUE)
  fns <- sub("^ts_session_checkpoint_([A-Za-z0-9_.]+)\\s*<-.*$", "\\1", fns)
  expect_false(any(grepl("timer|schedule|poll|later|interval|tick", fns, ignore.case = TRUE)))
})

test_that("the mirai seam refuses honestly with no daemon pool — never a silent main-thread fallback (contract §7.1)", {
  store <- .scratch_store()
  res <- ts_session_checkpoint_write_async(.tiny_layers(), dir = store,
                                           trigger = "manual",
                                           pool_probe = function() FALSE)
  expect_false(res$ok)
  expect_true(any(grepl("daemon|mirai|main thread", res$reasons, ignore.case = TRUE)))
  expect_length(list.files(store), 0)  # nothing was written on this thread
  # The default probe is a real function (the app layer wires the actual pool).
  expect_true(is.function(ts_session_checkpoint_daemons_active))
})

# -----------------------------------------------------------------------------
# Toggle-agnostic + purity (contract §4 — the toggle is ASR-3's, not the engine's)
# -----------------------------------------------------------------------------

test_that("the engine knows NOTHING of the toggle and stays pure: no shiny/reactive/toggle/Seurat token in code", {
  code <- .engine_code_lines()
  expect_length(grep("shiny|reactive|toggle|Seurat|global_data|shared_rv", code,
                     perl = TRUE, ignore.case = TRUE, value = TRUE), 0)
})

# -----------------------------------------------------------------------------
# Checkpoint directory resolution — under the data dir, honouring TRANSCRIPTO_*
# (open point §2, decision documented in the header)
# -----------------------------------------------------------------------------

test_that("checkpoint dir resolution honours TRANSCRIPTO_* and the project data dir fallback", {
  tmp1 <- tempfile(pattern = "ts_asr1_env1_"); dir.create(tmp1)
  tmp2 <- tempfile(pattern = "ts_asr1_env2_"); dir.create(tmp2)
  withr::local_envvar(TRANSCRIPTO_CHECKPOINT_DIR = tmp1,
                      TRANSCRIPTO_DRIVE_DATA_DIR = tmp2)
  expect_identical(ts_session_checkpoint_dir(), normalizePath(tmp1, winslash = "/"))
  # Explicit argument wins over everything (sandbox rule).
  tmp3 <- tempfile(pattern = "ts_asr1_explicit_"); dir.create(tmp3)
  expect_identical(ts_session_checkpoint_dir(tmp3), normalizePath(tmp3, winslash = "/"))
  # Without the dedicated knob, the operator's TRANSCRIPTO_DRIVE_DATA_DIR wins.
  withr::local_envvar(TRANSCRIPTO_CHECKPOINT_DIR = "")
  expect_identical(
    ts_session_checkpoint_dir(),
    normalizePath(file.path(tmp2, "session_checkpoints"), winslash = "/", mustWork = FALSE)
  )
  # Last resort: under the project's own data dir (data_sc), never tempdir().
  withr::local_envvar(TRANSCRIPTO_CHECKPOINT_DIR = "", TRANSCRIPTO_DRIVE_DATA_DIR = "")
  fallback <- ts_session_checkpoint_dir()
  expect_true(startsWith(fallback, ts_project_root()))
  expect_true(grepl("data_sc/session_checkpoints$", fallback))
})
