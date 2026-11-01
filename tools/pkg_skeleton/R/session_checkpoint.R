# =============================================================================
# tools/pkg_skeleton/R/session_checkpoint.R — ASR-1 checkpoint engine (O29)
# =============================================================================
# Contract: docs/contracts/SESSION_CHECKPOINT_RESTORE_CONTRACT.md (RED-first,
# O29 DECISIONS.md:106 — GO option A, ASR-1..5; this file is the ASR-1 jalon:
# the PURE engine — 3-layer payload assembly, manifest, atomic temp+rename+
# verify write, read/validate primitives, retention hooks, event-driven
# trigger surface. Restoration is ASR-2, the UI toggle/banner is ASR-3,
# retention settings UI is ASR-4, the drive integration is ASR-5 (GIVRE)).
#
# Purity rails (house rule, contract §1 portée):
#   * NO shiny, NO reactive, NO session — this file must source() anywhere a
#     plain R process runs, including inside a mirai daemon (it DOES: the
#     async seam re-sources this exact file on the daemon side);
#   * NO Seurat load, NO state-container access (C17) — heavy objects arrive
#     as plain function arguments and are stored verbatim; the engine never
#     reads or writes `global_data` (the layer-2 SCALARS are published by the
#     module layer, DSTV-1 producer, and passed in here);
#   * the engine NEVER sets timers, schedulers or pollers (contract §6.2) —
#     every write is a direct call the caller (module point de pose, wired
#     post F08/F02) makes AFTER a pipeline milestone completes.
#
# Atomicity discipline (contract §3.2, leçon scenario.json — the pattern
# temp+rename+verify is an OBLIGATION, drive precedent
# ts_drive_write_attempt() drive_watcher.R:302):
#   * every file is written as: unique tmp -> close -> read-back BYTE compare
#     -> unlink dest -> rename -> read-back BYTE compare at the destination;
#   * the three layers are assembled in a `.building_*` directory and the
#     PUBLISH is a single directory rename — couches 1+2+3 appear together or
#     not at all (contract §2);
#   * manifest.rds is written LAST inside the building directory: a checkpoint
#     directory without a verified manifest is an aborted write, never offered;
#   * ONE verified attempt per file, no retry loop: the drive writer's 8-attempt
#     budget answers IPC contention on a shared file, which a per-checkpoint
#     directory does not have — contention here means a broken store, and an
#     honest refusal beats a blind retry into it. No retry also keeps the
#     engine free of any timing behaviour (contract §6.2).
#
# Layer-2 field list (contract §2, fiche :56-57 + DSTV-1 A2 producer) — frozen
# top-level names: pipeline_steps_completed, manual_pipeline (status/step),
# business_state (the FIVE fiche-frozen scalars verbatim — has_data,
# object_class, n_elements, n_clusters, deconv_done, drive_watcher.R:3869-3896
# — this packager is the engine-side twin of ts_drive_feed_business_state(),
# deliberately re-implemented so the engine stays standalone), deconv_props,
# derived_tables, banners.
#
# Open-point decisions (contract §10.5 — tranchés conservativement, each
# documented in the ASR-1 test header test-session-checkpoint-engine.R):
#   * directory: explicit `dir` arg > TRANSCRIPTO_CHECKPOINT_DIR > first
#     TRANSCRIPTO_DRIVE_DATA_DIR entry + "/session_checkpoints" >
#     <project root>/data_sc/session_checkpoints — NEVER tempdir() alone (§2);
#   * naming: one directory per checkpoint checkpoint_<UTC>_<3 random bytes>,
#     layer1_<name>.rds one file per object (§2 table);
#   * schema_version initial = 1L; bump only on incompatible layout change;
#   * disk cap default = 4 GiB; purge oldest-first, the newest checkpoint of
#     every dataset scope is NEVER removed, an unsatisfiable cap is reported
#     cap_satisfied = FALSE instead of being silently ignored (§3.4);
#   * trigger vocabulary frozen: import_complete, pipeline_step_complete,
#     deconv_complete, manual (§3.1); unknown triggers are REFUSED — the
#     per-module points de pose belong to the module wiring (post F08/F02);
#   * mirai seam: ts_session_checkpoint_write_async() dispatches through a
#     daemon pool and REFUSES honestly (no silent main-thread fallback, §7.1)
#     when none is active; the synchronous ts_session_checkpoint_write() is
#     the pure primitive the daemon executes.
# =============================================================================

TS_SESSION_CHECKPOINT_SCHEMA_VERSION <- 1L
TS_SESSION_CHECKPOINT_RETENTION_N <- 3L
TS_SESSION_CHECKPOINT_DISK_CAP_BYTES <- 4294967296  # 4 GiB (contract §3.4, open point — conservative default)
TS_SESSION_CHECKPOINT_TRIGGERS <- c("import_complete", "pipeline_step_complete",
                                    "deconv_complete", "manual")

# -----------------------------------------------------------------------------
# Small defensive helpers — the engine never throws on bad input, it returns
# structured refusals (house rule: honest refusal beats a thrown condition;
# precedent .ts_drive_read_business_state, drive_watcher.R:3869-3896).
# -----------------------------------------------------------------------------

.sc_default <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.sc_scalar_text <- function(x) {
  v <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
  if (is.na(v) || !nzchar(v)) NULL else v
}

.sc_scalar_num <- function(x) {
  v <- tryCatch(suppressWarnings(as.numeric(x)[1]), error = function(e) NA_real_)
  if (is.na(v) || !is.finite(v)) NULL else v
}

#' Project root by app.R walk-up (same marker as tests/helper-source.R).
#'
#' The engine is self-contained and must resolve the project's data dir
#' without any app bootstrap: walk up from the working directory until app.R
#' is found, fall back to the working directory itself (never NA).
.sc_project_root <- function(start = getwd(), max_up = 8) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, "app.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

#' Resolve the checkpoint store directory (contract §2 — open point, decided).
#'
#' Resolution order: (1) explicit `dir` argument (tests / programmatic
#' callers); (2) `TRANSCRIPTO_CHECKPOINT_DIR` env var (dedicated knob);
#' (3) FIRST entry of the existing `TRANSCRIPTO_DRIVE_DATA_DIR` operator
#' data-dir knob + "/session_checkpoints"; (4) `<project root>/data_sc/
#' session_checkpoints`. Never tempdir() alone (contract §2: tempdir()
#' survives sleep and reload but not an R restart / reboot, fiche :39).
#' The directory is NOT created here — creation happens at write time, so a
#' read-side caller never leaves side effects on disk.
#'
#' @param dir Explicit store directory; wins over everything when non-NULL.
#' @return Absolute path (forward slashes), created on demand by the writer.
ts_session_checkpoint_dir <- function(dir = NULL) {
  if (!is.null(dir) && !identical(.sc_scalar_text(dir), NULL)) {
    return(normalizePath(dir, winslash = "/", mustWork = FALSE))
  }
  env <- Sys.getenv("TRANSCRIPTO_CHECKPOINT_DIR", "")
  if (nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  data_env <- Sys.getenv("TRANSCRIPTO_DRIVE_DATA_DIR", "")
  if (nzchar(data_env)) {
    first <- strsplit(data_env, .Platform$path.sep, fixed = TRUE)[[1]][1]
    if (nzchar(first)) {
      return(normalizePath(file.path(first, "session_checkpoints"),
                           winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(file.path(.sc_project_root(), "data_sc", "session_checkpoints"),
                winslash = "/", mustWork = FALSE)
}

# -----------------------------------------------------------------------------
# Layer packagers — the frozen payload shapes (contract §2)
# -----------------------------------------------------------------------------

#' Package the layer-2 business state (frozen shape, contract §2, fiche :56-57).
#'
#' The caller passes what the module layer published (DSTV-1 producer:
#' `manual_pipeline` status/step scalars and the FIVE `business_state`
#' scalars — has_data, object_class, n_elements, n_clusters, deconv_done,
#' names frozen verbatim — plus the retained deconvolution table, derived
#' tables and conditional banner states). ANY input yields the frozen shape,
#' never throws; unknown scalars stay NULL, flags default FALSE. The engine
#' stores scalars and tables verbatim — it never inspects a Seurat object.
#'
#' @param pipeline_steps_completed Named logical completion flags of pipeline
#'   steps (coerced to logical, names preserved).
#' @param manual_pipeline Named list(status =, step =) as published by DSTV-1.
#' @param business_state Named list of the five fiche-frozen scalars (or NULL).
#' @param deconv_props The retained deconvolution table (any R object / NULL).
#' @param derived_tables Named list of retained derived tables.
#' @param banners Named list of conditional banner states.
#' @return Named list with the six frozen top-level fields, always complete.
ts_session_checkpoint_layer2 <- function(pipeline_steps_completed = character(0),
                                         manual_pipeline = list(status = "idle", step = ""),
                                         business_state = NULL,
                                         deconv_props = NULL,
                                         derived_tables = list(),
                                         banners = list()) {
  # The five fiche-frozen scalars, honest defaults (DSTV-1 A2 shape).
  bs <- list(has_data = FALSE, object_class = NULL, n_elements = NULL,
             n_clusters = NULL, deconv_done = FALSE)
  if (is.list(business_state) && length(business_state) > 0) {
    if (!is.null(business_state$has_data)) bs$has_data <- isTRUE(business_state$has_data)
    oc <- .sc_scalar_text(business_state$object_class)
    if (!is.null(oc)) bs$object_class <- oc
    ne <- .sc_scalar_num(business_state$n_elements)
    if (!is.null(ne)) bs$n_elements <- ne
    nc <- .sc_scalar_num(business_state$n_clusters)
    if (!is.null(nc)) bs$n_clusters <- nc
    if (!is.null(business_state$deconv_done)) bs$deconv_done <- isTRUE(business_state$deconv_done)
  }
  mp <- list(
    status = .sc_default(.sc_scalar_text(manual_pipeline$status), "idle"),
    step = .sc_default(.sc_scalar_text(manual_pipeline$step), "")
  )
  steps <- pipeline_steps_completed
  if (is.null(steps) || length(steps) == 0) {
    steps <- character(0)
  } else if (!is.logical(steps)) {
    steps <- suppressWarnings(as.logical(steps))
  }
  list(
    pipeline_steps_completed = steps,
    manual_pipeline = mp,
    business_state = bs,
    deconv_props = deconv_props,
    derived_tables = if (is.list(derived_tables)) derived_tables else list(),
    banners = if (is.list(banners)) banners else list()
  )
}

#' Package the layer-3 disk-asset paths (frozen shape, contract §2, fiche :64-67).
#'
#' Each asset records its path, its kind (file/dir), its existence AT BUILD
#' TIME (fiche :66-67: "enregistrés avec leur existence au moment du
#' checkpoint") and, when one exists, the source d'import d'origine the
#' restoration can re-materialise from (contract §5.3).
#'
#' @param assets Named list of entry lists: path (required), kind
#'   ("file"/"dir", auto-detected when absent), existed_at_checkpoint
#'   (auto-stamped when absent), fallback_source (optional path string).
#' @return Named list with one field `assets` (named entry lists, complete).
ts_session_checkpoint_layer3 <- function(assets = list()) {
  out <- list()
  for (nm in names(assets)) {
    a <- assets[[nm]]
    if (!is.list(a)) a <- list(path = a)
    p <- .sc_scalar_text(a$path)
    kind <- .sc_scalar_text(a$kind)
    if (is.null(kind)) kind <- if (!is.null(p) && dir.exists(p)) "dir" else "file"
    existed <- if (!is.null(a$existed_at_checkpoint)) isTRUE(a$existed_at_checkpoint)
               else (!is.null(p) && file.exists(p))
    out[[nm]] <- list(
      path = p,
      kind = kind,
      existed_at_checkpoint = existed,
      fallback_source = .sc_scalar_text(a$fallback_source)
    )
  }
  list(assets = out)
}

# -----------------------------------------------------------------------------
# Atomic byte-write discipline (contract §3.2 — temp + rename + VERIFY)
# -----------------------------------------------------------------------------

#' Byte-level read-back verification (leçon scenario.json).
#'
#' Reads `path` back and compares it BYTE FOR BYTE against `bytes` (a raw
#' vector; any other object is serialized first, mirroring the writer).
#' Binary twin of the text normalisation compare in
#' `ts_drive_write_attempt()` (drive_watcher.R:302-345) — the verify is the
#' part of temp+rename+verify that catches a corrupted write, and it runs at
#' BOTH the temporary file and the final destination.
#'
#' @param path File to read back.
#' @param bytes Raw payload (or any R object, serialized for comparison).
#' @return TRUE only on an exact byte-level match.
ts_session_verify_bytes <- function(path, bytes) {
  if (!is.raw(bytes)) bytes <- serialize(bytes, connection = NULL)
  if (!file.exists(path)) return(FALSE)
  back <- tryCatch(
    readBin(path, "raw", n = max(file.info(path)$size, 0L)),
    error = function(e) NULL
  )
  if (is.null(back)) return(FALSE)
  length(back) == length(bytes) && identical(back, bytes)
}

#' One atomic write attempt: unique tmp -> close -> byte-verify -> rename ->
#' byte-verify at the destination (contract §3.2, house obligation).
#'
#' Split from the checkpoint assembly so the discipline has exactly one home
#' and is directly testable (precedent ts_drive_write_attempt(),
#' drive_watcher.R:302). Differences from the drive writer are deliberate and
#' documented in the file header: no retry loop (a per-checkpoint directory
#' has no IPC contention to out-wait, and a retry loop is timing behaviour
#' the engine must not own), and the comparison is byte-level on binary
#' payloads, not text-normalised.
#'
#' `con` is initialised in THIS frame before the attempt (measured rule,
#' drive_watcher.R:313-329: the writer may only close what the writer opened).
#'
#' @param bytes Raw payload (non-raw input is serialized first).
#' @param path Destination path.
#' @return list(ok = logical, error = character or NULL, bytes = length).
ts_session_atomic_write_bytes <- function(bytes, path) {
  con <- NULL
  if (!is.raw(bytes)) bytes <- serialize(bytes, connection = NULL)
  tmp <- sprintf("%s.%d.%s.tmp", path, Sys.getpid(),
                 paste(sprintf("%02x", sample.int(255, 3, TRUE)), collapse = ""))
  warn <- NULL
  ok <- withCallingHandlers(
    tryCatch({
      con <- file(tmp, open = "wb")
      writeBin(bytes, con)
      flush(con)
      close(con)  # close BEFORE the rename (Windows lock, drive_watcher.R:330-334)
      TRUE
    }, error = function(e) {
      if (!is.null(con)) try(close(con), silent = TRUE)
      conditionMessage(e)
    }),
    # file() on an unusable destination warns AND errors; the warning is
    # captured into the recorded error rather than escaping to the caller
    # (the drive writer captures rename warnings for the same reason).
    warning = function(w) {
      warn <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(ok)) {
    if (file.exists(tmp)) suppressWarnings(try(unlink(tmp), silent = TRUE))
    return(list(ok = FALSE,
                error = sprintf("write failed: %s",
                                paste(c(ok, warn), collapse = " | ")),
                bytes = NULL))
  }
  # 1. Read-back verify at the temporary file, BEFORE the rename.
  if (!ts_session_verify_bytes(tmp, bytes)) {
    suppressWarnings(try(unlink(tmp), silent = TRUE))
    return(list(ok = FALSE,
                error = "read-back verification of the temporary file failed",
                bytes = NULL))
  }
  # 2. The destination must be gone first: file.rename() does not overwrite
  #    on Windows (pre-flight, drive_watcher.R:347-354).
  if (file.exists(path)) {
    removed <- suppressWarnings(try(unlink(path), silent = TRUE))
    if (!identical(removed, 0L) && file.exists(path)) {
      suppressWarnings(try(unlink(tmp), silent = TRUE))
      return(list(ok = FALSE,
                  error = "target exists and cannot be removed (locked or read-only)",
                  bytes = NULL))
    }
  }
  # 3. The rename reports failure as a WARNING on Windows — captured, not
  #    muffled away (drive_watcher.R:356-368).
  reason <- NULL
  renamed <- withCallingHandlers(
    tryCatch(file.rename(tmp, path), error = function(e) FALSE),
    warning = function(w) {
      reason <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(renamed)) {
    suppressWarnings(try(unlink(tmp), silent = TRUE))
    return(list(ok = FALSE,
                error = sprintf("rename failed: %s", .sc_default(reason, "file.rename reported failure")),
                bytes = NULL))
  }
  # 4. Read-back verify at the DESTINATION (corruption between tmp and dest
  #    must not survive as a "successful" write).
  if (!ts_session_verify_bytes(path, bytes)) {
    return(list(ok = FALSE,
                error = "read-back verification of the destination failed",
                bytes = NULL))
  }
  list(ok = TRUE, error = NULL, bytes = length(bytes))
}

# -----------------------------------------------------------------------------
# Environment fingerprint (contract §3.3 — R + renv versions)
# -----------------------------------------------------------------------------

#' Environment fingerprint stored in every manifest (contract §3.3).
#'
#' R major.minor plus the sha256 of the project's renv.lock when it exists —
#' the lockfile digest fingerprints the exact package set without calling
#' renv (the engine stays dependency-light and daemon-sourceable).
#'
#' @return Named list(r_version_major_minor =, renv_lock_digest = or NULL).
ts_session_checkpoint_env_fingerprint <- function() {
  lock <- file.path(.sc_project_root(), "renv.lock")
  dig <- NULL
  if (file.exists(lock)) {
    dig <- tryCatch(digest::digest(file = lock, algo = "sha256"),
                    error = function(e) NULL)
  }
  list(
    r_version_major_minor = paste(R.version$major,
                                  sub("\\..*", "", R.version$minor), sep = "."),
    renv_lock_digest = dig
  )
}

.sc_env_refusal_reasons <- function(man_env, expect_env) {
  reasons <- character(0)
  me <- .sc_default(man_env, list())
  ee <- .sc_default(expect_env, list())
  a <- .sc_scalar_text(me$r_version_major_minor)
  b <- .sc_scalar_text(ee$r_version_major_minor)
  if (!is.null(a) && !is.null(b) && !identical(a, b)) {
    reasons <- c(reasons, sprintf(
      "environment mismatch: checkpoint was built with R %s, this session runs R %s",
      a, b))
  }
  la <- .sc_scalar_text(me$renv_lock_digest)
  lb <- .sc_scalar_text(ee$renv_lock_digest)
  if (!is.null(la) && !is.null(lb) && !identical(la, lb)) {
    reasons <- c(reasons, paste0(
      "environment mismatch: renv.lock digest differs between the checkpoint (",
      substr(la, 1, 12), "...) and this session (", substr(lb, 1, 12), "...)",
      " — package sets may be incompatible"))
  }
  reasons
}

# -----------------------------------------------------------------------------
# Write side — 3-layer assembly, manifest, atomic publish (contract §2-§3)
# -----------------------------------------------------------------------------

.sc_id <- function(now) {
  stamp <- format(as.POSIXct(now, tz = "UTC"), "%Y%m%dT%H%M%SZ", tz = "UTC")
  sprintf("checkpoint_%s_%s", stamp,
          paste(sprintf("%02x", sample.int(255, 3, TRUE)), collapse = ""))
}

#' Write one session checkpoint — the three layers TOGETHER, atomically.
#'
#' Assembly + manifest + atomic publish (contract §2-§3):
#'   1. validate the frozen trigger vocabulary and the three layers;
#'   2. build a `.building_<id>/` directory: one verified RDS per layer-1
#'      object, layer2_business_state.rds, layer3_assets.rds, then
#'      manifest.rds LAST (completeness marker);
#'   3. publish with a single directory rename — all three layers appear
#'      together or nothing is published;
#'   4. apply retention (N per dataset, disk cap) as hygiene AFTER the
#'      publish — a retention failure never fails a published checkpoint.
#'
#' The function is the pure synchronous primitive: it runs on WHATEVER thread
#' the caller chose. Writing layer 1 on the main thread of a live app is
#' FORBIDDEN by contract §7.1 — callers must dispatch through
#' [ts_session_checkpoint_write_async()].
#'
#' @param layers Named list with `layer1` (named list of heavy objects — one
#'   RDS file per object; object names must match `^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$`),
#'   `layer2` (see [ts_session_checkpoint_layer2()]) and `layer3` (see
#'   [ts_session_checkpoint_layer3()]). ALL THREE are required (contract §2).
#' @param dir Store directory (default: [ts_session_checkpoint_dir()]).
#' @param trigger Event-driven trigger, one of TS_SESSION_CHECKPOINT_TRIGGERS
#'   (contract §3.1 — checkpoints are posed AFTER a milestone, never by timer).
#' @param dataset_ids Character vector of dataset identifiers this checkpoint
#'   covers (drives the per-dataset retention scope).
#' @param retention_n Keep the N newest checkpoints per dataset (default 3;
#'   invalid values fall back to the default).
#' @param disk_cap_bytes Disk cap in bytes (default 4 GiB; Inf disables).
#' @param now Timestamp override (tests / deterministic replays).
#' @return list(ok = TRUE, id, path, manifest, bytes_total, retention) or
#'   list(ok = FALSE, refused = TRUE, reasons) — never throws.
ts_session_checkpoint_write <- function(layers, dir = NULL, trigger = "manual",
                                        dataset_ids = character(0),
                                        retention_n = TS_SESSION_CHECKPOINT_RETENTION_N,
                                        disk_cap_bytes = TS_SESSION_CHECKPOINT_DISK_CAP_BYTES,
                                        now = Sys.time()) {
  refuse <- function(reasons) {
    list(ok = FALSE, refused = TRUE, reasons = as.character(reasons))
  }
  store <- ts_session_checkpoint_dir(dir)
  trig <- .sc_scalar_text(trigger)
  if (is.null(trig) || !trig %in% TS_SESSION_CHECKPOINT_TRIGGERS) {
    return(refuse(sprintf(
      "unknown trigger '%s' — the vocabulary is frozen (no silent drift): %s",
      trigger, paste(TS_SESSION_CHECKPOINT_TRIGGERS, collapse = ", "))))
  }
  if (!is.list(layers)) {
    return(refuse("layers must be a named list carrying layer1, layer2 and layer3"))
  }
  for (ln in c("layer1", "layer2", "layer3")) {
    if (is.null(layers[[ln]])) {
      return(refuse(sprintf(
        "layer '%s' is missing — the three layers are written TOGETHER, atomically (contract §2)",
        ln)))
    }
  }
  l1 <- layers$layer1
  if (!is.list(l1) || length(l1) == 0 || is.null(names(l1)) || any(!nzchar(names(l1)))) {
    return(refuse("layer1 must be a NON-EMPTY named list of heavy objects (one RDS file per object)"))
  }
  bad <- names(l1)[!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$", names(l1))]
  if (length(bad)) {
    return(refuse(sprintf("unsafe layer-1 object name(s): %s",
                          paste(bad, collapse = ", "))))
  }
  if (!is.list(layers$layer2)) return(refuse("layer2 must be a list (the frozen business-state shape)"))
  if (!is.list(layers$layer3)) return(refuse("layer3 must be a list (asset paths + validation rules)"))
  ids <- as.character(dataset_ids)
  ids <- ids[nzchar(ids)]

  if (!dir.create(store, recursive = TRUE, showWarnings = FALSE) && !dir.exists(store)) {
    return(refuse(sprintf("checkpoint store '%s' cannot be created", store)))
  }
  id <- .sc_id(now)
  final <- file.path(store, id)
  building <- file.path(store, paste0(".building_", id))
  if (dir.exists(building)) unlink(building, recursive = TRUE, force = TRUE)
  if (!dir.create(building, recursive = TRUE, showWarnings = FALSE) || !dir.exists(building)) {
    return(refuse(sprintf("building directory '%s' cannot be created", building)))
  }

  l1_sizes <- list()
  l2_bytes <- 0
  l3_bytes <- 0
  ok_all <- TRUE
  err <- NULL
  for (nm in names(l1)) {
    bytes <- tryCatch(serialize(l1[[nm]], connection = NULL), error = function(e) NULL)
    if (is.null(bytes)) {
      ok_all <- FALSE
      err <- sprintf("layer1 object '%s' cannot be serialized: %s", nm, nm)
      break
    }
    w <- ts_session_atomic_write_bytes(bytes, file.path(building, paste0("layer1_", nm, ".rds")))
    if (!isTRUE(w$ok)) {
      ok_all <- FALSE
      err <- sprintf("layer1 '%s': %s", nm, w$error)
      break
    }
    l1_sizes[[nm]] <- as.numeric(w$bytes)
  }
  if (ok_all) {
    w <- ts_session_atomic_write_bytes(serialize(layers$layer2, connection = NULL),
                                       file.path(building, "layer2_business_state.rds"))
    if (!isTRUE(w$ok)) {
      ok_all <- FALSE
      err <- sprintf("layer2: %s", w$error)
    } else {
      l2_bytes <- as.numeric(w$bytes)
    }
  }
  if (ok_all) {
    # Fill honest existence for layer-3 entries that lack the stamp (fiche :66-67)
    # without overwriting an explicit value recorded at build time.
    l3 <- layers$layer3
    if (is.list(l3$assets)) {
      for (nm in names(l3$assets)) {
        a <- l3$assets[[nm]]
        if (is.list(a) && is.null(a$existed_at_checkpoint) && !is.null(a$path)) {
          a$existed_at_checkpoint <- file.exists(a$path)
          l3$assets[[nm]] <- a
        }
      }
    }
    w <- ts_session_atomic_write_bytes(serialize(l3, connection = NULL),
                                       file.path(building, "layer3_assets.rds"))
    if (!isTRUE(w$ok)) {
      ok_all <- FALSE
      err <- sprintf("layer3: %s", w$error)
    } else {
      l3_bytes <- as.numeric(w$bytes)
    }
  }
  manifest <- NULL
  if (ok_all) {
    manifest <- list(
      schema_version = TS_SESSION_CHECKPOINT_SCHEMA_VERSION,
      timestamp = format(as.POSIXct(now, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      trigger = trig,
      dataset_ids = ids,
      layer1_sizes = l1_sizes,
      layer2_bytes = l2_bytes,
      layer3_bytes = l3_bytes,
      total_bytes = sum(unlist(c(l1_sizes, l2_bytes, l3_bytes))),
      r_version = R.version.string,
      env = ts_session_checkpoint_env_fingerprint(),
      engine = "session_checkpoint.R ASR-1"
    )
    # Manifest LAST: its verified presence is the completeness marker.
    w <- ts_session_atomic_write_bytes(serialize(manifest, connection = NULL),
                                       file.path(building, "manifest.rds"))
    if (!isTRUE(w$ok)) {
      ok_all <- FALSE
      err <- sprintf("manifest: %s", w$error)
    }
  }
  if (!ok_all) {
    unlink(building, recursive = TRUE, force = TRUE)
    return(refuse(err))
  }
  # Publish: one directory rename = the three layers appear TOGETHER.
  renamed <- suppressWarnings(file.rename(building, final))
  if (!isTRUE(renamed)) {
    unlink(building, recursive = TRUE, force = TRUE)
    return(refuse(sprintf("atomic publish (directory rename) failed: %s -> %s",
                          building, final)))
  }
  # Retention hygiene AFTER the publish — never fails a published checkpoint.
  ret <- tryCatch(
    .sc_apply_retention(store, retention_n, disk_cap_bytes),
    error = function(e) list(removed = character(0), kept = character(0),
                             cap_satisfied = NA, bytes_total = 0,
                             error = conditionMessage(e))
  )
  list(ok = TRUE, refused = FALSE, id = id, path = final,
       manifest = manifest, bytes_total = manifest$total_bytes,
       retention = ret)
}

#' The mirai daemon entry point (contract §7.1 — couche 1 par daemon mirai).
#'
#' Dispatches [ts_session_checkpoint_write()] to a mirai daemon pool. When no
#' pool is active the write is REFUSED honestly — a silent main-thread
#' fallback would recreate exactly the UI freeze the M3 daemon exists to
#' avoid (fiche :52-53), so there is none. The engine file is re-sourced
#' inside the daemon (this file is pure R and self-contained).
#'
#' The returned deferred task is collected by the CALLER (mirai collection is
#' event-driven — the engine owns no polling, contract §6.2).
#'
#' @param layers,dir,trigger,dataset_ids,now See [ts_session_checkpoint_write()].
#' @param pool_probe Injectable availability probe (headless tests pass
#'   `function() FALSE`; the default probes the real mirai pool).
#' @return With no pool: list(ok = FALSE, refused = TRUE, reasons). With a
#'   pool: list(ok = TRUE, deferred = <mirai task>, mode = "mirai").
ts_session_checkpoint_write_async <- function(layers, dir = NULL, trigger = "manual",
                                              dataset_ids = character(0),
                                              now = Sys.time(),
                                              pool_probe = ts_session_checkpoint_daemons_active) {
  probe_ok <- tryCatch(isTRUE(pool_probe()), error = function(e) FALSE)
  if (!probe_ok) {
    return(list(
      ok = FALSE, refused = TRUE,
      reasons = c(paste0("no mirai daemon pool is active — refusing to write a ",
                         "checkpoint on the main thread (contract §7.1: couche 1 ",
                         "par daemon mirai uniquement)"))
    ))
  }
  engine_path <- file.path(.sc_project_root(), "tools", "pkg_skeleton", "R",
                           "session_checkpoint.R")
  task <- mirai::mirai(
    {
      source(engine_path, local = TRUE)
      ts_session_checkpoint_write(layers = layers, dir = dir, trigger = trigger,
                                  dataset_ids = dataset_ids, now = now)
    },
    engine_path = engine_path,
    layers = layers, dir = dir, trigger = trigger,
    dataset_ids = dataset_ids, now = now
  )
  list(ok = TRUE, refused = FALSE, deferred = task, mode = "mirai")
}

#' Is a mirai daemon pool currently active? (default probe of the async seam)
#'
#' TRUE only when the mirai package is installed AND a pool is set in this
#' process. The app layer owns pool boot (spatial_async precedent) — the
#' engine only probes, never boots.
ts_session_checkpoint_daemons_active <- function() {
  if (!requireNamespace("mirai", quietly = TRUE)) return(FALSE)
  n <- tryCatch(mirai::daemons(), error = function(e) 0L)
  is.numeric(n) && length(n) == 1L && !is.na(n) && n > 0
}

# -----------------------------------------------------------------------------
# Read / validate primitives (ASR-1 scope — restoration itself is ASR-2)
# -----------------------------------------------------------------------------

#' Read one checkpoint's manifest.
#'
#' @param id Checkpoint directory name (strictly `checkpoint_*`).
#' @param dir Store directory (default: [ts_session_checkpoint_dir()]).
#' @return list(ok = TRUE, manifest =, error = NULL) or
#'   list(ok = FALSE, manifest = NULL, error = reason). Never throws.
ts_session_checkpoint_read_manifest <- function(id, dir = NULL) {
  store <- ts_session_checkpoint_dir(dir)
  idc <- .sc_scalar_text(id)
  if (is.null(idc) || !grepl("^checkpoint_[A-Za-z0-9_-]+$", idc)) {
    return(list(ok = FALSE, manifest = NULL,
                error = sprintf("invalid checkpoint id '%s'", id)))
  }
  p <- file.path(store, idc, "manifest.rds")
  if (!file.exists(p)) {
    return(list(ok = FALSE, manifest = NULL,
                error = "manifest.rds is missing — incomplete or aborted checkpoint (never offered)"))
  }
  m <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(m) || !is.list(m)) {
    return(list(ok = FALSE, manifest = NULL,
                error = "manifest.rds is unreadable (corrupted) — checkpoint conserved, restoration refused"))
  }
  list(ok = TRUE, manifest = m, error = NULL)
}

#' List the published checkpoints of a store (newest first).
#'
#' Only COMPLETE checkpoints (verified manifest present) are offered by
#' default — a directory without one is an aborted write (contract §2:
#' the three layers are published together, or nothing is).
#'
#' @param dir Store directory.
#' @param include_incomplete Also report manifest-less directories
#'   (complete = FALSE) — diagnostic use only.
#' @return data.frame(id, timestamp, trigger, dataset_ids, total_bytes,
#'   complete), 0 rows when the store is empty.
ts_session_checkpoint_list <- function(dir = NULL, include_incomplete = FALSE) {
  store <- ts_session_checkpoint_dir(dir)
  empty <- data.frame(
    id = character(0), timestamp = as.POSIXct(character(0)),
    trigger = character(0), dataset_ids = I(vector("list", 0)),
    total_bytes = numeric(0), complete = logical(0),
    stringsAsFactors = FALSE
  )
  if (!dir.exists(store)) return(empty)
  rows <- list()
  for (d in list.dirs(store, full.names = TRUE, recursive = FALSE)) {
    nm <- basename(d)
    if (!grepl("^checkpoint_[A-Za-z0-9_-]+$", nm)) next  # .building_* etc. excluded
    m <- ts_session_checkpoint_read_manifest(nm, dir = store)
    if (isTRUE(m$ok)) {
      man <- m$manifest
      rows[[length(rows) + 1L]] <- data.frame(
        id = nm,
        timestamp = .sc_parse_manifest_ts(man$timestamp),
        trigger = .sc_default(.sc_scalar_text(man$trigger), NA_character_),
        dataset_ids = I(list(as.character(.sc_default(man$dataset_ids, character(0))))),
        total_bytes = as.numeric(.sc_default(man$total_bytes, 0)),
        complete = TRUE,
        stringsAsFactors = FALSE
      )
    } else if (isTRUE(include_incomplete)) {
      rows[[length(rows) + 1L]] <- data.frame(
        id = nm, timestamp = as.POSIXct(NA), trigger = NA_character_,
        dataset_ids = I(list(character(0))), total_bytes = 0,
        complete = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out[order(out$timestamp, decreasing = TRUE), , drop = FALSE]
}

.sc_parse_manifest_ts <- function(x) {
  v <- .sc_scalar_text(x)
  if (is.null(v)) return(as.POSIXct(NA))
  t <- tryCatch(as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                error = function(e) as.POSIXct(NA))
  if (!is.na(t)) return(t)
  tryCatch(as.POSIXct(v, tz = "UTC"), error = function(e) as.POSIXct(NA))
}

#' Validate one checkpoint WITHOUT restoring it (ASR-1 read/validate surface).
#'
#' Structured verdict, never throws, never deletes:
#'   * manifest missing/corrupt -> refused (the completeness marker is law);
#'   * schema_version mismatch -> refused (contract §3.3 — the OLD checkpoint
#'     is conserved, the engine has no delete path outside retention);
#'   * environment mismatch (R major.minor / renv.lock digest, both sides
#'     KNOWN) -> refused — an unknown on either side is not proof of
#'     incompatibility and is NOT a refusal;
#'   * layer-3 assets are REPORTED per the recorded validation rules
#'     (present / missing / recoverable-from-fallback) — a missing
#'     bpcells_dir is NOT a refusal: re-materialisation is ASR-2's job
#'     (contract §5.3). An asset that is missing AND whose fallback source
#'     is gone too is surfaced in `losses` — the exact list an honest
#'     restore refusal must present (contract §5.3, §6).
#'
#' @param id Checkpoint id.
#' @param dir Store directory.
#' @param expect_schema_version Expected schema version (default: the engine's).
#' @param expect_env Expected environment fingerprint
#'   (default: this session's, via [ts_session_checkpoint_env_fingerprint()]).
#' @return list(valid, refused, reasons, losses, manifest, assets).
ts_session_checkpoint_validate <- function(id, dir = NULL,
                                           expect_schema_version = TS_SESSION_CHECKPOINT_SCHEMA_VERSION,
                                           expect_env = ts_session_checkpoint_env_fingerprint()) {
  store <- ts_session_checkpoint_dir(dir)
  m <- ts_session_checkpoint_read_manifest(id, dir = store)
  if (!isTRUE(m$ok)) {
    return(list(valid = FALSE, refused = TRUE, reasons = m$error, losses = character(0),
                manifest = NULL, assets = NULL))
  }
  man <- m$manifest
  reasons <- character(0)
  if (!identical(man$schema_version, expect_schema_version)) {
    reasons <- c(reasons, sprintf(
      "schema_version mismatch: checkpoint carries %s, this engine expects %s — restoration refused, checkpoint conserved",
      .sc_default(man$schema_version, "NULL"), expect_schema_version))
  }
  reasons <- c(reasons, .sc_env_refusal_reasons(man$env, expect_env))
  if (length(reasons)) {
    return(list(valid = FALSE, refused = TRUE, reasons = reasons, losses = character(0),
                manifest = man, assets = NULL))
  }
  # Layer-3 asset validation rules — report, never silent (contract §5.3).
  assets_report <- list()
  losses <- character(0)
  l3 <- tryCatch(readRDS(file.path(store, .sc_scalar_text(id), "layer3_assets.rds")),
                 error = function(e) NULL)
  if (!is.null(l3) && is.list(l3$assets)) {
    for (nm in names(l3$assets)) {
      a <- l3$assets[[nm]]
      p <- .sc_scalar_text(a$path)
      exists_now <- if (identical(a$kind, "dir")) {
        !is.null(p) && dir.exists(p)
      } else {
        !is.null(p) && file.exists(p)
      }
      status <- if (is.null(p)) "invalid" else if (exists_now) "present" else "missing"
      fb <- .sc_scalar_text(a$fallback_source)
      fb_ok <- if (is.null(fb)) FALSE else file.exists(fb)
      assets_report[[nm]] <- list(
        path = p, kind = a$kind, status = status,
        existed_at_checkpoint = isTRUE(a$existed_at_checkpoint),
        fallback_source = fb, recoverable = status != "missing" || fb_ok
      )
      if (status == "missing" && !fb_ok) {
        losses <- c(losses, sprintf(
          "asset '%s' (%s) is missing and its fallback source is gone too — unrecoverable from this checkpoint",
          nm, .sc_default(p, "?")))
      }
    }
  }
  list(valid = TRUE, refused = FALSE, reasons = character(0), losses = losses,
       manifest = man, assets = assets_report)
}

# -----------------------------------------------------------------------------
# Retention hooks (contract §3.4 — N per dataset + disk cap)
# -----------------------------------------------------------------------------

#' Apply retention to a store: N newest checkpoints per dataset + disk cap.
#'
#' Rules (contract §3.4, decisions documented in the test header):
#'   * scope = the checkpoint's dataset_ids; a checkpoint covering no dataset
#'     forms its own "(no-dataset)" scope so unscoped checkpoints are bounded
#'     too;
#'   * keep set = union over scopes of the N newest checkpoints containing
#'     that scope;
#'   * the disk cap then purges OLDEST-first among the keep set, but the
#'     newest checkpoint of every scope is NEVER removed; when even that
#'     leaves the store above the cap, the survivors are kept and
#'     `cap_satisfied = FALSE` is reported honestly;
#'   * deletion is confined to `checkpoint_*` directories of the store.
#'
#' @param dir Store directory.
#' @param n Keep-N per dataset (invalid -> TS_SESSION_CHECKPOINT_RETENTION_N).
#' @param cap Disk cap in bytes (invalid -> the engine default; Inf disables).
#' @return list(removed, kept, cap_satisfied, bytes_total).
.sc_apply_retention <- function(store, n = TS_SESSION_CHECKPOINT_RETENTION_N,
                                cap = TS_SESSION_CHECKPOINT_DISK_CAP_BYTES) {
  n <- suppressWarnings(as.integer(n)[1])
  if (length(n) != 1L || is.na(n) || n < 1L) n <- TS_SESSION_CHECKPOINT_RETENTION_N
  cap <- suppressWarnings(as.numeric(cap)[1])
  if (length(cap) != 1L || is.na(cap)) cap <- TS_SESSION_CHECKPOINT_DISK_CAP_BYTES

  df <- ts_session_checkpoint_list(dir = store)
  if (nrow(df) == 0L) {
    return(list(removed = character(0), kept = character(0),
                cap_satisfied = TRUE, bytes_total = 0))
  }
  scopes <- lapply(df$dataset_ids, function(ds) {
    ds <- as.character(ds)
    if (length(ds) == 0) "(no-dataset)" else ds
  })
  in_scope <- function(idx, s) s %in% scopes[[idx]]
  newest_of_scope <- function(s) {
    idx <- which(vapply(seq_len(nrow(df)), in_scope, logical(1), s = s))
    idx[order(df$timestamp[idx], decreasing = TRUE)]
  }
  keep <- rep(FALSE, nrow(df))
  protected <- rep(FALSE, nrow(df))
  for (s in unique(unlist(scopes))) {
    idx <- newest_of_scope(s)
    keep[idx[seq_len(min(n, length(idx)))]] <- TRUE
    protected[idx[1]] <- TRUE
  }
  # Disk cap: purge oldest-first among the keep set, newest-per-scope protected.
  total <- sum(suppressWarnings(as.numeric(df$total_bytes[keep])), na.rm = TRUE)
  cap_satisfied <- TRUE
  if (is.finite(cap) && total > cap) {
    cand <- which(keep & !protected)
    cand <- cand[order(df$timestamp[cand])]
    for (i in cand) {
      if (total <= cap) break
      keep[i] <- FALSE
      total <- total - suppressWarnings(as.numeric(df$total_bytes[i]))
    }
    if (total > cap) cap_satisfied <- FALSE
  }
  gone <- which(!keep)
  for (i in gone) {
    suppressWarnings(try(unlink(file.path(store, df$id[i]), recursive = TRUE, force = TRUE),
                         silent = TRUE))
  }
  list(removed = df$id[gone], kept = df$id[keep],
       cap_satisfied = cap_satisfied, bytes_total = total)
}

#' Public retention hook (the write path calls it after each publish).
#'
#' Re-applies N-per-dataset + disk cap to a store at any time — this is the
#' seam ASR-4's settings layer and any caller can invoke directly.
#'
#' @param dir Store directory.
#' @param n,cap See [.sc_apply_retention()].
#' @return list(removed, kept, cap_satisfied, bytes_total).
ts_session_checkpoint_retention <- function(dir = NULL, n = TS_SESSION_CHECKPOINT_RETENTION_N,
                                            cap = TS_SESSION_CHECKPOINT_DISK_CAP_BYTES) {
  .sc_apply_retention(ts_session_checkpoint_dir(dir), n, cap)
}

# =============================================================================
# Restoration primitives (ASR-2 — contract §5 restauration :87-103, §6
# honnêteté, §7 comportements REFUSÉS; jalon §8 row ASR-2). PURE R: no shiny,
# no reactive, no session — the bannière UI that RENDERS the plan payload is
# ASR-3. The engine never touches a real `global_data` (C17): the caller
# passes the target container (environment or named list) to apply().
#
# ASR-2 open-point decisions (contract §10.4/§10.5 — tranchés
# conservativement, each documented in the ASR-2 test header
# test-session-checkpoint-restore.R and summarised here):
#   * layer-1 rebind naming (fiche couche-1 table :51) — the couche-1 names
#     ARE the global_data field names, VERBATIM (identity map): sc_obj,
#     bulk_obj, spatial_obj, spatial_datasets (fiche :51; app.R:572-590). A
#     layer-1 object outside this frozen vocabulary under an unchanged
#     schema_version is a foreign/mislabelled producer: the whole restore is
#     REFUSED (no half-restore, no silent drop) — the ASR-1 layout policy
#     bumps schema_version exactly when the payload layout changes;
#   * restore x epochs (F08, contract §5.6 open point) — default BUMP: a
#     restore rebinds sc_obj/bulk_obj, exactly the destructive dataset
#     replacement F08 guards (precedent app.R:1289-1291 load_session_file,
#     app.R:1599 confirm_reset; .ts_bump_all_epochs() state.R:129-137 —
#     NULL/NA/non-numeric -> 0L then +1L, CROSS-DOMAIN, both epochs together).
#     Bumping wrongly purges already-stale shared results; preserving wrongly
#     lets the pre-restore game's results survive into the restored session
#     (the consolidated report then mixes two games) — the worse failure.
#     Opt-out: epoch_action = "preserve". The bump is EXECUTED at apply time
#     from the container's CURRENT values (the plan cannot know them);
#   * layer-3 asset resolution order (contract §5.3, per asset, first match
#     wins) — (a) path exists NOW -> "as-is"; (b) path missing AND
#     fallback_source exists NOW -> "fallback": the plan carries
#     rematerialize_from and the RE-MATERIALISATION ITSELF (import
#     conversion, seam mod_import_sc.R:924) stays with the module wiring —
#     the pure engine plans it, it never runs the import engine; (c) else the
#     asset is LOST: it enters the losses vector (verbatim from validate(),
#     the §5.3 honest-refusal list) and apply() skips it. The §5.4 re-staging
#     of spatial_reference from a present reference manifest (precedent
#     mod_spatial_deconv_refviz.R:34-45) follows the same per-asset
#     resolution; the staging itself is module wiring. Resolved assets are
#     re-bound as a table under the container field `session_checkpoint_assets`.
#   * layer-2 field map — the six frozen top-level names rebind to container
#     fields of the SAME name (business_state / manual_pipeline exist verbatim
#     as global_data fields, drive_watcher.R:3869-3896; the other four are the
#     module wiring's names, post F08/F02). A NULL payload (nothing retained
#     at checkpoint) is planned "skipped" — honestly reported, never
#     fabricated as a restored value, never counted as a loss.
#
# Honest refusal (contract §6): ANY validate() refusal — missing/corrupt
# manifest, schema or environment mismatch — or a corrupt layer payload at
# plan time returns refused = TRUE with the reasons, plans NOTHING, and
# leaves the checkpoint conserved byte-for-byte (the restore primitives
# contain no deletion path whatsoever); apply() on a refused plan is a NO-OP.
# =============================================================================

TS_SESSION_CHECKPOINT_LAYER1_TARGETS <- c("sc_obj", "bulk_obj", "spatial_obj",
                                          "spatial_datasets")
TS_SESSION_CHECKPOINT_LAYER2_TARGETS <- c("pipeline_steps_completed", "manual_pipeline",
                                          "business_state", "deconv_props",
                                          "derived_tables", "banners")
TS_SESSION_CHECKPOINT_ASSETS_TARGET <- "session_checkpoint_assets"
TS_SESSION_CHECKPOINT_EPOCH_TARGETS <- c("sc_obj_epoch", "bulk_obj_epoch")

.sc_restore_refusal <- function(reasons, id = NULL, dir = NULL) {
  list(ok = FALSE, refused = TRUE, reasons = as.character(reasons),
       losses = character(0), items = NULL, assets = NULL,
       assets_target = TS_SESSION_CHECKPOINT_ASSETS_TARGET, epoch = NULL,
       banner = NULL, id = id, dir = dir)
}

#' Build the restoration PLAN from a validated checkpoint (ASR-2 — a PLAN,
#' never a mutation).
#'
#' Reads the checkpoint (contract §5.2: readRDS -> rebinding plan) through the
#' ASR-1 validate() gate FIRST: on any refusal — manifest missing/corrupt,
#' schema_version mismatch, environment mismatch — the plan is REFUSED with
#' the validate() reasons, nothing is planned and the checkpoint is conserved
#' (contract §6: never a partial silent restore, never a deletion). On a
#' valid checkpoint the plan carries, per restore ITEM:
#'   * layer-1 objects -> named payload targets (identity map over the frozen
#'     couche-1 vocabulary, see the header);
#'   * layer-2 business state -> the same-name field map (NULL payload ->
#'     status "skipped", honestly reported);
#'   * layer-3 assets -> resolutions ("as-is" / "fallback" with
#'     rematerialize_from / "lost"), the lost ones carrying the losses vector
#'     VERBATIM from validate();
#'   * the epoch decision ("bump" default / "preserve", header decision) and
#'     the bannière payload (timestamp, dataset scope, counts) ASR-3 renders.
#'
#' A corrupt layer payload discovered at plan time (valid manifest, unreadable
#' RDS) refuses the WHOLE restore — a half-plan would invite a half-restore.
#'
#' @param id Checkpoint id (strictly `checkpoint_*`).
#' @param dir Store directory (default: [ts_session_checkpoint_dir()]).
#' @param expect_schema_version,expect_env Passed to
#'   [ts_session_checkpoint_validate()].
#' @param epoch_action "bump" (default, F08 decision) or "preserve".
#' @return list(ok = TRUE, refused = FALSE, id, dir, schema_version, manifest,
#'   items, assets, assets_target, losses, epoch, banner) or
#'   list(ok = FALSE, refused = TRUE, reasons, ...). Never throws, never
#'   deletes.
ts_session_checkpoint_restore_plan <- function(id, dir = NULL,
                                               expect_schema_version = TS_SESSION_CHECKPOINT_SCHEMA_VERSION,
                                               expect_env = ts_session_checkpoint_env_fingerprint(),
                                               epoch_action = c("bump", "preserve")) {
  epoch_action <- match.arg(epoch_action)
  store <- ts_session_checkpoint_dir(dir)
  idc <- .sc_scalar_text(id)
  v <- ts_session_checkpoint_validate(idc, dir = store,
                                      expect_schema_version = expect_schema_version,
                                      expect_env = expect_env)
  if (!isTRUE(v$valid)) {
    r <- .sc_restore_refusal(
      .sc_default(v$reasons, "checkpoint validation refused the restoration"),
      id = idc, dir = store)
    r$losses <- as.character(.sc_default(v$losses, character(0)))
    return(r)
  }
  man <- v$manifest
  ck_dir <- file.path(store, idc)

  # --- layer 1: one verified payload per couche-1 named target --------------
  l1_files <- sort(list.files(ck_dir, pattern = "^layer1_.+\\.rds$", full.names = FALSE))
  l1_names <- sub("^layer1_(.+)\\.rds$", "\\1", l1_files)
  foreign <- setdiff(l1_names, TS_SESSION_CHECKPOINT_LAYER1_TARGETS)
  if (length(foreign)) {
    return(.sc_restore_refusal(sprintf(
      paste0("layer-1 object(s) %s are outside the frozen couche-1 rebind ",
             "vocabulary (%s) under schema_version %s — refusing the whole ",
             "restore (no half-restore, no silent drop); checkpoint conserved"),
      paste(foreign, collapse = ", "),
      paste(TS_SESSION_CHECKPOINT_LAYER1_TARGETS, collapse = ", "),
      .sc_default(man$schema_version, "NULL")), id = idc, dir = store))
  }
  items <- list()
  for (i in seq_along(l1_files)) {
    err <- tryCatch({
      payload <- readRDS(file.path(ck_dir, l1_files[i]))
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) {
      return(.sc_restore_refusal(sprintf(
        paste0("layer-1 object '%s' cannot be read (corrupted payload: %s) — ",
               "the whole restore is refused, nothing partial; checkpoint conserved"),
        l1_names[i], err), id = idc, dir = store))
    }
    items[[l1_names[i]]] <- list(kind = "layer1", target = l1_names[i],
                                 status = "restored", payload = payload,
                                 reason = NULL)
  }

  # --- layer 2: the frozen field map, NULL -> skipped honestly --------------
  l2_err <- tryCatch({
    l2 <- readRDS(file.path(ck_dir, "layer2_business_state.rds"))
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(l2_err)) {
    return(.sc_restore_refusal(sprintf(
      paste0("layer-2 business state cannot be read (corrupted payload: %s) — ",
             "the whole restore is refused, nothing partial; checkpoint conserved"),
      l2_err), id = idc, dir = store))
  }
  if (!is.list(l2)) {
    return(.sc_restore_refusal(
      "layer-2 payload is not the frozen business-state shape — refusing the whole restore; checkpoint conserved",
      id = idc, dir = store))
  }
  for (f in TS_SESSION_CHECKPOINT_LAYER2_TARGETS) {
    pay <- l2[[f]]
    items[[paste0("layer2:", f)]] <- if (is.null(pay)) {
      list(kind = "layer2", target = f, status = "skipped", payload = NULL,
           reason = sprintf("nothing was retained at checkpoint for '%s' — skipped honestly, never fabricated", f))
    } else {
      list(kind = "layer2", target = f, status = "restored", payload = pay,
           reason = NULL)
    }
  }

  # --- layer 3: per-asset resolution in the contract §5.3 order -------------
  l3_err <- tryCatch({
    l3 <- readRDS(file.path(ck_dir, "layer3_assets.rds"))
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(l3_err) || !is.list(l3) || !is.list(l3$assets)) {
    return(.sc_restore_refusal(sprintf(
      paste0("layer-3 asset table cannot be read (%s) — the whole restore is ",
             "refused, nothing partial; checkpoint conserved"),
      .sc_default(l3_err, "not the frozen asset-table shape")), id = idc, dir = store))
  }
  losses <- as.character(.sc_default(v$losses, character(0)))
  assets_table <- list()
  n_restored_assets <- 0L
  for (nm in names(l3$assets)) {
    a <- .sc_default(v$assets[[nm]], list())
    p <- .sc_scalar_text(a$path)
    st <- .sc_scalar_text(a$status)
    fb <- .sc_scalar_text(a$fallback_source)
    resolution <- if (identical(st, "present")) "as-is"
                  else if (isTRUE(a$recoverable)) "fallback" else "lost"
    reason <- NULL
    if (identical(resolution, "lost")) {
      hit <- losses[grepl(sprintf("asset '%s' ", nm), losses, fixed = TRUE)]
      reason <- .sc_default(hit, sprintf(
        "asset '%s' (%s) is unrecoverable from this checkpoint", nm, .sc_default(p, "?")))
    }
    items[[paste0("asset:", nm)]] <- list(
      kind = "layer3", target = TS_SESSION_CHECKPOINT_ASSETS_TARGET, asset = nm,
      status = if (identical(resolution, "lost")) "lost" else "restored",
      resolution = resolution,
      path = p, asset_kind = .sc_default(.sc_scalar_text(a$kind), "file"),
      asset_status = .sc_default(st, "missing"),
      existed_at_checkpoint = isTRUE(a$existed_at_checkpoint),
      rematerialize_from = if (identical(resolution, "fallback")) fb else NULL,
      reason = reason)
    if (!identical(resolution, "lost")) {
      n_restored_assets <- n_restored_assets + 1L
      assets_table[[nm]] <- list(
        path = p, kind = .sc_default(.sc_scalar_text(a$kind), "file"),
        status = .sc_default(st, "missing"), resolution = resolution,
        rematerialize_from = if (identical(resolution, "fallback")) fb else NULL,
        existed_at_checkpoint = isTRUE(a$existed_at_checkpoint))
    }
  }

  # --- epochs (F08 decision) + bannière payload -----------------------------
  epoch <- list(action = epoch_action, targets = TS_SESSION_CHECKPOINT_EPOCH_TARGETS,
                precedent = paste0("F08 — a restore REPLACES the sc/bulk games ",
                                   "(app.R:1289-1291, app.R:1599); bump semantics ",
                                   ".ts_bump_all_epochs() state.R:129-137"))
  if (identical(epoch_action, "bump")) {
    items[["obj_epochs"]] <- list(kind = "epoch", target = "obj_epochs",
                                  status = "restored", payload = NULL, reason = NULL)
  }
  statuses <- vapply(items, function(it) it$status, character(1))
  counts <- c(restored = sum(statuses == "restored"),
              skipped = sum(statuses == "skipped"),
              lost = sum(statuses == "lost"))
  ds <- as.character(.sc_default(man$dataset_ids, character(0)))
  banner <- list(
    timestamp = .sc_default(.sc_scalar_text(man$timestamp), NA_character_),
    trigger = .sc_default(.sc_scalar_text(man$trigger), NA_character_),
    dataset_ids = ds,
    n_datasets = length(ds),
    counts = counts,
    losses = losses,
    epoch_action = epoch_action)
  list(ok = TRUE, refused = FALSE, id = idc, dir = store,
       schema_version = man$schema_version, manifest = man,
       items = items, assets = assets_table,
       assets_target = TS_SESSION_CHECKPOINT_ASSETS_TARGET,
       losses = losses, epoch = epoch, banner = banner)
}

#' Execute a restore PLAN into a CALLER-SUPPLIED container (ASR-2 — pure
#' rebind, no I/O, no deletion).
#'
#' The engine never touches a real `global_data` (C17): the caller passes the
#' target container — an environment (rebound in place, the caller's explicit
#' choice) or a named list (rebound in a COPY; the caller must use the
#' RETURNED container, the input list is never mutated). Items planned
#' "restored" are written: layer-1 payloads and layer-2 fields to their
#' same-name targets, the resolved asset table to `session_checkpoint_assets`
#' (when at least one asset resolved), and — under the F08 "bump" decision —
#' sc_obj_epoch/bulk_obj_epoch incremented from the container's CURRENT
#' values. Items planned "skipped" or "lost" are SKIPPED honestly: they enter
#' no container field, they are reported, and the losses are returned
#' verbatim.
#'
#' A refused (or non-) plan is a NO-OP: refused = TRUE with the plan's
#' reasons and the container returned untouched — never a partial restore.
#'
#' @param plan The result of [ts_session_checkpoint_restore_plan()].
#' @param container Environment or named list supplied by the caller.
#' @return list(ok, refused, id, restored, skipped, lost, counts, container,
#'   epochs, assets_target). restored = the item names written; skipped =
#'   named list item name -> reason; lost = the plan's losses verbatim;
#'   counts = c(restored, skipped, lost) over the items (identical to the
#'   plan's bannière counts).
ts_session_checkpoint_restore_apply <- function(plan, container) {
  refuse_apply <- function(reasons) {
    list(ok = FALSE, refused = TRUE,
         reasons = as.character(reasons),
         restored = character(0), skipped = list(), lost = character(0),
         counts = c(restored = 0L, skipped = 0L, lost = 0L),
         container = container, epochs = NULL,
         assets_target = TS_SESSION_CHECKPOINT_ASSETS_TARGET)
  }
  if (!is.list(plan) || !isTRUE(plan$ok) || isTRUE(plan$refused) ||
      is.null(plan$items)) {
    return(refuse_apply(.sc_default(
      if (is.list(plan)) plan$reasons else NULL,
      "apply() refused: the plan is not a valid restoration plan (plan() refused or never ran)")))
  }
  is_env <- is.environment(container)
  is_lst <- is.list(container) && !is_env
  if (!is_env && !is_lst) {
    return(refuse_apply(paste0(
      "apply() refused: the container must be a caller-supplied environment ",
      "or named list — nothing was rebound, nothing was touched")))
  }
  # Normalize: all rebinds go through assign() on ONE working environment —
  # no <<- (its frame-search semantics would risk touching the caller's own
  # list). An environment container IS the working env (rebound in place,
  # the caller's explicit choice); a list container is COPIED in (the
  # caller's original list is never mutated — the rebinds live in the
  # returned list, rebuilt at the end).
  if (is_env) {
    work <- container
  } else {
    work <- new.env(parent = emptyenv())
    for (fld in names(container)) assign(fld, container[[fld]], envir = work)
  }
  restored <- character(0)
  skipped <- list()
  asset_rows <- list()
  epochs_out <- NULL
  wrote_assets <- FALSE
  for (nm in names(plan$items)) {
    it <- plan$items[[nm]]
    if (!identical(it$status, "restored")) {
      if (identical(it$status, "skipped")) skipped[[nm]] <- it$reason
      next  # "lost" items: reported through the losses vector only
    }
    if (identical(it$kind, "epoch")) {
      # F08 bump — executed from the container's CURRENT values, cross-domain
      # (both epochs together), semantics of .ts_bump_all_epochs (state.R:129).
      for (fld in TS_SESSION_CHECKPOINT_EPOCH_TARGETS) {
        cur <- if (exists(fld, envir = work, inherits = FALSE)) {
          get(fld, envir = work, inherits = FALSE)
        } else NULL
        bumped <- if (is.null(cur) || length(cur) != 1L || is.na(cur) ||
                      !is.numeric(cur)) 1L else as.integer(cur) + 1L
        assign(fld, bumped, envir = work)
        epochs_out[[fld]] <- bumped
      }
    } else if (identical(it$kind, "layer3")) {
      asset_rows[[it$asset]] <- list(
        path = it$path, kind = it$asset_kind, status = it$asset_status,
        resolution = it$resolution, rematerialize_from = it$rematerialize_from,
        existed_at_checkpoint = it$existed_at_checkpoint)
      wrote_assets <- TRUE
    } else {
      assign(it$target, it$payload, envir = work)
    }
    restored <- c(restored, nm)
  }
  if (wrote_assets) assign(plan$assets_target, asset_rows, envir = work)
  lost <- as.character(.sc_default(plan$losses, character(0)))
  out <- if (is_env) work else as.list(work)
  list(ok = TRUE, refused = FALSE, id = plan$id,
       restored = restored, skipped = skipped, lost = lost,
       counts = c(restored = length(restored), skipped = length(skipped),
                  lost = length(lost)),
       container = out, epochs = epochs_out,
       assets_target = plan$assets_target)
}
