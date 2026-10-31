# =============================================================================
# test-drive-session-tracker-feed.R — DSTV-1b RED tests (the §8.4 feed: the
# module-side half that actually FEEDS the DSTV-1 producer)
# =============================================================================
# Contract sources:
#   * docs/contracts/DSTV1_SESSION_TRACKER_CONTRACT.md §3 (A2 — the five
#     fiche-frozen business_state scalars, fiche :59-62), §6.4 (no new
#     timers), §6.5 (no vocabulary drift — TS_DRIVE_MANUAL_STATUSES,
#     drive_watcher.R:74), §8.4 (feed mechanism — open on disk at HEAD
#     ccbc340, decided conservatively exactly as the sibling RED file header
#     records it, test-drive-session-tracker.R:46-56):
#         global_data$manual_pipeline = list(status=, step=)     (scalars)
#         global_data$business_state  = list(has_data=, object_class=,
#                                            n_elements=, n_clusters=,
#                                            deconv_done=)        (scalars)
#   * docs/agent/swarm/round7/DSTV1_REMEASURE_2026-10-31.md §1b (measured at
#     HEAD: grep manual_pipeline/business_state over R/ modules/ app.R
#     global.R → 0 writers — the emitter is fed by honest defaults only) and
#     §4 (the transversal item: « publication des scalaires manuels par
#     mod_sc/mod_bulk/mod_spatial aux transitions de pipeline »).
#
# What these tests pin (the DSTV-1b gate — they MUST fail on the unmodified
# tree, then hold the feed once wired):
#   1. AT the manual-pipeline transition sites of the three pipeline modules,
#     the module publishes its scalars DIRECTLY on global_data — literal
#     `global_data$manual_pipeline <-` / `global_data$business_state <-`
#     assignments INSIDE the transition site (inline scalar lists; no
#     indirection that would hide the published shape):
#       * launch site → status "running"
#         (sc: input$sc_ap_confirm, mod_sc.R:1746; bulk: input$ap_confirm,
#         mod_bulk.R:402; spatial: input$btn_run_all via drive_trigger(),
#         mod_spatial_pipeline.R:766);
#       * step site → status "step" + step = the module's OWN accordion
#         panel VALUE ("0_autopipeline", mod_sc.R:714 / mod_bulk.R:24;
#         "panel_pipeline", mod_spatial.R:130-131) — the §8.3 convention;
#         NEVER the i18n label (T4);
#       * done site → status "done"
#         (sc: sc_pipeline_task$status() success, mod_sc.R:1663-1729;
#         bulk: ap_confirm success tail, mod_bulk.R:714-721; spatial:
#         .finish_pipeline, mod_spatial_pipeline.R:408-412).
#   2. business_state carries the FIVE frozen names verbatim and is derived
#      from SURFACE scalars only — no Seurat deep inspection (no
#      GetAssayData/FetchData/as.matrix/Embeddings/..., no @assays/
#      @reductions/@graphs): the module layer owns the object and reads its
#      metadata (status-bar precedent, mod_sc.R:1248-1266); the drive layer
#      never sees an object (contract §3; tools/_drive/README.md:108-115).
#   3. i18n-safe: no i18n$t()/.tr() inside any published expression —
#      machine fields never carry i18n rendering (T4, contract §2).
#   4. No new timers: the transition sites gain no invalidateLater /
#      reactiveTimer / later::later (contract §6.4), and the four module
#      files keep their measured 0 such calls (measured 2026-10-31 at HEAD:
#      grep -c → 0 in mod_sc.R, mod_bulk.R, mod_spatial.R,
#      mod_spatial_pipeline.R).
#
# Sandbox discipline: NO Shiny app is booted and NO drive root is touched —
# this file is pure source inspection (AST helpers, precedent
# tests/testthat/helper-ast.R) plus constant reads of the already-shipped
# DSTV-1 producer. There is no app session to sandbox under TRANSCRIPTO_*;
# the producer-side behavior (emitter + readers) is already pinned headless
# by test-drive-session-tracker.R:213+ — this file pins the MISSING half.
#
# Statuses published are exactly the ask's transition vocabulary
# {running, step, done}; "error" needs NO new publication site: the O18
# error ring already carries it (mod_sc.R:1669/:1736 ts_drive_record_error
# → pipeline_status="error", drive_watcher.R:3842-3847).
# =============================================================================

# Producer constants (same bootstrap as test-drive-session-tracker.R:64-74 —
# R/core first, skeleton tree fallback).
if (file.exists(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), local = FALSE)
}

if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"), local = FALSE)
}

SC          <- "modules/sc/mod_sc.R"
BULK        <- "modules/bulk/mod_bulk.R"
SPATIAL_UI  <- "modules/spatial/mod_spatial.R"
SPATIAL_PIPE <- "modules/spatial/mod_spatial_pipeline.R"

FIVE_SCALARS <- c("has_data", "object_class", "n_elements", "n_clusters", "deconv_done")
FORBIDDEN_DEEP <- c("GetAssayData", "SetAssayData", "FetchData", "as.matrix",
                    "Embeddings", "Loadings", "GetAssay", "JoinLayers")

# ── Local AST helpers (built on helper-ast.R primitives — extend, don't
#    duplicate; iteration by INDEX per helper-ast.R invariant (1)) ───────────

.feed_file <- function(relpath) ts_ast_parse(relpath)

# All RHS expressions of `global_data$<field> <- <rhs>` found in x.
.feed_pub_rhs <- function(x, field) {
  hits <- ts_ast_find_all(x, function(y) {
    if (!ts_ast_is_call_to(y, "<-")) return(FALSE)
    l <- tryCatch(as.list(y), error = function(e) NULL)
    if (is.null(l) || length(l) < 3L) return(FALSE)
    identical(ts_ast_deparse(l[[2]]), paste0("global_data$", field))
  })
  Filter(Negate(is.null), lapply(hits, function(h) tryCatch(as.list(h)[[3]], error = function(e) NULL)))
}

.feed_pub_deparses <- function(x, field) {
  vapply(.feed_pub_rhs(x, field), function(x) ts_ast_deparse(x), character(1))
}

# The literal bound to `status = "..."` in a deparsed publication (NA if the
# status is not a string literal — the feed publishes literals, not variables).
.feed_status_of <- function(d) {
  vapply(d, function(x) {
    m <- regmatches(x, regexpr('status *= *"[^"]*"', x))
    if (!length(m) || !nzchar(m)) NA_character_ else sub('^status *= *"', "", sub('"$', "", m))
  }, character(1), USE.NAMES = FALSE)
}

# The literal bound to `step = "..."` in a deparsed publication.
.feed_step_of <- function(d) {
  vapply(d, function(x) {
    m <- regmatches(x, regexpr('step *= *"[^"]*"', x))
    if (!length(m) || !nzchar(m)) NA_character_ else sub('^step *= *"', "", sub('"$', "", m))
  }, character(1), USE.NAMES = FALSE)
}

# The accordion panel VALUE of the panel housing `marker` (the §8.3
# convention source: the machine value, never the i18n label). Panels NEST
# (parent sections since LOT 2 — mod_sc.R:703-715), so the marker can appear
# in several enclosing panels: the INNERMOST match (smallest deparse) carries
# the button's own value.
.feed_panel_value <- function(relpath, marker) {
  p <- .feed_file(relpath)
  hits <- ts_ast_find_all(p, function(y) {
    if (!ts_ast_is_call_to(y, "accordion_panel")) return(FALSE)
    grepl(marker, ts_ast_deparse(y), fixed = TRUE)
  })
  if (!length(hits)) return(NULL)
  sizes <- vapply(hits, function(h) nchar(ts_ast_deparse(h)), integer(1))
  hit <- hits[[which.min(sizes)]]
  l <- as.list(hit)
  for (i in seq_along(l)) {                       # by INDEX (invariant (1))
    if (identical(names(l)[i], "value")) {
      return(gsub('^"|"$', "", ts_ast_deparse(l[[i]])))
    }
  }
  NULL
}

# Deep-inspection tokens in a publication RHS (must stay EMPTY — surface
# scalars only). @meta.data is allowed: the module layer owns the object and
# reads its metadata (status-bar precedent, mod_sc.R:1248-1266).
.feed_deep_hits <- function(rhs) {
  bad <- character(0)
  if (is.null(rhs)) return(bad)
  ts_ast_walk(rhs, function(y) {
    h <- tryCatch(as.character(y[[1]])[1], error = function(e) NULL)
    if (is.character(h) && h %in% FORBIDDEN_DEEP) bad <<- c(bad, h)
    if (is.call(y) && length(y) >= 2L && is.call(y[[1]]) && length(y[[1]]) == 3L &&
        as.character(y[[1]][[1]]) %in% c("::", ":::") &&
        identical(as.character(y[[1]][[2]]), "Seurat")) {
      bad <<- c(bad, paste0("Seurat::", as.character(y[[1]][[3]])))
    }
    d <- tryCatch(ts_ast_deparse(y), error = function(e) "")
    if (grepl("@assays|@reductions|@graphs", d)) bad <<- c(bad, d)
    FALSE
  })
  unique(bad)
}

.feed_assert_pubs_clean <- function(rhs_list, where) {
  for (i in seq_along(rhs_list)) {                # by INDEX (invariant (1))
    d <- ts_ast_deparse(rhs_list[[i]])
    expect_false(grepl("i18n\\$t\\(|\\.tr\\(", d),
                 info = sprintf("%s: publication %d carries i18n rendering (T4): %s",
                                where, i, d))
    deep <- .feed_deep_hits(rhs_list[[i]])
    expect_equal(length(deep), 0L,
                 info = sprintf("%s: publication %d inspects an object deeply (surface scalars only): %s",
                                where, i, paste(deep, collapse = ", ")))
  }
}

.feed_assert_no_timers <- function(body, where) {
  d <- ts_ast_deparse(body)
  expect_false(grepl("invalidateLater|reactiveTimer|later::later", d),
               info = sprintf("%s: the transition site must not add a timer (contract §6.4)", where))
}

# The body of a local `<name> <- function(...) { ... }` definition.
.feed_fun_body <- function(relpath, name) {
  hit <- ts_ast_find(.feed_file(relpath), function(y) {
    if (!ts_ast_is_call_to(y, "<-")) return(FALSE)
    l <- tryCatch(as.list(y), error = function(e) NULL)
    if (is.null(l) || length(l) < 3L) return(FALSE)
    identical(ts_ast_deparse(l[[2]]), name)
  })
  if (is.null(hit)) return(NULL)
  rhs <- tryCatch(as.list(hit)[[3]], error = function(e) NULL)
  if (is.null(rhs)) return(NULL)
  tryCatch(as.list(rhs)[[3]], error = function(e) NULL)
}

# ── 1. SC (the T3 flagship: autopipeline 1 clic) ────────────────────────────

test_that("SC launch site (input$sc_ap_confirm, mod_sc.R:1746) publishes manual_pipeline 'running' and the five business_state scalars (DSTV-1b, §8.4)", {
  block <- ts_ast_observe_block(SC, "sc_ap_confirm")
  mp_d <- .feed_pub_deparses(block, "manual_pipeline")
  expect_true(any(grepl('"running"', mp_d, fixed = TRUE)),
              info = sprintf("manual_pipeline publications at the SC launch site: %d — measured 0 writers at HEAD (re-mesure §1b)",
                             length(mp_d)))
  # §8.4 shape: list(status=, step=) — the step component is part of the shape.
  expect_true(any(grepl('step *=', mp_d)),
              info = "the published list must carry BOTH scalars (status, step)")
  bs <- .feed_pub_rhs(block, "business_state")
  expect_true(length(bs) >= 1L,
              info = "business_state must be published at the launch site (has_data=true must be visible DURING the run — T3)")
  bs_d <- .feed_pub_deparses(block, "business_state")
  for (nm in FIVE_SCALARS) {
    expect_true(any(grepl(nm, bs_d, fixed = TRUE)),
                info = sprintf("business_state must carry the frozen name '%s' verbatim (fiche :59-62)", nm))
  }
  .feed_assert_pubs_clean(c(.feed_pub_rhs(block, "manual_pipeline"), bs), "SC launch site")
  .feed_assert_no_timers(block, "SC launch site")
})

test_that("SC publishes a 'step' transition whose step value is the module's own accordion panel value (§8.3, mod_sc.R:714 == TS_DRIVE_SC_PANELS drive_allowlist.R:471)", {
  panel <- .feed_panel_value(SC, "btn_auto_pipeline_sc")
  expect_identical(panel, "0_autopipeline")
  expect_identical(panel, TS_DRIVE_SC_PANELS)
  d <- .feed_pub_deparses(.feed_file(SC), "manual_pipeline")
  sts <- .feed_status_of(d)
  step_pubs <- d[!is.na(sts) & sts == "step"]
  expect_true(length(step_pubs) >= 1L,
              info = sprintf("SC 'step' publications found: %d — pipeline_step never reaches state.json without one (drive_watcher.R:3858)",
                             length(step_pubs)))
  expect_true(any(vapply(step_pubs, function(x) grepl(sprintf('step *= *"%s"', panel), x), logical(1))),
              info = "the step value must be the panel VALUE '0_autopipeline', never an i18n label (T4)")
})

test_that("SC completion site (sc_pipeline_task$status() success, mod_sc.R:1663-1729) publishes 'done' and refreshes business_state (object replaced at mod_sc.R:1694-1701)", {
  block <- ts_ast_observe_block(SC, "sc_pipeline_task$status()")
  mp_d <- .feed_pub_deparses(block, "manual_pipeline")
  expect_true(any(grepl('"done"', mp_d, fixed = TRUE)),
              info = sprintf("manual_pipeline publications at the SC completion site: %d", length(mp_d)))
  bs_d <- .feed_pub_deparses(block, "business_state")
  expect_true(length(bs_d) >= 1L,
              info = "business_state must be refreshed when the new object lands (n_elements/n_clusters change)")
  for (nm in FIVE_SCALARS) {
    expect_true(any(grepl(nm, bs_d, fixed = TRUE)),
                info = sprintf("refreshed business_state must carry '%s'", nm))
  }
  .feed_assert_pubs_clean(c(.feed_pub_rhs(block, "manual_pipeline"), .feed_pub_rhs(block, "business_state")),
                          "SC completion site")
  .feed_assert_no_timers(block, "SC completion site")
})

# ── 2. Bulk (sync 1-clic pipeline — launch and completion share the site) ───

test_that("Bulk launch site (input$ap_confirm, mod_bulk.R:402) publishes manual_pipeline 'running' and the five business_state scalars (DSTV-1b, §8.4)", {
  block <- ts_ast_observe_block(BULK, "ap_confirm")
  mp_d <- .feed_pub_deparses(block, "manual_pipeline")
  expect_true(any(grepl('"running"', mp_d, fixed = TRUE)),
              info = sprintf("manual_pipeline publications at the bulk launch site: %d", length(mp_d)))
  expect_true(any(grepl('step *=', mp_d)),
              info = "the published list must carry BOTH scalars (status, step)")
  bs_d <- .feed_pub_deparses(block, "business_state")
  expect_true(length(bs_d) >= 1L, info = "business_state must be published at the bulk transition site")
  for (nm in FIVE_SCALARS) {
    expect_true(any(grepl(nm, bs_d, fixed = TRUE)),
                info = sprintf("business_state must carry the frozen name '%s' (bulk_obj is a surface list: $counts/$metadata, mod_bulk.R:404-406)", nm))
  }
  .feed_assert_pubs_clean(c(.feed_pub_rhs(block, "manual_pipeline"), .feed_pub_rhs(block, "business_state")),
                          "Bulk transition site")
  .feed_assert_no_timers(block, "Bulk transition site")
})

test_that("Bulk publishes a 'step' transition with the panel value '0_autopipeline' (mod_bulk.R:24)", {
  panel <- .feed_panel_value(BULK, "btn_auto_pipeline")
  expect_identical(panel, "0_autopipeline")
  d <- .feed_pub_deparses(.feed_file(BULK), "manual_pipeline")
  sts <- .feed_status_of(d)
  step_pubs <- d[!is.na(sts) & sts == "step"]
  expect_true(length(step_pubs) >= 1L,
              info = sprintf("bulk 'step' publications found: %d", length(step_pubs)))
  expect_true(any(vapply(step_pubs, function(x) grepl(sprintf('step *= *"%s"', panel), x), logical(1))),
              info = "the step value must be the panel VALUE, never an i18n label (T4)")
})

test_that("Bulk completion (ap_confirm success tail, mod_bulk.R:714-721) publishes 'done'", {
  block <- ts_ast_observe_block(BULK, "ap_confirm")
  mp_d <- .feed_pub_deparses(block, "manual_pipeline")
  expect_true(any(grepl('"done"', mp_d, fixed = TRUE)),
              info = "the sync bulk pipeline must publish 'done' at its success tail (same site, second transition)")
})

# ── 3. Spatial (staged pipeline — .finish_pipeline is the done site) ────────

test_that("Spatial launch site (input$btn_run_all via drive_trigger(), mod_spatial_pipeline.R:766) publishes manual_pipeline 'running' and the five business_state scalars (DSTV-1b, §8.4)", {
  block <- ts_ast_observe_block(SPATIAL_PIPE, "drive_trigger()")
  mp_d <- .feed_pub_deparses(block, "manual_pipeline")
  expect_true(any(grepl('"running"', mp_d, fixed = TRUE)),
              info = sprintf("manual_pipeline publications at the spatial launch site: %d (pipeline_state(\"running\") is set at :778 but never published to global_data)", length(mp_d)))
  expect_true(any(grepl('step *=', mp_d)),
              info = "the published list must carry BOTH scalars (status, step)")
  bs_d <- .feed_pub_deparses(block, "business_state")
  expect_true(length(bs_d) >= 1L, info = "business_state must be published at the spatial launch site")
  for (nm in FIVE_SCALARS) {
    expect_true(any(grepl(nm, bs_d, fixed = TRUE)),
                info = sprintf("business_state must carry the frozen name '%s'", nm))
  }
  .feed_assert_pubs_clean(c(.feed_pub_rhs(block, "manual_pipeline"), .feed_pub_rhs(block, "business_state")),
                          "Spatial launch site")
  .feed_assert_no_timers(block, "Spatial launch site")
})

test_that("Spatial publishes a 'step' transition with the panel value 'panel_pipeline' (mod_spatial.R:130-131)", {
  panel <- .feed_panel_value(SPATIAL_UI, "mod_spatial_pipeline_ui")
  expect_identical(panel, "panel_pipeline")
  d <- .feed_pub_deparses(.feed_file(SPATIAL_PIPE), "manual_pipeline")
  sts <- .feed_status_of(d)
  step_pubs <- d[!is.na(sts) & sts == "step"]
  expect_true(length(step_pubs) >= 1L,
              info = sprintf("spatial 'step' publications found: %d", length(step_pubs)))
  expect_true(any(vapply(step_pubs, function(x) grepl(sprintf('step *= *"%s"', panel), x), logical(1))),
              info = "the step value must be the panel VALUE 'panel_pipeline', never an i18n label (T4)")
})

test_that("Spatial completion (.finish_pipeline, mod_spatial_pipeline.R:408-412) publishes 'done'", {
  body <- .feed_fun_body(SPATIAL_PIPE, ".finish_pipeline")
  expect_false(is.null(body), info = ".finish_pipeline() definition not found")
  mp_d <- .feed_pub_deparses(body, "manual_pipeline")
  expect_true(any(grepl('"done"', mp_d, fixed = TRUE)),
              info = sprintf("manual_pipeline publications in .finish_pipeline(): %d", length(mp_d)))
  .feed_assert_pubs_clean(.feed_pub_rhs(body, "manual_pipeline"), ".finish_pipeline")
  .feed_assert_no_timers(body, ".finish_pipeline")
})

# ── 4. Cross-cutting: vocabulary, i18n-safety, surface-only, no timers ──────

test_that("Every published expression is i18n-safe, vocabulary-clean and surface-scalars-only; the feed adds NO timer (contract §2 T4, §3, §6.4, §6.5)", {
  all_mp <- character(0)
  all_bs <- character(0)
  for (f in c(SC, BULK, SPATIAL_PIPE)) {
    p <- .feed_file(f)
    all_mp <- c(all_mp, .feed_pub_deparses(p, "manual_pipeline"))
    all_bs <- c(all_bs, .feed_pub_deparses(p, "business_state"))
  }
  # The feed exists: at least launch+step+done per family and one business_state
  # publication per family (measured 0 at HEAD — re-mesure §1b).
  expect_true(length(all_mp) >= 9L,
              info = sprintf("manual_pipeline publications across the three modules: %d (need >= 9: running+step+done x sc/bulk/spatial)", length(all_mp)))
  expect_true(length(all_bs) >= 3L,
              info = sprintf("business_state publications across the three modules: %d (need >= 3)", length(all_bs)))
  # §6.5: no vocabulary drift — every published status is in the frozen set.
  sts <- unique(stats::na.omit(.feed_status_of(all_mp)))
  expect_true(length(sts) >= 3L && all(sts %in% TS_DRIVE_MANUAL_STATUSES),
              info = sprintf("published statuses: %s — frozen vocabulary drive_watcher.R:74: %s",
                             paste(sts, collapse = ", "), paste(TS_DRIVE_MANUAL_STATUSES, collapse = ", ")))
  # T4: machine fields never carry i18n rendering.
  expect_false(any(grepl("i18n\\$t\\(|\\.tr\\(", c(all_mp, all_bs))),
               info = "a published expression carries i18n rendering")
  # Surface scalars only, everywhere.
  for (f in c(SC, BULK, SPATIAL_PIPE)) {
    p <- .feed_file(f)
    .feed_assert_pubs_clean(.feed_pub_rhs(p, "manual_pipeline"), f)
    .feed_assert_pubs_clean(.feed_pub_rhs(p, "business_state"), f)
  }
  # §6.4: no new timers anywhere in the touched modules (measured 0 at HEAD,
  # 2026-10-31: grep -c invalidateLater → 0 in all four files).
  for (f in c(SC, BULK, SPATIAL_UI, SPATIAL_PIPE)) {
    src <- paste(readLines(file.path(ts_project_root(), f), warn = FALSE), collapse = "\n")
    expect_equal(sum(grepl("invalidateLater\\(|reactiveTimer\\(|later::later\\(", src)), 0L,
                 info = sprintf("%s must not gain a timer", f))
  }
})
