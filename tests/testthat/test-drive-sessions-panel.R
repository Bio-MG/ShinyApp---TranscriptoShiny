# =============================================================================
# test-drive-sessions-panel.R — DSTV-3 RED tests (Axe C : fiche :81-87 ; re-scope
# re-mesure « DSTV1_REMEASURE_2026-10-31.md » §3 :88-91)
# =============================================================================
# Contract: docs/proposals/DRIVE_SESSION_TRACKER_SPATIAL_WIRING_PROPOSAL.md
#   C1 (:81-82)  — `sessions[]` dans state.json, plafond 5, tenue par le watcher
#                  (attach enregistre, onSessionEnded/reap retire, tick rafraîchit),
#                  digest couvrant la table, test schéma étendu ;
#   C2 (:83-85)  — ligne d'activité « Agent : module X — action … — il y a Ns »
#                  dans le panneau « Contrôle agent (MCP) » (app.R:382-391),
#                  abonnée au CHANGEMENT DE HASH (zéro churn — le pattern
#                  d'événements du badge, app.R:659-681 + drive_watcher.R:4907-4911,
#                  est le précédent), i18n fr/en via tools/add_i18n_keys.R
#                  (leçon B1 SCOPÉE : un run nu flusherait ~164 entrées
#                  étrangères en attente — tools/add_i18n_keys.R:6-10) ;
#   C3 (:86-87)  — hors headless uniquement : le champ `viewer` sert de
#                  discriminant, la porte est `ts_drive_interactive()`
#                  (drive_watcher.R:770-775 ; mesuré re-mesure §3 :90 : un enfant
#                  non interactif sans l'option ne tick pas).
#
# RED-first: these tests are written AGAINST that scope BEFORE any
# implementation; they MUST fail on the unmodified tree (HEAD ba786cf), then
# pin the DSTV-3 gate. `sessions[]` n'existe nulle part à HEAD (grep 0
# occurrence, re-mesure §3 :84) — chaque bloc échoue d'abord sur l'API cible
# absente, nommée explicitement par .dstv3_api().
#
# Sandbox discipline (house style, precedents test-drive-session-tracker.R:20-34
# and test-drive-passive-state.R:42-69):
#   * every test boots its OWN temp root and sets TRANSCRIPTO_DRIVE_ROOT to it
#     (restored on exit) — the live tools/_drive/ of the working tree is never
#     touched;
#   * NO Shiny app is booted here: the DSTV-3 display logic is pinned through
#     the PURE R/ layer (badge precedent :4290-4345 — "pure decision, app.R only
#     assigns"), so no port is opened and none is needed;
#   * no arm.json is ever written: the passive channel observes WITHOUT arming
#     (re-mesure Session A : émission sans aucun armement).
#
# Open-point decisions encoded below (fiche-silent points, tranchés
# conservativement — each is the implementer's target; cf. DSTV-1 header):
#
#   D1 — hb_age_s computation. Computed AT EMISSION from the session's stored
#     absolute heartbeat time: round(max(0, now - hb_at), 1) seconds. Never a
#     self-incrementing counter (a counter would drift from the hb writes it
#     claims to mirror, drive_watcher.R:4841-4874).
#   D2 — digest covers the table MINUS its time-derived projection. The digest
#     covers each row's SEMANTIC fields (session_id, viewer, module, action,
#     armed) and deliberately EXCLUDES hb_age_s — the established timestamp
#     exclusion (drive_watcher.R:3790-3792 "Deliberately excludes timestamps so
#     unchanged semantic state produces identical hashes across ticks").
#     Otherwise an armed-but-idle session (heartbeat legitimately moving every
#     ~2 s, :4841-4846) would rewrite state.json on every beat and re-render
#     the panel — falsifying both O11's "n'écrit que si le contenu change" and
#     the fiche's zero-churn criterion (:103). Pinned consequence: the emitted
#     hb_age_s is the age AT THE LAST CONTENT CHANGE (honest, coarse — the
#     panel's "il y a Ns" refreshes on the next hash change, exactly the
#     fiche's :85 "abonné au changement de hash, pas au tick").
#   D3 — cap semantics. Plafond 5 = eviction is FIFO over the LAST-ACTIVITY
#     order: a refresh of an already-tracked row re-prioritizes it (LRU), so
#     a live session refreshing itself can never be the one evicted; a 6th
#     NEW registration evicts the row with the oldest last-refresh. Same
#     bounded-buffer pattern as the 20-entry error ring
#     (TS_DRIVE_MAX_ERRORS, drive_watcher.R:63 ; fiche :81 "même pattern que
#     le ring buffer").
#   D4 — action counter « n/m » : DEFERRED (not implemented at DSTV-3). The
#     wire carries no total: a scenario is ONE action ("One action to apply",
#     tools/_drive/README.md:13 ; seq is the per-action counter) and no
#     plan/batch concept exists, so "n/m" would fabricate the m — the very
#     fabrication this repo's doctrine forbids ("A declaration, never a
#     proof", README:22). Conservative: `action` carries the action LABEL
#     (clean string, "" before any action) and the line renders the label.
#     The « 3/9 » shape needs a wire field the protocol does not carry — a
#     protocol bump is a declared non-goal (fiche :93-94).
#   D5 — visibility gate (C3). ts_drive_activity_visible(interactive, viewer,
#     armed) = isTRUE(interactive) && viewer != "headless" && isTRUE(armed).
#     * interactive = ts_drive_interactive() (drive_watcher.R:770-775) — the
#       ONLY gate, overridable via options(ts.drive.interactive=) exactly
#       because the real gate is untestable otherwise ;
#     * "headless" hides (fiche :87 "une session headless n'affiche rien de
#       nouveau") ; an "unknown" viewer does NOT hide — the fiche names only
#       headless, and the line lives in a panel that exists only in
#       interactive sessions (app.R:388 build-time gate) ;
#     * armed hides — the line describes the AGENT (fiche :14-15 "quand une
#       session MCP pilote l'app") ; an unarmed session's navigation is the
#       HUMAN's activity and must not read "Agent :" ;
#     * the producer stays honest: a headless session IS recorded in
#       sessions[] (machine truth) — the gate is display-only.
#   D6 — event semantics (C2). The activity event fires on EVERY actual
#     state.json write (hash change) — and only then (no write, no event, no
#     re-render). It carries `armed` and the DISPLAY GATE decides: an
#     unarmed/armed-flip write is what lets the line disappear on disarm
#     without a second subscription. One-shot drain, precedent
#     pending_events() (drive_watcher.R:4907-4911, drained app.R:903-911).
#   D7 — schema. `sessions[]` is a top-level ADDITIVE field of state.json,
#     always present (possibly empty), rows carrying EXACTLY the six
#     fiche-frozen fields {session_id, viewer, module, action, hb_age_s,
#     armed} (:81-82). module/action are stripped of any HTML at the emitter
#     (T4 lesson — never trust upstream, .ts_drive_strip_html
#     drive_watcher.R:3752-3760) ; viewer is coerced to TS_DRIVE_VIEWERS,
#     out-of-set -> "unknown", never a lie (rule of ts_drive_viewer
#     drive_watcher.R:533-535).
#   D8 — T5 residue (lien jeton<->id) : OUT of the pinned scope (optional per
#     the ask and per re-mesure §3 :91). The line carries session_id (the
#     derived id already displayed by the control panel since 3d86100) ; no
#     new linkage UI is asserted here.
#
# Target API (GREEN implementer's spec — all in tools/pkg_skeleton/R/):
#   ts_drive_sessions_track(token, session_id, viewer, module = "",
#                           action = "", armed = FALSE, hb_at = Sys.time())
#       — register/refresh one session row (attach + per-tick refresh).
#   ts_drive_sessions_drop(token)
#       — remove one row (onSessionEnded drive_watcher.R:4703-4721 + reap).
#   ts_drive_sessions()
#       — wire-shaped rows (six frozen fields each), capped at 5.
#   ts_drive_pending_activity_events()
#       — one-shot drain of the activity events queued by the emitter on each
#         actual state.json write.
#   ts_drive_activity_view(event)
#       — whitelist projection for the UI (precedent ts_drive_badge_view,
#         drive_allowlist.R:2202-2232).
#   ts_drive_activity_visible(interactive, viewer, armed)
#       — the C3 display gate (precedent ts_drive_badge_visible,
#         drive_watcher.R:4340-4345).
#   … and the emitter/digest extension: ts_drive_emit_passive_state()
#     (:3965-4046) publishes `sessions[]` and queues the event ;
#     ts_drive_compute_state_hash() (:3808-3832) gains a `sessions` argument
#     (default NULL — historical calls keep their behavior, DSTV-1 pattern).
# =============================================================================

# Ensure allowlist and drive watcher are loaded (same bootstrap as
# test-drive-session-tracker.R:62-70 — R/core first, skeleton tree fallback).
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

.dstv3_or <- function(a, b) if (is.null(a)) b else a

# The frozen C1 row schema (fiche :81-82, verbatim).
.dstv3_session_fields <- c("session_id", "viewer", "module", "action",
                           "hb_age_s", "armed")

# The C2 i18n keys (fr source key + en pair) the activity line composes with.
.dstv3_fr_key <- "Agent : module %s \u2014 action %s \u2014 il y a %s s"
.dstv3_en_key <- "Agent: module %s \u2014 action %s \u2014 %s s ago"

# --- Fixtures ----------------------------------------------------------------

#' Crisp RED failure naming the missing target API. A RED test that dies in
#' "could not find function" is a red herring; this names the GREEN target.
.dstv3_api <- function(fns) {
  missing <- fns[!vapply(fns, function(f) exists(f, mode = "function"), logical(1))]
  if (length(missing)) {
    fail(paste0(
      "DSTV-3 target API absent from tools/pkg_skeleton/R/ (RED expected): ",
      paste(missing, collapse = ", "),
      " — see the target-API block in this file's header"
    ))
  }
  invisible(TRUE)
}

#' Own temp root + TRANSCRIPTO_DRIVE_ROOT sandbox for one test.
#' Never touches the live tools/_drive/ of the working tree. Also resets the
#' DSTV-3 session-global surfaces THROUGH the public API (drop every tracked
#' token, drain the event buffer) — precedent test-drive-watcher.R:33-44 (the
#' drive state lives in a session-global env that tests must not leak across).
.dstv3_sandbox <- function(tag, envir = parent.frame()) {
  root <- file.path(tempdir(), paste0("tsdrive-dstv3-", tag, "-",
                                      as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE,
             showWarnings = FALSE)
  withr::local_envvar(TRANSCRIPTO_DRIVE_ROOT = root, .local_envir = envir)
  ts_drive_boot(root)
  ts_drive_clear_errors()
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  # Reset the DSTV-3 session-global surfaces THROUGH the public API — only
  # when it exists (at RED the guard above has already named what is missing;
  # this keeps the failure output one crisp message per block instead of a
  # follow-on "could not find function" from the fixture itself). The table
  # is keyed by TOKEN (its rows carry session_id, not the key), so the reset
  # is the dedicated clear, not a drop-by-session_id loop.
  if (exists("ts_drive_sessions_clear", mode = "function")) {
    ts_drive_sessions_clear()
  }
  if (exists("ts_drive_pending_activity_events", mode = "function")) {
    ts_drive_pending_activity_events()  # drain: a fresh test starts silent
  }
  root
}

#' Register one tracked session with sane defaults (tests read better).
.dstv3_track <- function(token, session_id, viewer = "visible", module = "",
                         action = "", armed = FALSE, hb_at = Sys.time()) {
  ts_drive_sessions_track(token, session_id = session_id, viewer = viewer,
                          module = module, action = action, armed = armed,
                          hb_at = hb_at)
}

# ── C1 — sessions[] in state.json : schema, cap, removal, digest ─────────────

test_that("C1 schema: state.json carries sessions[] with EXACTLY the six frozen fields, always present, viewer coerced to TS_DRIVE_VIEWERS (fiche :81-82)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_sessions",
               "ts_drive_sessions_drop"))
  sb <- .dstv3_sandbox("schema")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  # The field is ALWAYS present — additive, complete defaults (business_state
  # precedent, drive_watcher.R:4028) — even with NOTHING tracked.
  expect_true(ts_drive_emit_passive_state(token = "tok_dstv3_schema_00"))
  st0 <- ts_drive_read_state()
  expect_false(is.null(st0))
  expect_false(is.null(st0$sessions))
  # Empty table -> empty (but PRESENT) sessions[].
  expect_identical(length(st0$sessions), 0L)

  tok <- "tok_dstv3_schema_01"
  .dstv3_track(tok, session_id = "sid-schema-aaaa-bbbb", viewer = "visible",
               module = "sc", action = "run_pipeline", armed = TRUE,
               hb_at = Sys.time() - 5)

  # The table changed -> a real content change -> the emitter writes.
  expect_true(ts_drive_emit_passive_state(token = tok))
  # hash-gated: a second identical emission writes nothing.
  expect_false(ts_drive_emit_passive_state(token = tok))

  rows <- ts_drive_sessions()
  expect_identical(length(rows), 1L)
  # EXACTLY the six frozen fields — no seventh, none missing (fiche :81-82).
  expect_setequal(names(rows[[1]]), .dstv3_session_fields)
  expect_identical(as.character(rows[[1]]$session_id), "sid-schema-aaaa-bbbb")
  expect_identical(as.character(rows[[1]]$module), "sc")
  expect_identical(as.character(rows[[1]]$action), "run_pipeline")
  expect_identical(isTRUE(rows[[1]]$armed), TRUE)
  # D1: hb_age_s = round(max(0, now - hb_at), 1) — the row was tracked with
  # hb_at = now - 5, so the age is ~5 s (band, never a clock race).
  expect_true(is.numeric(rows[[1]]$hb_age_s))
  expect_true(rows[[1]]$hb_age_s >= 4.5 && rows[[1]]$hb_age_s <= 7)

  # The wire document carries the same shape (the JSON round-trip turns armed
  # into TRUE/FALSE and keeps hb_age_s numeric).
  st2 <- ts_drive_read_state()
  doc_rows <- st2$sessions
  expect_identical(length(doc_rows), 1L)
  expect_setequal(names(doc_rows[[1]]), .dstv3_session_fields)
  expect_identical(doc_rows[[1]]$session_id, "sid-schema-aaaa-bbbb")
  expect_identical(doc_rows[[1]]$module, "sc")
  expect_identical(doc_rows[[1]]$armed, TRUE)
  expect_true(is.numeric(doc_rows[[1]]$hb_age_s))

  # D7: viewer out of TS_DRIVE_VIEWERS -> "unknown", NEVER a lie (rule of
  # ts_drive_viewer, drive_watcher.R:533-535).
  tok2 <- "tok_dstv3_schema_02"
  .dstv3_track(tok2, session_id = "sid-schema-cccc-dddd",
               viewer = "test_sandbox", module = "bulk", armed = FALSE)
  rows2 <- ts_drive_sessions()
  row2 <- rows2[vapply(rows2, function(r) {
    identical(as.character(r$session_id), "sid-schema-cccc-dddd")
  }, logical(1))][[1]]
  expect_identical(as.character(row2$viewer), "unknown")
})

test_that("C1 T4 guard: no HTML ever leaks into sessions[] module/action (machine fields, precedent .ts_drive_strip_html)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_sessions"))
  sb <- .dstv3_sandbox("html")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_dstv3_html_01"
  # The EXACT T4 reproduction (re-mesure §1a : nav sans value -> input$main_nav
  # EST le span i18n brut) : the tracker must not trust upstream.
  .dstv3_track(tok, session_id = "sid-html-01",
               viewer = "visible",
               module = "<span class=\"i18n\" data-key=\"Single-Cell\">Single-Cell</span>",
               action = "run_pipeline", armed = TRUE)

  expect_true(ts_drive_emit_passive_state(token = tok))
  st <- ts_drive_read_state()
  expect_identical(length(st$sessions), 1L)
  row <- st$sessions[[1]]
  # Stripped to the clean label — the T4 regression guard at the NEW surface.
  expect_identical(as.character(row$module), "Single-Cell")
  chars <- unlist(lapply(row, as.character))
  expect_false(any(grepl("<[a-zA-Z!/][^>]*>", chars)))
})

test_that("C1 cap: the table is capped at 5, FIFO eviction on NEW registrations, refresh-in-place never evicts (D3, ring-buffer pattern)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_sessions"))
  sb <- .dstv3_sandbox("cap")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  toks <- sprintf("tok_dstv3_cap_%02d", 1:5)
  for (i in seq_along(toks)) {
    .dstv3_track(toks[i], session_id = paste0("sid-cap-", i),
                 viewer = "visible", module = "sc", armed = FALSE)
  }
  expect_identical(length(ts_drive_sessions()), 5L)

  # A REFRESH of the oldest token re-prioritizes it (LRU, D3): the live
  # session must never be the one its own activity evicts.
  .dstv3_track(toks[1], session_id = "sid-cap-1", viewer = "visible",
               module = "sc", action = "snapshot", armed = FALSE)
  expect_identical(length(ts_drive_sessions()), 5L)

  # A 6th NEW token evicts the row with the OLDEST last-refresh (tok 2), not
  # the refreshed tok 1.
  .dstv3_track("tok_dstv3_cap_06", session_id = "sid-cap-6",
               viewer = "visible", module = "sc", armed = FALSE)
  rows <- ts_drive_sessions()
  expect_identical(length(rows), 5L)  # capped, never 6
  ids <- vapply(rows, function(r) as.character(r$session_id), character(1))
  expect_setequal(ids, c("sid-cap-1", "sid-cap-3", "sid-cap-4", "sid-cap-5",
                         "sid-cap-6"))
  expect_false("sid-cap-2" %in% ids)

  # The wire document inherits the cap.
  expect_true(ts_drive_emit_passive_state(token = toks[6]))
  st <- ts_drive_read_state()
  expect_identical(length(st$sessions), 5L)
})

test_that("C1 removal: a dropped session leaves the table AND the emitted document (onSessionEnded/reap side, drive_watcher.R:4703-4721)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_sessions",
               "ts_drive_sessions_drop"))
  sb <- .dstv3_sandbox("removal")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok1 <- "tok_dstv3_removal_01"
  tok2 <- "tok_dstv3_removal_02"
  .dstv3_track(tok1, session_id = "sid-removal-1", viewer = "visible",
               module = "sc", action = "run_pipeline", armed = TRUE)
  .dstv3_track(tok2, session_id = "sid-removal-2", viewer = "headless",
               module = "bulk", armed = FALSE)
  expect_true(ts_drive_emit_passive_state(token = tok1))
  st <- ts_drive_read_state()
  expect_identical(length(st$sessions), 2L)

  # Session end: the row is REMOVED, the change is a real content change, so
  # the next emission writes (hash covers the table, D2).
  expect_true(isTRUE(suppressWarnings(ts_drive_sessions_drop(tok2))))
  expect_identical(length(ts_drive_sessions()), 1L)
  expect_true(ts_drive_emit_passive_state(token = tok1))
  st2 <- ts_drive_read_state()
  expect_identical(length(st2$sessions), 1L)
  expect_identical(st2$sessions[[1]]$session_id, "sid-removal-1")

  # Dropping an unknown token is an honest no-op, not a crash — and NOT a
  # content change (the hash must not move).
  h_before <- ts_drive_read_state()$content_hash
  expect_false(isTRUE(suppressWarnings(ts_drive_sessions_drop("tok_absent"))))
  expect_false(ts_drive_emit_passive_state(token = tok1))
  expect_identical(ts_drive_read_state()$content_hash, h_before)
})

test_that("C1 digest: the digest COVERS the table (a row change emits) and EXCLUDES hb_age_s aging (an armed-but-idle session stays silent — D2, zero churn)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_sessions_drop"))
  sb <- .dstv3_sandbox("digest")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_dstv3_digest_01"
  hb <- Sys.time() - 5
  .dstv3_track(tok, session_id = "sid-digest-1", viewer = "visible",
               module = "sc", action = "run_pipeline", armed = TRUE,
               hb_at = hb)
  expect_true(ts_drive_emit_passive_state(token = tok))
  h1 <- ts_drive_read_state()$content_hash

  # (a) COVERAGE — a semantic row change (the action moved) is a content
  # change: the emitter MUST write and the hash MUST differ.
  .dstv3_track(tok, session_id = "sid-digest-1", viewer = "visible",
               module = "sc", action = "snapshot", armed = TRUE, hb_at = hb)
  expect_true(ts_drive_emit_passive_state(token = tok))
  h2 <- ts_drive_read_state()$content_hash
  expect_false(identical(h1, h2))

  # (b) EXCLUSION — same semantic row, only the heartbeat AGED (fixed hb_at,
  # 1.2 s of wall clock): the digest must NOT move, the emitter must stay
  # silent, and the document must keep its hash. This is what keeps an
  # armed-but-idle session from rewriting state.json every ~2 s beat and
  # re-rendering the panel (O11 + fiche :103).
  .dstv3_track(tok, session_id = "sid-digest-1", viewer = "visible",
               module = "sc", action = "snapshot", armed = TRUE, hb_at = hb)
  Sys.sleep(1.2)
  expect_false(ts_drive_emit_passive_state(token = tok))
  expect_identical(ts_drive_read_state()$content_hash, h2)

  # (c) A session LEAVING is a semantic change again (complements the removal
  # block): the digest cannot have been degenerated to a constant.
  expect_true(isTRUE(suppressWarnings(ts_drive_sessions_drop(tok))))
  expect_true(ts_drive_emit_passive_state(token = tok))
  h3 <- ts_drive_read_state()$content_hash
  expect_false(identical(h2, h3))
})

# ── C2 — the activity line : event on hash change, one-shot, zero churn ──────

test_that("C2 events: an actual state.json write queues ONE activity event (one-shot drain); no write queues NOTHING (zero churn, fiche :85/:103)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_pending_activity_events"))
  sb <- .dstv3_sandbox("events")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_dstv3_events_01"
  .dstv3_track(tok, session_id = "sid-events-1", viewer = "visible",
               module = "sc", action = "run_pipeline", armed = TRUE)

  # First emission = a real hash change -> exactly ONE event, carrying what
  # the line renders (D6: the display gate reads `armed` from it).
  expect_true(ts_drive_emit_passive_state(token = tok))
  evs <- ts_drive_pending_activity_events()
  expect_identical(length(evs), 1L)
  ev <- evs[[1]]
  expect_true(all(c("module_key", "action", "hb_age_s", "viewer",
                    "session_id", "armed") %in% names(ev)))
  expect_identical(as.character(ev$module_key), "sc")
  expect_identical(as.character(ev$action), "run_pipeline")
  expect_identical(isTRUE(ev$armed), TRUE)
  expect_identical(as.character(ev$viewer), "visible")
  expect_identical(as.character(ev$session_id), "sid-events-1")
  expect_true(is.numeric(ev$hb_age_s) && ev$hb_age_s >= 0)

  # One-shot: the drain empties the buffer (precedent pending_events(),
  # drive_watcher.R:4907-4911 — read once, then cleared).
  expect_identical(length(ts_drive_pending_activity_events()), 0L)

  # ZERO CHURN: an unchanged content writes NOTHING and queues NOTHING. This
  # is the exact assertion the fiche's "abonné au changement de hash, pas au
  # tick" (:85) and criterion #5 (:113-114) ask for.
  expect_false(ts_drive_emit_passive_state(token = tok))
  expect_identical(length(ts_drive_pending_activity_events()), 0L)

  # A new content change (the action moved) -> exactly one fresh event.
  .dstv3_track(tok, session_id = "sid-events-1", viewer = "visible",
               module = "sc", action = "snapshot", armed = TRUE)
  expect_true(ts_drive_emit_passive_state(token = tok))
  evs2 <- ts_drive_pending_activity_events()
  expect_identical(length(evs2), 1L)
  expect_identical(as.character(evs2[[1]]$action), "snapshot")

  # The disarm write ALSO queues (D6): it is the only way the line can
  # disappear when the agent lets go — without a second subscription.
  .dstv3_track(tok, session_id = "sid-events-1", viewer = "visible",
               module = "sc", action = "snapshot", armed = FALSE)
  expect_true(ts_drive_emit_passive_state(token = tok))
  evs3 <- ts_drive_pending_activity_events()
  expect_identical(length(evs3), 1L)
  expect_identical(isTRUE(evs3[[1]]$armed), FALSE)
})

test_that("C2 whitelist: ts_drive_activity_view() projects ONLY display-safe fields (no token, no root, no pid — badge_view precedent)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_pending_activity_events",
               "ts_drive_activity_view"))
  sb <- .dstv3_sandbox("view")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_dstv3_view_01"
  .dstv3_track(tok, session_id = "sid-view-1", viewer = "visible",
               module = "spatial", action = "set_inputs", armed = TRUE)
  expect_true(ts_drive_emit_passive_state(token = tok))
  ev <- ts_drive_pending_activity_events()[[1]]

  view <- ts_drive_activity_view(ev)
  # Whitelist, not blacklist (drive_allowlist.R:2196-2201): token, root, pid,
  # paths are unreachable from the UI BY CONSTRUCTION.
  expect_false(any(c("token", "session_token", "root", "pid", "path",
                     "payload") %in% names(view)))
  expect_true(all(c("module", "action", "age_s", "viewer", "session_id")
                  %in% names(view)))
  expect_identical(as.character(view$module), "spatial")
  expect_identical(as.character(view$action), "set_inputs")
  expect_identical(as.character(view$session_id), "sid-view-1")
  expect_true(is.numeric(view$age_s) && view$age_s >= 0)
  # A clean app key survives verbatim; free-text junk is bounded (a leaked
  # token is a bare 8+-char run — the row fields are app keys, but the view
  # still bounds anything that is not one).
  ev_junk <- ev; ev_junk$action <- "run_pipeline"
  expect_identical(as.character(ts_drive_activity_view(ev_junk)$action),
                   "run_pipeline")
  ev_long <- ev; ev_long$action <- paste(rep("x", 120), collapse = "")
  expect_lte(nchar(as.character(ts_drive_activity_view(ev_long)$action)), 80L)
})

test_that("C2 i18n: the activity line's fr/en key pair exists in i18n/translation.json (scoped add_i18n_keys.R mechanism, B1 lesson)", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) .dstv3_or(x$fr, ""), character(1))
  en <- vapply(j$translation, function(x) .dstv3_or(x$en, ""), character(1))
  # The line composes i18n$t() with sprintf placeholders (app.R .tr()
  # precedent, app.R:715-717): the fr SOURCE key and its en pair, exactly.
  expect_true(.dstv3_fr_key %in% fr,
              info = "clé FR de la ligne d'activité absente de translation.json")
  idx <- match(.dstv3_fr_key, fr)
  expect_identical(en[[idx]], .dstv3_en_key)
})

# ── C3 — viewer / headless discriminant (fiche :86-87) ───────────────────────

test_that("C3 gate: the activity line renders only OUTSIDE headless, only interactive, only for an armed session (D5)", {
  .dstv3_api(c("ts_drive_activity_visible"))
  # Interactive + visible + armed = the only case that shows.
  expect_true(ts_drive_activity_visible(interactive = TRUE, viewer = "visible",
                                        armed = TRUE))
  # Unarmed = the HUMAN is navigating, not an agent (fiche :14-15).
  expect_false(ts_drive_activity_visible(interactive = TRUE,
                                         viewer = "visible", armed = FALSE))
  # Headless shows NOTHING new (fiche :87 verbatim).
  expect_false(ts_drive_activity_visible(interactive = TRUE,
                                         viewer = "headless", armed = TRUE))
  # The Rscript child without the option does not tick (drive_watcher.R:770-775
  # ; re-mesure §3 :90) — the gate must agree with the real predicate.
  expect_false(ts_drive_activity_visible(interactive = FALSE,
                                         viewer = "visible", armed = TRUE))
  # Tied to the REAL gate, not an ad-hoc boolean: the option override is the
  # gate's own testability seam (:767-775).
  withr::with_options(list(ts.drive.interactive = FALSE), {
    expect_false(ts_drive_activity_visible(interactive = ts_drive_interactive(),
                                           viewer = "visible", armed = TRUE))
  })
  withr::with_options(list(ts.drive.interactive = TRUE), {
    expect_true(ts_drive_activity_visible(interactive = ts_drive_interactive(),
                                          viewer = "visible", armed = TRUE))
  })
  # An "unknown" viewer is NOT headless: the fiche names only headless (D5).
  expect_true(ts_drive_activity_visible(interactive = TRUE, viewer = "unknown",
                                        armed = TRUE))
})

test_that("C3 honesty: a headless session IS recorded in sessions[] (machine truth) but the display gate hides it (display-only, D5)", {
  .dstv3_api(c("ts_drive_sessions_track", "ts_drive_activity_visible"))
  sb <- .dstv3_sandbox("headless")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_dstv3_headless_01"
  .dstv3_track(tok, session_id = "sid-headless-1", viewer = "headless",
               module = "sc", action = "run_pipeline", armed = TRUE)
  expect_true(ts_drive_emit_passive_state(token = tok))
  st <- ts_drive_read_state()
  # The producer does NOT lie by omission: the headless session is on the wire.
  expect_identical(length(st$sessions), 1L)
  expect_identical(st$sessions[[1]]$viewer, "headless")
  expect_identical(st$sessions[[1]]$armed, TRUE)
  # …and the DISPLAY gate keeps it off the panel.
  expect_false(ts_drive_activity_visible(
    interactive = TRUE,
    viewer = as.character(st$sessions[[1]]$viewer),
    armed = isTRUE(st$sessions[[1]]$armed)
  ))
})
