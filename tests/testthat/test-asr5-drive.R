# =============================================================================
# test-asr5-drive.R — ASR-5 RED tests (intégration drive : santé checkpoint
# visible côté panneau agent — O29, DÉGIVRÉ O31)
# =============================================================================
# Contract sources:
#   * docs/contracts/SESSION_CHECKPOINT_RESTORE_CONTRACT.md
#       — §8 row ASR-5 (:150) : « partage du producteur business_state avec
#         DSTV-1 ; santé checkpoint visible côté panneau agent » (fiche :122) ;
#       — §10.2 (:177-182) : le dégivrage suit la livraison du producteur
#         partagé DSTV-1 (O31 DECISIONS.md:108 — « débloque ASR-5 ») ;
#       — §5.5/§7.6 (:100-103, :135) : ASR n'écrit RIEN dans tools/_drive côté
#         identité ; §4 (:77-81) : OFF = byte pour byte.
#   * docs/contracts/DSTV1_SESSION_TRACKER_CONTRACT.md
#       — §3 (:91-106) : discipline hash-gated — « TOUT nouveau champ publié
#         doit entrer au digest, sinon il n'émettrait jamais » (le digest
#         couvre les champs sémantiques des sessions[], DSTV-3 C1 — le champ
#         checkpoint prolonge exactement cette discipline) ;
#       — §6.5 (:218-225) : additivité — champs additifs, sémantique stable ;
#       — §10 (:338-345) : « Toute modification de la liste des champs publiés
#         (§3) ... = mise à jour de CE fichier » — l'évolution ASR-5 est donc
#         une row de §10, citant O29/O31.
#   * Le producteur partagé global_data$business_state est LIVRÉ (DSTV-1/1b,
#     commits ccbc340/826c167/33ae378) ; le couplage producteur (identité
#     layer2 <-> DSTV-1) est déjà testé par test-asr-wiring.R T4 — CETTE passe
#     porte la SANTÉ CÔTÉ PANNEAU AGENT.
#
# QU'EST ASSERTABLE HEADLESS (honnêteté du garde, motif test-asr-wiring.R et
# contrat §9) :
#   * Le chemin ok du publisher (pool mirai RÉEL + collection promises d'un
#     deferred) n'est PAS headless-assertable — précédent test-asr-wiring.R
#     (« Le chemin async COMPLET ... n'est PAS headless-assertable : aucun pool
#     n'est démarré en test »). Le résultat ok est donc épinglé LEXICALEMENT :
#     la branche onFulfilled de promises::then dans .sc_ckpt_publish appelle
#     ts_drive_sessions_checkpoint (AST, motif test-drive-session-tracker-
#     feed.R / T8 test-asr-wiring.R). Les chemins REFUS (probe FALSE, forcé
#     déterministe — même scénario que test-session-checkpoint-engine.R:504-515)
#     et OFF sont comportementaux (testServer, motif test-asr-wiring.R T1-T6).
#   * Le rendu NAVIGATEUR du panneau n'est pas headless : la projection
#     machine (événement d'activité + vue whitelistée) est épinglée
#     comportementalement (précédent test-drive-sessions-panel.R C2 — « pure
#     R/ layer ») et le rendu app.R l'est lexicalement (motif T8
#     test-asr-wiring.R : ancre unique + fenêtre ±N lignes).
#   * PAS de return() dans une expression testServer (rlang::eval_tidy —
#     mesuré à l'exécution RED de test-asr-wiring.R) : gardes if (ok_reg).
#
# ARCHITECTURE GELÉE (décisions de coordination de la passe — chaque test
# ci-dessous en est l'application) :
#   D1. Setter drive : ts_drive_sessions_checkpoint(token, summary) met à
#       jour le champ machine « checkpoint » de la ligne sessions[] de CE
#       token SEULEMENT. Disciplines module/action reprises à l'identique :
#       .ts_drive_strip_html (leçon T4 — une entrée i18n-rendue ne traverse
#       JAMAIS brute) + borne de taille (gelée ici : 200 caractères — le
#       résumé id/trigger/horodatage et les raisons moteur tiennent sous la
#       borne, une ligne secondaire de panneau ne porte pas plus). Token
#       vide/inconnu -> invisible(FALSE) et AUCUNE ligne fabriquée (motif
#       ts_drive_sessions_track :3990, ts_drive_sessions_drop :4033).
#       Le champ vaut "" avant toute publication (défaut honnête, motif
#       business_state :3670-3671 — jamais un mensonge ni un NA). Le setter
#       N'ÉCRIT RIEN sur disque : la santé passe par le canal hash-gated
#       existant au prochain tick (D2/D6), jamais par un nouveau chemin
#       (contrat DSTV §6.3).
#   D2. Digest : ts_drive_compute_state_hash couvre le champ checkpoint des
#       lignes sessions[] — l'émetteur le passe dans ses champs sémantiques
#       (drive_watcher.R:4242-4249, liste d'intersect étendue) sinon un
#       changement de santé n'émettrait JAMAIS (discipline §3 DSTV, précédent
#       « digest étendu » business_state DSTV-1 et sessions[] DSTV-3 C1).
#   D3. Alimentation module : après CHAQUE résultat de publication du helper
#       .sc_ckpt_publish (succès id/trigger/horodatage — branche onFulfilled
#       de promises::then ; refus §7.1 + raison — branche synchrone
#       !isTRUE(res$ok) ET onRejected), le module appelle
#       ts_drive_sessions_checkpoint(<token de la session>, <résumé borné>).
#       OFF -> RIEN (aucune écriture de santé, contrat §4/§7.5 : la purge du
#       publisher rend la garde des sites no-op y compris pour la santé).
#   D4. Panneau agent : l'événement d'activité (C2, UN par écriture réelle)
#       porte le checkpoint de la ligne ; ts_drive_activity_view le projette
#       dans sa whitelist (précédent ts_drive_badge_view drive_allowlist.R:
#       2196-2212 — le résumé y subit les règles de rédaction : un token
#       fuité ne monte pas à l'écran) ; app.R rend le champ sur la ligne
#       d'activité (ligne secondaire, style existant, ancre
#       output$drive_activity_line <- shiny::renderUI). Le libellé visible
#       nouveau est une paire i18n fr/en (append scopé, leçon B1) :
#       fr « Checkpoint : %s » / en « Checkpoint: %s ».
#   D5. Contrats : la row d'évolution §10 du contrat DSTV cite ASR-5
#       (O29/O31) et le champ checkpoint ; le freeze test DSTV concerné
#       (test-drive-sessions-panel.R, schéma C1) est ÉTENDU au champ (C8 :
#       tout contrat cité par un test — la row §10 crée l'exigence, le freeze
#       la porte).
#   D6. Identité drive préservée (contrat §5.5/§7.6) : la santé ne crée
#       AUCUN fichier — elle vit dans la ligne sessions[] existante et
#       n'atteint le disque QUE par le state.json hash-gated existant. Pas
#       d'arm.json/ready.json/scenario.json/result.json touchés, pas de
#       fichier « checkpoint » nouveau.
#   D7. SEAM du token dans le module — DÉVIATION MESURÉE de la décision
#       coordination « session$token » (remontée et arbitrée ICI, motif T7
#       test-asr-wiring.R : « remonté par cette passe RED, réponse
#       enregistrée ici ») : shiny 1.14.0 — ShinySession est R6
#       lock_objects=TRUE (mesuré : shiny:::ShinySession$lock_objects), un
#       champ arbitraire session$token <- ... est IMPOSSIBLE sur une session
#       réelle, et la session testServer refuse $<- (« Attempted to assign
#       value on session proxy », mesuré). Seam retenu, compatible réel ET
#       mock : app.R pose session$userData$drive_token <- .drive$token au
#       retour de ts_drive_attach (userData est un env public des deux
#       classes — mesuré assign+lecture dans moduleServer), le module lit
#       session$userData$drive_token. La SÉMANTIQUE gelée (le setter reçoit
#       le token de LA session qui publie, jamais un token deviné) est
#       inchangée.
#
# Target API (GREEN implementer's spec) :
#   tools/pkg_skeleton/R/drive_watcher.R :
#     ts_drive_sessions_checkpoint(token, summary)
#       — invisible(TRUE) si la ligne du token a été mise à jour (checkpoint
#         = .ts_drive_strip_html(summary), borné à 200 caractères) ;
#         invisible(FALSE) token vide/inconnu, rien de fabriqué ; champ
#         checkpoint = "" sur toute ligne avant publication (défaut honnête,
#         posé par ts_drive_sessions_track ET projeté par ts_drive_sessions)
#         ; le refresh d'émetteur (ts_drive_sessions_track sur une ligne
#         existante) PRÉSERVE le checkpoint posé — sinon chaque tick
#         l'effacerait.
#     ts_drive_emit_passive_state : l'intersect sémantique des lignes du
#       digest (:4242-4249) inclut « checkpoint », et l'événement d'activité
#       (:4291-4298) le porte.
#     ts_drive_activity_view : whitelist + « checkpoint » (règles de
#       rédaction du sanitizer inchangées — un run token-shapé ne monte pas).
#     ts_drive_compute_state_hash : le payload sessions[] reçu couvre le
#       champ (aucun changement de signature nécessaire — la liste est
#       hachée en bloc ; le test T3a est une garde de régression déjà verte à
#       RED, ce qui porte la discipline est le pin comportemental T3b/T3c).
#   modules/session/mod_session_checkpoint.R : chaque résultat de publication
#     alimente la santé VIA LE HELPER .sc_ckpt_feed_health (garde token +
#     tryCatch — une santé ne prend jamais le module bas), qui appelle
#     ts_drive_sessions_checkpoint(session$userData$drive_token, <résumé
#     borné>) — 4 sites dans .sc_ckpt_publish : saut honnête (raison), refus
#     synchrone §7.1 (raisons verbatim), onFulfilled (ok : trigger/id/
#     horodatage), onRejected (raisons) ; résumés en texte MACHINE (sans
#     i18n$t, leçon T4) ; AUCUN appel hors résultat de publication (OFF =
#     rien).
#   app.R : session$userData$drive_token <- .drive$token (au retour de
#     ts_drive_attach) + rendu du champ sur la ligne d'activité.
#
# ÉVIDENCE RED->GREEN (place réservée — le jalon GREEN complétera la ligne
# GREEN, ne rien réécrire d'autre) :
#   RED  (2026-11-01, « Rscript tools/run_tests.R asr5 » depuis la racine du
#      dépôt, exit = 1) : TOTAL (filter=asr5, files=1) failed=20 passed=15
#      error=4 skipped=0, fichier test-asr5-drive.R uniquement — 7 blocs/7
#      rouges. Inventaire mesuré : 6 gardes .asr5_api nommant l'API cible
#      absente (ts_drive_sessions_checkpoint — T1/T2/T3/T4/T5/T7) + 4 erreurs
#      « could not find function » du même manque (T1/T3/T5/T7, chaque bloc
#      portant déjà sa garde) ; T2 :4 (champ checkpoint absent de la ligne ET
#      du fil — défaut honnête non livré) ; T4 :8 (santé non alimentée au
#      refus 0 != 1, ligne non portée, OFF ligne != "", onFulfilled/onRejected
#      sans appel — n_calls=0, seam app.R userData absent) ; T6 :3 (§10 du
#      contrat DSTV sans ASR-5 ni checkpoint, freeze DSTV non étendu).
#      Gardes de régression déjà vertes à RED (15) : T3a mécanisme digest
#      (le payload sessions est haché en bloc — ce qui PORTE la discipline
#      est le pin comportemental T3b), T3d zéro churn inchangé, T4 OFF 0
#      appel de santé, ancres uniques (ts_drive_attach, drive_activity_line,
#      §10), publisher enregistré, refus §7.1 sans magasin, collection
#      promises::then unique. Quatre défauts de harnais mesurés puis
#      corrigés PENDANT la mise au point RED (ne pas réintroduire) :
#      expect_length()/expect_setequal()/expect_gte() n'acceptent PAS info=
#      dans cette version de testthat (expect_identical(..., info=) /
#      expect_true(..., info=) — motif test-asr-wiring.R) ; la recherche AST
#      du helper .sc_ckpt_publish doit parcourir en PROFONDEUR (helper
#      imbriqué dans moduleServer — ts_ast_find, pas une boucle top-level) ;
#      le wrapper de santé doit tolérer l'API absente (garde exists() — un
#      échec net par bloc, pas un « object not found » de fixture).
#   GREEN (2026-11-01, implémentation posée dans le périmètre du jalon :
#      tools/pkg_skeleton/R/drive_watcher.R — TS_DRIVE_CHECKPOINT_MAX_CHARS,
#      setter ts_drive_sessions_checkpoint, champ checkpoint par défaut ""
#      et PRÉSERVÉ au refresh dans ts_drive_sessions_track, projection wire
#      ts_drive_sessions, intersect sémantique du digest + événement
#      d'activité portant le champ, whitelist ts_drive_activity_view ;
#      modules/session/mod_session_checkpoint.R — helpers .sc_ckpt_drive_
#      token/.sc_ckpt_feed_health + 4 sites d'alimentation ; app.R — pose de
#      session$userData$drive_token au retour de ts_drive_attach + ligne
#      secondaire « Checkpoint : %s » du rendu drive_activity_line ;
#      docs/contracts/DSTV1_SESSION_TRACKER_CONTRACT.md — row d'évolution
#      ASR-5 en §10 ; tests/testthat/test-drive-sessions-panel.R — schéma
#      gelé C1 étendu au septième champ ; i18n/translation.json — append
#      SCOPÉ via add_i18n_keys.R au lot seul (tool restauré après run)) :
#      Porte 1 « Rscript tools/run_tests.R asr5 » -> exit 0, TOTAL
#      (filter=asr5, files=1) failed=0 passed=82 error=0. Porte 2
#      « Rscript tools/run_tests.R "asr|session-checkpoint|drive-session|
#      i18n" » -> exit 0, TOTAL (files=12) failed=0 passed=1048 error=0.
#      Porte 3 « Rscript tools/check_conventions.R » -> 0 erreur,
#      2 avertissements C8 (les DEUX préexistants, hors lane ASR — maintenus,
#      pas aggravés ; les contrats ASR et DSTV sont référencés par des tests
#      et ne sont pas signalés). UN ajustement mineur du harnais,
#      justifié et mesuré à la mise au point GREEN : le pin lexical T4c
#      porte désormais sur le helper .sc_ckpt_feed_health appelé par CHAQUE
#      branche de résultat (+ le helper appelant le setter) au lieu du nom
#      du setter directement dans .sc_ckpt_publish — l'implémentation livrée
#      route la santé par ce helper (garde token + tryCatch : une santé ne
#      prend jamais le module bas) ; la sémantique D3 est inchangée.
# =============================================================================

# Ensure allowlist and drive watcher are loaded (same bootstrap as
# test-drive-sessions-panel.R:131-141 — R/core first, skeleton tree fallback).
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

# Le module de checkpoint est testé avec le MOTEUR RÉEL (jamais un substitut,
# motif test-asr-wiring.R : le moteur ASR-1/2 ET le module ASR-3/4 sont
# sourcés directement).
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("modules/session/mod_session_checkpoint.R")

# i18n : repli no-op si global.R n'a pas fourni le Translator (motif
# test-asr-wiring.R — les clés SONT les chaînes françaises).
if (!exists("i18n", envir = globalenv(), inherits = FALSE)) {
  assign("i18n", list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() "fr"
  ), envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

# ── Constantes gelées de la passe ─────────────────────────────────────────────

# D1 — borne de taille du résumé (la ligne « checkpoint » est une ligne
# secondaire de panneau : résumé id/trigger/horodatage ou raison moteur).
.asr5_max_chars <- 200L

# D2 — les champs sémantiques des lignes sessions[] passés au digest APRÈS
# ASR-5 (les cinq DSTV-3 C1 + le champ santé).
.asr5_sess_digest_fields <- c("session_id", "viewer", "module", "action",
                              "armed", "checkpoint")

# D4 — libellé visible nouveau de la ligne d'activité (fr = clé source,
# en = paire, motif .dstv3_fr_key/.dstv3_en_key).
.asr5_fr_key <- "Checkpoint : %s"
.asr5_en_key <- "Checkpoint: %s"

# ── Fixtures ──────────────────────────────────────────────────────────────────

#' Crisp RED failure naming the missing target API (motif .dstv3_api,
#' test-drive-sessions-panel.R:157-167 — un « could not find function » est un
#' leurre ; le garde nomme la cible GREEN).
.asr5_api <- function(fns) {
  missing <- fns[!vapply(fns, function(f) exists(f, mode = "function"), logical(1))]
  if (length(missing)) {
    fail(paste0(
      "ASR-5 target API absent from tools/pkg_skeleton/R/ (RED expected): ",
      paste(missing, collapse = ", "),
      " — see the target-API block in this file's header"
    ))
  }
  invisible(TRUE)
}

#' Own temp root + TRANSCRIPTO_DRIVE_ROOT sandbox for one test (motif
#' .dstv3_sandbox, test-drive-sessions-panel.R:174-197 — le tools/_drive LIVE
#' du working tree n'est jamais touché ; aucune app Shiny n'est bootée ici).
.asr5_sandbox <- function(tag, envir = parent.frame()) {
  root <- file.path(tempdir(), paste0("tsdrive-asr5-", tag, "-",
                                      as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE,
             showWarnings = FALSE)
  withr::local_envvar(TRANSCRIPTO_DRIVE_ROOT = root, .local_envir = envir)
  ts_drive_boot(root)
  ts_drive_clear_errors()
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  if (exists("ts_drive_sessions_clear", mode = "function")) {
    ts_drive_sessions_clear()
  }
  if (exists("ts_drive_pending_activity_events", mode = "function")) {
    ts_drive_pending_activity_events()  # drain: a fresh test starts silent
  }
  root
}

#' Register one tracked session with sane defaults (motif .dstv3_track).
.asr5_track <- function(token, session_id, viewer = "visible", module = "",
                        action = "", armed = FALSE, hb_at = Sys.time()) {
  ts_drive_sessions_track(token, session_id = session_id, viewer = viewer,
                          module = module, action = action, armed = armed,
                          hb_at = hb_at)
}

#' The wire row of one session_id (NULL if absent).
.asr5_row_of <- function(session_id) {
  rows <- ts_drive_sessions()
  hits <- rows[vapply(rows, function(r) {
    identical(as.character(r$session_id), session_id)
  }, logical(1))]
  if (length(hits) == 0L) NULL else hits[[1]]
}

#' The row's checkpoint field, coerced to character (NULL -> NA marker).
.asr5_ckpt_of <- function(session_id) {
  r <- .asr5_row_of(session_id)
  if (is.null(r)) return(NA_character_)
  as.character(r$checkpoint)[1]
}

# ── Sentinelle boot pool + wrapper probe (motif test-asr-wiring.R : la
#    sentinelle est assignée dans GLOBALENV — le module résout le symbole dans
#    son Closure -> globalenv ; le probe PAR DÉFAUT du write_async est résolu
#    dans globalenv au moment de l'appel, le wrapper FALSE est donc consulté). --

.asr5_pool <- new.env(parent = emptyenv())
.asr5_pool$calls <- 0L

assign("init_spatial_daemons", function(...) {
  .asr5_pool$calls <- .asr5_pool$calls + 1L
  invisible(TRUE)
}, envir = globalenv())

.asr5_probe_false <- function() {
  saved <- new.env(parent = emptyenv())
  nm <- "ts_session_checkpoint_daemons_active"
  saved[[nm]] <- get(nm, envir = globalenv())
  assign(nm, function(...) FALSE, envir = globalenv())
  withr::defer({
    for (n in ls(saved)) assign(n, saved[[n]], envir = globalenv())
  }, envir = parent.frame())
  invisible(TRUE)
}

# ── Compteur de la santé : WRAPPER DÉLÉGANT autour du setter RÉEL (motif
#    .asrw_wrap_engines — on compte les invocations, on capture les arguments
#    qui traversent la couture, on ne remplace JAMAIS la fonction : la ligne
#    sessions[] est mise à jour par le VRAI setter). IDEMPOTENT : un second
#    appel dans le même bloc ne ré-emballe PAS le wrapper (sinon un appel du
#    module serait compté deux fois — wrapper sur wrapper) ; il remet juste le
#    compteur à zéro. ----------------------------------------------------------------

.asr5_health <- new.env(parent = emptyenv())
.asr5_health$calls <- list()
.asr5_health$real <- NULL

.asr5_wrap_health <- function() {
  nm <- "ts_drive_sessions_checkpoint"
  if (!exists(nm, mode = "function", envir = globalenv())) {
    # API cible absente (RED) : le garde .asr5_api a déjà nommé la cible —
    # le harnais ne doit pas avorter sur un « object not found » (motif
    # .dstv3_sandbox : un échec net par bloc, pas un follow-on de fixture).
    .asr5_health$calls <- list()
    return(invisible(FALSE))
  }
  if (is.null(.asr5_health$real)) {
    .asr5_health$real <- get(nm, envir = globalenv())
    real <- .asr5_health$real
    assign(nm, function(...) {
      args <- list(...)
      .asr5_health$calls[[length(.asr5_health$calls) + 1L]] <-
        list(token = args[[1]], summary = args[[2]])
      real(...)
    }, envir = globalenv())
    withr::defer({
      if (!is.null(.asr5_health$real)) {
        assign(nm, .asr5_health$real, envir = globalenv())
        .asr5_health$real <- NULL
        .asr5_health$calls <- list()
      }
    }, envir = parent.frame())
  }
  .asr5_health$calls <- list()
  invisible(TRUE)
}

#' global_data minimal du module (motif .asrw_gd — sc_obj présent pour que la
#' couche 1 du publisher ne saute pas honnêtement avant le dispatch).
.asr5_gd <- function() {
  shiny::reactiveValues(
    sc_obj = data.frame(x = 1:3), sc_obj_epoch = 1L,
    bulk_obj = NULL, bulk_obj_epoch = 0L,
    spatial_obj = NULL, spatial_datasets = list(),
    manual_pipeline = NULL, business_state = NULL,
    spatial_reference = NULL,
    session_checkpoint_settings = NULL,
    language = "fr", i18n = NULL
  )
}

#' Accès sûr à un output testServer (motif .asrw_output — un signal absent
#' devient un marqueur lisible, jamais une erreur qui avorte le bloc).
.asr5_output <- function(session, id) {
  tryCatch(
    paste(collapse = "", as.character(session$output[[id]])),
    error = function(e) "__ASR5_OUTPUT_NON_DEFINI__"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# T1 — le setter : champ posé sur la BONNE ligne, borné + strip_html, token
# vide/inconnu -> invisible(FALSE) et rien de fabriqué (D1).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T1 setter: checkpoint lands on THAT token's sessions[] row, strip_html + bounded; unknown/empty token -> invisible(FALSE), nothing fabricated (D1)", {
  .asr5_api(c("ts_drive_sessions_checkpoint"))
  sb <- .asr5_sandbox("setter")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_asr5_t1_01"
  .asr5_track(tok, session_id = "sid-asr5-t1-aaaa", viewer = "visible",
              module = "sc", action = "run_pipeline", armed = TRUE)
  tok2 <- "tok_asr5_t1_02"
  .asr5_track(tok2, session_id = "sid-asr5-t1-bbbb", viewer = "headless",
              module = "bulk", armed = FALSE)

  # (a) La santé pose le champ sur la ligne du token VISÉ, verbatim (un
  #     identifiant d'action comme "import_complete" — 15 caractères —
  #     SURVIT au setter : la discipline du setter est strip_html + borne
  #     (D1, « mêmes disciplines que module/action »), PAS la rédaction
  #     token-shapée du sanitize qui ne concerne que la vue UI).
  summ <- "Checkpoint pos\u00e9 \u00e0 2026-11-01T10:00:00Z. [import_complete \u00b7 \u00e9criture mirai v\u00e9rifi\u00e9e]"
  res <- ts_drive_sessions_checkpoint(tok, summ)
  expect_true(isTRUE(res),
              info = "le setter doit rendre invisible(TRUE) quand la ligne du token est mise à jour")
  expect_identical(.asr5_ckpt_of("sid-asr5-t1-aaaa"), summ,
                   info = paste("le résumé doit poser le champ checkpoint de la ligne",
                                "sessions[] du token VISÉ, verbatim (strip_html + borne",
                                "seulement — pas de rédaction au niveau machine)"))
  # La ligne de L'AUTRE session n'est pas touchée.
  expect_identical(.asr5_ckpt_of("sid-asr5-t1-bbbb"), "",
                   info = "la santé d'une session ne doit jamais teinter une autre ligne")

  # (b) Leçon T4 : une entrée i18n-RENDUE ne traverse jamais brute.
  ts_drive_sessions_checkpoint(tok,
    "<span class=\"i18n\" data-key=\"Single-Cell\">Single-Cell</span>")
  expect_identical(.asr5_ckpt_of("sid-asr5-t1-aaaa"), "Single-Cell",
                   info = paste("le champ machine checkpoint doit être DÉBARQUÉ de tout",
                                "rendu i18n (leçon T4, .ts_drive_strip_html) — jamais de",
                                "HTML dans un champ machine (contrat DSTV §3 A1)"))
  chars <- .asr5_ckpt_of("sid-asr5-t1-aaaa")
  expect_false(grepl("<[a-zA-Z!/][^>]*>", chars),
               info = "aucune balise ne survit dans le champ checkpoint")

  # (c) Borne de taille : 5000 caractères -> champ borné (D1 : 200).
  junk <- paste(rep("x", 5000), collapse = "")
  ts_drive_sessions_checkpoint(tok, junk)
  ck <- .asr5_ckpt_of("sid-asr5-t1-aaaa")
  expect_true(nchar(ck) <= .asr5_max_chars,
              info = sprintf("le champ checkpoint doit être borné (gelé : %d caractères) ; mesuré : %d",
                             .asr5_max_chars, nchar(ck)))
  expect_true(nzchar(ck), info = "un résumé borné reste NON VIDE (pas d'effacement)")

  # (d) Token inconnu -> invisible(FALSE) et AUCUNE ligne fabriquée (motif
  #     ts_drive_sessions_track :3990 / ts_drive_sessions_drop :4033).
  n_before <- length(ts_drive_sessions())
  expect_false(isTRUE(ts_drive_sessions_checkpoint("tok_asr5_absent", "x")),
               info = "token inconnu -> invisible(FALSE) — jamais de ligne fabriquée")
  expect_identical(length(ts_drive_sessions()), n_before,
                   info = "un appel sur token inconnu ne doit PAS créer de ligne")
  expect_null(.asr5_row_of("tok_asr5_absent"),
              info = "aucune ligne ne porte le token inconnu")

  # (e) Token vide / NA -> invisible(FALSE), rien de fabriqué.
  expect_false(isTRUE(ts_drive_sessions_checkpoint("", "x")),
               info = "token vide -> invisible(FALSE)")
  expect_false(isTRUE(ts_drive_sessions_checkpoint(NULL, "x")),
               info = "token NULL -> invisible(FALSE)")
  expect_false(isTRUE(ts_drive_sessions_checkpoint(NA_character_, "x")),
               info = "token NA -> invisible(FALSE)")
  expect_identical(length(ts_drive_sessions()), n_before,
                   info = "les appels sur token vide/NA ne fabriquent rien")
})

# ══════════════════════════════════════════════════════════════════════════════
# T2 — défaut honnête : une ligne neuve porte checkpoint "" — jamais un
# mensonge, jamais un NA, sur la table ET sur le fil (D1).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T2 honest default: a fresh row carries checkpoint \"\" — on the wire table AND in state.json, never NA/NULL/fabricated (D1)", {
  .asr5_api(c("ts_drive_sessions_checkpoint"))
  sb <- .asr5_sandbox("default")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_asr5_t2_01"
  .asr5_track(tok, session_id = "sid-asr5-t2-aaaa", viewer = "visible",
              module = "sc", armed = FALSE)

  # (a) La table fil porte le champ, à "" avant toute publication.
  row <- .asr5_row_of("sid-asr5-t2-aaaa")
  expect_false(is.null(row), info = "la ligne fraîchement tracquée doit exister")
  expect_true("checkpoint" %in% names(row),
              info = paste("la ligne sessions[] doit TOUJOURS porter le champ checkpoint",
                           "(additif complet, motif business_state :3670 — défauts",
                           "présents, jamais un champ absent)"))
  expect_identical(as.character(row$checkpoint), "",
                   info = paste("défaut honnête : \"\" avant toute publication — ni NULL,",
                                "ni NA, ni un texte inventé (jamais un mensonge)"))

  # (b) Le document émis porte le même défaut (le fil ne ment pas mieux).
  expect_true(ts_drive_emit_passive_state(token = tok))
  st <- ts_drive_read_state()
  expect_false(is.null(st$sessions) || length(st$sessions) != 1L)
  expect_true("checkpoint" %in% names(st$sessions[[1]]),
              info = "le document state.json porte le champ checkpoint (additif, §6.5 DSTV)")
  expect_identical(as.character(st$sessions[[1]]$checkpoint), "",
                   info = "le défaut honnête traverse jusqu'au fil")
})

# ══════════════════════════════════════════════════════════════════════════════
# T3 — digest : deux états différant SEULEMENT par checkpoint -> hashes
# différents (discipline §3 DSTV : sinon la santé n'émettrait jamais) ; et
# zéro churn conservé quand la santé ne bouge pas (D2).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T3 digest: two states differing ONLY by checkpoint emit different hashes; unchanged health stays silent (zero churn, DSTV §3 discipline + O11)", {
  .asr5_api(c("ts_drive_sessions_checkpoint"))
  sb <- .asr5_sandbox("digest")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_asr5_t3_01"
  hb <- Sys.time() - 5
  .asr5_track(tok, session_id = "sid-asr5-t3-aaaa", viewer = "visible",
              module = "sc", action = "run_pipeline", armed = TRUE, hb_at = hb)
  expect_true(ts_drive_emit_passive_state(token = tok))
  h1 <- ts_drive_read_state()$content_hash

  # (a) Garde de régression (déjà verte à RED) : le digest hache les lignes
  #     sessions[] en bloc — deux listes différant par checkpoint diffèrent.
  #     Ce qui PORTE la discipline est le pin comportemental (b) : c'est
  #     l'éMETTEUR qui choisit les champs passés au digest.
  sd0 <- list(list(session_id = "s", viewer = "visible", module = "sc",
                   action = "run_pipeline", armed = TRUE))
  sd1 <- list(list(session_id = "s", viewer = "visible", module = "sc",
                   action = "run_pipeline", armed = TRUE,
                   checkpoint = "ok \u00b7 import_complete"))
  expect_false(identical(
    ts_drive_compute_state_hash("tok", "sc", "idle", list(), sessions = sd0),
    ts_drive_compute_state_hash("tok", "sc", "idle", list(), sessions = sd1)),
    info = paste("le digest doit couvrir le champ checkpoint des lignes sessions[]",
                 "(payload sessions en bloc — garde de régression du mécanisme)"))

  # (b) Pin comportemental (les dents de la discipline §3) : un changement de
  #     la SEULE santé est un changement de contenu -> l'émetteur ÉCRIT et le
  #     hash bouge. Sinon un changement de checkpoint n'émettrait jamais.
  ck <- "Checkpoint pos\u00e9 \u00e0 2026-11-01T10:00:00Z. [pipeline_step_complete \u00b7 \u00e9criture mirai v\u00e9rifi\u00e9e]"
  expect_true(isTRUE(ts_drive_sessions_checkpoint(tok, ck)))
  expect_true(ts_drive_emit_passive_state(token = tok),
              info = paste("un changement SEULement checkpoint doit émettre — sinon la",
                           "santé n'atteindrait jamais le fil (discipline §3 DSTV)"))
  h2 <- ts_drive_read_state()$content_hash
  expect_false(identical(h1, h2),
               info = "le hash du document émis doit différer après un changement de santé")

  # (c) La ligne émise porte le résumé (avec les champs sémantiques intacts).
  st2 <- ts_drive_read_state()
  expect_identical(as.character(st2$sessions[[1]]$checkpoint), ck)
  expect_identical(as.character(st2$sessions[[1]]$module), "sc")
  expect_identical(as.character(st2$sessions[[1]]$action), "run_pipeline")
  expect_setequal(names(st2$sessions[[1]]),
                  c(.asr5_sess_digest_fields, "hb_age_s"))

  # (d) Zéro churn conservé : la santé INCHANGÉE ne réécrit pas (O11 + D2 —
  #     le digest étendu ne doit pas dégénérer en émission par tick).
  expect_false(ts_drive_emit_passive_state(token = tok),
               info = "santé inchangée -> aucune réécriture (hash-gated inchangé, O11)")
  expect_identical(ts_drive_read_state()$content_hash, h2)
})

# ══════════════════════════════════════════════════════════════════════════════
# T4 — alimentation module : refus (probe FALSE) -> résumé du refus sur la
# ligne du token ; ok -> résumé id/trigger/horodatage (pin lexical, chemin
# pool+promises non headless) ; OFF -> aucune écriture de santé (D3/D7).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T4 module feed: refusal (probe FALSE) -> refusal summary on the session's row; OK -> summary pinned lexically in onFulfilled; OFF -> no health write (D3/D7)", {
  .asr5_api(c("ts_drive_sessions_checkpoint"))
  sb <- .asr5_sandbox("feed")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_asr5_t4_01"
  .asr5_track(tok, session_id = "sid-asr5-t4-aaaa", viewer = "visible",
              module = "sc", action = "run_pipeline", armed = FALSE)
  store <- tempfile(pattern = "ts_asr5_store_")
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)

  # ── (a) REFUS §7.1 déterministe (probe FALSE) : le module alimente la
  #       santé de LA session avec un résumé borné du refus. ──
  .asr5_wrap_health()
  .asr5_probe_false()
  gd <- .asr5_gd()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    # D7 : le seam du token — app.R pose userData$drive_token (mesuré :
    # session$token littéral impossible, shiny 1.14.0).
    session$userData$drive_token <- "tok_asr5_t4_01"
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- is.function(gd$session_checkpoint_publish)
    expect_true(ok_reg,
                info = paste("toggle ON : le publisher doit être enregistré (prérequis",
                             "du pin de la santé — même mécanique que T1 test-asr-wiring)"))
    # PAS de return() dans une expression testServer (mesuré RED asr-wiring).
    if (ok_reg) {
      gd$session_checkpoint_publish("pipeline_step_complete")
      session$flushReact()
    }
  })
  expect_identical(length(.asr5_health$calls), 1L,
                   info = paste("un refus de publication doit alimenter la santé EXACTEMENT",
                                "une fois (D3 : après CHAQUE résultat) ; appels :",
                                length(.asr5_health$calls)))
  if (length(.asr5_health$calls) == 1L) {
    call <- .asr5_health$calls[[1]]
    expect_identical(as.character(call$token), tok,
                     info = paste("la santé porte le token de LA session qui publie (D3) —",
                                  "jamais un token deviné ni vide"))
    s <- tryCatch(as.character(call$summary)[1], error = function(e) NA_character_)
    expect_true(is.character(s) && !is.na(s) && nzchar(s),
                info = "le résumé du refus est non vide")
    expect_true(nchar(s) <= .asr5_max_chars,
                info = sprintf("le résumé est borné (gelé : %d) ; mesuré : %s",
                               .asr5_max_chars, nchar(s)))
    expect_true(grepl("no mirai daemon pool", s, fixed = TRUE),
                info = paste("le résumé du refus relaie la raison du moteur (§7.1 verbatim,",
                             "motif ligne d'état du module)"))
  }
  # La ligne sessions[] du token porte le résumé (le VRAI setter a tourné).
  ck <- .asr5_ckpt_of("sid-asr5-t4-aaaa")
  expect_true(is.character(ck) && !is.na(ck) && grepl("no mirai daemon pool", ck, fixed = TRUE),
              info = paste("la ligne sessions[] de la session porte le résumé du refus —",
                           "c'est ELLE que le panneau agent lira (D4)"))
  # Un refus n'écrit rien au magasin (garde de régression asr-wiring).
  expect_false(dir.exists(store), info = "un refus §7.1 ne crée pas le magasin")

  # ── (b) OFF : aucune écriture de santé (contrat §4/§7.5 — la purge du
  #       publisher rend la garde des sites no-op Y COMPRIS la santé). ──
  .asr5_wrap_health()
  .asr5_track("tok_asr5_t4_02", session_id = "sid-asr5-t4-bbbb",
              viewer = "visible", module = "sc", armed = FALSE)
  gd_off <- .asr5_gd()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd_off,
                         store_dir = tempfile(pattern = "ts_asr5_off_")), expr = {
    session$userData$drive_token <- "tok_asr5_t4_02"
    # Toggle JAMAIS activé : un site garde no-op (motif uniforme des 7 sites).
    if (is.function(gd_off$session_checkpoint_publish))
      gd_off$session_checkpoint_publish("import_complete")
  })
  expect_identical(length(.asr5_health$calls), 0L,
                   info = "OFF -> AUCUNE écriture de santé (contrat §4 : rien, jamais)")
  ck_off <- .asr5_ckpt_of("sid-asr5-t4-bbbb")
  expect_identical(ck_off, "",
                   info = "OFF : la ligne reste au défaut honnête \"\"")

  # ── (c) OK — pin LEXICAL (le chemin pool mirai réel + collection promises
  #       n'est pas headless-assertable, précédent test-asr-wiring) : la
  #       branche onFulfilled de promises::then appelle le setter, comme les
  #       deux branches de refus. AST, motif test-drive-session-tracker-feed.R. ──
  pub <- NULL
  mod_ast <- ts_ast_parse("modules/session/mod_session_checkpoint.R")
  # .sc_ckpt_publish est IMBRIQUÉE dans moduleServer : parcours en profondeur
  # (ts_ast_find, invariant (1) helper-ast.R), pas seulement le top-level.
  pub <- ts_ast_find(mod_ast, function(y) {
    if (!ts_ast_is_call_to(y, "<-")) return(FALSE)
    l <- tryCatch(as.list(y), error = function(e) NULL)
    !is.null(l) && length(l) >= 3 &&
      identical(deparse(l[[2]]), ".sc_ckpt_publish")
  })
  expect_false(is.null(pub), info = "le helper .sc_ckpt_publish doit rester identifiable dans le module")
  pub_txt <- ts_ast_deparse(pub)
  # Ajustement mineur GREEN (justifié, 2026-11-01) : l'implémentation livrée
  # route l'alimentation par le helper .sc_ckpt_feed_health (garde token +
  # tryCatch, jamais une santé qui prend le module bas) — le pin porte donc
  # sur CHAQUE branche de résultat appelant le helper, ET sur le helper
  # appelant le setter (la sémantique D3 est inchangée : chaque résultat
  # alimente la santé).
  n_calls <- lengths(regmatches(pub_txt, gregexpr(".sc_ckpt_feed_health", pub_txt, fixed = TRUE)))
  # NB : expect_gte() n'accepte pas info= dans cette version de testthat
  # (mesuré — même défaut de harnais que expect_length/expect_setequal) :
  # comparaison portée par expect_true(info=).
  expect_true(n_calls >= 3L,
              info = paste("D3 : APRÈS CHAQUE résultat — la branche de refus synchrone,",
                           "onFulfilled (ok) ET onRejected doivent alimenter la santé via le",
                           "helper (>= 3 sites) ; mesuré :", n_calls))
  feed <- ts_ast_find(mod_ast, function(y) {
    if (!ts_ast_is_call_to(y, "<-")) return(FALSE)
    l <- tryCatch(as.list(y), error = function(e) NULL)
    !is.null(l) && length(l) >= 3 &&
      identical(deparse(l[[2]]), ".sc_ckpt_feed_health")
  })
  expect_false(is.null(feed),
               info = "le helper .sc_ckpt_feed_health doit rester identifiable dans le module")
  expect_true(grepl("ts_drive_sessions_checkpoint", ts_ast_deparse(feed), fixed = TRUE),
              info = paste("le helper de santé doit appeler le setter",
                           "ts_drive_sessions_checkpoint (token de LA session, résumé borné)"))
  then_call <- ts_ast_find(pub, function(y) ts_ast_is_call_to(y, "then"))
  expect_false(is.null(then_call), info = "la collection promises::then reste la seule (jamais de [] bloquant)")
  if (!is.null(then_call)) {
    tl <- tryCatch(as.list(then_call), error = function(e) NULL)
    tl_names <- names(tl) %||% rep("", length(tl))
    for (br in c("onFulfilled", "onRejected")) {
      k <- match(br, tl_names)
      expect_false(is.na(k), info = sprintf("la branche %s de promises::then reste présente", br))
      if (!is.na(k)) {
        br_txt <- ts_ast_deparse(tl[[k]])
        expect_true(grepl(".sc_ckpt_feed_health", br_txt, fixed = TRUE),
                    info = paste("D3 : la branche", br, "doit alimenter la santé",
                                 "(ok -> résumé id/trigger/horodatage ; rejet -> raisons)"))
      }
    }
  }

  # ── (d) Seam D7 lexical : app.R pose le token de la session au retour de
  #       l'attach (sinon la santé n'a pas de token en production). ──
  app_txt <- paste(readLines(file.path(ts_project_root(), "app.R"),
                             warn = FALSE), collapse = "\n")
  attach_line <- grep("ts_drive_attach\\(session, input\\)",
                      readLines(file.path(ts_project_root(), "app.R"), warn = FALSE),
                      value = TRUE)
  expect_identical(length(attach_line), 1L,
                info = "l'ancre ts_drive_attach(session, input) doit rester unique dans app.R")
  expect_true(grepl("userData\\$drive_token\\s*<-\\s*\\.drive\\$token", app_txt),
              info = paste("D7 : app.R doit poser session$userData$drive_token <-",
                           ".drive$token au retour de ts_drive_attach (seam mesuré —",
                           "session$token littéral impossible sur shiny 1.14.0)"))
})

# ══════════════════════════════════════════════════════════════════════════════
# T5 — panneau : le rendu sessions[] expose le champ — l'événement d'activité
# le porte, la vue whitelistée le projette (rédaction : un token fuité ne
# monte pas), app.R rend la ligne secondaire, paire i18n fr/en (D4).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T5 panel: the activity event carries checkpoint, the whitelisted view projects it (redaction rules intact), app.R renders the secondary line, fr/en i18n pair (D4)", {
  .asr5_api(c("ts_drive_sessions_checkpoint", "ts_drive_activity_view",
              "ts_drive_pending_activity_events"))
  sb <- .asr5_sandbox("panel")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  tok <- "tok_asr5_t5_01"
  .asr5_track(tok, session_id = "sid-asr5-t5-aaaa", viewer = "visible",
              module = "sc", action = "run_pipeline", armed = TRUE)
  ck <- "Checkpoint pos\u00e9 \u00e0 2026-11-01T10:00:00Z. [import_complete]"
  expect_true(isTRUE(ts_drive_sessions_checkpoint(tok, ck)))
  expect_true(ts_drive_emit_passive_state(token = tok))

  # (a) L'événement d'activité (UN par écriture réelle, zéro churn) PORTE le
  #     checkpoint — c'est le seul flux qui atteint le panneau sans nouveau
  #     polling (abonné au changement de hash, fiche DSTV :85).
  evs <- ts_drive_pending_activity_events()
  expect_identical(length(evs), 1L, info = "une écriture réelle -> UN événement")
  ev <- evs[[1]]
  expect_true("checkpoint" %in% names(ev),
              info = "l'événement d'activité doit porter le champ checkpoint")
  expect_identical(as.character(ev$checkpoint), ck,
                   info = "l'événement porte le résumé de la ligne (machine truth)")

  # (b) La vue whitelistée projette le champ (précédent badge_view :
  #     whitelist, pas blacklist — et les règles de rédaction tiennent).
  view <- ts_drive_activity_view(ev)
  expect_true("checkpoint" %in% names(view),
              info = "la vue du panneau doit exposer le champ checkpoint (whitelist étendue)")
  vc <- suppressWarnings(as.character(view$checkpoint)[1])
  expect_true(!is.na(vc) && nchar(vc) <= .asr5_max_chars,
              info = "la vue borne le champ comme le reste du panneau")
  # Défense en profondeur : un run token-shapé fuité dans un résumé ne monte
  # pas à l'écran (règle badge_sanitize, drive_allowlist.R:2192).
  ev_leak <- ev
  ev_leak$checkpoint <- "ok \u00b7 fuite tokabcd1234 dans le r\u00e9sum\u00e9"
  vleak <- ts_drive_activity_view(ev_leak)$checkpoint
  expect_false(grepl("tokabcd1234", as.character(vleak), fixed = TRUE),
               info = "un token fuité via un résumé ne doit pas monter côté UI")
  # Un résumé propre (aucun run >= 8 alphanum) reste reconnaissable.
  ev_ok <- ev
  ev_ok$checkpoint <- "ok \u00b7 10:00 UTC"
  expect_identical(as.character(ts_drive_activity_view(ev_ok)$checkpoint),
                   "ok \u00b7 10:00 UTC",
                   info = "un résumé propre traverse la vue reconnaissable")

  # (c) app.R rend le champ sur la ligne d'activité (ligne secondaire, style
  #     existant) — pin lexical, ancre unique, fenêtre ±30 lignes (motif T8
  #     test-asr-wiring.R).
  app_lines <- readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
  anchor <- grep("output\\$drive_activity_line <- shiny::renderUI", app_lines)
  expect_identical(length(anchor), 1L,
                info = "l'ancre output$drive_activity_line <- shiny::renderUI doit rester unique")
  window <- paste(app_lines[anchor:min(length(app_lines), anchor + 30)], collapse = "\n")
  expect_true(grepl("checkpoint", window, fixed = TRUE),
              info = paste("le rendu de la ligne d'activité doit exposer le champ",
                           "checkpoint (ligne secondaire, style existant — D4)"))

  # (d) La paire i18n fr/en du libellé visible nouveau (append scopé, leçon
  #     B1 ; motif C2 i18n test-drive-sessions-panel.R:488-499).
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else as.character(x$fr), character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else as.character(x$en), character(1))
  expect_true(.asr5_fr_key %in% fr,
              info = "clé FR « Checkpoint : %s » absente de translation.json")
  idx <- match(.asr5_fr_key, fr)
  expect_identical(en[[idx]], .asr5_en_key,
                   info = "la paire EN du libellé checkpoint doit accompagner la clé FR")
})

# ══════════════════════════════════════════════════════════════════════════════
# T6 — contrats : la row d'évolution §10 du contrat DSTV cite ASR-5 + le champ
# checkpoint ; le freeze test DSTV concerné est étendu au champ (C8 : tout
# contrat cité par un test) (D5).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T6 contracts: the DSTV contract §10 evolution row cites ASR-5 (O29/O31) + the checkpoint field; the DSTV freeze test is extended to the field (C8)", {
  # (a) Contrat DSTV — §10 (Évolution) : « Toute modification de la liste des
  #     champs publiés = mise à jour de CE fichier » — l'ajout de sessions[]
  #     .checkpoint est exactement cela.
  dstv_path <- file.path(ts_project_root(), "docs", "contracts",
                         "DSTV1_SESSION_TRACKER_CONTRACT.md")
  dstv_lines <- readLines(dstv_path, warn = FALSE)
  s10 <- grep("^## 10\\.", dstv_lines)
  expect_identical(length(s10), 1L, info = "la section §10 du contrat DSTV doit rester unique")
  tail_txt <- paste(dstv_lines[s10:length(dstv_lines)], collapse = "\n")
  expect_true(grepl("ASR-5", tail_txt, fixed = TRUE),
              info = paste("la row d'évolution §10 du contrat DSTV doit citer ASR-5",
                           "(O29/O31 — l'ajout du champ checkpoint est une modification",
                           "de la liste des champs publiés, §10 verbatim)"))
  expect_true(grepl("checkpoint", tail_txt, ignore.case = TRUE),
              info = "la row d'évolution §10 doit nommer le champ checkpoint ajouté")

  # (b) Le freeze test DSTV concerné (schéma C1 de test-drive-sessions-panel.R)
  #     est étendu au champ — C8 : tout contrat cité par un test.
  freeze_path <- file.path(ts_project_root(), "tests", "testthat",
                           "test-drive-sessions-panel.R")
  freeze_txt <- paste(readLines(freeze_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("\"checkpoint\"", freeze_txt, fixed = TRUE),
              info = paste("C8 : le freeze test DSTV (test-drive-sessions-panel.R,",
                           "schéma C1 six champs) doit être ÉTENDU au champ checkpoint —",
                           "sinon la row §10 du contrat ne serait citée par aucun test"))
})

# ══════════════════════════════════════════════════════════════════════════════
# T7 — identité drive préservée : ASR-5 n'écrit RIEN dans tools/_drive côté
# identité (contrat §5.5/§7.6) — la santé vit dans la ligne sessions[] et
# n'atteint le disque QUE par le state.json hash-gated existant (D6).
# ══════════════════════════════════════════════════════════════════════════════

test_that("T7 identity: the health path writes NOTHING to tools/_drive beyond the existing hash-gated state.json channel — no identity file, no new file (D6, contract §5.5/§7.6)", {
  .asr5_api(c("ts_drive_sessions_checkpoint"))
  sb <- .asr5_sandbox("identity")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)
  drv <- dirname(ts_drive_path("state.json"))

  tok <- "tok_asr5_t7_01"
  .asr5_track(tok, session_id = "sid-asr5-t7-aaaa", viewer = "visible",
              module = "sc", armed = FALSE)
  # NB : expect_setequal() n'accepte pas info= dans cette version de testthat
  # (mesuré à l'exécution RED — même défaut de harnais que expect_length,
  # corrigé ici par expect_identical(sort(), sort(), info=), motif
  # test-asr-wiring.R « expect_length() n'accepte pas info= »).
  expect_identical(sort(list.files(drv, all.files = TRUE, no.. = TRUE)),
                   sort(character(0)),
                   info = "préambule : la sandbox ne doit rien contenir avant le test")

  # (a) Le setter (et la pose de santé en général) N'ÉCRIT RIEN : pas de
  #     fichier de santé nouveau, pas de fichier d'identité.
  expect_true(isTRUE(ts_drive_sessions_checkpoint(tok,
    "Checkpoint pos\u00e9 \u00e0 2026-11-01T10:00:00Z. [import_complete]")))
  expect_identical(sort(list.files(drv, all.files = TRUE, no.. = TRUE)),
                   sort(character(0)),
                   info = paste("contrat §5.5/§7.6 : la santé passe par la ligne sessions[]",
                                "existante — le setter n'écrit AUCUN fichier dans tools/_drive"))

  # (b) La santé n'atteint le disque QUE par le canal hash-gated EXISTANT :
  #     après émission, state.json est le SEUL fichier — arm.json /
  #     ready.json / scenario.json / result.json / tout fichier « checkpoint »
  #     nouveau restent absents.
  expect_true(ts_drive_emit_passive_state(token = tok))
  files <- list.files(drv, all.files = TRUE, no.. = TRUE)
  expect_identical(sort(files), sort("state.json"),
                   info = paste("la santé doit voyager par le state.json hash-gated existant",
                                "(O11) — aucun fichier nouveau ; présents :",
                                paste(files, collapse = ", ")))
  expect_false(file.exists(file.path(drv, "arm.json")),
               info = "AUCUNE écriture d'identité (contrat §5.5 : ASR n'écrit rien côté identité)")
  expect_false(file.exists(file.path(drv, "ready.json")),
               info = "la santé ne doit pas toucher au battement d'identité ready.json")
  expect_false(file.exists(file.path(drv, "result.json")),
               info = "la santé ne doit pas toucher au verdict result.json")
  expect_false(any(grepl("checkpoint|asr", files, ignore.case = TRUE)),
               info = "aucun fichier de santé nouveau ne doit apparaître dans tools/_drive")

  # (c) Le document émis reste le canal complet inchangé — la santé est une
  #     ligne de sessions[], pas un document parallèle.
  st <- ts_drive_read_state()
  expect_false(is.null(st$sessions))
  expect_identical(as.character(st$sessions[[1]]$checkpoint),
                   "Checkpoint pos\u00e9 \u00e0 2026-11-01T10:00:00Z. [import_complete]")
  expect_false("checkpoint_health" %in% names(st) || "checkpoint" %in% names(st),
               info = "PAS de champ top-level de santé : la santé vit dans la ligne sessions[] (D6)")
})
