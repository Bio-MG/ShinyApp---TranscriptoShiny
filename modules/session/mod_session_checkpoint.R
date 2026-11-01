# =============================================================================
# modules/session/mod_session_checkpoint.R — ASR-3 : UI des checkpoints de
# session (toggle opt-in + bannière de reprise + ligne d'état) (O29)
# =============================================================================
# Contract: docs/contracts/SESSION_CHECKPOINT_RESTORE_CONTRACT.md — §4 (toggle
# opt-in désactivé par défaut = comportement actuel byte pour byte), §5.1
# (offre non intrusive, JAMAIS d'injection silencieuse), §7.3/§7.5
# (comportements refusés), §8 row ASR-3 (fiche :118-120). Le moteur est
# ASR-1/2 (tools/pkg_skeleton/R/session_checkpoint.R, commits 6d35ef2/65ce906) :
# ce module NE RECALCULE RIEN — la bannière rend le payload $banner du PLAN
# ASR-2 (ts_session_checkpoint_restore_plan()) verbatim, et l'acceptation
# exécute le MÊME plan via ts_session_checkpoint_restore_apply().
#
# Montage app.R (les 3 fragments, même namespace "session_ckpt") :
#   * mod_session_checkpoint_ui()        -> sidebar, section Session (§ décision
#      2 de l'en-tête de test-asr3-ui.R) : hr + h6 + switch + ligne d'état ;
#   * mod_session_checkpoint_banner_ui() -> slot header= du page_navbar (même
#      tagList que le shim i18n et le badge drive, app.R:347-350) ;
#   * mod_session_checkpoint_server()    -> fin de server() (JAMAIS avant la
#      première instruction session$allowReconnect — garde lexical S-RES-1).
#
# Discipline OFF (contrat §4/§7.5) : tant que le toggle n'est pas activé,
# ce module n'appelle AUCUNE fonction du moteur (le scan événementiel vit
# dans le observeEvent du toggle, jamais à l'init ni au tick), ne rend ni
# bannière ni ligne d'état (uiOutput NULL = conteneur vide, empreinte nulle,
# précédent app.R:341-346), n'écrit rien. AUCUN timer/polling (contrat §3.1) :
# le magasin est relu à CHAQUE activation du toggle, point.
#
# 0 CSS www/ (règle post-O13) : primitives bslib/shiny + classes Bootstrap
# existantes et styles inline seulement — aucun fichier CSS, aucune
# injection de style dans le head (gardé par test-asr3-ui.R §9, lexical).
#
# ASR-4 — Rétention & réglages (contrat §3.4 + §8 row ASR-4) : deux réglages
# opérateur (N par dataset, plafond disque EN GIBIOCTETS) rendus dans la zone
# `settings_area` SEULEMENT quand le toggle est ON (OFF byte-pour-byte, §4 :
# aucun input de réglage n'existe sinon ; l'extinction rétablit l'état OFF
# exactement) et publiés dans global_data$session_checkpoint_settings =
# list(retention_n = <entier>, disk_cap_bytes = <octets>) pour le point de
# pose d'écriture (post F08/F02). Décisions (en-tête test-asr4-settings.R) :
# réglages SESSION-SCOPÉS sans fichier de persistance (le toggle lui-même est
# session-scopé, contrat muet) ; défauts = constantes moteur
# TS_SESSION_CHECKPOINT_RETENTION_N / _DISK_CAP_BYTES (session_checkpoint.R:68-69)
# ; bornes N<1 / plafond<=0 -> DÉFAUT RÉTABLI — publier ce que le moteur
# appliquera (il clampe pareil, session_checkpoint.R:832-835) ; changer un
# réglage n'appelle JAMAIS le moteur (aucune purge immédiate — déclenchement
# événementiel gelé §3.1 ; la rétention s'appliquera au prochain publish,
# session_checkpoint.R:577-583). Conversion Gio -> octets EXACTE (x1024^3).
#
# WRITE-SIDE (passe ASR-WIRING, 2026-11-01) — le helper de publication des
# points de pose : à l'ACTIVATION du toggle, ce module enregistre
#   global_data$session_checkpoint_publish <- function(trigger,
#     dataset_ids = character(0), layer2_extras = list())
# et le PURGE à l'extinction (NULL — OFF = byte pour byte, contrat §4). Les
# 7 sites import/pipeline appellent ce helper DERRIÈRE la garde uniforme
#   if (is.function(global_data$session_checkpoint_publish))
#     global_data$session_checkpoint_publish("<trigger>", <ids>)
# (import* -> "import_complete", pipelines -> "pipeline_step_complete",
# déconvolution -> "deconv_complete" + layer2_extras). Architecture gelée
# (en-tête test-asr-wiring.R) : couche 1 = objets PRÉSENTS SEULEMENT, noms
# verbatim de TS_SESSION_CHECKPOINT_LAYER1_TARGETS, tout absent -> saut
# honnête (raison à la ligne d'état), jamais de liste vide fabriquée ;
# couche 2 = relais VERBATIM de global_data$business_state / $manual_pipeline
# (MÊME source que le producteur DSTV-1, JAMAIS de re-dérivation depuis les
# objets), pipeline_steps_completed = nommé vide DOCUMENTÉ (aucun flag par
# étape n'existe aujourd'hui) ; couche 3 = actifs défensifs (bpcells_dir par
# échantillon, kind dir, fallback = raw_dir source d'import d'origine §5.3 ;
# raw_bpcells_dir ; manifeste de référence partagé), existed_at_checkpoint
# stampé moteur ; réglages = retention_n/disk_cap_bytes lus sur
# global_data$session_checkpoint_settings (absentes -> défauts moteur portés
# par les formels du seam, arbitrage T7 option a en tête du test) ; dispatch
# = ts_session_checkpoint_write_async SEUL (refus honnête sans pool §7.1,
# JAMAIS d'écriture synchrone sur le thread principal), collecte du différé
# par promises::then (jamais de collection bloquante), résultat ok/refus ->
# ligne d'état réactive (dernier résultat : horodatage/trigger/id ou raison).
# Boot pool (A8) : à l'activation, init_spatial_daemons() — idempotent
# (.spatial_async_env$daemons_ready, spatial_async.R:236), UN SEUL pool
# (règle §4 ROADMAP) ; échec = honnête, PAS fatal.
#
# i18n : clés = chaînes françaises (shiny.i18n), paires fr/en appendues par le
# mécanisme SCOPÉ (leçon B1/B5/DSTV-3 — le tool complet emporterait ~164
# entrées en attente d'autres lots). Statiques -> i18n$t() au build ; dynamiques
# -> global_data$i18n + re-render sur global_data$language (motif app.R).
#
# ASR-5 — INTÉGRATION DRIVE (contrat SESSION_CHECKPOINT_RESTORE_CONTRACT.md §8
# row ASR-5 : « santé checkpoint visible côté panneau agent », fiche :122 ;
# dégivrée O31 après la livraison DSTV-1) : APRÈS CHAQUE résultat de
# publication (succès id/trigger/horodatage — onFulfilled ; refus §7.1 +
# raisons — branche synchrone ET onRejected ; saut honnête — raison), ce
# module alimente la santé drive de LA session qui publie :
#   ts_drive_sessions_checkpoint(session$userData$drive_token, <résumé borné>)
# Le champ `checkpoint` de la ligne sessions[] de CE token est le support :
# AUCUN fichier nouveau (contrat §5.5/§7.6 — la santé atteint tools/_drive
# par le state.json hash-gated existant au prochain tick), OFF = aucune
# écriture de santé (la purge du publisher, A1, rend la santé no-op aussi).
# SEAM du token (D7, en-tête test-asr5-drive.R) : app.R pose
# session$userData$drive_token au retour de ts_drive_attach — session$token
# littéral est IMPOSSIBLE sur shiny 1.14.0 (ShinySession lock_objects=TRUE,
# mesuré). Les résumés sont des textes machine (SANS i18n$t — leçon T4 :
# jamais de rendu i18n dans un champ machine), bornés par le setter.
# =============================================================================

`%sc_ckpt_or%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── UI — panneau sidebar (toggle + ligne d'état) ─────────────────────────────

mod_session_checkpoint_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    h6(i18n$t("Checkpoints de session"), style = "font-weight: bold;"),
    # Toggle OPT-IN — OFF par défaut (contrat §4). La présence du switch est la
    # seule différence UI visible du mode OFF ; le chemin d'exécution, lui,
    # reste byte-identique (zéro appel moteur, zéro écriture, zéro bannière).
    bslib::input_switch(ns("enabled"),
                        i18n$t("Activer les checkpoints de session"),
                        value = FALSE),
    uiOutput(ns("settings_area")),
    uiOutput(ns("status_line"))
  )
}

# ── UI — bannière de reprise (fragment du slot header= du page_navbar) -------

mod_session_checkpoint_banner_ui <- function(id) {
  uiOutput(NS(id, "restore_banner"))
}

# ── Serveur ───────────────────────────────────────────────────────────────────
#
# @param id        namespace (app.R : "session_ckpt").
# @param global_data le conteneur d'état partagé de la session (reactiveValues =
#                    environnement) : l'acceptation le passe tel quel à
#                    ts_session_checkpoint_restore_apply() (rebinding sur place,
#                    bump F08 exécuté par le plan ASR-2).
# @param store_dir  magasin de checkpoints (défaut : résolution moteur
#                    ts_session_checkpoint_dir() — TRANSCRIPTO_CHECKPOINT_DIR >
#                    données projet). Argument explicite = couture de test
#                    (bac à sable, jamais l'arbre data_sc/ du projet).
mod_session_checkpoint_server <- function(id, global_data, store_dir = NULL) {
  moduleServer(id, function(input, output, session) {

    # L'offre courante : le PLAN ASR-2 complet (items + $banner payload) ou
    # NULL. Une offre est construite UNIQUEMENT à l'activation du toggle.
    offer <- shiny::reactiveVal(NULL)
    # Ligne d'état : le dernier checkpoint complet du magasin (timestamp/scope).
    last_ckpt <- shiny::reactiveVal(NULL)
    # WRITE-SIDE : dernier résultat du publisher (ok : id/trigger/horodatage ;
    # refus §7.1 : raisons du moteur ; saut honnête : raison) — rendu par la
    # ligne d'état. Les raisons SONT des clés i18n pour le saut, du texte
    # moteur verbatim pour un refus ; traduites au rendu (re-render langue).
    last_write <- shiny::reactiveVal(NULL)

    # Traduction dynamique (motif .tr app.R) — repli = la clé française.
    .tr_ui <- function(k) {
      tr <- global_data$i18n
      if (is.null(tr)) return(as.character(k))
      tryCatch(.strip_i18n_html(tr$t(k)), error = function(e) as.character(k))
    }
    # Interpolation nommée (motif .t_fmt global.R) — locale privée pour ne pas
    # dépendre de l'ordre de sourçage de global.R dans les tests.
    .fmt <- function(template, ...) {
      vals <- list(...)
      for (nm in names(vals)) {
        template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
      }
      template
    }
    # Horodatage AFFICHÉ : ISO-8601 UTC du manifeste -> "AAAA-MM-JJ HH:MM UTC".
    # Affichage seul : les champs du payload restent ceux d'ASR-2 (rendus, non
    # recalculés). POSIXct (ligne d'état) et chaîne ISO (payload) acceptés.
    .disp_ts <- function(t) {
      if (inherits(t, "POSIXct")) {
        if (is.na(t)) return(NA_character_)
        return(paste0(format(t, "%Y-%m-%d %H:%M", tz = "UTC"), " UTC"))
      }
      s <- tryCatch(as.character(t)[1], error = function(e) NA_character_)
      if (is.null(s) || is.na(s) || !nzchar(s)) return(NA_character_)
      pt <- tryCatch(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                     error = function(e) as.POSIXct(NA))
      if (is.na(pt)) return(s)  # chaîne inattendue : rendue telle quelle
      paste0(format(pt, "%Y-%m-%d %H:%M", tz = "UTC"), " UTC")
    }

    # ── ASR-5 : alimentation de la santé drive (contrat §8 row ASR-5) ────────
    # Le token de CETTE session est posé par app.R sur userData (D7, en-tête
    # test-asr5-drive.R — session$token littéral impossible, shiny 1.14.0).
    # Le résumé est un texte MACHINE (sans i18n$t, leçon T4) relayant le
    # résultat de publication ; le setter applique strip_html + borne —
    # jamais de ligne fabriquée pour un token inconnu, jamais d'écriture
    # disque ici (canal hash-gated au prochain tick).
    .sc_ckpt_drive_token <- function() {
      tryCatch(as.character(session$userData$drive_token)[1],
               error = function(e) NA_character_)
    }
    .sc_ckpt_feed_health <- function(summary) {
      tok <- .sc_ckpt_drive_token()
      if (is.null(tok) || is.na(tok) || !nzchar(tok)) return(invisible(FALSE))
      tryCatch(
        ts_drive_sessions_checkpoint(tok, summary),
        error = function(e) invisible(FALSE))
    }

    # ── WRITE-SIDE : le helper de publication des points de pose (A1) ────────
    # Enregistré dans global_data À L'ACTIVATION du toggle, purgé à
    # l'extinction. Un appel = UN checkpoint ÉVÉNEMENTIEL (jamais de timer,
    # §3.1/§7.2) : couches 1+2+3 assemblées puis dispatchées au SEAM mirai
    # (A7) — le refus honnête sans pool (§7.1) est la seule alternative à une
    # écriture daemon, jamais d'écriture synchrone sur le thread principal.
    # La collection du différé est promises::then (JAMAIS de [] bloquant) et
    # le résultat (ok/refus) est porté à la ligne d'état réactive.
    .sc_ckpt_publish <- function(trigger, dataset_ids = character(0),
                                 layer2_extras = list()) {
      # Couche 1 (A3) : objets PRÉSENTS SEULEMENT, noms VERBATIM du vocabulaire
      # gelé TS_SESSION_CHECKPOINT_LAYER1_TARGETS. Tout absent -> saut honnête
      # (raison à la ligne d'état), JAMAIS de liste vide fabriquée (le moteur
      # refuse une layer1 vide).
      l1 <- list()
      for (nm in TS_SESSION_CHECKPOINT_LAYER1_TARGETS) {
        v <- tryCatch(global_data[[nm]], error = function(e) NULL)
        if (identical(nm, "spatial_datasets")) {
          if (is.list(v) && length(v) > 0L) l1[[nm]] <- v
        } else if (!is.null(v)) {
          l1[[nm]] <- v
        }
      }
      if (length(l1) == 0L) {
        last_write(list(ok = FALSE, skipped = TRUE,
                        reason = "Aucun objet lourd \u00e0 enregistrer \u2014 checkpoint non pos\u00e9."))
        # ASR-5 : un saut honnête EST un résultat de publication (raison) —
        # la santé drive le porte aussi (jamais un mensonge par omission).
        .sc_ckpt_feed_health(paste0("saut\u00e9 \u00b7 Aucun objet lourd \u00e0 enregistrer \u2014 checkpoint non pos\u00e9."))
        return(invisible(FALSE))
      }
      # Couche 2 (A4) : relais VERBATIM de ce que le producteur DSTV-1 publie
      # (global_data$business_state / $manual_pipeline — JAMAIS de
      # re-dérivation depuis les objets). pipeline_steps_completed : nommé
      # vide DOCUMENTÉ — aucun flag par étape n'existe aujourd'hui. Extras =
      # layer2_extras ou défauts honnêtes du moteur (NULL normalisé).
      l2 <- ts_session_checkpoint_layer2(
        pipeline_steps_completed = character(0),
        manual_pipeline = global_data$manual_pipeline,
        business_state = global_data$business_state,
        deconv_props   = layer2_extras$deconv_props,
        derived_tables = layer2_extras$derived_tables,
        banners        = layer2_extras$banners
      )
      # Couche 3 (A5) : actifs défensifs — par échantillon spatial
      # bpcells_dir (kind "dir", fallback_source = raw_dir, la source
      # d'import d'origine, contrat §5.3) et raw_bpcells_dir ; manifeste de
      # référence partagé. existed_at_checkpoint est stampé MOTEUR.
      .pkg_path <- function(pkg, fld) {
        v <- tryCatch(pkg[[fld]], error = function(e) NULL)
        if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
      }
      l3_assets <- list()
      sd <- tryCatch(global_data$spatial_datasets, error = function(e) NULL)
      if (is.list(sd)) {
        for (smp in names(sd)) {
          pkg <- sd[[smp]]
          if (!is.list(pkg)) next
          bpc  <- .pkg_path(pkg, "bpcells_dir")
          rawb <- .pkg_path(pkg, "raw_bpcells_dir")
          rawd <- .pkg_path(pkg, "raw_dir")
          if (!is.null(bpc)) {
            l3_assets[[paste0(smp, ":bpcells_dir")]] <- list(
              path = bpc, kind = "dir", fallback_source = rawd)
          }
          if (!is.null(rawb)) {
            l3_assets[[paste0(smp, ":raw_bpcells_dir")]] <- list(path = rawb, kind = "dir")
          }
        }
      }
      ref <- tryCatch(global_data$spatial_reference$path, error = function(e) NULL)
      if (is.character(ref) && length(ref) == 1L && nzchar(ref)) {
        l3_assets[["reference_manifest"]] <- list(path = ref, kind = "file")
      }
      l3 <- ts_session_checkpoint_layer3(l3_assets)
      # Réglages (A6) : lus sur la publication ASR-4 ; absentes -> omis (les
      # formels du seam portent les défauts = constantes moteur, arbitrage T7
      # option a — en-tête test-asr-wiring.R).
      st <- tryCatch(global_data$session_checkpoint_settings, error = function(e) NULL)
      w_args <- list(layers = list(layer1 = l1, layer2 = l2, layer3 = l3),
                     dir = store_dir, trigger = trigger,
                     dataset_ids = as.character(dataset_ids))
      if (is.list(st) && !is.null(st$retention_n)) w_args$retention_n <- st$retention_n
      if (is.list(st) && !is.null(st$disk_cap_bytes)) w_args$disk_cap_bytes <- st$disk_cap_bytes
      # Dispatch (A7) : le SEAM unique — refus structuré sans pool (§7.1),
      # différé mirai sinon. Aucun [] bloquant sur ce thread.
      res <- tryCatch(do.call(ts_session_checkpoint_write_async, w_args),
                      error = function(e) list(ok = FALSE, refused = TRUE,
                                               reasons = conditionMessage(e)))
      if (!isTRUE(res$ok)) {
        last_write(list(ok = FALSE, refused = TRUE, trigger = trigger,
                        reasons = as.character(res$reasons %sc_ckpt_or% "raison inconnue")))
        # ASR-5 : refus §7.1 (synchrone) — le résumé du refus (raisons moteur
        # verbatim, texte machine sans i18n) alimente la santé drive.
        .sc_ckpt_feed_health(paste0(
          "refus \u00b7 ", trigger, " \u00b7 ",
          paste(as.character(res$reasons %sc_ckpt_or% "raison inconnue"),
                collapse = " | ")))
        return(invisible(FALSE))
      }
      promises::then(
        res$deferred,
        onFulfilled = function(val) {
          ck_id <- tryCatch(as.character(val$id)[1], error = function(e) NA_character_)
          ck_ts <- tryCatch(as.character(val$manifest$timestamp)[1],
                            error = function(e) NA_character_)
          last_write(list(ok = TRUE, trigger = trigger,
                          id = tryCatch(val$id, error = function(e) NULL),
                          timestamp = tryCatch(val$manifest$timestamp,
                                               error = function(e) NULL)))
          # ASR-5 : succès — résumé id/trigger/horodatage (contrat §8 row
          # ASR-5), texte machine borné par le setter.
          .sc_ckpt_feed_health(paste0(
            "ok \u00b7 ", trigger, " \u00b7 ", ck_id %sc_ckpt_or% "?",
            " \u00b7 ", ck_ts %sc_ckpt_or% "?"))
          invisible(NULL)
        },
        onRejected = function(err) {
          last_write(list(ok = FALSE, refused = TRUE, trigger = trigger,
                          reasons = conditionMessage(err)))
          # ASR-5 : rejet async de la collection — même discipline que le
          # refus synchrone (chaque résultat alimente la santé).
          .sc_ckpt_feed_health(paste0(
            "refus \u00b7 ", trigger, " \u00b7 ", conditionMessage(err)))
          invisible(NULL)
        })
      invisible(TRUE)
    }

    # ── ASR-4 : publication des réglages — AUCUN appel moteur, AUCUNE I/O ────
    # Publie EXACTEMENT ce que le moteur appliquera : N entier (n < 1 -> défaut,
    # même clamp que .sc_apply_retention, session_checkpoint.R:832-833) et
    # plafond en octets (Gio -> octets x1024^3 exact ; plafond <= 0 -> défaut,
    # divergence assumée avec la tolérance de banc du hook moteur, décision T4
    # en tête de test-asr4-settings.R). La rétention elle-même ne tournera
    # qu'au prochain publish d'un checkpoint (point de pose post F08/F02).
    .clamp_retention_n <- function(x) {
      n <- suppressWarnings(as.integer(x)[1])
      if (length(n) != 1L || is.na(n) || n < 1L) TS_SESSION_CHECKPOINT_RETENTION_N else n
    }
    .clamp_cap_bytes <- function(gib) {
      g <- suppressWarnings(as.numeric(gib)[1])
      if (length(g) != 1L || is.na(g) || g <= 0) TS_SESSION_CHECKPOINT_DISK_CAP_BYTES
      else g * 1024^3
    }
    .publish_settings <- function(n_raw, gib_raw) {
      global_data$session_checkpoint_settings <- list(
        retention_n    = .clamp_retention_n(n_raw),
        disk_cap_bytes = .clamp_cap_bytes(gib_raw)
      )
      invisible(NULL)
    }

    # ── Le scan événementiel — la SEULE porte du moteur dans ce module ────────
    # OFF (ou pas encore touché) : AUCUN appel, AUCUN rendu (contrat §7.5).
    # ON : un seul balayage du magasin (list -> dernier checkpoint complet ->
    # plan ASR-2). Un plan refusé = AUCUNE offre (contrat §5.1 : une offre
    # exige un checkpoint validé) — le refus reste consigné dans la ligne
    # d'état, le checkpoint est conservé (contrat §6).
    shiny::observeEvent(input$enabled, {
      if (!isTRUE(input$enabled)) {
        global_data$session_checkpoint_settings <- NULL  # ASR-4 : OFF = empreinte nulle (contrat §4)
        global_data$session_checkpoint_publish <- NULL   # WRITE-SIDE (A1) : OFF -> publisher purgé,
        last_write(NULL)                                 #   aucun site ne peut plus déclencher (§4/§7.5)
        offer(NULL)
        last_ckpt(NULL)
        return(invisible(NULL))
      }
      # ASR-4 : défauts moteur publiés à l'activation (inputs encore absents
      # -> NULL -> défauts). Aucun appel moteur ici (décision ASR-4 n°6).
      .publish_settings(input$retention_n, input$disk_cap_gib)
      # WRITE-SIDE (A8) : boot du pool mirai partagé — init_spatial_daemons()
      # est IDEMPOTENTE (garde .spatial_async_env$daemons_ready,
      # spatial_async.R:236) : UN SEUL pool quel que soit le nombre
      # d'activations (règle §4 ROADMAP). Un échec est HONNÊTE, PAS fatal :
      # l'activation continue et les écritures refuseront honnêtement (§7.1).
      tryCatch(init_spatial_daemons(), error = function(e) invisible(FALSE))
      # WRITE-SIDE (A1) : le helper de publication n'existe que toggle ON —
      # sa purge à l'extinction (branche OFF ci-dessus) rend la garde
      # uniforme des 7 sites no-op (contrat §4 byte pour byte).
      global_data$session_checkpoint_publish <- .sc_ckpt_publish
      lst <- tryCatch(ts_session_checkpoint_list(dir = store_dir),
                      error = function(e) NULL)
      if (is.null(lst) || !is.data.frame(lst) || nrow(lst) == 0L) {
        offer(NULL)
        last_ckpt(NULL)
        return(invisible(NULL))
      }
      newest <- lst[1L, ]  # le moteur liste le plus récent d'abord
      last_ckpt(list(
        timestamp   = newest$timestamp,
        dataset_ids = as.character(unlist(newest$dataset_ids %sc_ckpt_or% character(0)))
      ))
      plan <- tryCatch(ts_session_checkpoint_restore_plan(newest$id, dir = store_dir),
                       error = function(e) NULL)
      if (is.null(plan) || !isTRUE(plan$ok) || isTRUE(plan$refused)) {
        offer(NULL)  # checkpoint non validé : pas d'offre (contrat §5.1/§6)
        return(invisible(NULL))
      }
      offer(plan)
    })

    # ── ASR-4 : zone de réglages — rendue SEULEMENT toggle ON (contrat §4) ────
    # Aucun input de réglage n'existe tant que le toggle est OFF (uiOutput NULL
    # = empreinte nulle, motif ASR-3). Un changement de réglage re-publie
    # SANS rappeler le moteur (décision ASR-4 n°6). 0 CSS www/ : primitives
    # shiny + styles inline seulement.
    shiny::observeEvent(list(input$retention_n, input$disk_cap_gib), {
      if (!isTRUE(input$enabled)) return(invisible(NULL))
      .publish_settings(input$retention_n, input$disk_cap_gib)
    })
    output$settings_area <- shiny::renderUI({
      global_data$language          # re-render au changement de langue
      if (!isTRUE(input$enabled)) return(NULL)
      shiny::tagList(
        shiny::numericInput(session$ns("retention_n"),
                            .tr_ui("Checkpoints conserv\u00e9s par dataset"),
                            value = TS_SESSION_CHECKPOINT_RETENTION_N,
                            min = 1, step = 1),
        shiny::numericInput(session$ns("disk_cap_gib"),
                            .tr_ui("Plafond disque (Gio)"),
                            value = TS_SESSION_CHECKPOINT_DISK_CAP_BYTES / 1024^3,
                            min = 0.1, step = 1),
        shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                   .tr_ui("R\u00e9glages valables pour cette session uniquement."))
      )
    })

    # ── Ligne d'état (pendant que le toggle est ON) ───────────────────────────
    # WRITE-SIDE : enrichie du DERNIER résultat du publisher (saut honnête ->
    # raison ; refus §7.1 -> raisons du moteur verbatim ; ok -> horodatage,
    # trigger, id). Traduite au rendu (.tr_ui) — re-render sur la langue.
    .sc_ckpt_write_line <- function(lw) {
      if (isTRUE(lw$skipped)) {
        shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                   .tr_ui(lw$reason))
      } else if (isTRUE(lw$ok)) {
        shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                   .fmt(.tr_ui("Checkpoint pos\u00e9 \u00e0 {ts}."),
                        ts = .disp_ts(lw$timestamp)),
                   sprintf(" [%s \u00b7 %s]", lw$trigger %sc_ckpt_or% "?",
                           lw$id %sc_ckpt_or% "?"))
      } else {
        shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                   .fmt(.tr_ui("\u00c9chec de l'\u00e9criture du checkpoint : {err}"),
                        err = paste(lw$reasons %sc_ckpt_or% "raison inconnue",
                                    collapse = " | ")))
      }
    }
    output$status_line <- shiny::renderUI({
      global_data$language          # re-render au changement de langue
      if (!isTRUE(input$enabled)) return(NULL)
      lc <- last_ckpt()
      lw <- last_write()
      if (is.null(lc) && is.null(lw)) {
        return(shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                          .tr_ui("Aucun checkpoint disponible.")))
      }
      ck_bit <- if (is.null(lc)) NULL else {
        scope <- if (length(lc$dataset_ids)) paste(lc$dataset_ids, collapse = ", ")
                 else .tr_ui("(aucun dataset)")
        shiny::div(id = "asr3_status_line", class = "small text-muted",
                   style = "font-size:0.72rem; line-height:1.3;",
                   .fmt(.tr_ui("Dernier checkpoint : {ts} \u2014 port\u00e9e : {scope}"),
                        ts = .disp_ts(lc$timestamp), scope = scope))
      }
      shiny::tagList(ck_bit, if (!is.null(lw)) .sc_ckpt_write_line(lw) else NULL)
    })

    # ── Bannière de reprise — rend le payload ASR-2, n'applique RIEN ──────────
    # Les counts/losses/horodatage viennent du $banner du plan (ASR-2) ; ASR-3
    # les rend verbatim, ne les recalcule jamais (contrat §5.1, ask ASR-3).
    output$restore_banner <- shiny::renderUI({
      global_data$language
      if (!isTRUE(input$enabled)) return(NULL)
      pl <- offer()
      if (is.null(pl)) return(NULL)
      b <- pl$banner
      ts_txt <- .disp_ts(b$timestamp)
      losses_bit <- if (length(b$losses)) {
        shiny::tagList(
          shiny::strong(.tr_ui("Pertes :")),
          shiny::tags$ul(style = "font-size:0.75rem; margin:2px 0 4px 0;",
                         lapply(b$losses, function(l) shiny::tags$li(l))))
      } else NULL
      shiny::div(id = "asr3_restore_banner", class = "alert alert-warning",
                 role = "alert",
                 style = "padding:8px 12px; margin-bottom:8px; font-size:0.85rem;",
                 shiny::strong(.fmt(
                   .tr_ui("Reprendre la session du {ts} ({n} jeu(x) de donn\u00e9es)"),
                   ts = ts_txt, n = b$n_datasets)),
                 shiny::div(class = "small", style = "margin:2px 0;",
                            .fmt(.tr_ui("Contenu : {r} \u00e9l\u00e9ment(s) restaurable(s), {s} sans valeur conserv\u00e9e, {l} perte(s)."),
                                 r = b$counts[["restored"]], s = b$counts[["skipped"]],
                                 l = b$counts[["lost"]])),
                 losses_bit,
                 shiny::actionButton(session$ns("restore_accept"),
                                     .tr_ui("Reprendre la session"),
                                     class = "btn-sm btn-primary me-2"),
                 shiny::actionButton(session$ns("restore_decline"),
                                     .tr_ui("Continuer sans restaurer"),
                                     class = "btn-sm btn-outline-secondary"))
    })

    # ── Accepter — l'unique action qui touche `global_data` ───────────────────
    # Le plan est appliqué TEL QUEL (rebinding couche 1/2 + table d'actifs +
    # bump F08 exécuté par apply(), décision ASR-2) ; l'offre est purgée
    # ensuite. Un refus du moteur est notifié honnêtement (contrat §6).
    #
    # Couture reactiveValues : apply() est conçu pour un environnement NUES
    # (rebinding sur place par assign()). Un reactiveValues EST un
    # environnement, mais une enveloppe classée dont les champs vivent dans
    # .values — un assign() brut sur l'enveloppe ne toucherait ni les champs
    # ni la réactivité. Donc : stage dans un env nu (copie des références
    # courantes, dont les epochs que apply() lit pour le bump F08), apply()
    # écrit dedans, puis relais du DELTA dans global_data par le protocole
    # [[<- (la DÉCISION de rebind reste celle du moteur — le relais ne reflète
    # que ce que apply() a réellement écrit, champ par champ).
    shiny::observeEvent(input$restore_accept, {
      pl <- offer()
      if (is.null(pl)) return(invisible(NULL))
      stage <- new.env(parent = emptyenv())
      shiny::isolate({
        for (fld in names(global_data)) {
          assign(fld, global_data[[fld]], envir = stage)
        }
      })
      res <- tryCatch(ts_session_checkpoint_restore_apply(pl, stage),
                      error = function(e) list(ok = FALSE, refused = TRUE,
                                               reasons = conditionMessage(e)))
      offer(NULL)
      if (!isTRUE(res$ok)) {
        shiny::showNotification(
          .fmt(.tr_ui("\u00c9chec de la restauration du checkpoint : {err}"),
               err = paste(res$reasons %sc_ckpt_or% "raison inconnue", collapse = " | ")),
          type = "error", duration = 10)
        return(invisible(NULL))
      }
      after <- as.list(stage)
      for (fld in names(after)) {
        cur <- if (fld %in% names(global_data)) global_data[[fld]] else NULL
        if (is.null(cur) || !identical(cur, after[[fld]])) {
          global_data[[fld]] <- after[[fld]]
        }
      }
      b <- pl$banner
      shiny::showNotification(
        .fmt(.tr_ui("Session restaur\u00e9e depuis le checkpoint du {ts} \u2014 {r} \u00e9l\u00e9ment(s) restaur\u00e9(s), {s} ignor\u00e9(s), {l} perte(s)."),
             ts = .disp_ts(b$timestamp), r = res$counts[["restored"]],
             s = res$counts[["skipped"]], l = res$counts[["lost"]]),
        type = if (length(res$lost)) "warning" else "message", duration = 10)
      invisible(NULL)
    })

    # ── Refuser — tout reste en l'état (contrat §7.3) ─────────────────────────
    # Aucun appel moteur, aucune écriture : la seule conséquence est la purge
    # de l'offre (la bannière disparaît, la ligne d'état reste informative).
    shiny::observeEvent(input$restore_decline, {
      offer(NULL)
      invisible(NULL)
    })

  })
}
