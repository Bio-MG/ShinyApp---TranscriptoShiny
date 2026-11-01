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
# i18n : clés = chaînes françaises (shiny.i18n), paires fr/en appendues par le
# mécanisme SCOPÉ (leçon B1/B5/DSTV-3 — le tool complet emporterait ~164
# entrées en attente d'autres lots). Statiques -> i18n$t() au build ; dynamiques
# -> global_data$i18n + re-render sur global_data$language (motif app.R).
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

    # ── Le scan événementiel — la SEULE porte du moteur dans ce module ────────
    # OFF (ou pas encore touché) : AUCUN appel, AUCUN rendu (contrat §7.5).
    # ON : un seul balayage du magasin (list -> dernier checkpoint complet ->
    # plan ASR-2). Un plan refusé = AUCUNE offre (contrat §5.1 : une offre
    # exige un checkpoint validé) — le refus reste consigné dans la ligne
    # d'état, le checkpoint est conservé (contrat §6).
    shiny::observeEvent(input$enabled, {
      if (!isTRUE(input$enabled)) {
        offer(NULL)
        last_ckpt(NULL)
        return(invisible(NULL))
      }
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

    # ── Ligne d'état (pendant que le toggle est ON) ───────────────────────────
    output$status_line <- shiny::renderUI({
      global_data$language          # re-render au changement de langue
      if (!isTRUE(input$enabled)) return(NULL)
      lc <- last_ckpt()
      if (is.null(lc)) {
        return(shiny::div(class = "small text-muted", style = "font-size:0.72rem;",
                          .tr_ui("Aucun checkpoint disponible.")))
      }
      scope <- if (length(lc$dataset_ids)) paste(lc$dataset_ids, collapse = ", ")
               else .tr_ui("(aucun dataset)")
      shiny::div(id = "asr3_status_line", class = "small text-muted",
                 style = "font-size:0.72rem; line-height:1.3;",
                 .fmt(.tr_ui("Dernier checkpoint : {ts} \u2014 port\u00e9e : {scope}"),
                      ts = .disp_ts(lc$timestamp), scope = scope))
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
