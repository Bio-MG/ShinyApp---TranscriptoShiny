# =============================================================================
# test-asr4-settings.R -- ASR-4 : rétention & réglages (O29) — RED-first
# =============================================================================
# POURQUOI CE FICHIER
#
# Jalon ASR-4 (contrat SESSION_CHECKPOINT_RESTORE_CONTRACT.md : §3.4
# « Rétention — les N derniers checkpoints par dataset (défaut : 3) + plafond
# disque configurable » ; §4 « Désactivé = comportement actuel, byte pour
# byte » ; §7 comportements REFUSÉS ; §8 row ASR-4 — assertable headless
# « Oui »). Le MOTEUR est déjà complet et testé (jalon ASR-1/2 :
# tools/pkg_skeleton/R/session_checkpoint.R:67-71 — constantes
# TS_SESSION_CHECKPOINT_RETENTION_N = 3L et
# TS_SESSION_CHECKPOINT_DISK_CAP_BYTES = 4294967296 ; hooks de rétention
# .sc_apply_retention / ts_session_checkpoint_retention :830-891 ; tests
# test-session-checkpoint-engine.R:380-531) — CE FICHIER NE RETESTE PAS LE
# MOTEUR. Il teste la COUCHE RÉGLAGES ajoutée au module ASR-3
# (modules/session/mod_session_checkpoint.R) : deux réglages opérateur (N par
# dataset, plafond disque) rendus pendant que le toggle est ON et publiés dans
# global_data$session_checkpoint_settings, pour le futur point de pose
# d'écriture (post F08/F02, O29 DECISIONS.md:106).
#
# QU'EST ASSERTABLE HEADLESS (honnêteté du garde, motif test-asr3-ui.R:15-29) :
#   * testServer couvre la logique serveur et le HTML des outputs, pas un
#     navigateur : la mise en forme fine des numericInput dans le DOM réel
#     n'est pas affirmée (ids + valeurs par défaut rendus, c'est tout).
#   * Les valeurs publiées sont observables dans global_data (reactiveValues
#     = environnement) ; les appels moteur sont comptés par des WRAPPERS
#     déléguants autour du moteur RÉEL (motif test-asr3-ui.R:175-199 — on
#     compte les invocations, on ne remplace jamais le moteur).
#   * Avant le jalon GREEN, un output non défini lève dans testServer
#     (« output that hasn't been defined yet ») : .asr4_output() transforme
#     ce signal en échec d'assertion lisible au lieu d'avorter le bloc.
#
# OPEN-POINT DECISIONS (placements silencieux du contrat/fiche — tranchés
# conservativement par ASR-4, chacun documenté) :
#   1. SURFACE : les réglages vivent DANS le module ASR-3 déjà monté par
#      app.R (aucun montage app.R nouveau — app.R est hors de portée du
#      jalon). Zone de réglages = uiOutput `settings_area` du panneau
#      « Checkpoints de session », rendue SEULEMENT quand le toggle est ON
#      (motif ASR-3 : uiOutput NULL = conteneur vide, empreinte nulle,
#      test-asr3-ui.R:49-51) — c'est ce qui préserve OFF byte-pour-byte
#      (contrat §4) : aucun input de réglage n'existe tant que le toggle est
#      OFF, et l'extinction du toggle rétablit l'état OFF exactement.
#   2. IDENTIFIANTS ET FORME PUBLIÉE : inputs `retention_n` (checkpoints
#      conservés par dataset) et `disk_cap_gib` (plafond disque EN
#      GIBIOCTETS) dans le namespace "session_ckpt" ; publication =
#      global_data$session_checkpoint_settings, liste EXACTEMENT
#      list(retention_n = <entier>, disk_cap_bytes = <numérique, en octets>).
#      retention_n est publié ENTIER (5L, pas 5) ; disk_cap_bytes est le
#      nombre d'octets exact.
#   3. RÉGLAGES SESSION-SCOPÉS, SANS FICHIER DE PERSISTANCE : le contrat est
#      muet sur la persistance des réglages et le toggle opt-in est lui-même
#      session-scopé (rechargement = session neuve, contrat §1) — un réglage
#      persistant serait incohérent avec un toggle qui ne l'est pas, et tout
#      fichier de persistance est interdit au jalon. La publication repart
#      des défauts moteur à chaque session neuve. Garde : le module ne
#      définit aucune primitive d'écriture disque (assertion T2).
#   4. UNITÉ DU PLAFOND = GIO (GiB), CONVERSION EXACTE x1024^3 : la constante
#      moteur est en octets (session_checkpoint.R:69 — 4294967296 = 4 GiB) ;
#      l'unité opérateur Gio est lisible (4 Gio, pas 4294967296) et la
#      publication reconvertit exactement — 2 Gio -> 2147483648 octets,
#      jamais une approximation en 10^9.
#   5. T4 (BORNES) — DÉCISION : DÉFAUT RÉTABLI, pas refus structuré. Le
#      moteur lui-même défend ses hooks : n < 1 -> TS_SESSION_CHECKPOINT_
#      RETENTION_N, cap non numérique -> défaut (session_checkpoint.R:832-835)
#      ; la couche réglages publie honnêtement CE QUE LE MOTEUR APPLIQUERA —
#      publier une valeur que le moteur écraserait serait malhonnête, et un
#      numericInput n'a pas de canal de « refus structuré ». Divergence
#      assumée et documentée : le hook moteur TOLÈRE un cap volontairement
#      petit (sémantique de starvation, testée test-session-checkpoint-
#      engine.R:455-462 avec cap = 1) ; la surface réglages, elle, rétablit
#      le défaut pour cap <= 0 — un plafond de 0 octet atteint par un pas de
#      saisie est un état absurde côté opérateur, pas un banc d'essai.
#   6. CHANGER UN RÉGLAGE N'APPELLE JAMAIS LE MOTEUR : aucune purge immédiate.
#      Le vocabulaire de déclenchement est gelé (contrat §3.1 :
#      import_complete / pipeline_step_complete / deconv_complete / manual —
#      aucun « settings_changed ») et une purge destructive déclenchée par un
#      pas de numericInput contournerait le déclenchement événementiel (§7.2).
#      La rétention s'appliquera au prochain checkpoint écrit (le moteur
#      l'exécute après chaque publish, session_checkpoint.R:577-583) avec les
#      valeurs publiées ici. Garde : zéro appel moteur lors des changements
#      (T3) et magasin bac à sable JAMAIS créé.
#
# ÉVIDENCE RED->GREEN (place réservée — le jalon GREEN complétera la ligne
# GREEN, ne rien réécrire d'autre) :
#   RED  (2026-11-01, « Rscript tools/run_tests.R asr4 » depuis la racine du
#      dépôt, exit = 1) : TOTAL failed=20 passed=13 error=0 skipped=0,
#      fichier test-asr4-settings.R uniquement. Échecs par famille —
#      surface  :1 (id="session_ckpt-settings_area" absent du panneau,
#      test-asr4-settings.R:211) ;
#      T1       :7 (publication NULL != list(retention_n = 3L,
#                 disk_cap_bytes = 4294967296) :235 ; sync constantes
#                 :241/:244 ; ids/value des inputs :250/:253/:256/:258 — les
#                 constantes moteur gelées :226/:228 passent) ;
#      T2       :2 (settings_area non défini hors toggle :282/:315 — le reste
#                 de la discipline OFF passe déjà : zéro appel moteur :275,
#                 settings NULL :280, magasin jamais créé :288, zéro écriture
#                 disque du module :298, extinction sans rappel :307/:312/:317) ;
#      T3       :3 (N=5 -> NULL != list(retention_n = 5L, ...) :336 ; cap 2 Gio
#                 -> NULL != list(..., 2147483648) :341 ; conversion exacte
#                 .asr4_gib :345 — les gardes zéro-appel/magasin passent) ;
#      T4       :4 (N=0/-2 -> NULL != 3L :380/:383 ; cap 0/-1 -> NULL != 4 GiB
#                 :390 ; liste finale :393) ;
#      T5 i18n  :3 (les 3 clés FR absentes de translation.json :410) ;
#      T5 style :0 (garde lexical :426 passe — branche www/ vacueuse : www/
#                 n'existe pas au dépôt, même forme que test-asr3-ui.R:418).
#   GREEN (2026-11-01) : au début de l'acte de correction, l'implémentation
#      était ABSENTE du dépôt (git log au commit ASR-3 2693423, zéro token
#      ASR-4 dans modules/ et translation.json — les « portes rouges après
#      implémentation » reproduisaient l'état RED) ; l'implémentation a donc
#      été posée dans le périmètre du jalon :
#      modules/session/mod_session_checkpoint.R (uiOutput settings_area +
#      .publish_settings — clamps n<1 / plafond<=0 -> défauts moteur,
#      conversion Gio -> octets x1024^3 exacte, publication
#      global_data$session_checkpoint_settings, purge à l'extinction du
#      toggle, ZÉRO appel moteur) et i18n/translation.json (append scopé des
#      3 clés fr/en). Re-mesure :
#      - « Rscript tools/run_tests.R asr4 » -> exit = 0, TOTAL failed=0
#        passed=36 error=0 (7 blocs, T1-T5 tous verts) ;
#      - régression « Rscript tools/run_tests.R asr4 session-checkpoint
#        asr3 i18n » -> exit = 0, TOTAL (filter=asr4|session-checkpoint|
#        asr3|i18n, files=5) failed=0 passed=388 error=0 skipped=0
#        (asr3-ui 74, asr4-settings 36, i18n-integrity 15, moteur 128,
#        restore 135).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("modules/session/mod_session_checkpoint.R")

# i18n : global.R fournit le vrai Translator à l'exécution ; les tests reçoivent
# le repli no-op (même sémantique que global.R, motif test-asr3-ui.R:71-77) si
# absent — les clés SONT les chaînes françaises, donc $t() repli = clé elle-même.
if (!exists("i18n", envir = globalenv(), inherits = FALSE)) {
  assign("i18n", list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() "fr"
  ), envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

# ── Clés i18n ASR-4 (fr = clé, en = paire) — la surface visible des réglages --
#    La MÊME liste que le test i18n ci-dessous : toute chaîne visible d'un
#    réglage doit vivre ici et dans i18n/translation.json (append scopé).

.asr4_keys <- list(
  c("Checkpoints conserv\u00e9s par dataset", "Checkpoints kept per dataset"),
  c("Plafond disque (Gio)", "Disk cap (GiB)"),
  c("R\u00e9glages valables pour cette session uniquement.",
    "Settings apply to this session only.")
)

# ── Fixture : un chemin de magasin bac à sable JAMAIS créé — la couche
#    réglages ne doit ni lire, ni écrire, ni créer le magasin (décision 6). ---

.asr4_store <- function() tempfile(pattern = "ts_asr4_store_")

# ── Compteur d'appels moteur : WRAPPERS déléguants autour du moteur RÉEL
#    (motif test-asr3-ui.R:175-199) — on compte les invocations sans jamais
#    remplacer le moteur. Surface couverte = tout ce que la couche réglages
#    pourrait raisonnablement appeler (scan ASR-3 + hooks rétention + écritures). --

.asr4_counter <- new.env(parent = emptyenv())
.asr4_counter$calls <- character(0)

.asr4_engine_fns <- c("ts_session_checkpoint_list",
                      "ts_session_checkpoint_restore_plan",
                      "ts_session_checkpoint_restore_apply",
                      "ts_session_checkpoint_retention",
                      "ts_session_checkpoint_write",
                      "ts_session_checkpoint_write_async")

.asr4_wrap_engines <- function() {
  .asr4_counter$calls <- character(0)
  saved <- new.env(parent = emptyenv())
  for (nm in .asr4_engine_fns) {
    real <- get(nm, envir = globalenv())
    saved[[nm]] <- real
    assign(nm, local({
      nm_ <- nm; real_ <- real
      function(...) {
        .asr4_counter$calls <- c(.asr4_counter$calls, nm_)
        real_(...)
      }
    }), envir = globalenv())
  }
  withr::defer({
    for (nm in ls(saved)) assign(nm, saved[[nm]], envir = globalenv())
  }, envir = parent.frame())
  invisible(saved)
}

# ── Accès sûr à un output testServer : avant le jalon GREEN l'output
#    settings_area n'est pas défini et l'accès direct lève ("output that
#    hasn't been defined yet") — le garde transforme ce signal en marqueur
#    lisible dans l'assertion, au lieu d'avorter le bloc en erreur. ----------

.asr4_output <- function(session, id) {
  tryCatch(
    paste(collapse = "", as.character(session$output[[id]])),
    error = function(e) "__ASR4_OUTPUT_NON_DEFINI__"
  )
}

# ── global_data de test : le champ ASR-4 doit EXISTER et valoir NULL avant
#    toute activation (état OFF = comportement actuel, contrat §4). ----------

.asr4_gd <- function() {
  shiny::reactiveValues(
    sc_obj = NULL, sc_obj_epoch = 0L,
    bulk_obj = NULL, bulk_obj_epoch = 0L,
    spatial_obj = NULL, spatial_datasets = list(),
    language = "fr", i18n = NULL,
    session_checkpoint_settings = NULL
  )
}

# Conversion exacte Gio -> octets (x 1024^3, JAMAIS 10^9 — décision 4).
.asr4_gib <- function(gib) gib * 1024^3

# ── 1. La surface existe : la zone de réglages est déclarée dans le panneau --
#    (RED : le module ASR-3 livré ne déclare pas settings_area.)

test_that("ASR-4 surface: the settings area is declared in the module panel", {
  panel <- mod_session_checkpoint_ui("session_ckpt")
  html  <- paste(collapse = "", as.character(panel))
  expect_true(grepl('id="session_ckpt-settings_area"', html, fixed = TRUE),
              info = paste("la zone de réglages (uiOutput settings_area) doit être",
                           "déclarée dans le panneau « Checkpoints de session » —",
                           "jalon ASR-4 non livré"))
})

# ── T1. Défauts publiés : toggle ON -> les défauts MOTEUR sont publiés -------
#    (entier 3L, plafond numérique exact 4 GiB en octets) et rendus. ---------

test_that("ASR-4 T1 defaults: toggling ON publishes the engine defaults (retention_n = 3L, disk_cap_bytes = 4 GiB) and renders both settings inputs", {
  store <- .asr4_store()
  gd <- .asr4_gd()
  .asr4_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  # Constantes moteur gelées (session_checkpoint.R:68-69) — l'ancre des défauts.
  expect_identical(TS_SESSION_CHECKPOINT_RETENTION_N, 3L,
                   info = "TS_SESSION_CHECKPOINT_RETENTION_N a dérivé de 3L")
  expect_identical(TS_SESSION_CHECKPOINT_DISK_CAP_BYTES, 4294967296,
                   info = "TS_SESSION_CHECKPOINT_DISK_CAP_BYTES a dérivé de 4 GiB (4294967296 octets)")
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    # Publication EXACTE : la liste complète, deux champs, pas un de plus
    # (ask ASR-4 T1 : list(retention_n = 3L, disk_cap_bytes = 4294967296)).
    expect_identical(gd$session_checkpoint_settings,
                     list(retention_n = 3L, disk_cap_bytes = 4294967296),
                     info = paste("à l'activation, les défauts publiés doivent être",
                                  "list(retention_n = 3L, disk_cap_bytes = 4294967296) —",
                                  "entier 3L, plafond numérique exact 4 GiB en octets"))
    # Les défauts publiés sont CEUX DU MOTEUR (pas une copie driftée) :
    expect_identical(gd$session_checkpoint_settings$retention_n,
                     TS_SESSION_CHECKPOINT_RETENTION_N,
                     info = "retention_n publié != constante moteur TS_SESSION_CHECKPOINT_RETENTION_N")
    expect_identical(gd$session_checkpoint_settings$disk_cap_bytes,
                     TS_SESSION_CHECKPOINT_DISK_CAP_BYTES,
                     info = "disk_cap_bytes publié != constante moteur TS_SESSION_CHECKPOINT_DISK_CAP_BYTES")
    # Le rendu quand ON : la zone de réglages existe, avec les DEUX inputs et
    # leurs valeurs par défaut (3 checkpoints, 4 Gio).
    html <- .asr4_output(session, "settings_area")
    expect_true(grepl('id="session_ckpt-retention_n"', html, fixed = TRUE),
                info = paste("l'input retention_n n'est pas rendu quand le toggle",
                             "est ON (rendu :", html, ")"))
    expect_true(grepl('id="session_ckpt-disk_cap_gib"', html, fixed = TRUE),
                info = paste("l'input disk_cap_gib n'est pas rendu quand le toggle",
                             "est ON (rendu :", html, ")"))
    expect_true(grepl('value="3"', html, fixed = TRUE),
                info = "l'input retention_n ne rend pas sa valeur par défaut 3")
    expect_true(grepl('value="4"', html, fixed = TRUE),
                info = "l'input disk_cap_gib ne rend pas sa valeur par défaut 4 (Gio)")
  })
})

# ── T2. Discipline OFF : toggle OFF ou jamais touché -> AUCUN appel moteur,
#    settings NULL, AUCUN rendu de réglages (contrat §4 byte pour byte, §7.5) --

test_that("ASR-4 T2 OFF discipline: never touched means zero engine calls, no settings, no settings UI; switching OFF resets everything", {
  store <- .asr4_store()
  gd <- .asr4_gd()
  .asr4_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  # — a) toggle JAMAIS touché (session neuve : le champ doit rester NULL) —
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$flushReact()
    expect_identical(.asr4_counter$calls, character(0),
                     info = paste("des appels moteur ont eu lieu toggle OFF/jamais",
                                  "touché — contrat §7.5 (aucune écriture) et §4",
                                  "(byte pour byte) :",
                                  paste(.asr4_counter$calls, collapse = ", ")))
    expect_null(gd$session_checkpoint_settings,
                info = "des réglages sont publiés toggle OFF — interdit (contrat §4)")
    expect_identical(.asr4_output(session, "settings_area"), "",
                     info = paste("la zone de réglages est rendue toggle OFF — interdit",
                                  "(uiOutput NULL = empreinte nulle, motif ASR-3 ;",
                                  "rendu :", .asr4_output(session, "settings_area"), ")"))
  })
  # Aucune écriture disque : le magasin bac à sable n'a jamais été créé.
  expect_false(dir.exists(store),
               info = paste("le magasin de checkpoints a été créé par la couche",
                            "réglages — interdit (aucune écriture, contrat §4/§7.5)"))
  # Le module ne définit AUCUNE primitive d'écriture disque (décision 3 :
  # réglages session-scopés sans fichier de persistance).
  mod_src <- paste(readLines(file.path(ts_project_root(), "modules", "session",
                                       "mod_session_checkpoint.R"), warn = FALSE),
                   collapse = "\n")
  # value = TRUE : en cas d'échec, testthat affichera les lignes fautives
  # telles quelles. Un fichier de persistance est interdit (décision ASR-4 n°3).
  expect_length(grep("saveRDS|file\\.create|writeBin|writeLines|jsonlite::write|dir\\.create",
                     mod_src, perl = TRUE, value = TRUE), 0L)
  # — b) ON (réglage changé) -> OFF : tout revient à l'état OFF exact —
  gd2 <- .asr4_gd()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd2, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    n_scan <- length(.asr4_counter$calls)  # le scan événementiel ASR-3 a tourné
    session$setInputs(retention_n = 5); session$flushReact()
    expect_identical(length(.asr4_counter$calls), n_scan,
                     info = paste("changer un réglage a rappelé le moteur — interdit",
                                  "(décision ASR-4 n°6) :",
                                  paste(.asr4_counter$calls, collapse = ", ")))
    session$setInputs(enabled = FALSE); session$flushReact()
    expect_null(gd2$session_checkpoint_settings,
                info = paste("les réglages publiés survivent à l'extinction du toggle",
                             "— l'état OFF doit redevenir byte-pour-byte (contrat §4)"))
    expect_identical(.asr4_output(session, "settings_area"), "",
                     info = "la zone de réglages survit à l'extinction du toggle — interdit")
    expect_identical(length(.asr4_counter$calls), n_scan,
                     info = "l'extinction ne doit PAS rappeler le moteur (motif ASR-3)")
  })
})

# ── T3. Changement : N -> 5 publie retention_n = 5L ; plafond -> 2 (Gio)
#    publie disk_cap_bytes = 2147483648 (conversion EXACTE x1024^3) ; zéro
#    appel moteur, magasin jamais créé (décision 6). --------------------------

test_that("ASR-4 T3 changes: N -> 5 publishes retention_n = 5L; cap -> 2 (GiB) publishes disk_cap_bytes = 2147483648 (exact x1024^3); the engine is never called and the store is never created", {
  store <- .asr4_store()
  gd <- .asr4_gd()
  .asr4_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    n_scan <- length(.asr4_counter$calls)
    session$setInputs(retention_n = 5); session$flushReact()
    expect_identical(gd$session_checkpoint_settings,
                     list(retention_n = 5L, disk_cap_bytes = 4294967296),
                     info = paste("N -> 5 doit publier retention_n = 5L (ENTIER,",
                                  "pas 5 numérique), plafond inchangé"))
    session$setInputs(disk_cap_gib = 2); session$flushReact()
    expect_identical(gd$session_checkpoint_settings,
                     list(retention_n = 5L, disk_cap_bytes = 2147483648),
                     info = paste("plafond -> 2 (Gio) doit publier disk_cap_bytes =",
                                  "2147483648 octets (2 x 1024^3, conversion EXACTE)"))
    expect_identical(gd$session_checkpoint_settings$disk_cap_bytes, .asr4_gib(2),
                     info = paste("la conversion Gio -> octets n'est pas exacte",
                                  "(x1024^3 attendu, pas une approximation 10^9)"))
    # Type honnête : retention_n reste un ENTIER (déjà exigé par l'assertion
    # identical ci-dessus — 5L, pas 5 ; le diff testthat l'affiche sinon).
    # Zéro appel moteur pendant les changements (décision 6) : aucune purge
    # immédiate — le vocabulaire de déclenchement est gelé (contrat §3.1).
    expect_identical(length(.asr4_counter$calls), n_scan,
                     info = paste("changer les réglages a appelé le moteur :",
                                  paste(.asr4_counter$calls, collapse = ", ")))
    expect_false(any(c("ts_session_checkpoint_retention",
                       "ts_session_checkpoint_write",
                       "ts_session_checkpoint_write_async") %in% .asr4_counter$calls),
                 info = paste("un hook d'écriture/rétention a été appelé par la",
                              "couche réglages — interdit (décision ASR-4 n°6)"))
  })
  # Le magasin bac à sable n'a JAMAIS été créé (aucune IO disque des réglages).
  expect_false(dir.exists(store),
               info = "le magasin a été créé pendant la session de réglages — interdit")
})

# ── T4. Bornes : N < 1 ou plafond <= 0 -> DÉFAUT RÉTABLI (décision 5, en-
#    tête) — la couche réglages publie honnêtement ce que le moteur appliquera.

test_that("ASR-4 T4 bounds: N < 1 or cap <= 0 restores the engine default (documented decision — honest publish of what the engine will apply)", {
  store <- .asr4_store()
  gd <- .asr4_gd()
  .asr4_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    # N < 1 (0 puis négatif) : défaut rétabli — le moteur clampe n < 1 vers
    # TS_SESSION_CHECKPOINT_RETENTION_N (session_checkpoint.R:832-835).
    session$setInputs(retention_n = 0); session$flushReact()
    expect_identical(gd$session_checkpoint_settings$retention_n, 3L,
                     info = "N = 0 doit rétablir le défaut 3L (défaut rétabli, décision T4)")
    session$setInputs(retention_n = -2); session$flushReact()
    expect_identical(gd$session_checkpoint_settings$retention_n, 3L,
                     info = "N = -2 doit rétablir le défaut 3L")
    # Plafond <= 0 : défaut rétabli (4 GiB) — un plafond de 0 octet est un
    # état absurde côté opérateur (il affamerait tout l'historique) ;
    # divergence assumée avec la tolérance de banc du hook moteur (cap petit
    # = starvation, test-session-checkpoint-engine.R:455-462), décision T5.
    session$setInputs(disk_cap_gib = 0); session$flushReact()
    expect_identical(gd$session_checkpoint_settings$disk_cap_bytes, 4294967296,
                     info = "plafond = 0 doit rétablir le défaut 4 GiB (défaut rétabli, décision T4)")
    session$setInputs(disk_cap_gib = -1); session$flushReact()
    expect_identical(gd$session_checkpoint_settings,
                     list(retention_n = 3L, disk_cap_bytes = 4294967296),
                     info = paste("bornes franchies : l'état publié final doit revenir",
                                  "EXACTEMENT aux défauts moteur"))
  })
})

# ── T5a. i18n : chaque chaîne visible des réglages a sa paire fr/en dans
#    i18n/translation.json (append scopé — leçon B1/B5/DSTV-3). --------------

test_that("ASR-4 T5 i18n: every visible settings string has its fr/en key pair in i18n/translation.json (scoped append)", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))
  for (k in .asr4_keys) {
    idx <- match(k[1], fr)
    expect_false(is.na(idx),
                 info = paste("clé FR absente de translation.json :", k[1]))
    if (!is.na(idx)) {
      expect_identical(en[[idx]], k[2],
                       info = paste("paire EN absente ou divergente pour :", k[1]))
    }
  }
})

# ── T5b. 0 CSS www/ (règle post-O13) : primitives bslib/shiny seulement ------
#    (motif test-asr3-ui.R:413-424, garde lexicale sur le module ASR-4 inclus).

test_that("ASR-4 T5 style: no CSS in www/ and no style injection (post-O13 rule)", {
  mod_path <- file.path(ts_project_root(), "modules", "session",
                        "mod_session_checkpoint.R")
  mod_src <- paste(readLines(mod_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_src),
               info = paste("le module injecte du style (réglages ASR-4 inclus) —",
                            "interdit (0 CSS, primitives seulement)"))
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE,
                      ignore.case = TRUE, full.names = FALSE)
    # En cas d'échec, testthat affichera les noms de fichiers fautifs. Un CSS
    # ASR-4/réglages dans www/ est interdit (règle post-O13).
    expect_length(
      css[grep("asr4|retention|settings|session_ckpt|checkpoint", css,
               ignore.case = TRUE)],
      0L)
  }
})
