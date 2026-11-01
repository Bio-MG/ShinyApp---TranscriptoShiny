# =============================================================================
# test-asr-wiring.R -- passe WRITE-SIDE : câblage des points de pose (O29) — RED
# =============================================================================
# POURQUOI CE FICHIER
#
# Passe WRITE-SIDE du contrat SESSION_CHECKPOINT_RESTORE_CONTRACT.md :
#   * §2 (3 couches ensemble, atomiquement) ; §3.1 (déclenchement ÉVÉNEMENTIEL,
#     JAMAIS par timer — « la liste précise des points de pose par module
#     import/pipeline : (ouvert — à trancher à l'implémentation) » : tranché
#     ICI par l'alignement sur les publieurs DSTV-1 existants, liste A9
#     ci-dessous) ; §4 (OFF = byte pour byte) ; §6 (honnêteté) ; §7.1 (aucune
#     écriture de couche 1 sur le thread principal — le seam mirai
#     ts_session_checkpoint_write_async() REFUSE honnêtement sans pool, jamais
#     de fallback) ; §7.2 (aucun checkpoint par timer) ; §8 (RED-first).
# Le MOTEUR ASR-1/2 (tools/pkg_skeleton/R/session_checkpoint.R, commits
# 6d35ef2/65ce906) et la UI ASR-3/ASR-4 (modules/session/
# mod_session_checkpoint.R, commits 2693423/0101114) sont LIVRÉS. Ce test
# épingle la COUCHE DE CÂBLAGE : la publication du helper
# global_data$session_checkpoint_publish() par le module session à
# l'activation du toggle, et l'appel GARDÉ au publisher aux 7 points de pose.
#
# ARCHITECTURE GELÉE (décisions de coordination de la passe — chaque test
# ci-dessous en est l'application) :
#   A1. Le module session enregistre, À L'ACTIVATION DU TOGGLE,
#       global_data$session_checkpoint_publish <- function(trigger,
#         dataset_ids = character(0), layer2_extras = list()) — et remet ce
#       champ à NULL quand le toggle repasse OFF (OFF = byte pour byte, §4).
#   A2. Garde OFF : settings absentes/NULL -> invisible(FALSE), AUCUN appel
#       moteur, aucune écriture (§7.5).
#   A3. Couche 1 : objets présents SEULEMENT, noms VERBATIM du vocabulaire
#       TS_SESSION_CHECKPOINT_LAYER1_TARGETS (sc_obj, bulk_obj, spatial_obj,
#       spatial_datasets) ; tout absent -> saut honnête (invisible(FALSE) +
#       raison à la ligne d'état), jamais de liste vide fabriquée.
#   A4. Couche 2 : ts_session_checkpoint_layer2(
#         pipeline_steps_completed = character(0),   # HONNÊTE : aucun flag
#                                 # par étape n'existe aujourd'hui — nommé
#                                 # vide DOCUMENTÉ, pas un flag fabriqué
#         manual_pipeline = global_data$manual_pipeline,  # relais VERBATIM
#         business_state = global_data$business_state,    # MÊME source que le
#                    # producteur DSTV-1 (les 5 scalaires gelés — JAMAIS
#                    # re-dérivés des objets lourds)
#         deconv_props/derived_tables/banners = layer2_extras ou défauts
#         honnêtes)
#   A5. Couche 3 : actifs défensifs — par échantillon spatial
#       spatial_pkg$bpcells_dir (kind "dir"), $raw_bpcells_dir, $raw_dir
#       (enregistré comme fallback_source — la source d'import d'origine,
#       contrat §5.3) et global_data$spatial_reference$path (manifeste de
#       référence partagé) ; existed_at_checkpoint stampé MOTEUR
#       (ts_session_checkpoint_layer3 / ts_session_checkpoint_write).
#   A6. Réglages : retention_n/disk_cap_bytes lus sur
#       global_data$session_checkpoint_settings (publication ASR-4, 0101114 ;
#       présence <=> toggle ON).
#   A7. Dispatch : ts_session_checkpoint_write_async(..., trigger = ...) —
#       collection par promises::then (JAMAIS de collection bloquante [] sur
#       le thread principal) ; résultat (ok / refus) -> ligne d'état réactive.
#   A8. Boot pool : à l'activation du toggle, init_spatial_daemons()
#       (IDEMPOTENT — garde .spatial_async_env$daemons_ready,
#       spatial_async.R:236 ; UN SEUL pool, règle §4 ROADMAP) ; échec =
#       honnête, PAS fatal (l'activation continue, les écritures refuseront
#       honnêtement §7.1).
#   A9. SITES (les 7 points de pose, garde uniforme, 1-3 lignes, AUCUNE
#       refactorisation — ancres lexicales mesurées uniques au 2026-11-01) :
#         if (is.function(global_data$session_checkpoint_publish))
#           global_data$session_checkpoint_publish("<trigger>", <ids>)
#       * modules/import/mod_import_sc.R              (:406) "import_complete"
#         — succès import SC (global_data$sc_obj <- prepared) ;
#       * modules/import/mod_import_bulk.R            (:414) "import_complete"
#         — succès import bulk (global_data$bulk_datasets <- res) ;
#       * modules/import/mod_import_spatial.R         (:711) "import_complete"
#         — succès par échantillon (spatial_datasets[[sample_name]] <-
#         spatial_pkg ; $bpcells_dir/$raw_bpcells_dir/$raw_dir existent) ;
#       * modules/sc/mod_sc.R                         (:1767)
#         "pipeline_step_complete" — site « done » DSTV-1b de l'autopipeline
#         SC (manual_pipeline/business_state feed, mod_sc.R:1767-1772) ;
#       * modules/bulk/mod_bulk.R                     (:738)
#         "pipeline_step_complete" — site « done » DSTV-1b de l'autopipeline
#         bulk (mod_bulk.R:738-742) ;
#       * modules/spatial/mod_spatial_pipeline.R      (:417,
#         .finish_pipeline) "pipeline_step_complete" — succès pipeline
#         spatial (mod_spatial_pipeline.R:417-429) ;
#       * modules/spatial/deconv/mod_spatial_deconv.R (:199)
#         "deconv_complete" + layer2_extras =
#         list(deconv_props = shared_rv$deconv_props) — déconvolution
#         complète (mod_spatial_deconv.R:199).
#
# ARBITRAGE T7 (coordonnateur, 2026-11-01 — remonté par cette passe RED,
# réponse enregistrée ici) : le write_async LIVRÉ (session_checkpoint.R:605-631,
# commit 6d35ef2) ne porte PAS retention_n/disk_cap_bytes — l'appel daemon
# interne (session_checkpoint.R:623-624) appelle ts_session_checkpoint_write()
# SANS ces arguments, donc la rétention daemon tourne TOUJOURS avec les
# défauts moteur (3L / 4 GiB, session_checkpoint.R:443-444, :577-583) : les
# réglages ASR-4 publiés (0101114) ne pouvaient pas atteindre l'écriture par
# la couture livrée. L'option « rétention post-collection via
# ts_session_checkpoint_retention() » (session_checkpoint.R:888-891) a été
# REJETÉE : la rétention daemon PAR DÉFAUT tournerait destructivement AVANT
# toute ré-application (un réglage plus généreux, ex. N=10, serait trahi —
# les checkpoints 4..10 déjà purgés à 3). TRANCHÉ (option a) :
#   * T7 gèle que les réglages passent EN ARGUMENTS à
#     ts_session_checkpoint_write_async ; le jalon GREEN étend la signature du
#     write_async de DEUX paramètres — retention_n =
#     TS_SESSION_CHECKPOINT_RETENTION_N, disk_cap_bytes =
#     TS_SESSION_CHECKPOINT_DISK_CAP_BYTES (défauts = CONSTANTES MOTEUR :
#     comportement existant préservé byte pour byte pour tout appelant qui ne
#     les passe pas) — et les forward dans l'appel mirai vers
#     ts_session_checkpoint_write() (session_checkpoint.R:623-624).
#   * CONTREPARTIES : l'interdit moteur est levé pour CET AJOUT MINIMAL ET
#     UNIQUEMENT LUI ; la suite moteur complète (filter session-checkpoint)
#     reste dans la porte de régression — si un seul test moteur existant
#     bouge, l'extension n'est pas backward-compatible et le jalon est refusé ;
#     aucune autre ligne du moteur ne bouge ; aucune édition du contrat
#     (contrat §12 : une signature étendue avec défauts n'est aucune des
#     surfaces de mise à jour exigées — structure 3 couches / sémantique de
#     restauration / comportements refusés / table d'honnêteté).
# T7 gèle QUATRE assertions : (1) formels + défauts = constantes moteur ;
# (2) refus §7.1 inchangé avec pool_probe = function() FALSE explicite ;
# (3) forward lexical dans le corps du write_async (deux points : arguments
# nommés du mirai — seules variables que le daemon reçoit — et appel
# ts_session_checkpoint_write() du .expr) ; (4) les settings publiées
# atteignent l'APPEL write_async (wrapper délégant, valeurs capturées).
#
# QU'EST ASSERTABLE HEADLESS (honnêteté du garde, motif test-asr3-ui.R:15-29,
# contrat §9) :
#   * Le chemin async COMPLET (pool mirai réel + collection promises d'un ok)
#     n'est PAS headless-assertable : aucun pool n'est démarré en test. Ce qui
#     est testé : la couture write_async (arguments capturés par un WRAPPER
#     DÉLÉGANT autour du moteur RÉEL — on compte les invocations, on ne
#     remplace jamais le moteur, motif test-asr3-ui.R:175-199) et le chemin de
#     REFUS §7.1 (probe FALSE — forcé déterministe, même scénario que
#     test-session-checkpoint-engine.R:504-515). L'ok-path (écriture daemon
#     réelle + collecte promises + ligne d'état succès) reste non couvert
#     headless — déjà dans les non-couverts de la lane.
#   * init_spatial_daemons() est remplacée par une SENTINELLE comptable (PAS
#     un wrapper délégant) : le boot réel précharge Seurat+BPCells sur des
#     daemons mirai persistants — ni headless-testable ni acceptable dans un
#     test unitaire. Ce qui est asserté : appel à l'activation (exactement une
#     fois), zéro appel OFF, échec non fatal. L'idempotence RÉELLE du pool est
#     le sujet de spatial_async (garde .spatial_async_env$daemons_ready,
#     spatial_async.R:236).
#   * Les sites de milestone (mod_import_*, mod_sc, mod_bulk, mod_spatial_*)
#     ne sont PAS testServer-ables unitairement (chaînes de dépendances
#     app-sized) : T8 les épingle LEXICALEMENT (motif test-asr3-ui.R §8/§9) :
#     garde + trigger gelé à ±N lignes d'une ancre unique mesurée, zéro
#     timer/polling dans la fenêtre du point de pose.
#   * La chaîne « clé i18n rendue dans le navigateur » n'est pas assertable ;
#     testServer couvre la logique serveur et le HTML des outputs.
#   * PAS de return() dans une expression testServer : elle est évaluée par
#     rlang::eval_tidy (« no function to return from, jumping to top level »,
#     mesuré à l'exécution RED) — les sections dépendantes du publisher vivent
#     dans des gardes if (ok_reg), et un échec d'assertion reste un échec
#     d'assertion lisible, jamais une erreur qui avorte le bloc (motif
#     test-asr4-settings.R:30-32).
#
# ÉVIDENCE RED->GREEN (place réservée — le jalon GREEN complétera la ligne
# GREEN, ne rien réécrire d'autre) :
#   RED  (2026-11-01, « Rscript tools/run_tests.R asr-wiring » depuis la racine
#      du dépôt, exit = 1) : TOTAL (filter=asr-wiring, files=1) failed=25
#      passed=43 error=0 skipped=0, fichier test-asr-wiring.R uniquement.
#      Échecs par famille (inventaire mesuré, srcref comprises) —
#      T1 :4 (boot pool non appelé à l'activation : 0 != 1, en échec comme en
#      nominal ; publisher non enregistrée, boot-en-échec comme nominal) ;
#      T2 :0 (discipline OFF déjà verte — garde de régression) ; T3 :1 /
#      T4 :1 / T5 :1 / T6 :1 / T7b :1 (publisher absente à l'activation —
#      A1 non livré, le reste de chaque bloc est gardé if (ok_reg)) ; T7a :6
#      (formels retention_n/disk_cap_bytes absents du write_async livré :
#      :726/:730 ; forward mirai absent aux deux points :772/:776/:785/:788) ;
#      T8 :7 (appel gardé absent aux 7 sites — ancres toutes intactes, zéro
#      timer dans les fenêtres) ; T9 :3 (les 3 clés FR absentes de
#      translation.json). Gardes de régression déjà vertes : discipline OFF
#      (T2 : 0 appel moteur, 0 boot, magasin jamais créé, garde de site
#      no-op), vocabulaire couche-1 verbatim, refus §7.1 probe FALSE inchangé
#      (T7a-2), dérive de vocabulaire vacueuse, zéro timer module, 0 CSS.
#      Deux défauts de harnais mesurés puis corrigés PENDANT la mise au point
#      RED (ne pas réintroduire) : return() dans une expression testServer
#      lève « no function to return from » (rlang::eval_tidy) — gardes
#      if (ok_reg) ; expect_length() n'accepte pas info= dans cette version
#      de testthat — expect_identical(length(...), n, info=).
#   GREEN (2026-11-01, implémentation posée dans le périmètre du jalon :
#      tools/pkg_skeleton/R/session_checkpoint.R — extension T7 MINIMALE
#      arbitrage (a), en-tête ci-dessus ; modules/session/
#      mod_session_checkpoint.R — publisher + boot pool + ligne d'état
#      enrichie ; 7 sites — appel gardé ; i18n/translation.json — append
#      scopé via add_i18n_keys.R lot ASR-WIRING seul, tool rétabli) :
#      « Rscript tools/run_tests.R asr-wiring » -> exit = 0, TOTAL failed=0
#      passed=115 error=0. Deux AJUSTEMENTS MINEURS du harnais, justifiés et
#      mesurés à la mise au point GREEN : (1) la sentinelle init_spatial_
#      daemons doit être assignée dans GLOBALENV — testthat source ce fichier
#      dans un environnement propre et le module résout le symbole via son
#      Closure -> globalenv (sans assign() : « object not found » tryCatché
#      honnêtement, 0 appel, T1 rouge ; même mécanique que les wrappers
#      test-asr3-ui.R:188-193) ; (2) session$flushReact() avant la lecture de
#      la ligne d'état dans T3c et T6 — last_write() est mis à jour
#      synchronement par le publisher mais le renderUI ne se ré-exécute qu'au
#      prochain flush (motif test-asr3-ui.R:273-274). Les ajustements ne
#      desserrent AUCUNE assertion du contrat.
# =============================================================================

# Le moteur ASR-1/2 ET le module ASR-3/ASR-4 sont sourcés DIRECTEMENT
# (précédents test-asr3-ui.R:65-66, test-session-checkpoint-restore.R) : la
# couche de câblage testée consomme le moteur RÉEL, jamais un substitut.
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

# ── Clés i18n WRITE-SIDE (fr = clé, en = paire) — les NOUVELLES chaînes
#    visibles de la ligne d'état enrichie. La MÊME liste que le test i18n
#    (T9) ci-dessous : toute chaîne visible du câblage vit ici et dans
#    i18n/translation.json (append scopé). La raison de refus elle-même est le
#    texte du MOTEUR (structuré, §7.1) relayé verbatim — elle n'est pas une
#    clé ; la clé 2 en est le cadre français rendu par la ligne d'état. La
#    clé 3 (succès) n'est pas rendue headless (ok-path = pool réel, non
#    couvert) : elle n'est gelée que par sa présence dans translation.json. --

.asrw_keys <- list(
  c("Aucun objet lourd \u00e0 enregistrer \u2014 checkpoint non pos\u00e9.",
    "No heavy object to record \u2014 no checkpoint written."),
  c("\u00c9chec de l'\u00e9criture du checkpoint : {err}",
    "Checkpoint write failed: {err}"),
  c("Checkpoint pos\u00e9 \u00e0 {ts}.",
    "Checkpoint written at {ts}.")
)

# ── Fixtures ──────────────────────────────────────────────────────────────────

# Magasin bac à sable JAMAIS créé par les tests : seul un write RÉEL le
# créerait — le chemin de refus §7.1 et la discipline OFF ne doivent JAMAIS
# le toucher (aucune écriture hors publication, contrat §4/§7.5).
.asrw_store <- function() tempfile(pattern = "ts_asrw_store_")

.asrw_gd <- function() {
  shiny::reactiveValues(
    sc_obj = NULL, sc_obj_epoch = 0L,
    bulk_obj = NULL, bulk_obj_epoch = 0L,
    spatial_obj = NULL, spatial_datasets = list(),
    manual_pipeline = NULL, business_state = NULL,
    spatial_reference = NULL,
    session_checkpoint_settings = NULL,   # publication ASR-4 (présente <=> ON)
    language = "fr", i18n = NULL
  )
}

# Paquet spatial minimal (contrat paquet léger mod_import_spatial.R:132 :
# sketch + bpcells_dir + coords ; raw_bpcells_dir/raw_dir quand importés).
.asrw_spatial_pkg <- function() {
  bpc  <- tempfile(pattern = "ts_asrw_bpc_");  dir.create(bpc, recursive = TRUE)
  rawb <- tempfile(pattern = "ts_asrw_rawb_"); dir.create(rawb, recursive = TRUE)
  raw  <- tempfile(pattern = "ts_asrw_raw_", fileext = ".h5"); writeLines("h5", raw)
  list(bpcells_dir = bpc, raw_bpcells_dir = rawb, raw_dir = raw,
       sketch = matrix(1:4, 2),
       coords = data.frame(x = c(1, 2), y = c(3, 4)), n_total = 2L)
}

# Couches minimales VALIDES (le moteur exige les trois ensemble, §2).
.asrw_tiny_layers <- function() {
  list(
    layer1 = list(small_obj = data.frame(x = 1:3)),
    layer2 = ts_session_checkpoint_layer2(),
    layer3 = ts_session_checkpoint_layer3(list())
  )
}

# Accès sûr à un output testServer : avant le jalon GREEN la ligne d'état
# enrichie peut lever ("output that hasn't been defined yet") — le garde
# transforme ce signal en marqueur lisible (motif test-asr4-settings.R:199-204).
.asrw_output <- function(session, id) {
  tryCatch(
    paste(collapse = "", as.character(session$output[[id]])),
    error = function(e) "__ASRW_OUTPUT_NON_DEFINI__"
  )
}

# ── Sentinelle boot pool (A8) : comptable, PAS délégante — voir l'en-tête
#    (honnêteté headless). Résidue : compteur consulté par T1/T2.
#    ⚠️ testthat source ce fichier dans un environnement PROPRE (pas
#    globalenv) : le module résout init_spatial_daemons dans son Closure ->
#    globalenv — la sentinelle doit donc y être assignée EXPLICITEMENT
#    (même mécanique que les wrappers ci-dessus, test-asr3-ui.R:188-193).
#    Mesure ÉTAPE 2 : sans assign(), l'appel module échoue en « object not
#    found », tryCatché honnêtement (0 appel, non fatal) — T1 restait rouge. --

.asrw_pool <- new.env(parent = emptyenv())
.asrw_pool$calls <- 0L
.asrw_pool$fail <- FALSE

assign("init_spatial_daemons", function(...) {
  .asrw_pool$calls <- .asrw_pool$calls + 1L
  if (isTRUE(.asrw_pool$fail)) return(invisible(FALSE))
  invisible(TRUE)
}, envir = globalenv())

# ── Compteur d'appels moteur : WRAPPERS DÉLÉGANTS autour du moteur RÉEL
#    (motif test-asr3-ui.R:175-199) — on compte les invocations et on capture
#    les arguments qui TRAVERSENT la couture, sans jamais remplacer le moteur.
#    Le probe daemons est délégant (réel) sauf mode forcé FALSE (défaut) :
#    scénario §7.1 déterministe, aucun pool n'existe ni ne doit exister en
#    test (la sentinelle init ne boot rien). ---------------------------------

.asrw_counter <- new.env(parent = emptyenv())
.asrw_counter$calls <- character(0)
.asrw_counter$write_async_args <- list()
.asrw_counter$layer2_args <- list()
.asrw_counter$layer3_args <- list()
.asrw_counter$force_probe_false <- TRUE

.asrw_wrap_engines <- function() {
  .asrw_counter$calls <- character(0)
  .asrw_counter$write_async_args <- list()
  .asrw_counter$layer2_args <- list()
  .asrw_counter$layer3_args <- list()
  .asrw_counter$force_probe_false <- TRUE
  saved <- new.env(parent = emptyenv())
  for (nm in c("ts_session_checkpoint_write_async", "ts_session_checkpoint_write",
               "ts_session_checkpoint_layer2", "ts_session_checkpoint_layer3",
               "ts_session_checkpoint_retention")) {
    real <- get(nm, envir = globalenv())
    saved[[nm]] <- real
    assign(nm, local({
      nm_ <- nm; real_ <- real
      function(...) {
        .asrw_counter$calls <- c(.asrw_counter$calls, nm_)
        if (identical(nm_, "ts_session_checkpoint_write_async")) {
          .asrw_counter$write_async_args[[length(.asrw_counter$write_async_args) + 1L]] <- list(...)
        } else if (identical(nm_, "ts_session_checkpoint_layer2")) {
          .asrw_counter$layer2_args[[length(.asrw_counter$layer2_args) + 1L]] <- list(...)
        } else if (identical(nm_, "ts_session_checkpoint_layer3")) {
          .asrw_counter$layer3_args[[length(.asrw_counter$layer3_args) + 1L]] <- list(...)
        }
        real_(...)
      }
    }), envir = globalenv())
  }
  # Le probe PAR DÉFAUT du write_async (session_checkpoint.R:608) est résolu
  # dans globalenv au moment de l'appel : le wrapper ci-dessous est donc
  # consulté par tout write_async appelé sans pool_probe explicite.
  real_probe <- get("ts_session_checkpoint_daemons_active", envir = globalenv())
  saved[["ts_session_checkpoint_daemons_active"]] <- real_probe
  assign("ts_session_checkpoint_daemons_active", function(...) {
    .asrw_counter$calls <- c(.asrw_counter$calls, "ts_session_checkpoint_daemons_active")
    if (isTRUE(.asrw_counter$force_probe_false)) return(FALSE)
    real_probe(...)
  }, envir = globalenv())
  withr::defer({
    for (nm in ls(saved)) assign(nm, saved[[nm]], envir = globalenv())
  }, envir = parent.frame())
  invisible(saved)
}

# Le publisher enregistré : garde de lisibilité RED/GREEN (motif
# test-asr4-settings.R : un signal absent devient un échec d'assertion lisible,
# pas une erreur qui avorte le bloc).
.asrw_pub_ok <- function(gd) is.function(gd$session_checkpoint_publish)

# ── T1. Enregistrement du publisher (A1) + boot pool (A8) ---------------------
#    ON -> is.function(global_data$session_checkpoint_publish) et UN SEUL
#    appel init_spatial_daemons ; OFF / jamais touché -> NULL ; OFF après ON
#    -> NULL (état OFF rétabli byte pour byte, contrat §4). L'échec du boot
#    pool est HONNÊTE, pas fatal : le publisher est quand même enregistré.

test_that("ASR-WIRING T1 registration: toggle ON registers global_data$session_checkpoint_publish (one idempotent pool boot, failure non-fatal); OFF/untouched leaves it NULL", {
  # — a) activation AVEC échec du boot pool : honnête, pas fatal (A8) —
  store_a <- .asrw_store()
  gd_a <- .asrw_gd()
  .asrw_pool$calls <- 0L
  .asrw_pool$fail <- TRUE
  on.exit({ .asrw_pool$calls <- 0L; .asrw_pool$fail <- FALSE }, add = TRUE)
  .asrw_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd_a, store_dir = store_a), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    expect_identical(.asrw_pool$calls, 1L,
                     info = paste("init_spatial_daemons() n'a pas été appelée EXACTEMENT",
                                  "une fois à l'activation du toggle (boot pool A8 —",
                                  "UN SEUL pool, règle §4 ROADMAP) ; appels :",
                                  .asrw_pool$calls))
    expect_true(.asrw_pub_ok(gd_a),
                info = paste("boot pool en échec -> l'enregistrement du publisher a",
                             "avorté — l'échec du pool doit être HONNÊTE, pas fatal",
                             "(A8 : l'activation continue, les écritures refuseront",
                             "honnêtement §7.1)"))
  })
  expect_false(dir.exists(store_a),
               info = "l'activation (boot en échec) a créé le magasin — interdit")

  # — b) chemin nominal : ON -> function ; jamais touché / OFF -> NULL —
  store_b <- .asrw_store()
  gd_b <- .asrw_gd()
  .asrw_pool$calls <- 0L
  .asrw_pool$fail <- FALSE   # chemin nominal : le boot réussit (sentinelle)
  .asrw_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd_b, store_dir = store_b), expr = {
    session$flushReact()
    expect_null(gd_b$session_checkpoint_publish,
                info = paste("le publisher est enregistré toggle JAMAIS touché —",
                             "l'enregistrement ne peut avoir lieu qu'À L'ACTIVATION",
                             "(A1, contrat §4 : OFF = byte pour byte)"))
    session$setInputs(enabled = TRUE); session$flushReact()
    expect_identical(.asrw_pool$calls, 1L,
                     info = paste("init_spatial_daemons() doit être appelée UNE SEULE",
                                  "fois par activation (pool idempotent, A8) ; appels :",
                                  .asrw_pool$calls))
    ok_reg <- .asrw_pub_ok(gd_b)
    expect_true(ok_reg,
                info = paste("toggle ON : global_data$session_checkpoint_publish doit",
                             "être une FUNCTION (helper de publication enregistré par le",
                             "module session, A1) — câblage WRITE-SIDE absent"))
    # NB : PAS de return() dans une expression testServer (évaluée par
    # rlang::eval_tidy — mesuré à l'exécution RED) : garde if().
    if (ok_reg) {
      session$setInputs(enabled = FALSE); session$flushReact()
      expect_null(gd_b$session_checkpoint_publish,
                  info = paste("le publisher survit à l'extinction du toggle — l'état",
                               "OFF doit redevenir byte-pour-byte (contrat §4, A1)"))
    }
  })
  expect_false(dir.exists(store_b),
               info = "l'activation nominale a créé le magasin — interdit hors écriture")
})

# ── T2. Discipline OFF : AUCUN appel moteur, AUCUNE écriture quand OFF
#    (contrat §4 byte pour byte, §7.5) — garde de régression sur le module
#    ASR-3/ASR-4 livré, étendue au publisher et au boot pool. -------------------

test_that("ASR-WIRING T2 OFF discipline: untouched toggle means zero engine calls, zero pool boot, no write — even the sites' guard is a no-op", {
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_pool$calls <- 0L
  .asrw_wrap_engines()
  on.exit({ .asrw_pool$calls <- 0L; unlink(store, recursive = TRUE, force = TRUE) }, add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$flushReact()
    expect_identical(.asrw_counter$calls, character(0),
                     info = paste("des appels moteur ont eu lieu toggle OFF — contrat",
                                  "§7.5 (aucune écriture) et §4 (byte pour byte) :",
                                  paste(.asrw_counter$calls, collapse = ", ")))
    expect_identical(.asrw_pool$calls, 0L,
                     info = "le boot pool a eu lieu toggle OFF — interdit (A8 : à l'activation seulement)")
    expect_null(gd$session_checkpoint_publish,
                info = "le publisher est enregistré toggle OFF — interdit (A1)")
    # La garde UNIFORME des 7 sites (A9), exécutée telle quelle OFF : no-op.
    if (is.function(gd$session_checkpoint_publish)) {
      gd$session_checkpoint_publish("import_complete", character(0))
    }
    session$flushReact()
    expect_identical(.asrw_counter$calls, character(0),
                     info = paste("la garde de site a déclenché un appel moteur toggle",
                                  "OFF — la garde is.function() doit court-circuiter :",
                                  paste(.asrw_counter$calls, collapse = ", ")))
  })
  expect_false(dir.exists(store),
               info = paste("le magasin de checkpoints a été créé toggle OFF —",
                            "aucune écriture (contrat §4/§7.5)"))
})

# ── T3. Couche 1 sélective (A3) : objets PRÉSENTS SEULEMENT, noms VERBATIM
#    de TS_SESSION_CHECKPOINT_LAYER1_TARGETS ; tout absent -> saut honnête
#    (invisible(FALSE) + raison à la ligne d'état), jamais de liste vide. ------

test_that("ASR-WIRING T3 layer-1 selective: present objects only, frozen vocabulary verbatim; everything absent means an honest skip (invisible FALSE, reason on the status line), never a fabricated empty layer", {
  # Vocabulaire gelé du moteur (session_checkpoint.R:946-947) — l'ancre.
  expect_identical(TS_SESSION_CHECKPOINT_LAYER1_TARGETS,
                   c("sc_obj", "bulk_obj", "spatial_obj", "spatial_datasets"),
                   info = "TS_SESSION_CHECKPOINT_LAYER1_TARGETS a dérivé — ancre T3 invalide")
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- .asrw_pub_ok(gd)
    expect_true(ok_reg,
                info = paste("publisher absente à l'activation — couche 1 sélective",
                             "non testable (A1/A3 non livrés)"))
    if (ok_reg) {
      # — a) sc_obj SEUL présent : layer1 == c(sc_obj = ...) exactement —
      gd$sc_obj <- data.frame(cell = 1:3, cluster = c("a", "a", "b"))
      gd$session_checkpoint_publish("import_complete", dataset_ids = "ds_sc")
      expect_identical(length(.asrw_counter$write_async_args), 1L,
                       info = paste("l'import SC (sc_obj seul présent) n'a pas dispatché",
                                    "EXACTEMENT une écriture async ; appels :",
                                    length(.asrw_counter$write_async_args)))
      if (length(.asrw_counter$write_async_args) >= 1L) {
        a <- .asrw_counter$write_async_args[[1]]
        expect_identical(a$trigger, "import_complete",
                         info = "le trigger du site import SC doit être \"import_complete\" (A9)")
        expect_identical(a$dataset_ids, "ds_sc",
                         info = "les dataset_ids du site ne traversent pas la couture")
        expect_true(is.list(a$layers$layer1) &&
                      identical(names(a$layers$layer1), "sc_obj"),
                    info = paste("layer1 doit porter EXACTEMENT les objets présents sous",
                                 "leur nom VERBATIM du vocabulaire couche-1 (A3) ; reçus :",
                                 paste(names(a$layers$layer1), collapse = ", ")))
        expect_identical(a$layers$layer1$sc_obj, gd$sc_obj,
                         info = "l'objet couche-1 n'est pas stocké VERBATIM")
        expect_false(is.null(a$layers$layer2),
                     info = "layer2 absente — les 3 couches vont ENSEMBLE (contrat §2)")
        expect_false(is.null(a$layers$layer3),
                     info = "layer3 absente — les 3 couches vont ENSEMBLE (contrat §2)")
        expect_identical(a$dir, store,
                         info = paste("l'écriture n'est pas dirigée vers le magasin bac à",
                                      "sable (couture store_dir) — jamais l'arbre data_sc/"))
      }

      # — b) spatial_datasets seul non vide : layer1 == c(spatial_datasets = ...) —
      .asrw_counter$write_async_args <- list()
      gd$sc_obj <- NULL
      pkg <- .asrw_spatial_pkg()
      on.exit(unlink(c(pkg$bpcells_dir, pkg$raw_bpcells_dir, pkg$raw_dir),
                     recursive = TRUE, force = TRUE), add = TRUE)
      gd$spatial_datasets <- list(sampleB = pkg)
      gd$session_checkpoint_publish("import_complete", dataset_ids = "sampleB")
      if (length(.asrw_counter$write_async_args) >= 1L) {
        b <- .asrw_counter$write_async_args[[1]]
        expect_true(is.list(b$layers$layer1) &&
                      identical(names(b$layers$layer1), "spatial_datasets"),
                    info = paste("spatial_datasets non vide doit être inclus SEUL sous son",
                                 "nom verbatim quand les autres cibles sont absentes (A3) ;",
                                 "reçus :", paste(names(b$layers$layer1), collapse = ", ")))
        expect_identical(b$layers$layer1$spatial_datasets, gd$spatial_datasets,
                         info = "spatial_datasets n'est pas stocké VERBATIM")
      } else {
        fail("spatial_datasets non vide : aucun dispatch — A3 non appliqué")
      }

      # — c) tout absent : saut honnête, invisible(FALSE), AUCUN dispatch,
      #      raison visible à la ligne d'état (A3) —
      .asrw_counter$write_async_args <- list()
      gd$spatial_datasets <- list()
      res_c <- gd$session_checkpoint_publish("import_complete")
      expect_true(isFALSE(res_c),
                  info = paste("rien à enregistrer : le saut honnête doit retourner",
                               "invisible(FALSE) (A3) ; reçu :", paste(res_c, collapse = ", ")))
      expect_identical(length(.asrw_counter$write_async_args), 0L,
                       info = paste("tout absent : une écriture a été dispatchée avec une",
                                    "layer1 vide — JAMAIS de liste vide fabriquée (A3, le",
                                    "moteur refuserait une layer1 vide)"))
      st <- { session$flushReact(); .asrw_output(session, "status_line") }
      expect_true(grepl(.asrw_keys[[1]][1], st, fixed = TRUE),
                  info = paste("la raison du saut honnête n'est pas visible à la ligne",
                               "d'état (A3) ; rendu :", st))
    }
  })
  expect_false(dir.exists(store),
               info = "le magasin a été créé par le chemin T3 — interdit (refus §7.1 en test)")
})

# ── T4. Couche 2 — identité producteur (A4) : business_state publiée ==
#    scalaires de global_data$business_state (MÊME source que le producteur
#    DSTV-1, les 5 scalaires gelés verbatim — JAMAIS re-dérivés des objets) ;
#    manual_pipeline relais verbatim ; pipeline_steps_completed = nommé vide
#    DOCUMENTÉ ; défauts honnêtes pour deconv_props/derived_tables/banners. ---

test_that("ASR-WIRING T4 layer-2 producer identity: business_state is relayed from global_data (same DSTV-1 source, five frozen scalars), manual_pipeline verbatim, pipeline_steps_completed is the documented honest empty", {
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- .asrw_pub_ok(gd)
    expect_true(ok_reg,
                info = paste("publisher absente à l'activation — couche 2 non testable",
                             "(A1/A4 non livrés)"))
    if (ok_reg) {
      # Les 5 scalaires gelés du producteur DSTV-1 (drive_watcher.R:3869-3896,
      # fiche :59-62) + un objet lourd DÉLIBÉRÉMENT divergent : si le câblage
      # re-dérivait les scalaires depuis l'objet au lieu de relayer
      # global_data$business_state, l'identité ci-dessous saute (A4).
      gd$business_state <- list(has_data = TRUE, object_class = "Seurat",
                                n_elements = 1234, n_clusters = 7, deconv_done = TRUE)
      gd$manual_pipeline <- list(status = "done", step = "")
      gd$sc_obj <- data.frame(matrix(0, nrow = 3, ncol = 99))  # 99 != 1234 : leurre
      gd$session_checkpoint_publish("pipeline_step_complete")
      expect_identical(length(.asrw_counter$write_async_args), 1L,
                       info = "le site pipeline (pipeline_step_complete) n'a pas dispatché une écriture")
      if (length(.asrw_counter$write_async_args) >= 1L) {
        a <- .asrw_counter$write_async_args[[1]]
        expect_identical(a$trigger, "pipeline_step_complete",
                         info = "le trigger du site pipeline doit être \"pipeline_step_complete\" (A9)")
        l2 <- a$layers$layer2
        # L'attendu est produit par le MÊME moteur que le câblage consomme
        # (ts_session_checkpoint_layer2, session_checkpoint.R:162-199) — jamais
        # recalculé à la main dans le test (motif test-asr3-ui.R:156-162).
        exp_bs <- ts_session_checkpoint_layer2(business_state = gd$business_state)$business_state
        exp_mp <- ts_session_checkpoint_layer2(manual_pipeline = gd$manual_pipeline)$manual_pipeline
        expect_identical(l2$business_state, exp_bs,
                         info = paste("business_state n'est PAS le relais exact des scalaires de",
                                      "global_data$business_state (MÊME source que le producteur",
                                      "DSTV-1, A4 — re-dérivation depuis les objets interdite)"))
        expect_identical(l2$manual_pipeline, exp_mp,
                         info = "manual_pipeline n'est pas relayé VERBATIM depuis global_data (A4)")
        expect_identical(l2$pipeline_steps_completed, character(0),
                         info = paste("pipeline_steps_completed doit être le nommé VIDE",
                                      "DOCUMENTÉ (aucun flag par étape n'existe aujourd'hui,",
                                      "A4 — un flag fabriqué serait malhonnête)"))
        expect_null(l2$deconv_props,
                    info = paste("sans layer2_extras, deconv_props doit rester son défaut",
                                 "honnête NULL (A4)"))
        expect_identical(l2$derived_tables, list(),
                         info = paste("sans layer2_extras, derived_tables doit rester son",
                                      "défaut honnête list() (A4)"))
        expect_identical(l2$banners, list(),
                         info = paste("sans layer2_extras, banners doit rester son défaut",
                                      "honnête list() (A4)"))
      }
    }
  })
  expect_false(dir.exists(store),
               info = "le magasin a été créé par le chemin T4 — interdit (refus §7.1 en test)")
})

# ── T5. Couche 3 — actifs défensifs (A5) : par échantillon spatial
#    bpcells_dir (kind dir) / raw_bpcells_dir / raw_dir (fallback_source) et
#    spatial_reference$path ; existed_at_checkpoint stampé moteur. -------------

test_that("ASR-WIRING T5 layer-3 defensive assets: spatial sample dirs recorded (bpcells_dir kind dir, raw dirs, raw_dir as fallback_source), spatial_reference$path recorded, existed_at_checkpoint engine-stamped", {
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- .asrw_pub_ok(gd)
    expect_true(ok_reg,
                info = paste("publisher absente à l'activation — couche 3 non testable",
                             "(A1/A5 non livrés)"))
    if (ok_reg) {
      pkg <- .asrw_spatial_pkg()
      on.exit(unlink(c(pkg$bpcells_dir, pkg$raw_bpcells_dir, pkg$raw_dir),
                     recursive = TRUE, force = TRUE), add = TRUE)
      ref <- tempfile(pattern = "ts_asrw_ref_", fileext = ".rds"); writeLines("ref", ref)
      on.exit(unlink(ref, force = TRUE), add = TRUE)
      gd$spatial_datasets <- list(sampleA = pkg)
      gd$spatial_reference <- list(path = ref, n_cells = 10, n_genes = 5,
                                   backend = "bpcells", celltype_col = "ct")
      gd$session_checkpoint_publish("import_complete", dataset_ids = "sampleA")
      expect_identical(length(.asrw_counter$write_async_args), 1L,
                       info = "le site import spatial n'a pas dispatché une écriture")
      if (length(.asrw_counter$write_async_args) >= 1L) {
        a <- .asrw_counter$write_async_args[[1]]
        expect_identical(a$trigger, "import_complete",
                         info = "le trigger du site import spatial doit être \"import_complete\" (A9)")
        assets <- if (is.list(a$layers$layer3$assets)) a$layers$layer3$assets else list()
        find_asset <- function(match_fn) Filter(match_fn, assets)
        # Les actifs sont retrouvés PAR CONTENU (paths/kind/fallback) : les
        # clés du named list ne sont pas gelées par la passe, le contenu l'est
        # (A5).
        hit_bpc <- find_asset(function(e) identical(e$path, pkg$bpcells_dir))
        expect_identical(length(hit_bpc), 1L,
                      info = paste("l'actif bpcells_dir de l'échantillon spatial n'est pas",
                                   "enregistré en couche 3 (A5) ; actifs :",
                                   paste(vapply(assets, function(e) paste(e$path, collapse = ""), ""),
                                         collapse = " | ")))
        if (length(hit_bpc) == 1L) {
          expect_identical(hit_bpc[[1]]$kind, "dir",
                           info = "l'actif bpcells_dir doit porter kind \"dir\" (A5)")
          expect_true(isTRUE(hit_bpc[[1]]$existed_at_checkpoint),
                      info = paste("existed_at_checkpoint doit être stampé MOTEUR (TRUE pour",
                                   "un dir existant, fiche :66-67)"))
        }
        hit_rawb <- find_asset(function(e) identical(e$path, pkg$raw_bpcells_dir))
        expect_identical(length(hit_rawb), 1L,
                      info = "l'actif raw_bpcells_dir de l'échantillon spatial n'est pas enregistré (A5)")
        if (length(hit_rawb) == 1L) {
          expect_true(isTRUE(hit_rawb[[1]]$existed_at_checkpoint),
                      info = "existed_at_checkpoint doit être stampé moteur pour raw_bpcells_dir existant")
        }
        hit_fb <- find_asset(function(e) identical(e$fallback_source, pkg$raw_dir))
        expect_identical(length(hit_fb), 1L,
                      info = paste("la source d'import d'origine raw_dir doit être enregistrée",
                                   "comme fallback_source d'un actif (A5, contrat §5.3 : la",
                                   "re-matérialisation part de la source d'origine)"))
        hit_ref <- find_asset(function(e) identical(e$path, gd$spatial_reference$path))
        expect_identical(length(hit_ref), 1L,
                      info = paste("le manifeste de référence partagé",
                                   "global_data$spatial_reference$path n'est pas enregistré (A5)"))
        if (length(hit_ref) == 1L) {
          expect_true(isTRUE(hit_ref[[1]]$existed_at_checkpoint),
                      info = "existed_at_checkpoint doit être stampé moteur pour la référence existante")
        }
      }
    }
  })
  expect_false(dir.exists(store),
               info = "le magasin a été créé par le chemin T5 — interdit (refus §7.1 en test)")
})

# ── T6. Refus honnête sans pool (§7.1, probe FALSE injectable) : refus
#    structuré, AUCUN fichier écrit, raison visible à la ligne d'état. ---------

test_that("ASR-WIRING T6 honest refusal without pool: structured refusal relayed to the status line, zero files written, never a main-thread write (contract \u00a77.1)", {
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_wrap_engines()   # force_probe_false = TRUE par défaut : scénario §7.1
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- .asrw_pub_ok(gd)
    expect_true(ok_reg,
                info = paste("publisher absente à l'activation — le refus §7.1 n'est pas",
                             "testable (A1/A7 non livrés)"))
    if (ok_reg) {
      gd$sc_obj <- data.frame(x = 1:3)
      gd$session_checkpoint_publish("import_complete")
      expect_true("ts_session_checkpoint_write_async" %in% .asrw_counter$calls,
                  info = "l'écriture n'est pas passée par le SEAM write_async (A7)")
      expect_false("ts_session_checkpoint_write" %in% .asrw_counter$calls,
                   info = paste("une écriture SYNCHRONE a eu lieu sur le thread principal —",
                                "interdit absolu (contrat §7.1 : couche 1 par daemon mirai",
                                "uniquement, jamais de fallback)"))
      # Le refus RÉEL du moteur pour le même scénario — l'attendu vient du
      # moteur, pas du test (motif test-asr3-ui.R:156-162).
      refuse <- ts_session_checkpoint_write_async(
        .asrw_tiny_layers(), dir = store, trigger = "import_complete",
        pool_probe = function() FALSE)
      expect_true(isTRUE(refuse$refused) && length(refuse$reasons) >= 1L,
                  info = "le moteur n'a pas produit le refus structuré attendu (probe FALSE)")
      st <- { session$flushReact(); .asrw_output(session, "status_line") }
      if (isTRUE(refuse$refused) && length(refuse$reasons) >= 1L) {
        expect_true(any(vapply(refuse$reasons, function(r) grepl(r, st, fixed = TRUE), logical(1))),
                    info = paste("la raison du refus §7.1 n'est pas visible à la ligne d'état",
                                 "(A7 : résultat ok/refus -> ligne d'état réactive) ; rendu :", st))
      }
      expect_true(grepl("\u00c9chec de l'\u00e9criture du checkpoint", st, fixed = TRUE),
                  info = paste("le cadre français du refus (clé i18n 2) n'est pas rendu à la",
                               "ligne d'état ; rendu :", st))
    }
  })
  expect_false(dir.exists(store),
               info = paste("un fichier a été écrit malgré le refus sans pool — interdit",
                            "(§7.1 : AUCUN fichier, le refus précède toute écriture)"))
})

# ── T7a. Coutume T7 (arbitrage (a), 1/2) — la FORME du seam : formels + défauts
#    = constantes moteur, refus §7.1 inchangé, forward lexical dans l'appel
#    mirai. Aucun module requis : ce sont des assertions MOTEUR. ---------------

test_that("ASR-WIRING T7a settings seam form: write_async carries retention_n/disk_cap_bytes with ENGINE-CONSTANT defaults, the \u00a77.1 refusal is unchanged, and both forwards cross the mirai boundary (arbitrage (a), documented in header)", {
  f <- ts_session_checkpoint_write_async
  fn <- names(formals(f))
  expect_true("retention_n" %in% fn,
              info = paste("ARBITRAGE T7 (a) : formals(ts_session_checkpoint_write_async)",
                           "ne porte pas retention_n — les réglages ASR-4 ne peuvent pas",
                           "atteindre l'écriture (session_checkpoint.R:605-631)"))
  expect_true("disk_cap_bytes" %in% fn,
              info = "ARBITRAGE T7 (a) : formals() ne porte pas disk_cap_bytes")
  if (all(c("retention_n", "disk_cap_bytes") %in% fn)) {
    # Défauts = CONSTANTES MOTEUR (drift-proof) : tout appelant qui ne les
    # passe pas garde le comportement existant byte pour byte (contrepartie
    # de l'arbitrage — la suite moteur complète reste verte).
    expect_identical(deparse(formals(f)$retention_n), "TS_SESSION_CHECKPOINT_RETENTION_N",
                     info = paste("le défaut de retention_n doit être LA CONSTANTE MOTEUR",
                                  "(pas une copie littérale dérivée)"))
    expect_identical(deparse(formals(f)$disk_cap_bytes), "TS_SESSION_CHECKPOINT_DISK_CAP_BYTES",
                     info = "le défaut de disk_cap_bytes doit être LA CONSTANTE MOTEUR")
  }
  # (2) Refus §7.1 INCHANGÉ (probe FALSE explicite, scénario moteur
  #     test-session-checkpoint-engine.R:504-515) — garde de régression.
  store <- .asrw_store()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  res <- ts_session_checkpoint_write_async(.asrw_tiny_layers(), dir = store,
                                           trigger = "manual",
                                           pool_probe = function() FALSE)
  expect_false(isTRUE(res$ok), info = "le refus §7.1 a disparu avec l'extension T7 — régression")
  expect_true(isTRUE(res$refused) &&
                any(grepl("daemon|mirai|main thread", res$reasons, ignore.case = TRUE)),
              info = "le refus §7.1 n'est plus structuré avec sa raison — régression")
  expect_identical(length(list.files(store)), 0L,
                info = "le refus §7.1 écrit désormais — régression grave (contrat §7.1)")
  # (3) Forward LEXICAL (motif test-asr3-ui.R §8/§9) : les réglages doivent
  #     traverser la couture mirai DEUX FOIS — en arguments nommés du mirai
  #     (les seules variables que le daemon reçoit, sinon « objet introuvable »
  #     côté daemon, non headless-détectable) ET dans l'appel
  #     ts_session_checkpoint_write() du .expr (sinon le daemon écrirait avec
  #     les défauts, dropping silencieux des réglages). Code seul : les
  #     commentaires ne sont pas du code (règle C17, check_conventions.R).
  eng_lines <- readLines(file.path(ts_project_root(), "tools", "pkg_skeleton", "R",
                                   "session_checkpoint.R"), warn = FALSE)
  fn_norm <- gsub("[[:space:]]+", " ",
                  paste(deparse(f, width.cutoff = 500L), collapse = " "))
  mpos <- regexpr("mirai::mirai\\(", fn_norm, perl = TRUE)
  expect_true(mpos > 0, info = "le seam mirai a disparu du write_async — régression structurelle")
  if (mpos > 0) {
    mirai_tail <- substring(fn_norm, mpos)
    n_ret <- length(gregexpr("retention_n\\s*=\\s*retention_n\\b", mirai_tail, perl = TRUE)[[1]])
    n_cap <- length(gregexpr("disk_cap_bytes\\s*=\\s*disk_cap_bytes\\b", mirai_tail, perl = TRUE)[[1]])
    expect_true(n_ret >= 2L,
                info = paste("le forward retention_n = retention_n doit apparaître AUX DEUX",
                             "points (arguments nommés du mirai + appel",
                             "ts_session_checkpoint_write() du .expr) ; occurrences :", n_ret))
    expect_true(n_cap >= 2L,
                info = paste("le forward disk_cap_bytes = disk_cap_bytes doit apparaître AUX DEUX",
                             "points (arguments nommés du mirai + appel ts_session_checkpoint_write()) ;",
                             "occurrences :", n_cap))
    wpos <- regexpr("ts_session_checkpoint_write\\(", mirai_tail, perl = TRUE)
    expect_true(wpos > 0,
                info = "l'appel daemon ts_session_checkpoint_write() a disparu du .expr mirai")
    if (wpos > 0) {
      wtxt <- substring(mirai_tail, wpos, wpos + 400L)
      expect_true(grepl("retention_n\\s*=\\s*retention_n\\b", wtxt, perl = TRUE),
                  info = paste("l'appel daemon ts_session_checkpoint_write() ne reçoit pas",
                               "retention_n — dropping silencieux des réglages (T7 arbitrage (a))"))
      expect_true(grepl("disk_cap_bytes\\s*=\\s*disk_cap_bytes\\b", wtxt, perl = TRUE),
                  info = "l'appel daemon ts_session_checkpoint_write() ne reçoit pas disk_cap_bytes")
    }
  }
})

# ── T7b. Coutume T7 (arbitrage (a), 2/2) — les RÉGLAGES PUBLIÉS ASR-4
#    atteignent l'APPEL write_async avec les bonnes valeurs (wrapper
#    délégant = seam observable) ; settings absentes -> défauts moteur. -------

test_that("ASR-WIRING T7b settings dispatch: published retention_n/disk_cap_bytes reach the write_async call; absent settings mean the engine defaults; the sandbox store_dir crosses the seam", {
  store <- .asrw_store()
  gd <- .asrw_gd()
  .asrw_wrap_engines()
  on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    ok_reg <- .asrw_pub_ok(gd)
    expect_true(ok_reg,
                info = paste("publisher absente à l'activation — le dispatch des réglages",
                             "n'est pas testable (A1/A6 non livrés)"))
    if (ok_reg) {
      gd$sc_obj <- data.frame(x = 1:3)
      # Réglages publiés par ASR-4 (0101114) — la publication elle-même est le
      # sujet de test-asr4-settings.R ; ici elle est la SOURCE du câblage (A6).
      gd$session_checkpoint_settings <- list(retention_n = 5L, disk_cap_bytes = 2147483648)
      gd$session_checkpoint_publish("pipeline_step_complete")
      expect_identical(length(.asrw_counter$write_async_args), 1L,
                       info = "aucun dispatch vers write_async avec réglages publiés")
      if (length(.asrw_counter$write_async_args) >= 1L) {
        a <- .asrw_counter$write_async_args[[1]]
        expect_identical(a$retention_n, 5L,
                         info = paste("retention_n publié (5L) n'atteint PAS l'appel",
                                      "write_async — les réglages seraient dropping",
                                      "silencieusement (T7, A6)"))
        expect_identical(a$disk_cap_bytes, 2147483648,
                         info = paste("disk_cap_bytes publié (2 GiB en octets) n'atteint PAS",
                                      "l'appel write_async (A6)"))
        expect_identical(a$dir, store,
                         info = paste("l'écriture n'est pas dirigée vers le magasin bac à",
                                      "sable (couture store_dir)"))
        expect_identical(a$trigger, "pipeline_step_complete",
                         info = "le trigger ne traverse pas la couture (A7)")
      }
      # Settings absentes (NULL) : défauts MOTEUR à la couture — soit passés
      # explicitement, soit omis (les formels par défaut = constantes, T7a).
      .asrw_counter$write_async_args <- list()
      gd$session_checkpoint_settings <- NULL
      gd$session_checkpoint_publish("pipeline_step_complete")
      if (length(.asrw_counter$write_async_args) >= 1L) {
        a2 <- .asrw_counter$write_async_args[[1]]
        if (!is.null(a2$retention_n)) {
          expect_identical(a2$retention_n, TS_SESSION_CHECKPOINT_RETENTION_N,
                           info = "settings absentes : la valeur passée doit être le défaut moteur")
        }
        if (!is.null(a2$disk_cap_bytes)) {
          expect_identical(a2$disk_cap_bytes, TS_SESSION_CHECKPOINT_DISK_CAP_BYTES,
                           info = "settings absentes : le plafond passé doit être le défaut moteur")
        }
      } else {
        fail("settings NULL : aucun dispatch — le chemin par défaut n'est pas câblé")
      }
    }
  })
  expect_false(dir.exists(store),
               info = "le magasin a été créé par le chemin T7b — interdit (refus §7.1 en test)")
})

# ── T8. Garde lexicale des 7 sites (A9, motif test-asr3-ui.R §8/§9) : chaque
#    site contient l'appel GARDÉ au bon trigger près de son point de pose
#    (ancre unique mesurée), zéro timer/polling dans la fenêtre (§7.2). -------

.asrw_norm <- function(x) gsub("[[:space:]]+", " ", x)

.asrw_guard_rx <- function(trigger) {
  paste0("if\\s*\\(\\s*is\\.function\\(\\s*global_data\\$session_checkpoint_publish\\s*\\)\\s*\\)",
         "\\s*global_data\\$session_checkpoint_publish\\(\\s*\"", trigger, "\"")
}
.asrw_deconv_rx <- paste0(
  "if\\s*\\(\\s*is\\.function\\(\\s*global_data\\$session_checkpoint_publish\\s*\\)\\s*\\)",
  "\\s*global_data\\$session_checkpoint_publish\\(\\s*\"deconv_complete\"\\s*,\\s*",
  "layer2_extras\\s*=\\s*list\\(\\s*deconv_props\\s*=\\s*shared_rv\\$deconv_props\\s*\\)\\s*\\)")

# Jetons timer/polling — mêmes familles que la garde moteur
# (test-session-checkpoint-engine.R:497-500) : le déclenchement est
# ÉVÉNEMENTIEL (contrat §3.1/§7.2), la fenêtre du point de pose ne peut
# introduire aucun schedulier.
.asrw_banned_timer_rx <- paste0(
  "invalidateLater\\(|later::later\\(|\\blater\\(|reactivePoll\\(|reactiveTimer\\(|",
  "setInterval\\(|setTimer|scheduleTask\\(|Sys\\.sleep\\(|every\\(")

.asrw_sites <- list(
  list(label = "import SC (sc_obj)",
       file = "modules/import/mod_import_sc.R",
       trigger = "import_complete",
       anchor = 'global_data\\$sc_obj <- prepared',
       window = 10L, deconv = FALSE),
  list(label = "import bulk (bulk_datasets)",
       file = "modules/import/mod_import_bulk.R",
       trigger = "import_complete",
       anchor = 'global_data\\$bulk_datasets <- res',
       window = 10L, deconv = FALSE),
  list(label = "import spatial (par \u00e9chantillon)",
       file = "modules/import/mod_import_spatial.R",
       trigger = "import_complete",
       anchor = 'global_data\\$spatial_datasets\\[\\[sample_name\\]\\] <- spatial_pkg',
       window = 10L, deconv = FALSE),
  list(label = "autopipeline SC (site done DSTV-1b)",
       file = "modules/sc/mod_sc.R",
       trigger = "pipeline_step_complete",
       anchor = 'status = "done", step = ""',
       window = 12L, deconv = FALSE),
  list(label = "autopipeline bulk (site done DSTV-1b)",
       file = "modules/bulk/mod_bulk.R",
       trigger = "pipeline_step_complete",
       anchor = 'status = "done", step = ""',
       window = 12L, deconv = FALSE),
  list(label = "pipeline spatial (.finish_pipeline)",
       file = "modules/spatial/mod_spatial_pipeline.R",
       trigger = "pipeline_step_complete",
       anchor = '\\.finish_pipeline <- function\\(\\)',
       window = 12L, deconv = FALSE),
  list(label = "d\u00e9convolution compl\u00e8te",
       file = "modules/spatial/deconv/mod_spatial_deconv.R",
       trigger = "deconv_complete",
       anchor = 'shared_rv\\$deconv_props <- deconv_task\\$result\\(\\)',
       window = 8L, deconv = TRUE)
)

test_that("ASR-WIRING T8 lexical guard: each of the 7 milestone sites holds the guarded publish call at its frozen trigger near the milestone, with no new timer/polling (contract \u00a73.1/\u00a77.2)", {
  for (st in .asrw_sites) {
    path <- file.path(ts_project_root(), st$file)
    expect_true(file.exists(path), info = paste("fichier site introuvable :", st$file))
    if (!file.exists(path)) next
    lines <- readLines(path, warn = FALSE)
    hits <- grep(st$anchor, lines, perl = TRUE)
    expect_true(length(hits) >= 1L,
                info = paste("ancre rot\u00e9e pour le site", st$label, ":", st$anchor,
                             "— mettre à jour l'ancre mesurée, pas le contrat"))
    if (!length(hits)) next
    win <- unique(unlist(lapply(hits, function(i) seq(max(1, i - st$window),
                                                      min(length(lines), i + st$window)))))
    win_src <- .asrw_norm(paste(lines[win], collapse = " "))
    rx <- if (isTRUE(st$deconv)) .asrw_deconv_rx else .asrw_guard_rx(st$trigger)
    expect_true(grepl(rx, win_src, perl = TRUE),
                info = paste0("site \u00ab ", st$label, " \u00bb (", st$file, ") : l'appel GARD\u00c9 au ",
                              "publisher est absent de la fen\u00eatre du point de pose — ",
                              "forme gel\u00e9e : if (is.function(global_data$session_",
                              "checkpoint_publish)) global_data$session_checkpoint_",
                              "publish(\"", st$trigger, "\", ...)"))
    expect_identical(length(grep(.asrw_banned_timer_rx, win_src, perl = TRUE, value = TRUE)), 0L,
                  info = paste("un timer/polling est apparu dans la fenêtre du point de",
                               "pose du site", st$label, "— déclenchement ÉVÉNEMENTIEL",
                               "seulement (contrat §3.1/§7.2)"))
  }
})

test_that("ASR-WIRING T8 vocabulary drift: every publish call across the sites uses the frozen trigger vocabulary, and the module gains no timer/polling", {
  all_occ <- character(0)
  for (st in .asrw_sites) {
    src <- .asrw_norm(paste(readLines(file.path(ts_project_root(), st$file), warn = FALSE),
                            collapse = " "))
    all_occ <- c(all_occ, regmatches(src, gregexpr(
      'session_checkpoint_publish\\(\\s*"[A-Za-z_]+"', src, perl = TRUE))[[1]])
  }
  trigs <- sub('.*"([A-Za-z_]+)"$', "\\1", all_occ)
  expect_true(all(trigs %in% TS_SESSION_CHECKPOINT_TRIGGERS),
              info = paste("trigger hors vocabulaire GELÉ (import_complete,",
                           "pipeline_step_complete, deconv_complete, manual) :",
                           paste(unique(trigs[!trigs %in% TS_SESSION_CHECKPOINT_TRIGGERS]),
                                 collapse = ", ")))
  mod_src <- .asrw_norm(paste(readLines(
    file.path(ts_project_root(), "modules", "session", "mod_session_checkpoint.R"),
    warn = FALSE), collapse = " "))
  expect_identical(length(grep(.asrw_banned_timer_rx, mod_src, perl = TRUE, value = TRUE)), 0L,
                info = paste("le module session introduit un timer/polling — le magasin",
                             "est relu à CHAQUE activation du toggle, point (contrat §3.1/§7.2)"))
})

# ── T9. i18n fr/en des nouvelles chaînes visibles (append scopé) ; zéro CSS
#    www/ (règle post-O13, motif test-asr3-ui.R:413-424 / test-asr4-settings.R). -

test_that("ASR-WIRING T9 i18n: every new visible wiring string has its fr/en key pair in i18n/translation.json (scoped append)", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))
  for (k in .asrw_keys) {
    idx <- match(k[1], fr)
    expect_false(is.na(idx),
                 info = paste("clé FR absente de translation.json :", k[1]))
    if (!is.na(idx)) {
      expect_identical(en[[idx]], k[2],
                       info = paste("paire EN absente ou divergente pour :", k[1]))
    }
  }
})

test_that("ASR-WIRING T9 style: the wiring adds no CSS in www/ and no style injection (post-O13 rule)", {
  mod_path <- file.path(ts_project_root(), "modules", "session",
                        "mod_session_checkpoint.R")
  mod_src <- paste(readLines(mod_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_src),
               info = paste("le câblage injecte du style — interdit (0 CSS, primitives",
                            "bslib/shiny seulement, règle post-O13)"))
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE,
                      ignore.case = TRUE, full.names = FALSE)
    expect_identical(
      length(css[grep("asr|wiring|checkpoint|session_ckpt", css, ignore.case = TRUE)]),
      0L,
      info = paste("un fichier CSS WRITE-SIDE est apparu dans www/ — interdit",
                   "(règle post-O13)"))
  }
})
