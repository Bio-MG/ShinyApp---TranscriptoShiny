# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
# v10 (backlog court-terme #2, session "puck_01 bugfix + backlog" — voir
#    handoff_spatial_bio-mg.md) : le cache par-echantillon (v9 ci-dessous)
#    est deplace de reactiveVal(list()) local vers
#    global_data$spatial_results_cache -- app.R n'a aucun acces direct au
#    reactiveVal INTERNE d'un module Shiny, alors que global_data est deja
#    le bus partage que app.R serialise dans save_session_btn/restaure dans
#    load_session_file. Zero changement de logique de cache, uniquement de
#    "conteneur" -- rend une session Spatial multi-echantillon veritablement
#    resumable apres sauvegarde/rechargement (voir app.R pour la partie
#    serialisation).
#
# v9 (Chantier 3 round-2 — per-dataset result cache). Switching the active
# dataset used to unconditionally NULL every derived result (QC/clusters/
# deconv/niche) -- correct the first time a dataset is visited, but wasteful
# every time the user just wants to flip BACK to a sample already analyzed
# earlier in the session (reported: "le clustering doit etre recalcule quand
# on a un multi echantillon quand on veut visualiser"). results_cache() now
# snapshots shared_rv into an in-memory, SESSION-SCOPED cache keyed by
# dataset name right before switching away from it, and restores it if
# present when switching back -- a fresh dataset (never analyzed) still
# gets the same clean-slate NULLs as before. ROI state (roi_ids/roi_bbox/
# roi_markers) is deliberately NEVER cached/restored: it is tightly coupled
# to mod_spatial_viz.R's own local reactive state (umap_df, linked
# selections), which is not safely restorable from here, so ROI is always
# reset on a dataset switch, same as before this change. This cache does
# NOT persist across app restarts / session save-load (out of scope for
# this round -- see handoff note).
#
# v8 (Chantier 3 refonte — daemon badge honesty): 3-state daemon badge
# (ready/degraded/inactive) + diagnostics.
#
# v6 (Phase 4 — "Working with multiple slices"): 5th top-level tab,
# "Multi-echantillons" (mod_spatial_multi.R), active-dataset switcher.
#
# v6.1 (audit step 3.9b — multi-sample bugfix): `ns <- session$ns` fix.
#
# v5 (Phase 3 — ROI/crop): shared_rv gained roi_ids / roi_bbox / roi_markers.
#
# v4 (UX feedback): daemon status/reset row AND dataset banner folded into
# the tab strip itself.
#
# v2 (post-test-3): "Reinitialiser les daemons" button.
#
# v7 (Phase 5 — niches spatiales): 6th top-level tab, "Niches spatiales".
#
# v11 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    2 nouveaux onglets additifs, zero changement de logique existante :
#      - "Pipeline auto" (mod_spatial_pipeline.R) : QC -> Clustering ->
#        Deconvolution (si reference partagee) -> Niches, en 1 clic, memes
#        resultats/memes champs shared_rv que si chaque etape avait ete
#        lancee manuellement depuis son propre onglet.
#      - "7. Export" (mod_spatial_export.R) : paquet .zip (CSV/PNG deja
#        calcules) + script R reproductible.
#    shared_rv gagne 5 champs *_params (miroir des parametres utilises pour
#    chaque resultat, ecrits par chaque onglet au moment du clic sur son
#    propre bouton) -- lus par mod_spatial_export.R pour le script
#    reproductible ; caches/restaures comme le reste au changement
#    d'echantillon (voir .cacheable_fields ci-dessous).
#
# v12 (feedback biologiste, session suivante) :
#    - "8. Rapport" (mod_spatial_report.R) : rapport HTML/PDF multi-
#      echantillons (voir handoff pour details).
#    - shared_rv$umap_df AJOUTE et rendu cacheable : le sketch-UMAP (onglet
#      4) etait un reactiveVal LOCAL a mod_spatial_viz.R, perdu a chaque
#      changement d'echantillon en multi-echantillons -- deplace ici pour
#      beneficier du meme cache par-echantillon que le reste.
#    - Selecteur "Actif" : fix CSS (debordement sur le menu "Session" avec
#      un nom d'echantillon long).
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      id = ns("spatial_nav"),

      nav_panel("\U0001F680 Pipeline auto", icon = icon("bolt"),
                mod_spatial_pipeline_ui(ns("pipeline"))),

      nav_panel("1. QC & Autocorrelation", icon = icon("filter"),
                mod_spatial_qc_ui(ns("qc"))),
      
      nav_panel("2. Clustering (BANKSY-lite)", icon = icon("shapes"),
                mod_spatial_cluster_ui(ns("cluster"))),
      
      nav_panel("3. Deconvolution", icon = icon("puzzle-piece"),
                mod_spatial_deconv_ui(ns("deconv"))),
      
      nav_panel("4. Visualisation", icon = icon("eye"),
                mod_spatial_viz_ui(ns("viz"))),
      
      nav_panel("5. Multi-echantillons", icon = icon("layer-group"),
                mod_spatial_multi_ui(ns("multi"))),
      
      nav_panel("6. Niches spatiales", icon = icon("diagram-project"),
                mod_spatial_niche_ui(ns("niche"))),

      nav_panel("7. Export & Rapport", icon = icon("file-export"),
                navset_pill(
                  nav_panel("Paquet & Script", mod_spatial_export_ui(ns("export"))),
                  nav_panel("Rapport HTML/PDF", mod_spatial_report_ui(ns("report")))
                )),
      
      nav_spacer(),
      # v9 (fix UX) : un <select> imbrique dans un item de menu deroulant
      # Bootstrap est peu fiable au clic (le menu intercepte/ferme avant que
      # le <select> n'ouvre son propre popup). Sorti en nav_item() autonome,
      # directement cliquable, juste a cote du menu "Session".
      nav_item(uiOutput(ns("active_dataset_ui"))),
      bslib::nav_menu(
        title = tagList(icon("gear"), "Session"), align = "right",
        nav_item(uiOutput(ns("daemon_status_ui"))),
        nav_item(actionButton(ns("btn_reset_daemons"), "Reinitialiser les daemons",
                              class = "btn-outline-warning btn-sm w-100 mt-1", icon = icon("rotate")))
      )
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (!spatial_daemons_ready()) init_spatial_daemons(n_daemons = 6)
    
    # ── Shared reactive bus for all child modules ─────────────────────────
    shared_rv <- reactiveValues(
      active_tab       = "1. QC & Autocorrelation",
      qc_metrics       = NULL,
      qc_pass_idx      = NULL,
      qc_params        = NULL,
      moran_results    = NULL,
      moran_params     = NULL,
      cluster_labels   = NULL,
      cluster_params   = NULL,
      deconv_props     = NULL,
      deconv_params    = NULL,
      cluster_markers  = NULL,
      current_fov_crop = NULL,
      roi_ids          = NULL,
      roi_bbox          = NULL,
      roi_markers       = NULL,
      niche_labels      = NULL,
      niche_composition = NULL,
      niche_params      = NULL,
      # Feedback biologiste : le sketch-UMAP (onglet 4) se perdait a chaque
      # changement d'echantillon en multi-echantillons (etait un reactiveVal
      # LOCAL a mod_spatial_viz.R, jamais mis en cache). Deplace ici pour
      # beneficier GRATUITEMENT du meme mecanisme de cache par-echantillon
      # que qc_metrics/cluster_labels/etc. ci-dessus (voir .cacheable_fields).
      umap_df           = NULL,
      # Feedback biologiste ("ajouter la fonction d'ajout au rapport") :
      # vues personnalisees sauvegardees depuis l'onglet 4 (bouton "Ajouter
      # cette vue au rapport") -- named list, cle = label choisi par
      # l'utilisateur, valeur = cfg (color_by/qc_metric/gene/deconv_celltype/
      # show_cluster_labels). Rendu dans le rapport (onglet 7, section
      # "Rapport HTML/PDF") via build_saved_viz_df() (R/utils_spatial_report.R).
      saved_viz_list    = list()
    )
    
    # ── v10 (backlog court-terme #2): per-dataset result cache, now backed
    # by global_data$spatial_results_cache instead of a local reactiveVal.
    # A Shiny module's internal reactiveVal is invisible to app.R's server
    # function (different scope) -- global_data is the shared reactiveValues
    # bus every module already reads/writes, and app.R's save_session_btn/
    # load_session_file already serializes arbitrary global_data fields, so
    # this one-line change of "where the cache lives" makes multi-sample
    # Spatial sessions genuinely resumable after a save/reload, with zero
    # change to the caching LOGIC itself (still session-scoped in-memory
    # data structure, just addressed through global_data now).
    .get_cache <- function() global_data$spatial_results_cache %||% list()
    .set_cache <- function(cache) { global_data$spatial_results_cache <- cache }
    
    .cacheable_fields <- c("qc_metrics", "qc_pass_idx", "qc_params", "moran_results", "moran_params",
                           "cluster_labels", "cluster_params", "deconv_props", "deconv_params",
                           "cluster_markers", "niche_labels", "niche_composition", "niche_params",
                           "umap_df", "saved_viz_list")
    
    .snapshot_shared_rv <- function() {
      stats::setNames(lapply(.cacheable_fields, function(f) shared_rv[[f]]), .cacheable_fields)
    }
    .restore_shared_rv <- function(snap) {
      for (f in .cacheable_fields) shared_rv[[f]] <- snap[[f]] %||% NULL
    }
    .clear_shared_rv <- function() {
      for (f in .cacheable_fields) shared_rv[[f]] <- NULL
    }
    
    output$daemon_status_ui <- renderUI({
      input$btn_reset_daemons
      status <- tryCatch(spatial_daemon_status(), error = function(e) "inactive")
      label <- switch(status,
                      "ready"    = "\u2705 daemons actifs (verifies)",
                      "degraded" = "\u26a0\ufe0f daemons actifs -- preload degrade",
                      "\u26aa daemons inactifs"
      )
      css_class <- switch(status, "ready" = "text-success", "degraded" = "text-warning", "text-muted")
      tagList(
        tags$span(class = paste("small", css_class), style = "align-self:center;", label),
        if (identical(status, "degraded")) {
          tags$details(
            style = "font-size:0.68rem; margin-top:2px;",
            tags$summary("Diagnostic"),
            tags$pre(style = "white-space:pre-wrap; max-width:260px;",
                     tryCatch(spatial_daemon_diagnostics_text(), error = function(e) conditionMessage(e)))
          )
        }
      )
    })
    
    observeEvent(input$btn_reset_daemons, {
      ok <- tryCatch(reset_spatial_daemons(6), error = function(e) FALSE)
      if (isTRUE(ok)) {
        st <- tryCatch(spatial_daemon_status(), error = function(e) "inactive")
        msg <- if (identical(st, "ready")) {
          "\U1F504 Daemons mirai reinitialises et verifies — relancez votre tache."
        } else {
          "\U1F504 Daemons mirai reinitialises, mais la verification reste en echec — voir le diagnostic."
        }
        showNotification(msg, type = if (identical(st, "ready")) "message" else "warning", duration = 6)
      } else {
        showNotification("Echec de la reinitialisation des daemons — voir la console R.", type = "error", duration = 8)
      }
    })
    
    output$active_dataset_ui <- renderUI({
      ds_names <- names(global_data$spatial_datasets)
      if (length(ds_names) < 2) return(NULL)
      # FIX (UI feedback) : un nom d'echantillon long faisait deborder ce
      # <select> par-dessus le menu "Session" voisin (badge daemons + reset).
      # Cause : flex-item par defaut a min-width:auto, qui empeche tout
      # retrecissement/troncature reel meme avec un width fixe sur le
      # selectInput lui-meme. On borne le conteneur ET on force
      # min-width:0 sur le wrapper flex pour autoriser la troncature CSS
      # (text-overflow:ellipsis) au lieu du debordement.
      tags$span(
        style = "display:flex; align-items:center; gap:6px; max-width:240px; min-width:0;",
        tags$style(HTML(sprintf(
          "#%s { text-overflow: ellipsis; overflow: hidden; white-space: nowrap; }",
          ns("active_dataset_select")
        ))),
        tags$span(class = "small text-muted", style = "flex-shrink:0;", "Actif :"),
        div(style = "min-width:0; flex:1 1 auto;",
            title = global_data$active_spatial_dataset %||% ds_names[1],
            selectInput(ns("active_dataset_select"), NULL, choices = ds_names,
                        selected = global_data$active_spatial_dataset %||% ds_names[1],
                        width = "100%"))
      )
    })
    
    # v10 (feedback biologiste — fix cache multi-echantillons) : le
    # dropdown ne fait plus QUE proposer un nouveau nom -- toute la logique
    # de snapshot/restauration vit dans l'observer CENTRALISE ci-dessous,
    # qui reagit a global_data$active_spatial_dataset lui-meme plutot qu'a
    # input$active_dataset_select. Avant ce correctif, SEUL ce dropdown
    # declenchait le snapshot -- mod_import_spatial.R changeait
    # active_spatial_dataset DIRECTEMENT lors d'un 2e (ou N-ieme) import,
    # court-circuitant entierement cette logique : les resultats de
    # l'echantillon SORTANT n'etaient jamais snapshotes (perdus), et
    # shared_rv n'etait ni vide ni restaure pour le NOUVEL echantillon --
    # il heritait silencieusement des resultats de l'ancien. D'ou le bug
    # rapporte : "au passage au second echantillon, des donnees du premier
    # ne sont pas sauvegardees... impossible de revenir".
    observeEvent(input$active_dataset_select, {
      req(input$active_dataset_select %in% names(global_data$spatial_datasets))
      if (identical(input$active_dataset_select, global_data$active_spatial_dataset)) return()
      global_data$active_spatial_dataset <- input$active_dataset_select
    })
    
    # v10 (feedback biologiste — fix cache multi-echantillons) : synchronise
    # global_data$spatial_obj sur SA PROPRE observation, separee du
    # snapshot/restauration ci-dessous. Necessaire car re-importer SOUS UN
    # NOM DEJA ACTIF (ex: reimport apres correction d'orientation) ne fait
    # PAS changer la VALEUR de active_spatial_dataset (les reactiveValues de
    # Shiny n'invalident pas sur une reassignation identique) -- un seul
    # observer cale sur active_spatial_dataset manquerait donc ce cas et
    # laisserait spatial_obj pointer vers l'ancienne version (perimee) de
    # l'echantillon. Watcher AUSSI spatial_datasets lui-meme couvre ce cas.
    observeEvent(list(global_data$active_spatial_dataset, global_data$spatial_datasets), {
      nm <- global_data$active_spatial_dataset
      if (!is.null(nm) && nm %in% names(global_data$spatial_datasets)) {
        global_data$spatial_obj <- global_data$spatial_datasets[[nm]]
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    prev_active_dataset <- reactiveVal(NULL)
    
    observeEvent(global_data$active_spatial_dataset, {
      new_name <- global_data$active_spatial_dataset
      old_name <- prev_active_dataset()
      
      # Snapshot the OUTGOING dataset's results before switching away --
      # regardless of WHICH code changed active_spatial_dataset (dropdown
      # here, or a fresh import in mod_import_spatial.R). Guarded against
      # old_name no longer being a valid key (e.g. right after a full
      # session load wholesale-replaces spatial_datasets) -- snapshotting
      # under a stale/foreign key would be meaningless.
      if (!is.null(old_name) && !identical(old_name, new_name) &&
          old_name %in% names(global_data$spatial_datasets)) {
        cache <- .get_cache()
        cache[[old_name]] <- .snapshot_shared_rv()
        .set_cache(cache)
      }
      
      # ROI state stays tied to mod_spatial_viz.R's own local reactive state
      # (umap_df, linked lasso selection) -- not safely restorable from
      # here, always reset on a dataset switch (unchanged from before).
      shared_rv$roi_ids     <- NULL
      shared_rv$roi_bbox    <- NULL
      shared_rv$roi_markers <- NULL
      
      if (!is.null(new_name)) {
        cached <- .get_cache()[[new_name]]
        if (!is.null(cached)) {
          .restore_shared_rv(cached)
          if (!is.null(old_name)) {
            showNotification(sprintf("Echantillon actif : %s (resultats precedents restaures).", new_name),
                             type = "message", duration = 4)
          }
        } else {
          .clear_shared_rv()
          if (!is.null(old_name)) {
            showNotification(sprintf("Echantillon actif : %s", new_name), type = "message", duration = 4)
          }
        }
      }
      
      prev_active_dataset(new_name)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)
    mod_spatial_multi_server("multi", global_data, shared_rv)
    mod_spatial_niche_server("niche", global_data, shared_rv)
    mod_spatial_pipeline_server("pipeline", global_data, shared_rv)
    mod_spatial_export_server("export", global_data, shared_rv)
    mod_spatial_report_server("report", global_data, shared_rv)
    
    observeEvent(input$spatial_nav, { shared_rv$active_tab <- input$spatial_nav })
  })
}
