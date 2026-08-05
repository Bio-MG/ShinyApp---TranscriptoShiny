# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
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
# =============================================================================
# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
# v10 (multi-sample robustness fix): dataset-switch cache/reset is now
# driven by an observer on global_data$active_spatial_dataset ITSELF (keyed
# by "name@created_at" identity), not just the local nav selectInput. Root
# cause fixed: mod_import_spatial_server() auto-activates a freshly (re-)
# imported dataset by writing global_data$active_spatial_dataset/spatial_obj
# DIRECTLY, bypassing the old selectInput-only observer entirely -- stale
# shared_rv$qc_pass_idx from the PREVIOUS dataset then reached a NEW
# (smaller, or same-name-but-resized) BPCells matrix inside a mirai daemon,
# crashing with an opaque "vctrs::vec_slice" out-of-bounds error. Keying the
# cache by "name@created_at" (not just name) also fixes a related edge case:
# re-importing under an EXISTING name now always yields a clean slate
# instead of possibly restoring stale cached results with wrong dimensions.
# Paired with a defensive safe_pass_idx() guard (R/utils_spatial_async.R)
# in mod_spatial_qc.R/mod_spatial_cluster.R/mod_spatial_deconv.R.
#
# v9 (per-dataset result cache), v8 (daemon badge honesty), v6 (multi-slice),
# v6.1 (ns fix), v5 (ROI/crop), v4 (UX), v2 (daemon reset), v7 (niches).
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      id = ns("spatial_nav"),
      
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
      
      nav_spacer(),
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
    
    shared_rv <- reactiveValues(
      active_tab       = "1. QC & Autocorrelation",
      qc_metrics       = NULL,
      qc_pass_idx      = NULL,
      moran_results    = NULL,
      cluster_labels   = NULL,
      deconv_props     = NULL,
      cluster_markers  = NULL,
      current_fov_crop = NULL,
      roi_ids          = NULL,
      roi_bbox          = NULL,
      roi_markers       = NULL,
      niche_labels      = NULL,
      niche_composition = NULL
    )
    
    # ── Per-dataset-VERSION result cache (session-scoped, in-memory only) ──
    # Keyed by "name@created_at" (see dataset_identity() below), not just
    # name -- a re-import under an existing name gets a brand-new key.
    results_cache <- reactiveVal(list())
    
    .cacheable_fields <- c("qc_metrics", "qc_pass_idx", "moran_results",
                           "cluster_labels", "deconv_props", "cluster_markers",
                           "niche_labels", "niche_composition")
    
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
      tags$span(
        style = "display:flex; align-items:center; gap:6px;",
        tags$span(class = "small text-muted", "Actif :"),
        selectInput(ns("active_dataset_select"), NULL, choices = ds_names,
                    selected = global_data$active_spatial_dataset %||% ds_names[1],
                    width = "180px")
      )
    })
    
    # ── Manual switch (nav selectInput): only updates global_data. The
    # actual cache/reset logic lives in the identity observer below, so a
    # manual switch and a fresh (re-)import share the EXACT same path.
    observeEvent(input$active_dataset_select, {
      req(input$active_dataset_select %in% names(global_data$spatial_datasets))
      if (identical(input$active_dataset_select, global_data$active_spatial_dataset)) return()
      global_data$active_spatial_dataset <- input$active_dataset_select
      global_data$spatial_obj <- global_data$spatial_datasets[[input$active_dataset_select]]
    })
    
    # ── SINGLE source of truth for "the active dataset VERSION changed" ────
    dataset_identity <- reactive({
      nm <- global_data$active_spatial_dataset
      if (is.null(nm)) return(NULL)
      ts <- tryCatch(format(global_data$spatial_obj$created_at, "%Y%m%d%H%M%OS6"),
                     error = function(e) NA_character_)
      paste0(nm, "@", ts %||% "NA")
    })
    
    prev_identity <- reactiveVal(NULL)
    
    observeEvent(dataset_identity(), {
      new_id <- dataset_identity()
      old_id <- isolate(prev_identity())
      if (identical(new_id, old_id)) return()
      
      if (!is.null(old_id)) {
        cache <- results_cache()
        cache[[old_id]] <- .snapshot_shared_rv()
        results_cache(cache)
      }
      
      # ROI stays tied to mod_spatial_viz.R's own local reactive state --
      # always reset on any dataset-VERSION switch.
      shared_rv$roi_ids     <- NULL
      shared_rv$roi_bbox    <- NULL
      shared_rv$roi_markers <- NULL
      
      cached <- results_cache()[[new_id]]
      if (!is.null(cached)) {
        .restore_shared_rv(cached)
        if (!is.null(old_id)) {
          showNotification(sprintf("Echantillon actif : %s (resultats precedents restaures).",
                                   global_data$active_spatial_dataset),
                           type = "message", duration = 4)
        }
      } else {
        .clear_shared_rv()
        if (!is.null(old_id)) {
          showNotification(sprintf("Echantillon actif : %s", global_data$active_spatial_dataset),
                           type = "message", duration = 4)
        }
      }
      
      prev_identity(new_id)
    }, ignoreNULL = TRUE)
    
    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)
    mod_spatial_multi_server("multi", global_data, shared_rv)
    mod_spatial_niche_server("niche", global_data, shared_rv)
    
    observeEvent(input$spatial_nav, { shared_rv$active_tab <- input$spatial_nav })
  })
}