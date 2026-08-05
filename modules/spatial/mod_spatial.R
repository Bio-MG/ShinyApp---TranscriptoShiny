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
    
    # ── v9: per-dataset result cache (session-scoped, in-memory only) ─────
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
    
    observeEvent(input$active_dataset_select, {
      req(input$active_dataset_select %in% names(global_data$spatial_datasets))
      if (identical(input$active_dataset_select, global_data$active_spatial_dataset)) return()
      
      # v9: snapshot the OUTGOING dataset's results before switching away,
      # so flipping back later restores them instead of forcing a full
      # recompute (QC/clustering/deconvolution/niches).
      old_name <- global_data$active_spatial_dataset
      if (!is.null(old_name)) {
        cache <- results_cache()
        cache[[old_name]] <- .snapshot_shared_rv()
        results_cache(cache)
      }
      
      global_data$active_spatial_dataset <- input$active_dataset_select
      global_data$spatial_obj <- global_data$spatial_datasets[[input$active_dataset_select]]
      
      # ROI state stays tied to mod_spatial_viz.R's own local reactive state
      # (umap_df, linked lasso selection) -- not safely restorable from
      # here, always reset on a dataset switch (unchanged from before).
      shared_rv$roi_ids     <- NULL
      shared_rv$roi_bbox    <- NULL
      shared_rv$roi_markers <- NULL
      
      cached <- results_cache()[[input$active_dataset_select]]
      if (!is.null(cached)) {
        .restore_shared_rv(cached)
        showNotification(sprintf("Echantillon actif : %s (resultats precedents restaures).",
                                 input$active_dataset_select),
                         type = "message", duration = 4)
      } else {
        .clear_shared_rv()
        showNotification(sprintf("Echantillon actif : %s", input$active_dataset_select),
                         type = "message", duration = 4)
      }
    })
    
    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)
    mod_spatial_multi_server("multi", global_data, shared_rv)
    mod_spatial_niche_server("niche", global_data, shared_rv)
    
    observeEvent(input$spatial_nav, { shared_rv$active_tab <- input$spatial_nav })
  })
}