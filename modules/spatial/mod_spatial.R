# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
# v6 (Phase 4 — "Working with multiple slices", vignette parity): added a
# 5th top-level tab, "Multi-echantillons" (mod_spatial_multi.R), plus an
# active-dataset switcher in the header row. global_data gained
# $spatial_datasets (named list, one entry per imported dataset, see
# app.R/mod_import_spatial.R) and $active_spatial_dataset. $spatial_obj
# itself is UNCHANGED in shape — it always points at whichever ONE dataset
# is currently "active" — so tabs 1-4 (QC/Clustering/Deconvolution/
# Visualisation) needed ZERO code changes; switching the active dataset
# here is indistinguishable, from their point of view, from having
# reimported that dataset.
#
# v6.1 (audit step 3.9b — multi-sample bugfix): `ns <- session$ns` was
# NEVER assigned in this module's moduleServer() body. output$active_dataset_ui
# below calls ns("active_dataset_select") -- this only executes once
# length(spatial_datasets) >= 2, i.e. EXACTLY the multi-sample scenario --
# so it silently worked with 0/1 dataset and threw "object 'ns' not found"
# the moment a 2nd sample was imported. Root-caused during the same audit
# pass that fixed the sprintf() bug in mod_spatial_multi.R's
# dataset_picker_ui (malformed format string, same symptom class: only
# reproducible with >=2 datasets loaded).
#
# v5 (Phase 3 — ROI/crop): shared_rv gained roi_ids / roi_bbox / roi_markers
# for the new "ROI isolee" panel in mod_spatial_viz.R (lasso/rectangle
# selection -> isolated region + comparative view + regional markers). No
# other change here — the new fields just need to exist on the shared bus;
# mod_spatial_viz.R owns all the logic that reads/writes them.
#
# v4 (UX feedback): the daemon status/reset row AND the dataset banner used
# to sit in their own div above the tabs, eating two extra rows of vertical
# space. Both are now folded INTO the tab strip itself:
#   - daemon status + reset button -> nav_item()s inside the SAME
#     navset_card_underline(), pushed flush right via nav_spacer() (native
#     bslib idiom, same trick page_navbar() uses for right-aligned items).
#   - dataset banner (project/technology/n_total/sketch size) -> REMOVED
#     from here entirely, now rendered inside the QC tab itself
#     (mod_spatial_qc.R's new "Apercu du jeu de donnees" section) since QC
#     is where a biologist actually wants that context, not permanently
#     pinned above every tab.
#
# global_data$spatial_obj is the list produced by
# R/utils_spatial_io.R::convert_to_bpcells_and_fov() —
#   $sketch (Seurat, <=50k, in-RAM), $bpcells_dir (disk path, full res),
#   $coords, $technology, $n_total, $images, $project.
# Every read of global_data$spatial_obj below (and in every child module)
# goes through $sketch / $bpcells_dir explicitly.
#
# v2 (post-test-3): added a "Reinitialiser les daemons" button. Several
# heavy packages used by the async steps were found to spawn nested
# parallel worker processes from inside a mirai daemon (see
# mod_spatial_cluster.R / mod_spatial_deconv.R headers) — when one of those
# hangs or errors ungracefully, the daemon that handled it can be left in a
# bad state for every future task routed to it (daemons are long-lived).
# This button tears the pool down and respawns it fresh, without needing to
# restart R.
#
# v7 (Phase 5 — niches spatiales): added a 6th top-level tab,
# "Niches spatiales" (mod_spatial_niche.R, seurat5_spatial_vignette_2.Rmd
# parity — BuildNicheAssay()). shared_rv gained niche_labels/niche_composition,
# reset on active-dataset switch alongside every other per-dataset field
# below. mod_spatial_viz.R (tab 4) reads shared_rv$niche_labels as one more
# color_by option ("niche") — see that module's own small addition.
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

      # v9 (Phase 5 — fix "onglet 6 gache une ligne entiere"): a separate
      # full-width header row above the tab strip (v8) reliably fixed the
      # 6th-tab-invisible bug but always ate one blank row of vertical
      # space, even when it had almost nothing to show (1 dataset -> no
      # switcher, just badge+button). Folded back into the tab strip, but
      # as a SINGLE compact nav_menu() dropdown ("Session") instead of 3
      # separate nav_item()s (v4) — one small trigger, not three items, so
      # the strip stays narrow enough for all 6 tabs even on a cramped
      # viewport (e.g. RStudio's Viewer pane) while costing zero extra rows.
      nav_spacer(),
      bslib::nav_menu(
        title = tagList(icon("gear"), "Session"), align = "right",
        nav_item(uiOutput(ns("active_dataset_ui"))),
        nav_item(uiOutput(ns("daemon_status_ui"))),
        nav_item(actionButton(ns("btn_reset_daemons"), "Reinitialiser les daemons",
                              class = "btn-outline-warning btn-sm w-100 mt-1", icon = icon("rotate")))
      )
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    # FIX (audit step 3.9b): this was missing entirely. output$active_dataset_ui
    # below needs ns() to build the id of its dynamically-inserted
    # selectInput() -- without this line R throws "object 'ns' not found"
    # as soon as that output actually renders (i.e. as soon as a 2nd
    # spatial dataset gets imported).
    ns <- session$ns
    
    # Defensive: no-op if already initialized (see app.R for the primary call).
    if (!spatial_daemons_ready()) init_spatial_daemons(n_daemons = 6)
    
    # ── Shared reactive bus for all child modules ─────────────────────────
    shared_rv <- reactiveValues(
      active_tab       = "1. QC & Autocorrelation",
      qc_metrics       = NULL,   # data.frame(id, nCount, nFeature, pct_mt, pct_ribo) — full res
      qc_pass_idx      = NULL,   # integer indices (into bpcells_dir columns) passing thresholds
      moran_results    = NULL,   # data.frame(gene, moran_i, p_value) — top HVGs only
      cluster_labels   = NULL,   # named character vector: spot/cell id -> cluster id
      deconv_props     = NULL,   # data.frame: id + one column per cell type (proportions or
      # label-transfer prediction scores — same contract either way,
      # see mod_spatial_deconv.R "labeltransfer" mode)
      cluster_markers  = NULL,   # data.frame: cluster, gene, avg_log2FC, p_val_adj, ... (regional DE)
      current_fov_crop = NULL,   # list(fov=, x=c(min,max), y=c(min,max)) for Crop()-based zoom
      # ── Phase 3 — ROI ("Subset out anatomical regions", vignette parity) ──
      roi_ids          = NULL,   # character vector: ids isolated via lasso/rectangle (mod_spatial_viz.R)
      roi_bbox          = NULL,  # list(x=c(min,max), y=c(min,max)) of the isolated ROI
      roi_markers       = NULL,  # data.frame: gene, avg_log2FC, pct.1, pct.2, p_val, p_val_adj (ROI vs reste)
      # ── Phase 5 — Niches spatiales (BuildNicheAssay-lite, mod_spatial_niche.R) ──
      niche_labels      = NULL,  # named character vector: id -> niche id ("N1".."N<k>"), same
      # shape as cluster_labels so mod_spatial_viz.R can treat it as just
      # another color_by option.
      niche_composition = NULL   # data.frame(niche, <one column per cluster/cell type>) — mean
      # neighborhood composition per niche (interpretation aid).
    )
    
    output$daemon_status_ui <- renderUI({
      input$btn_reset_daemons  # invalidate after reset
      ready <- tryCatch(spatial_daemons_ready(), error = function(e) FALSE)
      tags$span(class = "small text-muted", style = "align-self:center;",
                if (ready) "\u2705 daemons actifs" else "\u26aa daemons inactifs")
    })
    
    observeEvent(input$btn_reset_daemons, {
      ok <- tryCatch(reset_spatial_daemons(6), error = function(e) FALSE)
      if (isTRUE(ok)) {
        showNotification("\U1F504 Daemons mirai reinitialises — relancez votre tache.", type = "message", duration = 5)
      } else {
        showNotification("Echec de la reinitialisation des daemons — voir la console R.", type = "error", duration = 8)
      }
    })
    
    # ── Active-dataset switcher (Phase 4 — multi-echantillons) ────────────
    # Tabs 1-4 (QC/Clustering/Deconvolution/Visualisation) only ever read
    # global_data$spatial_obj — they have NO idea multiple datasets can be
    # imported. This dropdown is the ONE place that repoints $spatial_obj at
    # a different entry of global_data$spatial_datasets, so switching here
    # is exactly equivalent (from tabs 1-4's point of view) to having
    # reimported that dataset. See mod_import_spatial.R for how
    # $spatial_datasets gets populated on each import.
    output$active_dataset_ui <- renderUI({
      ds_names <- names(global_data$spatial_datasets)
      if (length(ds_names) < 2) return(NULL)  # nothing to switch between yet
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
      global_data$active_spatial_dataset <- input$active_dataset_select
      global_data$spatial_obj <- global_data$spatial_datasets[[input$active_dataset_select]]
      # Reset per-dataset derived state (QC thresholds, clusters, etc. from
      # the PREVIOUS active dataset do not apply to this one) — same clean
      # slate as a fresh import, consistent with every module downstream
      # expecting shared_rv to describe the CURRENTLY active dataset only.
      shared_rv$qc_metrics      <- NULL
      shared_rv$qc_pass_idx     <- NULL
      shared_rv$moran_results   <- NULL
      shared_rv$cluster_labels  <- NULL
      shared_rv$deconv_props    <- NULL
      shared_rv$cluster_markers <- NULL
      shared_rv$roi_ids         <- NULL
      shared_rv$roi_bbox        <- NULL
      shared_rv$roi_markers     <- NULL
      shared_rv$niche_labels      <- NULL
      shared_rv$niche_composition <- NULL
      showNotification(sprintf("Echantillon actif : %s", input$active_dataset_select),
                       type = "message", duration = 4)
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
