# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
# BREAKING CHANGE vs the pre-refactor version of this file: global_data$spatial_obj
# is no longer a raw Seurat object. It is the list produced by
# R/utils_spatial_io.R::convert_to_bpcells_and_fov() —
#   $sketch (Seurat, <=50k, in-RAM), $bpcells_dir (disk path, full res),
#   $coords, $technology, $n_total, $images, $project.
# Every read of global_data$spatial_obj below (and in every child module)
# must go through $sketch / $bpcells_dir explicitly. Old code that did
# `ncol(global_data$spatial_obj)` or `Images(global_data$spatial_obj)`
# directly on the object no longer works — this file (and app.R's
# global_status_panel) is exactly that: already migrated.
#
# This module owns `shared_rv`, the reactive bus between the four child
# modules (QC -> Clustering -> Deconvolution -> Visualization), and starts
# the mirai daemon pool (idempotent; also called once from app.R at startup —
# harmless either way, see R/utils_spatial_async.R).
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("spatial_status_ui")),

    navset_card_underline(
      id = ns("spatial_nav"),

      nav_panel("1. QC & Autocorrelation", icon = icon("filter"),
                mod_spatial_qc_ui(ns("qc"))),

      nav_panel("2. Clustering (BANKSY)", icon = icon("shapes"),
                mod_spatial_cluster_ui(ns("cluster"))),

      nav_panel("3. Deconvolution", icon = icon("puzzle-piece"),
                mod_spatial_deconv_ui(ns("deconv"))),

      nav_panel("4. Visualisation", icon = icon("eye"),
                mod_spatial_viz_ui(ns("viz")))
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    # Defensive: no-op if already initialized (see app.R for the primary call).
    if (!spatial_daemons_ready()) init_spatial_daemons(n_daemons = 6)

    # ── Shared reactive bus for all child modules ─────────────────────────
    shared_rv <- reactiveValues(
      active_tab       = "1. QC & Autocorrelation",
      qc_metrics       = NULL,   # data.frame(id, nCount, nFeature, pct_mt, pct_ribo) — full res
      qc_pass_idx      = NULL,   # integer indices (into bpcells_dir columns) passing thresholds
      moran_results    = NULL,   # data.frame(gene, moran_i, p_value) — top HVGs only
      cluster_labels   = NULL,   # named character vector: spot/cell id -> cluster id
      deconv_props     = NULL,   # data.frame: id + one column per cell type (proportions)
      current_fov_crop = NULL    # list(fov=, x=c(min,max), y=c(min,max)) for Crop()-based zoom
    )

    output$spatial_status_ui <- renderUI({
      if (is.null(global_data$spatial_obj)) {
        div(class = "alert alert-danger",
            bsicons::bs_icon("exclamation-triangle"),
            " Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'.")
      } else {
        obj <- global_data$spatial_obj
        disk_ok <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)
        div(class = if (disk_ok) "alert alert-success" else "alert alert-warning",
            bsicons::bs_icon(if (disk_ok) "check-circle" else "exclamation-triangle"),
            sprintf(" %s (%s) : %s elements au total, %s en memoire (sketch).%s",
                    obj$project %||% "Objet spatial", obj$technology,
                    format(obj$n_total, big.mark = ","),
                    format(ncol(obj$sketch), big.mark = ","),
                    if (!disk_ok) " Donnees sur disque introuvables — reimportez pour relancer les calculs lourds (clustering/deconvolution)." else ""))
      }
    })

    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)

    observeEvent(input$spatial_nav, { shared_rv$active_tab <- input$spatial_nav })
  })
}
