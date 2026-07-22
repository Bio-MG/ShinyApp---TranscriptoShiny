# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
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
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("spatial_status_ui")),

    div(style = "display:flex;justify-content:flex-end;margin-bottom:6px;",
        actionButton(ns("btn_reset_daemons"), "\U1F504 Reinitialiser les daemons",
                     class = "btn-outline-warning btn-sm"),
        uiOutput(ns("daemon_status_ui"), inline = TRUE)),

    navset_card_underline(
      id = ns("spatial_nav"),

      nav_panel("1. QC & Autocorrelation", icon = icon("filter"),
                mod_spatial_qc_ui(ns("qc"))),

      nav_panel("2. Clustering (BANKSY-lite)", icon = icon("shapes"),
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

    output$daemon_status_ui <- renderUI({
      input$btn_reset_daemons  # invalidate after reset
      ready <- tryCatch(spatial_daemons_ready(), error = function(e) FALSE)
      tags$span(class = "small text-muted", style = "margin-left:8px;align-self:center;",
                if (ready) "\u2705 daemons mirai actifs" else "\u26aa daemons inactifs")
    })

    observeEvent(input$btn_reset_daemons, {
      ok <- tryCatch(reset_spatial_daemons(6), error = function(e) FALSE)
      if (isTRUE(ok)) {
        showNotification("\U1F504 Daemons mirai reinitialises — relancez votre tache.", type = "message", duration = 5)
      } else {
        showNotification("Echec de la reinitialisation des daemons — voir la console R.", type = "error", duration = 8)
      }
    })

    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)

    observeEvent(input$spatial_nav, { shared_rv$active_tab <- input$spatial_nav })
  })
}
