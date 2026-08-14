# =============================================================================
# modules/spatial/mod_spatial_export.R — Export ("paquet complet" + script R)
# =============================================================================
# NEW (moyen terme a/b, voir handoff_spatial_bio-mg.md) : sur le modele de
# modules/bulk/mod_bulk_report.R (paquet .zip + script R reproductible),
# decline pour le module Spatial. Voir R/utils_spatial_export.R pour les
# fonctions pures sous-jacentes (build_spatial_export_bundle(),
# generate_spatial_pipeline_script()).
#
# 100% synchrone (pas de mirai) : n'exporte que des resultats DEJA calcules,
# assis dans shared_rv -- aucun nouveau calcul lourd ici, donc aucun besoin
# d'ExtendedTask ni de preload daemon pour ce module.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

mod_spatial_export_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Export", width = 360,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("archive"),
          " Regroupe les resultats DEJA CALCULES (QC, clustering, deconvolution, ",
          "niches, cartes) pour l'echantillon actif dans une archive .zip, ou ",
          "genere un script R autonome (+ contexte) pour reproduire le pipeline ",
          "hors Shiny."),

      checkboxGroupInput(ns("sections"), "Sections a inclure dans le paquet complet",
        choices = c("QC" = "qc", "Clustering" = "cluster", "Deconvolution" = "deconv",
                    "Niches" = "niche", "Moran's I (SVGs)" = "moran", "Cartes (PNG)" = "maps",
                    "Vues sauvegardees (onglet 4)" = "custom_viz"),
        selected = c("qc", "cluster", "deconv", "niche", "moran", "maps", "custom_viz")),
      downloadButton(ns("dl_bundle"), "\U0001F4E6 Paquet complet (.zip)",
                     class = "btn-success w-100 mb-3"),

      hr(),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          bsicons::bs_icon("code-slash"),
          " Le script reproduit UNIQUEMENT les etapes DEJA LANCEES dans l'app (QC/",
          "clustering/deconvolution RCTD/niches) avec les MEMES parametres. Necessite ",
          "que le dossier BPCells reste accessible au meme chemin (voir README/",
          "commentaires inclus dans le script)."),
      downloadButton(ns("dl_script"), "\U0001F9FE Script R reproductible (.zip)",
                     class = "btn-outline-secondary w-100")
    ),
    uiOutput(ns("export_preview_ui"))
  )
}

mod_spatial_export_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    output$export_preview_ui <- renderUI({
      obj <- global_data$spatial_obj
      if (is.null(obj)) {
        return(div(class = "alert alert-danger",
                    "Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'."))
      }
      row <- function(label, value) tags$tr(tags$td(label), tags$td(value))
      tagList(
        h6("Etat actuel de l'echantillon (ce qui sera exporte)", style = "font-weight:bold;"),
        tags$table(class = "table table-sm table-striped",
          row("Echantillon actif", obj$project %||% "-"),
          row("QC applique", if (!is.null(shared_rv$qc_pass_idx)) sprintf("%d elements retenus", length(shared_rv$qc_pass_idx)) else "Non"),
          row("Clustering", if (!is.null(shared_rv$cluster_labels)) sprintf("%d clusters", length(unique(shared_rv$cluster_labels))) else "Non calcule"),
          row("Deconvolution", if (!is.null(shared_rv$deconv_props)) sprintf("%d types cellulaires", ncol(shared_rv$deconv_props) - 1) else "Non calculee"),
          row("Niches", if (!is.null(shared_rv$niche_labels)) sprintf("%d niches", length(unique(shared_rv$niche_labels))) else "Non calculees"),
          row("Moran's I", if (!is.null(shared_rv$moran_results)) sprintf("%d genes testes", nrow(shared_rv$moran_results)) else "Non calcule")
        )
      )
    })

    .snapshot_results <- function() {
      list(
        qc_metrics = shared_rv$qc_metrics, qc_pass_idx = shared_rv$qc_pass_idx, qc_params = shared_rv$qc_params,
        cluster_labels = shared_rv$cluster_labels, cluster_params = shared_rv$cluster_params,
        cluster_markers = shared_rv$cluster_markers,
        deconv_props = shared_rv$deconv_props, deconv_params = shared_rv$deconv_params,
        moran_results = shared_rv$moran_results, moran_params = shared_rv$moran_params,
        niche_labels = shared_rv$niche_labels, niche_composition = shared_rv$niche_composition,
        niche_params = shared_rv$niche_params, saved_viz_list = shared_rv$saved_viz_list
      )
    }

    output$dl_bundle <- downloadHandler(
      filename = function() {
        nm <- global_data$spatial_obj$project %||% "spatial"
        paste0("export_spatial_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(global_data$spatial_obj)
        tmp_dir <- tempfile("spatial_export_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        withProgress(message = "Preparation du paquet complet...", value = 0.2, {
          written <- tryCatch(
            build_spatial_export_bundle(
              spatial_obj = global_data$spatial_obj,
              results     = .snapshot_results(),
              sections    = input$sections %||% character(0),
              out_dir     = tmp_dir
            ),
            error = function(e) {
              showNotification(paste("Erreur export :", conditionMessage(e)), type = "error", duration = 10)
              character(0)
            }
          )
          incProgress(0.6, detail = "Compression (.zip)...")
          if (length(written) == 0) {
            fallback <- file.path(tmp_dir, "README.txt")
            writeLines("Aucune section disponible/selectionnee -- rien a exporter.", fallback)
            written <- fallback
          }
          zip::zip(file, files = written, mode = "cherry-pick")
        })
      }
    )

    output$dl_script <- downloadHandler(
      filename = function() {
        nm <- global_data$spatial_obj$project %||% "spatial"
        paste0("script_spatial_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(global_data$spatial_obj$coords)
        tmp_dir <- tempfile("spatial_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        script_text <- generate_spatial_pipeline_script(
          spatial_obj = global_data$spatial_obj,
          params = list(qc = shared_rv$qc_params, cluster = shared_rv$cluster_params,
                        deconv = shared_rv$deconv_params, niche = shared_rv$niche_params)
        )
        script_path <- file.path(tmp_dir, sprintf("pipeline_spatial_%s.R", format(Sys.time(), "%Y%m%d_%H%M%S")))
        writeLines(script_text, script_path)

        context_path <- file.path(tmp_dir, "context.rds")
        saveRDS(list(coords = global_data$spatial_obj$coords,
                     project = global_data$spatial_obj$project,
                     technology = global_data$spatial_obj$technology), context_path)

        zip::zip(file, files = c(script_path, context_path), mode = "cherry-pick")
      }
    )
  })
}
