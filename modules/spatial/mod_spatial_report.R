# =============================================================================
# modules/spatial/mod_spatial_report.R — Rapport HTML/PDF (multi-echantillons)
# =============================================================================
# NEW (feedback biologiste — "ajoute de quoi generer un rapport html/pdf...
# DOIT GERER le multi echantillonnage... sc_report_template prend exemple").
# Meme architecture de rendu que modules/bulk/mod_bulk_report.R (repris a
# l'identique : output_format en STRING pour laisser le YAML du .Rmd choisir
# les options html_document/pdf_document, tempdir() par rendu, zip si
# plusieurs formats) ; meme conventions de structure Rmd (params, sections,
# render_plot() pour l'enrobage plotly, tables DT/kable) que
# modules/sc/sc_report_template.Rmd (fourni comme reference explicite).
#
# Multi-echantillons : shared_rv ne contient QUE les resultats de
# l'echantillon ACTIF (voir mod_spatial.R) ; les autres echantillons deja
# analyses dans la session ont leur dernier etat dans
# global_data$spatial_results_cache[[nom]] (ecrit par mod_spatial.R au
# changement d'echantillon). .gather_datasets() ci-dessous fusionne les
# deux sources pour batir, pour CHAQUE echantillon importe, le meme snapshot
# leger (build_spatial_report_dataset(), R/utils_spatial_report.R) que le
# Rmd consomme -- qu'il soit actif ou non au moment du clic.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

mod_spatial_report_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Rapport", width = 380,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("file-earmark-text"),
          " Genere un rapport HTML et/ou PDF a partir de TOUS les resultats DEJA CALCULES ",
          "(QC, clustering, deconvolution, Moran's I, niches, UMAP) -- une section n'apparait ",
          "que si le calcul correspondant a deja ete lance au moins une fois, MEME si vous ne ",
          "consultez pas cet onglet actuellement."),

      uiOutput(ns("scope_status_ui")),
      radioButtons(ns("report_scope"), "Portee",
                   choices = c("Echantillon actif uniquement" = "active",
                               "Tous les echantillons analyses (multi-echantillons)" = "all"),
                   selected = "active"),

      checkboxGroupInput(ns("sections"), "Sections a inclure",
        choices = c("Apercu" = "overview", "QC" = "qc", "Clustering" = "cluster",
                    "Deconvolution" = "deconv", "Moran's I / SVG" = "moran",
                    "Niches" = "niche", "UMAP (sketch)" = "umap",
                    "Vues sauvegardees (onglet 4)" = "custom_viz",
                    "Multi-echantillons (integration conjointe)" = "multi"),
        selected = c("overview", "qc", "cluster", "deconv", "moran", "niche", "umap", "custom_viz", "multi")),

      checkboxInput(ns("interactive_plots"), "Graphiques interactifs (HTML uniquement, plus lourd)", value = TRUE),

      textInput(ns("report_title"), "Titre", value = "Analyse Spatiale"),
      textInput(ns("report_subtitle"), "Sous-titre (optionnel)", value = ""),
      textAreaInput(ns("report_notes"), "Notes (optionnel, markdown supporte)", rows = 3, value = ""),

      hr(),
      checkboxGroupInput(ns("formats"), "Format(s) de sortie",
                         choices = c("HTML" = "html", "PDF" = "pdf"), selected = "html"),
      div(class = "text-muted", style = "font-size:0.7rem;",
          "PDF necessite une distribution LaTeX (ex: tinytex::install_tinytex()) sur la machine ",
          "qui heberge l'app -- en son absence le rendu PDF echoue proprement avec un message ",
          "clair (notification), sans bloquer un eventuel export HTML demande en parallele."),

      downloadButton(ns("dl_report"), "\U0001F4C4 Generer le rapport", class = "btn-success w-100 mt-2"),
      div(class = "small text-muted mt-1", textOutput(ns("report_status")))
    ),
    uiOutput(ns("report_preview_ui"))
  )
}

mod_spatial_report_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggleState("dl_report", condition = !is.null(global_data$spatial_obj))
    })

    output$report_status <- renderText({
      if (is.null(global_data$spatial_obj)) "Importez d'abord un echantillon spatial."
      else "Pret -- selectionnez les sections puis cliquez sur 'Generer le rapport'."
    })

    .snapshot_results <- function() {
      list(
        qc_metrics = shared_rv$qc_metrics, qc_pass_idx = shared_rv$qc_pass_idx, qc_params = shared_rv$qc_params,
        cluster_labels = shared_rv$cluster_labels, cluster_params = shared_rv$cluster_params,
        cluster_markers = shared_rv$cluster_markers,
        deconv_props = shared_rv$deconv_props, deconv_params = shared_rv$deconv_params,
        moran_results = shared_rv$moran_results, moran_params = shared_rv$moran_params,
        niche_labels = shared_rv$niche_labels, niche_composition = shared_rv$niche_composition,
        niche_params = shared_rv$niche_params, umap_df = shared_rv$umap_df,
        saved_viz_list = shared_rv$saved_viz_list
      )
    }

    # ── Fusionne l'etat LIVE (shared_rv, echantillon actif) avec le cache
    # par-echantillon (global_data$spatial_results_cache, voir mod_spatial.R)
    # pour donner au rapport une vue de TOUT ce qui a ete calcule dans la
    # session, pas seulement l'onglet actuellement affiche. ────────────────
    .gather_datasets <- function() {
      req(global_data$spatial_obj)
      scope <- input$report_scope %||% "active"
      active_name <- global_data$active_spatial_dataset

      if (identical(scope, "active") || length(global_data$spatial_datasets) < 2) {
        nm <- active_name %||% (global_data$spatial_obj$project %||% "Echantillon")
        return(stats::setNames(
          list(build_spatial_report_dataset(global_data$spatial_obj, .snapshot_results())), nm
        ))
      }

      ds_names <- names(global_data$spatial_datasets)
      cache <- global_data$spatial_results_cache %||% list()
      stats::setNames(lapply(ds_names, function(nm) {
        res <- if (identical(nm, active_name)) .snapshot_results() else (cache[[nm]] %||% list())
        build_spatial_report_dataset(global_data$spatial_datasets[[nm]], res)
      }), ds_names)
    }

    output$scope_status_ui <- renderUI({
      n_ds <- length(global_data$spatial_datasets)
      if (n_ds < 2) {
        return(div(class = "text-muted", style = "font-size:0.72rem; margin-bottom:6px;",
                   "Un seul echantillon charge -- la portee \"multi-echantillons\" sera ignoree."))
      }
      div(class = "text-muted", style = "font-size:0.72rem; margin-bottom:6px;",
          sprintf("%d echantillons disponibles : %s.", n_ds,
                  paste(names(global_data$spatial_datasets), collapse = ", ")))
    })

    output$report_preview_ui <- renderUI({
      ds <- tryCatch(.gather_datasets(), error = function(e) NULL)
      if (is.null(ds) || length(ds) == 0) {
        return(div(class = "alert alert-danger",
                    "Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'."))
      }
      ok <- function(x) if (!is.null(x) && length(x) > 0) "\u2705" else "\u2014"
      rows <- lapply(names(ds), function(nm) {
        r <- ds[[nm]]$results
        tags$tr(
          tags$td(strong(nm)), tags$td(ok(r$qc_pass_idx)), tags$td(ok(r$cluster_labels)),
          tags$td(ok(r$deconv_props)), tags$td(ok(r$moran_results)), tags$td(ok(r$niche_labels)),
          tags$td(ok(r$umap_df)), tags$td(ok(r$saved_viz_list))
        )
      })
      tagList(
        h6("Apercu (ce qui sera inclus dans le rapport)", style = "font-weight:bold;"),
        tags$table(class = "table table-sm table-striped",
          tags$thead(tags$tr(tags$th("Echantillon"), tags$th("QC"), tags$th("Cluster"),
                             tags$th("Deconv"), tags$th("Moran"), tags$th("Niches"), tags$th("UMAP"),
                             tags$th("Vues sauv."))),
          tags$tbody(rows)
        ),
        if (!is.null(global_data$spatial_multi_integration) && "multi" %in% (input$sections %||% character(0))) {
          div(class = "alert alert-info small",
              sprintf("Integration multi-echantillons disponible (%s, %s) -- incluse.",
                      global_data$spatial_multi_integration$reduction_used %||% "?",
                      paste(global_data$spatial_multi_integration$datasets %||% character(0), collapse = ", ")))
        } else if ("multi" %in% (input$sections %||% character(0)) && length(ds) > 1) {
          div(class = "alert alert-light small",
              "Aucune integration conjointe calculee (onglet \"5. Multi-echantillons\") -- section omise.")
        }
      )
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        fmt <- input$formats %||% "html"
        ext <- if (length(fmt) > 1) "zip" else fmt[1]
        paste0("rapport_spatial_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(length(input$formats %||% character(0)) > 0)

        ds_list <- tryCatch(.gather_datasets(), error = function(e) NULL)
        validate(need(!is.null(ds_list) && length(ds_list) > 0, "Aucun echantillon a inclure dans le rapport."))

        template_path <- tryCatch(find_spatial_report_template(), error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 15)
          NULL
        })
        req(template_path)

        tmp_dir <- tempfile("spatial_report_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        tmp_rmd <- file.path(tmp_dir, "spatial_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite = TRUE)
        child_src <- file.path(dirname(template_path), "spatial_report_dataset_child.Rmd")
        if (file.exists(child_src)) {
          file.copy(child_src, file.path(tmp_dir, "spatial_report_dataset_child.Rmd"), overwrite = TRUE)
        }

        render_params <- list(
          datasets           = ds_list,
          active_dataset     = global_data$active_spatial_dataset,
          multi_integration  = if ("multi" %in% (input$sections %||% character(0))) global_data$spatial_multi_integration else NULL,
          sections           = input$sections %||% character(0),
          report_title       = input$report_title %||% "Analyse Spatiale",
          report_subtitle    = input$report_subtitle %||% "",
          report_notes       = input$report_notes %||% "",
          interactive        = isTRUE(input$interactive_plots),
          fig_width          = 9,
          fig_height         = 7
        )

        withProgress(message = "Generation du rapport...", value = 0.15, {
          formats_needed <- if ("pdf" %in% (input$formats %||% "html")) {
            if ("html" %in% input$formats) c("html_document", "pdf_document") else "pdf_document"
          } else "html_document"

          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail = sprintf("Rendu %s...", fmt))
            ext_i <- if (fmt == "pdf_document") "pdf" else "html"
            out_path <- file.path(tmp_dir, paste0("rapport_spatial_", ext_i, ".", ext_i))
            res <- tryCatch(
              rmarkdown::render(input = tmp_rmd, output_format = fmt, output_file = out_path,
                                params = render_params, envir = new.env(parent = globalenv()), quiet = TRUE),
              error = function(e) {
                showNotification(paste0("\u274c ", fmt, " : ", conditionMessage(e)), type = "error", duration = 15)
                NULL
              }
            )
            if (!is.null(res) && file.exists(res)) out_files <- c(out_files, res)
          }

          validate(need(length(out_files) > 0, "Aucun format n'a pu etre genere -- voir les notifications d'erreur."))

          if (length(out_files) == 1) {
            file.copy(out_files[1], file, overwrite = TRUE)
          } else {
            zip::zip(file, files = out_files, mode = "cherry-pick")
          }
        })
      }
    )
  })
}
