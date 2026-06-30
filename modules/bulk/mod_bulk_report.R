# =============================================================================
# mod_bulk_report.R  —  Bulk Child 4: Rapport HTML/PDF Complet
# =============================================================================
# Pure sidebar module — renders modules/bulk_report_template.Rmd into a
# standalone HTML and/or PDF report (PCA, QC, Volcano, Heatmap, Table,
# Pathways), shareable with a collaborator without an R install.
#
# Several parameters needed by the Rmd live in OTHER children's input$
# namespaces (pca_color_by lives in mod_bulk_filter, lfc_thresh/padj_thresh/
# heatmap_top_n live in mod_bulk_de, pathway_db lives in mod_bulk_pathways).
# Those modules mirror the values into shared_rv (see each file's header
# contract) so this module can read them here without crossing module
# namespaces directly.
#
# Depends on:
#   modules/bulk_report_template.Rmd (parametrised R Markdown, unchanged)
#
# State contract (shared_rv):
#   READ ONLY : shared_rv$vst_mat, shared_rv$contrasts, shared_rv$active_contrast,
#               shared_rv$pathway_results, shared_rv$pca_color_by,
#               shared_rv$lfc_thresh, shared_rv$padj_thresh,
#               shared_rv$heatmap_top_n, shared_rv$pathway_db
#
# UI split:
#   mod_bulk_report_ui(id) -> sidebar accordion body (Step 4 controls)
#   (no output_ui — this child owns no main-panel tab)
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
        "Génère un rapport autonome (PCA, QC, Volcano, Heatmap, Pathways) — ",
        "partageable avec un collaborateur sans installation R."),

    textInput(ns("report_title"), "Titre du rapport",
              value = "Analyse RNA-seq Bulk", placeholder = "Ex: Projet Cornée — Cov2 vs Mock"),
    textInput(ns("report_subtitle"), "Sous-titre (optionnel)", placeholder = "Ex: GSE164073"),
    textAreaInput(ns("report_notes"), "Notes / Commentaires (markdown supporté)",
                  rows = 3, placeholder = "Ex: Échantillons CoV2 issus du donneur D3, voir cahier de labo p.12."),

    checkboxGroupInput(ns("report_sections"), "Sections à inclure",
                       choices = c("PCA" = "pca", "QC Échantillons" = "qc",
                                   "Volcano + MA-Plot" = "volcano",
                                   "Heatmap Top Gènes" = "heatmap",
                                   "Table DE complète" = "table",
                                   "Pathway Enrichment" = "pathway"),
                       selected = c("pca", "qc", "volcano", "heatmap", "table", "pathway")),

    radioButtons(ns("report_format"), "Format de sortie",
                choices = c("HTML interactif" = "html",
                            "PDF statique"     = "pdf",
                            "Les deux (.zip)"  = "both"),
                selected = "html"),

    conditionalPanel(
      condition = "input.report_format != 'pdf'", ns = ns,
      checkboxInput(ns("report_interactive"), "Graphiques interactifs (PCA, Volcano) — HTML uniquement",
                   value = TRUE)
    ),
    div(class = "small text-muted",
        "Le PDF requiert LaTeX (package 'tinytex') et repasse automatiquement en graphiques statiques — ",
        "les widgets interactifs ne peuvent pas s'imprimer."),

    downloadButton(ns("dl_report"), "📄 Générer le Rapport",
                   class = "btn-dark w-100 mt-2"),

    div(class = "small text-muted mt-1", textOutput(ns("report_status")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_report_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # ── Polish UI: disable the report download until Step 1 has run ────────
    observe({
      shinyjs::toggleState("dl_report", condition = !is.null(shared_rv$vst_mat))
    })

    output$report_status <- renderText({
      if (is.null(shared_rv$vst_mat)) "Lancez d'abord l'étape 1 (Filtrage & VST) pour activer le rapport."
      else "Prêt — sélectionnez les sections puis cliquez sur 'Générer Rapport HTML'."
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html = "html", pdf = "pdf", both = "zip")
        paste0("rapport_bulk_", Sys.Date(), ".", ext)
      },
      content  = function(file) {
        req(shared_rv$vst_mat)

        # ── Active DE result: plain list lookup (no reactive — we're inside
        #    a downloadHandler, req() on a missing key would just abort here,
        #    which is the desired behaviour if Step 2 was never run) ───────
        de_results <- NULL
        ac <- shared_rv$active_contrast
        if (!is.null(ac) && ac %in% names(shared_rv$contrasts)) {
          de_results <- shared_rv$contrasts[[ac]]
        }

        padj_thresh_now <- shared_rv$padj_thresh %||% 0.05
        lfc_thresh_now  <- shared_rv$lfc_thresh  %||% 1

        # ── FIX (Bug #2) : the report used to only ever see the single
        #    "active" contrast — when pairwise-auto produces several, the
        #    rest were silently invisible in the exported report. Build a
        #    n_sig/n_up/n_down summary for EVERY contrast in
        #    shared_rv$contrasts, filtered with the CURRENT thresholds (not
        #    whatever they were at the moment each contrast was computed —
        #    stays consistent if the user changes the threshold afterwards).
        all_contrasts <- shared_rv$contrasts %||% list()
        all_contrasts_summary <- if (length(all_contrasts) == 0) {
          NULL
        } else {
          summarize_contrasts_updown(all_contrasts, lfc_thresh = lfc_thresh_now,
                                     padj_thresh = padj_thresh_now, active_contrast = ac)
        }

        template_path <- file.path("modules", "bulk", "bulk_report_template.Rmd")  # était file.path("modules", ...)
        tmp_rmd <- file.path(tempdir(), "bulk_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite = TRUE)

        render_params <- list(
          vst_mat               = shared_rv$vst_mat,
          metadata              = global_data$bulk_obj$metadata,
          de_results            = de_results,
          contrast_name         = shared_rv$active_contrast,
          all_contrasts_summary = all_contrasts_summary,
          all_contrasts         = all_contrasts,
          pathway_results       = shared_rv$pathway_results,
          pathway_db            = shared_rv$pathway_db %||% "GOBP",
          sections              = input$report_sections %||% character(0),
          pca_color_by          = if (nzchar(shared_rv$pca_color_by %||% "")) shared_rv$pca_color_by else NULL,
          heatmap_top_n         = shared_rv$heatmap_top_n %||% 30,
          lfc_thresh            = lfc_thresh_now,
          padj_thresh           = padj_thresh_now,
          report_title          = input$report_title %||% "Analyse RNA-seq Bulk",
          report_subtitle       = input$report_subtitle %||% "",
          report_notes          = input$report_notes %||% "",
          interactive           = isTRUE(input$report_interactive) && input$report_format != "pdf"
        )

        withProgress(message = "Génération du rapport...", value = 0.2, {

          formats_needed <- switch(input$report_format,
            html = "html_document", pdf = "pdf_document", both = c("html_document", "pdf_document"))

          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail = paste("Rendu", fmt))
            out_path <- file.path(tempdir(), paste0("bulk_report_", fmt, "_", as.integer(Sys.time())))
            res <- tryCatch({
              rmarkdown::render(
                input = tmp_rmd, output_format = fmt, output_file = out_path,
                params = render_params, envir = new.env(parent = globalenv()), quiet = TRUE
              )
            }, error = function(e) {
              showNotification(
                paste0("❌ Erreur génération ", fmt, ": ", conditionMessage(e),
                       if (fmt == "pdf_document") " (vérifiez que 'tinytex' est installé : tinytex::install_tinytex())" else ""),
                type = "error", duration = 12
              )
              NULL
            })
            if (!is.null(res)) out_files <- c(out_files, res)
          }

          if (length(out_files) == 0) {
            stop("Aucun format n'a pu être généré — voir la notification d'erreur.")
          } else if (length(out_files) == 1) {
            file.copy(out_files[1], file, overwrite = TRUE)
          } else {
            zip::zip(file, files = out_files, mode = "cherry-pick")
          }
        })
      }
    )

  }) # /moduleServer
}
