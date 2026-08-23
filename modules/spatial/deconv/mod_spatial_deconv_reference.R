# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv_reference.R — Reference state & prep
# =============================================================================
# Owns: ref_state reactiveValues, file upload, reference parsing, artifact
# preparation, shared-reference radio, status badges, cell-type table.
#
# Plain function (NOT a nested Shiny module) called from the orchestrator's
# moduleServer() — same pattern as bulk_de/.de_*_server().
#
# State contract:
#   WRITES (module-local): ref_state, ref_path, ref_artifact_info, ref_meta_cols
#   EXPOSES via return: list(reference_is_ready, effective_ref_path, ref_state)
# =============================================================================

.deconv_reference_server <- function(input, output, session, ns, global_data, shared_rv) {

  # ── Reactive state ──────────────────────────────────────────────────────
  ref_state <- reactiveValues(
    obj = NULL, staged_path = NULL, orig_ext = NULL,
    status = "empty", message = NULL
  )
  ref_path          <- reactiveVal(NULL)
  ref_artifact_info <- reactiveVal(NULL)
  ref_meta_cols     <- reactiveVal(character(0))

  # ── Helpers ─────────────────────────────────────────────────────────────
  .version_hint <- function(msg) {
    if (!grepl("GetAssayData|slot.*defunct|defunct.*slot", msg, ignore.case = TRUE)) return("")
    sprintf(" [Seurat %s | SeuratObject %s | schard %s | SeuratDisk %s]",
            tryCatch(as.character(utils::packageVersion("Seurat")), error = function(e) "?"),
            tryCatch(as.character(utils::packageVersion("SeuratObject")), error = function(e) "?"),
            tryCatch(as.character(utils::packageVersion("schard")), error = function(e) "absent"),
            tryCatch(as.character(utils::packageVersion("SeuratDisk")), error = function(e) "absent"))
  }

  run_reference_pipeline <- function(staged_path, orig_ext) {
    ref_state$obj         <- NULL
    ref_state$staged_path <- NULL
    ref_state$status      <- "loading"
    ref_state$message     <- NULL
    ref_meta_cols(character(0))
    ref_path(NULL)
    ref_artifact_info(NULL)

    withProgress(message = "Preparation de la reference...", value = 0, {
      incProgress(0.15, detail = sprintf("Lecture (.%s)...", if (nzchar(orig_ext)) orig_ext else "?"))
      raw_ref <- tryCatch(read_reference_scrna(staged_path), error = function(e) {
        msg <- conditionMessage(e)
        ref_state$status  <- "error"
        ref_state$message <- sprintf("Lecture (.%s) : %s%s", orig_ext, msg, .version_hint(msg))
        showNotification(paste("Reference — erreur de lecture :", msg), type = "error", duration = 15)
        NULL
      })
      if (is.null(raw_ref)) return(invisible(FALSE))

      incProgress(0.4, detail = "Conversion en objet Seurat...")
      ref_obj <- tryCatch(prepare_reference_seurat(raw_ref, project_name = "Reference"), error = function(e) {
        msg <- conditionMessage(e)
        ref_state$status  <- "error"
        ref_state$message <- sprintf("Conversion Seurat : %s%s", msg, .version_hint(msg))
        showNotification(paste("Reference — erreur de conversion :", msg), type = "error", duration = 15)
        NULL
      })
      if (is.null(ref_obj)) return(invisible(FALSE))
      if (!inherits(ref_obj, "Seurat")) {
        ref_state$status  <- "error"
        ref_state$message <- "Le fichier n'a pas pu etre converti en objet Seurat."
        showNotification("Reference — le fichier n'a pas pu etre converti en objet Seurat.",
                          type = "error", duration = 10)
        return(invisible(FALSE))
      }

      incProgress(0.3, detail = "Lecture des metadonnees...")
      meta_cols <- tryCatch(colnames(ref_obj@meta.data), error = function(e) {
        ref_state$status  <- "error"
        ref_state$message <- paste("Lecture metadata :", conditionMessage(e))
        showNotification(paste("Reference — erreur de lecture metadata :", conditionMessage(e)),
                          type = "error", duration = 10)
        NULL
      })
      if (is.null(meta_cols)) return(invisible(FALSE))

      ref_state$obj         <- ref_obj
      ref_state$staged_path <- staged_path
      ref_state$orig_ext    <- orig_ext
      ref_state$status      <- "loaded"
      ref_state$message     <- NULL
      ref_meta_cols(meta_cols)
      incProgress(0.15, detail = "Termine.")
      showNotification(sprintf("Reference chargee : %d cellules x %d genes (.%s).",
                                ncol(ref_obj), nrow(ref_obj), if (nzchar(orig_ext)) orig_ext else "?"),
                        type = "message", duration = 5)
      invisible(TRUE)
    })
  }

  # ── File upload ─────────────────────────────────────────────────────────
  observeEvent(input$ref_file, {
    req(input$ref_file)
    orig_ext <- tolower(tools::file_ext(input$ref_file$name))
    staged_path <- tryCatch({
      if (nzchar(orig_ext)) {
        p <- tempfile(fileext = paste0(".", orig_ext))
        file.copy(input$ref_file$datapath, p, overwrite = TRUE)
        p
      } else {
        input$ref_file$datapath
      }
    }, error = function(e) {
      ref_state$status  <- "error"
      ref_state$message <- paste("Copie du fichier :", conditionMessage(e))
      showNotification(paste("Reference — erreur de copie du fichier :", conditionMessage(e)),
                        type = "error", duration = 10)
      NULL
    })
    req(staged_path)
    run_reference_pipeline(staged_path, orig_ext)
  })

  observeEvent(input$btn_recheck_ref, {
    path <- ref_state$staged_path %||% NULL
    ext  <- ref_state$orig_ext %||% ""
    if (is.null(path)) {
      showNotification("Aucun fichier a revérifier — uploadez d'abord une reference.",
                        type = "warning", duration = 6)
      return()
    }
    run_reference_pipeline(path, ext)
  })

  # ── Shared reference (global_data$spatial_reference) ────────────────────
  using_shared_ref <- reactive({
    !is.null(global_data$spatial_reference) &&
      (is.null(input$ref_source) || identical(input$ref_source, "shared"))
  })

  reference_is_ready <- reactive({
    if (using_shared_ref()) return(!is.null(global_data$spatial_reference$path))
    identical(ref_state$status, "loaded") && !is.null(ref_path())
  })

  effective_ref_path <- reactive({
    if (using_shared_ref()) global_data$spatial_reference$path else ref_path()
  })

  output$ref_source_picker_ui <- renderUI({
    req(global_data$spatial_reference)
    radioButtons(ns("ref_source"), "Source de la reference",
                 choices = c("Reference partagee (Import > Spatial)" = "shared",
                             "Uploader une nouvelle reference (cette session)" = "upload"),
                 selected = "shared")
  })

  output$shared_ref_status_ui <- renderUI({
    req(global_data$spatial_reference)
    info <- global_data$spatial_reference
    div(class = "alert alert-success", style = "font-size:0.75rem;",
        bsicons::bs_icon("check-circle"),
        sprintf(" %s cellules x %s genes (colonne '%s', backend: %s)%s — preparee le %s.",
                format(info$n_cells, big.mark = ","), format(info$n_genes, big.mark = ","),
                info$celltype_col %||% "?", info$backend %||% "?",
                if (!is.null(info$source_label) && nzchar(info$source_label)) {
                  sprintf(" [%s]", info$source_label)
                } else "",
                tryCatch(format(info$created_at, "%d/%m %H:%M"), error = function(e) "?")))
  })

  output$ref_status_badge_ui <- renderUI({
    st <- ref_state$status
    badge <- switch(st,
      "empty" = div(class = "alert alert-light", style = "font-size:0.75rem; margin-bottom:4px;",
                    bsicons::bs_icon("circle"), " Aucune reference chargee."),
      "loading" = div(class = "alert alert-info", style = "font-size:0.75rem; margin-bottom:4px;",
                      bsicons::bs_icon("hourglass-split"), " Chargement de la reference en cours..."),
      "error" = div(class = "alert alert-danger", style = "font-size:0.75rem; margin-bottom:4px;",
                    bsicons::bs_icon("x-circle"),
                    sprintf(" Erreur : %s", ref_state$message %||% "cause inconnue")),
      "loaded" = if (reference_is_ready()) {
        div(class = "alert alert-success", style = "font-size:0.75rem; margin-bottom:4px;",
            bsicons::bs_icon("check-circle"),
            sprintf(" Reference chargee : %d cellules x %d genes.",
                    ncol(ref_state$obj), nrow(ref_state$obj)))
      } else {
        div(class = "alert alert-warning", style = "font-size:0.75rem; margin-bottom:4px;",
            bsicons::bs_icon("exclamation-triangle"),
            " Reference chargee — choisissez la colonne 'type cellulaire' puis cliquez ",
            "'1) Preparer la reference (disque)' ci-dessous.")
      },
      div(class = "alert alert-light", style = "font-size:0.75rem;", "Statut inconnu.")
    )
    tagList(
      badge,
      if (identical(st, "error") || identical(st, "loaded")) {
        actionLink(ns("btn_recheck_ref"), "\U1F504 Revérifier la reference (sans re-upload)",
                   style = "font-size:0.72rem;")
      }
    )
  })

  # ── Cell-type column + artifact preparation ─────────────────────────────
  output$ref_celltype_col_ui <- renderUI({
    req(ref_state$obj, length(ref_meta_cols()) > 0)
    ref_obj <- ref_state$obj
    cols <- ref_meta_cols()
    n_levels <- vapply(cols, function(cn) length(unique(ref_obj@meta.data[[cn]])), integer(1))
    useful <- cols[n_levels >= 2 & n_levels <= 200]
    useful <- useful[order(-n_levels[useful])]
    choices <- if (length(useful) > 0) useful else cols
    tagList(
      selectInput(ns("ref_celltype_col"), "Colonne 'type cellulaire'", choices = choices),
      checkboxInput(ns("merge_rare_types"),
                    sprintf("Fusionner/exclure les types rares (< %d cellules)", RCTD_CELL_MIN_INSTANCE),
                    value = TRUE),
      checkboxInput(ns("cap_ref_cells"),
                    "Limiter le nombre de cellules par type (RAM/vitesse)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("cap_ref_cells")),
        numericInput(ns("max_cells_per_type"), "Max cellules par type", 500, min = 25, max = 5000, step = 25)
      ),
      div(class = "text-muted", style = "font-size:0.68rem;",
          "Sous-echantillonnage stratifie (chaque type garde sa proportion relative) — ",
          "accelere nettement RCTD/Label Transfer sur une grosse reference (ex: >50k cellules)."),
      actionButton(ns("btn_prepare_ref"), "1) Preparer la reference (disque)",
                   icon = icon("database"), class = "btn-sm btn-outline-primary w-100 mt-2"),
      uiOutput(ns("ref_artifact_status_ui"))
    )
  })

  observeEvent(list(ref_state$obj, input$ref_celltype_col, input$merge_rare_types,
                     input$cap_ref_cells, input$max_cells_per_type), {
    ref_path(NULL)
    ref_artifact_info(NULL)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$btn_prepare_ref, {
    req(ref_state$obj, input$ref_celltype_col)
    ref_obj <- ref_state$obj
    withProgress(message = "Preparation de l'artifact de reference...", value = 0.1, {
      incProgress(0.2, detail = "Filtrage / fusion des types rares...")
      result <- tryCatch({
        artifact <- prepare_reference_artifact(
          ref_obj            = ref_obj,
          celltype_col       = input$ref_celltype_col,
          merge_rare_types   = isTRUE(input$merge_rare_types),
          min_cells_per_type = RCTD_CELL_MIN_INSTANCE,
          max_cells_per_type = if (isTRUE(input$cap_ref_cells)) input$max_cells_per_type else NA_integer_
        )
        incProgress(0.6, detail = "Ecriture sur disque...")
        artifact
      }, error = function(e) {
        showNotification(paste("Erreur preparation reference:", conditionMessage(e)), type = "error", duration = 10)
        NULL
      })
      if (!is.null(result)) {
        ref_path(result$path)
        ref_artifact_info(result)
        incProgress(0.1, detail = "Termine.")
        showNotification(sprintf(
          "Reference preparee : %d cellules x %d genes (backend: %s).%s",
          result$n_cells, result$n_genes, result$backend,
          if (result$n_dropped_rare > 0) sprintf(" %d cellule(s) trop rare(s) exclue(s).", result$n_dropped_rare) else ""
        ), type = "message", duration = 6)
      }
    })
  })

  output$ref_artifact_status_ui <- renderUI({
    info <- ref_artifact_info()
    if (is.null(info)) {
      return(div(class = "alert alert-light", style = "font-size:0.72rem; margin-top:4px;",
                 bsicons::bs_icon("info-circle"),
                 " Reference pas encore preparee pour la deconvolution — cliquez ci-dessus."))
    }
    div(class = "alert alert-success", style = "font-size:0.72rem; margin-top:4px;",
        bsicons::bs_icon("check-circle"),
        sprintf(" Pret : %d cellules x %d genes (backend: %s).%s",
                info$n_cells, info$n_genes, info$backend,
                if (info$n_dropped_rare > 0) sprintf(" %d cellule(s) exclue(s) (trop rares).", info$n_dropped_rare) else ""))
  })

  # ── Cell-type counts table ──────────────────────────────────────────────
  ref_celltype_counts <- reactive({
    req(ref_state$obj, input$ref_celltype_col)
    req(input$ref_celltype_col %in% colnames(ref_state$obj@meta.data))
    ct <- as.character(ref_state$obj@meta.data[[input$ref_celltype_col]])
    ct <- ct[!is.na(ct) & nzchar(ct)]
    validate(need(length(ct) > 0, "Aucune valeur non vide dans cette colonne."))
    tab_counts <- table(ct)
    tab <- data.frame(`Type cellulaire` = names(tab_counts),
                      Effectif          = as.integer(tab_counts),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[order(-tab$Effectif), ]
  })

  output$ref_celltype_summary_ui <- renderUI({
    tab <- tryCatch(ref_celltype_counts(), error = function(e) NULL)
    req(tab)
    rare <- tab[tab$Effectif < RCTD_CELL_MIN_INSTANCE, , drop = FALSE]
    tagList(
      if (nrow(rare) > 0) {
        div(class = "alert alert-warning", style = "font-size:0.72rem;",
            bsicons::bs_icon("exclamation-triangle"),
            sprintf(" %d type(s) sous %d cellules : %s.", nrow(rare), RCTD_CELL_MIN_INSTANCE,
                    paste(sprintf("%s (%d)", rare$`Type cellulaire`, rare$Effectif), collapse = ", ")),
            " Avec 'Fusionner/exclure les types rares' coche, ces cellules seront regroupees ",
            "sous 'Autre' puis, si ce groupe reste trop petit, exclues de la reference.")
      },
      div(style = "max-height:160px; overflow-y:auto;",
          DT::DTOutput(ns("ref_celltype_table")))
    )
  })
  output$ref_celltype_table <- DT::renderDT({
    DT::datatable(ref_celltype_counts(), rownames = FALSE,
                  options = list(pageLength = 6, dom = "tp"))
  })

  # ── Expose state to orchestrator ────────────────────────────────────────
  list(
    ref_state          = ref_state,
    ref_path           = ref_path,
    ref_artifact_info  = ref_artifact_info,
    ref_meta_cols      = ref_meta_cols,
    reference_is_ready = reference_is_ready,
    effective_ref_path = effective_ref_path,
    using_shared_ref   = using_shared_ref
  )
}