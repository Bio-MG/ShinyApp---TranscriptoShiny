# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution / Label Transfer
# =============================================================================
# v10 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    shared_rv$deconv_params ecrit au moment du clic sur "2) Lancer la
#    deconvolution" (miroir des parametres UI utilises + chemin de la
#    reference effectivement utilisee) -- purement additif, lu uniquement
#    par mod_spatial_export.R (script R reproductible) ; zero changement de
#    comportement pour cet onglet.
#
# v9 (backlog court-terme #1 et #3, session "puck_01 bugfix + backlog" —
#    voir handoff_spatial_bio-mg.md) :
#   1. Reference partagee (Import > Spatial, global_data$spatial_reference) :
#      quand une reference a ete preparee UNE FOIS dans l'onglet Import >
#      Spatial (nouveau panneau, mod_import_spatial.R), ce module propose de
#      la reutiliser directement (radio "Source de la reference") au lieu de
#      re-uploader/re-preparer a chaque session — additif, invisible tant
#      qu'aucune reference partagee n'existe (comportement local inchange).
#   2. STdeconvolve : ses labels de topics auto-generes ("T1_gene1.gene2...")
#      suivent maintenant la MEME regle de sanitisation que RCTD/Label
#      Transfer ("/" et "_" -> "-", voir R/utils_spatial_reference.R::
#      sanitize_celltype_labels()) — coherence de nommage inter-methodes.
#
# v8 (round-2 fixes from real-data testing on Allen cortex + human lymph
# node references):
#
#   1. Artifact preparation is no longer automatic/silent. The previous
#      observe() block ran prepare_reference_artifact() synchronously on the
#      main Shiny thread every time an input changed, with a static (non-
#      incrementing) withProgress() -- on a real ~70k-cell reference this
#      looked "stuck" (no real progress feedback) and gave no way to verify,
#      BEFORE clicking "Lancer", that the artifact actually matched the
#      currently-selected reference. Replaced with an explicit
#      "1) Preparer la reference (disque)" button + a persistent
#      confirmation panel (cell/gene counts, backend, cells dropped for
#      being too rare). Any change to the reference/column/merge/cap inputs
#      immediately invalidates ref_path() (so a stale artifact can never be
#      silently reused) without eagerly rebuilding it.
#   2. RCTD_TIMEOUT_MS: RCTD (doublet_mode="full") can run long independent
#      of reference size (reported: a 73k-cell / ~20-cell-type reference
#      exceeded the previous shared 20-minute ceiling) -- now has its own
#      dedicated ceiling, same pattern as Label Transfer's.
#   3. max_cells_per_type: optional stratified per-cell-type subsampling
#      (R/utils_spatial_reference.R::prepare_reference_artifact()) exposed
#      here as a UI control -- a much better RAM/speed lever for large real
#      references than shrinking the whole reference uniformly.
#   4. ref_celltype_counts() rewritten defensively (reported crash:
#      "'names' attribute [2] must be the same length as the vector [1]"
#      inside colnames(tab) <- ...): built directly from names()/values()
#      of table() instead of as.data.frame(table()), plus a guard against a
#      momentarily-stale ref_celltype_col value during a reference switch.
#   5. Error notification on deconv_task failure now names the CORRECT
#      timeout ceiling for whichever mode was actually invoked.
#
# v7 (Chantier 3 refonte — reference pipeline architecture fix): reference
# parsing (read_reference_scrna()/prepare_reference_seurat(),
# R/utils_spatial_reference.R) happens ONLY on the main Shiny process;
# nothing in the mirai daemon needs load_single_cell_data()/
# prepare_seurat_object() (those names are not defined anywhere in this
# codebase) or any multi-format reader package. The daemon reads a small,
# self-contained prepared artifact with base R + BPCells only.
#
# v6 (Chantier 2 refonte — reference reactive-state consolidation): ref_state
# carries an explicit $status ("empty"/"loading"/"error"/"loaded") + $message,
# unconditionally RESET at the start of every upload/recheck attempt, and a
# single reference_is_ready() reactive is the ONE place that decides "can we
# dispatch RCTD/Label Transfer".
#
# v5 (Phase 5): reference upload accepts .rds/.h5ad/.h5/.loom; RCTD's
# 25-cell-per-type minimum surfaced pre-flight.
#
# v4 (reported timeout with allen_cortex.rds): Label Transfer's fast
# LogNormalize path (default) + SCTransform opt-in, ncells control, own
# longer mirai timeout (LABEL_TRANSFER_TIMEOUT_MS).
#
# v3 (Phase 3 — "Integration with single-cell data"): Label Transfer mode.
#
# REWRITE (post-test-3): RCTD forced to max_cores=1; STdeconvolve bypassed
# via direct topicmodels::LDA() calls.
#
# Install:
#   remotes::install_github("dmcable/spacexr")                        # RCTD
#   install.packages(c("STdeconvolve", "topicmodels", "slam"))        # LDA
#   remotes::install_github("cellgeni/schard")           # .h5ad reference reader
#   (Label Transfer needs no extra package; optionally glmGamPoi for speed.)
# =============================================================================

RCTD_CELL_MIN_INSTANCE <- 25L
LABEL_TRANSFER_MIN_SHARED_GENES <- 50L

mod_spatial_deconv_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Deconvolution cellulaire", width = 380,

      radioButtons(ns("mode"), "Methode",
                   choices = c("Avec reference scRNA-seq (RCTD)" = "rctd",
                               "Transfert d'ancres (Label Transfer, Seurat)" = "labeltransfer",
                               "Sans reference (LDA, type STdeconvolve)" = "stdeconvolve"),
                   selected = "rctd"),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd' || input['%s'] == 'labeltransfer'", ns("mode"), ns("mode")),
        uiOutput(ns("ref_source_picker_ui")),
        conditionalPanel(
          condition = sprintf("!(input['%s'] && input['%s'] == 'shared')", ns("ref_source"), ns("ref_source")),
          fileInput(ns("ref_file"), "Reference scRNA-seq (.rds Seurat, .h5ad, .h5, .loom)",
                    accept = c(".rds", ".h5ad", ".h5", ".loom")),
          uiOutput(ns("ref_status_badge_ui")),
          uiOutput(ns("ref_celltype_col_ui")),
          uiOutput(ns("ref_celltype_summary_ui"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'shared'", ns("ref_source")),
          uiOutput(ns("shared_ref_status_ui"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go. Execute ",
            "en mono-coeur (max_cores=1). Necessite au moins 25 cellules par ",
            "type dans la reference.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'labeltransfer'", ns("mode")),
        radioButtons(ns("lt_norm_method"), "Normalisation",
                     choices = c("LogNormalize (rapide, recommande CPU)" = "lognorm",
                                 "SCTransform (vignette Seurat, plus lent)" = "sct"),
                     selected = "lognorm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sct'", ns("lt_norm_method")),
          numericInput(ns("lt_ncells"), "Cellules pour l'apprentissage SCTransform (ncells)",
                       3000, min = 500, max = 10000, step = 500),
          div(class = "alert alert-warning", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-triangle"),
              " Installez 'glmGamPoi' (BiocManager::install('glmGamPoi')) pour accelerer, ",
              "sinon reduisez plutot le nombre de cellules par type (section reference).")
        ),
        numericInput(ns("lt_npcs"), "Composantes PCA (requete spatiale)", 30, min = 5, max = 50, step = 5)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("mode")),
        numericInput(ns("n_topics"), "Nombre de types cellulaires (K)", 6, min = 2, max = 30, step = 1),
        numericInput(ns("n_top_od"), "Genes surdisperses maximum (vitesse)", 1000, min = 200, max = 3000, step = 100)
      ),

      bslib::input_task_button(ns("btn_deconv"), "2) Lancer la deconvolution",
                                icon = icon("puzzle-piece")),
      verbatimTextOutput(ns("deconv_progress_text"), placeholder = TRUE)
    ),

    navset_card_underline(
      nav_panel("Proportions par spot/cellule",
                plotOutput(ns("deconv_bar_plot"), height = "550px"),
                DT::DTOutput(ns("deconv_table"))),

      nav_panel("Reference (UMAP/PCA)",
                div(class = "alert alert-light small mb-2",
                    bsicons::bs_icon("info-circle"),
                    " Verifiez visuellement que les types cellulaires sont bien separes avant de ",
                    "lancer RCTD/Label Transfer (sidebar). Recalcule (asynchrone) si aucune reduction ",
                    "n'existe deja dans la reference."),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  selectInput(ns("ref_viz_reduction"), "Reduction",
                              choices = c("UMAP" = "umap", "PCA" = "pca"), selected = "umap"),
                  checkboxInput(ns("ref_viz_interactive"), "Vue interactive (Plotly)", value = TRUE),
                  bslib::input_task_button(ns("btn_ref_viz"), "Calculer / afficher", icon = icon("chart-line"))
                ),
                verbatimTextOutput(ns("ref_viz_progress_text"), placeholder = TRUE),
                conditionalPanel(condition = sprintf("input['%s']", ns("ref_viz_interactive")),
                                 plotly::plotlyOutput(ns("ref_viz_plotly"), height = "480px")),
                conditionalPanel(condition = sprintf("!input['%s']", ns("ref_viz_interactive")),
                                 plotOutput(ns("ref_viz_plot"), height = "480px"))),

      nav_panel("Colocalisation (types cellulaires)",
                div(class = "alert alert-light small mb-2",
                    "Correlation entre proportions de types cellulaires par spot/cellule -- deux types ",
                    "fortement correles positivement tendent a co-localiser ; negativement, a s'exclure."),
                plotOutput(ns("deconv_coloc_plot"), height = "520px"))
    )
  )
}

mod_spatial_deconv_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    discrete_palette_colors_fallback <- function(lv) {
      cols <- sc_discrete_colors(lv, palette = "okabeito")
      if (is.null(cols)) cols <- grDevices::hcl.colors(max(length(lv), 1), palette = "Dark 3")
      stats::setNames(cols, lv)
    }

    log_file <- spatial_log_path(session, "deconv")
    tracker  <- create_reactive_tracker(session, log_file)

    ref_state <- reactiveValues(
      obj = NULL, staged_path = NULL, orig_ext = NULL,
      status = "empty", message = NULL
    )
    ref_path        <- reactiveVal(NULL)
    ref_artifact_info <- reactiveVal(NULL)   # v8: confirmation panel data
    ref_meta_cols   <- reactiveVal(character(0))
    last_deconv_mode <- reactiveVal(NULL)    # v8: for mode-aware error text

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

    reference_is_ready <- reactive({
      if (using_shared_ref()) return(!is.null(global_data$spatial_reference$path))
      identical(ref_state$status, "loaded") && !is.null(ref_path())
    })

    # ── Backlog #1 (shared/persistent reference, Import > Spatial) ────────
    # global_data$spatial_reference is set by mod_import_spatial.R's own
    # "Reference scRNA-seq partagee" panel (upload ONCE, reuse across every
    # dataset import / RCTD / Label Transfer run in the session, without
    # re-parsing). Purely additive: when NULL (nobody has set one up yet),
    # ref_source_picker_ui renders NULL and every reactive below falls back
    # to the exact pre-existing local-upload behavior.
    using_shared_ref <- reactive({
      !is.null(global_data$spatial_reference) &&
        (is.null(input$ref_source) || identical(input$ref_source, "shared"))
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

    # v8: any change to what WOULD go into the artifact invalidates the
    # current one immediately (cheap) -- guarantees ref_path() can never
    # silently point at a reference/column/setting combination different
    # from what is currently selected in the UI. Rebuilding itself only
    # happens on the explicit button click below.
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

    # v8: rebuilt directly from names()/values() of table() -- immune to
    # the "wrong column count" edge case that crashed this output
    # previously ("'names' attribute [2] must be the same length as the
    # vector [1]") -- plus a guard against a momentarily-stale
    # ref_celltype_col value right after switching to a new reference file.
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

    # ── Reference UMAP/PCA preview (feedback biologiste, voir header UI) ──
    ref_viz_log_file <- spatial_log_path(session, "ref_viz")
    ref_viz_tracker  <- create_reactive_tracker(session, ref_viz_log_file)

    ref_viz_task <- ExtendedTask$new(function(mode, ref_obj_path, ref_manifest_path, celltype_col,
                                               reduction, max_cells, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement de la reference...", 1, 4)
          if (identical(mode, "local")) {
            obj <- readRDS(ref_obj_path)
            if (!celltype_col %in% colnames(obj@meta.data)) stop("Colonne 'type cellulaire' introuvable.")
            cell_types <- as.character(obj@meta.data[[celltype_col]])
            names(cell_types) <- colnames(obj)
          } else {
            manifest <- readRDS(ref_manifest_path)
            counts <- if (identical(manifest$backend, "bpcells")) {
              BPCells::open_matrix_dir(manifest$counts_path)
            } else readRDS(manifest$counts_path)
            obj <- Seurat::CreateSeuratObject(counts = counts)
            cell_types <- as.character(manifest$cell_types)[match(colnames(obj), names(manifest$cell_types))]
            names(cell_types) <- colnames(obj)
          }

          if (ncol(obj) > max_cells) {
            write_mirai_log(log_file, sprintf("Sous-echantillonnage (%d -> %d cellules, apercu rapide)...",
                                               ncol(obj), max_cells), 2, 4)
            set.seed(1)
            keep <- sample(colnames(obj), max_cells)
            obj <- obj[, keep]
            cell_types <- cell_types[keep]
          }

          has_red <- reduction %in% names(obj@reductions)
          if (has_red) {
            write_mirai_log(log_file, sprintf("Reduction '%s' deja presente -- reutilisee.", reduction), 3, 4)
          } else {
            write_mirai_log(log_file, "Normalisation + PCA...", 2, 4)
            obj <- Seurat::NormalizeData(obj, verbose = FALSE)
            obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
            obj <- Seurat::ScaleData(obj, verbose = FALSE)
            obj <- Seurat::RunPCA(obj, npcs = 30, verbose = FALSE)
            if (identical(reduction, "umap")) {
              write_mirai_log(log_file, "UMAP...", 3, 4)
              obj <- Seurat::RunUMAP(obj, dims = 1:30, verbose = FALSE)
            }
          }

          write_mirai_log(log_file, "Termine.", 4, 4)
          emb <- as.data.frame(Seurat::Embeddings(obj, reduction)[, 1:2])
          colnames(emb) <- c("dim1", "dim2")
          emb$id <- rownames(emb)
          emb$cell_type <- cell_types[emb$id]
          emb
        },
        mode = mode, ref_obj_path = ref_obj_path, ref_manifest_path = ref_manifest_path,
        celltype_col = celltype_col, reduction = reduction, max_cells = max_cells, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(ref_viz_task, "btn_ref_viz")

    observeEvent(input$btn_ref_viz, {
      reset_log(ref_viz_log_file)
      if (using_shared_ref()) {
        req(global_data$spatial_reference$path)
        ref_viz_task$invoke(mode = "shared", ref_obj_path = NULL,
                            ref_manifest_path = global_data$spatial_reference$path,
                            celltype_col = global_data$spatial_reference$celltype_col %||% "",
                            reduction = input$ref_viz_reduction, max_cells = 20000,
                            log_file = ref_viz_log_file)
      } else {
        if (is.null(ref_state$obj) || is.null(input$ref_celltype_col)) {
          showNotification("Chargez d'abord une reference et choisissez la colonne 'type cellulaire'.",
                           type = "warning", duration = 8)
          return()
        }
        tmp <- tempfile(fileext = ".rds")
        saveRDS(ref_state$obj, tmp)
        ref_viz_task$invoke(mode = "local", ref_obj_path = tmp, ref_manifest_path = NULL,
                            celltype_col = input$ref_celltype_col, reduction = input$ref_viz_reduction,
                            max_cells = 20000, log_file = ref_viz_log_file)
      }
    })

    ref_viz_result <- reactiveVal(NULL)
    observeEvent(ref_viz_task$status(), {
      if (ref_viz_task$status() == "success") {
        ref_viz_result(ref_viz_task$result())
      } else if (ref_viz_task$status() == "error") {
        showNotification("Erreur lors du calcul UMAP/PCA de la reference -- voir le journal.",
                         type = "error", duration = 10)
      }
    })

    output$ref_viz_progress_text <- renderText({
      lines <- ref_viz_tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    output$ref_viz_plot <- renderPlot({
      req(ref_viz_result())
      emb <- ref_viz_result()
      lv  <- sort(unique(stats::na.omit(as.character(emb$cell_type))))
      pal <- stats::setNames(grDevices::hcl.colors(max(length(lv), 1), palette = "Dark 3"), lv)
      centroids <- stats::aggregate(cbind(dim1, dim2) ~ cell_type, data = emb, FUN = stats::median)
      ggplot2::ggplot(emb, ggplot2::aes(x = dim1, y = dim2, color = cell_type)) +
        ggplot2::geom_point(size = 0.6, alpha = 0.7) +
        ggplot2::scale_color_manual(values = pal, na.value = "#CCCCCC") +
        ggplot2::geom_text(data = centroids, ggplot2::aes(label = cell_type), color = "black",
                           size = 3.2, fontface = "bold") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = paste0(toupper(input$ref_viz_reduction), "_1"),
                      y = paste0(toupper(input$ref_viz_reduction), "_2"),
                      title = "Reference scRNA-seq") +
        ggplot2::theme(legend.position = "none")
    })

    output$ref_viz_plotly <- plotly::renderPlotly({
      req(ref_viz_result())
      emb <- ref_viz_result()
      lv  <- sort(unique(stats::na.omit(as.character(emb$cell_type))))
      pal <- discrete_palette_colors_fallback(lv)
      plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cell_type, colors = pal,
                      type = "scattergl", mode = "markers",
                      marker = list(size = 5, opacity = 0.75),
                      text = ~paste0("Type: ", cell_type), hoverinfo = "text") |>
        plotly::layout(
          xaxis = list(title = paste0(toupper(input$ref_viz_reduction), "_1")),
          yaxis = list(title = paste0(toupper(input$ref_viz_reduction), "_2")),
          margin = list(l = 20, r = 20, t = 20, b = 20)
        )
    })

    # (B2, Vague 4) Colocalisation : correlation Pearson entre colonnes de
    # deconv_props, midpoint=0 fixe ("point fixe") -- pas de mirai (synchrone,
    # matrice deja en RAM, cout negligeable).
    output$deconv_coloc_plot <- renderPlot({
      req(shared_rv$deconv_props)
      df <- shared_rv$deconv_props
      cts <- setdiff(colnames(df), "id")
      validate(need(length(cts) >= 2, "Au moins 2 types cellulaires necessaires."))
      cor_mat <- stats::cor(df[, cts, drop = FALSE], use = "pairwise.complete.obs", method = "pearson")
      long <- reshape2::melt(cor_mat, varnames = c("Type1", "Type2"), value.name = "correlation")
      ggplot2::ggplot(long, ggplot2::aes(x = Type1, y = Type2, fill = correlation)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", correlation)), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                      midpoint = 0, limits = c(-1, 1)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = NULL, y = NULL, fill = "Correlation\n(Pearson)",
                      title = "Colocalisation des types cellulaires (proportions par spot)")
    })

    # ── Async deconvolution/integration task (mode chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells,
                                              min_shared_genes, log_file) {
      mirai::mirai(
        {
          .load_reference_artifact <- function(manifest_path) {
            manifest <- tryCatch(readRDS(manifest_path), error = function(e) {
              stop("Lecture du manifest de reference impossible (", manifest_path, ") : ",
                   conditionMessage(e))
            })
            counts <- if (identical(manifest$backend, "bpcells")) {
              if (!requireNamespace("BPCells", quietly = TRUE)) {
                stop("Package 'BPCells' requis pour lire la reference preparee (backend bpcells).")
              }
              BPCells::open_matrix_dir(manifest$counts_path)
            } else {
              readRDS(manifest$counts_path)
            }
            list(counts = counts, cell_types = manifest$cell_types)
          }

          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
          coords_df <- coords[match(colnames(mat), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(mat)
          keep <- stats::complete.cases(coords_df)
          mat <- mat[, keep, drop = FALSE]
          coords_df <- coords_df[keep, , drop = FALSE]

          sctransform_sequential <- function(obj_to_transform, ncells = 3000) {
            old_plan <- future::plan()
            on.exit(future::plan(old_plan), add = TRUE)
            future::plan("sequential")
            Seurat::SCTransform(obj_to_transform, ncells = ncells, verbose = FALSE)
          }

          if (identical(mode, "rctd")) {
            if (!requireNamespace("spacexr", quietly = TRUE)) {
              stop("Package 'spacexr' requis (remotes::install_github('dmcable/spacexr')).")
            }
            write_mirai_log(log_file, "Chargement de la reference preparee (artifact disque)...", 2, 5)
            reloaded   <- .load_reference_artifact(ref_path)
            ref_counts <- reloaded$counts
            if (!inherits(ref_counts, c("dgCMatrix", "matrix"))) {
              ref_counts <- methods::as(ref_counts, "dgCMatrix")
            }
            write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                               ncol(ref_counts), nrow(ref_counts)), 2, 5)
            reference <- spacexr::Reference(counts = ref_counts, cell_types = reloaded$cell_types)

            write_mirai_log(log_file, "Construction du 'puck' spatial (SpatialRNA)...", 3, 5)
            counts_dense <- methods::as(mat, "dgCMatrix")
            puck <- spacexr::SpatialRNA(coords = coords_df, counts = counts_dense)

            write_mirai_log(log_file, "RCTD (mode full, mono-coeur — pas de sous-processus imbrique)...", 4, 5)
            rctd <- spacexr::create.RCTD(puck, reference, max_cores = 1)
            rctd <- spacexr::run.RCTD(rctd, doublet_mode = "full")
            w <- as.matrix(rctd@results$weights)
            w <- sweep(w, 1, rowSums(w), "/")

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(w), w, row.names = NULL, check.names = FALSE)

          } else if (identical(mode, "labeltransfer")) {
            use_sct <- identical(lt_norm_method, "sct")

            if (use_sct) {
              has_glmgampoi <- requireNamespace("glmGamPoi", quietly = TRUE)
              write_mirai_log(log_file, sprintf(
                "Preparation de la reference (SCTransform, ncells=%d%s)...",
                lt_ncells, if (has_glmgampoi) "" else " -- 'glmGamPoi' absent, SCTransform sera plus lent"
              ), 2, 5)
            } else {
              write_mirai_log(log_file, "Preparation de la reference (LogNormalize, rapide)...", 2, 5)
            }

            reloaded <- .load_reference_artifact(ref_path)
            ref_obj  <- Seurat::CreateSeuratObject(counts = reloaded$counts)
            ref_obj$cell_type <- as.character(reloaded$cell_types)[match(colnames(ref_obj), names(reloaded$cell_types))]
            ref_obj  <- subset(ref_obj, cells = colnames(ref_obj)[!is.na(ref_obj$cell_type)])
            if (ncol(ref_obj) < 10) {
              stop("Reference trop petite apres filtrage des annotations manquantes (< 10 cellules annotees).")
            }
            write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                               ncol(ref_obj), nrow(ref_obj)), 2, 5)

            if (use_sct) {
              ref_obj <- sctransform_sequential(ref_obj, ncells = lt_ncells)
            } else {
              ref_obj <- Seurat::NormalizeData(ref_obj, verbose = FALSE)
              ref_obj <- Seurat::FindVariableFeatures(ref_obj, verbose = FALSE)
              ref_obj <- Seurat::ScaleData(ref_obj, verbose = FALSE)
            }
            ref_obj <- Seurat::RunPCA(ref_obj, npcs = 50, verbose = FALSE)

            write_mirai_log(log_file, sprintf(
              "Preparation de la requete spatiale (%s)...",
              if (use_sct) "SCTransform" else "LogNormalize, rapide"
            ), 3, 5)
            query <- Seurat::CreateSeuratObject(counts = mat)
            if (use_sct) {
              query <- sctransform_sequential(query, ncells = lt_ncells)
            } else {
              query <- Seurat::NormalizeData(query, verbose = FALSE)
              query <- Seurat::FindVariableFeatures(query, verbose = FALSE)
              query <- Seurat::ScaleData(query, verbose = FALSE)
            }
            n_pc  <- max(2, min(lt_npcs, ncol(query) - 1, nrow(query) - 1, 50))
            query <- Seurat::RunPCA(query, npcs = n_pc, verbose = FALSE)

            shared_genes <- intersect(rownames(ref_obj), rownames(query))
            if (length(shared_genes) < min_shared_genes) {
              stop(sprintf(
                paste0(
                  "Seulement %d gene(s) commun(s) entre la reference et les donnees spatiales ",
                  "(minimum requis : %d). Causes probables : conventions d'identifiants differentes ",
                  "(symboles vs Ensembl) ou organismes differents (humain/souris)."
                ),
                length(shared_genes), min_shared_genes
              ), call. = FALSE)
            }
            write_mirai_log(log_file, sprintf("%d genes communs reference/requete.", length(shared_genes)), 3, 5)

            write_mirai_log(log_file, sprintf("FindTransferAnchors (methode %s)...",
                                               if (use_sct) "SCT" else "LogNormalize"), 4, 5)
            anchors <- Seurat::FindTransferAnchors(
              reference = ref_obj, query = query,
              normalization.method = if (use_sct) "SCT" else "LogNormalize",
              npcs = min(30, n_pc)
            )
            predictions <- Seurat::TransferData(
              anchorset = anchors, refdata = ref_obj$cell_type,
              prediction.assay = TRUE,
              weight.reduction = query[["pca"]], dims = seq_len(n_pc)
            )

            write_mirai_log(log_file, "Termine.", 5, 5)
            pred_mat <- t(as.matrix(SeuratObject::LayerData(predictions, layer = "data")))
            pred_mat <- pred_mat[, setdiff(colnames(pred_mat), "max"), drop = FALSE]
            data.frame(id = rownames(pred_mat), pred_mat, row.names = NULL, check.names = FALSE)

          } else {
            if (!requireNamespace("STdeconvolve", quietly = TRUE) ||
                !requireNamespace("topicmodels", quietly = TRUE) ||
                !requireNamespace("slam", quietly = TRUE)) {
              stop("Packages 'STdeconvolve', 'topicmodels' et 'slam' requis.")
            }
            write_mirai_log(log_file, "Pretraitement (genes surdisperses)...", 2, 5)
            counts_dense <- as.matrix(methods::as(mat, "dgCMatrix"))
            storage.mode(counts_dense) <- "integer"
            corpus <- STdeconvolve::restrictCorpus(
              counts_dense, alpha = 0.05,
              nTopOD = min(n_top_od, nrow(counts_dense)), verbose = FALSE, plot = FALSE
            )

            write_mirai_log(log_file, sprintf("Ajustement LDA (K=%d, mono-coeur, iterations plafonnees)...", n_topics), 3, 5)
            corpus_stm <- slam::as.simple_triplet_matrix(t(as.matrix(corpus)))
            lda_model <- topicmodels::LDA(
              corpus_stm, k = n_topics,
              control = list(seed = 0, verbose = 0, keep = 0, estimate.alpha = FALSE,
                             em = list(iter.max = 100), var = list(iter.max = 50))
            )

            write_mirai_log(log_file, "Extraction des proportions (theta)...", 4, 5)
            post  <- topicmodels::posterior(lda_model)
            theta <- post$topics
            beta  <- post$terms
            theta[theta < 0.05] <- 0
            theta <- theta / rowSums(theta)
            theta[is.na(theta)] <- 0

            top_marker_genes <- apply(beta, 1, function(row) {
              ord <- order(row, decreasing = TRUE)
              paste(colnames(beta)[ord[seq_len(min(3, length(ord)))]], collapse = ".")
            })
            colnames(theta) <- paste0("T", seq_len(ncol(theta)), "_", top_marker_genes)
            # Backlog #3 (unification): apply the SAME character-substitution
            # rule as R/utils_spatial_reference.R::sanitize_celltype_labels()
            # (used by RCTD/Label Transfer) to STdeconvolve's own generated
            # topic labels -- "/" and "_" -> "-" -- so that IF a topic label
            # (built from gene symbols) is ever compared/joined against a
            # RCTD or Label Transfer column from a reference, both sides
            # follow the same naming convention. Inlined rather than
            # sourced: utils_spatial_reference.R is intentionally NOT
            # preloaded into this daemon (main-process-only, see
            # R/utils_spatial_async.R header) -- keeping this a one-line
            # inline regex avoids adding that dependency for a single rule.
            colnames(theta) <- gsub("[/_\\\\]", "-", colnames(theta))

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(theta), theta, row.names = NULL, check.names = FALSE)
          }
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od,
        lt_npcs = lt_npcs, lt_norm_method = lt_norm_method, lt_ncells = lt_ncells,
        min_shared_genes = min_shared_genes, log_file = log_file,
        # v8: RCTD now also has its own dedicated ceiling (see
        # R/utils_spatial_async.R::RCTD_TIMEOUT_MS) instead of sharing the
        # generic 20-minute one.
        .timeout = switch(mode,
                          "labeltransfer" = LABEL_TRANSFER_TIMEOUT_MS,
                          "rctd"          = RCTD_TIMEOUT_MS,
                          MIRAI_TASK_TIMEOUT_MS)
      )
    })
    bslib::bind_task_button(deconv_task, "btn_deconv")

    observeEvent(input$btn_deconv, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)

      if (input$mode %in% c("rctd", "labeltransfer")) {
        if (!reference_is_ready()) {
          reason <- if (using_shared_ref()) {
            "reference partagee introuvable ou invalide -- reimportez-la depuis l'onglet Import > Spatial"
          } else {
            switch(ref_state$status,
              "empty"   = "aucune reference chargee (section 'Reference scRNA-seq')",
              "loading" = "chargement de la reference encore en cours -- patientez puis reessayez",
              "error"   = sprintf("la reference a echoue au chargement (%s)", ref_state$message %||% "cause inconnue"),
              "loaded"  = if (is.null(input$ref_celltype_col) || !nzchar(input$ref_celltype_col)) {
                "aucune colonne 'type cellulaire' selectionnee"
              } else {
                "reference pas encore preparee -- cliquez '1) Preparer la reference (disque)' puis reessayez"
              },
              "cause inconnue"
            )
          }
          showNotification(paste0("Chargez d'abord une reference scRNA-seq complete : ", reason),
                            type = "warning", duration = 14)
          return()
        }
      }

      if (identical(input$mode, "rctd")) {
        ref_check <- tryCatch(readRDS(effective_ref_path()), error = function(e) NULL)
        if (is.null(ref_check)) {
          showNotification("Reference introuvable ou illisible — reimportez le fichier de reference.",
                            type = "error", duration = 8)
          return()
        }
        tab <- table(as.character(ref_check$cell_types))
        too_small <- names(tab)[tab < RCTD_CELL_MIN_INSTANCE]
        if (length(too_small) > 0) {
          showNotification(sprintf(
            "RCTD necessite au moins %d cellules par type dans la reference. Type(s) insuffisant(s) : %s. ",
            RCTD_CELL_MIN_INSTANCE,
            paste(sprintf("%s (%d)", too_small, as.integer(tab[too_small])), collapse = ", ")
          ), type = "error", duration = 14)
          showNotification(
            if (using_shared_ref()) {
              paste0("Repreparez la reference partagee depuis l'onglet Import > Spatial ",
                    "(case 'Fusionner/exclure les types rares') avant de relancer.")
            } else {
              paste0("Cochez 'Fusionner/exclure les types rares' (sidebar) puis cliquez a nouveau ",
                    "'1) Preparer la reference' avant de relancer.")
            },
            type = "warning", duration = 14)
          return()
        }
      }

      last_deconv_mode(input$mode)
      reset_log(log_file)
      # v10 (export/script reproductible) : miroir des parametres UTILISES
      # (y compris le chemin de reference EFFECTIF, partagee ou locale) --
      # lu uniquement par mod_spatial_export.R -- purement additif.
      shared_rv$deconv_params <- list(
        mode = input$mode,
        ref_path = if (input$mode %in% c("rctd", "labeltransfer")) effective_ref_path() else NULL,
        ref_source_label = if (input$mode %in% c("rctd", "labeltransfer")) {
          if (using_shared_ref()) "reference partagee (Import > Spatial)" else "upload local (session)"
        } else NULL,
        n_topics = input$n_topics, n_top_od = input$n_top_od,
        lt_npcs = input$lt_npcs, lt_norm_method = input$lt_norm_method, lt_ncells = input$lt_ncells
      )
      deconv_task$invoke(
        bpcells_dir      = global_data$spatial_obj$bpcells_dir,
        pass_idx         = shared_rv$qc_pass_idx,
        coords           = global_data$spatial_obj$coords,
        mode             = input$mode,
        ref_path         = effective_ref_path(),
        n_topics         = input$n_topics,
        n_top_od         = input$n_top_od %||% 1000,
        lt_npcs          = input$lt_npcs %||% 30,
        lt_norm_method   = input$lt_norm_method %||% "lognorm",
        lt_ncells        = input$lt_ncells %||% 3000,
        min_shared_genes = LABEL_TRANSFER_MIN_SHARED_GENES,
        log_file         = log_file
      )
    })

    observeEvent(deconv_task$status(), {
      if (deconv_task$status() == "success") {
        shared_rv$deconv_props <- deconv_task$result()
        showNotification("Deconvolution terminee.", type = "message", duration = 5)
      } else if (deconv_task$status() == "error") {
        ceiling_txt <- switch(last_deconv_mode() %||% "?",
                              "labeltransfer" = "45 min",
                              "rctd"          = "40 min",
                              "20 min")
        showNotification(sprintf(
          "Erreur (ou depassement du delai de %s) pendant la deconvolution — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez.",
          ceiling_txt
        ), type = "error", duration = 12)
      }
    })

    output$deconv_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })

    output$deconv_bar_plot <- renderPlot({
      req(shared_rv$deconv_props)
      df <- shared_rv$deconv_props
      long <- reshape2::melt(df, id.vars = "id", variable.name = "cell_type", value.name = "proportion")
      ids_show <- utils::head(unique(long$id), 60)
      ggplot2::ggplot(long[long$id %in% ids_show, ],
                       ggplot2::aes(x = id, y = proportion, fill = cell_type)) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text(size = 8)) +
        ggplot2::labs(x = "Spots/cellules (echantillon)", y = "Proportion", fill = "Type")
    })

    output$deconv_table <- DT::renderDT({
      req(shared_rv$deconv_props)
      DT::datatable(shared_rv$deconv_props, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
        DT::formatRound(setdiff(colnames(shared_rv$deconv_props), "id"), 3)
    })
  })
}
