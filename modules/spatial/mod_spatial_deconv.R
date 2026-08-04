# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution / Label Transfer
# =============================================================================
# v7 (Chantier 3 refonte — reference pipeline architecture fix). Root cause:
# this module's daemon-side reference reload called
# load_single_cell_data()/prepare_seurat_object() -- documented (helpers_io.R
# header) as living in helpers_sc.R, but NOT actually defined anywhere in
# this codebase. That is why RCTD/Label Transfer failed with "could not
# find function", both in the daemon AND, transitively, in this module's own
# run_reference_pipeline() on the main process. This is fixed by removing
# the dependency entirely rather than re-adding a preload entry:
#
#   - run_reference_pipeline() now calls read_reference_scrna()/
#     prepare_reference_seurat() (R/utils_spatial_reference.R) -- a small,
#     self-contained multi-format reader (.rds/.h5ad/.h5/.loom), used ONLY
#     on the main Shiny process.
#   - Once a cell-type column is chosen, prepare_reference_artifact()
#     (same file) writes a SELF-CONTAINED on-disk artifact (raw counts +
#     cell-type labels, dgCMatrix RDS or BPCells dir for very large
#     references) -- multi-format parsing happens EXACTLY ONCE, here.
#   - deconv_task's mirai body no longer calls ANY project helper function
#     to read the reference: `.load_reference_artifact()` (inlined directly
#     in the daemon body below) uses ONLY base R + BPCells. This removes the
#     whole class of "daemon preload drift" bug for the reference path --
#     there is nothing left that a future refactor of helpers_io.R/
#     helpers_sc.R could silently break.
#   - A second, same-class latent bug was found and fixed while auditing
#     this: LABEL_TRANSFER_MIN_SHARED_GENES (a top-level constant of THIS
#     file) was referenced directly inside the mirai body. mod_spatial_deconv.R
#     itself was never part of R/utils_spatial_async.R's daemon source_files,
#     so that reference would ALSO have failed with "object not found" the
#     first time a Label Transfer run actually hit that code path. It is now
#     passed explicitly as a named task argument (min_shared_genes), the
#     same pattern already used for every other value the daemon needs.
#
# v6 (Chantier 2 refonte — reference reactive-state consolidation): ref_state
# carries an explicit $status ("empty"/"loading"/"error"/"loaded") + $message,
# unconditionally RESET at the start of every upload/recheck attempt, and a
# single reference_is_ready() reactive is the ONE place that decides "can we
# dispatch RCTD/Label Transfer" — used by both a persistent sidebar badge and
# the dispatch guard itself. Unchanged by v7 above (still the right design);
# only WHAT gets parsed/stored under the hood has changed.
#
# v5 (Phase 5 — real-world failure): reference upload accepts .rds/.h5ad/
# .h5/.loom (kept, see read_reference_scrna()); RCTD's 25-cell-per-type
# minimum is surfaced pre-flight instead of failing deep inside the daemon
# (kept, RCTD_CELL_MIN_INSTANCE below).
#
# v4 (reported timeout with allen_cortex.rds): Label Transfer's fast
# LogNormalize path (default) + SCTransform opt-in, ncells control, own
# longer mirai timeout (LABEL_TRANSFER_TIMEOUT_MS) — all unchanged.
#
# v3 (Phase 3 — "Integration with single-cell data"): Label Transfer mode,
# same shared_rv$deconv_props contract as RCTD/STdeconvolve — unchanged.
#
# REWRITE (post-test-3): RCTD forced to max_cores=1 (spacexr opens its own
# parallel::makeCluster() otherwise); STdeconvolve bypassed via direct
# topicmodels::LDA() calls (BiocParallel::SnowParam() otherwise hardcoded
# inside STdeconvolve::fitLDA()) — both unchanged, not touched by v7.
#
# Install:
#   remotes::install_github("dmcable/spacexr")                        # RCTD
#   install.packages(c("STdeconvolve", "topicmodels", "slam"))        # LDA
#   remotes::install_github("cellgeni/schard")           # .h5ad reference reader
#   (Label Transfer needs no extra package beyond Seurat itself; optionally
#   install Bioconductor's glmGamPoi for a faster SCTransform.)
#
# Hard rule compliance: the scRNA-seq reference is never shipped into the
# daemon as a live Seurat object, and — as of v7 — never as a pointer to the
# RAW uploaded file either. Only the small PREPARED artifact (counts +
# cell_types, written once by prepare_reference_artifact()) travels in.
# SCTransform() is still forced onto a local future::plan("sequential")
# inside the daemon before being called (defensive, cheap insurance).
# =============================================================================

# RCTD's own hard minimum (spacexr::process_cell_type_info() ->
# CELL_MIN_INSTANCE) — mirrored here (not importable, spacexr has no
# exported constant) so the app can warn/block BEFORE dispatching a doomed
# daemon task, using the exact number spacexr itself enforces.
RCTD_CELL_MIN_INSTANCE <- 25L

# Minimum shared reference/query genes below which FindTransferAnchors()
# either fails outright or produces meaningless anchors. Passed explicitly
# into the mirai task as `min_shared_genes` (see v7 changelog above) rather
# than referenced as a free variable inside the daemon body.
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

      # ── Reference upload: shared by RCTD and Label Transfer (both need
      #    an annotated scRNA-seq reference; only the algorithm differs).
      #    Accepts .rds/.h5ad/.h5/.loom -- see read_reference_scrna()
      #    (R/utils_spatial_reference.R). ──
      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd' || input['%s'] == 'labeltransfer'", ns("mode"), ns("mode")),
        fileInput(ns("ref_file"), "Reference scRNA-seq (.rds Seurat, .h5ad, .h5, .loom)",
                  accept = c(".rds", ".h5ad", ".h5", ".loom")),
        # Persistent status badge (Chantier 2 refonte) -- visible the
        # moment something goes wrong, not only after clicking "Lancer".
        uiOutput(ns("ref_status_badge_ui")),
        uiOutput(ns("ref_celltype_col_ui")),
        uiOutput(ns("ref_celltype_summary_ui"))
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go. Execute ",
            "en mono-coeur (max_cores=1) pour eviter tout sous-processus imbrique. ",
            "Necessite au moins 25 cellules par type dans la reference (limite ",
            "propre a RCTD — voir le recapitulatif ci-dessus). Les colonnes du ",
            "tableau resultat portent directement les noms de type cellulaire ",
            "de votre reference (ex: 'Astrocytes', 'Neurons_L4').")
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
              " SCTransform() est signifcativement plus lente sans le package 'glmGamPoi' ",
              "(non installe par defaut) — installez-le via ",
              "BiocManager::install('glmGamPoi') pour accelerer nettement cette etape. ",
              "'ncells' reprend l'astuce de la vignette Seurat elle-meme : apprend le ",
              "modele de bruit sur un sous-echantillon (3000 par defaut) tout en ",
              "normalisant l'ensemble des cellules — reduit le temps de calcul sans perte ",
              "de qualite notable.")
        ),
        numericInput(ns("lt_npcs"), "Composantes PCA (requete spatiale)", 30, min = 5, max = 50, step = 5),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " FindTransferAnchors()/TransferData() (vignette Seurat Spatial — methode ",
            "d'integration \"par ancres\", souvent recommandee plutot que la deconvolution ",
            "pure). Trouve des paires cellule/spot similaires (\"ancres\") apres normalisation ",
            "de la reference et de la requete, puis transfere un score de probabilite ",
            "par type cellulaire a chaque spot — les colonnes du tableau resultat portent, ",
            "comme pour RCTD, les noms de type cellulaire de votre reference. Pas de minimum ",
            "de cellules impose (contrairement a RCTD), mais un type tres rare donnera un ",
            "score plus bruite.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("mode")),
        numericInput(ns("n_topics"), "Nombre de types cellulaires (K)", 6, min = 2, max = 30, step = 1),
        numericInput(ns("n_top_od"), "Genes surdisperses maximum (vitesse)", 1000, min = 200, max = 3000, step = 100),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " Allocation de Dirichlet Latente (LDA) : extrait K 'themes' ",
            "d'expression purs sans reference externe. Reduisez K ou le ",
            "nombre de genes pour accelerer."),
        div(class = "alert alert-warning", style = "font-size:0.75rem;",
            bsicons::bs_icon("exclamation-triangle"),
            " Sans reference, les topiques sont des SIGNATURES D'EXPRESSION numerotees ",
            "(1, 2, 3...), pas des types cellulaires identifies. Pour rester interpretable, ",
            "chaque colonne du tableau resultat est etiquetee avec ses 3 genes les plus ",
            "caracteristiques (ex: 'T3_Gfap.Aqp4.Mbp') — a vous d'interpreter biologiquement ",
            "chaque signature a partir de ces marqueurs.")
      ),

      bslib::input_task_button(ns("btn_deconv"), "Lancer la deconvolution",
                                icon = icon("puzzle-piece")),
      verbatimTextOutput(ns("deconv_progress_text"), placeholder = TRUE)
    ),

    card(
      full_screen = TRUE,
      card_header("Proportions par spot/cellule"),
      plotOutput(ns("deconv_bar_plot"), height = "550px"),
      DT::DTOutput(ns("deconv_table"))
    )
  )
}

mod_spatial_deconv_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_file <- spatial_log_path(session, "deconv")
    tracker  <- create_reactive_tracker(session, log_file)

    # ── Consolidated reference state (Chantier 2 refonte, unchanged by v7) ─
    # "empty"   -> nothing uploaded yet
    # "loading" -> a stage-1..4 pipeline run is currently in flight
    # "error"   -> the last attempt failed; $message explains why. obj/
    #              staged_path are ALWAYS cleared on error.
    # "loaded"  -> obj/staged_path are valid; ref_path() (the prepared
    #              artifact the daemon actually reads) may still be NULL for
    #              a moment while the observe() block below reacts to the
    #              freshly-rendered celltype_col input -- see reference_is_ready().
    ref_state <- reactiveValues(
      obj = NULL, staged_path = NULL, orig_ext = NULL,
      status = "empty", message = NULL
    )
    ref_path      <- reactiveVal(NULL)
    ref_meta_cols <- reactiveVal(character(0))

    .version_hint <- function(msg) {
      if (!grepl("GetAssayData|slot.*defunct|defunct.*slot", msg, ignore.case = TRUE)) return("")
      sprintf(" [Seurat %s | SeuratObject %s | schard %s | SeuratDisk %s]",
              tryCatch(as.character(utils::packageVersion("Seurat")), error = function(e) "?"),
              tryCatch(as.character(utils::packageVersion("SeuratObject")), error = function(e) "?"),
              tryCatch(as.character(utils::packageVersion("schard")), error = function(e) "absent"),
              tryCatch(as.character(utils::packageVersion("SeuratDisk")), error = function(e) "absent"))
    }

    # ── Reference parse/convert pipeline (main process ONLY) ───────────────
    # v7: read_reference_scrna()/prepare_reference_seurat()
    # (R/utils_spatial_reference.R) replace the previously-referenced
    # load_single_cell_data()/prepare_seurat_object() -- those names were
    # documented as living in helpers_sc.R but are not actually defined
    # there in this codebase (root cause of "could not find function", both
    # here and, more importantly, inside the mirai daemon). Nothing here
    # ever runs inside a daemon; the daemon only ever reads the prepared
    # artifact built by the observe() block further down.
    run_reference_pipeline <- function(staged_path, orig_ext) {
      ref_state$obj         <- NULL
      ref_state$staged_path <- NULL
      ref_state$status      <- "loading"
      ref_state$message     <- NULL
      ref_meta_cols(character(0))
      ref_path(NULL)

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
      identical(ref_state$status, "loaded") && !is.null(ref_path())
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
              sprintf(" Reference prete : %d cellules x %d genes.",
                      ncol(ref_state$obj), nrow(ref_state$obj)))
        } else {
          div(class = "alert alert-warning", style = "font-size:0.75rem; margin-bottom:4px;",
              bsicons::bs_icon("exclamation-triangle"),
              " Reference chargee — choisissez la colonne 'type cellulaire' ci-dessous ",
              "pour finaliser la preparation.")
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
                      "Fusionner les types rares (< 25 cellules) en 'Autre'",
                      value = TRUE)
      )
    })

    ref_celltype_counts <- reactive({
      req(ref_state$obj, input$ref_celltype_col)
      ct <- as.character(ref_state$obj@meta.data[[input$ref_celltype_col]])
      tab <- as.data.frame(table(ct), stringsAsFactors = FALSE)
      colnames(tab) <- c("Type cellulaire", "Effectif")
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
              " RCTD echouera sur ces types tant qu'ils ne sont pas fusionnes/exclus ",
              "(voir la case a cocher ci-dessus) ou qu'une colonne moins fine n'est choisie.")
        },
        div(style = "max-height:160px; overflow-y:auto;",
            DT::DTOutput(ns("ref_celltype_table")))
      )
    })
    output$ref_celltype_table <- DT::renderDT({
      DT::datatable(ref_celltype_counts(), rownames = FALSE,
                    options = list(pageLength = 6, dom = "tp"))
    })

    # ── Build the self-contained, daemon-ready reference artifact ─────────
    # v7: this used to save ONLY a pointer (staged_path + cell_types) and
    # let the daemon re-parse the RAW file itself. prepare_reference_artifact()
    # (R/utils_spatial_reference.R) now writes the actual counts + labels to
    # disk HERE, on the main process -- see that function's header for the
    # backend choice (dgCMatrix RDS vs BPCells dir above bpcells_threshold).
    observe({
      req(ref_state$obj, input$ref_celltype_col)
      ref_obj <- ref_state$obj
      withProgress(message = "Preparation de l'artifact de reference (disque)...", value = 0.5, {
        tryCatch({
          artifact <- prepare_reference_artifact(
            ref_obj            = ref_obj,
            celltype_col       = input$ref_celltype_col,
            merge_rare_types   = isTRUE(input$merge_rare_types),
            min_cells_per_type = RCTD_CELL_MIN_INSTANCE
          )
          ref_path(artifact$path)
        }, error = function(e) {
          ref_path(NULL)
          showNotification(paste("Erreur preparation reference:", conditionMessage(e)), type = "error", duration = 8)
        })
      })
    })

    # ── Async deconvolution/integration task (mode chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells,
                                              min_shared_genes, log_file) {
      mirai::mirai(
        {
          # v7: self-contained artifact loader -- ONLY base R + BPCells, no
          # project helper function and no multi-format reader package
          # required inside the daemon. `manifest_path` points either at a
          # plain dgCMatrix RDS or a BPCells on-disk directory (written by
          # prepare_reference_artifact() on the main process).
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
            # spacexr::Reference() expects an in-memory (sparse) matrix --
            # materialize only here, at point of use (same "small enough at
            # this point" pattern already used for the spatial puck below).
            if (!inherits(ref_counts, c("dgCMatrix", "matrix"))) {
              ref_counts <- methods::as(ref_counts, "dgCMatrix")
            }
            write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                               ncol(ref_counts), nrow(ref_counts)), 2, 5)
            reference <- spacexr::Reference(counts = ref_counts, cell_types = reloaded$cell_types)

            write_mirai_log(log_file, "Construction du 'puck' spatial (SpatialRNA)...", 3, 5)
            counts_dense <- methods::as(mat, "dgCMatrix")  # small: already QC-subset, single technical materialization
            puck <- spacexr::SpatialRNA(coords = coords_df, counts = counts_dense)

            write_mirai_log(log_file, "RCTD (mode full, mono-coeur — pas de sous-processus imbrique)...", 4, 5)
            rctd <- spacexr::create.RCTD(puck, reference, max_cores = 1)
            rctd <- spacexr::run.RCTD(rctd, doublet_mode = "full")
            w <- as.matrix(rctd@results$weights)
            w <- sweep(w, 1, rowSums(w), "/")  # normalize to proportions

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

            # Preflight: near-zero reference/query gene overlap most often
            # means mismatched ID conventions -- fail clearly instead of
            # letting FindTransferAnchors() error cryptically or return
            # near-meaningless anchors. `min_shared_genes` is passed in
            # explicitly (see v7 changelog) rather than read as a free
            # top-level variable, which would not exist inside this daemon.
            shared_genes <- intersect(rownames(ref_obj), rownames(query))
            if (length(shared_genes) < min_shared_genes) {
              stop(sprintf(
                paste0(
                  "Seulement %d gene(s) commun(s) entre la reference et les donnees spatiales ",
                  "(minimum requis : %d). Causes probables : conventions d'identifiants differentes ",
                  "(symboles vs Ensembl) ou organismes differents (humain/souris) entre la reference ",
                  "et les donnees spatiales. Verifiez rownames(reference) vs rownames(objet spatial)."
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
            # TransferData() appends a "max" feature (top prediction score per
            # spot) — not a cell type, drop it so every column is a genuine class.
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
            storage.mode(counts_dense) <- "integer"  # LDA requires integer counts
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
            beta  <- post$terms   # K x nGenes: P(gene | topic) -- used only to LABEL topics below
            theta[theta < 0.05] <- 0
            theta <- theta / rowSums(theta)
            theta[is.na(theta)] <- 0

            top_marker_genes <- apply(beta, 1, function(row) {
              ord <- order(row, decreasing = TRUE)
              paste(colnames(beta)[ord[seq_len(min(3, length(ord)))]], collapse = ".")
            })
            colnames(theta) <- paste0("T", seq_len(ncol(theta)), "_", top_marker_genes)

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(theta), theta, row.names = NULL, check.names = FALSE)
          }
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od,
        lt_npcs = lt_npcs, lt_norm_method = lt_norm_method, lt_ncells = lt_ncells,
        min_shared_genes = min_shared_genes, log_file = log_file,
        .timeout = if (identical(mode, "labeltransfer")) LABEL_TRANSFER_TIMEOUT_MS else MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(deconv_task, "btn_deconv")

    observeEvent(input$btn_deconv, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)

      if (input$mode %in% c("rctd", "labeltransfer")) {
        if (!reference_is_ready()) {
          reason <- switch(ref_state$status,
            "empty"   = "aucune reference chargee (section 'Reference scRNA-seq')",
            "loading" = "chargement de la reference encore en cours -- patientez puis reessayez",
            "error"   = sprintf("la reference a echoue au chargement (%s)", ref_state$message %||% "cause inconnue"),
            "loaded"  = if (is.null(input$ref_celltype_col) || !nzchar(input$ref_celltype_col)) {
              "aucune colonne 'type cellulaire' selectionnee"
            } else {
              "preparation de la reference pas encore terminee (patientez 1-2s apres avoir choisi la colonne, puis reessayez)"
            },
            "cause inconnue"
          )
          showNotification(paste0("Chargez d'abord une reference scRNA-seq complete : ", reason),
                            type = "warning", duration = 14)
          return()
        }
      }

      # RCTD hard-fails deep inside the daemon if any cell type has < 25
      # cells (spacexr::process_cell_type_info()) — check BEFORE dispatching.
      # readRDS(ref_path()) now reads the prepared MANIFEST (see
      # prepare_reference_artifact()), which still carries a `cell_types`
      # field with the exact same name/shape as before -- no change needed
      # to this block.
      if (identical(input$mode, "rctd")) {
        ref_check <- tryCatch(readRDS(ref_path()), error = function(e) NULL)
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
            "Cochez 'Fusionner les types rares en Autre' (sidebar) ou choisissez une colonne ",
            "d'annotation moins fine, puis relancez.", type = "warning", duration = 14)
          return()
        }
      }

      reset_log(log_file)
      deconv_task$invoke(
        bpcells_dir      = global_data$spatial_obj$bpcells_dir,
        pass_idx         = shared_rv$qc_pass_idx,
        coords           = global_data$spatial_obj$coords,
        mode             = input$mode,
        ref_path         = ref_path(),
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
        showNotification(
          "Erreur (ou depassement du delai de 20 min) pendant la deconvolution — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez.",
          type = "error", duration = 12)
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
