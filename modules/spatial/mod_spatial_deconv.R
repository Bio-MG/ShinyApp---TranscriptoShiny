# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution / Label Transfer
# =============================================================================
# v6 (Chantier 2 refonte — reference reactive-state consolidation + daemon
# fix): two INDEPENDENT root causes were found for "RCTD/Label Transfer
# always fail, STdeconvolve always works":
#
#   1. STRUCTURAL (this is why it failed 100% of the time, regardless of
#      reference quality): R/utils_spatial_async.R's init_spatial_daemons()
#      never included helpers_io.R/helpers_sc.R in the files preloaded into
#      each mirai daemon — only R/utils_spatial_*.R. The RCTD and Label
#      Transfer daemon bodies below call load_single_cell_data() +
#      prepare_seurat_object() to re-read the staged reference file INSIDE
#      the daemon (by design — see the "hard rule compliance" note further
#      down): those two functions were simply undefined in that process,
#      so every run failed with a raw "could not find function" error deep
#      in the daemon, surfaced only as the generic ExtendedTask failure
#      notification. STdeconvolve needs neither function (no reference),
#      which is exactly why it alone kept working. Fixed in
#      R/utils_spatial_async.R (source_files now includes both files) —
#      this file's own change is to make a *recurrence* of this class of
#      bug loud instead of silent: both calls are now wrapped in tryCatch()
#      with an explicit write_mirai_log() + stop() naming the missing
#      function, so if a future refactor removes the daemon preload again,
#      the log says exactly why instead of a bare ExtendedTask error.
#
#   2. UX/ROBUSTNESS (this is what made troubleshooting attempt #1 above
#      hard to pin down): the reference upload state was correct in
#      isolation but only surfaced itself AT CLICK TIME, and a FAILED
#      re-upload (e.g. a second, bad file after a first good one) left the
#      previous successful ref_state$obj in place — so the click-time guard
#      could pass on a stale reference the user believed had just failed.
#      ref_state now carries an explicit $status ("empty"/"loading"/
#      "error"/"loaded") + $message, is unconditionally RESET at the start
#      of every upload/recheck attempt (no more stale success surviving a
#      failed re-upload), and a single reference_is_ready() reactive is the
#      ONE place that decides "can we dispatch RCTD/Label Transfer" — used
#      by both a persistent sidebar badge (visible the moment something
#      goes wrong, not just on click) and the dispatch guard itself. A new
#      "Revérifier la reference" button re-runs the parse/convert/metadata
#      steps from the already-staged file without requiring a re-upload —
#      isolates reference PREPARATION problems from RCTD/Label Transfer
#      ALGORITHM problems, per the "chemin de test" requirement.
#
# v5 (Phase 5 — reported real-world failure): two issues surfaced testing
# against a real external reference (cell2location's integrated lymphoid
# organ scRNA-seq reference, .h5ad):
#   1. Reference upload only accepted .rds (a pre-saved Seurat object) — any
#      other standard format (.h5ad/.h5/.loom) had no path in. Reference
#      loading now goes through helpers_io.R::load_single_cell_data() +
#      prepare_seurat_object() (the SAME helpers the main Single-Cell import
#      module already uses) instead of a hardcoded readRDS() — "universalize"
#      the reference importer for free by reusing existing, shared code
#      rather than duplicating format-specific logic here. See
#      helpers_io.R's own v-next changelog for the .h5ad reader change
#      (now prefers 'schard' over the fragile SeuratDisk convert round-trip).
#   2. RCTD (spacexr::create.RCTD()) hard-requires >= 25 cells per cell type
#      in the reference (its own CELL_MIN_INSTANCE constant) and previously
#      failed deep inside the mirai daemon with a cryptic message
#      ("process_cell_type_info error: need a minimum of 25 cells...") —
#      the user only discovered this via the R console, not the app UI. Now
#      surfaced as: (a) a live per-cell-type count table + warning as soon as
#      a cell-type column is picked, (b) an opt-in "merge rare types into
#      'Autre'" checkbox (default ON) applied before saving the trimmed
#      reference, and (c) a hard pre-flight check right before invoking RCTD
#      that blocks with a clear, actionable French message instead of
#      dispatching a doomed daemon task. Label Transfer has no such hard
#      minimum (FindTransferAnchors/TransferData tolerate small classes,
#      just with noisier scores for them) so it is NOT blocked, only shown
#      the same informational table.
#
# v4 (reported timeout with allen_cortex.rds): Label Transfer gained a fast
# LogNormalize path (now the default) alongside SCTransform (vignette-exact,
# opt-in), an `ncells` control for SCTransform (the vignette's own speedup:
# learn the noise model on a subsample, still correct every cell), a
# glmGamPoi availability check logged into the task log (its absence is a
# major, easy-to-miss slowdown), and its OWN longer mirai timeout
# (LABEL_TRANSFER_TIMEOUT_MS, R/utils_spatial_async.R) instead of sharing
# the 20-minute ceiling used by every other spatial async task.
#
# v3 (Phase 3 — "Integration with single-cell data", vignette parity): added
# a THIRD mode, "labeltransfer" — Seurat's own anchor-based integration
# (FindTransferAnchors()/TransferData(), normalization.method="SCT"),
# exactly the workflow the Seurat Spatial Vignette recommends over pure
# deconvolution methods. NOT a replacement for RCTD (different method,
# vignette explicitly frames it as often-superior-but-different noise
# model) — a third choice. Reuses the SAME reference upload/prep pipeline
# already built for RCTD (trimmed to counts + cell_types, see below) and
# writes its result into shared_rv$deconv_props using the EXACT SAME
# contract as RCTD/STdeconvolve (data.frame: id + one column per class) —
# every downstream reader (bar plot, table, viz "Type cellulaire
# (deconvolution)" color-by) works completely unchanged.
#
# REWRITE (post-test-3): both backends were found to spawn NESTED parallel
# worker processes from inside the mirai daemon:
#   - STdeconvolve::fitLDA() hardcodes BiocParallel::SnowParam(workers=ncores)
#     internally (not overridable via any public argument) -> now bypassed
#     entirely: we call topicmodels::LDA() + topicmodels::posterior() directly
#     (exactly what fitLDA()/getBetaTheta() do under the hood for a single K,
#     verified against STdeconvolve source), no BiocParallel involved at all.
#   - spacexr::create.RCTD() only opens a parallel::makeCluster() when
#     max_cores > 1 (verified against spacexr source) -> now hardcoded to 1.
# Both are fragile in general and were observed to hang indefinitely on a
# Windows project path containing spaces/brackets. A MIRAI_TASK_TIMEOUT_MS
# ceiling (R/utils_spatial_async.R) is also a last-resort safety net.
#
# Install:
#   remotes::install_github("dmcable/spacexr")                        # RCTD
#   install.packages(c("STdeconvolve", "topicmodels", "slam"))        # or via Bioc/GitHub
#   remotes::install_github("cellgeni/schard")            # lecture .h5ad robuste (reference)
#   (Label Transfer needs no extra package: FindTransferAnchors()/
#   TransferData()/SCTransform() all ship with Seurat itself; optionally
#   install Bioconductor's glmGamPoi for a faster SCTransform.)
#
# Hard rule compliance: the (optional) scRNA-seq reference is never shipped
# into the daemon as a live Seurat object either — it is trimmed to
# (counts, cell_types) and written to its own small .rds on disk once, on
# upload; only that path travels into mirai::mirai(). SCTransform() is
# additionally forced onto a local `future::plan("sequential")` inside the
# daemon before being called (same defensive pattern as
# R/utils_spatial_io.R::build_sketch(), which fixed a real ambient-plan
# crash in the main Shiny process) — cheap insurance even though mirai
# daemons do not inherit the app's plan(multisession) by default.
# =============================================================================

# RCTD's own hard minimum (spacexr::process_cell_type_info() ->
# CELL_MIN_INSTANCE) — mirrored here (not importable, spacexr has no
# exported constant) so the app can warn/block BEFORE dispatching a doomed
# daemon task, using the exact number spacexr itself enforces.
RCTD_CELL_MIN_INSTANCE <- 25L

# Minimum shared reference/query genes below which FindTransferAnchors()
# either fails outright or produces meaningless anchors (near-zero overlap
# most often means mismatched gene-ID conventions between reference and
# spatial data -- symbols vs Ensembl IDs, or human vs mouse casing). Not a
# Seurat-imposed constant (unlike RCTD_CELL_MIN_INSTANCE) -- a conservative
# app-level guard to fail clearly instead of letting FindTransferAnchors()
# either error cryptically or silently return near-random anchors.
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
      #    Accepts anything helpers_io.R::load_single_cell_data() supports
      #    (Phase 5: .rds Seurat objects, but also .h5ad/.h5/.loom now that
      #    the reference importer routes through that shared helper). ──
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

    # ── Consolidated reference state (Chantier 2 refonte) ──────────────────
    # ONE reactiveValues, ONE lifecycle. `status` is the single source of
    # truth for both the persistent badge and the dispatch guard:
    #   "empty"   -> nothing uploaded yet
    #   "loading" -> a stage-1..4 pipeline run is currently in flight
    #   "error"   -> the last attempt failed; $message explains why. obj/
    #                staged_path are ALWAYS cleared on error (no stale
    #                success can survive a failed re-upload/recheck).
    #   "loaded"  -> obj/staged_path are valid; ref_path() (the small,
    #                trimmed file the daemon actually reads) may still be
    #                NULL for a few hundred ms while the observe() block
    #                below reacts to the freshly-rendered celltype_col
    #                input -- see reference_is_ready().
    ref_state <- reactiveValues(
      obj = NULL, staged_path = NULL, orig_ext = NULL,
      status = "empty", message = NULL
    )
    ref_path      <- reactiveVal(NULL)
    ref_meta_cols <- reactiveVal(character(0))

    # Package-version suffix appended to error notifications that look like
    # the known SeuratObject v5 "GetAssayData()/slot defunct" class of
    # error -- most often caused by a THIRD-PARTY reader (schard,
    # SeuratDisk...) internally still calling the old pre-5.0 API against a
    # SeuratObject build where `slot=` was fully removed (not just
    # deprecated-with-warning). Surfacing versions inline means the next
    # failure is diagnosable from the notification alone, no console access
    # needed.
    .version_hint <- function(msg) {
      if (!grepl("GetAssayData|slot.*defunct|defunct.*slot", msg, ignore.case = TRUE)) return("")
      sprintf(" [Seurat %s | SeuratObject %s | schard %s | SeuratDisk %s]",
              tryCatch(as.character(utils::packageVersion("Seurat")), error = function(e) "?"),
              tryCatch(as.character(utils::packageVersion("SeuratObject")), error = function(e) "?"),
              tryCatch(as.character(utils::packageVersion("schard")), error = function(e) "absent"),
              tryCatch(as.character(utils::packageVersion("SeuratDisk")), error = function(e) "absent"))
    }

    # ── Reference parse/convert pipeline, factored out so it can run from
    # EITHER a fresh upload OR the "Revérifier la reference" button without
    # duplicating the 4 stages. Always starts by RESETTING ref_state to a
    # clean "loading" slate -- a failed run can never leave a PREVIOUS
    # successful obj/staged_path in place looking falsely ready.
    run_reference_pipeline <- function(staged_path, orig_ext) {
      ref_state$obj         <- NULL
      ref_state$staged_path <- NULL
      ref_state$status      <- "loading"
      ref_state$message     <- NULL
      ref_meta_cols(character(0))
      ref_path(NULL)

      withProgress(message = "Preparation de la reference...", value = 0, {
        incProgress(0.15, detail = sprintf("Lecture (.%s)...", if (nzchar(orig_ext)) orig_ext else "?"))
        raw_ref <- tryCatch(load_single_cell_data(staged_path), error = function(e) {
          msg <- conditionMessage(e)
          ref_state$status  <- "error"
          ref_state$message <- sprintf("Lecture (.%s) : %s%s", orig_ext, msg, .version_hint(msg))
          showNotification(paste("Reference — erreur de lecture :", msg), type = "error", duration = 15)
          NULL
        })
        if (is.null(raw_ref)) return(invisible(FALSE))

        incProgress(0.4, detail = "Conversion en objet Seurat...")
        ref_obj <- tryCatch(prepare_seurat_object(raw_ref, project_name = "Reference"), error = function(e) {
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

        # Single atomic success update -- obj/staged_path/status always set
        # TOGETHER, never partially.
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
      # Re-stage upload under its ORIGINAL extension. Shiny's upload
      # datapath is not guaranteed to carry it in every version, and
      # load_single_cell_data() dispatches on tools::file_ext() — cheap
      # (one file.copy(), no in-RAM duplication of the reference itself).
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

    # ── Single source of truth: "can we dispatch RCTD/Label Transfer?" ────
    # Used by BOTH the persistent badge and the click-time guard, so the two
    # can never disagree.
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
      # FIX (Phase 5, real-world reference had 45 metadata columns): sort
      # candidates by descending level count and drop columns that cannot
      # plausibly be a cell-type annotation -- <2 levels (e.g. "orig.ident",
      # everything the same value) or >200 levels (e.g. a continuous score
      # or a near-per-cell ID) -- so a genuinely useful, fine-grained
      # annotation surfaces FIRST instead of the alphabetically/positionally
      # first column (which defaulted to "orig.ident" == 1 level == an
      # instant RCTD failure in practice). Never leaves the selector empty:
      # falls back to the unfiltered list if nothing passes the heuristic.
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

    # ── Live per-cell-type counts: informs the merge-rare-types checkbox
    # and surfaces RCTD's 25-cell minimum BEFORE the user hits "Lancer" ────
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

    # Builds/updates the trimmed reference saved to disk — reacts to BOTH
    # the column choice and the merge-rare toggle (plain observe() tracks
    # every reactive read in its body, no need to list dependencies
    # explicitly). Every dependency here is a REAL reactiveValues field
    # (ref_state$obj / ref_state$staged_path).
    #
    # Saves only the TINY (staged_path, cell_types) pair -- NEVER the counts
    # matrix itself on the main Shiny thread (that used to take ~34s / ~211
    # Mo on a realistic reference, a full UI freeze). The daemon re-reads
    # and re-extracts counts itself, via the same
    # load_single_cell_data()/prepare_seurat_object() helpers used at
    # upload time (helpers_io.R + helpers_sc.R — preloaded on every daemon,
    # see R/utils_spatial_async.R's source_files, fixed in this refonte).
    observe({
      req(ref_state$obj, ref_state$staged_path, input$ref_celltype_col)
      ref_obj <- ref_state$obj
      tryCatch({
        cell_types_raw <- as.character(ref_obj@meta.data[[input$ref_celltype_col]])
        names(cell_types_raw) <- colnames(ref_obj)

        if (isTRUE(input$merge_rare_types)) {
          tab <- table(cell_types_raw)
          rare_types <- names(tab)[tab < RCTD_CELL_MIN_INSTANCE]
          if (length(rare_types) > 0) {
            cell_types_raw[cell_types_raw %in% rare_types] <- "Autre"
          }
        }
        cell_types <- factor(cell_types_raw)

        tmp <- tempfile(fileext = ".rds")
        saveRDS(list(staged_path = ref_state$staged_path,
                     cell_types  = cell_types), tmp)
        ref_path(tmp)
      }, error = function(e) {
        showNotification(paste("Erreur preparation reference:", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    # ── Async deconvolution/integration task (mode chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells, log_file) {
      mirai::mirai(
        {
          # Wraps a load_single_cell_data()/prepare_seurat_object() reload
          # in a clear, named failure -- if either function is undefined
          # (the exact bug this refonte fixes in R/utils_spatial_async.R),
          # this now names the missing function explicitly in the log
          # instead of leaving a bare, unexplained daemon crash.
          .reload_reference <- function(ref_path_local) {
            ref_meta <- tryCatch(readRDS(ref_path_local), error = function(e) {
              stop("Lecture du fichier de reference trimme (", ref_path_local, ") impossible : ",
                   conditionMessage(e))
            })
            if (!exists("load_single_cell_data", mode = "function")) {
              stop("Fonction 'load_single_cell_data' introuvable dans ce daemon -- ",
                   "helpers_io.R/helpers_sc.R ne sont pas (ou plus) preloades ",
                   "(voir R/utils_spatial_async.R::init_spatial_daemons(), argument source_files).")
            }
            if (!exists("prepare_seurat_object", mode = "function")) {
              stop("Fonction 'prepare_seurat_object' introuvable dans ce daemon -- ",
                   "helpers_io.R/helpers_sc.R ne sont pas (ou plus) preloades ",
                   "(voir R/utils_spatial_async.R::init_spatial_daemons(), argument source_files).")
            }
            raw_ref <- tryCatch(load_single_cell_data(ref_meta$staged_path), error = function(e) {
              stop("load_single_cell_data() a echoue sur '", ref_meta$staged_path, "' : ",
                   conditionMessage(e))
            })
            ref_seurat <- tryCatch(prepare_seurat_object(raw_ref, project_name = "Reference"), error = function(e) {
              stop("prepare_seurat_object() a echoue : ", conditionMessage(e))
            })
            list(seurat = ref_seurat, cell_types = ref_meta$cell_types)
          }

          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
          coords_df <- coords[match(colnames(mat), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(mat)
          keep <- stats::complete.cases(coords_df)
          mat <- mat[, keep, drop = FALSE]
          coords_df <- coords_df[keep, , drop = FALSE]

          # Defensive: SCTransform() can pull in an ambient future::plan()
          # (same class of bug already fixed in
          # R/utils_spatial_io.R::build_sketch(), which runs in the MAIN
          # Shiny process where plan(multisession) IS active). A mirai
          # daemon is a fresh R process and does not inherit that plan, but
          # forcing sequential here is cheap insurance against any package
          # internally setting one — kept for consistency with the rest of
          # this project's SCTransform call sites. `ncells` mirrors the
          # Seurat Spatial Vignette's own speedup for large references
          # (e.g. allen_cortex.rds): learn the regularized-NB noise model on
          # a subsample, still normalize/correct every cell.
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
            write_mirai_log(log_file, "Chargement de la reference scRNA-seq (depuis le fichier stage)...", 2, 5)
            reloaded    <- .reload_reference(ref_path)
            ref_seurat  <- reloaded$seurat
            ref_counts  <- SeuratObject::LayerData(ref_seurat, layer = "counts")
            write_mirai_log(log_file, sprintf("Reference relue : %d cellules x %d genes.",
                                               ncol(ref_seurat), nrow(ref_seurat)), 2, 5)
            reference   <- spacexr::Reference(counts = ref_counts, cell_types = reloaded$cell_types)

            write_mirai_log(log_file, "Construction du 'puck' spatial (SpatialRNA)...", 3, 5)
            counts_dense <- methods::as(mat, "dgCMatrix")  # small: already QC-subset, single technical materialization
            puck <- spacexr::SpatialRNA(coords = coords_df, counts = counts_dense)

            write_mirai_log(log_file, "RCTD (mode full, mono-coeur — pas de sous-processus imbrique)...", 4, 5)
            # FIX: max_cores > 1 makes spacexr open its own parallel::makeCluster()
            # from inside this daemon (verified against spacexr source) — fragile,
            # observed to hang on Windows paths with spaces. Force max_cores=1.
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

            reloaded <- .reload_reference(ref_path)
            ref_obj  <- reloaded$seurat
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
            # npcs deliberately generous (50, the UI's own max for lt_npcs)
            # rather than tied to the query's n_pc computed below: this
            # RunPCA() result is NOT what FindTransferAnchors() actually
            # uses for its own internal reference PCA (it recomputes one
            # itself unless `reference.reduction` is passed, which we don't
            # do -- verified against Seurat's own FindTransferAnchors()
            # docs). Kept mainly so ref_obj carries a usable ["pca"] for any
            # future diagnostic/plot; a fixed generous ceiling avoids ever
            # being the accidental bottleneck if that assumption changes.
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
            # means mismatched ID conventions (symbols vs Ensembl, human vs
            # mouse) -- FindTransferAnchors() either errors cryptically or
            # silently returns near-meaningless anchors in that case. Fail
            # clearly instead, BEFORE the expensive anchor search.
            shared_genes <- intersect(rownames(ref_obj), rownames(query))
            if (length(shared_genes) < LABEL_TRANSFER_MIN_SHARED_GENES) {
              stop(sprintf(
                paste0(
                  "Seulement %d gene(s) commun(s) entre la reference et les donnees spatiales ",
                  "(minimum requis : %d). Causes probables : conventions d'identifiants differentes ",
                  "(symboles vs Ensembl) ou organismes differents (humain/souris) entre la reference ",
                  "et les donnees spatiales. Verifiez rownames(reference) vs rownames(objet spatial)."
                ),
                length(shared_genes), LABEL_TRANSFER_MIN_SHARED_GENES
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
            # weight.reduction = query's OWN PCA (not the default anchor-space
            # "pcaproject") -- this is exactly what the Seurat Spatial
            # Vignette's "Integration with single-cell data" section does for
            # spot-resolution data (`weight.reduction = cortex[["pca"]]`).
            predictions <- Seurat::TransferData(
              anchorset = anchors, refdata = ref_obj$cell_type,
              prediction.assay = TRUE,
              weight.reduction = query[["pca"]], dims = seq_len(n_pc)
            )

            write_mirai_log(log_file, "Termine.", 5, 5)
            pred_mat <- t(as.matrix(SeuratObject::LayerData(predictions, layer = "data")))
            # TransferData() appends a "max" feature (top prediction score per
            # spot, see Seurat::GetTransferPredictions()) — not a cell type,
            # drop it so every column of the result is a genuine class.
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
            # FIX (post-test-4): estimate.alpha=TRUE (topicmodels' default)
            # runs a nested Newton-Raphson alpha optimization with no
            # effective iteration cap -- on noisy spatial count data this
            # can fail to converge for a very long time. A stuck native
            # (C++) computation inside a mirai daemon CANNOT be interrupted
            # from the outside (confirmed: neither .timeout nor "Reinitialiser
            # les daemons" helps once it's running -- reap() closes the
            # connection but does not kill a busy process). The real fix is
            # to bound the computation itself: fixed alpha (standard 50/k
            # heuristic, no estimation) + hard iteration caps.
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
            # Same filtering as STdeconvolve::getBetaTheta()/filterTheta() (not
            # exported, reimplemented inline): drop near-zero contributions,
            # renormalize each spot's proportions back to 1.
            theta[theta < 0.05] <- 0
            theta <- theta / rowSums(theta)
            theta[is.na(theta)] <- 0

            # LDA topics are NUMBERED, not biologically named (no reference
            # was used) -- label each with its top-3 marker genes (highest
            # P(gene|topic) in `beta`) so "topic 3" becomes something a
            # biologist can actually read, e.g. "T3_Gfap.Aqp4.Mbp" -- the
            # same marker-gene convention STdeconvolve's own annotation
            # workflow uses. Renaming theta's columns directly means every
            # downstream consumer (viz color-by dropdown, bar plot legend,
            # DT table headers) inherits the readable label for free, with
            # no other file needing to know about this.
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
        log_file = log_file,
        # FIX (reported timeout with allen_cortex.rds): Label Transfer runs
        # up to two SCTransform passes + FindTransferAnchors, genuinely
        # heavier than the other spatial async tasks -- give it its own
        # longer ceiling (see R/utils_spatial_async.R) instead of raising
        # the shared one for every task.
        .timeout = if (identical(mode, "labeltransfer")) LABEL_TRANSFER_TIMEOUT_MS else MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(deconv_task, "btn_deconv")

    observeEvent(input$btn_deconv, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)

      # Single source of truth (reference_is_ready()) -- if it's FALSE,
      # ref_state$status/$message already say exactly why (also shown live
      # in the persistent badge above, not just here at click time).
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

      # FIX (Phase 5): RCTD hard-fails deep inside the daemon if any cell
      # type has < 25 cells (spacexr::process_cell_type_info()) — check
      # BEFORE dispatching so the user gets an immediate, actionable
      # message in the app instead of a cryptic error only visible in the
      # R console (exactly what was reported). Label Transfer has no such
      # hard minimum, so it is not blocked here.
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
        bpcells_dir    = global_data$spatial_obj$bpcells_dir,
        pass_idx       = shared_rv$qc_pass_idx,
        coords         = global_data$spatial_obj$coords,
        mode           = input$mode,
        ref_path       = ref_path(),
        n_topics       = input$n_topics,
        n_top_od       = input$n_top_od %||% 1000,
        lt_npcs        = input$lt_npcs %||% 30,
        lt_norm_method = input$lt_norm_method %||% "lognorm",
        lt_ncells      = input$lt_ncells %||% 3000,
        log_file       = log_file
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
