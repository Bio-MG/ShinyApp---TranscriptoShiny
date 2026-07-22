# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution (RCTD / STdeconvolve)
# =============================================================================
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
#
# Hard rule compliance: the (optional) scRNA-seq reference is never shipped
# into the daemon as a live Seurat object either — it is trimmed to
# (counts, cell_types) and written to its own small .rds on disk once, on
# upload; only that path travels into mirai::mirai().
# =============================================================================

mod_spatial_deconv_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Deconvolution cellulaire", width = 380,

      radioButtons(ns("mode"), "Methode",
                   choices = c("Avec reference scRNA-seq (RCTD)" = "rctd",
                               "Sans reference (LDA, type STdeconvolve)" = "stdeconvolve"),
                   selected = "rctd"),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        fileInput(ns("ref_file"), "Reference scRNA-seq (.rds, objet Seurat annote)",
                  accept = ".rds"),
        uiOutput(ns("ref_celltype_col_ui")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go. Execute ",
            "en mono-coeur (max_cores=1) pour eviter tout sous-processus imbrique.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("mode")),
        numericInput(ns("n_topics"), "Nombre de types cellulaires (K)", 6, min = 2, max = 30, step = 1),
        numericInput(ns("n_top_od"), "Genes surdisperses maximum (vitesse)", 1000, min = 200, max = 3000, step = 100),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " Allocation de Dirichlet Latente (LDA) : extrait K 'themes' ",
            "d'expression purs sans reference externe. Reduisez K ou le ",
            "nombre de genes pour accelerer.")
      ),

      bslib::input_task_button(ns("btn_deconv"), "Lancer la deconvolution",
                                icon = icon("puzzle-piece")),
      verbatimTextOutput(ns("deconv_progress_text"), placeholder = TRUE)
    ),

    card(
      card_header("Proportions par spot/cellule"),
      plotOutput(ns("deconv_bar_plot"), height = "400px"),
      DT::DTOutput(ns("deconv_table"))
    )
  )
}

mod_spatial_deconv_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    log_file <- spatial_log_path(session, "deconv")
    tracker  <- create_reactive_tracker(session, log_file)

    # ── Reference upload: trim to (counts, cell_types), write a small .rds ──
    ref_path <- reactiveVal(NULL)
    ref_meta_cols <- reactiveVal(character(0))

    observeEvent(input$ref_file, {
      req(input$ref_file)
      tryCatch({
        ref_obj <- readRDS(input$ref_file$datapath)
        if (!inherits(ref_obj, "Seurat")) stop("Le fichier de reference doit contenir un objet Seurat.")
        ref_meta_cols(colnames(ref_obj@meta.data))
        session$userData$spatial_deconv_ref_obj <- ref_obj
      }, error = function(e) {
        showNotification(paste("Erreur reference:", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    output$ref_celltype_col_ui <- renderUI({
      req(length(ref_meta_cols()) > 0)
      selectInput(ns("ref_celltype_col"), "Colonne 'type cellulaire'", choices = ref_meta_cols())
    })

    observeEvent(input$ref_celltype_col, {
      req(session$userData$spatial_deconv_ref_obj, input$ref_celltype_col)
      ref_obj <- session$userData$spatial_deconv_ref_obj
      tryCatch({
        counts <- SeuratObject::LayerData(ref_obj, layer = "counts")
        cell_types <- factor(ref_obj@meta.data[[input$ref_celltype_col]])
        names(cell_types) <- colnames(ref_obj)
        tmp <- tempfile(fileext = ".rds")
        saveRDS(list(counts = counts, cell_types = cell_types), tmp)
        ref_path(tmp)
      }, error = function(e) {
        showNotification(paste("Erreur preparation reference:", conditionMessage(e)), type = "error", duration = 8)
      })
    })

    # ── Async deconvolution task (RCTD or direct-LDA, chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
          coords_df <- coords[match(colnames(mat), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(mat)
          keep <- stats::complete.cases(coords_df)
          mat <- mat[, keep, drop = FALSE]
          coords_df <- coords_df[keep, , drop = FALSE]

          if (identical(mode, "rctd")) {
            if (!requireNamespace("spacexr", quietly = TRUE)) {
              stop("Package 'spacexr' requis (remotes::install_github('dmcable/spacexr')).")
            }
            write_mirai_log(log_file, "Preparation de la reference scRNA-seq...", 2, 5)
            ref_data <- readRDS(ref_path)
            reference <- spacexr::Reference(counts = ref_data$counts, cell_types = ref_data$cell_types)

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
            # Same filtering as STdeconvolve::getBetaTheta()/filterTheta() (not
            # exported, reimplemented inline): drop near-zero contributions,
            # renormalize each spot's proportions back to 1.
            theta[theta < 0.05] <- 0
            theta <- theta / rowSums(theta)
            theta[is.na(theta)] <- 0

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(theta), theta, row.names = NULL, check.names = FALSE)
          }
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(deconv_task, "btn_deconv")

    observeEvent(input$btn_deconv, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      if (input$mode == "rctd") req(ref_path())
      reset_log(log_file)
      deconv_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = shared_rv$qc_pass_idx,
        coords      = global_data$spatial_obj$coords,
        mode        = input$mode,
        ref_path    = ref_path(),
        n_topics    = input$n_topics,
        n_top_od    = input$n_top_od %||% 1000,
        log_file    = log_file
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
        ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::labs(x = "Spots/cellules (echantillon)", y = "Proportion", fill = "Type")
    })

    output$deconv_table <- DT::renderDT({
      req(shared_rv$deconv_props)
      DT::datatable(shared_rv$deconv_props, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
        DT::formatRound(setdiff(colnames(shared_rv$deconv_props), "id"), 3)
    })
  })
}
