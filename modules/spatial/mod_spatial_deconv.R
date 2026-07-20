# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution (RCTD / STdeconvolve)
# =============================================================================
# RCTD path uses the 'spacexr' package (verified against dmcable/spacexr
# source): Reference(counts, cell_types), SpatialRNA(coords, counts),
# create.RCTD(spatialRNA, reference), run.RCTD(RCTD, doublet_mode="full")
# -> RCTD@results$weights. Install: remotes::install_github("dmcable/spacexr").
#
# STdeconvolve path (reference-free, LDA-based cell "topics") is
# ASSUMPTION-heavy — the general shape (preprocess -> fitLDA -> optimalModel
# -> getBetaTheta) reflects the package's documented workflow, but exact
# argument names should be double-checked against the installed version
# before relying on it in production (see integration checklist).
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
                               "Sans reference (STdeconvolve, LDA)" = "stdeconvolve"),
                   selected = "rctd"),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        fileInput(ns("ref_file"), "Reference scRNA-seq (.rds, objet Seurat annote)",
                  accept = ".rds"),
        uiOutput(ns("ref_celltype_col_ui")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("mode")),
        numericInput(ns("n_topics"), "Nombre de types cellulaires (K)", 8, min = 2, max = 30, step = 1),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " Allocation de Dirichlet Latente (LDA) : extrait K 'themes' ",
            "d'expression purs sans reference externe.")
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
        # Stash the raw object path in a session-local temp file; the actual
        # (counts, cell_types) trim happens once a cell-type column is chosen
        # (see below) — we don't know which metadata column to use yet.
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

    # ── Async deconvolution task (RCTD or STdeconvolve, chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, log_file) {
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

            write_mirai_log(log_file, "RCTD (mode full, programmation quadratique)...", 4, 5)
            rctd <- spacexr::create.RCTD(puck, reference, max_cores = 2)
            rctd <- spacexr::run.RCTD(rctd, doublet_mode = "full")
            w <- as.matrix(rctd@results$weights)
            w <- sweep(w, 1, rowSums(w), "/")  # normalize to proportions

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(w), w, row.names = NULL, check.names = FALSE)

          } else {
            if (!requireNamespace("STdeconvolve", quietly = TRUE)) {
              stop("Package 'STdeconvolve' requis.")
            }
            write_mirai_log(log_file, "Pretraitement (STdeconvolve::preprocess)...", 2, 5)
            counts_dense <- methods::as(mat, "dgCMatrix")
            corpus <- STdeconvolve::restrictCorpus(counts_dense)

            write_mirai_log(log_file, sprintf("Ajustement LDA (K=%d)...", n_topics), 3, 5)
            lda_models <- STdeconvolve::fitLDA(t(as.matrix(corpus)), Ks = n_topics, verbose = FALSE)
            opt <- STdeconvolve::optimalModel(models = lda_models, opt = n_topics)

            write_mirai_log(log_file, "Extraction des proportions (theta)...", 4, 5)
            res <- STdeconvolve::getBetaTheta(opt)
            theta <- res$theta  # spots x topics proportions

            write_mirai_log(log_file, "Termine.", 5, 5)
            data.frame(id = rownames(theta), theta, row.names = NULL, check.names = FALSE)
          }
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, log_file = log_file
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
        log_file    = log_file
      )
    })

    observeEvent(deconv_task$status(), {
      if (deconv_task$status() == "success") {
        shared_rv$deconv_props <- deconv_task$result()
        showNotification("Deconvolution terminee.", type = "message", duration = 5)
      } else if (deconv_task$status() == "error") {
        showNotification("Erreur pendant la deconvolution — voir le log.", type = "error", duration = 8)
      }
    })

    output$deconv_progress_text <- renderText({
      p <- parse_log_progress(tracker())
      if (!is.na(p$pct)) sprintf("%s (%d%%)", p$text, p$pct) else p$text
    })

    output$deconv_bar_plot <- renderPlot({
      req(shared_rv$deconv_props)
      df <- shared_rv$deconv_props
      long <- reshape2::melt(df, id.vars = "id", variable.name = "cell_type", value.name = "proportion")
      # Show a manageable subset for the stacked bar (first 60 spots) — full
      # detail available in the table below / the spatial map (viz tab).
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
