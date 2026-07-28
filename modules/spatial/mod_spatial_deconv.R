# =============================================================================
# modules/spatial/mod_spatial_deconv.R — Cell-type Deconvolution / Label Transfer
# =============================================================================
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
      #    an annotated scRNA-seq reference; only the algorithm differs) ──
      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd' || input['%s'] == 'labeltransfer'", ns("mode"), ns("mode")),
        fileInput(ns("ref_file"), "Reference scRNA-seq (.rds, objet Seurat annote)",
                  accept = ".rds"),
        uiOutput(ns("ref_celltype_col_ui"))
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go. Execute ",
            "en mono-coeur (max_cores=1) pour eviter tout sous-processus imbrique. ",
            "Les colonnes du tableau resultat portent directement les noms de type ",
            "cellulaire de votre reference (ex: 'Astrocytes', 'Neurons_L4').")
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
              " SCTransform() est signifcativement plus lent sans le package 'glmGamPoi' ",
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
            "comme pour RCTD, les noms de type cellulaire de votre reference."),
        div(class = "alert alert-warning", style = "font-size:0.75rem;",
            bsicons::bs_icon("exclamation-triangle"),
            " Plus lourd que RCTD (deux normalisations + PCA + recherche d'ancres) — reservez ",
            "aux jeux de donnees raisonnables (quelques dizaines de milliers de spots ",
            "QC-filtres au maximum) ou reduisez d'abord via les seuils QC (onglet 1). ",
            "Delai etendu a 45 min pour cette methode specifiquement (au lieu de 20 min).")
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

    # ── Reference upload: trim to (counts, cell_types), write a small .rds ──
    # Shared by RCTD and Label Transfer — both consume the same trimmed
    # reference shape, just run different algorithms on it inside the daemon.
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

    # ── Async deconvolution/integration task (mode chosen at invoke time) ──
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells, log_file) {
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

            ref_data <- readRDS(ref_path)
            ref_obj  <- Seurat::CreateSeuratObject(counts = ref_data$counts)
            ref_obj$cell_type <- as.character(ref_data$cell_types)[match(colnames(ref_obj), names(ref_data$cell_types))]
            ref_obj  <- subset(ref_obj, cells = colnames(ref_obj)[!is.na(ref_obj$cell_type)])
            if (ncol(ref_obj) < 10) {
              stop("Reference trop petite apres filtrage des annotations manquantes (< 10 cellules annotees).")
            }

            if (use_sct) {
              ref_obj <- sctransform_sequential(ref_obj, ncells = lt_ncells)
            } else {
              ref_obj <- Seurat::NormalizeData(ref_obj, verbose = FALSE)
              ref_obj <- Seurat::FindVariableFeatures(ref_obj, verbose = FALSE)
              ref_obj <- Seurat::ScaleData(ref_obj, verbose = FALSE)
            }
            ref_obj <- Seurat::RunPCA(ref_obj, npcs = 30, verbose = FALSE)

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
      if (input$mode %in% c("rctd", "labeltransfer")) req(ref_path())
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
