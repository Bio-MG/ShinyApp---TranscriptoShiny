# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv.R — ORCHESTRATOR
# =============================================================================
# Phase 2 refactor: the former ~1008-line monolithic file is now split into:
#   mod_spatial_deconv_ui.R        — all UI builders
#   mod_spatial_deconv_reference.R — .deconv_reference_server()
#   mod_spatial_deconv_refviz.R    — .deconv_refviz_server()
#   mod_spatial_deconv_outputs.R   — .deconv_outputs_server()
#   This file                      — orchestrator (wires sub-servers, owns
#                                    the deconv task dispatch + result handling)
#
# The mirai deconv task body lives in R/utils_spatial_deconv_tasks.R
# (run_spatial_deconv_body), preloaded into every daemon.
#
# State contract (shared_rv):
#   WRITE : shared_rv$deconv_props, shared_rv$deconv_params
# =============================================================================

RCTD_CELL_MIN_INSTANCE <- 25L
LABEL_TRANSFER_MIN_SHARED_GENES <- 50L

mod_spatial_deconv_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Session-scoped scalar translation (plain strings, never HTML spans).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n: push translated labels/choices for build-time-frozen inputs on
    # every language change (values NEVER change; selection is preserved).
    # The bslib task buttons cannot be relabelled server-side
    # (update_task_button() has no label argument) — they keep their
    # client-side JS-shim translation from i18n$t() at build time.
    observeEvent(global_data$language, {
      updateRadioButtons(session, "mode",
        label = .tr("Methode"),
        choices = stats::setNames(
          c("rctd", "labeltransfer", "stdeconvolve"),
          c(.tr("Avec reference scRNA-seq (RCTD)"),
            .tr("Transfert d'ancres (Label Transfer, Seurat)"),
            .tr("Sans reference (LDA, type STdeconvolve)"))))
      updateNumericInput(session, "rctd_n_hvg",
                         label = .tr("Max genes (HVG) avant RCTD (RAM/vitesse)"))
      updateRadioButtons(session, "lt_norm_method",
        label = .tr("Normalisation"),
        choices = stats::setNames(
          c("lognorm", "sct"),
          c(.tr("LogNormalize (rapide, recommande CPU)"),
            .tr("SCTransform (vignette Seurat, plus lent)"))))
      updateNumericInput(session, "lt_ncells",
                         label = .tr("Cellules pour l'apprentissage SCTransform (ncells)"))
      updateNumericInput(session, "lt_npcs",
                         label = .tr("Composantes PCA (requete spatiale)"))
      updateNumericInput(session, "n_topics",
                         label = .tr("Nombre de types cellulaires (K)"))
      updateNumericInput(session, "n_top_od",
                         label = .tr("Genes surdisperses maximum (vitesse)"))
      updateSelectInput(session, "ref_viz_reduction", label = .tr("Reduction"))
      updateCheckboxInput(session, "ref_viz_interactive",
                          label = .tr("Vue interactive (Plotly)"))
    }, ignoreInit = TRUE)

    log_file <- spatial_log_path(session, "deconv")
    tracker  <- create_reactive_tracker(session, log_file)
    last_deconv_mode <- reactiveVal(NULL)

    # ── Sub-server 1: Reference state & preparation ───────────────────────
    ref <- .deconv_reference_server(input, output, session, ns, global_data, shared_rv)

    # ── Sub-server 2: Reference UMAP/PCA preview ──────────────────────────
    .deconv_refviz_server(
      input, output, session, ns, global_data, shared_rv,
      ref_state = ref$ref_state,
      input_ref_celltype_col = reactive(input$ref_celltype_col)
    )

    # ── Sub-server 3: Result outputs (bar plot, DT, colocalisation) ───────
    .deconv_outputs_server(input, output, session, ns, global_data, shared_rv)

    # ── Main deconvolution task (calls shared body) ───────────────────────
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells,
                                              min_shared_genes, rctd_n_hvg, log_file) {
      mirai::mirai(
        {
          run_spatial_deconv_body(
            bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
            mode = mode, ref_path = ref_path, n_topics = n_topics,
            n_top_od = n_top_od, lt_npcs = lt_npcs, lt_norm_method = lt_norm_method,
            lt_ncells = lt_ncells, min_shared_genes = min_shared_genes,
            rctd_n_hvg = rctd_n_hvg, log_file = log_file
          )
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od,
        lt_npcs = lt_npcs, lt_norm_method = lt_norm_method, lt_ncells = lt_ncells,
        min_shared_genes = min_shared_genes, rctd_n_hvg = rctd_n_hvg, log_file = log_file,
        .timeout = switch(mode,
                          "labeltransfer" = LABEL_TRANSFER_TIMEOUT_MS,
                          "rctd"          = RCTD_TIMEOUT_MS,
                          MIRAI_TASK_TIMEOUT_MS)
      )
    })
    bslib::bind_task_button(deconv_task, "btn_deconv")

    # ── Dispatch button ───────────────────────────────────────────────────
    observeEvent(input$btn_deconv, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)

      if (input$mode %in% c("rctd", "labeltransfer")) {
        if (!ref$reference_is_ready()) {
          reason <- if (ref$using_shared_ref()) {
            .tr("reference partagee introuvable ou invalide -- reimportez-la depuis l'onglet Import > Spatial")
          } else {
            switch(ref$ref_state$status,
              "empty"   = .tr("Aucune reference chargee (section 'Reference scRNA-seq')"),
              "loading" = .tr("chargement de la reference encore en cours -- patientez puis reessayez"),
              "error"   = sprintf(.tr("la reference a echoue au chargement (%s)"),
                                  ref$ref_state$message %||% .tr("cause inconnue")),
              "loaded"  = if (is.null(input$ref_celltype_col) || !nzchar(input$ref_celltype_col)) {
                .tr("aucune colonne 'type cellulaire' selectionnee")
              } else {
                .tr("reference pas encore preparee -- cliquez '1) Preparer la reference (disque)' puis reessayez")
              },
              .tr("cause inconnue")
            )
          }
          showNotification(paste(.tr("Chargez d'abord une reference scRNA-seq complete :"), reason),
                            type = "warning", duration = 14)
          return()
        }
      }

      if (identical(input$mode, "rctd")) {
        ref_check <- tryCatch(readRDS(ref$effective_ref_path()), error = function(e) NULL)
        if (is.null(ref_check)) {
          showNotification(.tr("Reference introuvable ou illisible — reimportez le fichier de reference."),
                            type = "error", duration = 8)
          return()
        }
        tab <- table(as.character(ref_check$cell_types))
        too_small <- names(tab)[tab < RCTD_CELL_MIN_INSTANCE]
        if (length(too_small) > 0) {
          showNotification(.t_fmt(
            .tr("RCTD necessite au moins {min_cells} cellules par type dans la reference. Type(s) insuffisant(s) : {bad_types}. "),
            min_cells = RCTD_CELL_MIN_INSTANCE,
            bad_types = paste(sprintf("%s (%d)", too_small, as.integer(tab[too_small])), collapse = ", ")
          ), type = "error", duration = 14)
          showNotification(
            if (ref$using_shared_ref()) {
              .tr(paste0("Repreparez la reference partagee depuis l'onglet Import > Spatial ",
                         "(case 'Fusionner/exclure les types rares') avant de relancer."))
            } else {
              .tr(paste0("Cochez 'Fusionner/exclure les types rares' (sidebar) puis cliquez a nouveau ",
                         "'1) Preparer la reference' avant de relancer."))
            },
            type = "warning", duration = 14)
          return()
        }
      }

      last_deconv_mode(input$mode)
      reset_log(log_file)
      shared_rv$deconv_params <- list(
        mode = input$mode,
        ref_path = if (input$mode %in% c("rctd", "labeltransfer")) ref$effective_ref_path() else NULL,
        ref_source_label = if (input$mode %in% c("rctd", "labeltransfer")) {
          if (ref$using_shared_ref()) "reference partagee (Import > Spatial)" else "upload local (session)"
        } else NULL,
        n_topics = input$n_topics, n_top_od = input$n_top_od,
        lt_npcs = input$lt_npcs, lt_norm_method = input$lt_norm_method, lt_ncells = input$lt_ncells,
        rctd_n_hvg = input$rctd_n_hvg %||% DECONV_DEFAULT_N_HVG
      )
      deconv_task$invoke(
        bpcells_dir      = global_data$spatial_obj$bpcells_dir,
        pass_idx         = shared_rv$qc_pass_idx,
        coords           = global_data$spatial_obj$coords,
        mode             = input$mode,
        ref_path         = ref$effective_ref_path(),
        n_topics         = input$n_topics,
        n_top_od         = input$n_top_od %||% 1000,
        lt_npcs          = input$lt_npcs %||% 30,
        lt_norm_method   = input$lt_norm_method %||% "lognorm",
        lt_ncells        = input$lt_ncells %||% 3000,
        min_shared_genes = LABEL_TRANSFER_MIN_SHARED_GENES,
        rctd_n_hvg       = input$rctd_n_hvg %||% DECONV_DEFAULT_N_HVG,
        log_file         = log_file
      )
    })

    # ── Task status handling ──────────────────────────────────────────────
    observeEvent(deconv_task$status(), {
      if (deconv_task$status() == "success") {
        shared_rv$deconv_props <- deconv_task$result()
        showNotification(.tr("Deconvolution terminee."), type = "message", duration = 5)
      } else if (deconv_task$status() == "error") {
        ceiling_txt <- switch(last_deconv_mode() %||% "?",
                              "labeltransfer" = "45 min",
                              "rctd"          = "40 min",
                              "20 min")
        showNotification(.t_fmt(
          .tr("Erreur (ou depassement du delai de {ceiling}) pendant la deconvolution — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez."),
          ceiling = ceiling_txt
        ), type = "error", duration = 12)
      }
    })

    output$deconv_progress_text <- renderText({
      global_data$language  # i18n: re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })
  })
}