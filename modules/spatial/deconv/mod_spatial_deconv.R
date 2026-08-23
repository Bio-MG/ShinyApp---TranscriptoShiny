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

    log_file <- spatial_log_path(session, "deconv")
    tracker  <- create_reactive_tracker(session, log_file)
    last_deconv_mode <- reactiveVal(NULL)

    # ── Sub-server 1: Reference state & preparation ───────────────────────
    ref <- .deconv_reference_server(input, output, session, ns, global_data, shared_rv)

    # ── Sub-server 2: Reference UMAP/PCA preview ──────────────────────────
    .deconv_refviz_server(
      input, output, session, ns, global_data,
      ref_state = ref$ref_state,
      input_ref_celltype_col = reactive(input$ref_celltype_col)
    )

    # ── Sub-server 3: Result outputs (bar plot, DT, colocalisation) ───────
    .deconv_outputs_server(input, output, session, ns, shared_rv)

    # ── Main deconvolution task (calls shared body) ───────────────────────
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode,
                                              ref_path, n_topics, n_top_od,
                                              lt_npcs, lt_norm_method, lt_ncells,
                                              min_shared_genes, log_file) {
      mirai::mirai(
        {
          run_spatial_deconv_body(
            bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
            mode = mode, ref_path = ref_path, n_topics = n_topics,
            n_top_od = n_top_od, lt_npcs = lt_npcs, lt_norm_method = lt_norm_method,
            lt_ncells = lt_ncells, min_shared_genes = min_shared_genes,
            log_file = log_file
          )
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od,
        lt_npcs = lt_npcs, lt_norm_method = lt_norm_method, lt_ncells = lt_ncells,
        min_shared_genes = min_shared_genes, log_file = log_file,
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
            "reference partagee introuvable ou invalide -- reimportez-la depuis l'onglet Import > Spatial"
          } else {
            switch(ref$ref_state$status,
              "empty"   = "aucune reference chargee (section 'Reference scRNA-seq')",
              "loading" = "chargement de la reference encore en cours -- patientez puis reessayez",
              "error"   = sprintf("la reference a echoue au chargement (%s)", ref$ref_state$message %||% "cause inconnue"),
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
        ref_check <- tryCatch(readRDS(ref$effective_ref_path()), error = function(e) NULL)
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
            if (ref$using_shared_ref()) {
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
      shared_rv$deconv_params <- list(
        mode = input$mode,
        ref_path = if (input$mode %in% c("rctd", "labeltransfer")) ref$effective_ref_path() else NULL,
        ref_source_label = if (input$mode %in% c("rctd", "labeltransfer")) {
          if (ref$using_shared_ref()) "reference partagee (Import > Spatial)" else "upload local (session)"
        } else NULL,
        n_topics = input$n_topics, n_top_od = input$n_top_od,
        lt_npcs = input$lt_npcs, lt_norm_method = input$lt_norm_method, lt_ncells = input$lt_ncells
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
        log_file         = log_file
      )
    })

    # ── Task status handling ──────────────────────────────────────────────
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
  })
}