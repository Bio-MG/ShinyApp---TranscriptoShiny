# =============================================================================
# mod_sc_velocity.R — RNA Velocity Phase 3A Hardening
# =============================================================================
# Strict RDS/MTX import, matrix validation, orientation, cell/gene alignment,
# precomputed UMAP vector validation, phase portraits, provenance export,
# reset when Seurat object changes. No inference, no loom/h5ad, no projection.
# =============================================================================

mod_sc_velocity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.9em;border-left:3px solid #2980B9;",
        i18n$t("RNA Velocity — import strict des matrices spliced/unspliced alignees sur l'objet Seurat. "),
        i18n$t("Aucune inference n'est effectuee dans l'application.")),

    radioButtons(ns("velocity_import_mode"), i18n$t("Source d'import"),
                  choices = setNames(c("rds", "mtx"),
                              c(.tr_plain("RDS combine (spliced + unspliced)"),
                                .tr_plain("Matrix Market (MTX + barcodes + features)"))),
                  selected = "rds"),

    conditionalPanel(
      condition = "input.velocity_import_mode == 'rds'", ns = ns,
      fileInput(ns("velocity_rds_file"), i18n$t("Fichier RDS velocity (liste spliced/unspliced)"),
                accept = c(".rds", ".RDS"), width = "100%"),
      div(class = "small text-muted mb-2",
           i18n$t("Le RDS doit être une liste nommée avec spliced et unspliced. Noms inattendus bloqués."))
    ),

    conditionalPanel(
      condition = "input.velocity_import_mode == 'mtx'", ns = ns,
      h6(i18n$t("Matrice spliced (MTX)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_spliced"), i18n$t("Fichier .mtx (spliced)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_spliced_barcodes"), i18n$t("Barcodes spliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_spliced_features"), i18n$t("Features/genes spliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      h6(i18n$t("Matrice unspliced (MTX)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_unspliced"), i18n$t("Fichier .mtx (unspliced)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_unspliced_barcodes"), i18n$t("Barcodes unspliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_unspliced_features"), i18n$t("Features/genes unspliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      h6(i18n$t("Matrice ambiguous (optionnel)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_ambiguous"), i18n$t("Fichier .mtx (ambiguous)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_ambiguous_barcodes"), i18n$t("Barcodes ambiguous"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_ambiguous_features"), i18n$t("Features/genes ambiguous"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      numericInput(ns("velocity_feature_column"),
                    i18n$t("Colonne d'identifiants dans features.tsv"),
                    value = 1, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
           HTML(paste(i18n$t("Les fichiers 10x contiennent souvent gene_id (colonne 1) ET symbole "),
                 i18n$t("(colonne 2). La colonne choisie doit correspondre aux rownames de "),
                 i18n$t("l'objet Seurat — la premiere colonne n'est jamais supposee correcte."))))
    ),

    hr(),
    selectInput(
      ns("velocity_orientation"),
      i18n$t("Orientation des matrices"),
      choices = setNames(c("auto_strict", "genes_x_cells", "cells_x_genes"),
        c(.tr_plain("Détection automatique stricte"),
          .tr_plain("Genes x cellules"),
          .tr_plain("Cellules x genes"))),
      selected = "auto_strict"
    ),
    checkboxInput(
      ns("velocity_strip_cell_suffix"),
      i18n$t("Retirer les suffixes de barcodes (ex. -1, -2)"),
      value = FALSE
    ),
    checkboxInput(
      ns("velocity_strip_gene_version"),
      i18n$t("Retirer les suffixes de version Ensembl (ex. .1, .2)"),
      value = FALSE
    ),
    checkboxInput(
      ns("velocity_allow_low_overlap"),
      i18n$t("Forcer l'alignement malgre un recouvrement < 80%"),
      value = FALSE
    ),
    div(
      class = "small text-muted",
      i18n$t("Les corrections d'identifiants sont desactivees par defaut. "),
      " ",
      i18n$t("Une collision bloque la validation.")
    ),
    hr(),
    actionButton(ns("velocity_validate"), i18n$t("Valider les matrices velocity"),
                 class = "btn-primary w-100", icon = icon("check")),
    div(class = "small text-muted mt-1", textOutput(ns("velocity_status"))),
    hr(),
    selectInput(ns("velocity_gene"), i18n$t("Gene pour phase portrait"), choices = character(0)),
    selectInput(ns("velocity_reduction"), i18n$t("Reduction pour vecteurs"), choices = c("umap", "pca")),
    downloadButton(ns("dl_velocity_provenance"), i18n$t("Exporter provenance (CSV)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_velocity_output_ui <- function(id) {
  ns <- NS(id)
    card(
    full_screen = TRUE,
    card_header(i18n$t("RNA Velocity — Phase portrait & vecteurs pre-calcules")),
    navset_tab(
      nav_panel(i18n$t("Phase portrait"),
                plotOutput(ns("velocity_phase_plot"), height = "500px")),
      nav_panel(i18n$t("Vecteurs UMAP"),
                plotOutput(ns("velocity_embedding_plot"), height = "550px"),
                div(class = "small text-muted", textOutput(ns("velocity_vector_status"))))
    )
  )
}

mod_sc_velocity_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }



    velocity_state <- reactiveValues(
      inputs = NULL,
      validated = NULL,
      result = NULL,
      alignment = NULL,
      umap_vectors = NULL,
      partial_embedding = NULL,
      embedding_alignment = NULL,
      vector_validation = NULL,
      plot_cache = list(),
      export_cache = list(),
      object_fingerprint = NULL
    )

    observeEvent(global_data$language, {
      updateRadioButtons(session, "velocity_import_mode",
        label = .tr("Source d'import"),
        choices = setNames(
          c("rds", "mtx"),
          c(.tr("RDS combine (spliced + unspliced)"), .tr("Matrix Market (MTX + barcodes + features)"))
        ),
        selected = isolate(input$velocity_import_mode) %||% "rds"
      )
      updateSelectInput(session, "velocity_orientation",
        label = .tr("Orientation des matrices"),
        choices = setNames(
          c("auto_strict", "genes_x_cells", "cells_x_genes"),
          c(.tr("Détection automatique stricte"), .tr("Genes x cellules"), .tr("Cellules x genes"))
        ),
        selected = isolate(input$velocity_orientation) %||% "auto_strict"
      )
      updateCheckboxInput(session, "velocity_strip_cell_suffix", label = .tr("Retirer les suffixes de barcodes (ex. -1, -2)"))
      updateCheckboxInput(session, "velocity_strip_gene_version", label = .tr("Retirer les suffixes de version Ensembl (ex. .1, .2)"))
      updateCheckboxInput(session, "velocity_allow_low_overlap", label = .tr("Forcer l'alignement malgre un recouvrement < 80%"))
      updateActionButton(session, "velocity_validate", label = .tr("Valider les matrices velocity"))
      updateSelectInput(session, "velocity_gene", label = .tr("Gene pour phase portrait"))
      updateSelectInput(session, "velocity_reduction", label = .tr("Reduction pour vecteurs"))
    }, ignoreInit = TRUE)
    velocity_status_rv <- reactiveVal(.tr("En attente d'import velocity..."))

    output$velocity_status <- renderText({ velocity_status_rv() })
    output$velocity_vector_status <- renderText({
      parts <- character(0)
      ali <- velocity_state$embedding_alignment
      if (!is.null(ali)) {
        parts <- c(parts, sprintf(
          "Coordonnees UMAP : %d/%d cellules couvertes (%d manquantes).",
          ali$n_embedding_matched, ali$n_velocity_cells, ali$n_embedding_missing
        ))
      }
      if (!is.null(velocity_state$umap_vectors)) {
        parts <- c(parts, sprintf(
          "%d vecteurs valides (%s).",
          nrow(velocity_state$umap_vectors),
          velocity_state$vector_validation$field %||% "?"
        ))
      } else {
        parts <- c(parts,
          "Aucun vecteur velocity pre-calcule valide — fleches desactivees.")
      }
      paste(parts, collapse = " ")
    })

    # Keep reduction choices in sync with Seurat object
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "velocity_reduction", choices = character(0))
        return()
      }
      reds <- names(obj@reductions)
      if (!length(reds)) reds <- c("umap", "pca")
      updateSelectInput(session, "velocity_reduction",
                        choices = reds,
                        selected = if ("umap" %in% reds) "umap" else reds[1])
    }, ignoreInit = TRUE)

    # Reset when Seurat object changes (BUG 9) — no velocity state may
    # survive from a previous Seurat object.
    observeEvent(global_data$sc_obj, {
      velocity_state$inputs <- NULL
      velocity_state$validated <- NULL
      velocity_state$result <- NULL
      velocity_state$alignment <- NULL
      velocity_state$umap_vectors <- NULL
      velocity_state$partial_embedding <- NULL
      velocity_state$embedding_alignment <- NULL
      velocity_state$vector_validation <- NULL
      velocity_state$plot_cache <- list()
      velocity_state$export_cache <- list()
      velocity_state$object_fingerprint <- NULL

      velocity_status_rv(
        .tr("Objet Seurat modifie : reimportez et validez les donnees velocity.")
      )

      updateSelectInput(
        session,
        "velocity_gene",
        choices = character(0),
        selected = character(0)
      )

      updateCheckboxInput(
        session,
        "velocity_strip_cell_suffix",
        value = FALSE
      )

      updateCheckboxInput(
        session,
        "velocity_strip_gene_version",
        value = FALSE
      )

      updateCheckboxInput(
        session,
        "velocity_allow_low_overlap",
        value = FALSE
      )

      updateSelectInput(
        session,
        "velocity_orientation",
        selected = "auto_strict"
      )
    }, ignoreInit = TRUE)

    # Helper to check fingerprint before plot/export
    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(velocity_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(velocity_state$object_fingerprint, current_fp)) {
        showNotification(
          .tr_plain("Les donnees RNA velocity ne correspondent plus a l'objet Seurat courant."),
          type = "error",
          duration = 8
        )
        shiny::validate(shiny::need(FALSE, .tr_plain("Les donnees RNA velocity ne correspondent plus a l'objet Seurat courant.")))
      }
      invisible(TRUE)
    }

    # Validation button (BUG 1-8, 10)
    observeEvent(input$velocity_validate, {
      req(global_data$sc_obj)

      tryCatch({
        # CHRYSALIS 2E : garde d'entree explicite — le req() ci-dessus laisse
        # l'observateur silencieux si l'objet est absent/non-Seurat ; l'assert
        # produit desormais un message FR clair dans le canal d'erreur.
        assert_seurat(global_data$sc_obj, context = "velocity")

        # 1. Require Seurat object already ensured by req
        seurat_cells <- colnames(global_data$sc_obj)
        seurat_genes <- rownames(global_data$sc_obj)

        # 2. Read selected RDS/MTX inputs
        spliced <- NULL
        unspliced <- NULL
        ambiguous <- NULL
        rds_meta <- list()
        vector_field <- NULL
        vector_matrix <- NULL

        mode <- input$velocity_import_mode %||% "rds"

        if (identical(mode, "rds")) {
          req(input$velocity_rds_file)
          rds_path <- input$velocity_rds_file$datapath
          # read_velocity_rds() enforces the whitelist AND validates metadata
          velocity_input <- read_velocity_rds(rds_path)
          spliced <- velocity_input$spliced
          unspliced <- velocity_input$unspliced
          ambiguous <- velocity_input$ambiguous %||% NULL
          rds_meta <- velocity_input
          # BUG 3/BUG 8: ONLY these fields are dx/dy vectors in embedding
          # space. umap_embedding is coordinates and is NEVER routed here.
          for (f in c("umap_velocity", "embedding_velocity", "umap_vectors", "vectors")) {
            if (!is.null(velocity_input[[f]])) {
              vector_field <- f
              vector_matrix <- velocity_input[[f]]
              break
            }
          }
        } else {
          # MTX mode: require both spliced and unspliced triplets
          req(input$velocity_mtx_spliced, input$velocity_mtx_spliced_barcodes, input$velocity_mtx_spliced_features,
              input$velocity_mtx_unspliced, input$velocity_mtx_unspliced_barcodes, input$velocity_mtx_unspliced_features)
          fc <- input$velocity_feature_column %||% 1L
          spliced <- read_velocity_mtx(
            matrix_path = input$velocity_mtx_spliced$datapath,
            barcode_path = input$velocity_mtx_spliced_barcodes$datapath,
            feature_path = input$velocity_mtx_spliced_features$datapath,
            feature_column = fc
          )
          unspliced <- read_velocity_mtx(
            matrix_path = input$velocity_mtx_unspliced$datapath,
            barcode_path = input$velocity_mtx_unspliced_barcodes$datapath,
            feature_path = input$velocity_mtx_unspliced_features$datapath,
            feature_column = fc
          )
          # optional ambiguous triplet (only when all three files provided)
          if (!is.null(input$velocity_mtx_ambiguous) &&
              !is.null(input$velocity_mtx_ambiguous_barcodes) &&
              !is.null(input$velocity_mtx_ambiguous_features)) {
            ambiguous <- read_velocity_mtx(
              matrix_path = input$velocity_mtx_ambiguous$datapath,
              barcode_path = input$velocity_mtx_ambiguous_barcodes$datapath,
              feature_path = input$velocity_mtx_ambiguous_features$datapath,
              feature_column = fc
            )
          }
        }

        # 3. Read orientation, 4. suffix/version, 5. low overlap confirmation (BUG 5/6)
        orientation <- input$velocity_orientation %||% "auto_strict"
        strip_cell_suffix <- isTRUE(input$velocity_strip_cell_suffix)
        strip_gene_version <- isTRUE(input$velocity_strip_gene_version)
        allow_low_overlap <- isTRUE(input$velocity_allow_low_overlap)

        # 6. Validate matrices, 7. Align by barcode and gene ID.
        # Normalization applies to BOTH sides (velocity + Seurat barcodes).
        validated <- validate_velocity_matrices(
          spliced = spliced,
          unspliced = unspliced,
          ambiguous = ambiguous,
          seurat_cells = seurat_cells,
          seurat_genes = seurat_genes,
          orientation = orientation,
          strip_cell_suffix = strip_cell_suffix,
          strip_gene_version = strip_gene_version,
          allow_low_overlap = allow_low_overlap
        )

        # Zero aligned cells makes every downstream view (phase portrait,
        # embedding, export) empty without any visible explanation.
        if (validated$n_cells_matched == 0L) {
          stop(
            "Aucune cellule alignee entre les matrices velocity et ",
            "l'objet Seurat courant. Verifiez que l'objet charge ",
            "correspond au meme run Cell Ranger et activez le retrait ",
            "des suffixes (-1) si necessaire."
          )
        }

        # BUG 10: preserve RDS metadata on the validated result (never in
        # Seurat meta.data — this stays module-local).
        validated$velocity_source <- rds_meta$velocity_source %||% "unknown"
        validated$velocity_method <- rds_meta$velocity_method %||% "precomputed"
        validated$input_orientation <- rds_meta$orientation %||% NA_character_
        validated$embedding_reduction <- rds_meta$embedding_reduction %||%
          rds_meta$stored_reduction %||%
          rds_meta$reduction %||% NA_character_
        validated$clusters <- rds_meta$clusters %||% NULL
        validated$umap_embedding <- rds_meta$umap_embedding %||% NULL

        # 8. Store validated result
        velocity_state$validated <- validated
        velocity_state$result <- validated
        velocity_state$alignment <- list(cells = validated$cell_names, genes = validated$gene_names)

        # Reset per-import derived state before repopulating
        velocity_state$partial_embedding <- NULL
        velocity_state$embedding_alignment <- NULL
        velocity_state$vector_validation <- NULL
        velocity_state$umap_vectors <- NULL

        sel_red <- input$velocity_reduction %||% "umap"

        # BUG 3/BUG 4: partial coordinate display via align_velocity_embedding()
        if (!is.null(validated$umap_embedding)) {
          ali <- tryCatch(
            align_velocity_embedding(
              embedding = validated$umap_embedding,
              cell_names = validated$cell_names,
              selected_reduction = sel_red,
              stored_reduction = validated$embedding_reduction
            ),
            error = function(e) {
              showNotification(paste("Embedding UMAP ignore :", conditionMessage(e)),
                               type = "warning", duration = 8)
              NULL
            }
          )
          if (!is.null(ali)) {
            velocity_state$partial_embedding <- ali$embedding
            velocity_state$embedding_alignment <- ali
            validated$embedding_alignment <- ali
          }
        }

        # BUG 8: strict vector validation — no arrows on any failure
        if (!is.null(vector_matrix)) {
          vec_stored_red <- validated$embedding_reduction
          if (!identical(vector_field, "umap_velocity") &&
              !identical(vector_field, "embedding_velocity") &&
              !identical(vector_field, "umap_vectors") &&
              !identical(vector_field, "vectors")) {
            vector_matrix <- NULL
          }
          if (!is.null(vector_matrix)) {
            tryCatch({
              velocity_state$umap_vectors <- validate_precomputed_velocity_vectors(
                vectors = vector_matrix,
                cell_names = validated$cell_names,
                selected_reduction = sel_red,
                stored_reduction = vec_stored_red
              )
              velocity_state$vector_validation <- list(field = vector_field, ok = TRUE)
            }, error = function(e) {
              velocity_state$umap_vectors <- NULL
              velocity_state$vector_validation <- list(field = vector_field, ok = FALSE,
                                                       error = conditionMessage(e))
              showNotification(paste("Vecteurs velocity refuses :", conditionMessage(e)),
                               type = "error", duration = 8)
            })
          }
        }

        # 9. Store current object fingerprint
        velocity_state$object_fingerprint <- velocity_object_fingerprint(global_data$sc_obj)

        # CHRYSALIS 2E : provenance PRODUITE a l'etape de validation (regle 7
        # AGENTS.md) — parametres deja disponibles uniquement ; consolidation
        # reservee au rapport (macro-step 4F).
        provenance_append(shared_rv, new_provenance_entry(
          analysis_id = "sc-velocity",
          method      = validated$velocity_method %||% "precomputed",
          parameters  = list(
            import_mode        = mode,
            orientation        = validated$orientation,
            strip_cell_suffix  = strip_cell_suffix,
            strip_gene_version = strip_gene_version,
            allow_low_overlap  = allow_low_overlap,
            n_cells_matched    = validated$n_cells_matched,
            n_genes_matched    = validated$n_genes_matched
          ),
          dataset    = global_data$sc_obj,
          cells_used = validated$n_cells_matched
        ))

        # 10. Populate gene selector
        gene_choices <- rownames(validated$spliced)
        updateSelectInput(session, "velocity_gene", choices = gene_choices, selected = gene_choices[1])

        # 11. French status summary (+ BUG 3 message when only coordinates exist)
        msg <- sprintf(
          paste0(
            "Validation OK : %d cellules, %d genes. ",
            "Orientation : %s. Alignement cellules : %s. ",
            "Alignement genes : %s."
          ),
          validated$n_cells_matched,
          validated$n_genes_matched,
          validated$orientation,
          validated$cell_match_mode,
          validated$gene_match_mode
        )
        if (is.null(vector_matrix) && !is.null(velocity_state$partial_embedding)) {
          msg <- paste(
            msg,
            "Coordonnees UMAP disponibles, mais aucun vecteur velocity pre-calcule n'est fourni.",
            sep = "\n"
          )
        }
        velocity_status_rv(msg)
        showNotification(.tr("Validation velocity reussie."), type = "message", duration = 4)

      }, error = function(e) {
        velocity_status_rv(
          paste(.tr("Erreur validation velocity :"), conditionMessage(e))
        )
        showNotification(
          paste(.tr("Erreur validation velocity :"), conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    # Phase portrait plot
    output$velocity_phase_plot <- renderPlot({
      req(velocity_state$result)
      req(input$velocity_gene)
      .check_fingerprint()
      if (ncol(velocity_state$result$spliced) == 0L) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = "Aucune cellule alignee : revalidez les donnees velocity.",
            size = 5, colour = "red") +
          ggplot2::theme_void())
      }
      tryCatch({
        plot_velocity_phase_portrait(velocity_state$result, input$velocity_gene)
      }, error = function(e) {
        ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = conditionMessage(e), size = 5, colour = "red") + ggplot2::theme_void()
      })
    })

    # Embedding plot (BUG 3/4/8): coordinates and arrows are separate layers.
    # Base coordinates: partial RDS umap_embedding if available, else the
    # Seurat reduction restricted to validated velocity cells. Arrows are
    # drawn ONLY from strictly validated dx/dy vectors; missing vectors or
    # failed validation => points only, never fabricated arrows.
    output$velocity_embedding_plot <- renderPlot({
      req(velocity_state$result)
      .check_fingerprint()
      red <- input$velocity_reduction %||% "umap"

      base_coords <- velocity_state$partial_embedding
      if (is.null(base_coords)) {
        if (!red %in% names(global_data$sc_obj@reductions)) {
          shiny::validate(shiny::need(
            FALSE,
            sprintf("Reduction '%s' non calculee sur l'objet Seurat.", red)
          ))
        }
        emb <- Seurat::Embeddings(global_data$sc_obj, reduction = red)
        common_cells <- intersect(velocity_state$result$cell_names, rownames(emb))
        if (!length(common_cells)) {
          shiny::validate(shiny::need(FALSE,
            "Aucune cellule velocity trouvee dans l'embedding Seurat."
          ))
        }
        base_coords <- emb[common_cells, seq_len(min(2L, ncol(emb))), drop = FALSE]
      }

      # velocity_state$umap_vectors is non-NULL ONLY after strict validation
      # passed (BUG 8); NULL here means no arrows at all.
      vectors <- velocity_state$umap_vectors

      tryCatch(
        plot_velocity_embedding(base_coords, vectors),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = conditionMessage(e), size = 5, colour = "red") +
            ggplot2::theme_void()
        }
      )
    })

    # Provenance export
    output$dl_velocity_provenance <- downloadHandler(
      filename = function() paste0("velocity_provenance_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        req(velocity_state$result)
        req(input$velocity_gene)
        .check_fingerprint()
        df <- build_velocity_provenance_export(
          velocity_result = velocity_state$result,
          gene = input$velocity_gene,
          selected_reduction = input$velocity_reduction %||% NA_character_
        )
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # Expose state for tests (optional)
    return(velocity_state)
  })
}
