# =============================================================================
# mod_sc_da_design.R — Abondance differentielle 4E-0 : validation du plan
# experimental (Stage 13)
# =============================================================================
# Le module ORCHESTRE uniquement : la validation (validate_da_design), le
# resultat canonique (finalize_da_design_result), les verdicts d'eligibilite,
# la provenance et les exports residuent dans R/sc/sc_abundance_design.R.
# L'UI explique accepte / averti / bloque avec les raisons scientifiques.
# Principe affiche : les cellules ne sont PAS des replicats biologiques.
# =============================================================================

mod_sc_da_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #8E44AD;",
        i18n$t("Abondance différentielle — validation du plan expérimental AVANT tout calcul (Milo / scCODA). "),
        i18n$t("Les cellules ne sont PAS des réplicats biologiques : l'unité statistique est l'échantillon.")),

    selectInput(ns("da_sample_col"), i18n$t("Colonne ID échantillon (requis)"),
                choices = character(0), width = "100%"),
    selectInput(ns("da_condition_col"), i18n$t("Colonne condition (requis)"),
                choices = character(0), width = "100%"),
    selectInput(ns("da_replicate_col"), i18n$t("Colonne réplicat biologique"),
                choices = c("L'échantillon EST le réplicat" = ""),
                selected = "", width = "100%"),
    selectInput(ns("da_batch_col"), i18n$t("Colonne batch (optionnel)"),
                choices = c("(aucun)" = ""), selected = "", width = "100%"),
    selectInput(ns("da_identity_col"), i18n$t("Colonne identité / cluster (optionnel)"),
                choices = c("(aucune)" = ""), selected = "", width = "100%"),
    div(class = "small text-muted mb-2",
        i18n$t("L'équivalence « échantillon = réplicat » doit être déclarée explicitement ci-dessus — elle est tracée dans la provenance.")),
    actionButton(ns("da_validate"), i18n$t("Valider le design"),
                 class = "btn-primary w-100", icon = icon("clipboard-check")),
    div(class = "small text-muted mt-1", textOutput(ns("da_status"))),
    hr(),
    downloadButton(ns("dl_da_summary"), i18n$t("Exporter résumé du design (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_da_samples"), i18n$t("Exporter table échantillons (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_da_rds"), i18n$t("Exporter résultat (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_da_design_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Abondance différentielle — plan expérimental")),
    navset_tab(
      nav_panel(i18n$t("Verdict"),
                uiOutput(ns("da_verdict"))),
      nav_panel(i18n$t("Conditions"),
                DT::dataTableOutput(ns("da_conditions"), height = "420px")),
      nav_panel(i18n$t("Échantillons"),
                DT::dataTableOutput(ns("da_samples"), height = "420px")),
      nav_panel(i18n$t("Conditions × Batch"),
                uiOutput(ns("da_batch_ui"))),
      nav_panel(i18n$t("Couverture identités"),
                uiOutput(ns("da_identity_ui"))),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("Les cellules ne sont pas des réplicats biologiques : aucun résumé par cellule ne peut servir de test statistique.")),
                div(class = "alert alert-info m-3", style = "font-size:0.9em;",
                    i18n$t("Un design bloqué produit un rapport explicatif mais n'est JAMAIS consommable par Milo ou scCODA.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Seuils : 2 réplicats minimum par condition (bloc), 10 cellules par échantillon et 5 cellules par identité (avertissements) — config/defaults.R.")))
    )
  )
}

mod_sc_da_design_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    .status_label <- function(st) {
      lab <- da_design_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    da_state <- reactiveValues(result = NULL, object_fingerprint = NULL)
    da_status_rv <- reactiveVal(.tr("En attente de validation du design..."))

    output$da_status <- renderText({ da_status_rv() })

    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(da_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(da_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # UN SEUL observateur par declencheur (garde anti-duplication) : reset +
    # resynchronisation des choix de colonnes avec les metadonnees.
    observeEvent(global_data$sc_obj, {
      da_state$result <- NULL
      da_state$object_fingerprint <- NULL
      da_status_rv(.tr("Objet Seurat modifié : revalidez le design."))
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        for (s in c("da_sample_col", "da_condition_col"))
          updateSelectInput(session, s, choices = character(0))
        updateSelectInput(session, "da_replicate_col",
                          choices = setNames("", .tr("L'échantillon EST le réplicat")))
        updateSelectInput(session, "da_batch_col", choices = setNames("", .tr("(aucun)")))
        updateSelectInput(session, "da_identity_col", choices = setNames("", .tr("(aucune)")))
        return()
      }
      meta_cols <- colnames(obj@meta.data)
      updateSelectInput(session, "da_sample_col", choices = meta_cols,
                        selected = if ("sample_id" %in% meta_cols) "sample_id" else meta_cols[1])
      updateSelectInput(session, "da_condition_col", choices = meta_cols,
                        selected = if ("condition" %in% meta_cols) "condition" else meta_cols[min(2L, length(meta_cols))])
      updateSelectInput(session, "da_replicate_col",
                        choices = setNames(c("", meta_cols),
                                           c(.tr("L'échantillon EST le réplicat"), meta_cols)),
                        selected = if ("replicate_id" %in% meta_cols) "replicate_id" else "")
      updateSelectInput(session, "da_batch_col",
                        choices = setNames(c("", meta_cols), c(.tr("(aucun)"), meta_cols)),
                        selected = if ("batch" %in% meta_cols) "batch" else "")
      updateSelectInput(session, "da_identity_col",
                        choices = setNames(c("", meta_cols), c(.tr("(aucune)"), meta_cols)),
                        selected = if ("seurat_clusters" %in% meta_cols) "seurat_clusters" else "")
    }, ignoreInit = TRUE)

    # ── Validation du design (orchestration uniquement) ────────────────────
    observeEvent(input$da_validate, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "design DA")

        sample_col <- input$da_sample_col
        cond_col <- input$da_condition_col
        if (is.null(sample_col) || !nzchar(sample_col) ||
            is.null(cond_col) || !nzchar(cond_col)) {
          stop("Choisissez d'abord les colonnes ID échantillon et condition.", call. = FALSE)
        }
        assert_metadata_column(obj, sample_col, context = "design DA")
        assert_metadata_column(obj, cond_col, context = "design DA")
        rep_col <- if (!is.null(input$da_replicate_col) && nzchar(input$da_replicate_col)) {
          assert_metadata_column(obj, input$da_replicate_col, context = "design DA")
          input$da_replicate_col
        } else NULL
        batch_col <- if (!is.null(input$da_batch_col) && nzchar(input$da_batch_col)) {
          assert_metadata_column(obj, input$da_batch_col, context = "design DA")
          input$da_batch_col
        } else NULL
        ident_col <- if (!is.null(input$da_identity_col) && nzchar(input$da_identity_col)) {
          assert_metadata_column(obj, input$da_identity_col, context = "design DA")
          input$da_identity_col
        } else NULL

        # Resultat canonique — produit MEME pour un design bloque (le rapport
        # et les raisons sont affiches) ; les methodes passeront par
        # assert_da_design_result(method = ...), qui refusera.
        validated <- validate_da_design(
          metadata    = obj@meta.data,
          sample_id   = sample_col,
          condition   = cond_col,
          replicate_id = rep_col,
          batch       = batch_col,
          identity    = ident_col,
          context     = "design DA"
        )
        canonical <- finalize_da_design_result(
          validated   = validated,
          seurat_obj  = obj,
          analysis_id = "sc-da-design"
        )
        da_state$result <- canonical
        da_state$object_fingerprint <- velocity_object_fingerprint(obj)

        provenance_append(shared_rv, canonical$provenance)

        verdict <- if (identical(canonical$status, "valid")) "✅"
                   else if (identical(canonical$status, "valid_with_warnings")) "⚠️"
                   else "⛔"
        msg <- sprintf(
          "%s %s — %d échantillon(s), %d condition(s), %d cellule(s).",
          verdict, .status_label(canonical$status),
          canonical$provenance$parameters$n_samples,
          canonical$provenance$parameters$n_conditions,
          canonical$provenance$parameters$n_cells
        )
        da_status_rv(msg)
        showNotification(.tr("Validation du design terminée."), type = "message", duration = 4)

      }, error = function(e) {
        da_state$result <- NULL
        da_state$object_fingerprint <- NULL
        da_status_rv(paste(.tr("Erreur validation design :"), conditionMessage(e)))
        showNotification(
          paste(.tr("Erreur validation design :"), conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    .verdict_box <- function(r) {
      st <- r$status
      cls <- if (identical(st, "valid")) "success"
             else if (identical(st, "valid_with_warnings")) "warning"
             else "danger"
      icon_st <- if (identical(st, "valid")) "✅" else if (identical(st, "valid_with_warnings")) "⚠️" else "⛔"
      elig <- function(name, e) {
        tagList(
          tags$li(sprintf(
            "%s : %s",
            name,
            if (isTRUE(e$eligible)) .tr("éligible")
            else paste0(.tr("NON éligible — "), paste(e$blockers %||% character(0), collapse = " | "))
          ))
        )
      }
      tagList(
        div(class = sprintf("alert alert-%s m-3", cls), style = "font-size:0.95em;",
            tags$b(paste(icon_st, .status_label(st)))),
        div(class = "m-3",
            tags$b(.tr("Éligibilité des méthodes (design actuel) :")),
            tags$ul(
              elig("Milo", r$milo_eligibility %||% list(eligible = FALSE)),
              elig("scCODA", r$sccoda_eligibility %||% list(eligible = FALSE))
            )),
        div(class = "m-3 small text-muted",
            sprintf(.tr("Unité de composition : %s. Colonnes : échantillon « %s », condition « %s », réplicat « %s », batch « %s », identité « %s »."),
                    r$composition_unit %||% "sample",
                    r$config$sample_id, r$config$condition,
                    r$config$replicate_id %||% NA_character_,
                    r$config$batch %||% NA_character_,
                    r$config$identity %||% NA_character_))
      )
    }

    output$da_verdict <- renderUI({
      req(da_state$result)
      .check_fingerprint()
      .verdict_box(da_state$result)
    })

    output$da_conditions <- DT::renderDataTable({
      req(da_state$result)
      .check_fingerprint()
      cs <- da_state$result$condition_summary
      if (is.null(cs) || nrow(cs) == 0L) {
        return(DT::datatable(data.frame(Message = .tr("Aucune condition exploitable.")),
                             rownames = FALSE))
      }
      DT::datatable(cs, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$da_samples <- DT::renderDataTable({
      req(da_state$result)
      .check_fingerprint()
      DT::datatable(da_state$result$sample_summary, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$da_batch_ui <- renderUI({
      req(da_state$result)
      .check_fingerprint()
      cbt <- da_state$result$condition_batch_table
      if (is.null(cbt) || nrow(cbt) == 0L) {
        return(div(class = "alert alert-light m-3",
                   .tr("Aucun batch demandé (ou aucune paire condition × batch exploitable) — le terme batch ne sera pas modelisé.")))
      }
      tagList(
        div(class = "m-3 small text-muted",
            .tr("Comptage d'échantillons par condition × batch — une condition confinée à un seul batch bloquerait un modèle batch.")),
        DT::dataTableOutput(session$ns("da_batch_table"), height = "380px")
      )
    })

    output$da_batch_table <- DT::renderDataTable({
      req(da_state$result)
      DT::datatable(da_state$result$condition_batch_table, rownames = FALSE,
                    options = list(pageLength = 10))
    })

    output$da_identity_ui <- renderUI({
      req(da_state$result)
      .check_fingerprint()
      ic <- da_state$result$identity_coverage
      if (is.null(ic) || nrow(ic) == 0L) {
        return(div(class = "alert alert-light m-3",
                   .tr("Aucune colonne d'identité sélectionnée — la couverture par type cellulaire ne peut pas être évaluée.")))
      }
      tagList(
        div(class = "m-3 small text-muted",
            .tr("Représentation de chaque identité par échantillon — les identités présentes dans moins de 2 échantillons ne seront pas interprétables par condition.")),
        DT::dataTableOutput(session$ns("da_identity_table"), height = "380px")
      )
    })

    output$da_identity_table <- DT::renderDataTable({
      req(da_state$result)
      DT::datatable(da_state$result$identity_coverage, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # ── Exports (traces par analysis_id) ───────────────────────────────────
    output$dl_da_summary <- downloadHandler(
      filename = function() da_design_export_filename(da_state$result, "da_design_summary", "csv"),
      content = function(file) {
        req(da_state$result)
        .check_fingerprint()
        df <- build_da_design_summary(da_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_da_samples <- downloadHandler(
      filename = function() da_design_export_filename(da_state$result, "da_design_samples", "csv"),
      content = function(file) {
        req(da_state$result)
        .check_fingerprint()
        df <- build_da_design_sample_export(da_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_da_rds <- downloadHandler(
      filename = function() da_design_export_filename(da_state$result, "da_design_result", "rds"),
      content = function(file) {
        req(da_state$result)
        .check_fingerprint()
        saveRDS(da_state$result, file)
      }
    )

    # Expose state for tests (optional)
    return(da_state)
  })
}
