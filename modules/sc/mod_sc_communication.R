# =============================================================================
# mod_sc_communication.R — Communication cellule-cellule 4D-1 (Stage 11)
# =============================================================================
# Import et validation uniquement : tables de resultats CellChat / CellPhoneDB
# deja produites hors de l'application. Le module ORCHESTRE uniquement —
# parseurs par source, harmonisation des identites (exact match), QC d'import,
# statut canonique, provenance et exports residuent dans R/sc/sc_communication.R.
# Aucun calcul CellChat/CellPhoneDB, aucun reseau/chord/circle plot, aucune
# inference de pathway, aucun matching flou de labels (Stage 12).
# Les scores de sources differentes ne sont jamais presentes comme comparables.
# =============================================================================

mod_sc_communication_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #2980B9;",
        i18n$t("Communication cellule-cellule — import de resultats externes uniquement (CellChat / CellPhoneDB). "),
        i18n$t("Aucun recalcul, aucun score recompose. Les scores de sources differentes ne sont pas comparables.")),

    radioButtons(ns("comm_source"), i18n$t("Source des resultats"),
                 choices = setNames(c("cellchat", "cellphonedb"),
                                    c(.tr_plain("CellChat (table exportee)"),
                                      .tr_plain("CellPhoneDB (means.txt)"))),
                 selected = "cellchat"),

    conditionalPanel(
      condition = "input.comm_source == 'cellchat'", ns = ns,
      fileInput(ns("comm_cellchat_file"), i18n$t("Table CellChat exportee (CSV/TSV)"),
                accept = c(".csv", ".tsv", ".txt"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Colonnes attendues : source, target, ligand, receptor, prob (pathway/pval optionnels)."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'cellphonedb'", ns = ns,
      fileInput(ns("comm_cpdb_means"), i18n$t("CellPhoneDB means.txt (requis)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      fileInput(ns("comm_cpdb_pvalues"), i18n$t("CellPhoneDB pvalues.txt (optionnel)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Format v2 : colonne interacting_pair (ligand|receptor) + une colonne par paire sender|receiver."))
    ),

    hr(),
    selectInput(ns("comm_identity_column"),
                i18n$t("Colonne d'identites cellulaires (Seurat)"),
                choices = character(0), width = "100%"),
    div(class = "small text-muted mb-2",
        i18n$t("Harmonisation par exact match uniquement : aucun renommage de populations, labels sans correspondance listes integralement.")),
    actionButton(ns("comm_import"), i18n$t("Importer et valider"),
                 class = "btn-primary w-100", icon = icon("check")),
    div(class = "small text-muted mt-1", textOutput(ns("comm_status"))),
    hr(),
    downloadButton(ns("dl_comm_table"), i18n$t("Exporter table canonique (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_mapping"), i18n$t("Exporter harmonisation identites (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_summary"), i18n$t("Exporter resume d'import (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_rds"), i18n$t("Exporter resultat valide (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_communication_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Communication cellule-cellule — resultats importes")),
    navset_tab(
      nav_panel(i18n$t("Table interactions"),
                DT::dataTableOutput(ns("comm_table"), height = "520px")),
      nav_panel(i18n$t("Schema source"),
                DT::dataTableOutput(ns("comm_schema"), height = "420px"),
                div(class = "small text-muted mt-2", textOutput(ns("comm_schema_note")))),
      nav_panel(i18n$t("Harmonisation identites"),
                DT::dataTableOutput(ns("comm_mapping"), height = "420px")),
      nav_panel(i18n$t("QC import"),
                uiOutput(ns("comm_qc"))),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("Validite technique uniquement : un import valide n'implique aucune validite biologique.")),
                div(class = "alert alert-info m-3", style = "font-size:0.9em;",
                    i18n$t("Les scores ne sont pas compares entre sources differentes (CellChat vs CellPhoneDB).")),
                div(class = "alert alert-info m-3", style = "font-size:0.9em;",
                    i18n$t("Une association ligand-receptor n'etablit aucune causalite.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Les labels sans correspondance exacte dans la colonne choisie sont conserves tels quels et listes — jamais renommes.")))
    )
  )
}

mod_sc_communication_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # Libelle francais d'un etat de validite (fallback = code d'etat).
    .status_label <- function(st) {
      lab <- communication_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    comm_state <- reactiveValues(result = NULL, object_fingerprint = NULL)
    comm_status_rv <- reactiveVal(.tr("En attente d'import de resultats de communication..."))

    output$comm_status <- renderText({ comm_status_rv() })

    # ── Lecture CSV/TSV explicite (check.names = FALSE : les colonnes de
    # paires CellPhoneDB "A|B" ne doivent jamais etre mangles en "A.B") ────
    .read_table_auto <- function(path) {
      ext <- tolower(tools::file_ext(path))
      read_fn <- switch(ext,
        "csv" = function(p) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = ""),
        function(p) utils::read.delim(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = "")
      )
      tryCatch(read_fn(path), error = function(e) {
        stop(sprintf(
          "Lecture du fichier impossible (%s) : %s",
          basename(path), conditionMessage(e)
        ), call. = FALSE)
      })
    }

    # Garde de peremption avant tout rendu/export (etat derive du contrat).
    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(comm_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(comm_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # ── Choix de la colonne d'identites, synchronise avec l'objet Seurat ───
    # UN SEUL observateur par declencheur (garde anti-duplication) : reset du
    # resultat (aucun import ne survit a un changement d'objet — resultat
    # perime par construction) + resynchronisation des choix de colonne.
    observeEvent(global_data$sc_obj, {
      comm_state$result <- NULL
      comm_state$object_fingerprint <- NULL
      comm_status_rv(.tr("Objet Seurat modifie : reimportez la table de communication."))
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "comm_identity_column", choices = character(0))
        return()
      }
      meta_cols <- colnames(obj@meta.data)
      updateSelectInput(session, "comm_identity_column",
                        choices = meta_cols,
                        selected = if ("seurat_clusters" %in% meta_cols) "seurat_clusters" else meta_cols[1])
    }, ignoreInit = TRUE)

    # ── Import + validation (orchestration uniquement) ─────────────────────
    observeEvent(input$comm_import, {
      req(global_data$sc_obj)
      tryCatch({
        # CHRYSALIS 2E : garde d'entree explicite (message FR dans le canal
        # d'erreur au lieu d'un req() silencieux).
        obj <- assert_seurat(global_data$sc_obj, context = "communication import")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
          stop("Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.", call. = FALSE)
        }
        assert_metadata_column(obj, identity_col, context = "communication import")

        src <- input$comm_source %||% "cellchat"
        warnings_all <- character(0)

        if (identical(src, "cellchat")) {
          req(input$comm_cellchat_file)
          tab <- .read_table_auto(input$comm_cellchat_file$datapath)
          parsed <- parse_cellchat_import(tab, source_file = input$comm_cellchat_file$name)
          files <- list(table = input$comm_cellchat_file$name)
        } else {
          req(input$comm_cpdb_means)
          means <- .read_table_auto(input$comm_cpdb_means$datapath)
          pvals <- NULL
          files <- list(means = input$comm_cpdb_means$name)
          if (!is.null(input$comm_cpdb_pvalues)) {
            pvals <- .read_table_auto(input$comm_cpdb_pvalues$datapath)
            files$pvalues <- input$comm_cpdb_pvalues$name
          }
          parsed <- parse_cellphonedb_import(means, pvals, source_file = input$comm_cpdb_means$name)
        }
        warnings_all <- c(warnings_all, parsed$warnings)

        # Harmonisation des identites : vecteur d'identites extrait de la
        # colonne EXPLICITEMENT choisie (exact match, jamais de renommage).
        identities <- unique(as.character(obj@meta.data[[identity_col]]))
        harm <- harmonize_communication_identities(
          parsed$table, identities, identity_col, context = "communication import"
        )
        warnings_all <- c(warnings_all, harm$warnings)

        # QC d'import : comptages avant/apres, duplications, auto-interactions.
        qcr <- communication_import_qc(harm$table)
        warnings_all <- c(warnings_all, qcr$warnings)

        if (nrow(qcr$table) == 0L) {
          stop("Toutes les lignes ont ete supprimees au QC (sender/receiver/ligand/receptor vides) : aucun resultat canonique produit.", call. = FALSE)
        }

        # Stage 11 : resultat canonique — contrat documente dans
        # docs/contracts/COMMUNICATION_RESULT_CONTRACT.md. Provenance PRODUITE
        # ici puis appendee a l'etat partage (regle 7 AGENTS.md).
        canonical <- finalize_communication_result(
          canonical_table = qcr$table,
          source_method   = src,
          source_files    = files,
          identity_column = identity_col,
          identity_mapping = harm$mapping,
          identity_summary = harm$summary,
          column_mapping  = parsed$column_mapping,
          qc              = qcr$counts,
          n_input_rows    = parsed$n_input_rows,
          seurat_obj      = obj,
          extra_warnings  = warnings_all,
          analysis_id     = "sc-communication-import"
        )
        comm_state$result <- canonical
        comm_state$object_fingerprint <- velocity_object_fingerprint(obj)

        provenance_append(shared_rv, canonical$provenance)

        msg <- sprintf(
          paste0("Import OK : %d lignes canoniques (%d lignes source). ",
                 "Source : %s. Colonne d'identites : %s. ",
                 "Labels harmonises : %d/%d."),
          canonical$input_summary$n_rows_canonical,
          canonical$input_summary$n_rows_input,
          src, identity_col,
          harm$summary$n_labels_matched, harm$summary$n_labels
        )
        if (harm$summary$n_labels_unmatched > 0L) {
          msg <- paste0(msg, "\n", sprintf(
            "%d label(s) sans correspondance (conserves tels quels) : %s",
            harm$summary$n_labels_unmatched,
            paste(utils::head(harm$summary$unmatched_labels, 5), collapse = ", ")
          ))
        }
        comm_status_rv(paste0("[", .status_label(canonical$status), "]\n", msg))
        showNotification(.tr("Import communication valide."), type = "message", duration = 4)

      }, error = function(e) {
        comm_state$result <- NULL
        comm_state$object_fingerprint <- NULL
        comm_status_rv(paste(.tr("Erreur import communication :"), conditionMessage(e)))
        showNotification(
          paste(.tr("Erreur import communication :"), conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    output$comm_table <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      DT::datatable(
        comm_state$result$canonical_table,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE),
        filter = "top"
      )
    })

    output$comm_schema <- DT::renderDataTable({
      req(comm_state$result)
      cm <- comm_state$result$column_mapping %||% list()
      df <- data.frame(
        champ_canonique = names(cm),
        colonne_source = vapply(cm, paste, character(1), collapse = " / "),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15))
    })

    output$comm_schema_note <- renderText({
      req(comm_state$result)
      .tr("Mapping resolu de facon deterministe a l'import — aucune conversion supposee entre schemas incompatibles.")
    })

    output$comm_mapping <- DT::renderDataTable({
      req(comm_state$result)
      req(comm_state$result$identity_mapping)
      DT::datatable(
        comm_state$result$identity_mapping,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$comm_qc <- renderUI({
      req(comm_state$result)
      r <- comm_state$result
      qc <- r$qc %||% list()
      ids <- r$identity_summary %||% list()

      .row <- function(k, v) tags$tr(tags$th(style = "width:65%;", k), tags$td(v))
      rows <- list(
        .row(.tr("Lignes source (avant conversion)"), as.character(r$input_summary$n_rows_input)),
        .row(.tr("Lignes canoniques (apres QC)"), as.character(r$input_summary$n_rows_canonical)),
        .row(.tr("Lignes supprimees (champs requis vides)"), as.character(qc$n_dropped_required_fields %||% 0L)),
        .row(.tr("Auto-interactions (sender = receiver)"), as.character(qc$n_self_interactions %||% 0L)),
        .row(.tr("Interactions dupliquees (conservees et flagees)"), as.character(qc$n_duplicate_interactions %||% 0L)),
        .row(.tr("Pathways manquants"), as.character(qc$n_pathway_missing %||% 0L)),
        .row(.tr("P-values hors [0,1]"), as.character(qc$n_p_value_out_of_range %||% 0L)),
        .row(.tr("Labels sources distincts"), as.character(ids$n_labels %||% NA_integer_)),
        .row(.tr("Labels harmonises (exact match)"), as.character(ids$n_labels_matched %||% NA_integer_)),
        .row(.tr("Labels sans correspondance"), as.character(ids$n_labels_unmatched %||% NA_integer_))
      )
      warn <- r$warnings %||% character(0)
      tagList(
        div(class = "m-3",
            tags$table(class = "table table-sm table-bordered",
                       tags$tbody(lapply(rows, function(rw) rw)))),
        if (length(warn)) div(class = "m-3", h6(.tr("Avertissements produits a l'import")),
                              tags$ul(lapply(warn, function(w) tags$li(w))))
      )
    })

    # ── Exports (traces par analysis_id) ───────────────────────────────────
    output$dl_comm_table <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_table", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        utils::write.csv(comm_state$result$canonical_table, file, row.names = FALSE)
      }
    )

    output$dl_comm_mapping <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_identity_mapping", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_identity_mapping_export(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_summary <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_summary", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_import_summary(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_rds <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_result", "rds"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        saveRDS(comm_state$result, file)
      }
    )

    # Expose state for tests (optional)
    return(comm_state)
  })
}
