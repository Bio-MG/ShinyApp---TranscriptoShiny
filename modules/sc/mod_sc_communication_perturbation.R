# =============================================================================
# mod_sc_communication_perturbation.R — onglet « Perturbation (in silico) »
# du panneau Communication (roadmap avancée Phase 4 / V1.x-D)
# =============================================================================
# Orchestration uniquement : la science residue dans
# R/sc/sc_communication_perturbation.R (contrat
# COMMUNICATION_PERTURBATION_CONTRACT). GARDE ABSOLU : bandeau « IN SILICO
# PERTURBATION » permanent — ce n'est PAS une expérience KO. La valeur de
# cible est choisie DANS la table (aucun matching flou) ; le calcul est sur
# bouton explicite ; la table canonique n'est jamais modifiée.
# =============================================================================

mod_sc_communication_perturbation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-danger", style = "font-size:0.85em;padding:6px;",
        icon("flask"), " ",
        i18n$t("IN SILICO PERTURBATION — simulation sur le réseau INFÉRÉ : ce n'est PAS une expérience KO, PAS un effet biologique validé. Effets de premier ordre sur les scores importés (aucune propagation modélisée).")),
    fluidRow(
      column(4, selectInput(ns("pert_target"), i18n$t("Cible"),
                            choices = setNames(communication_perturbation_targets(),
                                               c(.tr_plain("Ligand"), .tr_plain("Récepteur"),
                                                 .tr_plain("Interaction (ligand -> récepteur)"),
                                                 .tr_plain("Population sender"), .tr_plain("Population receiver"))),
                            width = "100%")),
      column(4, selectInput(ns("pert_value"), i18n$t("Valeur (exacte, issue de la table)"),
                            choices = character(0), width = "100%")),
      column(4,
             radioButtons(ns("pert_mode"), i18n$t("Mode"),
                          choices = setNames(c("remove", "attenuate"),
                                             c(.tr_plain("Suppression"), .tr_plain("Atténuation"))),
                          selected = "remove", inline = TRUE, width = "100%"))),
    fluidRow(
      column(4, conditionalPanel(
        condition = "input.pert_mode == 'attenuate'", ns = ns,
        numericInput(ns("pert_factor"), i18n$t("Facteur d'atténuation (0-1)"),
                     value = 0.5, min = 0.01, max = 0.99, step = 0.05, width = "100%"))),
      column(8, div(class = "small text-muted mt-4",
                    textOutput(ns("pert_target_info"))))),
    actionButton(ns("pert_compute"), i18n$t("Simuler la perturbation"),
                 class = "btn-primary w-100", icon = icon("flask")),
    div(class = "small text-muted mt-1", textOutput(ns("pert_status"))),
    hr(),
    DT::dataTableOutput(ns("pert_table"), height = "320px"),
    fluidRow(
      column(6, plotOutput(ns("pert_delta"), height = "360px"),
             fluidRow(
               column(6, downloadButton(ns("dl_pert_delta_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_pert_delta_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      column(6, plotOutput(ns("pert_nodes"), height = "360px"),
             fluidRow(
               column(6, downloadButton(ns("dl_pert_nodes_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_pert_nodes_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary"))))),
    fluidRow(
      column(6, downloadButton(ns("dl_pert_csv"), i18n$t("Exporter deltas (CSV)"), class = "btn-sm btn-info w-100")),
      column(6, verbatimTextOutput(ns("pert_recap"))))
  )
}

mod_sc_communication_perturbation_server <- function(id, comm_state, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }
    .error_plot <- function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = conditionMessage(e), size = 5, colour = "red") +
        ggplot2::theme_void()
    }
    pert_ctx <- reactiveVal(NULL)

    .guard_stale <- function() {
      req(global_data$sc_obj, comm_state$result, comm_state$object_fingerprint)
      if (!identical(velocity_object_fingerprint(global_data$sc_obj),
                     comm_state$object_fingerprint)) {
        shiny::validate(shiny::need(FALSE, .tr("Résultat de communication périmé : réimportez.")))
      }
    }

    # Choix de valeur synchronisé avec la cible ET la table (importée ou
    # filtrée par les filtres du panneau parent — aucun matching flou).
    .target_col <- function() {
      switch(input$pert_target,
             ligand = "ligand", receptor = "receptor", interaction = "interaction",
             sender = "sender_node", receiver = "receiver_node")
    }
    .node_table <- function() {
      t <- comm_state$result$canonical_table
      sm <- t$sender_mapped %||% rep(NA_character_, nrow(t))
      rm <- t$receiver_mapped %||% rep(NA_character_, nrow(t))
      t$sender_node <- ifelse(!is.na(sm), sm, t$sender)
      t$receiver_node <- ifelse(!is.na(rm), rm, t$receiver)
      t
    }
    .update_value_choices <- function() {
      if (is.null(comm_state$result)) {
        updateSelectInput(session, "pert_value", choices = character(0))
        return()
      }
      t <- .node_table()
      col <- .target_col()
      vals <- sort(unique(stats::na.omit(as.character(t[[col]]))))
      updateSelectInput(session, "pert_value", choices = vals,
                        selected = if (length(vals)) vals[1] else character(0))
    }
    observeEvent(input$pert_target, { .update_value_choices() }, ignoreInit = TRUE)
    # UN SEUL observateur par déclencheur (garde anti-duplication) : reset du
    # contexte précédent + resynchronisation des choix de valeur.
    observeEvent(comm_state$result, {
      pert_ctx(NULL)
      .update_value_choices()
    }, ignoreInit = TRUE)

    output$pert_target_info <- renderText({
      req(comm_state$result)
      t <- .node_table()
      col <- .target_col()
      n <- length(unique(stats::na.omit(as.character(t[[col]]))))
      sprintf(.tr("%d valeur(s) disponible(s) dans la table pour cette cible — la perturbation s'applique à une copie d'affichage, la table canonique n'est jamais modifiée."), n)
    })

    observeEvent(input$pert_compute, {
      .guard_stale()
      tryCatch({
        ctx <- build_communication_perturbation(
          comm_state$result,
          target = input$pert_target,
          value  = input$pert_value,
          mode   = input$pert_mode,
          factor = if (identical(input$pert_mode, "attenuate"))
            input$pert_factor %||% 0.5 else 0.5
        )
        pert_ctx(ctx)
        if (!is.null(shared_rv)) provenance_append(shared_rv, ctx$provenance)
        showNotification(
          sprintf(.tr("Perturbation in silico : %d interaction(s) supprimée(s), %d atténuée(s) — %d paire(s) affectée(s)."),
                  ctx$qc$n_removed, ctx$qc$n_attenuated, ctx$qc$n_pairs_affected),
          type = "message", duration = 5
        )
      }, error = function(e) {
        pert_ctx(NULL)
        showNotification(paste(.tr("Erreur perturbation :"), conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    output$pert_status <- renderText({
      ctx <- pert_ctx()
      req(ctx)
      sprintf(.tr("IN SILICO — interactions : %d → %d ; paires affectées : %d ; senders affectés : %s."),
              ctx$baseline_summary$n_interactions, ctx$perturbed_summary$n_interactions,
              ctx$qc$n_pairs_affected,
              if (length(ctx$qc$affected_senders)) paste(ctx$qc$affected_senders, collapse = ", ") else "—")
    })

    output$pert_recap <- renderText({
      ctx <- pert_ctx()
      req(ctx)
      paste0(
        "IN SILICO PERTURBATION\n",
        sprintf("Cible   : %s = %s\n", ctx$target, ctx$value),
        sprintf("Mode    : %s (facteur %s)\n", ctx$mode,
                if (is.na(ctx$params$factor)) "—" else format(ctx$params$factor)),
        sprintf("Réseau  : %d → %d interactions\n",
                ctx$baseline_summary$n_interactions, ctx$perturbed_summary$n_interactions),
        "Effets  : premier ordre — aucune propagation modélisée."
      )
    })

    output$pert_table <- DT::renderDataTable({
      ctx <- pert_ctx()
      req(ctx)
      DT::datatable(ctx$delta_table, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    output$pert_delta <- renderPlot({
      ctx <- pert_ctx()
      req(ctx)
      tryCatch(plot_communication_perturbation_delta(ctx), error = function(e) .error_plot(e))
    })

    output$pert_nodes <- renderPlot({
      ctx <- pert_ctx()
      req(ctx)
      tryCatch(plot_communication_perturbation_nodes(ctx), error = function(e) .error_plot(e))
    })

    .fig_download <- function(output_id, kind, ext, plot_fn) {
      output[[output_id]] <- downloadHandler(
        filename = function() {
          ctx <- pert_ctx()
          if (is.null(ctx)) return(paste0(kind, ".", ext))
          paste0(kind, "_", ctx$analysis_id, "_", ctx$timestamp_utc, ".", ext)
        },
        content = function(file) {
          ctx <- pert_ctx()
          req(ctx)
          .guard_stale()
          ggplot2::ggsave(file, plot_fn(), width = 8, height = 6.5, dpi = 300)
        }
      )
    }
    .fig_download("dl_pert_delta_png", "communication_perturbation_delta", "png",
                  function() plot_communication_perturbation_delta(pert_ctx()))
    .fig_download("dl_pert_delta_pdf", "communication_perturbation_delta", "pdf",
                  function() plot_communication_perturbation_delta(pert_ctx()))
    .fig_download("dl_pert_nodes_png", "communication_perturbation_nodes", "png",
                  function() plot_communication_perturbation_nodes(pert_ctx()))
    .fig_download("dl_pert_nodes_pdf", "communication_perturbation_nodes", "pdf",
                  function() plot_communication_perturbation_nodes(pert_ctx()))

    output$dl_pert_csv <- downloadHandler(
      filename = function() {
        ctx <- pert_ctx()
        if (is.null(ctx)) return("communication_perturbation.csv")
        paste0("communication_perturbation_", ctx$analysis_id, "_", ctx$timestamp_utc, ".csv")
      },
      content = function(file) {
        ctx <- pert_ctx()
        req(ctx)
        .guard_stale()
        utils::write.csv(build_communication_perturbation_export(ctx), file, row.names = FALSE)
      }
    )

    invisible(NULL)
  })
}
