# =============================================================================
# mod_sc_communication_velocity.R — onglet « Contexte vélocité » du panneau
# Communication (roadmap avancée Phase 2 / V1.x-C)
# =============================================================================
# Orchestration uniquement : la science residue dans
# R/sc/sc_communication_velocity.R (contrat COMMUNICATION_VELOCITY_CONTRACT).
# Le module consomme le résultat velocity canonique (shared_rv$velocity_result,
# contrat Stage 10) et le seuil de recouvrement déclaré
# (TS_VELOCITY_OVERLAP_MIN de config/thresholds.R — jamais codé en dur).
# Sans vecteur validé : message explicite (état unavailable_no_vectors) —
# aucune substitution, aucune ré-inférence.
# =============================================================================

mod_sc_communication_velocity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning", style = "font-size:0.85em;padding:6px;",
        i18n$t("« Associé à l'état de vitesse » n'établit aucune causalité : la magnitude des vecteurs précalculés est une quantité descriptive par population.")),
    div(class = "small text-muted mb-2", textOutput(ns("vel_info"))),
    # 3F (O28, spéc §2.18 point 1) : garde PRÉVENTIVE — bandeau info + bouton
    # désactivé avant tout clic quand les vecteurs sont absents/inutilisables
    # (le constat 3F : la seule garde existante — stop :84 — était post-clic).
    uiOutput(ns("vel_guard")),
    fluidRow(
      column(6, numericInput(ns("vel_min_overlap"), i18n$t("Recouvrement minimal (fraction)"),
                             value = TS_VELOCITY_OVERLAP_MIN, min = 0, max = 1, step = 0.05,
                             width = "100%")),
      column(6, div(class = "small text-muted mt-4",
                    i18n$t("Défaut consommé de config/thresholds.R (TS_VELOCITY_OVERLAP_MIN).")))),
    actionButton(ns("vel_compute"), i18n$t("Calculer le contexte vélocité"),
                 class = "btn-primary w-100", icon = icon("tachometer-alt")),
    div(class = "small text-muted mt-1", textOutput(ns("vel_status"))),
    hr(),
    DT::dataTableOutput(ns("vel_table"), height = "300px"),
    plotOutput(ns("vel_plot"), height = "380px"),
    fluidRow(
      column(3, downloadButton(ns("dl_vel_csv"), i18n$t("CSV"), class = "btn-sm btn-outline-secondary")),
      column(3, downloadButton(ns("dl_vel_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
      column(3, downloadButton(ns("dl_vel_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))
  )
}

mod_sc_communication_velocity_server <- function(id, comm_state, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }
    vel_ctx <- reactiveVal(NULL)

    .guard_stale <- function() {
      req(global_data$sc_obj, comm_state$result, comm_state$object_fingerprint)
      if (!identical(velocity_object_fingerprint(global_data$sc_obj),
                     comm_state$object_fingerprint)) {
        shiny::validate(shiny::need(FALSE, .tr("Résultat de communication périmé : réimportez.")))
      }
    }

    observeEvent(comm_state$result, { vel_ctx(NULL) }, ignoreInit = TRUE)

    output$vel_info <- renderText({
      vr <- if (!is.null(shared_rv)) state_get(shared_rv, "velocity_result") else NULL
      if (is.null(vr)) {
        .tr("Aucun résultat velocity importé : importez d'abord des vecteurs précalculés dans le panneau Vélocité (8.6).")
      } else {
        sprintf(.tr("Résultat velocity consommé : statut « %s », %d cellule(s) alignée(s). Les vecteurs ne sont jamais ré-inférés."),
                vr$status, length(vr$cell_names %||% character(0)))
      }
    })

    # ── 3F (O28, spéc §2.18 point 1) : garde préventive ──────────────────────
    # États utilisables du contrat velocity (sc_velocity.R:141-151) :
    # valid / valid_partial_embedding portent des vecteurs consommables ;
    # valid_no_vectors (objet valide, AUCUN vecteur) et les invalid_* n'en
    # portent pas. Préventif = bandeau + bouton désactivé AVANT le clic ; le
    # stop post-clic (:84) reste en seconde ligne de défense.
    .vel_vr <- reactive({
      if (!is.null(shared_rv)) state_get(shared_rv, "velocity_result") else NULL
    })
    .vel_ready <- function(vr) {
      st <- if (!is.null(vr)) vr$status %||% NA_character_ else NA_character_
      !is.null(vr) && st %in% c("valid", "valid_partial_embedding")
    }
    observeEvent(.vel_vr(), {
      vr <- .vel_vr()
      shiny::updateActionButton(session, "vel_compute", disabled = !.vel_ready(vr))
    })
    output$vel_guard <- renderUI({
      vr <- .vel_vr()
      if (.vel_ready(vr)) return(NULL)
      msg <- if (is.null(vr)) {
        .tr("Le calcul de contexte nécessite des vecteurs de vélocité précalculés complets — aucun résultat velocity importé.")
      } else if (identical(vr$status, "valid_no_vectors")) {
        .tr("Le calcul de contexte nécessite des vecteurs de vélocité précalculés complets (absents dans l'import actuel).")
      } else {
        sprintf(.tr("Statut velocity « %s » : vecteurs inutilisables — réimportez depuis le panneau Vélocité (8.6)."),
                as.character(vr$status %||% "inconnu"))
      }
      div(class = "alert alert-info", style = "font-size:0.85em;padding:6px;",
          icon("circle-info"), " ", msg)
    })

    .identities <- function() {
      obj <- global_data$sc_obj
      col <- comm_state$result$identity_column %||% NA_character_
      if (is.null(obj) || is.na(col) || !col %in% colnames(obj@meta.data)) return(NULL)
      meta <- obj@meta.data
      if (!is.null(comm_state$context_cells)) {
        meta <- meta[rownames(meta) %in% as.character(comm_state$context_cells), , drop = FALSE]
      }
      setNames(as.character(meta[[col]]), rownames(meta))
    }

    observeEvent(input$vel_compute, {
      .guard_stale()
      tryCatch({
        vr <- if (!is.null(shared_rv)) state_get(shared_rv, "velocity_result") else NULL
        if (is.null(vr)) {
          stop("Aucun résultat velocity importé : utilisez d'abord le panneau Vélocité (8.6).", call. = FALSE)
        }
        idents <- .identities()
        if (is.null(idents)) {
          stop(sprintf("Colonne d'identités '%s' introuvable — réimportez la communication.",
                       comm_state$result$identity_column %||% NA), call. = FALSE)
        }
        # 3F (O28, spéc §2.18) : jauge de progression — le constat 3F mesurait
        # 0 withProgress dans ce module (calcul potentiellement long, gel
        # perçu de la session).
        ctx <- withProgress(message = .tr("Calcul du contexte vélocité..."), value = 0.4, {
          build_communication_velocity_context(
            comm_state$result,
            velocity_result  = vr,
            cell_identities  = idents,
            identity_column  = comm_state$result$identity_column,
            min_overlap      = input$vel_min_overlap %||% NA_real_
          )
        })
        vel_ctx(ctx)
        if (!is.null(shared_rv)) provenance_append(shared_rv, ctx$provenance)
        if (identical(ctx$status, "valid")) {
          showNotification(
            sprintf(.tr("Contexte vélocité : %d paire(s) cartographiée(s) — recouvrement %.1f%%."),
                    ctx$qc$n_pairs_mapped, 100 * ctx$qc$overlap_fraction),
            type = "message", duration = 5
          )
        } else {
          showNotification(
            sprintf(.tr("Contexte vélocité indisponible (statut : %s) — voir les avertissements."),
                    ctx$status),
            type = "warning", duration = 8
          )
        }
      }, error = function(e) {
        vel_ctx(NULL)
        showNotification(paste(.tr("Erreur contexte vélocité :"), conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    output$vel_status <- renderText({
      ctx <- vel_ctx()
      req(ctx)
      if (identical(ctx$status, "valid")) {
        sprintf(.tr("Statut : %s — %d paire(s) cartographiée(s) ; recouvrement %.1f%%."),
                ctx$status, ctx$qc$n_pairs_mapped, 100 * ctx$qc$overlap_fraction)
      } else {
        paste(.tr("Statut :"), ctx$status, "—", utils::head(ctx$warnings, 1))
      }
    })

    output$vel_table <- DT::renderDataTable({
      ctx <- vel_ctx()
      req(ctx)
      ts_datatable(ctx$pair_table, page_length = 15L,
                   filename_base = "sc_comm_velocity_pair_table", filter = "none")
    })

    output$vel_plot <- renderPlot({
      ctx <- vel_ctx()
      req(ctx)
      tryCatch(plot_communication_velocity_context(ctx), error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = conditionMessage(e), size = 5, colour = "red") +
          ggplot2::theme_void()
      })
    })

    output$dl_vel_csv <- downloadHandler(
      filename = function() {
        ctx <- vel_ctx()
        if (is.null(ctx)) return("communication_velocity_context.csv")
        paste0("communication_velocity_context_", ctx$analysis_id, "_", ctx$timestamp_utc, ".csv")
      },
      content = function(file) {
        ctx <- vel_ctx()
        req(ctx)
        .guard_stale()
        utils::write.csv(build_communication_velocity_export(ctx), file, row.names = FALSE)
      }
    )

    .fig_download <- function(output_id, kind, ext) {
      output[[output_id]] <- downloadHandler(
        filename = function() {
          ctx <- vel_ctx()
          if (is.null(ctx)) return(paste0(kind, ".", ext))
          paste0(kind, "_", ctx$analysis_id, "_", ctx$timestamp_utc, ".", ext)
        },
        content = function(file) {
          ctx <- vel_ctx()
          req(ctx)
          .guard_stale()
          ts_export_plot(file, plot_communication_velocity_context(ctx),
                          width = 8, height = 6, dpi = 300)
        }
      )
    }
    .fig_download("dl_vel_png", "communication_velocity_context", "png")
    .fig_download("dl_vel_pdf", "communication_velocity_context", "pdf")

    invisible(NULL)
  })
}
