# =============================================================================
# mod_sc_da_cross.R — Abondance differentielle 4E-3 : vues croisées Milo x
# scCODA (Stage 16)
# =============================================================================
# Le module ORCHESTRE uniquement : il consomme les DEUX resultats canoniques
# exposes par les panels 8d (shared_rv$da_milo_result) et 8e
# (shared_rv$da_sccoda_result) et les vues pures de
# R/sc/sc_abundance_cross_views.R. AUCUN calcul : la comparaison est
# DESCRIPTIVE, sensible a la methode, sans p-value de consensus. Les options
# d'affichage sont enregistrees dans la provenance au moment de l'export.
# =============================================================================

mod_sc_da_cross_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #2C3E50;",
        i18n$t("Vues croisées DESCRIPTIVES : Milo teste des VOISINAGES (régions d'un graphe), scCODA teste la COMPOSITION par échantillon — deux grandeurs différentes, jamais fusionnées. "),
        i18n$t("Aucune « p-value de consensus » n'est calculée.")),
    uiOutput(ns("cross_comparability_box")),
    actionButton(ns("cross_refresh"), i18n$t("Rafraîchir la comparaison"),
                 class = "btn-primary w-100", icon = icon("scale-balanced")),
    hr(),
    downloadButton(ns("dl_cross_concordance"), i18n$t("Exporter concordance par identité (CSV)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_da_cross_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Abondance différentielle — vues croisées (Milo × scCODA)")),
    navset_tab(
      nav_panel(i18n$t("Concordance par identité"),
                plotOutput(ns("cross_concordance"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_cross_concordance_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_cross_concordance_pdf"), "PDF", class = "btn-sm btn-outline-secondary")),
                DT::dataTableOutput(ns("cross_table"), height = "300px")),
      nav_panel(i18n$t("Mappage voisinages → identités"),
                plotOutput(ns("cross_nhood_map"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_cross_nhood_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_cross_nhood_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Composition échantillons"),
                plotOutput(ns("cross_compo"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_cross_compo_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_cross_compo_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Effets scCODA (rappel)"),
                plotOutput(ns("cross_sccoda_effects"), height = "480px")),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-danger m-3", style = "font-size:0.9em;",
                    i18n$t("Deux GRANDEURS DIFFÉRENTES : la médiane des logFC de voisinages (Milo, niveau voisinage) et l'effet compositionnel (scCODA, niveau échantillon) ne mesurent pas la même chose — la concordance est descriptive et exploratoire.")),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("Les cellules ne sont pas des réplicats biologiques : chaque vue porte le contexte de composition par échantillon. Un désaccord peut provenir des échelles, des effectifs ou d'une biologie captée différemment — rien ne tranche.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    uiOutput(ns("cross_meanings"))))
    )
  )
}

mod_sc_da_cross_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    cross_state <- reactiveValues(summary = NULL, provenance = NULL)
    .has_both <- function() {
      !is.null(shared_rv) && !is.null(shared_rv$da_milo_result) &&
        !is.null(shared_rv$da_sccoda_result)
    }

    # ── Comparabilite (pure lecture des deux resultats canoniques) ────────
    .cross_summary <- eventReactive(c(input$cross_refresh, .has_both()), {
      if (!.has_both()) return(NULL)
      tryCatch(
        build_da_cross_method_summary(shared_rv$da_milo_result,
                                      shared_rv$da_sccoda_result),
        error = function(e) {
          showNotification(paste(.tr("Erreur vues croisées :"), conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        }
      )
    })

    output$cross_comparability_box <- renderUI({
      sm <- .cross_summary()
      if (is.null(sm)) {
        return(div(class = "alert alert-light py-1", style = "font-size:0.85em;",
                   .tr("Lancez Milo (8d) ET scCODA (8e) pour activer la lecture croisée.")))
      }
      if (isTRUE(sm$comparability$fully_comparable)) {
        div(class = "alert alert-success py-1", style = "font-size:0.85em;",
            .tr("Résultats comparables : même objet, même contraste, même colonne d'identité."))
      } else {
        div(class = "alert alert-warning py-1", style = "font-size:0.85em;",
            tags$b(.tr("Comparabilité partielle — lisez les réserves :")),
            tags$ul(lapply(sm$comparability$caveats, function(cv) tags$li(cv))))
      }
    })

    # ── Vues (consommatrices pures des deux resultats) ─────────────────────
    .concordance_plot <- function() {
      plot_da_cross_concordance(shared_rv$da_milo_result,
                                shared_rv$da_sccoda_result)
    }
    .nhood_plot <- function() {
      plot_da_cross_nhood_mapping(shared_rv$da_milo_result)
    }
    .compo_plot <- function() {
      plot_da_cross_sample_composition(shared_rv$da_milo_result,
                                       shared_rv$da_sccoda_result)
    }
    .sccoda_effects_plot <- function() {
      plot_sccoda_effects(shared_rv$da_sccoda_result)
    }

    output$cross_concordance <- renderPlot({
      req(.has_both()); .cross_summary(); .concordance_plot()
    })
    output$cross_nhood_map <- renderPlot({
      req(.has_both()); .cross_summary(); .nhood_plot()
    })
    output$cross_compo <- renderPlot({
      req(.has_both()); .cross_summary(); .compo_plot()
    })
    output$cross_sccoda_effects <- renderPlot({
      req(.has_both()); .cross_summary(); .sccoda_effects_plot()
    })

    output$cross_table <- DT::renderDataTable({
      sm <- .cross_summary()
      req(sm)
      DT::datatable(sm$concordance, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatSignif(columns = c("milo_frac_significant",
                                     "milo_median_logfc", "sccoda_effect",
                                     "sccoda_inclusion_probability"),
                         digits = 4)
    })

    output$cross_meanings <- renderUI({
      sm <- .cross_summary()
      req(sm)
      tags$ul(
        tags$li(sm$method_meanings$milo),
        tags$li(sm$method_meanings$sccoda),
        tags$li(sm$method_meanings$comparison)
      )
    })

    # ── Exports (chaque export PRODUIT une entree de provenance figee) ─────
    .export_figure <- function(plot_fn, file) {
      ggplot2::ggsave(file, plot_fn(), width = 8, height = 6.5, dpi = 150)
    }
    .append_provenance <- function() {
      sm <- .cross_summary()
      if (is.null(sm)) return(NULL)
      entry <- build_da_cross_provenance(
        shared_rv$da_milo_result, shared_rv$da_sccoda_result,
        options = list(view = "da_cross_module")
      )
      provenance_append(shared_rv, entry)
      entry
    }

    output$dl_cross_concordance <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_concordance", "csv"),
      content = function(file) {
        req(.has_both())
        .append_provenance()
        utils::write.csv(build_da_cross_concordance_export(
          shared_rv$da_milo_result, shared_rv$da_sccoda_result
        ), file, row.names = FALSE)
      }
    )
    output$dl_cross_concordance_png <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_concordance", "png"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.concordance_plot, file) }
    )
    output$dl_cross_concordance_pdf <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_concordance", "pdf"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.concordance_plot, file) }
    )
    output$dl_cross_nhood_png <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_nhood_mapping", "png"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.nhood_plot, file) }
    )
    output$dl_cross_nhood_pdf <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_nhood_mapping", "pdf"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.nhood_plot, file) }
    )
    output$dl_cross_compo_png <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_sample_composition", "png"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.compo_plot, file) }
    )
    output$dl_cross_compo_pdf <- downloadHandler(
      filename = function() da_cross_export_filename("da_cross_sample_composition", "pdf"),
      content = function(file) { req(.has_both()); .append_provenance(); .export_figure(.compo_plot, file) }
    )

    return(cross_state)
  })
}
