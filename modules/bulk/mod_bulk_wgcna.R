# =============================================================================
# mod_bulk_wgcna.R — Bulk V2 M4 : réseaux de co-expression WGCNA (safe-mode)
# — UI + orchestration.
# =============================================================================
# Logique pure : R/bulk/bulk_wgcna.R (contrat gelé). Deux étapes guidées :
#   1) « Analyse du power » (pickSoftThreshold — plots fit/connectivité) ;
#   2) « Construire les modules » (blockwiseModules, power retenu ou manuel)
#      puis corrélations MEs <-> traits (bicor).
# GARDES mission (gelés côté R/) : N >= 15 (arrêt dur), 2000-5000 gènes
# variables, maxBlockSize borné, threads désactivés, matrice transformée.
# =============================================================================

mod_bulk_wgcna_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-info", style = "font-size:0.8em;",
        icon("info-circle"), " ",
        i18n$t("WGCNA exige au moins 15 \u00e9chantillons et une matrice transform\u00e9e (VST, \u00e9tape 1). Le r\u00e9seau est construit sur 2000\u20135000 g\u00e8nes les plus variables (garde m\u00e9moire 32 Go, TOM born\u00e9).")),
    h6(i18n$t("1. Analyse du power (soft-thresholding)"), style = "font-weight:bold;"),
    sliderInput(ns("wgcna_n_genes"), i18n$t("G\u00e8nes les plus variables"),
                min = TS_BULK_WGCNA_MIN_GENES, max = TS_BULK_WGCNA_MAX_GENES,
                value = TS_BULK_WGCNA_MAX_GENES, step = 500),
    actionButton(ns("run_wgcna_power"), i18n$t("Analyser le power"),
                 class = "btn-warning w-100", icon = icon("chart-line")),
    div(class = "small text-muted mt-1", textOutput(ns("wgcna_power_status"))),

    hr(),
    h6(i18n$t("2. Modules et traits cliniques"), style = "font-weight:bold;"),
    numericInput(ns("wgcna_power_override"), i18n$t("Power (vide = retenu \u00e0 l'\u00e9tape 1)"),
                 value = NA, min = 1, max = 20, step = 1),
    selectizeInput(ns("wgcna_traits"), i18n$t("Traits cliniques (num\u00e9riques / binaires)"),
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = .tr_placeholder())),
    actionButton(ns("run_wgcna_modules"), i18n$t("Construire les modules"),
                 class = "btn-danger w-100", icon = icon("project-diagram")),
    div(class = "small text-muted mt-1", textOutput(ns("wgcna_status"))),

    hr(),
    downloadButton(ns("dl_wgcna_genes"), i18n$t("Export CSV (g\u00e8nes -> modules)"),
                   class = "btn-sm btn-info w-100"),
    downloadButton(ns("dl_wgcna_rds"), i18n$t("Export RDS (r\u00e9sultats complets)"),
                   class = "btn-sm btn-secondary w-100 mt-2")
  )
}

mod_bulk_wgcna_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header("WGCNA"),
    navset_tab(
      id = ns("wgcna_tabs"),
      nav_panel(i18n$t("Power"), plotOutput(ns("wgcna_power_fit"), height = "380px"),
                plotOutput(ns("wgcna_power_conn"), height = "380px")),
      nav_panel(i18n$t("Dendrogramme"), uiOutput(ns("wgcna_dendro_ui"))),
      nav_panel(i18n$t("Modules <-> Traits"), plotOutput(ns("wgcna_trait_plot"), height = "520px")),
      nav_panel(i18n$t("Table modules"), DTOutput(ns("wgcna_module_table")))
    )
  )
}

mod_bulk_wgcna_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSliderInput(session, "wgcna_n_genes", label = .tr("Gènes les plus variables"))
      updateActionButton(session, "run_wgcna_power", label = .tr("Analyser le power"))
      updateNumericInput(session, "wgcna_power_override",
                         label = .tr("Power (vide = retenu à l'étape 1)"))
      updateSelectizeInput(session, "wgcna_traits",
                           label = .tr("Traits cliniques (numériques / binaires)"))
      updateActionButton(session, "run_wgcna_modules", label = .tr("Construire les modules"))
      updateActionButton(session, "dl_wgcna_genes", label = .tr("Export CSV (gènes -> modules)"))
      updateActionButton(session, "dl_wgcna_rds", label = .tr("Export RDS (résultats complets)"))
    }, ignoreInit = TRUE)

    # Traits candidats : colonnes numériques ou binaires des métadonnées
    # (les multi-niveaux sont écartés par bulk_wgcna_prepare_traits — on ne
    # les propose même pas, avec l'explication en helpText du rejet).
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      ok <- vapply(names(meta), function(cl) {
        x <- meta[[cl]]
        (is.numeric(x) && sum(is.finite(x)) >= 3L) ||
          (is.factor(x) || is.character(x) || is.logical(x)) &&
            length(unique(stats::na.omit(as.character(x)))) == 2L
      }, logical(1))
      updateSelectizeInput(session, "wgcna_traits", choices = names(meta)[ok],
                           server = FALSE)
    }, ignoreNULL = TRUE)

    observe({
      shinyjs::toggleState("run_wgcna_power",   condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("run_wgcna_modules", condition = !is.null(shared_rv$wgcna_power))
      shinyjs::toggleState("dl_wgcna_genes", condition = !is.null(shared_rv$wgcna_modules))
      shinyjs::toggleState("dl_wgcna_rds",
                           condition = !is.null(shared_rv$wgcna_power) ||
                             !is.null(shared_rv$wgcna_modules))
    })

    output$wgcna_power_status <- renderText({
      global_data$language
      pw <- shared_rv$wgcna_power
      if (is.null(pw)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      else .t_fmt(.tr("\u2713 Power retenu : {p} (R2 = {r}) — {g} g\u00e8nes, {n} \u00e9chantillons."),
                  p = format(pw$chosen$power), r = round(pw$chosen$r2, 3),
                  g = format(pw$n_genes_used, big.mark = ","), n = pw$n_samples)
    })
    output$wgcna_status <- renderText({
      global_data$language
      md <- shared_rv$wgcna_modules
      if (is.null(md)) .tr("Étape 2 en attente — analyse du power d'abord.")
      else .t_fmt(.tr("\u2713 {k} modules d\u00e9tect\u00e9s sur {g} g\u00e8nes (power = {p})."),
                  k = md$n_modules, g = format(md$n_genes_used, big.mark = ","),
                  p = format(md$power))
    })

    # ── Étape 1 : power analysis ─────────────────────────────────────────
    observeEvent(input$run_wgcna_power, {
      req(shared_rv$vst_mat)
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Analyse du power (soft-thresholding)..."), value = 0.2)
      tryCatch({
        pw <- bulk_wgcna_pick_power(shared_rv$vst_mat, n_top = input$wgcna_n_genes)
        shared_rv$wgcna_power <- pw
        # Stockage récapitulatif côté bulk_obj (traçabilité de session).
        bo <- global_data$bulk_obj
        if (!is.list(bo$wgcna)) bo$wgcna <- list()
        bo$wgcna$power <- list(chosen_power = pw$chosen$power,
                               r2 = pw$chosen$r2, n_genes_used = pw$n_genes_used)
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        if (!isTRUE(pw$chosen$target_reached)) {
          showNotification(.tr(paste0("\u26a0\ufe0f Aucun power n'atteint R2 = 0.80 — le meilleur ",
                                       "compromis est retenu (voir sous-titre du graphique).")),
                           type = "warning", duration = 8)
        }
        showNotification(.t_fmt(.tr("\u2713 Power retenu : {p} (R2 = {r})."),
                                 p = format(pw$chosen$power), r = round(pw$chosen$r2, 3)),
                         type = "message")
        nav_select(id = "wgcna_tabs", selected = "wgcna_power_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur WGCNA:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$wgcna_power <- NULL
      })
    })

    # ── Étape 2 : modules + traits ───────────────────────────────────────
    observeEvent(input$run_wgcna_modules, {
      req(shared_rv$vst_mat, shared_rv$wgcna_power)
      power <- if (!is.na(input$wgcna_power_override) && !is.null(input$wgcna_power_override)) {
        input$wgcna_power_override
      } else shared_rv$wgcna_power$chosen$power
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Construction des modules (blockwiseModules)..."), value = 0.2)
      tryCatch({
        md <- bulk_wgcna_build_modules(shared_rv$vst_mat, power = power,
                                       n_top = input$wgcna_n_genes)
        shared_rv$wgcna_modules <- md
        bo <- global_data$bulk_obj
        if (!is.list(bo$wgcna)) bo$wgcna <- list()
        bo$wgcna$modules <- list(power = md$power, n_modules = md$n_modules,
                                 module_sizes = as.list(md$module_sizes))
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        showNotification(.t_fmt(.tr("\u2713 {k} modules d\u00e9tect\u00e9s."),
                                 k = md$n_modules), type = "message")
        nav_select(id = "wgcna_tabs", selected = "wgcna_trait_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur WGCNA:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$wgcna_modules <- NULL
      })
    })

    # ── Corrélations MEs <-> traits (recalculées à chaque sélection) ─────
    wgcna_trait_cor <- reactive({
      req(shared_rv$wgcna_modules)
      tr_sel <- input$wgcna_traits
      prep <- bulk_wgcna_prepare_traits(global_data$bulk_obj$metadata,
                                        candidate_cols = if (length(tr_sel)) tr_sel else NULL)
      bulk_wgcna_module_trait(shared_rv$wgcna_modules, prep$traits)
    }) %>% shiny::debounce(500)

    # ── Outputs ──────────────────────────────────────────────────────────
    output$wgcna_power_fit <- renderPlot({
      global_data$language
      req(shared_rv$wgcna_power)
      plots <- plot_wgcna_soft_threshold(shared_rv$wgcna_power, tr = .tr_fn(global_data))
      print(plots$fit)
    })
    output$wgcna_power_conn <- renderPlot({
      global_data$language
      req(shared_rv$wgcna_power)
      plots <- plot_wgcna_soft_threshold(shared_rv$wgcna_power, tr = .tr_fn(global_data))
      print(plots$connectivity)
    })
    output$wgcna_dendro_ui <- renderUI({
      global_data$language
      if (is.null(shared_rv$wgcna_modules)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ",
                   .tr("Construisez d'abord les modules (étape 2 du panneau de gauche).")))
      }
      plotOutput(ns("wgcna_dendro"), height = "700px")
    })
    output$wgcna_dendro <- renderPlot({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      # Dendrogramme + bande de couleurs — dessin WGCNA (base graphics).
      tryCatch({
        cols <- md$dendro_colors %||% md$colors
        WGCNA::plotDendroAndColors(
          md$dendro, cols, groupLabels = .tr("Modules"),
          dendroLabels = FALSE, cex.dendroLabels = 0.6,
          cex.colorLabels = 0.8, marAll = c(2, 5, 2, 2))
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste(.tr("Erreur:"), conditionMessage(e)), cex = 0.9)
      })
    })
    output$wgcna_trait_plot <- renderPlot({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      tryCatch(
        print(plot_wgcna_trait_heatmap(wgcna_trait_cor(), tr = .tr_fn(global_data))),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            ggplot2::theme_void()
        }
      )
    })
    output$wgcna_module_table <- renderDT({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      sizes <- as.data.frame(md$module_sizes)
      colnames(sizes) <- c("Module", "Gènes")
      sizes <- sizes[order(sizes$Module), ]
      DT::datatable(sizes, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$dl_wgcna_genes <- downloadHandler(
      filename = function() paste0("wgcna_gene_modules_", Sys.Date(), ".csv"),
      content  = function(file) {
        md <- shared_rv$wgcna_modules; req(md)
        tc <- tryCatch(wgcna_trait_cor(), error = function(e) NULL)
        write.csv(build_wgcna_export(md, tc), file, row.names = FALSE)
      }
    )
    output$dl_wgcna_rds <- downloadHandler(
      filename = function() paste0("wgcna_results_", Sys.Date(), ".rds"),
      content  = function(file) {
        out <- list(power = shared_rv$wgcna_power, modules = shared_rv$wgcna_modules,
                    trait_cor = tryCatch(wgcna_trait_cor(), error = function(e) NULL),
                    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
        saveRDS(out, file)
      }
    )

  }) # /moduleServer
}
