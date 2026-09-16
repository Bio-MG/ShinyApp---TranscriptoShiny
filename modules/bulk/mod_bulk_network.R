# =============================================================================
# modules/bulk/mod_bulk_network.R — Réseau PCSF / interactome (NEW-3)
# =============================================================================
# UI + orchestration UNIQUEMENT : tout le calcul vit dans R/bulk/bulk_network.R
# (logique pure, contract-first — docs/contracts/BULK_NETWORK_CONTRACT.md).
#
# ⚠️ SÉMANTIQUE AFFICHÉE, PAS SEULEMENT DOCUMENTÉE. Le réseau est DÉRIVÉ DES
# VOIES Reactome : un relais est un CO-MEMBRE DE VOIE, pas un partenaire
# d'interaction physique. L'écran doit le dire, sinon le résultat sera lu comme
# un PPI — c'est la limite biologique n°1 de la proposition §9.
#
# Primes : résultat DE du contraste actif (même convention que
# mod_bulk_pathways / mod_bulk_pattern). Espèce DÉCLARÉE (pas de défaut deviné).
# Le moteur est une HEURISTIQUE : l'UI l'écrit et ne promet aucun optimum.
# =============================================================================

mod_bulk_network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning", style = "font-size:0.9em;border-left:3px solid #E67E22;",
        strong(.tr_plain("Interactome dérivé des voies Reactome — ce n'est PAS un PPI.")),
        br(),
        .tr_plain("Une arête signifie « ces deux protéines participent à la même voie », pas « interaction physique démontrée ». Les nœuds gris (relais) sont des CO-MEMBRES DE VOIE prédits, pas des partenaires de liaison.")),
    selectInput(ns("network_species"), i18n$t("Espèce (déclarée)"),
                choices = setNames("hsapiens", .tr_plain("Humain (hsapiens)")),
                selected = "hsapiens"),
    radioButtons(ns("network_source"), i18n$t("Source des gènes d'intérêt"),
                 choices = setNames(c("all_sig", "up", "down"),
                                    c(.tr_plain("Tous les significatifs"),
                                      .tr_plain("Surexprimés (up)"),
                                      .tr_plain("Sous-exprimés (down)")))),
    radioButtons(ns("network_prize"), i18n$t("Prime (score du nœud)"),
                 choices = setNames(c("padj", "lfc"),
                                    c(.tr_plain("-log10(padj) — recommandé"),
                                      .tr_plain("|log2FC| — direction ignorée")))),
    numericInput(ns("network_threshold"), i18n$t("Seuil de prime (déclaré)"),
                 value = 1.3, min = 0, step = 0.1),
    div(class = "small text-muted",
        .tr_plain("Seuls les gènes dont la prime DÉPASSE strictement ce seuil deviennent des primes. 1,3 = p = 0,05 sur l'échelle -log10(padj).")),
    hr(),
    h6(i18n$t("Paramètres du moteur (heuristique)")),
    fluidRow(
      column(4, numericInput(ns("network_omega"), i18n$t("ω (ouverture d'arbre)"),
                             value = 10, min = 0, step = 1)),
      column(4, numericInput(ns("network_beta"), i18n$t("β (par arête)"),
                             value = 1, min = 0, step = 0.5)),
      column(4, numericInput(ns("network_mu"), i18n$t("μ (par relais)"),
                             value = 1, min = 0, step = 0.5))
    ),
    div(class = "small text-muted", textOutput(ns("network_hops_hint"))),
    actionButton(ns("run_network"), i18n$t("Calculer le sous-réseau"),
                 class = "btn-warning w-100", icon = icon("project-diagram")),
    hr(),
    div(class = "small text-muted", textOutput(ns("network_status")))
  )
}

mod_bulk_network_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Sous-réseau PCSF (heuristique)"), class = "mb-0"),
                    downloadButton(ns("dl_network"), i18n$t("Export CSV"),
                                   class = "btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Réseau"), plotOutput(ns("network_plot"), height = "600px")),
      nav_panel(i18n$t("Nœuds"), DTOutput(ns("network_table"))),
      nav_panel(i18n$t("Paramètres & QC"), DTOutput(ns("network_qc")))
    )
  )
}

mod_bulk_network_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "network_species",
        label = .tr("Espèce (déclarée)"),
        choices = setNames("hsapiens", .tr("Humain (hsapiens)")),
        selected = isolate(input$network_species) %||% "hsapiens")
      updateRadioButtons(session, "network_source",
        label = .tr("Source des gènes d'intérêt"),
        choices = setNames(c("all_sig", "up", "down"),
                           c(.tr("Tous les significatifs"),
                             .tr("Surexprimés (up)"),
                             .tr("Sous-exprimés (down)"))),
        selected = isolate(input$network_source) %||% "all_sig")
      updateRadioButtons(session, "network_prize",
        label = .tr("Prime (score du nœud)"),
        choices = setNames(c("padj", "lfc"),
                           c(.tr("-log10(padj) — recommandé"),
                             .tr("|log2FC| — direction ignorée"))),
        selected = isolate(input$network_prize) %||% "padj")
      updateNumericInput(session, "network_threshold",
                         label = .tr("Seuil de prime (déclaré)"))
      updateNumericInput(session, "network_omega",
                         label = .tr("ω (ouverture d'arbre)"))
      updateNumericInput(session, "network_beta", label = .tr("β (par arête)"))
      updateNumericInput(session, "network_mu", label = .tr("μ (par relais)"))
      updateActionButton(session, "run_network",
                         label = .tr("Calculer le sous-réseau"))
    }, ignoreInit = TRUE)

    # Rend le critère de rentabilité LISIBLE : « ω = 10 » ne dit rien, « les
    # groupes séparés par au plus 5 arêtes peuvent être reliés » dit tout.
    output$network_hops_hint <- renderText({
      global_data$language
      p <- tryCatch(
        bulk_network_pcsf_params(list(omega = input$network_omega %||% 10,
                                      beta  = input$network_beta  %||% 1,
                                      mu    = input$network_mu    %||% 1)),
        error = function(e) NULL)
      if (is.null(p)) return(.tr("Paramètres invalides : ω, β et μ doivent être des nombres >= 0."))
      .t_fmt(.tr("Deux groupes ne sont reliés que s'ils sont séparés par au plus {d} arête(s)."),
             d = p$max_join_hops)
    })

    observe({
      shinyjs::toggleState("run_network", condition = !is.null(shared_rv$vst_mat))
    })

    # Contraste DE actif — même convention que mod_bulk_pathways / pattern
    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    output$network_status <- renderText({
      global_data$language
      res <- shared_rv$network_result
      if (is.null(res)) {
        .tr("En attente — lancez d'abord l'étape 1 (Filtrage & VST) et l'étape 2 (Analyse Différentielle).")
      } else {
        warn_txt <- if (length(res$warnings)) {
          paste0(" ", .t_fmt(.tr("{n} avertissement(s)."), n = length(res$warnings)))
        } else {
          ""
        }
        paste0(
          .t_fmt(.tr("✓ {n} nœuds ({t} primes, {r} relais) et {e} arêtes en {c} composante(s)."),
                 n = res$qc$n_nodes, t = res$qc$n_terminals,
                 r = res$qc$n_relay, e = res$qc$n_edges, c = res$qc$n_trees),
          warn_txt)
      }
    })

    observeEvent(input$run_network, {
      req(shared_rv$vst_mat)
      res_de <- .active_de_results()
      if (is.null(res_de)) {
        showNotification(.tr("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle)."),
                         type = "warning")
        return()
      }
      p <- shiny::Progress$new(); on.exit(p$close())
      tryCatch({
        # Primes : le seuil de significativité reste celui de l'app (padj/LFC),
        # le seuil de PRIME est celui déclaré ci-dessus — les deux sont distincts
        # et ne doivent pas être confondus.
        sig <- res_de$padj < (shared_rv$padj_thresh %||% 0.05) &
          abs(res_de$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
        sig[is.na(sig)] <- FALSE
        keep <- switch(input$network_source,
                       up      = sig & res_de$log2FoldChange > 0,
                       down    = sig & res_de$log2FoldChange < 0,
                       all_sig = sig)
        keep[is.na(keep)] <- FALSE
        genes <- unique(trimws(as.character(res_de$gene[keep])))
        genes <- genes[nzchar(genes)]
        if (!length(genes)) {
          showNotification(.tr("⚠️ Aucun gène significatif pour cette source — élargissez les seuils de l'étape 2."),
                           type = "warning", duration = 6)
          return()
        }
        # Prime : -log10(padj) (recommandée, monotone) ou |log2FC|
        pri <- if (identical(input$network_prize, "lfc")) {
          abs(res_de$log2FoldChange[match(genes, as.character(res_de$gene))])
        } else {
          -log10(res_de$padj[match(genes, as.character(res_de$gene))])
        }
        prizes <- stats::setNames(pri, genes)
        prizes <- prizes[!is.na(prizes)]

        p$set(message = .tr("Chargement du réseau de voies (Reactome, hors ligne)..."),
              value = 0.25)
        net <- load_bulk_network(species = input$network_species)
        p$set(message = .tr("Calcul du sous-réseau PCSF (heuristique)..."),
              value = 0.6)
        res <- run_bulk_network_pcsf(
          prizes     = prizes,
          network    = net,
          species    = input$network_species,
          threshold  = input$network_threshold,
          params     = list(omega = input$network_omega,
                            beta  = input$network_beta,
                            mu    = input$network_mu),
          convert_ids = TRUE)
        assert_bulk_network_result(res, context = "module réseau PCSF")
        shared_rv$network_result <- res
        p$set(value = 1)
        for (w in res$warnings) {
          showNotification(w, type = "warning", duration = 8)
        }
        showNotification(.t_fmt(
          .tr("✓ {n} nœuds retenus ({t} primes, {r} relais). Heuristique — ce n'est pas un optimum."),
          n = res$qc$n_nodes, t = res$qc$n_terminals, r = res$qc$n_relay),
          type = "message")
        shared_rv$active_tab <- "tab_bulk_network"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur réseau PCSF:"), conditionMessage(e)),
                         type = "error", duration = 12)
        shared_rv$network_result <- NULL
      })
    })

    output$network_plot <- renderPlot({
      global_data$language
      req(shared_rv$network_result)
      plot_bulk_network(shared_rv$network_result, tr = .tr)
    })

    output$network_table <- renderDT({
      req(shared_rv$network_result)
      ts_datatable(build_bulk_network_table_export(shared_rv$network_result),
                   page_length = 15L, filename_base = "pcsf_nodes")
    })

    output$network_qc <- renderDT({
      req(shared_rv$network_result)
      r <- shared_rv$network_result
      qc <- r$qc
      tab <- data.frame(
        param = c("algorithme", "seuil de prime", "primes en entrée",
                  "primes non positives", "sous le seuil", "hors réseau",
                  "terminaux", "relais", "nœuds", "arêtes", "composantes",
                  "prime totale", "score", "liaison max (arêtes)",
                  "nœuds du réseau", "arêtes du réseau",
                  "taux de correspondance", "espèce", "source", "version source"),
        valeur = c(qc$algorithm, format(qc$threshold), qc$n_prizes_input,
                   qc$n_prizes_non_positive, qc$n_prizes_below_threshold,
                   qc$n_prizes_outside_network, qc$n_terminals, qc$n_relay,
                   qc$n_nodes, qc$n_edges, qc$n_trees,
                   round(qc$total_prize, 3), round(qc$score, 3),
                   qc$max_join_hops, qc$network_nodes, qc$network_edges,
                   if (is.na(r$map_rate)) "n/a" else sprintf("%.1f %%", 100 * r$map_rate),
                   r$species, r$source_db, r$source_version),
        stringsAsFactors = FALSE)
      ts_datatable(tab, page_length = 25L, filename_base = "pcsf_qc")
    })

    output$dl_network <- downloadHandler(
      filename = function() {
        req(shared_rv$network_result)
        paste0("pcsf_network_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(shared_rv$network_result)
        utils::write.csv(build_bulk_network_table_export(shared_rv$network_result),
                         file, row.names = FALSE)
      }
    )
  })
}
