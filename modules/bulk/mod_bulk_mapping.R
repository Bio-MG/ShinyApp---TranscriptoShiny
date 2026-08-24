# =============================================================================
# mod_bulk_mapping.R — Bulk Child 0 (Optional): Gene identifier mapping
# =============================================================================
# i18n PILOT MODULE (Phase 2) — "JS-shim aware" architecture:
#
#   1. STATIC UI (mod_bulk_mapping_ui):
#      - Input LABELS and choice DISPLAY NAMES use .tr_plain() -> plain
#        length-1 scalars at build time (NEVER i18n$t() inside setNames():
#        at UI-build time $t() emits a length-3 <span> fragment and crashes
#        stats::setNames() — see i18n post-mortem).
#      - Choice VALUES ("human"/"mouse"/"ensembl"/...) are untranslated
#        ASCII constants — backend contract, never localized.
#
#   2. SERVER-SIDE LANGUAGE PUSH (the core pattern for Phase 3 rollout):
#      observeEvent(global_data$language) re-sends every input label and
#      choice display name via update*Input() in the CURRENT session
#      language. This works with or without the client-side JS shim, and
#      update*Input() preserves the selected VALUE (values never change).
#
#   3. DYNAMIC CONTENT (renderUI / notifications):
#      - global_data$i18n$t() via .tr() — session-scoped Translator,
#        Path A, always plain strings.
#      - Every renderUI reads global_data$language -> re-renders on switch.
#      - Strings stored in shared_rv stay LANGUAGE-NEUTRAL
#        (mapping_summary_data); the sentence is rebuilt at display time.
#
#   TEST INTEGRITY: all ns() input IDs identical to pre-i18n version.
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Server-rendered intro alert (switches language even if the JS shim
    # does not fire — rebuilt whenever global_data$language changes).
    uiOutput(ns("mapping_intro_ui")),

    uiOutput(ns("detected_id_type_ui")),

    fluidRow(
      column(6, selectInput(ns("map_organism"), .tr_plain("Organisme"),
                            choices = stats::setNames(
                              c("human", "mouse"),
                              c(.tr_plain("Humain"), .tr_plain("Souris"))))),
      column(6, selectInput(ns("map_from_type"), .tr_plain("Type source"),
                            choices = stats::setNames(
                              c("ensembl", "entrez", "affy_probe"),
                              c(.tr_plain("Ensembl Gene ID"),
                                .tr_plain("Entrez ID"),
                                .tr_plain("Probe Affymetrix")))))
    ),

    checkboxInput(ns("strip_ensembl_version"),
                  .tr_plain("Retirer le suffixe de version Ensembl (ENSG...5 → ENSG...)"),
                  value = TRUE),

    radioButtons(ns("collapse_method"),
                 .tr_plain("Fusion des doublons (plusieurs ID → même symbole)"),
                 choices = stats::setNames(
                   c("sum", "max_mean"),
                   c(.tr_plain("Somme des counts (recommandé)"),
                     .tr_plain("Garder l'ID le plus exprimé")))),

    actionButton(ns("run_mapping"),
                 label = paste("\U0001f504", .tr_plain("Appliquer le Mapping")),
                 class = "btn-info w-100"),
    uiOutput(ns("undo_btn_ui")),

    div(class = "mt-2", uiOutput(ns("mapping_status")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_mapping_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Session-scoped scalar translation. Inside moduleServer a reactive
    # domain EXISTS, so global_data$i18n$t() takes Path A and returns a
    # plain string in the CURRENT session language — never an HTML span.
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n: push translated labels/choices on EVERY language change ──────
    # Values NEVER change (backend contract); only display names move.
    # update*Input() preserves the current selection since values persist.
    observeEvent(global_data$language, {
      updateSelectInput(session, "map_organism",
        label = .tr("Organisme"),
        choices = stats::setNames(c("human", "mouse"),
                                  c(.tr("Humain"), .tr("Souris"))))
      updateSelectInput(session, "map_from_type",
        label = .tr("Type source"),
        choices = stats::setNames(c("ensembl", "entrez", "affy_probe"),
                                  c(.tr("Ensembl Gene ID"), .tr("Entrez ID"),
                                    .tr("Probe Affymetrix"))))
      updateCheckboxInput(session, "strip_ensembl_version",
        label = .tr("Retirer le suffixe de version Ensembl (ENSG...5 → ENSG...)"))
      updateRadioButtons(session, "collapse_method",
        label = .tr("Fusion des doublons (plusieurs ID → même symbole)"),
        choices = stats::setNames(c("sum", "max_mean"),
                                  c(.tr("Somme des counts (recommandé)"),
                                    .tr("Garder l'ID le plus exprimé"))))
      updateActionButton(session, "run_mapping",
        label = paste("\U0001f504", .tr("Appliquer le Mapping")))
    }, ignoreInit = TRUE)

    # ── Intro alert — server-rendered (fully bilingual, no JS-shim reliance) ─
    output$mapping_intro_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      div(class = "alert alert-light", style = "font-size:0.82em;",
          bsicons::bs_icon("info-circle"),
          " ", .tr("Étape facultative — uniquement si vos identifiants de gènes ne sont PAS déjà des symboles (ex: ENSG00000141510, 7157, '1007_s_at'). Si vos données utilisent déjà des symboles (TP53, ACTB...), ignorez cette étape."))
    })

    # ── Auto-detect probable ID type as soon as raw counts are available ────
    output$detected_id_type_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(global_data$bulk_obj)
      ids        <- rownames(global_data$bulk_obj$counts)
      detected   <- tryCatch(detect_gene_id_type(ids), error = function(e) "unknown")
      example_id <- if (length(ids) > 0) ids[1] else "?"

      if (detected == "unknown") {
        return(div(class = "alert alert-danger", style = "font-size:0.8em;",
          icon("circle-exclamation"),
          " ", .t_fmt(.tr("Type d'identifiant NON reconnu (ex. trouvé dans vos données : {example}). Si ce ne sont PAS des identifiants de gènes (ex: comptages personnalisés, autre type de feature), ignorez cette étape facultative et passez directement à l'étape 1."),
                      example = example_id)))
      }
      label <- switch(detected,
        ensembl    = .tr("Ensembl Gene ID"),
        entrez     = .tr("Entrez ID"),
        affy_probe = .tr("Probe Affymetrix"),
        symbol     = .tr("Symbole — déjà mappé, cette étape est probablement inutile")
      )
      cls <- if (detected == "symbol") "alert-success" else "alert-info"
      div(class = paste("alert", cls), style = "font-size:0.8em;",
          .t_fmt(.tr("Type détecté automatiquement : {label} (ex: '{example}')."),
                 label = label, example = example_id))
    })

    # Pre-select the detected type (values only — i18n-neutral by design).
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj)
      detected <- tryCatch(detect_gene_id_type(rownames(global_data$bulk_obj$counts)),
                           error = function(e) "unknown")
      if (detected %in% c("ensembl", "entrez", "affy_probe")) {
        updateSelectInput(session, "map_from_type", selected = detected)
      }
    }, ignoreNULL = TRUE)

    # =========================================================================
    # STEP 0 — Apply mapping
    # =========================================================================
    observeEvent(input$run_mapping, {
      req(global_data$bulk_obj)
      counts_orig <- global_data$bulk_obj$counts

      ids <- rownames(counts_orig)
      if (isTRUE(input$strip_ensembl_version) && input$map_from_type == "ensembl") {
        ids <- gsub("\\.[0-9]+$", "", ids)
      }
      counts_work <- counts_orig
      rownames(counts_work) <- ids

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Mapping des identifiants..."), value = 0.3)

      tryCatch({
        result <- remap_gene_ids_to_symbol(
          counts_work, from_type = input$map_from_type,
          organism = input$map_organism, collapse_method = input$collapse_method
        )
        n_total      <- result$n_mapped + result$n_unmapped
        pct_unmapped <- if (n_total > 0) 100 * result$n_unmapped / n_total else 0

        if (pct_unmapped > 20) {
          showNotification(
            .t_fmt(.tr("⚠️ {pct}% des identifiants n'ont pas pu être mappés. Vérifiez l'organisme et le type source."),
                   pct = sprintf("%.0f", pct_unmapped)),
            type = "warning", duration = 10)
        }

        # Backup the TRUE original exactly once (true undo).
        if (is.null(shared_rv$counts_original)) {
          shared_rv$counts_original <- counts_orig
        }
        shared_rv$counts_mapped   <- result$matrix
        shared_rv$mapping_applied <- TRUE

        # i18n-safe STATE: language-neutral numbers. The display sentence is
        # rebuilt at render time in the CURRENT language (mapping_status).
        shared_rv$mapping_summary_data <- list(
          n_mapped = result$n_mapped, n_unmapped = result$n_unmapped,
          pct_unmapped = pct_unmapped, n_collapsed = result$n_collapsed,
          n_final = nrow(result$matrix)
        )
        # Legacy display field, kept for non-migrated writers (mod_bulk.R
        # auto-pipeline writes it directly — migrated in Phase 3.1).
        shared_rv$mapping_summary <- .summary_text(shared_rv$mapping_summary_data)

        showNotification(.tr("✓ Mapping appliqué — relancez l'étape 1 (Filtrage & VST) pour l'utiliser."),
                         type = "message", duration = 7)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur de mapping:"), conditionMessage(e)),
                         type = "error", duration = 15)
      })
    })

    # Summary sentence builder (single dictionary key, named placeholders).
    .summary_key <- "✓ {mapped} mappés, {unmapped} non-mappés ({pct}%), {collapsed} fusion(s) de doublon → {final} gènes finaux. Relancez l'étape 1 (Filtrage & VST) pour appliquer."
    .summary_text <- function(d) {
      .t_fmt(.tr(.summary_key),
             mapped = d$n_mapped, unmapped = d$n_unmapped,
             pct = sprintf("%.1f", d$pct_unmapped),
             collapsed = d$n_collapsed, final = d$n_final)
    }

    # ── Undo ──────────────────────────────────────────────────────────────────
    observeEvent(input$undo_mapping, {
      shared_rv$counts_mapped      <- NULL
      shared_rv$mapping_applied    <- FALSE
      shared_rv$mapping_summary    <- NULL
      shared_rv$mapping_summary_data <- NULL
      showNotification(.tr("↩ Mapping annulé — relancez l'étape 1 pour revenir aux identifiants originaux."),
                       type = "message", duration = 6)
    })

    output$undo_btn_ui <- renderUI({
      global_data$language  # i18n
      if (isTRUE(shared_rv$mapping_applied)) {
        actionButton(ns("undo_mapping"),
                     label = paste("\u21a9", .tr("Annuler le Mapping")),
                     class = "btn-outline-danger btn-sm w-100 mt-1")
      } else NULL
    })

    output$mapping_status <- renderUI({
      global_data$language  # i18n: rebuild in the CURRENT language
      d <- shared_rv$mapping_summary_data
      if (!is.null(d)) {
        return(div(class = "alert alert-success", style = "font-size:0.8em;",
                   .summary_text(d)))
      }
      if (!is.null(shared_rv$mapping_summary)) {
        # Legacy writer path (auto-pipeline pre-Phase 3.1): display as-is.
        return(div(class = "alert alert-success", style = "font-size:0.8em;",
                   shared_rv$mapping_summary))
      }
      tags$em(.tr("Aucun mapping appliqué."), style = "color:#999;font-size:0.82em;")
    })

  }) # /moduleServer
}
