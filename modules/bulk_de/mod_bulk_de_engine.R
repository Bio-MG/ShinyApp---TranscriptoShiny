# =============================================================================
# mod_bulk_de_engine.R — Bulk Child 2: control-plane / shared plumbing
# (i18n Phase 3.1 — central language observer)
# =============================================================================
# Owns everything that is NOT specific to "running a DE contrast" or to one
# particular output tab:
#   - DE engine availability + selection (DESeq2/edgeR/limma-voom)
#   - Metadata-driven choices (condition_col, covariates, heatmap_annot)
#   - group_ref/group_target sync when condition_col changes
#   - Design formula preview + proactive readiness/validation banner
#   - Mirroring scalars (lfc_thresh, padj_thresh, heatmap_top_n,
#     heatmap_annot, active_condition_col) to shared_rv, read by
#     mod_bulk_report.R across module namespaces
#   - "Contraste actif" selector sync (keeps the dropdown in sync with
#     shared_rv$contrasts regardless of WHO wrote it — run_de, pairwise, or
#     the auto-pipeline in mod_bulk.R writing shared_rv$contrasts directly)
#   - de_status text output
#   - .de_make_helpers(): factory for the two small closures shared by
#     mod_bulk_de_run.R, mod_bulk_de_pairwise.R and mod_bulk_de_multimethod.R
#     (design_str(), register_contrast()) — single source of truth instead
#     of duplicating them in three files.
#
# i18n Phase 3.1:
#   - One central observeEvent(global_data$language) pushes ALL DE input
#     labels/choices via update*Input (all .de_*_server share same session).
#   - Engine availability + de_status translated via session-scoped .tr().
#   - Static choice labels use .tr_plain() pattern (values stay ASCII).
#
# Depends on helpers_bulk.R: validate_bulk_design(), check_design_confounding().
# Depends on global.R: has_limma, .strip_i18n_html, .tr_plain, .t_fmt.
# =============================================================================

#' Build the two small closures shared across the DE sub-modules
#'
#' @param input Shiny `input` object for the "de" module namespace.
#' @param shared_rv reactiveValues shared across bulk sibling modules.
#' @return list(design_str = function(), register_contrast = function(name, res))
.de_make_helpers <- function(input, shared_rv) {
  design_str <- function() {
    terms <- unique(c(input$covariates, input$condition_col))
    paste0("~ ", paste(terms, collapse = " + "))
  }
  register_contrast <- function(name, res) {
    current <- shared_rv$contrasts
    current[[name]] <- res
    shared_rv$contrasts <- current
  }
  list(design_str = design_str, register_contrast = register_contrast)
}

#' Control-plane server logic (see file header for scope)
#' @param input,output,session,ns Standard Shiny module pieces (from the
#'   PARENT moduleServer(id, ...) call in mod_bulk_de.R — this is a plain R
#'   function, not a nested Shiny module, so it shares the exact same
#'   reactive namespace as every other `.de_*_server()` sibling function).
#' @param global_data,shared_rv App-wide and bulk-wide reactiveValues.
.de_engine_server <- function(input, output, session, ns, global_data, shared_rv) {

  # ── Engine availability (graceful degradation, language-aware) ─────────
  available_engines <- reactive({
    global_data$language
    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }
    c(
      if (requireNamespace("DESeq2", quietly = TRUE)) stats::setNames(c("deseq2"), c(.trl("DESeq2 (recommand\u00e9)"))),
      if (requireNamespace("edgeR",  quietly = TRUE)) c("edgeR" = "edger"),
      if (has_limma)                                  c("limma-voom" = "limma")
    )
  })

  observe({
    eng <- available_engines()
    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }
    validate(need(length(eng) > 0, .trl("Aucun moteur DE disponible (installez DESeq2, edgeR ou limma).")))
    updateSelectInput(session, "de_engine", choices = eng, selected = eng[1])
  })

  # ── i18n (Phase 3.1): central label push for the whole DE namespace ────
  # All .de_*_server() functions share this moduleServer's session/input, so
  # one observer pushes labels for every DE input on language switch.
  observeEvent(global_data$language, {
    .trl <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }
    updateSelectInput(session, "condition_col", label = .trl("Variable de Condition (principale)"))
    updateSelectizeInput(session, "covariates", label = .trl("Covariables additionnelles (optionnel)"))
    updateSelectInput(session, "group_ref",    label = .trl("Groupe R\u00e9f\u00e9rence"))
    updateSelectInput(session, "group_target", label = .trl("Groupe Cible"))
    updateSelectInput(session, "de_engine",    label = .trl("Moteur Statistique"))
    updateCheckboxInput(session, "shrink_lfc",  label = .trl("Shrinkage LFC (apeglm) \u2014 DESeq2 uniquement"))
    updateNumericInput(session, "lfc_thresh",   label = .trl("Seuil |Log2FC|"))
    updateNumericInput(session, "padj_thresh",  label = .trl("Seuil p-adj"))
    updateTextInput(session, "contrast_name",    label = .trl("Nom du contraste (auto si vide)"))
    updateTextInput(session, "adhoc_contrast_name", label = .trl("Nom du contraste"))
    updateCheckboxGroupInput(session, "adhoc_group_a", label = .trl("Groupe A"))
    updateCheckboxGroupInput(session, "adhoc_group_b", label = .trl("Groupe B"))
    # heatmap tab inputs
    updateNumericInput(session, "heatmap_top_n", label = .trl("Top N g\u00e8nes (par p-adj)"))
    updateSelectizeInput(session, "heatmap_annot", label = .trl("Annotation colonnes"))
    updateSelectInput(session, "heatmap_direction", label = .trl("Sous-ensemble"),
      choices = stats::setNames(c("all","sig","up","down","ns"),
        c(.trl("Tous (par p-adj)"), .trl("Significatifs (Up+Down)"),
          .trl("Up-r\u00e9gul\u00e9s"), .trl("Down-r\u00e9gul\u00e9s"), .trl("Non-significatifs"))))
    updateSelectInput(session, "heatmap_export_fmt", label = .trl("Format export"))
    # venn tab
    updateSelectizeInput(session, "venn_contrasts", label = .trl("Contrastes \u00e0 comparer"))
    updateRadioButtons(session, "venn_type", label = .trl("Type de diagramme"),
      choices = stats::setNames(c("upset","venn"),
        c(.trl("UpSet (recommand\u00e9)"), .trl("Venn (2-4 contrastes)"))))
    updateCheckboxInput(session, "venn_direction_aware",
      label = .trl("Distinguer Up / Down (chaque contraste devient 2 ensembles)"))
    # multimethod tab
    updateRadioButtons(session, "mm_venn_type", label = .trl("Type de diagramme"),
      choices = stats::setNames(c("upset","venn"),
        c(.trl("UpSet (recommand\u00e9)"), .trl("Venn (2-3 m\u00e9thodes)"))))
  }, ignoreInit = TRUE)

  # ── Refresh metadata-driven choices when bulk_obj changes ────────────────
  observeEvent(global_data$bulk_obj, {
    req(global_data$bulk_obj, global_data$bulk_obj$metadata)
    meta <- global_data$bulk_obj$metadata
    cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
    cat_cols <- if (length(cat_cols) == 0) names(meta) else cat_cols

    updateSelectInput(session, "condition_col", choices = cat_cols)
    updateSelectizeInput(session, "covariates",  choices = cat_cols, server = TRUE)
    updateSelectizeInput(session, "heatmap_annot", choices = cat_cols, server = FALSE)
  }, ignoreNULL = TRUE)

  # ── Update group_ref/group_target when condition_col changes ────────────
  observeEvent(input$condition_col, {
    req(global_data$bulk_obj, input$condition_col)
    meta <- global_data$bulk_obj$metadata
    lvls <- unique(na.omit(as.character(meta[[input$condition_col]])))
    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }
    validate(need(length(lvls) >= 2, .trl("La colonne de condition doit avoir au moins 2 niveaux.")))
    updateSelectInput(session, "group_ref",    choices = lvls, selected = lvls[1])
    updateSelectInput(session, "group_target", choices = lvls, selected = lvls[min(2, length(lvls))])
  })

  # ── Mirror report-relevant scalars to shared_rv ─────────────────────────
  observe({
    shared_rv$lfc_thresh           <- input$lfc_thresh
    shared_rv$padj_thresh          <- input$padj_thresh
    shared_rv$heatmap_top_n        <- input$heatmap_top_n
    shared_rv$heatmap_annot        <- input$heatmap_annot
    shared_rv$active_condition_col <- input$condition_col  # Step-3.0: used by R script export
  })

  # ── Design formula preview ───────────────────────────────────────────────
  output$design_formula_preview <- renderText({
    req(input$condition_col)
    terms <- c(input$covariates, input$condition_col)
    paste0("Design: ~ ", paste(unique(terms), collapse = " + "))
  })

  # ── Proactive validation banner (didactic — shown BEFORE the user clicks
  #    "Lancer l'Analyse Différentielle", not after a cryptic crash) ───────
  output$de_readiness_check <- renderUI({
    global_data$language
    req(global_data$bulk_obj, input$condition_col)
    meta <- global_data$bulk_obj$metadata

    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }
    issues <- character(0)
    if (is.null(shared_rv$filtered_counts)) {
      issues <- c(issues, .trl("\u00c9tape 1 (Filtrage & VST) non lanc\u00e9e \u2014 obligatoire avant le calcul DE."))
    }
    if (input$condition_col %in% colnames(meta)) {
      issues <- c(issues, validate_bulk_design(meta, input$condition_col, input$covariates %||% character(0)))
    }

    if (length(issues) == 0) return(NULL)
    div(class = "alert alert-warning", style = "font-size:0.82em;",
        icon("triangle-exclamation"), tags$strong(paste0(" ", .trl("Avant de lancer :"))),
        tags$ul(lapply(issues, tags$li)))
  })

  # ── "Contraste actif" selector sync — keeps the dropdown in sync with
  #    shared_rv$contrasts regardless of WHO wrote it (run_de / pairwise /
  #    ad-hoc all live in other files now; the auto-pipeline in mod_bulk.R
  #    writes shared_rv$contrasts directly and has no way to reach into this
  #    module's own input namespace) ────────────────────────────────────────
  observeEvent(input$active_contrast_view, {
    req(input$active_contrast_view %in% names(shared_rv$contrasts))
    shared_rv$active_contrast <- input$active_contrast_view
  })

  observeEvent(shared_rv$contrasts, {
    nm  <- names(shared_rv$contrasts)
    cur <- shared_rv$active_contrast
    sel <- if (!is.null(cur) && cur %in% nm) cur else if (length(nm) > 0) nm[1] else NULL
    updateSelectInput(session, "active_contrast_view", choices = nm, selected = sel)
  }, ignoreNULL = FALSE)

  output$de_status <- renderText({
    global_data$language
    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }
    if (length(shared_rv$contrasts) == 0) .trl("Aucun contraste calcul\u00e9.")
    else paste(.trl("Contraste actif:"), shared_rv$active_contrast %||% "-")
  })
}
