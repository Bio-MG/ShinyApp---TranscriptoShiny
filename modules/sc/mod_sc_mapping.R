# =============================================================================
# mod_sc_mapping.R  —  SC Child 0 (Optional): Mapping d'identifiants de gènes
# =============================================================================
# Mirrors mod_bulk_mapping.R for Single-Cell. Operates directly on
# global_data$sc_obj (rebuilds it from raw counts via remap_seurat_ids_to_symbol()
# in helpers_sc.R) — must run BEFORE i18n$t("1. Pipeline") since normalisation/PCA/
# clusters are invalidated when rownames change.
# =============================================================================

mod_sc_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light", style="font-size:0.82em;",
        bsicons::bs_icon("info-circle"),
        span(i18n$t("Étape facultative — uniquement si vos identifiants ne sont PAS déjà des symboles (ex: ENSG00000141510, 7157).")),
        " ",
        tags$strong(i18n$t("À lancer AVANT l'étape '1. Pipeline' :")),
        " ",
        span(i18n$t("toute normalisation ou clustering déjà calculé sera réinitialisé."))),

    uiOutput(ns("detected_id_type_ui")),

    fluidRow(
      column(6, selectInput(ns("map_organism"), i18n$t("Organisme"),
                            choices = setNames(c("human","mouse"), c(.tr_plain("Humain"), .tr_plain("Souris"))))),
      column(6, selectInput(ns("map_from_type"), i18n$t("Type source"),
                            choices = setNames(c("ensembl","entrez"), c(.tr_plain("Ensembl Gene ID"), .tr_plain("Entrez ID")))))
    ),

    checkboxInput(ns("strip_ensembl_version"),
                  i18n$t("Retirer le suffixe de version Ensembl (ENSG...5 → ENSG...)"),
                  value = TRUE),

    radioButtons(ns("collapse_method"),
                 i18n$t("Fusion des doublons (plusieurs ID → même symbole)"),
                 choices = setNames(c("sum","max_mean"), c(.tr_plain("Somme des counts (recommandé)"), .tr_plain("Garder l'ID le plus exprimé")))),

    actionButton(ns("run_mapping"), i18n$t("Appliquer le Mapping"),
                 icon = icon("arrows-rotate"), class="btn-info w-100"),

    div(class="mt-2", uiOutput(ns("mapping_status")))
  )
}


mod_sc_mapping_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }



    mapping_status_rv <- reactiveVal(NULL)

    observeEvent(global_data$language, {
      # Re-translate dynamic labels on language switch
    }, ignoreInit = TRUE)
    # ── Auto-detect gene ID type when sc_obj changes ──────────────────────
    output$detected_id_type_ui <- renderUI({
      req(global_data$sc_obj)
      ids        <- rownames(global_data$sc_obj)
      detected   <- tryCatch(detect_gene_id_type(ids), error=function(e) "unknown")
      example_id <- if (length(ids)) ids[1] else "?"

      if (detected == "unknown") {
        return(div(class="alert alert-warning", style="font-size:0.8em;",
          icon("circle-exclamation"),
          .t_fmt(.tr("Type d'identifiant NON reconnu (ex. trouvé dans vos données : {example}). Si ce ne sont PAS des identifiants de gènes (ex: comptages personnalisés, autre type de feature), ignorez cette étape facultative et passez directement à l'étape 1."), example = example_id)))
      }
      label <- switch(detected,
        ensembl    = .tr("Ensembl Gene ID"),
        entrez     = .tr("Entrez ID"),
        affy_probe = .tr("Probe Affymetrix (non supporté pour SC)"),
        symbol     = .tr("Symbole — déjà mappé, cette étape est probablement inutile"))
      cls <- if (detected == "symbol") "alert-success" else "alert-info"
      detected_org <- tryCatch(detect_organism_from_ids(ids), error=function(e) NA_character_)
      org_line <- if (!is.na(detected_org))
        tags$div(class="mt-1", .t_fmt(.tr("Organisme détecté : {org}."), org = if (detected_org=="mouse") .tr("Souris") else .tr("Humain")))
      else NULL
      div(class=paste("alert", cls), style="font-size:0.8em;",
          .t_fmt(.tr("Type détecté automatiquement : {label} (ex: '{example}')."), label = label, example = example_id), org_line)
    })

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      ids      <- rownames(global_data$sc_obj)
      detected <- tryCatch(detect_gene_id_type(ids), error=function(e) "unknown")
      if (detected %in% c("ensembl","entrez"))
        updateSelectInput(session, "map_from_type", selected=detected)

      # Step-3.8B: pre-select organism from ID prefix -- was always left on
      # the "Humain" default, the same class of bug fixed in the auto-pipeline.
      detected_org <- tryCatch(detect_organism_from_ids(ids), error=function(e) NA_character_)
      if (!is.na(detected_org))
        updateSelectInput(session, "map_organism", selected=detected_org)
    }, ignoreNULL=TRUE)

    # ── Run mapping ───────────────────────────────────────────────────────
    observeEvent(input$run_mapping, {
      req(global_data$sc_obj)
      showNotification(.tr("Mapping des identifiants..."), type="message", duration=4)

      obj            <- global_data$sc_obj
      has_downstream <- "seurat_clusters" %in% colnames(obj@meta.data) ||
                        length(obj@reductions) > 0

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message=.tr("Mapping des identifiants..."), value=0.2)

      tryCatch({
        result <- withCallingHandlers(
          remap_seurat_ids_to_symbol(
            obj,
            from_type       = input$map_from_type,
            organism        = input$map_organism,
            collapse_method = input$collapse_method,
            strip_version   = isTRUE(input$strip_ensembl_version)
          ),
          warning = function(w) {
            showNotification(paste("\u2139\ufe0f", conditionMessage(w)), type="message", duration=8)
            invokeRestart("muffleWarning")
          }
        )

        n_total      <- result$n_mapped + result$n_unmapped
        pct_unmapped <- if (n_total > 0) 100 * result$n_unmapped / n_total else 0

        if (pct_unmapped > 20)
          showNotification(
            .t_fmt(.tr("⚠️ {pct}% des identifiants n'ont pas pu être mappés. Vérifiez l'organisme et le type source."), pct = round(pct_unmapped)),
            type="warning", duration=10)

        global_data$sc_obj <- result$object

        mapping_status_rv(paste0(
          .t_fmt(.tr("✓ {mapped} mappés, {unmapped} non-mappés ({pct}%), {collapsed} fusion(s) de doublon → {final} gènes finaux."), mapped = result$n_mapped, unmapped = result$n_unmapped, pct = sprintf("%.1f", pct_unmapped), collapsed = result$n_collapsed, final = nrow(result$object)),
          if (has_downstream) paste0(" ", .tr("Pipeline réinitialisé — relancez l'étape 1.")) else ""
        ))
        showNotification(.tr("✓ Mapping appliqué — relancez l'étape 1 (Filtrage & VST) pour l'utiliser."), type="message", duration=5)

      }, error=function(e) {
        showNotification(paste(.tr("Erreur de mapping:"), conditionMessage(e)),
                         type="error", duration=15)
      })
    })

    # ── Status display ────────────────────────────────────────────────────
    output$mapping_status <- renderUI({
      if (is.null(mapping_status_rv()))
        return(tags$em(.tr("Aucun mapping appliqué."), style="color:#999;font-size:0.82em;"))
      div(class="alert alert-success", style="font-size:0.8em;", mapping_status_rv())
    })

  }) # /moduleServer
}
