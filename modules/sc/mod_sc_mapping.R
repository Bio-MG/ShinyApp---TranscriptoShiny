# =============================================================================
# mod_sc_mapping.R  —  SC Child 0 (Optional): Mapping d'identifiants de gènes
# =============================================================================
# Mirrors mod_bulk_mapping.R for Single-Cell. Operates directly on
# global_data$sc_obj (rebuilds it from raw counts via remap_seurat_ids_to_symbol()
# in helpers_sc.R) — must run BEFORE "1. Pipeline" since normalisation/PCA/
# clusters are invalidated when rownames change.
# =============================================================================

mod_sc_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light", style="font-size:0.82em;",
        bsicons::bs_icon("info-circle"),
        " Étape facultative — uniquement si vos identifiants ne sont PAS déjà des symboles ",
        "(ex: ENSG00000141510, 7157). À lancer ", tags$strong("AVANT l'étape '1. Pipeline'"),
        " : toute normalisation/clustering déjà calculé sera réinitialisé."),

    uiOutput(ns("detected_id_type_ui")),

    fluidRow(
      column(6, selectInput(ns("map_organism"), "Organisme",
                            choices = c("Humain"="human","Souris"="mouse"))),
      column(6, selectInput(ns("map_from_type"), "Type source",
                            choices = c("Ensembl Gene ID"="ensembl",
                                        "Entrez ID"       ="entrez")))
    ),

    checkboxInput(ns("strip_ensembl_version"),
                  "Retirer le suffixe de version Ensembl (ENSG...5 → ENSG...)",
                  value = TRUE),

    radioButtons(ns("collapse_method"),
                 "Fusion des doublons (plusieurs ID → même symbole)",
                 choices = c("Somme des counts (recommandé)"="sum",
                             "Garder l'ID le plus exprimé"  ="max_mean")),

    actionButton(ns("run_mapping"), "\U0001f504 Appliquer le Mapping",
                 class="btn-info w-100"),

    div(class="mt-2", uiOutput(ns("mapping_status")))
  )
}


mod_sc_mapping_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    mapping_status_rv <- reactiveVal(NULL)

    # ── Auto-detect gene ID type when sc_obj changes ──────────────────────
    output$detected_id_type_ui <- renderUI({
      req(global_data$sc_obj)
      ids        <- rownames(global_data$sc_obj)
      detected   <- tryCatch(detect_gene_id_type(ids), error=function(e) "unknown")
      example_id <- if (length(ids)) ids[1] else "?"

      if (detected == "unknown") {
        return(div(class="alert alert-warning", style="font-size:0.8em;",
          icon("circle-exclamation"),
          sprintf(" Type non reconnu (ex: '%s'). Si ce ne sont pas des IDs de gènes standards, ignorez cette étape.",
                  example_id)))
      }
      label <- switch(detected,
        ensembl    = "Ensembl Gene ID",
        entrez     = "Entrez ID",
        affy_probe = "Probe Affymetrix (non supporté pour SC)",
        symbol     = "Symbole — déjà mappé \u2713")
      cls <- if (detected == "symbol") "alert-success" else "alert-info"
      div(class=paste("alert", cls), style="font-size:0.8em;",
          sprintf("Type détecté : %s (ex: '%s').", label, example_id))
    })

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      detected <- tryCatch(detect_gene_id_type(rownames(global_data$sc_obj)),
                           error=function(e) "unknown")
      if (detected %in% c("ensembl","entrez"))
        updateSelectInput(session, "map_from_type", selected=detected)
    }, ignoreNULL=TRUE)

    # ── Run mapping ───────────────────────────────────────────────────────
    observeEvent(input$run_mapping, {
      req(global_data$sc_obj)
      showNotification("\U0001f504 Mapping en cours...", type="message", duration=4)

      obj            <- global_data$sc_obj
      has_downstream <- "seurat_clusters" %in% colnames(obj@meta.data) ||
                        length(obj@reductions) > 0

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Mapping des identifiants...", value=0.2)

      tryCatch({
        result <- remap_seurat_ids_to_symbol(
          obj,
          from_type       = input$map_from_type,
          organism        = input$map_organism,
          collapse_method = input$collapse_method,
          strip_version   = isTRUE(input$strip_ensembl_version)
        )

        n_total      <- result$n_mapped + result$n_unmapped
        pct_unmapped <- if (n_total > 0) 100 * result$n_unmapped / n_total else 0

        if (pct_unmapped > 20)
          showNotification(
            sprintf("\u26a0\ufe0f %.0f%% des identifiants n'ont pas pu être mappés. Vérifiez organisme/type.", pct_unmapped),
            type="warning", duration=10)

        global_data$sc_obj <- result$object

        mapping_status_rv(sprintf(
          "\u2713 %d mappés, %d non-mappés (%.1f%%), %d fusion(s) \u2192 %d gènes finaux.%s",
          result$n_mapped, result$n_unmapped, pct_unmapped,
          result$n_collapsed, nrow(result$object),
          if (has_downstream) " Pipeline réinitialisé — relancez l'étape 1." else ""
        ))
        showNotification("\u2713 Mapping appliqué.", type="message", duration=5)

      }, error=function(e) {
        showNotification(paste("Erreur de mapping:", conditionMessage(e)),
                         type="error", duration=15)
      })
    })

    # ── Status display ────────────────────────────────────────────────────
    output$mapping_status <- renderUI({
      if (is.null(mapping_status_rv()))
        return(tags$em("Aucun mapping appliqué.", style="color:#999;font-size:0.82em;"))
      div(class="alert alert-success", style="font-size:0.8em;", mapping_status_rv())
    })

  }) # /moduleServer
}
