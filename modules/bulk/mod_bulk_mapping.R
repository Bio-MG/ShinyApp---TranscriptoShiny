# =============================================================================
# mod_bulk_mapping.R  —  Bulk Child 0 (Optional): Mapping d'identifiants de gènes
# =============================================================================
# Converts Ensembl/Entrez/Affy probe row identifiers to gene SYMBOLs before
# the rest of the pipeline runs. Purely additive/optional — if the counts
# matrix already uses symbols, this step is a no-op the user can skip.
#
# Owns NO main-panel tab — sidebar controls only. Operates on the IMMUTABLE
# global_data$bulk_obj$counts; the mapped result is exposed via shared_rv so
# mod_bulk_filter (Step 1) can transparently pick it up.
#
# Depends on global.R:
#   detect_gene_id_type(), remap_gene_ids_to_symbol()
#
# State contract (shared_rv):
#   READ  : none (reads global_data$bulk_obj directly — the raw import)
#   WRITE : shared_rv$counts_mapped    — matrix with SYMBOL rownames, or NULL
#                                         if no mapping has been applied/it was
#                                         undone. mod_bulk_filter reads this
#                                         with `shared_rv$counts_mapped %||%
#                                         global_data$bulk_obj$counts`.
#           shared_rv$counts_original  — one-time backup of the untouched
#                                         counts matrix (set on first mapping
#                                         only, never overwritten — true undo)
#           shared_rv$mapping_applied  — logical, drives the conditional
#                                         "Annuler" button
#           shared_rv$mapping_summary  — character, human-readable result
#
# UI split:
#   mod_bulk_mapping_ui(id) -> sidebar accordion body ("0. Mapping IDs")
#   (no output_ui — informational only, no plot/table of its own)
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.82em;",
        bsicons::bs_icon("info-circle"),
        " Étape facultative — uniquement si vos identifiants de gènes ne sont PAS déjà ",
        "des symboles (ex: ENSG00000141510, 7157, '1007_s_at'). Si vos données utilisent ",
        "déjà des symboles (TP53, ACTB...), ignorez cette étape."),

    uiOutput(ns("detected_id_type_ui")),

    fluidRow(
      column(6, selectInput(ns("map_organism"), "Organisme",
                            choices = c("Humain" = "human", "Souris" = "mouse"))),
      column(6, selectInput(ns("map_from_type"), "Type source",
                            choices = c("Ensembl Gene ID" = "ensembl",
                                        "Entrez ID"        = "entrez",
                                        "Probe Affymetrix" = "affy_probe")))
    ),

    checkboxInput(ns("strip_ensembl_version"),
                 "Retirer le suffixe de version Ensembl (ENSG...5 → ENSG...)",
                 value = TRUE),

    radioButtons(ns("collapse_method"), "Fusion des doublons (plusieurs ID → même symbole)",
                choices = c("Somme des counts (recommandé)" = "sum",
                            "Garder l'ID le plus exprimé"    = "max_mean")),

    actionButton(ns("run_mapping"), "🔄 Appliquer le Mapping", class = "btn-info w-100"),
    uiOutput(ns("undo_btn_ui")),

    div(class = "mt-2", uiOutput(ns("mapping_status")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_mapping_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Auto-detect probable ID type as soon as raw counts are available ────
    output$detected_id_type_ui <- renderUI({
      req(global_data$bulk_obj)
      ids <- rownames(global_data$bulk_obj$counts)
      detected <- tryCatch(detect_gene_id_type(ids), error = function(e) "unknown")
      example_id <- if (length(ids) > 0) ids[1] else "?"

      if (detected == "unknown") {
        return(div(class = "alert alert-danger", style = "font-size:0.8em;",
          icon("circle-exclamation"),
          sprintf(
            " Type d'identifiant NON reconnu (ex. trouvé dans vos données : '%s'). Si ce ne sont PAS des identifiants de gènes (ex: comptages personnalisés, autre type de feature), ",
            example_id
          ),
          tags$strong("ignorez cette étape facultative"), " — passez directement à l'étape 1."
        ))
      }
      label <- switch(detected,
        ensembl    = "Ensembl Gene ID",
        entrez     = "Entrez ID",
        affy_probe = "Probe Affymetrix",
        symbol     = "Symbole — déjà mappé, cette étape est probablement inutile"
      )
      cls <- if (detected == "symbol") "alert-success" else "alert-info"
      div(class = paste("alert", cls), style = "font-size:0.8em;",
          sprintf("Type détecté automatiquement : %s (ex: '%s').", label, example_id))
    })

    # Pre-select the detected type to save a click. When detection is
    # "unknown" we deliberately do NOT touch the dropdown — it would
    # otherwise silently stay on its first choice ("Ensembl"), which is
    # exactly how a non-gene dataset (e.g. "eye_count"-style custom rows)
    # used to get mapped against ENSEMBL by mistake and crash downstream.
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
      p$set(message = "Mapping des identifiants...", value = 0.3)

      tryCatch({
        result <- remap_gene_ids_to_symbol(
          counts_work, from_type = input$map_from_type,
          organism = input$map_organism, collapse_method = input$collapse_method
        )
        n_total      <- result$n_mapped + result$n_unmapped
        pct_unmapped <- if (n_total > 0) 100 * result$n_unmapped / n_total else 0

        if (pct_unmapped > 20) {
          showNotification(
            sprintf("⚠️ %.0f%% des identifiants n'ont pas pu être mappés. Vérifiez l'organisme et le type source.",
                    pct_unmapped),
            type = "warning", duration = 10
          )
        }

        # Backup the TRUE original exactly once, so repeated re-mapping
        # attempts never lose the real starting point (undo always restores
        # the pristine import, not the previous mapping attempt).
        if (is.null(shared_rv$counts_original)) {
          shared_rv$counts_original <- counts_orig
        }
        shared_rv$counts_mapped   <- result$matrix
        shared_rv$mapping_applied <- TRUE
        shared_rv$mapping_summary <- sprintf(
          "✓ %d mappés, %d non-mappés (%.1f%%), %d fusion(s) de doublon → %d gènes finaux. Relancez l'étape 1 (Filtrage & VST) pour appliquer.",
          result$n_mapped, result$n_unmapped, pct_unmapped, result$n_collapsed, nrow(result$matrix)
        )
        showNotification("✓ Mapping appliqué — relancez l'étape 1 (Filtrage & VST) pour l'utiliser.",
                         type = "message", duration = 7)
      }, error = function(e) {
        showNotification(paste("Erreur de mapping:", conditionMessage(e)), type = "error", duration = 15)
      })
    })

    # ── Undo: discard the mapped matrix, fall back to the original import ───
    observeEvent(input$undo_mapping, {
      shared_rv$counts_mapped   <- NULL
      shared_rv$mapping_applied <- FALSE
      shared_rv$mapping_summary <- NULL
      showNotification("↩ Mapping annulé — relancez l'étape 1 pour revenir aux identifiants originaux.",
                       type = "message", duration = 6)
    })

    output$undo_btn_ui <- renderUI({
      if (isTRUE(shared_rv$mapping_applied)) {
        actionButton(ns("undo_mapping"), "↩ Annuler le Mapping",
                    class = "btn-outline-danger btn-sm w-100 mt-1")
      } else NULL
    })

    output$mapping_status <- renderUI({
      if (is.null(shared_rv$mapping_summary)) {
        tags$em("Aucun mapping appliqué — identifiants originaux utilisés.", style = "color:#999;font-size:0.82em;")
      } else {
        div(class = "alert alert-success", style = "font-size:0.8em;", shared_rv$mapping_summary)
      }
    })

  }) # /moduleServer
}
