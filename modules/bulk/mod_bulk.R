# =============================================================================
# mod_bulk.R  —  Bulk RNA-seq Parent Router Module (refactored, slim)
# =============================================================================
# Orchestrates 4 child modules. Owns:
#   - accordion (sidebar, 4 steps) + navset_card_underline (7 tabs)
#   - shared_rv: cross-child ephemeral state (the bulk equivalent of mod_sc's
#     shared_rv — see header comments in each child for the full contract)
#   - tab navigation bridge (children write shared_rv$active_tab; the parent
#     performs nav_select with the correctly-scoped session)
#
# Why this file is now ~150 lines instead of ~780: each accordion step used
# to be a self-contained block inside one giant moduleServer. That made any
# patch risk a silent collision with an unrelated section. Each step is now
# its own file/module — see modules/mod_bulk_mapping.R, mod_bulk_filter.R,
# mod_bulk_de.R, mod_bulk_pathways.R, mod_bulk_report.R for the actual logic.
#
# Child modules — MUST be sourced in app.R BEFORE this file (same convention
# already used by mod_sc.R / mod_sc_*.R):
#   source("modules/mod_bulk_mapping.R")
#   source("modules/mod_bulk_filter.R")
#   source("modules/mod_bulk_de.R")
#   source("modules/mod_bulk_pathways.R")
#   source("modules/mod_bulk_report.R")
#   source("modules/mod_bulk.R")
#
# State contract:
#   global_data$bulk_obj — list(counts, metadata, project, ...), owned by
#                           mod_import_bulk.R (read-only here)
#   shared_rv             — ephemeral cross-child state (owned here):
#     $counts_mapped    : matrix        | mod_bulk_mapping writes (Step 0, optional);
#                                          mod_bulk_filter reads instead of the raw import
#     $counts_original  : matrix        | mod_bulk_mapping writes once (undo backup)
#     $mapping_applied  : logical       | mod_bulk_mapping writes
#     $mapping_summary  : character     | mod_bulk_mapping writes
#     $filtered_counts  : matrix        | mod_bulk_filter writes
#     $dds_blind        : DESeqDataSet  | mod_bulk_filter writes (design ~1)
#     $vst_mat          : matrix        | mod_bulk_filter writes; read by de/report
#     $dds_full         : DESeqDataSet  | mod_bulk_de writes (real design)
#     $contrasts        : list[df]      | mod_bulk_de writes; read by pathways/report
#     $active_contrast  : character     | mod_bulk_de writes; read by pathways/report
#     $pathway_results  : data.frame    | mod_bulk_pathways writes; read by report
#     $active_tab       : string        | any child sets; parent observes
#     -- mirrored UI scalars (read-only echoes for mod_bulk_report, since the
#        report cannot reach into a sibling module's input$ namespace) --
#     $pca_color_by, $pca_shape_by      : mirrored by mod_bulk_filter
#     $lfc_thresh, $padj_thresh,
#     $heatmap_top_n, $heatmap_annot    : mirrored by mod_bulk_de
#     $pathway_db                       : mirrored by mod_bulk_pathways
# =============================================================================


# ── UI ────────────────────────────────────────────────────────────────────────

mod_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),

    layout_sidebar(
      sidebar = sidebar(
        width = 420,
        title = "RNA Bulk — Analyse",

        div(class = "alert alert-info", style = "font-size:0.8rem;padding:5px;",
            bsicons::bs_icon("info-circle"),
            "Importez d'abord vos données dans l'onglet 'Import Données > RNA Bulk'."),

        uiOutput(ns("pipeline_status_bar")),

        accordion(
          id   = ns("acc_bulk"),
          open = "1. Filtrage & VST",

          accordion_panel(
            "0. Mapping IDs (Optionnel)", icon = icon("arrows-rotate"),
            mod_bulk_mapping_ui(ns("mapping"))
          ),

          accordion_panel(
            "1. Filtrage & VST", icon = icon("filter"),
            mod_bulk_filter_ui(ns("filter"))
          ),

          accordion_panel(
            "2. Design & Contrastes", icon = icon("sliders"),
            mod_bulk_de_ui(ns("de"))
          ),

          accordion_panel(
            "3. Pathway Enrichment", icon = icon("dna"),
            mod_bulk_pathways_ui(ns("pathways"))
          ),

          accordion_panel(
            "4. Rapport Complet", icon = icon("file-export"),
            mod_bulk_report_ui(ns("report"))
          )
        )
      ),

      # ── Main panel ────────────────────────────────────────────────────────
      navset_card_underline(
        id    = ns("main_tabs"),
        title = "Résultats Bulk RNA-seq",

        nav_panel("PCA",             value = "tab_pca",     mod_bulk_filter_pca_ui(ns("filter"))),
        nav_panel("QC Échantillons", value = "tab_qc",      mod_bulk_filter_qc_ui(ns("filter"))),
        nav_panel("Volcano Plot",    value = "tab_volcano", mod_bulk_de_volcano_ui(ns("de"))),
        nav_panel("MA-Plot",         value = "tab_ma",      mod_bulk_de_ma_ui(ns("de"))),
        nav_panel("Heatmap",         value = "tab_heatmap", mod_bulk_de_heatmap_ui(ns("de"))),
        nav_panel("Table DE",        value = "tab_table",   mod_bulk_de_table_ui(ns("de"))),
        nav_panel("Résumé Up/Down",  value = "tab_updown",  mod_bulk_de_summary_ui(ns("de"))),
        nav_panel("Multi-méthodes",  value = "tab_multimethod", mod_bulk_de_multimethod_ui(ns("de"))),
        nav_panel("Venn / UpSet",    value = "tab_venn",    mod_bulk_de_venn_ui(ns("de"))),
        nav_panel("Pathway",         value = "tab_pathway", mod_bulk_pathways_output_ui(ns("pathways")))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    # ── 1. Shared ephemeral state (cross-child) ───────────────────────────
    shared_rv <- reactiveValues(
      counts_mapped    = NULL,
      counts_original  = NULL,
      mapping_applied  = FALSE,
      mapping_summary  = NULL,
      filtered_counts  = NULL,
      dds_blind        = NULL,
      vst_mat          = NULL,
      dds_full         = NULL,
      contrasts        = list(),
      active_contrast  = NULL,
      pathway_results  = NULL,
      active_tab       = NULL,
      # mirrored scalars — see header contract above
      pca_color_by     = NULL,
      pca_shape_by     = NULL,
      lfc_thresh       = 1,
      padj_thresh      = 0.05,
      heatmap_top_n    = 30,
      heatmap_annot    = NULL,
      pathway_db       = "GOBP"
    )

    # ── 2. Tab navigation bridge ───────────────────────────────────────────
    # Any child writes shared_rv$active_tab; the parent performs nav_select
    # with `session` explicitly set so the namespace resolves to THIS
    # module's "main_tabs" id regardless of which child triggered it.
    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(id = "main_tabs", selected = shared_rv$active_tab, session = session)
    })

    # ── 2bis. Pipeline status bar — ✅ fait / ⚪ en attente / 🔒 verrouillé.
    #    Lives in the parent because it's the only place that legitimately
    #    holds ALL of shared_rv at once; no child needs to know about siblings.
    output$pipeline_status_bar <- renderUI({
      step0 <- if (isTRUE(shared_rv$mapping_applied)) "✅" else "⚪"
      step1 <- if (!is.null(shared_rv$filtered_counts)) "✅" else "⚪"
      step2 <- if (length(shared_rv$contrasts) > 0) "✅"
               else if (is.null(shared_rv$filtered_counts)) "🔒" else "⚪"
      step3 <- if (!is.null(shared_rv$pathway_results)) "✅"
               else if (length(shared_rv$contrasts) == 0) "🔒" else "⚪"
      step4 <- if (!is.null(shared_rv$vst_mat)) "⚪" else "🔒"

      div(
        style = "display:flex;justify-content:space-around;font-size:0.72em;background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;padding:4px 2px;margin-bottom:8px;",
        tags$span(style = "padding:2px 4px;", step0, " Map"),
        tags$span(style = "padding:2px 4px;", step1, " Filtre"),
        tags$span(style = "padding:2px 4px;", step2, " DE"),
        tags$span(style = "padding:2px 4px;", step3, " Pathway"),
        tags$span(style = "padding:2px 4px;", step4, " Rapport")
      )
    })

    # ── 3. Call child servers ──────────────────────────────────────────────
    mod_bulk_mapping_server( "mapping",   global_data, shared_rv)
    mod_bulk_filter_server(   "filter",   global_data, shared_rv)
    mod_bulk_de_server(       "de",       global_data, shared_rv)
    mod_bulk_pathways_server( "pathways", global_data, shared_rv)
    mod_bulk_report_server(   "report",   global_data, shared_rv)

  }) # /moduleServer
}
