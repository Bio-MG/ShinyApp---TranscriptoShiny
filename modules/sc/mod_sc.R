# =============================================================================
# mod_sc.R  —  Parent Router Module (Step-3.7)
# =============================================================================
# Step-3.6 changes (recap): report_viz_list + traj_reduction in reactiveValues,
# "0. Mapping IDs" panel, saved-viz basket, extended auto-pipeline, pipeline
# status bar + Résumé Pipeline tab.
#
# Step-3.7 changes:
#   [1] Auto-pipeline: secondary t-SNE always computed after UMAP (capped via
#       .AUTO_TSNE_MAX_CELLS, defined in mod_sc_pipeline.R) — same rationale
#       as the standalone "1. Pipeline" module: PCA/UMAP/t-SNE all available
#       in the Viz "Réduction à visualiser" picker without an extra manual run.
#   [2] Auto-pipeline: FindAllMarkers / Gene Correlation steps now run on a
#       RAM-safety-capped subsample (shared_rv$max_cells_heavy, set in
#       "1. Pipeline") instead of always the full object.
#   [3] render_params$traj_genes forwarded to the report (mirrors
#       shared_rv$traj_genes, written live by mod_sc_trajectory.R) so the new
#       "Gènes vs Pseudotemps" report section renders the same genes the user
#       was looking at live.
# =============================================================================

mod_sc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 420, title = "Single-Cell Workflow",
      div(class="alert alert-info", style="font-size:0.8rem;padding:5px;",
          bsicons::bs_icon("info-circle"), " Étapes séquentielles recommandées."),
      actionButton(ns("btn_auto_pipeline_sc"), "\u25b6 Lancer Pipeline Complet (SC)",
                   icon=icon("play-circle"), class="btn-outline-success w-100 mb-1"),
      verbatimTextOutput(ns("sc_auto_log")),
      uiOutput(ns("sc_pipeline_status_bar")),
      accordion(
        id=ns("acc_workflow"), open="1. Pipeline",
        accordion_panel("0. Mapping IDs (Optionnel)", icon=icon("arrows-rotate"),
                        mod_sc_mapping_ui(ns("mapping"))),
        accordion_panel("1. Pipeline",           icon=icon("cogs"),
                        mod_sc_pipeline_ui(ns("pipeline"))),
        accordion_panel("2. Annotation",         icon=icon("user-tag"),
                        mod_sc_annotation_ui(ns("annotation"))),
        accordion_panel("3. Visualisation",      icon=icon("chart-area"),
                        mod_sc_viz_ui(ns("viz"))),
        accordion_panel("4. Marqueurs",          icon=icon("magnifying-glass-chart"),
                        mod_sc_markers_ui(ns("markers"))),
        accordion_panel("5. Gene Correlation",   icon=icon("project-diagram"),
                        mod_sc_corr_ui(ns("corr"))),
        accordion_panel("6. Pathway Enrichment", icon=icon("sitemap"),
                        mod_sc_pathways_ui(ns("pathways"))),
        accordion_panel("7. Trajectory Analysis",icon=icon("route"),
                        mod_sc_trajectory_ui(ns("trajectory"))),
        accordion_panel(
          "8. Rapport Complet", icon=icon("file-export"),
          div(class="alert alert-light",style="font-size:0.85em;border-left:3px solid #2C3E50;",
              "Rapport autonome (QC, Réduction, Annotation, Marqueurs, Pathways, Trajectoire)."),
          textInput(ns("report_title"),    "Titre",    value="Analyse Single-Cell"),
          textInput(ns("report_subtitle"), "Sous-titre (optionnel)"),
          textAreaInput(ns("report_notes"), "Notes", rows=3),
          checkboxGroupInput(ns("report_sections"), "Sections",
            choices=c("QC"="qc",
                      "Réduction Dimensionnelle"="dim",
                      "Annotation"="annotation",
                      "Marqueurs"="markers",
                      "Réseau Corrélation"="correlation",
                      "Pathway Enrichment"="pathway",
                      "Trajectoire"="trajectory",
                      "Visualisations Sauvegardées"="custom_viz"),
            selected=c("qc","dim","annotation","markers","pathway")),
          # Saved viz basket manager
          div(class="border rounded p-2 mb-2", style="background:#f8f9fa;",
              h6("📌 Visualisations sauvegardées", style="font-size:0.85em;font-weight:bold;"),
              uiOutput(ns("saved_viz_list_ui")),
              actionButton(ns("clear_saved_viz"), "🗑️ Vider la liste",
                          class="btn-outline-danger btn-sm w-100 mt-1")),
          radioButtons(ns("report_format"), "Format",
            choices=c("HTML interactif"="html","PDF statique"="pdf","Les deux (.zip)"="both"),
            selected="html"),
          conditionalPanel(condition="input.report_format != 'pdf'", ns=ns,
            checkboxInput(ns("report_interactive"), "Graphiques interactifs (HTML)", value=TRUE)),
          div(class="small text-muted", "PDF requiert tinytex::install_tinytex()."),
          downloadButton(ns("dl_report"), "\U0001f4c4 Générer le Rapport",
                         class="btn-dark w-100 mt-2"),
          hr(),
          div(class="alert alert-light",style="font-size:0.82em;border-left:3px solid #18BC9C;",
              bsicons::bs_icon("code-slash"),
              " Script R reproductible (.zip) + objet Seurat traité."),
          downloadButton(ns("dl_sc_r_script"), "\U0001f9fe Export Script R (.zip)",
                         class="btn-outline-secondary w-100"),
          div(class="small text-muted mt-1", textOutput(ns("report_status")))
        )
      )
    ),
    navset_card_underline(
      id=ns("main_tabs"), title="Résultats",
      nav_panel("Graphiques",       value="tab_viz",         mod_sc_viz_output_ui(ns("viz"))),
      nav_panel("Table Marqueurs",  value="tab_table",       mod_sc_markers_output_ui(ns("markers"))),
      nav_panel("Annotation",       value="tab_annotation",  mod_sc_annotation_output_ui(ns("annotation"))),
      nav_panel("Gènes Corrélés",   value="tab_correlation", mod_sc_corr_output_ui(ns("corr"))),
      nav_panel("Pathways",         value="tab_pathway",     mod_sc_pathways_output_ui(ns("pathways"))),
      nav_panel("Trajectory",       value="tab_trajectory",  mod_sc_trajectory_output_ui(ns("trajectory"))),
      nav_panel("QC", value="tab_qc",
        card(max_height=750,
          div(class="card-header bg-light", h5("Contrôle Qualité", class="card-title mb-0")),
          plotOutput(ns("plot_qc"), height="650px"))),
      nav_panel("Résumé Pipeline", value="tab_summary",
        card(card_header("Résumé du Pipeline Single-Cell"),
             uiOutput(ns("pipeline_summary_panel"))))
    )
  )
}


mod_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    shared_rv <- reactiveValues(
      markers_data     = NULL,
      correlated_genes = NULL,
      corr_target_gene = NULL,
      pathway_results  = NULL,
      pathway_db       = NULL,
      selected_genes   = character(0),
      active_tab       = NULL,
      report_viz_list  = list(),    # basket for "📌 Ajouter au Rapport"
      traj_reduction   = NULL,      # mirrors last trajectory reduction used
      traj_genes       = character(0),  # Step-3.7: mirrors "Genes vs Pseudotemps" picker
      max_cells_heavy  = Inf        # Step-3.7: RAM-safety cap (set by mod_sc_pipeline.R)
    )

    observeEvent(shared_rv$active_tab, {
      req(shared_rv$active_tab)
      nav_select(id="main_tabs", selected=shared_rv$active_tab, session=session)
    })

    # ── QC plot ──────────────────────────────────────────────────────────────
    output$plot_qc <- renderPlot({
      req(global_data$sc_obj)
      VlnPlot(global_data$sc_obj,
              features=c("nFeature_RNA","nCount_RNA","percent.mt"), ncol=3, pt.size=0)
    })

    # ── Pipeline status bar ───────────────────────────────────────────────────
    output$sc_pipeline_status_bar <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      meta <- obj@meta.data
      s_qc      <- if ("percent.mt"      %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_norm    <- if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))) > 0) "\u2705" else "\u26aa"
      s_cluster <- if ("seurat_clusters" %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_umap    <- if ("umap"            %in% names(obj@reductions))              "\u2705" else "\u26aa"
      s_annot   <- if (any(grepl("^SingleR_", colnames(meta))))                   "\u2705" else "\u26aa"
      s_markers <- if (!is.null(shared_rv$markers_data))                          "\u2705" else "\u26aa"
      div(style=paste0("display:flex;justify-content:space-around;font-size:0.72em;",
                       "background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;",
                       "padding:4px 2px;margin-bottom:8px;"),
          tags$span(style="padding:2px 4px;", s_qc,      " QC"),
          tags$span(style="padding:2px 4px;", s_norm,    " Norm"),
          tags$span(style="padding:2px 4px;", s_cluster, " Cluster"),
          tags$span(style="padding:2px 4px;", s_umap,    " UMAP"),
          tags$span(style="padding:2px 4px;", s_annot,   " Annot"),
          tags$span(style="padding:2px 4px;", s_markers, " Marqueurs"))
    })

    # ── Pipeline summary panel ────────────────────────────────────────────────
    output$pipeline_summary_panel <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj))
        return(div(class="alert alert-info m-3", "Aucun objet Single-Cell chargé."))
      meta         <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      reductions   <- names(obj@reductions)
      n_clusters   <- if ("seurat_clusters" %in% colnames(meta))
                        length(levels(factor(meta$seurat_clusters))) else NA
      rows <- list(
        c("Cellules",              format(ncol(obj), big.mark=",")),
        c("Gènes",                 format(nrow(obj), big.mark=",")),
        c("Réductions",            if (length(reductions)) paste(reductions,collapse=", ") else "\u2014"),
        c("Clusters",              if (!is.na(n_clusters)) as.character(n_clusters) else "Non calculé"),
        c("Annotation SingleR",    if (length(singler_cols)) paste(singler_cols,collapse=", ") else "Non effectuée"),
        c("Gènes variables",       if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))))
                                     format(length(VariableFeatures(obj)),big.mark=",") else "\u2014"),
        c("Marqueurs calculés",    if (!is.null(shared_rv$markers_data))
                                     paste(nrow(shared_rv$markers_data),"marqueurs") else "Non calculés"),
        c("Pathways",              if (!is.null(shared_rv$pathway_results))
                                     paste(nrow(shared_rv$pathway_results),"pathways") else "Non calculés"),
        c("Pseudotemps",           if ("pseudotime" %in% colnames(meta)) "Calculé" else "Non calculé"),
        c("Backend stockage",      if (sc_backend_status(obj) == "disk") "\U0001f4bd Disque (BPCells)" else "\U0001f9e0 RAM (standard)"),
        c("Sous-échant. (marqueurs/corr)", if (is.finite(shared_rv$max_cells_heavy %||% Inf))
                                     paste0("max ", format(shared_rv$max_cells_heavy, big.mark=","), " cellules/groupe")
                                   else "désactivé"),
        c("Viz. sauvegardées",     paste(length(shared_rv$report_viz_list), "plot(s) dans le panier"))
      )
      tagList(
        div(class="m-3",
          tags$table(class="table table-sm table-bordered",
            tags$tbody(lapply(rows, function(r) {
              tags$tr(tags$th(style="width:40%;",r[1]), tags$td(r[2]))
            }))),
          div(class="small text-muted", paste("Mis à jour :", format(Sys.time(),"%H:%M:%S")))
        )
      )
    })

    # ── Saved viz basket UI ───────────────────────────────────────────────────
    output$saved_viz_list_ui <- renderUI({
      lst <- shared_rv$report_viz_list %||% list()
      if (!length(lst))
        return(div(class="text-muted small", "Aucune visualisation sauvegardée. ",
                   "Utilisez '📌 Ajouter au Rapport' dans l'onglet Graphiques."))
      tags$ul(style="font-size:0.8em;margin-bottom:0;",
              lapply(names(lst), function(nm) tags$li(nm)))
    })

    observeEvent(input$clear_saved_viz, {
      shared_rv$report_viz_list <- list()
      showNotification("🗑️ Liste de visualisations vidée.", type="message", duration=3)
    })

    # ── Child servers ─────────────────────────────────────────────────────────
    mod_sc_mapping_server(   "mapping",   global_data)
    mod_sc_pipeline_server(  "pipeline",  global_data, shared_rv)
    mod_sc_annotation_server("annotation",global_data, shared_rv)
    mod_sc_viz_server(       "viz",       global_data, shared_rv)
    mod_sc_markers_server(   "markers",   global_data, shared_rv)
    mod_sc_corr_server(      "corr",      global_data, shared_rv)
    mod_sc_pathways_server(  "pathways",  global_data, shared_rv)
    mod_sc_trajectory_server("trajectory",global_data, shared_rv)

    # ── traj_reduction / traj_genes mirrors (written by mod_sc_trajectory_server)

    # =========================================================================
    # AUTO-PIPELINE MODAL
    # =========================================================================
    sc_log_rv <- reactiveVal("")
    output$sc_auto_log <- renderText({ sc_log_rv() })

    # ── Step-3.8A: sketch preset hint + PCA-dims sync ──────────────────────
    output$sc_ap_sketch_hint <- renderUI({
      req(global_data$sc_obj, input$sc_ap_sketch_preset)
      n_total <- ncol(global_data$sc_obj)
      params  <- resolve_sketch_preset(input$sc_ap_sketch_preset, n_total,
                                        input$sc_ap_sketch_ncells_custom)
      will_sketch <- params$ncells < n_total
      div(class="small", style=paste0("color:", if (will_sketch) "#18BC9C" else "#666", ";"),
          sprintf("%s / %s cellules \u2014 npcs sugg\u00e9r\u00e9 : %d%s",
                  format(params$ncells, big.mark=" "), format(n_total, big.mark=" "),
                  params$npcs,
                  if (will_sketch) " (sketch actif, slider Dims PCA ajust\u00e9)"
                  else " (pas de sketch)"))
    })

    observeEvent(input$sc_ap_sketch_preset, {
      req(global_data$sc_obj)
      params <- resolve_sketch_preset(input$sc_ap_sketch_preset, ncol(global_data$sc_obj),
                                       input$sc_ap_sketch_ncells_custom)
      updateSliderInput(session, "sc_ap_pca_dim", value = params$npcs)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_auto_pipeline_sc, {
      req(global_data$sc_obj)
      ns_m <- session$ns
      # Step-3.8B: pre-select organism from actual ID prefixes (ENSMUSG.../
      # ENSG...) -- was always defaulting to "Humain", the direct cause of
      # "None of the keys entered are valid keys for 'ENSEMBL'" on the real
      # mouse dataset (org.Hs.eg.db has no ENSMUSG keys). The mapping call
      # itself now also auto-corrects (see remap_seurat_ids_to_symbol()) but
      # pre-selecting here avoids relying on that silent correction.
      detected_map_org <- tryCatch(detect_organism_from_ids(rownames(global_data$sc_obj)),
                                   error = function(e) NA_character_)
      mapping_org_selected <- if (!is.na(detected_map_org)) detected_map_org else "human"
      showModal(modalDialog(
        title="\u25b6 Pipeline SC \u2014 Paramètres", size="m", easyClose=TRUE,

        # ── Step 0: Mapping ─────────────────────────────────────────────────
        checkboxInput(ns_m("sc_ap_mapping"),
                      "\U0001f504 Mapping IDs → Symbol (auto-détecté, avant QC)", value=TRUE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_mapping")),
          selectInput(ns_m("sc_ap_mapping_org"), "Organisme (mapping)",
                      c("Humain"="human","Souris"="mouse"), selected = mapping_org_selected)),
        checkboxInput(ns_m("sc_ap_bpcells"),
                      sprintf("\U0001f4bd Backend disque (BPCells) si > %s cellules",
                              format(.BPCELLS_AUTO_THRESHOLD, big.mark=" ")),
                      value = TRUE),
        hr(),

        # ── Step 1: QC ──────────────────────────────────────────────────────
        fluidRow(
          column(6,
            h6("QC", style="font-weight:bold;"),
            numericInput(ns_m("sc_ap_min_gene"), "Min gènes/cellule",   100, min=0),
            numericInput(ns_m("sc_ap_max_gene"), "Max gènes/cellule", 8000, min=0),
            sliderInput(ns_m("sc_ap_mt"), "% Mito max", 0, 50, 20, step=1)
          ),
          column(6,
            h6("Normalisation & Réduction", style="font-weight:bold;"),
            radioButtons(ns_m("sc_ap_norm"), "Normalisation",
                         c("LogNormalize"="log","SCTransform"="sct")),
            sliderInput(ns_m("sc_ap_pca_dim"), "Dims PCA", 5, 50, 20),
            numericInput(ns_m("sc_ap_res"), "Résolution clustering", 0.5, min=0.1, step=0.1),
            selectInput(ns_m("sc_ap_cluster_algo"), "Algorithme de clustering",
                       choices = c("Louvain"="1","Louvain (multilevel)"="2",
                                  "SLM"="3","Leiden (reticulate)"="4"), selected="1")
          )
        ),
        checkboxInput(ns_m("sc_ap_compute_umap"),
                     "\u2713 Calculer UMAP (d\u00e9cochez pour PCA seul \u2014 bien plus rapide, mode debug)",
                     value = TRUE),
        div(class="small text-muted mb-2",
            "Si coché : UMAP + t-SNE secondaire (si dataset raisonnable) sont calculés ",
            "(le plus lent du pipeline). Si décoché : PCA seul \u2014 previews/trajectoire ",
            "se rabattent automatiquement sur PCA, rien ne plante."),
        hr(),

        # ── Step-3.8A: Sketch (sous-échantillonnage intelligent) ─────────────
        h6("Sketch — gros datasets", style="font-weight:bold;"),
        div(class="small text-muted mb-1",
            "PCA/Clustering/UMAP tournent sur un sous-ensemble représentatif ",
            "(LeverageScore), puis sont projetés sur toutes les cellules. ",
            "Accélère fortement les gros datasets (ex: 1,3M cellules) sans ",
            "perdre les clusters rares. Ignoré si SCTransform est choisi."),
        fluidRow(
          column(7, selectInput(ns_m("sc_ap_sketch_preset"), "Preset sketch",
            choices = list(
              "Rapide (test, 5 000 cellules)"         = "fast",
              "Léger (10 000 cellules)"                = "light",
              "Moyen (25 000 cellules)"                = "medium",
              "Standard (50 000 cellules)"             = "standard",
              "Élevé (100 000 cellules)"                = "high",
              "Max (dataset complet, pas de sketch)"   = "max",
              "Personnalisé"                            = "custom"
            ), selected = "standard")),
          column(5, conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns_m("sc_ap_sketch_preset")),
            numericInput(ns_m("sc_ap_sketch_ncells_custom"), "N cellules",
                         value = 20000, min = 1000, max = 500000, step = 1000)))
        ),
        uiOutput(ns_m("sc_ap_sketch_hint")),
        hr(),

        # ── Steps 2-7 optional ──────────────────────────────────────────────
        h6("Options supplémentaires", style="font-weight:bold;"),
        checkboxInput(ns_m("sc_ap_singler"), "\U0001f9ec Annotation SingleR", value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_singler")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_singler_ref"), "Référence",
                       c("Human Primary Cell Atlas"="hpca","Blueprint Encode"="blueprint",
                         "ImmGen (Souris)"="immgen","DICE Immune"="dice"))),
            column(6, radioButtons(ns_m("sc_ap_singler_level"), "Niveau",
                       c("Main"="main","Fine"="fine"), inline=TRUE))
          )
        ),
        checkboxInput(ns_m("sc_ap_markers"),
                      "\U0001f9ec FindAllMarkers après clustering", value=FALSE),
        checkboxInput(ns_m("sc_ap_pathway"),
                      "\U0001f9ec Pathway ORA sur top marqueurs", value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_pathway")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_pathway_db"), "Base",
                       c("GO BP"="GOBP","KEGG"="KEGG","Reactome"="Reactome"))),
            column(6, selectInput(ns_m("sc_ap_pathway_org"), "Organisme",
                       c("Humain"="human","Souris"="mouse")))
          )
        ),
        checkboxInput(ns_m("sc_ap_correlation"),
                      "\U0001f9ec Gene Correlation (auto: gène le plus significatif)", value=FALSE),
        helpText(style="font-size:0.78em;color:#666;",
                 "Requiert 'Marqueurs' coché. Corrèle le gène à p-adj minimal avec tous les autres."),
        checkboxInput(ns_m("sc_ap_trajectory"),
                      "\U0001f9ec Trajectory / Pseudotemps (UMAP, racine auto)", value=FALSE),

        footer=tagList(
          modalButton("Annuler"),
          actionButton(ns_m("sc_ap_confirm"), "\u25b6 Lancer", class="btn-success"))
      ))
    })

    # =========================================================================
    # AUTO-PIPELINE SERVER
    # =========================================================================
    observeEvent(input$sc_ap_confirm, {
      removeModal()
      req(global_data$sc_obj)

      ll <- character(0)
      log_sc <- function(msg) {
        ll <<- c(ll, paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg))
        sc_log_rv(paste(ll, collapse="\n"))
      }

      p <- shiny::Progress$new(); on.exit(p$close())

      tryCatch({
        obj <- global_data$sc_obj

        # ── Step 0: Mapping IDs ─────────────────────────────────────────────
        if (isTRUE(input$sc_ap_mapping)) {
          detected <- tryCatch(detect_gene_id_type(rownames(obj)),
                               error=function(e) "unknown")
          if (detected %in% c("ensembl","entrez")) {
            p$set(0.02,"Mapping IDs..."); log_sc(sprintf("Mapping IDs (%s)...", detected))
            map_res <- tryCatch(
              withCallingHandlers(
                remap_seurat_ids_to_symbol(obj,
                  from_type        = detected,
                  organism         = input$sc_ap_mapping_org %||% "human",
                  collapse_method  = "sum"),
                warning = function(w) { log_sc(paste("\u2139\ufe0f", conditionMessage(w))); invokeRestart("muffleWarning") }
              ),
              error=function(e) { log_sc(paste("\u26a0\ufe0f Mapping ignoré:", e$message)); NULL }
            )
            if (!is.null(map_res)) {
              obj <- map_res$object
              log_sc(sprintf("\u2713 Mapping : %d gènes finaux (%d mappés, %d non-mappés)",
                             nrow(obj), map_res$n_mapped, map_res$n_unmapped))
            }
          } else {
            log_sc("Mapping IDs : symboles déjà détectés (ou type inconnu) — ignoré.")
          }
        }

        # ── Step 1: QC ──────────────────────────────────────────────────────
        p$set(0.05,"QC..."); log_sc("QC...")
        mt_pat <- if (any(grepl("^MT-",rownames(obj)))) "^MT-"
                  else if (any(grepl("^mt-",rownames(obj)))) "^mt-" else NULL
        obj[["percent.mt"]] <- if (!is.null(mt_pat))
          PercentageFeatureSet(obj, pattern=mt_pat) else 0
        n_before <- ncol(obj)
        obj <- subset(obj,
                      subset = nFeature_RNA > input$sc_ap_min_gene &
                               nFeature_RNA < input$sc_ap_max_gene &
                               percent.mt   < input$sc_ap_mt)
        if (ncol(obj) < 10) stop(sprintf(
          "Seulement %d cellule(s) après QC (départ: %d). Réduisez les seuils.", ncol(obj), n_before))
        log_sc(sprintf("\u2713 QC : %d cellules (retirées: %d)", ncol(obj), n_before-ncol(obj)))

        # ── Step 1b: Backend disque (BPCells) — Step-3.7A ────────────────────
        if (isTRUE(input$sc_ap_bpcells) && ncol(obj) > .BPCELLS_AUTO_THRESHOLD &&
            sc_backend_status(obj) == "memory") {
          if (!.bpcells_available()) {
            log_sc("\u26a0\ufe0f BPCells non installé — pipeline exécuté en RAM.")
          } else {
            conv <- tryCatch(convert_seurat_to_bpcells(obj),
                             error=function(e){ log_sc(paste("\u26a0\ufe0f BPCells:", e$message)); NULL })
            if (!is.null(conv)) {
              obj <- conv$object
              if (!isTRUE(conv$already_disk)) {
                session$onSessionEnded(function() unlink(conv$dir, recursive = TRUE))
                log_sc(sprintf("\u2713 Backend disque (BPCells) activé — %s cellules",
                               format(conv$n_cells, big.mark=" ")))
              }
            }
          }
        }

        # ── Step 2-5: Normalisation / PCA / Clustering / UMAP (Step-3.8A) ────
        # Sketch workflow (Seurat v5): analyse sur un sous-ensemble représentatif
        # (LeverageScore) puis projection sur le dataset complet via ProjectData().
        # Voir resolve_sketch_preset()/standardize_sketch_reductions() (helpers_sc.R).
        n_total_cells <- ncol(obj)
        sketch_params <- resolve_sketch_preset(
          input$sc_ap_sketch_preset %||% "standard", n_total_cells,
          input$sc_ap_sketch_ncells_custom)
        use_sketch <- !identical(input$sc_ap_norm, "sct") &&
                      sketch_params$ncells < n_total_cells
        pca_dim <- input$sc_ap_pca_dim  # fallback / full-dataset path

        if (sc_backend_status(obj) == "disk") {
          .ap_old_plan <- future::plan()
          on.exit(future::plan(.ap_old_plan), add = TRUE)
          future::plan("sequential")
          log_sc("\u2139\ufe0f Backend disque : future séquentiel forcé (Normalisation \u2192 Clustering) pour éviter un crash 'globals size'.")
        }

        if (isTRUE(use_sketch)) {
          # ── Sketch: analyse sur sous-ensemble ────────────────────────────
          .t_sketch <- Sys.time()
          p$set(0.15,"Sketch..."); log_sc(sprintf(
            "Sketch : %s / %s cellules (preset '%s')...",
            format(sketch_params$ncells, big.mark=" "), format(n_total_cells, big.mark=" "),
            input$sc_ap_sketch_preset))
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj, verbose=FALSE)
          obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
          obj <- SketchData(object=obj, ncells=sketch_params$ncells,
                            method="LeverageScore", sketched.assay="sketch")
          DefaultAssay(obj) <- "sketch"
          log_sc(sprintf("\u2713 Sketch OK (%.0fs)", as.numeric(difftime(Sys.time(), .t_sketch, units="secs"))))

          p$set(0.30,"Normalisation (sketch)..."); log_sc("Normalisation (sketch)...")
          obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
          obj <- ScaleData(obj, verbose=FALSE)
          log_sc("\u2713 Normalisation OK")

          p$set(0.40,"PCA (sketch)...")
          obj <- RunPCA(obj, npcs=sketch_params$npcs, verbose=FALSE)
          log_sc(sprintf("\u2713 PCA (%d dims, sketch)", sketch_params$npcs))

          p$set(0.55,"Clustering (sketch)...")
          obj <- FindNeighbors(obj, dims=1:sketch_params$npcs, verbose=FALSE)
          obj <- robust_find_clusters(obj, resolution=input$sc_ap_res, algo=input$sc_ap_cluster_algo,
                                      log_fn=function(m) log_sc(paste("\u26a0\ufe0f", m)))
          log_sc(sprintf("\u2713 Clustering sketch OK (res %.1f)", input$sc_ap_res))

          # Step-3.8B: UMAP is the slowest step by far on large sketches --
          # skippable for fast debug iteration. When skipped, ProjectData()
          # below simply omits umap.model= (PCA-only projection); trajectory
          # (Step 9) and any live/report preview fall back to PCA automatically.
          compute_umap_sketch <- isTRUE(input$sc_ap_compute_umap)
          if (compute_umap_sketch) {
            p$set(0.63,"UMAP (sketch)...")
            obj <- RunUMAP(obj, dims=1:sketch_params$npcs, reduction="pca",
                           return.model=TRUE, verbose=FALSE)
            log_sc("\u2713 UMAP sketch OK")
          } else {
            log_sc("\u2139\ufe0f UMAP d\u00e9sactiv\u00e9 (mode PCA seul, debug rapide) \u2014 previews/trajectoire utiliseront PCA.")
          }

          # ── Projection sketch → dataset complet ──────────────────────────
          .t_project <- Sys.time()
          p$set(0.68,"Projection sur le dataset complet...")
          log_sc("Projection (ProjectData) sur le dataset complet...")
          project_args <- list(
            object=obj, assay="RNA", sketched.assay="sketch",
            sketched.reduction="pca", full.reduction="pca.full",
            dims=1:sketch_params$npcs,
            refdata=list(seurat_clusters="seurat_clusters"))
          if (compute_umap_sketch) project_args$umap.model <- "umap"
          obj <- do.call(ProjectData, project_args)
          obj <- standardize_sketch_reductions(obj, full_pca_name="pca.full")
          DefaultAssay(obj) <- "RNA"
          pca_dim <- sketch_params$npcs
          log_sc(sprintf("\u2713 Projection OK \u2014 %s cellules (%.0fs)",
                         format(ncol(obj), big.mark=" "), as.numeric(difftime(Sys.time(), .t_project, units="secs"))))

        } else {
          # ── Dataset complet (comportement existant, inchangé) ────────────
          if (identical(input$sc_ap_norm, "sct"))
            log_sc("\u2139\ufe0f Sketch non support\u00e9 avec SCTransform \u2014 pipeline sur dataset complet.")
          else
            log_sc("\u2139\ufe0f Sketch ignor\u00e9 : preset \u2265 taille du dataset \u2014 pipeline sur dataset complet.")

          p$set(0.20,"Normalisation..."); log_sc("Normalisation...")
          if (input$sc_ap_norm=="sct") {
            obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")
          } else {
            DefaultAssay(obj) <- "RNA"
            obj <- NormalizeData(obj, verbose=FALSE)
            obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
            obj <- smart_scale_data(obj)   # Step-3.7A: RAM-safe (VariableFeatures only)
          }
          log_sc("\u2713 Normalisation OK")

          p$set(0.40,"PCA...")
          obj <- RunPCA(obj, verbose=FALSE, npcs=pca_dim)
          log_sc(sprintf("\u2713 PCA (%d dims)", pca_dim))

          p$set(0.55,"Clustering...")
          obj <- FindNeighbors(obj, dims=1:pca_dim, verbose=FALSE)
          obj <- robust_find_clusters(obj, resolution=input$sc_ap_res, algo=input$sc_ap_cluster_algo,
                                      log_fn=function(m) log_sc(paste("\u26a0\ufe0f", m)))
          log_sc(sprintf("\u2713 %d clusters (res %.1f)", length(unique(obj$seurat_clusters)), input$sc_ap_res))

          if (isTRUE(input$sc_ap_compute_umap)) {
            p$set(0.68,"UMAP...")
            obj <- RunUMAP(obj, dims=1:pca_dim, verbose=FALSE)
            log_sc("\u2713 UMAP OK")
          } else {
            log_sc("\u2139\ufe0f UMAP d\u00e9sactiv\u00e9 (mode PCA seul, debug rapide).")
          }
        }

        n_cl <- length(unique(obj$seurat_clusters))
        if (isTRUE(use_sketch)) log_sc(sprintf("\u2713 %d clusters (projet\u00e9s sur dataset complet)", n_cl))

        # ── Step 5b: t-SNE secondaire (Step-3.7) ──────────────────────────────
        # Toujours calculé (si dataset raisonnable) pour être disponible aux
        # côtés de PCA/UMAP dans le picker "Réduction à visualiser" — même
        # constante de garde que le module "1. Pipeline" (.AUTO_TSNE_MAX_CELLS).
        if (!isTRUE(input$sc_ap_compute_umap)) {
          log_sc("\u2139\ufe0f t-SNE secondaire ignor\u00e9 (UMAP d\u00e9sactiv\u00e9, mode PCA seul).")
        } else {
          p$set(0.72,"t-SNE (secondaire)...")
          if (ncol(obj) > .AUTO_TSNE_MAX_CELLS) {
            log_sc(sprintf("\u26a0\ufe0f t-SNE secondaire ignoré (%s cellules > %s max).",
                           format(ncol(obj), big.mark=" "), format(.AUTO_TSNE_MAX_CELLS, big.mark=" ")))
          } else {
            obj <- tryCatch(RunTSNE(obj, dims=1:pca_dim, verbose=FALSE),
                            error=function(e){ log_sc(paste("\u26a0\ufe0f t-SNE secondaire ignoré:", e$message)); obj })
            log_sc("\u2713 t-SNE secondaire OK")
          }
        }

        # ── Step 6: SingleR (optional) ───────────────────────────────────────
        if (isTRUE(input$sc_ap_singler)) {
          if (!requireNamespace("SingleR",quietly=TRUE) ||
              !requireNamespace("celldex",quietly=TRUE)) {
            log_sc("\u26a0\ufe0f SingleR/celldex non installés — annotation ignorée.")
          } else {
            .t_singler <- Sys.time()
            p$set(0.76,"Annotation SingleR...")
            result <- tryCatch(
              withCallingHandlers(
                .run_singler_safe(obj, input$sc_ap_singler_ref, input$sc_ap_singler_level),
                warning=function(w) {
                  log_sc(paste("\u26a0\ufe0f", conditionMessage(w)))
                  invokeRestart("muffleWarning")
                }),
              error=function(e) { log_sc(paste("\u26a0\ufe0f SingleR:", e$message)); NULL }
            )
            if (!is.null(result)) {
              col_name <- paste0("SingleR_", input$sc_ap_singler_ref, "_", input$sc_ap_singler_level)
              obj[[col_name]] <- result$labels
              log_sc(sprintf("\u2713 Annoté [%s] — %d types (%.0fs)",
                             result$method, length(unique(result$labels)),
                             as.numeric(difftime(Sys.time(), .t_singler, units="secs"))))
            }
          }
        }

        # ── Step 7: FindAllMarkers (optional, also needed for correlation) ───
        # Step-3.7: runs on a RAM-safety-capped subsample (shared_rv$max_cells_heavy,
        # set in "1. Pipeline") — `obj` itself (UMAP/t-SNE/clusters) stays full-size.
        if (isTRUE(input$sc_ap_markers) || isTRUE(input$sc_ap_correlation)) {
          p$set(0.82,"FindAllMarkers...")
          cap_m   <- shared_rv$max_cells_heavy %||% Inf
          sub_res <- subsample_seurat_for_analysis(obj, max_per_group = cap_m, group_col = "seurat_clusters")
          if (sub_res$was_subsampled)
            log_sc(sprintf("\u2139\ufe0f Sous-échantillonnage marqueurs : %d \u2192 %d cellules (max %d/cluster)",
                           sub_res$n_before, sub_res$n_after, cap_m))
          log_sc("FindAllMarkers...")
          markers <- tryCatch({
            Idents(sub_res$object) <- sub_res$object$seurat_clusters
            FindAllMarkers(sub_res$object, only.pos=TRUE, min.pct=0.1,
                           logfc.threshold=0.25, verbose=FALSE)
          }, error=function(e) { log_sc(paste("\u26a0\ufe0f Markers:", e$message)); NULL })

          if (!is.null(markers) && nrow(markers) > 0) {
            markers <- as.data.frame(markers); rownames(markers) <- NULL
            if (!"gene"       %in% colnames(markers)) markers$gene       <- rownames(markers)
            if (!"avg_log2FC" %in% colnames(markers)) markers$avg_log2FC <- markers$avg_logFC %||% 0
            if (!"p_val_adj"  %in% colnames(markers)) markers$p_val_adj  <- 1
            if (!"cluster"    %in% colnames(markers)) markers$cluster    <- "Unknown"
            if (!"pct.1"      %in% colnames(markers)) markers$pct.1      <- NA_real_
            if (!"pct.2"      %in% colnames(markers)) markers$pct.2      <- NA_real_
            shared_rv$markers_data <- markers
            log_sc(sprintf("\u2713 %d marqueurs", nrow(markers)))

            # Step 7b: Pathway ORA on top markers (optional)
            if (isTRUE(input$sc_ap_pathway)) {
              .t_pathway <- Sys.time()
              log_sc("Pathway ORA...")
              pathway_org <- input$sc_ap_pathway_org %||% "human"
              top_g_raw   <- head(markers$gene[order(markers$p_val_adj)], 100)
              # Step-3.8B: .remap_if_ensg() (mod_sc_pathways.R, globally
              # available -- sourced before this module in app.R) converts
              # ENSEMBL marker IDs to symbols before bitr(); a no-op if
              # markers are already symbols (e.g. Step 0 mapping succeeded).
              # Without this, sketch/auto-pipeline runs where mapping was
              # skipped or failed always produced "Aucun gene converti".
              top_g <- .remap_if_ensg(top_g_raw, pathway_org,
                                      notify_fn = function(msg, ...) log_sc(paste("\u2139\ufe0f", msg)))
              if (length(top_g) == 0) {
                log_sc(sprintf("\u26a0\ufe0f Pathway ignoré : 0/%d gènes convertibles (organisme '%s'). Exemples : %s.",
                               length(top_g_raw), pathway_org, paste(head(top_g_raw, 5), collapse=", ")))
              } else {
                pw <- tryCatch(
                  run_pathway_enrichment(top_g,
                                         organism = pathway_org,
                                         database = input$sc_ap_pathway_db %||% "GOBP",
                                         pval_cutoff = 0.05),
                  error=function(e) { log_sc(paste("\u26a0\ufe0f Pathway:", e$message,
                                                    "\u2014 exemples testés :",
                                                    paste(head(top_g, 5), collapse=", "))); NULL }
                )
                if (!is.null(pw) && nrow(pw) > 0) {
                  shared_rv$pathway_results <- pw
                  shared_rv$pathway_db      <- input$sc_ap_pathway_db %||% "GOBP"
                  log_sc(sprintf("\u2713 %d pathways (%d/%d gènes convertis, %.0fs)", nrow(pw), length(top_g), length(top_g_raw),
                                 as.numeric(difftime(Sys.time(), .t_pathway, units="secs"))))
                }
              }
            }

          } else {
            log_sc("\u26a0\ufe0f Aucun marqueur trouvé.")
          }
        }

        # ── Step 8: Gene Correlation (optional) — top significant marker ─────
        # Step-3.7: also subsampled (stratified by orig.ident) with the same cap.
        if (isTRUE(input$sc_ap_correlation)) {
          p$set(0.90,"Corrélation..."); log_sc("Gene Correlation...")
          target_gene <- NULL
          if (!is.null(shared_rv$markers_data) && nrow(shared_rv$markers_data) > 0) {
            ranked      <- shared_rv$markers_data[order(shared_rv$markers_data$p_val_adj), ]
            target_gene <- ranked$gene[1]
          }
          if (is.null(target_gene)) {
            log_sc("\u26a0\ufe0f Corrélation ignorée : aucun marqueur disponible (cochez 'Marqueurs').")
          } else {
            cap_c     <- shared_rv$max_cells_heavy %||% Inf
            sub_res_c <- subsample_seurat_for_analysis(obj, max_per_group = cap_c, group_col = "orig.ident")
            if (sub_res_c$was_subsampled)
              log_sc(sprintf("\u2139\ufe0f Sous-échantillonnage corrélation : %d \u2192 %d cellules (max %d/échantillon)",
                             sub_res_c$n_before, sub_res_c$n_after, cap_c))
            corr_res <- tryCatch(
              find_correlated_genes(sub_res_c$object, target_gene=target_gene,
                                    method="pearson", threshold=0.3, top_n=50),
              error=function(e) { log_sc(paste("\u26a0\ufe0f Corrélation:", e$message)); NULL }
            )
            if (!is.null(corr_res) && nrow(corr_res) > 0) {
              shared_rv$correlated_genes <- corr_res
              shared_rv$corr_target_gene <- target_gene
              log_sc(sprintf("\u2713 %d gènes corrélés avec %s (top marqueur)",
                             nrow(corr_res), target_gene))
            } else {
              log_sc(sprintf("\u26a0\ufe0f Aucun gène corrélé pour %s (seuil |r|\u22650.3).", target_gene))
            }
          }
        }

        # ── Step 9: Trajectory (optional) ────────────────────────────────────
        if (isTRUE(input$sc_ap_trajectory)) {
          p$set(0.95,"Trajectoire..."); log_sc("Trajectory / Pseudotime...")
          if (ncol(obj) > .MAX_TRAJECTORY_CELLS) {
            log_sc(sprintf("\u26a0\ufe0f Trajectoire ignorée : dataset trop grand (%d > %d).",
                           ncol(obj), .MAX_TRAJECTORY_CELLS))
          } else {
            # Step-3.8B: fall back to PCA if UMAP was skipped ("PCA seul" mode)
            traj_red_use <- if ("umap" %in% names(obj@reductions)) "umap" else "pca"
            traj_res <- tryCatch(
              calculate_pseudotime(obj, reduction=traj_red_use, root_cells=NULL),
              error=function(e) { log_sc(paste("\u26a0\ufe0f Trajectoire:", e$message)); NULL }
            )
            if (!is.null(traj_res)) {
              obj                      <- traj_res
              shared_rv$traj_reduction <- traj_red_use
              log_sc(sprintf("\u2713 Pseudotemps calculé (racine auto, réduction: %s)", toupper(traj_red_use)))
            }
          }
        }

        # ── Commit ───────────────────────────────────────────────────────────
        global_data$sc_obj   <- obj
        shared_rv$active_tab <- "tab_viz"
        showNotification(
          sprintf("\u2713 Pipeline SC : %d cellules, %d clusters", ncol(obj), n_cl),
          type="message", duration=6)

      }, error=function(e) {
        log_sc(paste("\u274c Erreur:", e$message))
        showNotification(paste("Erreur pipeline SC:", e$message), type="error", duration=10)
      })
    })

    # =========================================================================
    # REPORT STATUS
    # =========================================================================
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) "Importez et traitez un objet SC."
      else sprintf("Prêt — %d viz. sauvegardée(s) dans le panier.",
                   length(shared_rv$report_viz_list %||% list()))
    })

    # =========================================================================
    # HTML / PDF REPORT
    # =========================================================================
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html="html", pdf="pdf", both="zip")
        paste0("rapport_singlecell_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(global_data$sc_obj)
        template_path <- file.path("modules","sc","sc_report_template.Rmd")
        if (!file.exists(template_path))
          stop("Template introuvable : modules/sc/sc_report_template.Rmd")
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite=TRUE)

        # NULL-guard corr params
        corr_genes  <- if (!is.null(shared_rv$correlated_genes) &&
                           is.data.frame(shared_rv$correlated_genes) &&
                           nrow(shared_rv$correlated_genes) > 0) shared_rv$correlated_genes else NULL
        corr_target <- if (!is.null(shared_rv$corr_target_gene) &&
                           nchar(shared_rv$corr_target_gene %||% "") > 0) shared_rv$corr_target_gene else NULL

        render_params <- list(
          sc_obj           = global_data$sc_obj,
          markers_data     = shared_rv$markers_data,
          pathway_results  = shared_rv$pathway_results,
          pathway_db       = shared_rv$pathway_db,
          correlated_genes = corr_genes,
          corr_target_gene = corr_target,
          sections         = input$report_sections %||% character(0),
          reduction        = "umap",
          traj_reduction   = shared_rv$traj_reduction %||% "umap",
          traj_genes       = shared_rv$traj_genes %||% character(0),  # Step-3.7
          saved_viz_list   = if (length(shared_rv$report_viz_list)) shared_rv$report_viz_list else NULL,
          group_by         = "seurat_clusters",
          report_title     = input$report_title    %||% "Analyse Single-Cell",
          report_subtitle  = input$report_subtitle %||% "",
          report_notes     = input$report_notes    %||% "",
          interactive      = isTRUE(input$report_interactive) && input$report_format != "pdf"
        )

        withProgress(message="Génération du rapport...", value=0.2, {
          formats_needed <- switch(input$report_format,
            html="html_document", pdf="pdf_document",
            both=c("html_document","pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail=paste("Rendu", fmt))
            ext_i    <- if (fmt=="html_document") "html" else "pdf"
            out_path <- tempfile(pattern=paste0("sc_report_",ext_i,"_"),
                                 fileext=paste0(".",ext_i))
            res <- tryCatch(
              rmarkdown::render(input=tmp_rmd, output_format=fmt, output_file=out_path,
                                params=render_params, envir=new.env(parent=globalenv()),
                                quiet=TRUE),
              error=function(e) {
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)),
                                 type="error", duration=12); NULL })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (!length(out_files)) stop("Aucun format généré.")
          else if (length(out_files)==1) file.copy(out_files[1], file, overwrite=TRUE)
          else zip::zip(file, files=out_files, mode="cherry-pick")
        })
      }
    )

    # =========================================================================
    # SC REPRODUCIBLE R SCRIPT
    # =========================================================================
    output$dl_sc_r_script <- downloadHandler(
      filename = function() paste0("analyse_sc_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$sc_obj)
        obj     <- global_data$sc_obj
        tmp_dir <- tempfile("sc_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive=TRUE), add=TRUE)
        stamp       <- format(Sys.time(),"%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_sc_",stamp,".R"))
        rds_path    <- file.path(tmp_dir, "sc_obj.rds")
        writeLines(.sc_r_script_text(obj, shared_rv), script_path)
        saveRDS(obj, rds_path)
        zip::zip(file, files=c(script_path, rds_path), mode="cherry-pick")
        showNotification("\u2713 Script R généré.", type="message", duration=4)
      }
    )

  }) # /moduleServer
}


# =============================================================================
# .sc_r_script_text — reproducible SC analysis script (introspects Seurat obj)
# =============================================================================
.sc_r_script_text <- function(obj, shared_rv = NULL) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  meta         <- obj@meta.data
  n_cells      <- ncol(obj)
  n_genes      <- nrow(obj)
  date         <- format(Sys.Date(), "%Y-%m-%d")
  has_umap     <- "umap"            %in% names(obj@reductions)
  has_pca      <- "pca"             %in% names(obj@reductions)
  has_mt       <- "percent.mt"      %in% colnames(meta)
  has_clusters <- "seurat_clusters" %in% colnames(meta)
  singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
  has_singler  <- length(singler_cols) > 0
  has_markers  <- !is.null(shared_rv) && !is.null(shared_rv$markers_data) &&
                  nrow(shared_rv$markers_data) > 0
  has_corr     <- !is.null(shared_rv) && !is.null(shared_rv$corr_target_gene)
  has_traj     <- "pseudotime" %in% colnames(meta)
  pca_dims     <- if (has_pca) min(ncol(Embeddings(obj,"pca")), 50) else 20
  n_clusters   <- if (has_clusters) length(levels(factor(meta$seurat_clusters))) else "?"

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny (Single-Cell)
# Généré le : ', date, '
# Dataset    : ', n_genes, ' gènes \u00d7 ', n_cells, ' cellules
# Pipeline   : PCA=', if(has_pca)"oui" else "non",
  ', UMAP=', if(has_umap)"oui" else "non",
  ', clusters=', if(has_clusters) n_clusters else "non",
  ', SingleR=', if(has_singler) paste(singler_cols,collapse=",") else "non",
  ', Corr=', if(has_corr) shared_rv$corr_target_gene else "non",
  ', Traj=', if(has_traj) "oui" else "non", '
# =============================================================================

library(Seurat); library(ggplot2); library(patchwork)

# \u2500\u2500 0. Charger l\'objet ────────────────────────────────────────────────────────
obj <- readRDS("sc_obj.rds")
cat(sprintf("Objet : %d cellules, %d gènes\\n", ncol(obj), nrow(obj)))

# \u2500\u2500 1. QC ──────────────────────────────────────────────────────────────────────
',
if (!has_mt) '
mt_pat <- if (any(grepl("^MT-",rownames(obj)))) "^MT-"
          else if (any(grepl("^mt-",rownames(obj)))) "^mt-" else NULL
if (!is.null(mt_pat)) obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern=mt_pat)
' else '# percent.mt déjà calculé dans l\'objet exporté.',
'
VlnPlot(obj, features=c("nFeature_RNA","nCount_RNA"',
if(has_mt) ',"percent.mt"' else '','), ncol=', if(has_mt) 3 else 2, ', pt.size=0)

MIN_GENES <- 100; MAX_GENES <- 8000; MAX_MT <- 20
# obj <- subset(obj, subset=nFeature_RNA>MIN_GENES & nFeature_RNA<MAX_GENES & percent.mt<MAX_MT)

# \u2500\u2500 2. Normalisation ────────────────────────────────────────────────────────────
DefaultAssay(obj) <- "RNA"
obj <- NormalizeData(obj); obj <- FindVariableFeatures(obj, nfeatures=2000); obj <- ScaleData(obj)
# Alternative : obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")

# \u2500\u2500 3. PCA + Clustering + UMAP/t-SNE ────────────────────────────────────────────
PCA_DIMS  <- ', pca_dims, '
CLUST_RES <- 0.5
obj <- RunPCA(obj, npcs=PCA_DIMS, verbose=FALSE)
obj <- FindNeighbors(obj, dims=1:PCA_DIMS); obj <- FindClusters(obj, resolution=CLUST_RES)
obj <- RunUMAP(obj, dims=1:PCA_DIMS, verbose=FALSE)
obj <- RunTSNE(obj, dims=1:PCA_DIMS, verbose=FALSE)   # secondaire, dispo dans l\'app aux côtés d\'UMAP
p_umap <- DimPlot(obj, reduction="umap", label=TRUE, pt.size=0.5)
print(p_umap)
ggsave(paste0("umap_clusters_',date,'.png"), p_umap, width=8, height=6, dpi=300)

# \u2500\u2500 4. Marqueurs ────────────────────────────────────────────────────────────────
',
if (has_markers) paste0('# ', nrow(shared_rv$markers_data), ' marqueurs dans l\'app (recalculés ci-dessous, sur',
  ' l\'objet complet — l\'app peut avoir sous-échantillonné pour accélérer le calcul) :') else '',
'
Idents(obj) <- obj$seurat_clusters
markers <- FindAllMarkers(obj, only.pos=TRUE, min.pct=0.1, logfc.threshold=0.25, verbose=FALSE)
write.csv(markers, paste0("markers_', date, '.csv"), row.names=FALSE)
top5 <- markers |> dplyr::group_by(cluster) |> dplyr::slice_min(p_val_adj, n=5)
print(DotPlot(obj, features=unique(top5$gene)) + theme(axis.text.x=element_text(angle=45,hjust=1)))

# \u2500\u2500 5. Annotation SingleR (optionnel) ────────────────────────────────────────────
# BiocManager::install(c("SingleR","celldex"))
',
if (has_singler) paste0(
'# Déjà annoté (colonne : ', tail(singler_cols,1), ') :
print(DimPlot(obj, group.by="', tail(singler_cols,1), '", label=TRUE, repel=TRUE))'
) else
'# if (requireNamespace("SingleR",quietly=TRUE)) {
#   ref  <- celldex::HumanPrimaryCellAtlasData()
#   sce  <- as.SingleCellExperiment(obj)
#   pred <- SingleR::SingleR(test=sce, ref=ref, labels=ref$label.main)
#   obj[["SingleR"]] <- pred$labels
# }',
'

# \u2500\u2500 6. Gene Correlation ──────────────────────────────────────────────────────────
',
if (has_corr) paste0(
'# Gène cible utilisé dans l\'app : ', shared_rv$corr_target_gene, '
TARGET_GENE <- "', shared_rv$corr_target_gene, '"
# Corrélation (top 50 gènes les plus corrélés) :
# corr_df <- find_correlated_genes(obj, TARGET_GENE, method="pearson", threshold=0.3, top_n=50)
# print(plot_gene_correlation_network(corr_df, TARGET_GENE, top_n=20))'
) else
'# Lancez l\'étape 5 (Gene Correlation) dans l\'app pour générer le code de corrélation.',
'

# \u2500\u2500 7. Trajectory / Pseudotemps ────────────────────────────────────────────────
',
if (has_traj) paste0(
'# Pseudotemps déjà calculé dans l\'objet exporté.
ggplot(data.frame(pseudotime=obj$pseudotime, cluster=obj$seurat_clusters),
       aes(x=pseudotime, fill=cluster)) +
  geom_density(alpha=0.6) + scale_fill_viridis_d(option="turbo") +
  labs(title="Distribution Pseudotemps", x="Pseudotemps", y="Densité") + theme_minimal()'
) else
'# obj <- calculate_pseudotime(obj, reduction="umap")
# ggplot(data.frame(pseudotime=obj$pseudotime, cluster=obj$seurat_clusters),
#        aes(x=pseudotime, fill=cluster)) +
#   geom_density(alpha=0.6) + scale_fill_viridis_d(option="turbo") + theme_minimal()',
'

# \u2500\u2500 8. Sauvegarde ────────────────────────────────────────────────────────────────
saveRDS(obj, paste0("sc_obj_processed_', date, '.rds"))
cat("Objet sauvegardé.\\n")
'
  )
}
