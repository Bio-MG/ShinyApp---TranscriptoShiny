# mod_sc_pathways.R  —  Child 6: GO / KEGG / Reactome enrichment
# Step-3.6: auto-remap ENSG→Symbol before enrichment when gene IDs are Ensembl
# Step-3.7: BUG1 fix — pathway_rv (+ the i18n$t("Base de donnees") selector) is now
#   synced from shared_rv$pathway_results / shared_rv$pathway_db, so results
#   written by the auto-pipeline (mod_sc.R) show up here immediately instead
#   of needing a manual i18n$t("Lancer Enrichissement") click (same class of bug as
#   mod_sc_markers.R / mod_sc_corr.R).

# ── Helper: remap ENSG IDs → Symbols if detected ─────────────────────────────
.remap_if_ensg <- function(genes, organism = "human", notify_fn = NULL) {
  id_type <- tryCatch(detect_gene_id_type(genes), error = function(e) "unknown")
  if (id_type != "ensembl") return(genes)

  orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"
  if (!requireNamespace("AnnotationDbi", quietly = TRUE) ||
      !requireNamespace(orgdb_pkg,       quietly = TRUE)) {
    if (!is.null(notify_fn))
      notify_fn(paste0("⚠️ IDs ENSEMBL détectés mais ", orgdb_pkg,
                       " non installé — enrichissement peut échouer."), type = "warning")
    return(genes)
  }

  orgdb     <- getExportedValue(orgdb_pkg, orgdb_pkg)
  ids_clean <- gsub("\\.[0-9]+$", "", genes)   # strip version suffix
  sym <- tryCatch(
    AnnotationDbi::mapIds(orgdb, keys = unique(ids_clean),
                          keytype = "ENSEMBL", column = "SYMBOL", multiVals = "first"),
    error = function(e) NULL
  )
  if (is.null(sym)) return(genes)

  mapped <- as.character(sym[!is.na(sym) & nchar(sym) > 0])
  if (length(mapped) == 0) return(genes)

  if (!is.null(notify_fn))
    notify_fn(sprintf("ℹ️ %d IDs ENSEMBL convertis en symboles avant enrichissement.", length(mapped)),
              type = "message", duration = 5)
  mapped
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light",style="font-size:0.9em;border-left:3px solid #9B59B6;",
        i18n$t("Analyse d'enrichissement de voies biologiques."),
        tags$br(),
        tags$small(i18n$t("Les IDs Ensembl (ENSG…) sont convertis automatiquement en symboles."))),
    selectInput(ns("pathway_source"), i18n$t("Source de genes"),
      choices=setNames(c("markers","correlated","manual"), c(.tr_plain("Marqueurs calculés"), .tr_plain("Genes correles"), .tr_plain("Selection manuelle")))),
    conditionalPanel(condition="input.pathway_source == 'manual'", ns=ns,
      selectizeInput(ns("pathway_genes"), "Genes", choices=NULL, multiple=TRUE,
                     options=list(placeholder=i18n$t("Selectionnez genes")))),
    fluidRow(
      column(6, selectInput(ns("pathway_db"), i18n$t("Base de donnees"),
               choices=setNames(c("GOBP","KEGG","Reactome"), c(.tr_plain("GO Biological Process"), .tr_plain("KEGG Pathways"), .tr_plain("Reactome"))))),
      column(6, selectInput(ns("pathway_org"), i18n$t("Organisme"),
               choices=setNames(c("human","mouse"), c(.tr_plain("Humain"), .tr_plain("Souris")))))
    ),
    numericInput(ns("pathway_pval"), i18n$t("P-value cutoff"), value=0.05, min=0.001, max=0.1, step=0.01),
    actionButton(ns("run_pathway"), i18n$t("Lancer Enrichissement"), class="btn-warning w-100", icon=icon("dna")),
    hr(),
    downloadButton(ns("dl_pathway"), "Export CSV", class="btn-sm btn-info w-100"),
    hr(),
    div(class="small text-muted", textOutput(ns("pathway_status")))
  )
}

mod_sc_pathways_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen=TRUE,
    card_header(div(style="display:flex;justify-content:space-between;align-items:center;",
                    h5("Pathway Enrichment", class="mb-0"),
                    downloadButton(ns("dl_pathway_header"), "Export", class="btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Barplot Top 15"), plotOutput(ns("pathway_barplot"), height="500px")),
      nav_panel(i18n$t("Dotplot"),        plotOutput(ns("pathway_dotplot"), height="500px")),
      nav_panel(i18n$t("Table"),          DTOutput(ns("pathway_table")))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }


    pathway_rv <- reactiveVal(NULL)

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      updateSelectizeInput(session, "pathway_genes",
                           choices=rownames(global_data$sc_obj), server=TRUE)
    })

    observeEvent(global_data$language, {
      # Re-translate dynamic labels on language switch
    }, ignoreInit = TRUE)
    # ── Step-3.7 BUG1 fix: sync local table + db selector from shared_rv,
    #    written by either this module's own button OR the auto-pipeline. ──────
    observeEvent(shared_rv$pathway_results, {
      pathway_rv(shared_rv$pathway_results)
    }, ignoreNULL = FALSE)

    observeEvent(shared_rv$pathway_db, {
      req(shared_rv$pathway_db)
      updateSelectInput(session, "pathway_db", selected = shared_rv$pathway_db)
    }, ignoreInit = TRUE)

    observeEvent(input$run_pathway, {
      req(global_data$sc_obj)
      genes_to_test <- NULL

      if (input$pathway_source == "markers") {
        df <- shared_rv$markers_data
        if (is.null(df) || nrow(df)==0) {
          showNotification(.tr("⚠️ Lancez d'abord l'étape 4 (Marqueurs)."), type="warning"); return()
        }
        genes_to_test <- head(df$gene, 100)

      } else if (input$pathway_source == "correlated") {
        df <- shared_rv$correlated_genes
        if (is.null(df) || nrow(df)==0) {
          showNotification(.tr("⚠️ Lancez d'abord l'étape 5 (Corrélation)."), type="warning"); return()
        }
        genes_to_test <- c(shared_rv$corr_target_gene, df$gene)

      } else if (input$pathway_source == "manual") {
        req(input$pathway_genes)
        genes_to_test <- input$pathway_genes
      }

      genes_to_test <- unique(trimws(genes_to_test[nchar(trimws(genes_to_test)) > 0]))

      # Step-3.6: auto-remap ENSG → Symbol before bitr
      genes_to_test <- .remap_if_ensg(
        genes_to_test, input$pathway_org,
        notify_fn = function(msg, ...) showNotification(msg, ...)
      )
      genes_to_test <- unique(genes_to_test[nchar(genes_to_test) > 0])

      if (length(genes_to_test) < 10) {
        showNotification(sprintf(.tr("⚠️ Trop peu de gènes (%d). Minimum 10."), length(genes_to_test)),
                         type="warning"); return()
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message=.tr("Enrichissement..."), value=0.3)

      tryCatch({
        res <- run_pathway_enrichment(genes=genes_to_test, organism=input$pathway_org,
                                      database=input$pathway_db, pval_cutoff=input$pathway_pval,
                                      universe = rownames(global_data$sc_obj))
        if (nrow(res)==0) {
          showNotification(.tr("ℹ️ Aucun pathway enrichi."), type="warning")
          pathway_rv(NULL); return()
        }
        pathway_rv(res)
        shared_rv$pathway_results <- res
        shared_rv$pathway_db      <- input$pathway_db
        showNotification(paste("✅", nrow(res), "pathways enrichis"), type="message")
        shared_rv$active_tab <- "tab_pathway"
      }, error=function(e) {
        showNotification(paste0("❌ Erreur pathway: ", as.character(e$message)[1]),
                         type="error", duration=5)
        pathway_rv(NULL)
      })
    })

    output$pathway_status <- renderText({
      if (is.null(pathway_rv())) .tr("Aucune analyse en cours")
      else paste("✓", nrow(pathway_rv()), .tr("pathways ["), input$pathway_db, "]")
    })

    output$pathway_barplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_barplot(pathway_rv(), db_label = input$pathway_db, top_n = 15, tr = .tr,
                           palette = shared_rv$sc_palette %||% "default",
                           manual_gradient = shared_rv$sc_manual_gradient)
    })
    output$pathway_dotplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_dotplot(pathway_rv(), db_label = input$pathway_db, top_n = 20, tr = .tr,
                           palette = shared_rv$sc_palette %||% "default",
                           manual_gradient = shared_rv$sc_manual_gradient)
    })
    output$pathway_table <- renderDT({
      req(pathway_rv()); build_pathway_dt(pathway_rv(), tr = .tr)
    })

    .dl <- function() downloadHandler(
      filename = function() paste0("pathways_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(pathway_rv()); write.csv(pathway_rv(), file, row.names=FALSE) }
    )
    output$dl_pathway        <- .dl()
    output$dl_pathway_header <- .dl()
  })
}
