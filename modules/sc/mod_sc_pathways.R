# mod_sc_pathways.R  —  Child 6: GO / KEGG / Reactome enrichment
# Step-3.6: auto-remap ENSG→Symbol before enrichment when gene IDs are Ensembl
# Step-3.7: BUG1 fix — pathway_rv (+ the "Base de donnees" selector) is now
#   synced from shared_rv$pathway_results / shared_rv$pathway_db, so results
#   written by the auto-pipeline (mod_sc.R) show up here immediately instead
#   of needing a manual "Lancer Enrichissement" click (same class of bug as
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
        "Analyse d'enrichissement de voies biologiques.",
        tags$br(),
        tags$small("Les IDs Ensembl (ENSG…) sont convertis automatiquement en symboles.")),
    selectInput(ns("pathway_source"), "Source de genes",
      choices=c("Marqueurs calcules"="markers","Genes correles"="correlated","Selection manuelle"="manual")),
    conditionalPanel(condition="input.pathway_source == 'manual'", ns=ns,
      selectizeInput(ns("pathway_genes"), "Genes", choices=NULL, multiple=TRUE,
                     options=list(placeholder="Selectionnez genes"))),
    fluidRow(
      column(6, selectInput(ns("pathway_db"), "Base de donnees",
               choices=c("GO Biological Process"="GOBP","KEGG Pathways"="KEGG","Reactome"="Reactome"))),
      column(6, selectInput(ns("pathway_org"), "Organisme",
               choices=c("Humain"="human","Souris"="mouse")))
    ),
    numericInput(ns("pathway_pval"), "P-value cutoff", value=0.05, min=0.001, max=0.1, step=0.01),
    actionButton(ns("run_pathway"), "Lancer Enrichissement", class="btn-warning w-100", icon=icon("dna")),
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
      nav_panel("Barplot Top 15", plotOutput(ns("pathway_barplot"), height="500px")),
      nav_panel("Dotplot",        plotOutput(ns("pathway_dotplot"), height="500px")),
      nav_panel("Table",          DTOutput(ns("pathway_table")))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    pathway_rv <- reactiveVal(NULL)

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      updateSelectizeInput(session, "pathway_genes",
                           choices=rownames(global_data$sc_obj), server=TRUE)
    })

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
          showNotification("⚠️ Lancez d'abord l'étape 4 (Marqueurs).", type="warning"); return()
        }
        genes_to_test <- head(df$gene, 100)

      } else if (input$pathway_source == "correlated") {
        df <- shared_rv$correlated_genes
        if (is.null(df) || nrow(df)==0) {
          showNotification("⚠️ Lancez d'abord l'étape 5 (Corrélation).", type="warning"); return()
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
        showNotification(sprintf("⚠️ Trop peu de gènes (%d). Minimum 10.", length(genes_to_test)),
                         type="warning"); return()
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Enrichissement...", value=0.3)

      tryCatch({
        res <- run_pathway_enrichment(genes=genes_to_test, organism=input$pathway_org,
                                      database=input$pathway_db, pval_cutoff=input$pathway_pval)
        if (nrow(res)==0) {
          showNotification("ℹ️ Aucun pathway enrichi.", type="warning")
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
      if (is.null(pathway_rv())) "Aucune analyse en cours"
      else paste("✓", nrow(pathway_rv()), "pathways [", input$pathway_db, "]")
    })

    output$pathway_barplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_barplot(pathway_rv(), db_label=input$pathway_db, top_n=15)
    })
    output$pathway_dotplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_dotplot(pathway_rv(), db_label=input$pathway_db, top_n=20)
    })
    output$pathway_table <- renderDT({
      req(pathway_rv()); build_pathway_dt(pathway_rv())
    })

    .dl <- function() downloadHandler(
      filename = function() paste0("pathways_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(pathway_rv()); write.csv(pathway_rv(), file, row.names=FALSE) }
    )
    output$dl_pathway        <- .dl()
    output$dl_pathway_header <- .dl()
  })
}
