# =============================================================================
# mod_sc_pathways.R  —  Child 6: GO / KEGG / Reactome enrichment
# =============================================================================
# Depends on helpers_pathway.R (sourced by global.R, not defined there):
#   run_pathway_enrichment(genes, organism, database, pvalcutoff)
#   -> data.frame: ID, Description, p.adjust, Count, GeneRatio
#   plot_pathway_barplot(df, db_label, top_n)
#   plot_pathway_dotplot(df, db_label, top_n)
#   build_pathway_dt(df)
#
# State contract (shared_rv):
#   READ  : shared_rv$markers_data     -> data.frame (from mod_sc_markers)
#           shared_rv$correlated_genes -> data.frame (from mod_sc_corr)
#   WRITE : shared_rv$pathway_results  -> consumed downstream (optional)
#           shared_rv$active_tab       -> "tab_pathway" after successful run
#
# UI split:
#   mod_sc_pathways_ui(id)         -> sidebar accordion body
#   mod_sc_pathways_output_ui(id)  -> main panel "Pathway Enrichment" tab
#
# NOTE: required Bioconductor packages (install once, outside the app):
#   BiocManager::install(c("org.Mm.eg.db", "ReactomePA", "KEGGREST"), ask = FALSE, update = FALSE)
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    div(
      class = "alert alert-light",
      style = "font-size:0.9em;border-left:3px solid #9B59B6;",
      "Analyse d'enrichissement de voies biologiques."
    ),
    
    selectInput(
      ns("pathway_source"),
      "Source de genes",
      choices = c(
        "Marqueurs calcules"  = "markers",
        "Genes correles"      = "correlated",
        "Selection manuelle"  = "manual"
      )
    ),
    
    conditionalPanel(
      condition = "input.pathway_source == 'manual'",
      ns = ns,
      selectizeInput(
        ns("pathway_genes"),
        "Genes",
        choices = NULL,
        multiple = TRUE,
        options  = list(placeholder = "Selectionnez genes")
      )
    ),
    
    fluidRow(
      column(6,
             selectInput(ns("pathway_db"), "Base de donnees",
                         choices = c("GO Biological Process" = "GOBP",
                                     "KEGG Pathways"         = "KEGG",
                                     "Reactome"              = "Reactome"))
      ),
      column(6,
             selectInput(ns("pathway_org"), "Organisme",
                         choices = c("Humain" = "human", "Souris" = "mouse"))
      )
    ),
    
    numericInput(ns("pathway_pval"), "P-value cutoff",
                 value = 0.05, min = 0.001, max = 0.1, step = 0.01),
    
    actionButton(ns("run_pathway"), "Lancer Enrichissement",
                 class = "btn-warning w-100", icon = icon("dna")),
    
    hr(),
    
    downloadButton(ns("dl_pathway"), "Export CSV", class = "btn-sm btn-info w-100"),
    
    hr(),
    div(class = "small text-muted", textOutput(ns("pathway_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_pathways_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5("Pathway Enrichment", class = "mb-0"),
        downloadButton(ns("dl_pathway_header"), "Export", class = "btn-sm btn-info")
      )
    ),
    navset_tab(
      nav_panel(
        "Barplot Top 15",
        plotOutput(ns("pathway_barplot"), height = "500px")
      ),
      nav_panel(
        "Dotplot",
        plotOutput(ns("pathway_dotplot"), height = "500px")
      ),
      nav_panel(
        "Table",
        DTOutput(ns("pathway_table"))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_sc_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    pathway_rv <- reactiveVal(NULL)
    
    # ── Refresh manual gene picker when sc_obj changes ─────────────────────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      genes <- rownames(global_data$sc_obj)
      updateSelectizeInput(session, "pathway_genes", choices = genes, server = TRUE)
    })
    
    # ── 1. Run enrichment ──────────────────────────────────────────────────
    observeEvent(input$run_pathway, {
      req(global_data$sc_obj)
      genes_to_test <- NULL
      
      if (input$pathway_source == "markers") {
        df <- shared_rv$markers_data
        if (is.null(df) || nrow(df) == 0) {
          showNotification("⚠️ Lancez d'abord l'étape 4 (Marqueurs).", type = "warning", duration = 5)
          return()
        }
        genes_to_test <- head(df$gene, 100)
        
      } else if (input$pathway_source == "correlated") {
        df <- shared_rv$correlated_genes
        if (is.null(df) || nrow(df) == 0) {
          showNotification("⚠️ Lancez d'abord l'étape 5 (Corrélation).", type = "warning", duration = 5)
          return()
        }
        genes_to_test <- c(input$target_gene, df$gene)
        
      } else if (input$pathway_source == "manual") {
        req(input$pathway_genes)
        genes_to_test <- input$pathway_genes
      }
      
      genes_to_test <- unique(trimws(genes_to_test))
      genes_to_test <- genes_to_test[nchar(genes_to_test) > 0]
      
      if (length(genes_to_test) < 10) {
        showNotification(sprintf("⚠️ Trop peu de gènes (%d). Minimum 10 requis.", length(genes_to_test)),
                         type = "warning", duration = 5)
        return()
      }
      
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Enrichissement...", value = 0.3)
      
      tryCatch({
        res <- run_pathway_enrichment(
          genes = genes_to_test, organism = input$pathway_org,
          database = input$pathway_db, pval_cutoff = input$pathway_pval
        )
        if (nrow(res) == 0) {
          showNotification("ℹ️ Aucun pathway enrichi trouvé.", type = "warning")
          pathway_rv(NULL); return()
        }
        pathway_rv(res)
        showNotification(paste("✅", nrow(res), "pathways enrichis"), type = "message")
        nav_select(id = "main_tabs", selected = "tab_pathway")
      }, error = function(e) {
        showNotification(paste0("❌ Erreur pathway: ", as.character(e$message)[1]),
                         type = "error", duration = 5)
        pathway_rv(NULL)
      })
    })
    
    # ── 2. Status ─────────────────────────────────────────────────────────
    output$pathway_status <- renderText({
      if (is.null(pathway_rv())) "Aucune analyse en cours"
      else paste("✓", nrow(pathway_rv()), "pathways trouvés [", input$pathway_db, "]")
    })
    
    # ── 3. Barplot top-15 (helper partagé global.R) ─────────────────────────
    output$pathway_barplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_barplot(pathway_rv(), db_label = input$pathway_db, top_n = 15)
    })
    
    # ── 4. Dotplot (helper partagé global.R) ────────────────────────────────
    output$pathway_dotplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_dotplot(pathway_rv(), db_label = input$pathway_db, top_n = 20)
    })
    
    # ── 5. Table (helper partagé global.R) ──────────────────────────────────
    output$pathway_table <- renderDT({
      req(pathway_rv())
      build_pathway_dt(pathway_rv())
    })
    
    # ── 6. Downloads ───────────────────────────────────────────────────────
    .dl_handler <- function() {
      downloadHandler(
        filename = function() paste0("pathways_", input$pathway_db, "_", Sys.Date(), ".csv"),
        content  = function(file) { req(pathway_rv()); write.csv(pathway_rv(), file, row.names = FALSE) }
      )
    }
    output$dl_pathway        <- .dl_handler()
    output$dl_pathway_header <- .dl_handler()
  }) # /moduleServer
}