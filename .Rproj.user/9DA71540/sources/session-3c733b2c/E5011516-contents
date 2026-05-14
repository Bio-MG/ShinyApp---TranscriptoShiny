# =============================================================================
# mod_sc_pathways.R  —  Child 6: GO / KEGG / Reactome enrichment
# =============================================================================
# Depends on global.R:
#   run_pathway_enrichment(genes, organism, database, pvalcutoff)
#   -> data.frame: ID, Description, p.adjust, Count, GeneRatio
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

#if (!require("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")

# 1. Install Mouse Database (for "Souris" option)
#BiocManager::install("org.Mm.eg.db", ask = FALSE, update = FALSE)

# 2. Install Reactome Pathway Analysis (for "Reactome" option)
#BiocManager::install("ReactomePA", ask = FALSE, update = FALSE)

# Optional: If you want KEGG to work smoothly too
#BiocManager::install("KEGGREST", ask = FALSE, update = FALSE)
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
    
    # ── 3. Barplot top-15 ─────────────────────────────────────────────────
    output$pathway_barplot <- renderPlot({
      req(pathway_rv())
      df     <- pathway_rv()
      df_top <- head(df, 15)
      df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))
      
      ggplot(df_top, aes(x = Count, y = Description, fill = -log10(p.adjust))) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_viridis_c(option = "plasma", direction = -1) +
        labs(title = paste("Top 15 Pathways -", input$pathway_db),
             x = "Nombre de gènes", y = NULL, fill = "-log10(P-adj)") +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
              legend.position = "right", panel.grid.major.y = element_blank())
    })
    
    # ── 4. Dotplot ─────────────────────────────────────────────────────────
    output$pathway_dotplot <- renderPlot({
      req(pathway_rv())
      df     <- pathway_rv()
      df_top <- head(df, 20)
      df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))
      
      if ("GeneRatio" %in% colnames(df_top)) {
        df_top$GeneRatioNum <- sapply(df_top$GeneRatio, function(r) {
          parts <- strsplit(as.character(r), "/")[[1]]
          if (length(parts) == 2) as.numeric(parts[1]) / as.numeric(parts[2]) else NA
        })
      } else {
        df_top$GeneRatioNum <- df_top$Count / max(df_top$Count, na.rm = TRUE)
      }
      
      ggplot(df_top, aes(x = GeneRatioNum, y = Description, color = -log10(p.adjust), size = Count)) +
        geom_point(alpha = 0.85) +
        scale_color_viridis_c(option = "magma", direction = -1) +
        labs(title = paste("Dotplot Pathways -", input$pathway_db),
             x = "Ratio de gènes", y = NULL, color = "-log10(P-adj)", size = "Nb gènes") +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),
              legend.position = "right")
    })
    
    # ── 5. Table ───────────────────────────────────────────────────────────
    output$pathway_table <- renderDT({
      req(pathway_rv())
      df <- pathway_rv()
      cols_available <- intersect(c("ID", "Description", "p.adjust", "Count", "GeneRatio"), colnames(df))
      df_display <- df[, cols_available, drop = FALSE]
      colnames(df_display) <- c("ID", "Description", "P-adj", "Nb Gènes", "Ratio")[seq_along(cols_available)]
      
      datatable(df_display, filter = "top", rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip"), extensions = "Buttons") %>%
        formatStyle("P-adj", color = styleInterval(c(0.001, 0.01, 0.05), c("darkgreen", "green", "orange", "red")))
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