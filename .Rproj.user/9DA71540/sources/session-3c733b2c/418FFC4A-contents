# =============================================================================
# mod_sc_corr.R  —  Child 5: Gene Correlation (igraph network + top genes)
# =============================================================================
# Inputs  (from parent):
#   global_data  : reactiveValues(sc_obj = NULL)
#   shared_rv    : reactiveValues()
#     WRITE : shared_rv$correlated_genes  -> consumed by mod_sc_pathways
#             shared_rv$selected_genes    -> push top-corr genes to viz basket
#             shared_rv$active_tab        -> "tab_correlation" after search
#
# Expects in global.R / helper file:
#   find_correlated_genes(seurat_obj, target_gene, method, threshold, top_n)
#   -> returns data.frame: gene, correlation, abs_correlation, p_value, p_adj
#
# UI split:
#   mod_sc_corr_ui(id)         -> sidebar accordion body
#   mod_sc_corr_output_ui(id)  -> main panel "Genes Correles" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_corr_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      class = "alert alert-light",
      style = "font-size:0.9em;border-left:3px solid #E74C3C;",
      "Trouve les genes correles avec un gene cible."
    ),

    # Target gene
    div(
      style = "background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
      h6("Gene Cible", style = "font-weight:bold;color:#E74C3C;"),
      selectizeInput(ns("target_gene"), NULL, choices = NULL, multiple = FALSE,
                     options = list(placeholder = "Ex: CD3D, CD8A")),
      helpText("Gene de reference pour calculer les correlations")
    ),

    h6("Parametres de Recherche", style = "font-weight:bold;"),
    fluidRow(
      column(6,
        radioButtons(ns("cor_method"), "Methode",
                     choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                     selected = "pearson")
      ),
      column(6,
        numericInput(ns("cor_threshold"), "Seuil |r|",
                     value = 0.3, min = 0, max = 1, step = 0.05)
      )
    ),
    numericInput(ns("cor_top_n"), "Nombre max de genes",
                 value = 50, min = 10, max = 200, step = 10),

    actionButton(ns("find_correlated"), "Rechercher Genes Correles",
                 class = "btn-primary w-100 mb-2", icon = icon("search")),
    hr(),

    h6("Actions Rapides", style = "font-weight:bold;"),
    div(
      class = "d-grid gap-2",
      actionButton(ns("add_correlated_to_viz"), "-> Ajouter a Visualisation",
                   class = "btn-sm btn-success w-100", icon = icon("chart-line")),
      downloadButton(ns("dl_correlated"), "Export CSV", class = "btn-sm btn-info w-100")
    ),
    #adding test
    downloadButton(ns("dl_network_plot"), "Export Network (PNG)", 
                     class = "btn-sm btn-secondary w-100 mt-1"),
    downloadButton(ns("dl_network_edges"), "Export Edge List (CSV)", 
                   class = "btn-sm btn-secondary w-100 mt-1"),
    
    hr(),

    div(class = "small text-muted", textOutput(ns("correlation_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_corr_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    div(
      class = "card-header bg-light",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5("Analyse de Correlation", class = "card-title mb-0"),
        actionButton(ns("quick_add_corr"), "Ajouter selection a Viz",
                     class = "btn-sm btn-primary", icon = icon("chart-line"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Network Plot"),
        max_height = "400px",
        plotOutput(ns("correlation_network"), height = "350px")
      ),
      card(
        card_header("Table des Correlations"),
        max_height = "400px",
        DTOutput(ns("correlation_table"))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_corr_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # ── Module-local reactive ─────────────────────────────────────────────────
    corr_rv <- reactiveVal(NULL)   # data.frame: gene, correlation, abs_correlation, p_value, p_adj

    # ── Refresh gene choices when sc_obj updates ──────────────────────────────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj          <- global_data$sc_obj
      var_features <- VariableFeatures(obj)
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "target_gene", choices = gene_choices, server = TRUE)
    })

    # ── 1. Find correlated genes ──────────────────────────────────────────────
    observeEvent(input$find_correlated, {
      req(global_data$sc_obj, input$target_gene)
      obj <- global_data$sc_obj
      
      p <- shiny::Progress$new()
      on.exit(p$close())
      p$set(message = "Calcul des corrélations...", value = 0)
      
      tryCatch({
        p$set(0.30, "Extraction données")
        cor_df <- find_correlated_genes(
          seurat_obj = obj,
          target_gene = input$target_gene,
          method      = input$cor_method,
          threshold   = input$cor_threshold,
          top_n       = input$cor_top_n
        )
        
        # ── PATCH 1A: Force atomic columns to prevent list-column paste() errors ──
        cor_df$correlation     <- as.numeric(cor_df$correlation)
        cor_df$p_value         <- as.numeric(cor_df$p_value)
        cor_df$abs_correlation <- as.numeric(cor_df$abs_correlation)
        cor_df$p_adj           <- as.numeric(cor_df$p_adj)
        cor_df$gene            <- as.character(cor_df$gene)
        
        p$set(0.80, "Formatage résultats")
        if (nrow(cor_df) == 0) {
          showNotification("Aucun gène corrélé trouvé. Réduisez le seuil.", type = "warning", duration = 5)
          corr_rv(NULL)
          shared_rv$correlated_genes <- NULL
          return()
        }
        
        corr_rv(cor_df)
        shared_rv$correlated_genes <- cor_df
        shared_rv$active_tab <- "tab_correlation"
        
        # ── PATCH 1B: Safe scalar notification ──
        msg <- paste0("✓ Trouvé ", nrow(cor_df), " gènes corrélés avec ", as.character(input$target_gene)[1])
        showNotification(msg, type = "message", duration = 4)
        
      }, error = function(e) {
        # ── PATCH 1C: Safe error message (handles vectorized e$message) ──
        err_msg <- paste0("Erreur corrélation: ", as.character(e$message)[1])
        showNotification(err_msg, type = "error", duration = 5)
        corr_rv(NULL)
        shared_rv$correlated_genes <- NULL
      })
    })

    # ── 2. igraph network plot ────────────────────────────────────────────────
    output$correlation_network <- renderPlot({
      req(corr_rv(), input$target_gene)
      df  <- corr_rv()
      top <- head(df, 20)

      library(igraph)
      edges <- data.frame(
        from       = input$target_gene,
        to         = top$gene,
        weight     = top$abs_correlation,
        correlation = top$correlation
      )
      g <- graph_from_data_frame(edges, directed = FALSE)

      edge_colors <- ifelse(edges$correlation > 0, "#27AE60", "#E74C3C")
      edge_widths <- scales::rescale(edges$weight, to = c(1, 5))
      layout      <- layout_with_fr(g)
      node_sizes  <- ifelse(V(g)$name == input$target_gene, 15, 8)
      node_colors <- ifelse(V(g)$name == input$target_gene, "#3498DB", "#95A5A6")

      par(mar = c(0, 0, 2, 0))
      plot(
        g,
        layout            = layout,
        vertex.size       = node_sizes,
        vertex.color      = node_colors,
        vertex.label.cex  = 0.7,
        vertex.label.color = "black",
        vertex.label.family = "sans",
        edge.color        = edge_colors,
        edge.width        = edge_widths,
        main              = paste("Reseau de Correlation -", input$target_gene),
        edge.curved       = 0.2
      )
      legend("bottomleft",
             legend = c("Correlation positive", "Correlation negative", "Gene cible"),
             col    = c("#27AE60", "#E74C3C", "#3498DB"),
             pch    = c(15, 15, 19), pt.cex = c(2, 2, 2.5),
             bty = "n", cex = 0.9)
    })

    # ── 3. Correlation table ──────────────────────────────────────────────────
    output$correlation_table <- renderDT({
      req(corr_rv())
      df <- corr_rv()
      df_display <- data.frame(
        Gene        = df$gene,
        Correlation = round(df$correlation, 3),
        `|r|`       = round(df$abs_correlation, 3),
        P_value     = format(df$p_value, scientific = TRUE, digits = 2),
        P_adj       = format(df$p_adj,   scientific = TRUE, digits = 2),
        check.names = FALSE
      )
      datatable(
        df_display,
        selection = list(mode = "multiple", target = "row", selected = NULL),
        filter    = "top",
        rownames  = FALSE,
        options   = list(
          pageLength = 15,
          scrollX    = TRUE,
          language   = list(
            info = "Lignes _START_ a _END_ sur _TOTAL_ | Selectionnez des lignes"
          )
        ),
        callback = JS("table.on('select.dt', function() {
          Shiny.setInputValue('corr-correlation_table_changed', Math.random(), {priority: 'event'});
        });")
      ) %>%
        formatStyle(
          "Correlation",
          background         = styleColorBar(range(df_display$Correlation), "lightblue"),
          backgroundSize     = "98% 88%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center"
        ) %>%
        formatStyle(
          "P_adj",
          color = styleInterval(c(0.001, 0.01, 0.05),
                                c("darkgreen", "green", "orange", "red"))
        )
    })

    # ── 4. Row selection → update shared gene basket ──────────────────────────
    observeEvent(input$correlation_table_rows_selected, {
      req(corr_rv())
      df    <- corr_rv()
      idx   <- input$correlation_table_rows_selected
      valid <- idx[idx > 0 & idx <= nrow(df)]
      if (length(valid) > 0) {
        genes   <- unique(df$gene[valid])
        current <- shared_rv$selected_genes %||% character(0)
        shared_rv$selected_genes <- unique(c(current, genes))
      }
    })

    # ── 5. Quick-add from output panel header ─────────────────────────────────
    observeEvent(input$quick_add_corr, {
      req(corr_rv(), input$correlation_table_rows_selected)
      df    <- corr_rv()
      idx   <- input$correlation_table_rows_selected
      valid <- idx[idx > 0 & idx <= nrow(df)]
      if (length(valid) > 0) {
        genes   <- unique(df$gene[valid])
        current <- shared_rv$selected_genes %||% character(0)
        shared_rv$selected_genes <- unique(c(current, genes))
        shared_rv$active_tab     <- "tab_viz"
        showNotification(paste("Added", length(genes), "gene(s) to viz"),
                         type = "message", duration = 3)
      } else {
        showNotification("Selectionnez des lignes dans la table d'abord", type = "warning")
      }
    })

    # ── 6. Sidebar "Ajouter a Visualisation": push target + top-10 ───────────
    observeEvent(input$add_correlated_to_viz, {
      req(corr_rv())
      top10   <- head(corr_rv()$gene, 10)
      to_push <- unique(c(input$target_gene, top10))
      current <- shared_rv$selected_genes %||% character(0)
      shared_rv$selected_genes <- unique(c(current, to_push))
      shared_rv$active_tab     <- "tab_viz"
      showNotification(
        paste("Added target +", length(top10), "top correlated genes to viz"),
        type = "message", duration = 4
      )
    })

    # ── 7. Status label ───────────────────────────────────────────────────────
    output$correlation_status <- renderText({
      if (is.null(corr_rv())) {
        "Aucune analyse en cours"
      } else {
        paste("✓", nrow(corr_rv()), "genes correles avec", input$target_gene)
      }
    })

    # ── 8. CSV export ─────────────────────────────────────────────────────────
    output$dl_correlated <- downloadHandler(
      filename = function() paste0("correlated_", input$target_gene, "_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(corr_rv())
        write.csv(corr_rv(), file, row.names = FALSE)
      }
    )
    # ── Export Network Plot (PNG) ──────────────────────────────────────────────
    output$dl_network_plot <- downloadHandler(
      filename = function() paste0("network_", input$target_gene, "_", Sys.Date(), ".png"),
      content = function(file) {
        req(corr_rv(), input$target_gene)
        df <- corr_rv()
        top <- head(df, 20)
        edges <- data.frame(from = input$target_gene, to = top$gene, 
                            weight = top$abs_correlation, correlation = top$correlation)
        g <- igraph::graph_from_data_frame(edges, directed = FALSE)
        
        png(file, width = 1200, height = 900, res = 150)
        par(mar = c(0, 0, 2, 0))
        plot(g, layout = igraph::layout_with_fr(g),
             vertex.size = ifelse(igraph::V(g)$name == input$target_gene, 15, 8),
             vertex.color = ifelse(igraph::V(g)$name == input$target_gene, "#3498DB", "#95A5A6"),
             vertex.label.cex = 0.7, edge.color = ifelse(edges$correlation > 0, "#27AE60", "#E74C3C"),
             edge.width = scales::rescale(edges$weight, to = c(1, 5)),
             main = paste("Network -", input$target_gene), edge.curved = 0.2)
        dev.off()
      }
    )
    
    # ── Export Edge List (CSV for Cytoscape) ───────────────────────────────────
    output$dl_network_edges <- downloadHandler(
      filename = function() paste0("network_edges_", input$target_gene, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(corr_rv(), input$target_gene)
        df <- corr_rv()
        top <- head(df, 50)
        edges <- data.frame(
          source      = input$target_gene,
          target      = top$gene,
          correlation = round(top$correlation, 4),
          abs_corr    = round(top$abs_correlation, 4),
          p_adj       = top$p_adj
        )
        write.csv(edges, file, row.names = FALSE)
      }
    )
  }) # /moduleServer
}
