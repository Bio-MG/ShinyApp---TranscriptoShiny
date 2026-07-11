# mod_sc_corr.R  —  Child 5: Gene Correlation
# Step-3.6 fix: shared_rv$corr_target_gene now written on every successful run
#   (was missing → sc_report_template corr_plot received NULL target_gene
#    → data.frame(from=NULL, to=top$gene[20]) → "0, 20" crash in PDF export)

mod_sc_corr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light",style="font-size:0.9em;border-left:3px solid #E74C3C;",
        "Trouve les genes correles avec un gene cible."),
    div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
        h6("Gene Cible",style="font-weight:bold;color:#E74C3C;"),
        selectizeInput(ns("target_gene"),NULL,choices=NULL,multiple=FALSE,
                       options=list(placeholder="Ex: CD3D, CD8A")),
        helpText("Gene de reference pour calculer les correlations")),
    h6("Parametres de Recherche",style="font-weight:bold;"),
    fluidRow(
      column(6,radioButtons(ns("cor_method"),"Methode",
                            choices=c("Pearson"="pearson","Spearman"="spearman"),selected="pearson")),
      column(6,numericInput(ns("cor_threshold"),"Seuil |r|",value=0.3,min=0,max=1,step=0.05))),
    numericInput(ns("cor_top_n"),"Nombre max de genes",value=50,min=10,max=200,step=10),
    actionButton(ns("find_correlated"),"Rechercher Genes Correles",
                 class="btn-primary w-100 mb-2",icon=icon("search")),
    hr(),
    h6("Actions Rapides",style="font-weight:bold;"),
    div(class="d-grid gap-2",
        actionButton(ns("add_correlated_to_viz"),"-> Ajouter a Visualisation",
                     class="btn-sm btn-success w-100",icon=icon("chart-line")),
        downloadButton(ns("dl_correlated"),"Export CSV",class="btn-sm btn-info w-100")),
    downloadButton(ns("dl_network_plot"),"Export Network (PNG)",class="btn-sm btn-secondary w-100 mt-1"),
    downloadButton(ns("dl_network_edges"),"Export Edge List (CSV)",class="btn-sm btn-secondary w-100 mt-1"),
    hr(),
    div(class="small text-muted",textOutput(ns("correlation_status")))
  )
}

mod_sc_corr_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen=TRUE,
    div(class="card-header bg-light",
        div(style="display:flex;justify-content:space-between;align-items:center;",
            h5("Analyse de Correlation",class="card-title mb-0"),
            actionButton(ns("quick_add_corr"),"Ajouter selection a Viz",
                         class="btn-sm btn-primary",icon=icon("chart-line")))),
    layout_columns(
      col_widths=c(12),
      card(card_header("Network Plot"),max_height="400px",
           plotOutput(ns("correlation_network"),height="350px")),
      card(card_header("Table des Correlations"),max_height="400px",
           DTOutput(ns("correlation_table"))))
  )
}

mod_sc_corr_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    corr_rv <- reactiveVal(NULL)

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj          <- global_data$sc_obj
      var_features <- VariableFeatures(obj)
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "target_gene", choices = gene_choices, server = TRUE)
    })

    observeEvent(input$find_correlated, {
      req(global_data$sc_obj, input$target_gene)
      obj <- global_data$sc_obj
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Calcul des corrélations...", value = 0)

      tryCatch({
        p$set(0.30, "Extraction données")
        cor_df <- find_correlated_genes(
          seurat_obj  = obj,
          target_gene = input$target_gene,
          method      = input$cor_method,
          threshold   = input$cor_threshold,
          top_n       = input$cor_top_n
        )
        cor_df$correlation     <- as.numeric(cor_df$correlation)
        cor_df$p_value         <- as.numeric(cor_df$p_value)
        cor_df$abs_correlation <- as.numeric(cor_df$abs_correlation)
        cor_df$p_adj           <- as.numeric(cor_df$p_adj)
        cor_df$gene            <- as.character(cor_df$gene)

        p$set(0.80, "Formatage résultats")
        if (nrow(cor_df) == 0) {
          showNotification("Aucun gène corrélé trouvé. Réduisez le seuil.", type="warning", duration=5)
          corr_rv(NULL)
          shared_rv$correlated_genes <- NULL
          shared_rv$corr_target_gene <- NULL          # ← always reset together
          return()
        }

        corr_rv(cor_df)
        shared_rv$correlated_genes <- cor_df
        shared_rv$corr_target_gene <- as.character(input$target_gene)[1]  # ← FIX: was missing
        shared_rv$active_tab       <- "tab_correlation"

        msg <- paste0("✓ Trouvé ", nrow(cor_df), " gènes corrélés avec ", as.character(input$target_gene)[1])
        showNotification(msg, type="message", duration=4)

      }, error = function(e) {
        err_msg <- paste0("Erreur corrélation: ", as.character(e$message)[1])
        showNotification(err_msg, type="error", duration=5)
        corr_rv(NULL)
        shared_rv$correlated_genes <- NULL
        shared_rv$corr_target_gene <- NULL
      })
    })

    output$correlation_network <- renderPlot({
      req(corr_rv(), input$target_gene)
      df  <- corr_rv()
      top <- head(df, 20)
      library(igraph)
      edges <- data.frame(from=input$target_gene, to=top$gene,
                          weight=top$abs_correlation, correlation=top$correlation)
      g <- graph_from_data_frame(edges, directed=FALSE)
      edge_colors <- ifelse(edges$correlation > 0, "#27AE60", "#E74C3C")
      edge_widths <- scales::rescale(edges$weight, to=c(1,5))
      layout      <- layout_with_fr(g)
      node_sizes  <- ifelse(V(g)$name == input$target_gene, 15, 8)
      node_colors <- ifelse(V(g)$name == input$target_gene, "#3498DB", "#95A5A6")
      par(mar=c(0,0,2,0))
      plot(g, layout=layout, vertex.size=node_sizes, vertex.color=node_colors,
           vertex.label.cex=0.7, vertex.label.color="black", vertex.label.family="sans",
           edge.color=edge_colors, edge.width=edge_widths,
           main=paste("Reseau de Correlation -", input$target_gene), edge.curved=0.2)
      legend("bottomleft", legend=c("Correlation positive","Correlation negative","Gene cible"),
             col=c("#27AE60","#E74C3C","#3498DB"), pch=c(15,15,19), pt.cex=c(2,2,2.5), bty="n", cex=0.9)
    })

    output$correlation_table <- renderDT({
      req(corr_rv())
      df <- corr_rv()
      df_display <- data.frame(
        Gene        = df$gene,
        Correlation = round(df$correlation, 3),
        `|r|`       = round(df$abs_correlation, 3),
        P_value     = format(df$p_value, scientific=TRUE, digits=2),
        P_adj       = format(df$p_adj, scientific=TRUE, digits=2),
        check.names = FALSE
      )
      datatable(df_display, selection=list(mode="multiple",target="row",selected=NULL),
                filter="top", rownames=FALSE,
                options=list(pageLength=15, scrollX=TRUE)) %>%
        formatStyle("Correlation",
                    background=styleColorBar(range(df_display$Correlation),"lightblue"),
                    backgroundSize="98% 88%", backgroundRepeat="no-repeat",
                    backgroundPosition="center") %>%
        formatStyle("P_adj", color=styleInterval(c(0.001,0.01,0.05),
                                                  c("darkgreen","green","orange","red")))
    })

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
        showNotification(paste("Added", length(genes), "gene(s) to viz"), type="message", duration=3)
      } else {
        showNotification("Selectionnez des lignes dans la table d'abord", type="warning")
      }
    })

    observeEvent(input$add_correlated_to_viz, {
      req(corr_rv())
      top10   <- head(corr_rv()$gene, 10)
      to_push <- unique(c(input$target_gene, top10))
      current <- shared_rv$selected_genes %||% character(0)
      shared_rv$selected_genes <- unique(c(current, to_push))
      shared_rv$active_tab     <- "tab_viz"
      showNotification(paste("Added target +", length(top10), "top correlated genes"),
                       type="message", duration=4)
    })

    output$correlation_status <- renderText({
      if (is.null(corr_rv())) "Aucune analyse en cours"
      else paste("✓", nrow(corr_rv()), "genes correles avec", input$target_gene)
    })

    output$dl_correlated <- downloadHandler(
      filename = function() paste0("correlated_", input$target_gene, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(corr_rv()); write.csv(corr_rv(), file, row.names=FALSE) }
    )

    output$dl_network_plot <- downloadHandler(
      filename = function() paste0("network_", input$target_gene, "_", Sys.Date(), ".png"),
      content  = function(file) {
        req(corr_rv(), input$target_gene)
        df    <- corr_rv(); top <- head(df, 20)
        edges <- data.frame(from=input$target_gene, to=top$gene,
                            weight=top$abs_correlation, correlation=top$correlation)
        g <- igraph::graph_from_data_frame(edges, directed=FALSE)
        png(file, width=1200, height=900, res=150)
        par(mar=c(0,0,2,0))
        plot(g, layout=igraph::layout_with_fr(g),
             vertex.size=ifelse(igraph::V(g)$name==input$target_gene,15,8),
             vertex.color=ifelse(igraph::V(g)$name==input$target_gene,"#3498DB","#95A5A6"),
             vertex.label.cex=0.7,
             edge.color=ifelse(edges$correlation>0,"#27AE60","#E74C3C"),
             edge.width=scales::rescale(edges$weight,to=c(1,5)),
             main=paste("Network -",input$target_gene), edge.curved=0.2)
        dev.off()
      }
    )

    output$dl_network_edges <- downloadHandler(
      filename = function() paste0("network_edges_", input$target_gene, "_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(corr_rv(), input$target_gene)
        df    <- corr_rv(); top <- head(df, 50)
        edges <- data.frame(source=input$target_gene, target=top$gene,
                            correlation=round(top$correlation,4),
                            abs_corr=round(top$abs_correlation,4), p_adj=top$p_adj)
        write.csv(edges, file, row.names=FALSE)
      }
    )
  })
}
