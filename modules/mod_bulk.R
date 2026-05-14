# modules/mod_bulk.R

mod_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        h4("Analyse d'Expression Différentielle"),
        
        uiOutput(ns("ui_bulk_controls")),
        
        actionButton(ns("run_de"), "Lancer l'Analyse DE", class = "btn-primary w-100"),
        hr(),
        p(class = "text-muted", "Utilise un modèle DESeq2/edgeR (simulation ici).")
      ),
      
      # Zone principale
      navset_card_underline(
        title = "Résultats Bulk RNA-seq",
        nav_panel("Volcano Plot", 
                  plotOutput(ns("plot_volcano"), height = "600px")
        ),
        nav_panel("Table DE", 
                  DTOutput(ns("table_de"))
        )
      )
    )
  )
}

mod_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    
    # UI dynamique pour sélection des conditions
    output$ui_bulk_controls <- renderUI({
      req(global_data$bulk_meta)
      
      meta_cols <- global_data$bulk_meta_cols
      
      tagList(
        selectInput(session$ns("condition_col"), "Colonne de Condition", 
                    choices = meta_cols),
        # On utilise une liste d'échantillons ou de niveaux de facteurs pour les groupes
        uiOutput(session$ns("group_selectors")),
        numericInput(session$ns("logfc_thresh"), "Seuil |Log2FC|", value = 1, min = 0, step = 0.1),
        numericInput(session$ns("padj_thresh"), "Seuil p-adj", value = 0.05, min = 0, max = 1, step = 0.01)
      )
    })
    
    # Sélecteurs de groupes basés sur la colonne de condition
    output$group_selectors <- renderUI({
      req(input$condition_col, global_data$bulk_meta)
      
      # Assurer que la colonne sélectionnée existe
      condition_data <- global_data$bulk_meta[[input$condition_col]]
      if (is.null(condition_data)) return(NULL)
      
      levels <- unique(as.character(condition_data))
      
      tagList(
        selectInput(session$ns("group_ref"), "Groupe de Référence", 
                    choices = levels),
        selectInput(session$ns("group_target"), "Groupe Cible", 
                    choices = levels)
      )
    })
    
    # SIMULATION de l'analyse DE (remplacer par DESeq2/edgeR si données réelles)
    de_results <- eventReactive(input$run_de, {
      req(global_data$bulk_counts, global_data$bulk_meta)
      req(input$condition_col, input$group_ref, input$group_target)
      
      # Si on avait de vrais résultats DESeq2, on les afficherait.
      # Ici, nous créons un jeu de données simulé pour le graphique.
      
      showNotification("Simulation d'Analyse DE terminée. (Pour la version complète, implémenter DESeq2/edgeR ici.)", type = "warning", duration = 5)
      
      set.seed(42) # Pour la reproductibilité
      genes <- rownames(global_data$bulk_counts)[1:1000]
      data.frame(
        Gene = genes,
        log2FoldChange = rnorm(1000, sd = 2),
        pvalue = runif(1000, min = 0, max = 1)
      ) %>%
        mutate(padj = p.adjust(pvalue, method = "BH")) %>%
        # Définir la catégorie
        mutate(
          Expression = case_when(
            padj < input$padj_thresh & abs(log2FoldChange) > input$logfc_thresh & log2FoldChange > 0 ~ "Significatif (Up)",
            padj < input$padj_thresh & abs(log2FoldChange) > input$logfc_thresh & log2FoldChange < 0 ~ "Significatif (Down)",
            TRUE ~ "Non Significatif"
          )
        )
    })
    
    # Volcan Plot
    output$plot_volcano <- renderPlot({
      res <- de_results()
      req(res)
      
      ggplot(res, aes(x = log2FoldChange, y = -log10(padj), color = Expression)) +
        geom_point(alpha = 0.8, size = 1.5) +
        scale_color_manual(values = c("Significatif (Up)" = "#E41A1C", "Significatif (Down)" = "#377EB8", "Non Significatif" = "grey")) +
        geom_hline(yintercept = -log10(input$padj_thresh), linetype = "dashed", color = "grey") +
        geom_vline(xintercept = c(-input$logfc_thresh, input$logfc_thresh), linetype = "dashed", color = "grey") +
        labs(
          title = "Volcano Plot - Expression Différentielle (Simulée)",
          x = expression("Log"[2] * " Fold Change"),
          y = expression("-Log"[10] * " Adjusted P-value")
        ) +
        theme_minimal()
    })
    
    # Table de Résultats DE
    output$table_de <- renderDT({
      res <- de_results()
      req(res)
      
      datatable(
        res,
        options = list(
          pageLength = 10,
          order = list(list(3, 'desc')) # Trier par padj
        )
      ) %>%
        formatRound(c('log2FoldChange', 'pvalue', 'padj'), digits = 4)
    })
  })
}
