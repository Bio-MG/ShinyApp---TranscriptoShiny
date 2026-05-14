# =============================================================================
# mod_sc_viz.R  —  Child 3: Visualisation (11 types) + Export
# =============================================================================
# Inputs  (from parent):
#   global_data  : reactiveValues(sc_obj = NULL)
#   shared_rv    : reactiveValues()
#     READ  : shared_rv$selected_genes  — gene basket pushed by markers/corr
#     WRITE : shared_rv$selected_genes  — updated after add/clear
#             shared_rv$active_tab      — set to "tab_viz" after gene add
#
# UI split:
#   mod_sc_viz_ui(id)         -> sidebar accordion body (controls)
#   mod_sc_viz_output_ui(id)  -> main panel tab body   (plot + export popover)
#
# Helper functions expected in global.R / sourced helper file:
#   plot_enhanced_scatter()      (scatter type)
#   plot_violin_enhanced()       (single violin)
#   plot_correlation_matrix()    (correlation_matrix type)
#   plot_multi_sample()          (multi_sample type)
#   plot_trajectory()            (used by mod_sc_trajectory, not here)
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(

    selectInput(
      ns("viz_type"), "Style de Visualisation",
      choices = c(
        "Reduction Dimensionnelle (UMAP/PCA/t-SNE)" = "dim",
        "Feature Plot (Expression sur Reduction)"   = "feature",
        "Correlation Genes (Scatter Ameliore)"      = "scatter",
        "Distribution (Violin)"                     = "violin",
        "Stacked Violin Plot"                        = "stacked_violin",
        "Densite (Ridge Plot)"                       = "ridge",
        "DotPlot"                                   = "dot",
        "Heatmap"                                   = "heatmap",
        "Matrice Correlation"                        = "correlation_matrix",
        "Comparaison Multi-Echantillons"             = "multi_sample",
        "Volcano Plot"                               = "volcano"
      )
    ),

    # Conditional: DimPlot reduction picker
    conditionalPanel(
      condition = "input.viz_type == 'dim'", ns = ns,
      selectInput(
        ns("viz_reduction"), "Reduction a Visualiser",
        choices  = c("UMAP" = "umap", "PCA" = "pca", "t-SNE" = "tsne", "Diffusion Map" = "dm"),
        selected = "umap"
      )
    ),

    # Conditional: Scatter gene X / Y
    conditionalPanel(
      condition = "input.viz_type == 'scatter'", ns = ns,
      div(
        style = "border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
        h6("Gene / Feature X", style = "font-weight:bold;color:#2C3E50;"),
        selectizeInput(ns("scatter_gene1"), NULL, choices = NULL, multiple = FALSE,
                       options = list(placeholder = "Ex: CD4"))
      ),
      div(
        style = "border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
        h6("Gene / Feature Y", style = "font-weight:bold;color:#2C3E50;"),
        selectizeInput(ns("scatter_gene2"), NULL, choices = NULL, multiple = FALSE,
                       options = list(placeholder = "Ex: CD8A"))
      ),
      div(
        style = "background:#f8f9fa;padding:10px;border-radius:5px;",
        radioButtons(ns("scatter_cor_method"), "Methode",
                     choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                     selected = "pearson", inline = TRUE),
        checkboxInput(ns("scatter_smooth"), "Ligne de tendance", value = TRUE)
      )
    ),

    # Conditional: Multi-sample
    conditionalPanel(
      condition = "input.viz_type == 'multi_sample'", ns = ns,
      selectizeInput(ns("multi_gene"), "Gene a Comparer", choices = NULL, multiple = FALSE),
      radioButtons(ns("multi_plot_type"), "Type",
                   choices = c("Violin" = "violin", "Box" = "box", "Jitter" = "jitter"),
                   inline = TRUE)
    ),

    # Group-by
    div(
      style = "display:flex;align-items:center;justify-content:space-between;",
      tags$label("Grouper/Colorer par:", class = "control-label"),
      tooltip(bsicons::bs_icon("info-circle"), "Variable de metadonnees pour groupement")
    ),
    selectInput(ns("group_by"), NULL, choices = NULL),

    # Gene basket (hidden for scatter / multi_sample / volcano)
    conditionalPanel(
      condition = "input.viz_type != 'scatter' && input.viz_type != 'multi_sample' && input.viz_type != 'volcano'",
      ns = ns,
      div(
        class = "border-bottom pb-2 mb-2",
        selectizeInput(
          ns("feat_sel"), "Genes a Visualiser",
          choices  = NULL,
          multiple = TRUE,
          options  = list(maxOptions = 5000, placeholder = "Ex: CD4, PTPRC", maxItems = 10)
        ),
        div(class = "small text-muted mb-2", textOutput(ns("gene_selection_status"))),
        actionButton(ns("clear_viz_genes"), "Vider", class = "btn-outline-danger btn-sm w-100")
      )
    ),

    # Violin: boxplot overlay
    conditionalPanel(
      condition = "input.viz_type == 'violin'", ns = ns,
      checkboxInput(ns("violin_boxplot"), "Superposer Boxplot", value = FALSE)
    ),

    # Volcano settings
    conditionalPanel(
      condition = "input.viz_type == 'volcano'", ns = ns,
      div(
        style = "background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
        h6("Groupes de Comparaison", style = "font-weight:bold;"),
        selectInput(ns("volcano_group1"), "Group 1 (Test)",      choices = NULL),
        selectInput(ns("volcano_group2"), "Group 2 (Reference)", choices = c("All other cells" = "rest")),
        helpText("Compare Group 1 vs Group 2 (ou 'rest' pour vs tous les autres)")
      ),
      fluidRow(
        column(6, numericInput(ns("volcano_logfc"), "Log2FC Threshold", value = 0.25, step = 0.05)),
        column(6, numericInput(ns("volcano_pval"),  "P-adj Threshold",   value = 0.05, step = 0.001))
      ),
      actionButton(ns("volcano_add_sig"), "-> Add Significant Genes to Viz",
                   class = "btn-sm btn-success w-100")
    ),
    checkboxInput(ns("volcano_show_labels"), "Afficher labels gènes sig.", value = TRUE),#adding test
    sliderInput(ns("pt_size"), "Taille points", 0.1, 3, 0.5, 0.1)
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_viz_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "750px",
    div(
      class = "card-header bg-light",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5("Visualisation Interactive", class = "card-title mb-0"),
        popover(
          trigger = actionButton(ns("plot_settings"), "Personnaliser",
                                 class = "btn-sm btn-outline-primary"),
          title = "Export",
          selectInput(ns("plot_theme"), "Theme",
                      choices = c("Minimal" = "minimal", "Classique" = "classic",
                                  "BW" = "bw",           "Vide"      = "void")),
          numericInput(ns("plot_width"),  "Largeur", 800, min = 400, max = 2000),
          numericInput(ns("plot_height"), "Hauteur", 600, min = 300, max = 1500),
          selectInput(ns("export_format"), "Format",
                      choices = c("PNG" = "png", "PDF" = "pdf")),
          downloadButton(ns("export_plot"), "Exporter")
        )
      )
    ),
    div(style = "height:650px;overflow:auto;",
        uiOutput(ns("plot_container")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive: ggplot object for export ────────────────────────────────────
    current_plot <- reactiveVal(NULL)

    # ── Helper: ggplot theme ──────────────────────────────────────────────────
    get_custom_theme <- function() {
      switch(
        input$plot_theme %||% "minimal",
        classic = theme_classic(),
        minimal = theme_minimal(),
        bw      = theme_bw(),
        void    = theme_void(),
        theme_minimal()
      )
    }

    # ── Helper: add genes to basket ───────────────────────────────────────────
    add_genes_to_viz <- function(genes_input) {
      req(global_data$sc_obj)
      clean <- trimws(unlist(strsplit(genes_input, "[, \n\t]+")))
      clean <- clean[nchar(clean) > 0]
      if (length(clean) == 0) {
        showNotification("No valid genes provided", type = "warning", duration = 3)
        return(invisible(NULL))
      }
      valid   <- intersect(clean, rownames(global_data$sc_obj))
      invalid <- setdiff(clean, valid)
      current <- isolate(input$feat_sel) %||% character(0)
      new_sel <- unique(c(current, valid))

      updateSelectizeInput(session, "feat_sel",
                           selected = new_sel,
                           choices  = rownames(global_data$sc_obj),
                           server   = TRUE)
      shared_rv$selected_genes <- new_sel

      if (length(valid) > 0) {
        showNotification(
          sprintf("Added %d gene(s) (%d invalid skipped)", length(valid), length(invalid)),
          type = "message", duration = 5
        )
        shared_rv$active_tab <- "tab_viz"
      } else {
        showNotification(
          paste("No valid genes. Invalid:", paste(invalid, collapse = ", ")),
          type = "error", duration = 5
        )
      }
      invisible(valid)
    }

    # ── Sync gene basket from sibling modules ─────────────────────────────────
    observeEvent(shared_rv$selected_genes, {
      req(global_data$sc_obj)
      genes <- shared_rv$selected_genes
      if (length(genes) > 0) {
        updateSelectizeInput(session, "feat_sel",
                             selected = genes,
                             choices  = rownames(global_data$sc_obj),
                             server   = TRUE)
      }
    }, ignoreInit = TRUE)

    # ── Refresh UI choices on new sc_obj ─────────────────────────────────────
    # ── Helper: safe named vector (Guards against length mismatch) ────────────
    .safe_setnames <- function(values, labels) {
      labels <- as.character(labels)
      values <- as.character(values)
      n      <- length(values)
      if (length(labels) != n) labels <- labels[seq_len(n)]
      setNames(values, labels)
    }
    
    # ── Refresh UI choices on new sc_obj (PATCHED) ───────────────────────────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      
      # ── 1. Group-by choices 
      valid_cols <- names(meta)[vapply(meta, function(x) is.factor(x) || is.character(x), logical(1))]
      valid_cols <- unique(c("seurat_clusters", "orig.ident", valid_cols))
      valid_cols <- valid_cols[valid_cols %in% names(meta)] 
      
      cur_grp <- isolate(input$group_by)
      sel_grp <- if (!is.null(cur_grp) && cur_grp %in% valid_cols) cur_grp else "seurat_clusters"
      updateSelectInput(session, "group_by", choices = valid_cols, selected = sel_grp)
      
      # ── 2. Gene pickers 
      var_features <- tryCatch(VariableFeatures(obj), error = function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      
      updateSelectizeInput(session, "feat_sel",      choices = gene_choices,  server = TRUE)
      updateSelectizeInput(session, "scatter_gene1", choices = rownames(obj), server = TRUE)
      updateSelectizeInput(session, "scatter_gene2", choices = rownames(obj), server = TRUE)
      updateSelectizeInput(session, "multi_gene",    choices = gene_choices,  server = TRUE)
      
      # ── 3. Available reductions 
      avail_red <- names(obj@reductions)
      pref_red  <- c("umap", "umap_harmony", "pca", "tsne", "dm")
      red_choices <- avail_red[avail_red %in% pref_red]
      if (length(red_choices) == 0) red_choices <- avail_red
      
      updateSelectInput(session, "viz_reduction",
                        choices  = red_choices,
                        selected = if ("umap" %in% red_choices) "umap" else red_choices[1])
      
      # ── 4. Volcano group pickers (safe setNames) 
      grp_col <- isolate(input$group_by) %||% "seurat_clusters"
      if (grp_col %in% names(meta)) {
        idents_levels <- tryCatch({
          lvls <- levels(factor(as.character(meta[[grp_col]])))
          lvls[nchar(lvls) > 0]
        }, error = function(e) character(0))
        
        if (length(idents_levels) >= 2) {
          updateSelectInput(session, "volcano_group1",
                            choices  = idents_levels,
                            selected = idents_levels[1])
          
          volcano2_choices <- c(
            "All other cells" = "rest",
            .safe_setnames(idents_levels, paste("Group:", idents_levels))
          )
          updateSelectInput(session, "volcano_group2",
                            choices  = volcano2_choices,
                            selected = "rest")
        }
      }
    }, ignoreInit = TRUE)

    # ── Plot container router ─────────────────────────────────────────────────
    output$plot_container <- renderUI({
      if (isTRUE(input$viz_type %in% c("heatmap", "stacked_violin"))) {
        plotOutput(ns("plot_static"),      height = "600px")
      } else {
        plotlyOutput(ns("plot_interactive"), height = "600px")
      }
    })

    # ── Static renders (heatmap / stacked_violin) ─────────────────────────────
    output$plot_static <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      type <- input$viz_type

      p <- tryCatch({
        if (type == "heatmap") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          DoHeatmap(obj, features = head(valid, 30), group.by = input$group_by) +
            get_custom_theme()

        } else if (type == "stacked_violin") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          plots <- lapply(head(valid, 8), function(g) {
            VlnPlot(obj, features = g, group.by = input$group_by, pt.size = 0) +
              theme(legend.position = "none",
                    axis.title.x    = element_blank(),
                    axis.text.x     = element_blank()) +
              ggtitle(g)
          })
          wrap_plots(plots, ncol = 1) + get_custom_theme()
        }
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 1, y = 1, label = paste("Erreur:", e$message)) +
          theme_void()
      })
      current_plot(p)
      p
    })

    # ── Interactive renders (all other types) ─────────────────────────────────
    output$plot_interactive <- renderPlotly({
      req(global_data$sc_obj, input$group_by)
      obj  <- global_data$sc_obj
      type <- input$viz_type

      if (type %in% c("heatmap", "stacked_violin")) return(plotly_empty())

      p <- tryCatch({

        # 1. DimPlot
        if (type == "dim") {
          red <- input$viz_reduction %||% "umap"
          validate(need(red %in% names(obj@reductions), "Reduction non calculee"))
          DimPlot(obj, reduction = red, group.by = input$group_by,
                  label = TRUE, pt.size = input$pt_size) +
            get_custom_theme() +
            ggtitle(paste(toupper(red), "-", input$group_by))

        # 2. FeaturePlot
        } else if (type == "feature") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          FeaturePlot(obj, features = head(valid, 4), ncol = 2,
                      pt.size = input$pt_size) + get_custom_theme()

        # 3. Scatter (enhanced)
        } else if (type == "scatter") {
          req(input$scatter_gene1, input$scatter_gene2)
          validate(need(input$scatter_gene1 != input$scatter_gene2, "Genes differents requis"))
          plot_enhanced_scatter(
            obj, input$scatter_gene1, input$scatter_gene2,
            group.by   = input$group_by,
            method     = input$scatter_cor_method,
            add_smooth = input$scatter_smooth,
            pt.size    = input$pt_size
          ) + get_custom_theme()

        # 4. Violin
        } else if (type == "violin") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          if (length(valid) == 1) {
            plot_violin_enhanced(obj, valid[1], input$group_by,
                                 add_boxplot = input$violin_boxplot) + get_custom_theme()
          } else {
            plots <- lapply(head(valid, 4), function(g) {
              VlnPlot(obj, features = g, group.by = input$group_by, pt.size = 0) +
                get_custom_theme() + ggtitle(g)
            })
            wrap_plots(plots, ncol = 2)
          }

        # 5. Ridge
        } else if (type == "ridge") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          RidgePlot(obj, features = head(valid, 6), ncol = 2) + get_custom_theme()

        # 6. DotPlot
        } else if (type == "dot") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) > 0, "Aucun gene valide"))
          DotPlot(obj, features = head(valid, 20), group.by = input$group_by) +
            get_custom_theme() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # 7. Correlation matrix
        } else if (type == "correlation_matrix") {
          req(input$feat_sel)
          valid <- intersect(input$feat_sel, rownames(obj))
          validate(need(length(valid) >= 2, "Au moins 2 genes requis"))
          plot_correlation_matrix(obj, head(valid, 30), method = "pearson") +
            get_custom_theme()

        # 8. Multi-sample
        } else if (type == "multi_sample") {
          req(input$multi_gene)
          validate(need(length(unique(obj$orig.ident)) >= 2, ">= 2 echantillons requis"))
          plot_multi_sample(obj, input$multi_gene, input$multi_plot_type) +
            get_custom_theme()

          # 9. Volcano (Native plot_ly)
        } else if (type == "volcano") {
          req(input$volcano_group1, input$group_by)
          grp <- input$group_by
          req(grp %in% colnames(obj@meta.data))
          
          Idents(obj) <- factor(as.character(obj@meta.data[[grp]]))
          valid_idents <- levels(Idents(obj))
          if (!input$volcano_group1 %in% valid_idents) {
            return(plotly_empty(type = "scatter", mode = "markers") |>
                     layout(title = "Groupe 1 invalide — relancez le pipeline"))
          }
          
          ident2 <- if (input$volcano_group2 == "rest") NULL else input$volcano_group2
          pt_sz  <- as.numeric(input$pt_size) * 5
          
          markers <- tryCatch(
            FindMarkers(obj, ident.1 = input$volcano_group1, ident.2 = ident2,
                        only.pos = FALSE, min.pct = 0.1, logfc.threshold = 0, verbose = FALSE),
            error = function(e) { showNotification(as.character(e$message)[1], type = "error"); NULL }
          )
          req(!is.null(markers), nrow(markers) > 0)
          
          markers$gene <- rownames(markers)
          lfc  <- input$volcano_logfc
          pval <- input$volcano_pval
          markers$status <- dplyr::case_when(
            markers$avg_log2FC >  lfc & markers$p_val_adj < pval ~ "Up",
            markers$avg_log2FC < -lfc & markers$p_val_adj < pval ~ "Down",
            TRUE ~ "NS"
          )
          color_map <- c("Up" = "#E74C3C", "Down" = "#2980B9", "NS" = "#BDC3C7")
          
          show_lbl <- isTRUE(input$volcano_show_labels)
          sig_up   <- head(markers[markers$status == "Up", ][order(-markers[markers$status == "Up", ]$avg_log2FC), ], 10)
          sig_down <- head(markers[markers$status == "Down", ][order(markers[markers$status == "Down", ]$avg_log2FC), ], 10)
          lbl_genes <- c(sig_up$gene, sig_down$gene)
          markers$label <- ifelse(show_lbl & markers$gene %in% lbl_genes, markers$gene, "")
          
          # Return native plotly object directly (bypasses ggplotly wrapper)
          return(
            plot_ly(data = markers, x = ~avg_log2FC, y = ~-log10(p_val_adj + 1e-300),
                    type = "scatter", mode = "markers+text",
                    marker = list(color = ~color_map[status], size = pt_sz, opacity = 0.75,
                                  line = list(width = 0)),
                    text = ~label, textposition = "top center",
                    hovertext = ~paste0("<b>", gene, "</b><br>",
                                        "Log2FC: ", round(avg_log2FC, 3), "<br>",
                                        "-log10(padj): ", round(-log10(p_val_adj + 1e-300), 2), "<br>",
                                        "Status: ", status),
                    hoverinfo = "text") |>
              layout(
                title  = paste0("Volcano: ", input$volcano_group1, " vs ",
                                if(is.null(ident2)) "rest" else ident2),
                xaxis  = list(title = "Log2 Fold Change", zeroline = TRUE),
                yaxis  = list(title = "-log10(P.adj)"),
                shapes = list(
                  list(type = "line", x0 =  lfc, x1 =  lfc, y0 = 0, y1 = 1, yref = "paper",
                       line = list(dash = "dot", color = "#E74C3C", width = 1)),
                  list(type = "line", x0 = -lfc, x1 = -lfc, y0 = 0, y1 = 1, yref = "paper",
                       line = list(dash = "dot", color = "#2980B9", width = 1)),
                  list(type = "line", x0 = -15, x1 = 15, y0 = -log10(pval), y1 = -log10(pval),
                       line = list(dash = "dot", color = "#7F8C8D", width = 1))
                ),
                showlegend = FALSE)
          )
        }

      }, error = function(e) {
        ggplot() +
          annotate("text", x = 1, y = 1, label = paste("Erreur:", e$message)) +
          theme_void()
      })

      current_plot(p)
      tryCatch(suppressWarnings(ggplotly(p, tooltip = "text")),
               error = function(e) plotly_empty())
    })

    # ── Export ────────────────────────────────────────────────────────────────
    output$export_plot <- downloadHandler(
      filename = function() {
        paste0("plot_", Sys.Date(), ".", input$export_format %||% "png")
      },
      content  = function(file) {
        req(current_plot())
        ggsave(
          file,
          plot   = current_plot(),
          width  = (input$plot_width  %||% 800)  / 100,
          height = (input$plot_height %||% 600)  / 100,
          dpi    = 300
        )
      }
    )
    # ── PATCH : Fix "Vider" (clear genes) button ──
    observeEvent(input$clear_viz_genes, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj
      var_features <- tryCatch(VariableFeatures(obj), error = function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      
      # Explicitly pass choices + selected = character(0) for server-side selectize
      updateSelectizeInput(
        session, "feat_sel",
        choices  = gene_choices,
        selected = character(0),
        server   = TRUE
      )
    })
  }) # /moduleServer
}
