# =============================================================================
# mod_sc_trajectory.R  —  Child 7: Slingshot trajectory + pseudotime
# =============================================================================
# Depends on global.R:
#   calculate_pseudotime(seuratobj, reduction, rootcells)
#     -> Seurat object with $pseudotime column added
#     -> BLOCKED internally for > 100k cells (via stop())
#   plot_trajectory(seuratobj, reduction, colorby)
#     -> ggplot object
#
# Safety block:
#   Hard ceiling at MAX_CELLS = 100 000. Shows a clear UI warning instead
#   of attempting the calculation, which mirrors the guard in global.R.
#
# State contract (shared_rv):
#   WRITE : shared_rv$active_tab -> "tab_trajectory" after successful run
#
# UI split:
#   mod_sc_trajectory_ui(id)         -> sidebar accordion body
#   mod_sc_trajectory_output_ui(id)  -> main panel "Trajectory" tab
# =============================================================================

.MAX_TRAJECTORY_CELLS <- 100000L   # mirrors the guard in global.R


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_trajectory_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      class = "alert alert-light",
      style = "font-size:0.9em;border-left:3px solid #3498DB;",
      "Analyse de trajectoire et pseudotemps."
    ),

    uiOutput(ns("cell_count_badge")),   # live warning for large datasets

    selectInput(
      ns("traj_reduction"),
      "Reduction a utiliser",
      choices  = c("UMAP" = "umap", "PCA" = "pca"),
      selected = "umap"
    ),

    checkboxInput(ns("traj_auto_root"), "Detection auto racine", value = TRUE),

    conditionalPanel(
      condition = "!input.traj_auto_root",
      ns = ns,
      numericInput(ns("traj_root_cell"), "Index cellule racine",
                   value = 1, min = 1, step = 1)
    ),

    actionButton(ns("calc_trajectory"), "Calculer Trajectoire",
                 class = "btn-info w-100", icon = icon("project-diagram")),

    hr(),

    h6("Visualisation", style = "font-weight:bold;"),

    selectInput(
      ns("traj_color"),
      "Colorer par",
      choices = c("Pseudotime" = "pseudotime", "Clusters" = "seurat_clusters")
    ),

    actionButton(ns("plot_trajectory_btn"), "Actualiser Plot",
                 class = "btn-outline-primary btn-sm w-100 mt-1"),

    hr(),
    downloadButton(ns("dl_pseudotime"), "Export pseudotemps CSV",
                   class = "btn-sm btn-info w-100"),

    hr(),
    div(class = "small text-muted", textOutput(ns("trajectory_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_trajectory_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Trajectory Analysis"),
    navset_tab(
      nav_panel(
        "Plot Trajectoire",
        plotOutput(ns("trajectory_plot"), height = "550px"),
      
        #test adding fix
        downloadButton(ns("dl_trajectory_png"), "Export Plot (PNG)", class = "btn-sm btn-secondary w-100 mt-2"),
        
        hr(),
        h6("Analyses complémentaires", style = "font-weight:bold; color:#3498DB"),
        selectizeInput(ns("traj_genes_export"), "Gènes vs Pseudotemps",
                       choices = NULL, multiple = TRUE,
                       options = list(maxItems = 6, placeholder = "Ex: CD4, PTPRC")),
        fluidRow(
          column(6, plotOutput(ns("plot_pseudotime_dist"),  height = "280px")),
          column(6, plotOutput(ns("plot_genes_pseudotime"), height = "280px"))
        ),
        fluidRow(
          column(6, downloadButton(ns("dl_pseudotime_dist"),
                                   "Export Distribution", class = "btn-sm btn-info w-100")),
          column(6, downloadButton(ns("dl_genes_pseudotime"),
                                   "Export Gènes/Pseudo", class = "btn-sm btn-info w-100")),
          
          selectInput(ns("traj_export_fmt"), "Format export plots", 
                      choices = c("PNG" = "png", "PDF" = "pdf"), selected = "png", width = "100%")
        )
        
        ##
        
        ),
      nav_panel(
        "Distribution Pseudotemps",
        plotOutput(ns("pseudotime_distribution"), height = "400px"),
        #add
        downloadButton(ns("dl_dist_png"), "Export Distribution (PNG)", class = "btn-sm btn-secondary w-100 mt-2"),
        
        
      ),
      nav_panel(
        "Genes vs Pseudotemps",
        fluidRow(
          column(4,
            selectizeInput(ns("traj_genes"), "Genes a tracer",
                           choices = NULL, multiple = TRUE,
                           options = list(maxItems = 8,
                                         placeholder = "Ex: CD3D, MS4A1"))
          ),
          column(4,
            radioButtons(ns("traj_smooth"), "Lissage",
                         choices = c("LOESS" = "loess", "GAM" = "gam",
                                     "Lineaire" = "lm"),
                         inline = TRUE, selected = "loess")
          ),
          column(4,
            actionButton(ns("plot_gene_trajectory"), "Tracer",
                         class = "btn-sm btn-primary mt-4")
          )
        ),
        plotOutput(ns("gene_pseudotime_plot"), height = "450px"),
        
        
        #adding 
        downloadButton(ns("dl_genes_png"), "Export Gènes/Pseudo (PNG)", class = "btn-sm btn-secondary w-100 mt-2")
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_trajectory_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    traj_status_rv <- reactiveVal("En attente du calcul...")
    
    # ── Live cell-count badge ──────────────────────────────────────────────
    output$cell_count_badge <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      n <- ncol(obj)
      if (n > .MAX_TRAJECTORY_CELLS) {
        div(class = "alert alert-danger alert-sm", style = "font-size:0.85em;padding:6px;",
            icon("exclamation-triangle"),
            sprintf("Dataset trop grand (%s cellules > %s max). La trajectoire est désactivée.",
                    format(n, big.mark = " "), format(.MAX_TRAJECTORY_CELLS, big.mark = " ")))
      } else {
        div(class = "alert alert-success alert-sm", style = "font-size:0.85em;padding:6px;",
            icon("check-circle"),
            sprintf("%s cellules — trajectoire disponible.", format(n, big.mark = " ")))
      }
    })
    
    # ── Update choices after pipeline ──────────────────────────────────────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      valid_cols <- c("seurat_clusters", "orig.ident",
                      names(meta)[sapply(meta, function(x) is.factor(x) || is.character(x))])
      traj_choices <- c("Pseudotime" = "pseudotime", "Clusters" = "seurat_clusters")
      if ("pseudotime" %in% colnames(meta)) {
        traj_choices <- c(traj_choices, setNames(valid_cols, valid_cols))
      }
      updateSelectInput(session, "traj_color", choices = unique(traj_choices))
      
      var_genes <- VariableFeatures(obj)
      all_genes <- c(var_genes, setdiff(rownames(obj), var_genes))
      updateSelectizeInput(session, "traj_genes", choices = all_genes, server = TRUE)
      updateSelectizeInput(session, "traj_genes_export", choices = all_genes, server = TRUE) # FIX
    })
    
    # ── 1. Calculate trajectory ────────────────────────────────────────────
    observeEvent(input$calc_trajectory, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj
      if (ncol(obj) > .MAX_TRAJECTORY_CELLS) {
        showNotification("Dataset trop grand pour la trajectoire.", type = "error", duration = 6)
        traj_status_rv("BLOQUÉ: dataset trop grand.")
        return()
      }
      if (!input$traj_reduction %in% names(obj@reductions)) {
        showNotification("Réduction non trouvée. Lancez le pipeline d'abord.", type = "error", duration = 6)
        return()
      }
      
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Calcul trajectoire...", value = 0.4)
      
      tryCatch({
        root_cells <- if (input$traj_auto_root) NULL else input$traj_root_cell
        # FIX: aligned with global.R signature
        obj_updated <- calculate_pseudotime(
          seurat_obj = obj,
          reduction  = input$traj_reduction,
          root_cells = root_cells
        )
        global_data$sc_obj <- obj_updated
        shared_rv$active_tab <- "tab_trajectory"
        traj_status_rv("✓ Pseudotemps calculé avec succès")
        showNotification("✓ Trajectoire calculée", type = "message", duration = 4)
        
        meta <- obj_updated@meta.data
        valid_cols <- names(meta)[sapply(meta, function(x) is.factor(x) || is.character(x))]
        traj_choices <- c("Pseudotime" = "pseudotime", "Clusters" = "seurat_clusters",
                          setNames(valid_cols, valid_cols))
        updateSelectInput(session, "traj_color", choices = unique(traj_choices), selected = "pseudotime")
      }, error = function(e) {
        traj_status_rv(paste("Erreur:", e$message))
        showNotification(paste("Erreur trajectoire:", e$message), type = "error", duration = 8)
      })
    })
    
    # ── 2. Main trajectory plot ────────────────────────────────────────────
    output$trajectory_plot <- renderPlot({
      input$plot_trajectory_btn
      req(global_data$sc_obj)
      obj <- global_data$sc_obj
      colorby <- input$traj_color
      if (colorby == "pseudotime" && !"pseudotime" %in% colnames(obj@meta.data)) {
        return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pseudotemps non calculé.", size=6, hjust=0.5) + theme_void())
      }
      # FIX: aligned with global.R signature
      tryCatch(plot_trajectory(seurat_obj = obj, reduction = input$traj_reduction, color_by = colorby),
               error = function(e) ggplot() + annotate("text", x=0.5, y=0.5, label=paste("Erreur plot:", e$message), size=5, color="red", hjust=0.5) + theme_void())
    })
    
    # ── 3. Pseudotime distribution (main tab) ──────────────────────────────
    output$pseudotime_distribution <- renderPlot({
      req(global_data$sc_obj)
      obj <- global_data$sc_obj
      validate(need("pseudotime" %in% colnames(obj@meta.data), "Pseudotemps non calculé."))
      plot_df <- data.frame(pseudotime = obj@meta.data$pseudotime, cluster = as.character(obj@meta.data$seurat_clusters))
      ggplot(plot_df, aes(x = pseudotime, fill = cluster)) +
        geom_density(alpha = 0.6) + scale_fill_viridis_d(option = "turbo") +
        labs(title = "Distribution du Pseudotemps par Cluster", x = "Pseudotemps", y = "Densité", fill = "Cluster") +
        theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14))
    })
    
    # ── 4. Genes vs pseudotime (main tab) ──────────────────────────────────
    output$gene_pseudotime_plot <- renderPlot({
      input$plot_gene_trajectory
      req(global_data$sc_obj, input$traj_genes)
      obj <- global_data$sc_obj
      validate(need("pseudotime" %in% colnames(obj@meta.data), "Pseudotemps non calculé."))
      valid_genes <- intersect(input$traj_genes, rownames(obj))
      validate(need(length(valid_genes) > 0, "Aucun gène valide sélectionné"))
      expr_df <- FetchData(obj, vars = c("pseudotime", valid_genes))
      expr_long <- tidyr::pivot_longer(expr_df, cols = -pseudotime, names_to = "gene", values_to = "expression")
      ggplot(expr_long, aes(x = pseudotime, y = expression, color = gene)) +
        geom_point(alpha = 0.3, size = 0.8) + geom_smooth(method = input$traj_smooth, se = TRUE, linewidth = 1) +
        facet_wrap(~gene, scales = "free_y", ncol = 2) + scale_color_viridis_d(option = "turbo") +
        labs(title = "Expression génique le long du Pseudotemps", x = "Pseudotemps", y = "Expression Normalisée") +
        theme_minimal() + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 13), strip.text = element_text(face = "bold"))
    })
    
    # ── 5. Complementary Plots (FIXED: moved outside downloadHandler) ──────
    output$plot_pseudotime_dist <- renderPlot({
      req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
      df <- data.frame(pseudotime = global_data$sc_obj@meta.data$pseudotime,
                       cluster = as.character(global_data$sc_obj@meta.data$seurat_clusters))
      ggplot(df, aes(x = pseudotime, fill = cluster)) +
        geom_density(alpha = 0.6) + scale_fill_viridis_d(option = "turbo") +
        labs(title = "Distribution du Pseudotemps", x = "Pseudotemps", y = "Densité", fill = "Cluster") +
        theme_minimal() + theme(plot.title = element_text(face = "bold", size = 12))
    })
    
    output$plot_genes_pseudotime <- renderPlot({
      req(global_data$sc_obj, input$traj_genes_export, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
      valid_genes <- intersect(input$traj_genes_export, rownames(global_data$sc_obj))
      req(length(valid_genes) > 0)
      expr_df <- FetchData(global_data$sc_obj, vars = c("pseudotime", valid_genes))
      expr_long <- tidyr::pivot_longer(expr_df, cols = -pseudotime, names_to = "gene", values_to = "expression")
      ggplot(expr_long, aes(x = pseudotime, y = expression, color = gene)) +
        geom_point(alpha = 0.3, size = 0.6) + geom_smooth(method = "loess", se = TRUE, linewidth = 0.8) +
        facet_wrap(~gene, scales = "free_y", ncol = 2) + scale_color_viridis_d(option = "turbo") +
        labs(title = "Expression le long du Pseudotemps", x = "Pseudotemps", y = "Expression") +
        theme_minimal() + theme(legend.position = "none", plot.title = element_text(face = "bold", size = 12))
    })
    
    # ── 6. Status ──────────────────────────────────────────────────────────
    output$trajectory_status <- renderText({ traj_status_rv() })
    
    # ── 7. CSV Exports ─────────────────────────────────────────────────────
    output$dl_pseudotime <- downloadHandler(
      filename = function() paste0("pseudotime_", Sys.Date(), ".csv"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        out <- data.frame(cell_barcode = rownames(global_data$sc_obj@meta.data),
                          pseudotime = global_data$sc_obj@meta.data$pseudotime,
                          seurat_clusters = global_data$sc_obj@meta.data$seurat_clusters)
        write.csv(out, file, row.names = FALSE)
      }
    )
    output$dl_pseudotime_dist <- downloadHandler(
      filename = function() paste0("pseudotime_distribution_", Sys.Date(), ".csv"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        df <- data.frame(cell_barcode = rownames(global_data$sc_obj@meta.data),
                         pseudotime = global_data$sc_obj@meta.data$pseudotime,
                         seurat_clusters = global_data$sc_obj@meta.data$seurat_clusters)
        write.csv(df, file, row.names = FALSE)
      }
    )
    output$dl_genes_pseudotime <- downloadHandler(
      filename = function() paste0("genes_pseudotime_", Sys.Date(), ".csv"),
      content = function(file) {
        req(global_data$sc_obj, input$traj_genes_export, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        valid_genes <- intersect(input$traj_genes_export, rownames(global_data$sc_obj))
        req(length(valid_genes) > 0)
        expr_df <- FetchData(global_data$sc_obj, vars = c("pseudotime", valid_genes))
        expr_df$cell_barcode <- rownames(expr_df)
        write.csv(expr_df, file, row.names = FALSE)
      }
    )
    
    # ── 8. PNG Plot Exports (NEW) ──────────────────────────────────────────
    output$dl_trajectory_png <- downloadHandler(
      filename = function() paste0("trajectory_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        p <- plot_trajectory(seurat_obj = global_data$sc_obj, reduction = input$traj_reduction, color_by = input$traj_color)
        ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
      }
    )
    output$dl_dist_png <- downloadHandler(
      filename = function() paste0("pseudotime_dist_", Sys.Date(), ".png"),
      content = function(file) { ggsave(file, plot = output$plot_pseudotime_dist(), width = 7, height = 5, dpi = 300) }
    )
    output$dl_genes_png <- downloadHandler(
      filename = function() paste0("genes_pseudotime_", Sys.Date(), ".png"),
      content = function(file) { ggsave(file, plot = output$plot_genes_pseudotime(), width = 8, height = 6, dpi = 300) }
    )
  }) # /moduleServer
}