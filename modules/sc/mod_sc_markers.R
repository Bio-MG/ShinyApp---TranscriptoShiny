# =============================================================================
# mod_sc_markers.R  —  Child 4: FindAllMarkers + DT basket
# =============================================================================
# Inputs  (from parent):
#   global_data  : reactiveValues(sc_obj = NULL)
#   shared_rv    : reactiveValues()
#     READ  : shared_rv$max_cells_heavy -> RAM-safety cap (mod_sc_pipeline.R)
#     WRITE : shared_rv$markers_data    -> consumed by mod_sc_pathways
#             shared_rv$selected_genes  -> consumed by mod_sc_viz (gene basket)
#             shared_rv$active_tab      -> "tab_table" after markers run
#
# Step-3.7 changes:
#   - BUG1 fix: markers_rv was ONLY ever written by this module's own
#     "Trouver Marqueurs" button. The auto-pipeline (mod_sc.R) writes
#     shared_rv$markers_data directly, which this module never read back —
#     so after an auto-pipeline run, the "Table Marqueurs" tab stayed empty
#     until the user manually clicked "Trouver Marqueurs" again. Fixed with
#     an observeEvent(shared_rv$markers_data, ...) sync, mirrored in
#     mod_sc_corr.R and mod_sc_pathways.R for the same class of bug.
#   - RAM-safety: FindAllMarkers now runs on a per-cluster-capped subsample
#     (shared_rv$max_cells_heavy, set in "1. Pipeline") instead of always the
#     full object — global_data$sc_obj itself is untouched.
#
# UI split:
#   mod_sc_markers_ui(id)         -> sidebar accordion body
#   mod_sc_markers_output_ui(id)  -> main panel "Table Marqueurs" tab
# =============================================================================


# ── Private helper: normalise FindAllMarkers column names ─────────────────────
.normalize_marker_cols <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  col_map <- list(
    gene       = c("gene", "features", "Feature"),
    avg_log2FC = c("avg_log2FC", "avg_logFC", "avg_log2FoldChange", "log2FC", "logFC"),
    p_val_adj  = c("p_val_adj", "p_val", "p.value", "p.value.adj", "adj.P.Val"),
    cluster    = c("cluster", "ident", "group"),
    pct.1      = c("pct.1", "pct1", "percent_expressed_1"),
    pct.2      = c("pct.2", "pct2", "percent_expressed_2")
  )

  for (std in names(col_map)) {
    for (alt in col_map[[std]]) {
      if (alt %in% colnames(df) && !std %in% colnames(df)) {
        colnames(df)[colnames(df) == alt] <- std
        break
      }
    }
  }

  # Ensure required columns always exist
  if (!"gene"       %in% colnames(df)) df$gene       <- rownames(df)
  if (!"avg_log2FC" %in% colnames(df)) df$avg_log2FC <- 0
  if (!"p_val_adj"  %in% colnames(df)) df$p_val_adj  <- 1
  if (!"cluster"    %in% colnames(df)) df$cluster    <- "Unknown"
  if (!"pct.1"      %in% colnames(df)) df$pct.1      <- NA_real_
  if (!"pct.2"      %in% colnames(df)) df$pct.2      <- NA_real_

  df
}


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_markers_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(6,
        selectInput(ns("marker_test"), "Test",
                    choices  = c("Wilcoxon" = "wilcox", "t-test" = "t", "LRT" = "LR"),
                    selected = "wilcox")
      ),
      column(6,
        numericInput(ns("marker_min_pct"), "Min %",
                     value = 0.10, min = 0, max = 1, step = 0.05)
      )
    ),

    layout_columns(
      numericInput(ns("marker_logfc"), "Min Log2FC", value = 0.25, step = 0.05),
      selectInput(ns("sort_by"), "Trier Table",
                  choices = c("Log2FC" = "logfc", "P-value" = "pval"))
    ),

    actionButton(ns("run_markers"), "Trouver Marqueurs",
                 icon = icon("search"), class = "btn-success w-100"),

    div(
      class = "border-top pt-2 mt-2",
      actionButton(ns("add_to_viz"), "-> Ajouter selection a Viz",
                   class = "btn-success btn-sm w-100"),
      actionButton(ns("clear_all"), "Effacer selection",
                   class = "btn-outline-secondary btn-sm w-100 mt-1")
    ),

    hr(),

    # Hybrid Gene Selector
    fluidRow(
      column(12,
        div(
          class = "border rounded p-3 mb-3",
          style = "background-color:#f8f9fa;",
          h6("Hybrid Gene Selector", class = "mb-2 fw-bold"),
          textAreaInput(
            ns("bulk_gene_input"),
            label       = "Paste genes (comma, space or newline separated):",
            rows        = 3,
            placeholder = "Example: CD3D, MS4A1, CD8A, NKG7"
          ),
          actionButton(ns("add_bulk_genes"), "Add to Visualization",
                       class = "btn-primary btn-sm w-100 mb-3"),
          div(
            class = "small text-muted",
            textOutput(ns("viz_gene_count")),
            hr(class = "my-2"),
            span("Current selection in selectize box above", class = "text-info")
          )
        )
      )
    ),

    verbatimTextOutput(ns("marker_log_output"))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_markers_output_ui <- function(id) {
  ns <- NS(id)
  card(
    max_height = "750px",
    div(
      class = "card-header bg-light",
      div(
        style = "display:flex;justify-content:space-between;",
        h5("Marqueurs Differentiels", class = "card-title mb-0"),
        div(
          actionButton(ns("quick_add_markers"), "Ajouter selection",
                       class = "btn-sm btn-primary me-1", icon = icon("shopping-basket")),
          downloadButton(ns("dl_markers_excel"), "Excel", class = "btn-sm btn-success me-1"),
          downloadButton(ns("dl_markers_csv"),   "CSV",   class = "btn-sm btn-primary")
        )
      )
    ),
    div(style = "height:650px;overflow:auto;",
        DTOutput(ns("table_markers")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_markers_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # ── Module-local reactive values ──────────────────────────────────────────
    markers_rv       <- reactiveVal(NULL)   # data.frame of markers
    selected_genes_rv <- reactiveVal(character(0))
    marker_log_rv    <- reactiveVal("En attente du calcul...")

    # ── Step-3.7 BUG1 fix: keep the local table in sync with shared_rv,
    #    whichever module wrote it (this one's button, OR the auto-pipeline
    #    in mod_sc.R, OR a restored session). ignoreNULL=FALSE so an explicit
    #    reset (e.g. "0 marqueurs trouvés") also clears the table. ──────────
    observeEvent(shared_rv$markers_data, {
      markers_rv(shared_rv$markers_data)
      if (!is.null(shared_rv$markers_data))
        marker_log_rv(paste("Trouve", nrow(shared_rv$markers_data), "marqueurs"))
    }, ignoreNULL = FALSE)

    # ── Helper: push genes to the shared viz basket ───────────────────────────
    .push_to_viz <- function(genes) {
      if (length(genes) == 0) return(invisible(NULL))
      current <- shared_rv$selected_genes %||% character(0)
      shared_rv$selected_genes <- unique(c(current, genes))
      shared_rv$active_tab     <- "tab_viz"
    }

    # ── 1. Run FindAllMarkers ─────────────────────────────────────────────────
    observeEvent(input$run_markers, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj

      # group_by is owned by mod_sc_viz; read from the object metadata directly
      # (mod_sc_viz writes back to global_data$sc_obj after updating Idents)
      # Fallback: use current Idents
      grp_col <- "seurat_clusters"
      if ("seurat_clusters" %in% colnames(obj@meta.data)) grp_col <- "seurat_clusters"

      marker_log_rv("Recherche en cours...")
      p <- shiny::Progress$new()
      on.exit(p$close())
      p$set(message = "Recherche Marqueurs...", value = 0.3)

      tryCatch({
        groups <- obj@meta.data[[grp_col]]
        if (length(unique(groups)) < 2) stop("Au moins 2 groupes necessaires")
        Idents(obj) <- as.factor(groups)

        # ── RAM-safety (Step-3.7): cap cells/cluster before FindAllMarkers.
        #    Computation happens on a LOCAL subsample only — global_data$sc_obj
        #    (used everywhere else: Viz, Trajectory, export...) is untouched. ──
        cap     <- shared_rv$max_cells_heavy %||% Inf
        sub_res <- subsample_seurat_for_analysis(obj, max_per_group = cap, group_col = grp_col)
        if (sub_res$was_subsampled) {
          p$set(0.4, "Sous-échantillonnage...")
          showNotification(
            sprintf("ℹ️ Sous-échantillonnage : %s → %s cellules (max %s/cluster) pour accélérer FindAllMarkers.",
                    format(sub_res$n_before, big.mark=","), format(sub_res$n_after, big.mark=","),
                    format(cap, big.mark=",")),
            type = "message", duration = 6)
        }
        obj_use <- sub_res$object
        Idents(obj_use) <- as.factor(obj_use@meta.data[[grp_col]])

        # Step-3.8: on a disk-backed (BPCells) object, re-orient the
        # (already cell-capped) subsample to row-major storage before
        # FindAllMarkers — avoids the repeated internal transpositions
        # BPCells otherwise performs on every marker test (see
        # optimize_bpcells_for_markers() docstring, helpers_sc_bpcells.R).
        bpc <- tryCatch(optimize_bpcells_for_markers(obj_use), error = function(e) NULL)
        if (!is.null(bpc) && isTRUE(bpc$transposed)) {
          obj_use <- bpc$object
          Idents(obj_use) <- as.factor(obj_use@meta.data[[grp_col]])
          session$onSessionEnded(function() unlink(bpc$dir, recursive = TRUE))
        }

        p$set(0.6, "FindAllMarkers...")
        markers <- FindAllMarkers(
          obj_use,
          test.use      = input$marker_test,
          min.pct       = input$marker_min_pct,
          logfc.threshold = input$marker_logfc,
          only.pos      = TRUE,
          verbose       = FALSE
        )

        if (nrow(markers) == 0) {
          marker_log_rv("Aucun marqueur trouve")
          markers_rv(NULL)
          shared_rv$markers_data <- NULL
          return()
        }

        markers          <- as.data.frame(markers)
        rownames(markers) <- NULL
        markers          <- .normalize_marker_cols(markers)
        markers          <- markers[order(markers$p_val_adj, -abs(markers$avg_log2FC)), ]

        markers_rv(markers)
        shared_rv$markers_data <- markers   # expose to mod_sc_pathways
        marker_log_rv(paste("Trouve", nrow(markers), "marqueurs"))
        showNotification(paste("✓", nrow(markers), "marqueurs"), type = "message")
        shared_rv$active_tab <- "tab_table"

      }, error = function(e) {
        marker_log_rv(paste("Erreur:", e$message))
        markers_rv(NULL)
        shared_rv$markers_data <- NULL
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    })

    output$marker_log_output <- renderText({ marker_log_rv() })

    # ── 2. DataTable ──────────────────────────────────────────────────────────
    output$table_markers <- renderDT({
      req(markers_rv())
      df <- markers_rv()
      if (!"gene" %in% colnames(df)) return(datatable(data.frame(Message = "Aucun marqueur")))

      df_sorted <- if (input$sort_by == "logfc") {
        df[order(-df$avg_log2FC), ]
      } else {
        df[order(df$p_val_adj), ]
      }

      df_display <- data.frame(
        Gene    = df_sorted$gene,
        Cluster = df_sorted$cluster,
        Log2FC  = round(df_sorted$avg_log2FC, 3),
        P_adj   = format(df_sorted$p_val_adj, scientific = TRUE, digits = 3),
        Pct.1   = round(df_sorted$pct.1 * 100, 1),
        Pct.2   = round(df_sorted$pct.2 * 100, 1),
        stringsAsFactors = FALSE
      )

      datatable(
        df_display,
        selection = list(mode = "multiple", target = "row", selected = NULL),
        filter    = "top",
        rownames  = FALSE,
        options   = list(
          pageLength = 10,
          scrollX    = TRUE,
          language   = list(
            search = "Filtrer:",
            info   = "Lignes _START_ a _END_ sur _TOTAL_ | Selectionnez des lignes"
          )
        ),
        callback = JS("table.on('select.dt', function() {
          Shiny.setInputValue('markers-table_markers_changed', Math.random(), {priority: 'event'});
        });")
      ) %>%
        formatStyle(
          "Log2FC",
          backgroundColor = styleColorBar(range(df_display$Log2FC), "lightblue"),
          backgroundSize  = "98% 88%", backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        ) %>%
        formatStyle(
          "P_adj",
          color = styleInterval(c(0.001, 0.01, 0.05),
                                c("darkgreen", "green", "orange", "red"))
        )
    })

    # ── 3. Row selection -> update local selected_genes_rv ────────────────────
    observeEvent(input$table_markers_rows_selected, {
      req(markers_rv())
      df <- markers_rv()
      df_sorted <- if (input$sort_by == "logfc") df[order(-df$avg_log2FC), ] else df[order(df$p_val_adj), ]
      idx   <- input$table_markers_rows_selected
      valid <- idx[idx > 0 & idx <= nrow(df_sorted)]
      if (length(valid) > 0) {
        genes <- unique(df_sorted$gene[valid])
        selected_genes_rv(genes)
        # Mirror into bulk textarea for convenience
        updateTextAreaInput(session, "bulk_gene_input", value = paste(genes, collapse = ", "))
      }
    })

    # ── 4. Quick-add from table header button ─────────────────────────────────
    observeEvent(input$quick_add_markers, {
      req(markers_rv(), input$table_markers_rows_selected)
      df  <- markers_rv()
      idx <- input$table_markers_rows_selected
      valid <- idx[idx > 0 & idx <= nrow(df)]
      if (length(valid) > 0) {
        .push_to_viz(unique(df$gene[valid]))
      } else {
        showNotification("Selectionnez des lignes dans le tableau d'abord", type = "warning")
      }
    })

    # ── 5. Sidebar "Ajouter selection a Viz" ─────────────────────────────────
    observeEvent(input$add_to_viz, {
      req(selected_genes_rv())
      genes <- selected_genes_rv()
      if (length(genes) > 0) .push_to_viz(genes)
    })

    # ── 6. Bulk gene paste ────────────────────────────────────────────────────
    observeEvent(input$add_bulk_genes, {
      req(input$bulk_gene_input, global_data$sc_obj)
      raw   <- trimws(unlist(strsplit(input$bulk_gene_input, "[, \n\t]+")))
      clean <- raw[nchar(raw) > 0]
      valid <- intersect(clean, rownames(global_data$sc_obj))
      if (length(valid) > 0) {
        .push_to_viz(valid)
      } else {
        showNotification("Aucun gene valide trouve.", type = "error", duration = 4)
      }
    })

    # ── 7. Clear selection ────────────────────────────────────────────────────
    observeEvent(input$clear_all, {
      selected_genes_rv(character(0))
      updateTextAreaInput(session, "bulk_gene_input", value = "")
      # Also clear the viz basket
      shared_rv$selected_genes <- character(0)
    })

    # ── 8. Gene count label ───────────────────────────────────────────────────
    output$viz_gene_count <- renderText({
      paste("Currently selected:", length(shared_rv$selected_genes %||% character(0)), "gene(s)")
    })

    # ── 9. Downloads ──────────────────────────────────────────────────────────
    output$dl_markers_csv <- downloadHandler(
      filename = function() paste0("markers_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(markers_rv())
        write.csv(markers_rv(), file, row.names = FALSE)
      }
    )

    output$dl_markers_excel <- downloadHandler(
      filename = function() paste0("markers_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(markers_rv())
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(markers_rv(), file)
        } else {
          write.csv(markers_rv(), file, row.names = FALSE)
        }
      }
    )

  }) # /moduleServer
}
