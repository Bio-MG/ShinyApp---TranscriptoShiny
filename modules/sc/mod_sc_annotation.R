# =============================================================================
# mod_sc_annotation.R  —  Child 2: SingleR (safe + cluster-level fallback)
# =============================================================================
# FIX applied vs original (claude-sonnet):
#   showNotification() inside .run_singler_safe() moved to observeEvent() server —
#   that helper is NOT called inside a reactive context on the cluster-aggregate
#   path, so any direct showNotification() there would throw
#   "Operation not allowed without an active reactive context".
#
# No custom helper file needed — calls Seurat/SingleR/celldex APIs directly
# (packages attached via global.R; no project-specific wrapper function).
#
# State contract (shared_rv):
#   WRITE : shared_rv$active_tab -> "tab_viz" after successful annotation
#
# UI split:
#   mod_sc_annotation_ui(id)         -> sidebar accordion body
#   mod_sc_annotation_output_ui(id)  -> main panel "Annotation" tab
# =============================================================================

# ── Private helpers (file-scoped, not exported) ───────────────────────────────

.annot_is_big <- function(obj) ncol(obj) > 100000L

.annot_is_ondisk <- function(obj) {
  if (!"RNA" %in% names(obj@assays)) return(FALSE)
  tryCatch({
    dl <- LayerData(obj, layer = "data")
    inherits(dl, "IterableMatrix") || inherits(dl, "DelayedMatrix")
  }, error = function(e) FALSE)
}

.load_ref <- function(code) {
  switch(code,
    hpca      = celldex::HumanPrimaryCellAtlasData(),
    blueprint = celldex::BlueprintEncodeData(),
    immgen    = celldex::ImmGenData(),
    dice      = celldex::DatabaseImmuneCellExpressionData(),
    stop(paste("Unknown reference:", code))
  )
}

# Returns: list(labels = character_vector, method = "full" | "cluster_aggregate")
# NOTE: NO showNotification() here — called outside reactive context on the
#       cluster-aggregate path. Notifications are issued in the observeEvent below.
.run_singler_safe <- function(obj, refcode, labellevel, maxcells = 50000L) {

  label_col <- if (labellevel == "main") "label.main" else "label.fine"

  # ── Standard path: small or in-memory dataset ─────────────────────────
  if (!.annot_is_big(obj) && !.annot_is_ondisk(obj)) {
    ref <- .load_ref(refcode)
    sce <- as.SingleCellExperiment(obj)
    if (!"logcounts" %in% names(assays(sce))) {
      DefaultAssay(obj) <- "RNA"
      obj  <- NormalizeData(obj)
      sce  <- as.SingleCellExperiment(obj)
    }
    pred <- SingleR::SingleR(test = sce, ref = ref, labels = ref[[label_col]])
    return(list(labels = pred$labels, method = "full"))
  }

  # ── Cluster-aggregate path: large or on-disk dataset ──────────────────
  # Issues a base R warning so the caller can surface it via showNotification()
  warning("Large dataset: using cluster-level aggregation for SingleR")

  if (!"seurat_clusters" %in% colnames(obj@meta.data))
    stop("Please run clustering (Pipeline step) before annotation.")

  clusters <- unique(obj$seurat_clusters)

  # Build pseudo-bulk profiles per cluster
  cluster_profiles <- lapply(setNames(clusters, as.character(clusters)), function(clust) {
    cells <- Cells(obj)[obj$seurat_clusters == clust]
    if (length(cells) > maxcells) cells <- sample(cells, maxcells)
    tmp <- subset(obj, cells = cells)
    DefaultAssay(tmp) <- "RNA"
    tmp  <- NormalizeData(tmp)
    exprs <- GetAssayData(tmp, slot = "data")
    Matrix::rowMeans(exprs)
  })

  profile_matrix <- do.call(cbind, cluster_profiles)
  ref  <- .load_ref(refcode)
  pred <- SingleR::SingleR(test = profile_matrix, ref = ref, labels = ref[[label_col]])

  # Map cluster labels back to individual cells
  cluster_labels <- setNames(as.character(pred$labels), colnames(profile_matrix))
  cell_labels    <- cluster_labels[as.character(obj$seurat_clusters)]

  list(labels = cell_labels, method = "cluster_aggregate")
}


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(
      class = "alert alert-light",
      style = "font-size:0.9em;border-left:3px solid #18BC9C;",
      "Utilise SingleR pour annoter automatiquement les clusters."
    ),

    selectInput(
      ns("ref_singler"),
      "Reference Cellulaire",
      choices = c(
        "Human Primary Cell Atlas" = "hpca",
        "Blueprint Encode"         = "blueprint",
        "ImmGen (Mouse)"           = "immgen",
        "DICE Immune"              = "dice"
      )
    ),

    radioButtons(
      ns("label_level"),
      "Niveau de Detail",
      choices = c("Main (General)" = "main", "Fine (Specifique)" = "fine"),
      inline  = TRUE
    ),

    actionButton(ns("run_annot"), "Annoter avec SingleR",
                 class = "btn-warning w-100", icon = icon("user-tag")),

    hr(),
    div(class = "small text-muted", textOutput(ns("annot_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_annotation_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5("Annotation Automatique (SingleR)", class = "mb-0"),
        downloadButton(ns("dl_annotation"), "Export CSV", class = "btn-sm btn-info")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("UMAP — types cellulaires predits"),
        plotOutput(ns("annot_umap_plot"), height = "380px")
      ),
      card(
        card_header("Table — distribution par cluster"),
        DTOutput(ns("annot_table"))
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_annotation_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    annot_status_rv <- reactiveVal("En attente de l'annotation...")

    # ── 1. Run SingleR on button click ────────────────────────────────────
    # ignoreInit = TRUE: do NOT fire on module load (req() would catch it
    # anyway, but explicit is clearer and avoids a spurious isolate cascade).
    observeEvent(input$run_annot, ignoreInit = TRUE, {
      req(global_data$sc_obj)

      obj     <- isolate(global_data$sc_obj)
      refcode <- isolate(input$ref_singler)
      lv      <- isolate(input$label_level)

      p <- shiny::Progress$new()
      on.exit(p$close())
      p$set(message = "Annotation SingleR...", value = 0.1)

      tryCatch({
        p$set(0.35, "Chargement reference...")

        # ── FIX: catch the warning from cluster-aggregate path ──────────
        result <- withCallingHandlers(
          .run_singler_safe(obj, refcode, lv),
          warning = function(w) {
            # Surface the large-dataset notice from inside the helper as a
            # proper Shiny notification — we are safely inside a reactive context here
            if (grepl("cluster-level aggregation", conditionMessage(w))) {
              showNotification(
                "Dataset large: SingleR tourne sur les profils de clusters (pseudo-bulk).",
                type     = "warning",
                duration = 6
              )
              invokeRestart("muffleWarning")
            }
          }
        )

        p$set(0.90, "Mise a jour de l'objet...")
        col_name          <- paste0("SingleR_", refcode, "_", lv)
        obj[[col_name]]   <- result$labels
        Idents(obj)       <- result$labels

        global_data$sc_obj   <- obj      # write-back triggers siblings
        shared_rv$active_tab <- "tab_viz"

        annot_status_rv(paste0(
          "✓ Annote [", result$method, "] — ",
          length(unique(result$labels)), " types cellulaires"
        ))
        showNotification(
          paste("Annotation terminee [", result$method, "] —",
                length(unique(result$labels)), "types cellulaires"),
          type     = "message",
          duration = 5
        )

      }, error = function(e) {
        annot_status_rv(paste("Erreur:", e$message))
        showNotification(paste("Erreur annotation:", e$message),
                         type = "error", duration = 8)
      })
    })

    # ── 2. Status text ────────────────────────────────────────────────────
    output$annot_status <- renderText({ annot_status_rv() })

    # ── 3. UMAP colored by predicted cell type ───────────────────────────
    output$annot_umap_plot <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data

      # Find the most recently added SingleR column
      singler_cols <- grep("^SingleR_", colnames(meta), value = TRUE)
      validate(need(length(singler_cols) > 0,
                    "Aucune annotation trouvee. Lancez 'Annoter avec SingleR'."))

      col_use <- tail(singler_cols, 1)
      validate(need("umap" %in% names(obj@reductions),
                    "UMAP non calcule. Lancez le pipeline d'abord."))

      DimPlot(obj, reduction = "umap", group.by = col_use,
              label = TRUE, repel = TRUE, pt.size = 0.5) +
        labs(title = paste("Annotation SingleR:", col_use)) +
        theme_minimal() +
        theme(plot.title     = element_text(face = "bold", size = 13),
              legend.position = "right")
    })

    # ── 4. Distribution table ─────────────────────────────────────────────
    output$annot_table <- renderDT({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data

      singler_cols <- grep("^SingleR_", colnames(meta), value = TRUE)
      validate(need(length(singler_cols) > 0, "Aucune annotation trouvee."))

      col_use <- tail(singler_cols, 1)
      tbl <- as.data.frame(table(
        Cluster  = meta$seurat_clusters,
        CellType = meta[[col_use]]
      ))
      tbl <- tbl[tbl$Freq > 0, ]
      tbl <- tbl[order(tbl$Cluster, -tbl$Freq), ]

      datatable(
        tbl,
        rownames = FALSE,
        filter   = "top",
        options  = list(pageLength = 15, scrollX = TRUE)
      ) %>%
        formatStyle(
          "Freq",
          background         = styleColorBar(range(tbl$Freq), "#18BC9C"),
          backgroundSize     = "98% 88%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center"
        )
    })

    # ── 5. CSV export ─────────────────────────────────────────────────────
    output$dl_annotation <- downloadHandler(
      filename = function() paste0("annotation_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(global_data$sc_obj)
        obj  <- global_data$sc_obj
        meta <- obj@meta.data
        singler_cols <- grep("^SingleR_", colnames(meta), value = TRUE)
        validate(need(length(singler_cols) > 0, "Aucune annotation a exporter."))
        out <- data.frame(
          cell_barcode    = rownames(meta),
          seurat_clusters = meta$seurat_clusters,
          meta[, singler_cols, drop = FALSE],
          check.names = FALSE
        )
        write.csv(out, file, row.names = FALSE)
      }
    )

  }) # /moduleServer
}
