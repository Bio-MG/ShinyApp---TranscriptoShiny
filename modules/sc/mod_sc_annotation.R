# mod_sc_annotation.R  —  Child 2: SingleR
# Step-3.6: integrated user's working ENSG→Symbol fix (extended for mouse)
#   Standard path now always normalizes first, then auto-remaps if no gene overlap.

.annot_is_big    <- function(obj) ncol(obj) > 100000L
.annot_is_ondisk <- function(obj) {
  if (!"RNA" %in% names(obj@assays)) return(FALSE)
  tryCatch({ dl <- LayerData(obj, layer="data")
             inherits(dl,"IterableMatrix") || inherits(dl,"DelayedMatrix") },
           error=function(e) FALSE)
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

.refcode_to_organism <- function(code) if (code == "immgen") "mouse" else "human"

.run_singler_safe <- function(obj, refcode, labellevel, maxcells = 50000L) {
  label_col <- if (labellevel == "main") "label.main" else "label.fine"
  organism  <- .refcode_to_organism(refcode)

  # ── Standard path ──────────────────────────────────────────────────────────
  if (!.annot_is_big(obj) && !.annot_is_ondisk(obj)) {
    ref <- .load_ref(refcode)

    # Always normalize to ensure logcounts (user-validated fix)
    DefaultAssay(obj) <- "RNA"
    obj <- NormalizeData(obj, verbose = FALSE)
    sce <- as.SingleCellExperiment(obj)

    test_ids <- rownames(sce)
    ref_ids  <- rownames(ref)

    # Auto-remap ENSG → Symbol when no overlap with reference
    if (!any(test_ids %in% ref_ids)) {
      orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"

      if (requireNamespace("AnnotationDbi", quietly = TRUE) &&
          requireNamespace(orgdb_pkg,       quietly = TRUE)) {

        orgdb <- getExportedValue(orgdb_pkg, orgdb_pkg)
        sym <- tryCatch(
          AnnotationDbi::mapIds(orgdb, keys = test_ids, keytype = "ENSEMBL",
                                column = "SYMBOL", multiVals = "first"),
          error = function(e) NULL
        )

        if (!is.null(sym)) {
          keep <- !is.na(sym) & nchar(sym) > 0
          if (sum(keep) > 0) {
            mat          <- SummarizedExperiment::assay(sce, "logcounts")
            rownames(mat) <- sym
            mat          <- mat[!duplicated(rownames(mat)), , drop = FALSE]
            sce          <- SingleCellExperiment::SingleCellExperiment(
              assays  = list(logcounts = mat),
              colData = SummarizedExperiment::colData(sce)
            )
            warning(sprintf(
              "SingleR: %d IDs ENSEMBL remappés en Symboles (%s) avant annotation.",
              sum(keep), organism
            ))
          }
        }
      } else {
        # No AnnotationDbi / orgdb → clear actionable error
        stop(sprintf(
          paste0(
            "Aucun chevauchement entre votre objet (ex: %s) et la référence '%s' (attend des Symboles).\n",
            "Solution : installez %s via BiocManager::install('%s') pour la conversion automatique."
          ),
          head(test_ids, 1), refcode, orgdb_pkg, orgdb_pkg
        ))
      }
    }

    pred <- SingleR::SingleR(test = sce, ref = ref, labels = ref[[label_col]])
    return(list(labels = pred$labels, method = "full"))
  }

  # ── Cluster-aggregate path (large / on-disk) ───────────────────────────────
  warning("Large dataset: using cluster-level aggregation for SingleR")
  if (!"seurat_clusters" %in% colnames(obj@meta.data))
    stop("Lancez le clustering (Pipeline) avant l'annotation.")

  ref      <- .load_ref(refcode)
  clusters <- unique(obj$seurat_clusters)

  cluster_profiles <- lapply(setNames(clusters, as.character(clusters)), function(clust) {
    cells <- Cells(obj)[obj$seurat_clusters == clust]
    if (length(cells) > maxcells) cells <- sample(cells, maxcells)
    tmp   <- subset(obj, cells = cells)
    DefaultAssay(tmp) <- "RNA"
    tmp   <- NormalizeData(tmp)
    Matrix::rowMeans(GetAssayData(tmp, slot = "data"))
  })
  profile_matrix <- do.call(cbind, cluster_profiles)

  # Also try ENSG remap for cluster-aggregate path
  ref_ids <- rownames(ref)
  if (!any(rownames(profile_matrix) %in% ref_ids)) {
    orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"
    if (requireNamespace("AnnotationDbi",quietly=TRUE) && requireNamespace(orgdb_pkg,quietly=TRUE)) {
      orgdb <- getExportedValue(orgdb_pkg, orgdb_pkg)
      sym   <- tryCatch(
        AnnotationDbi::mapIds(orgdb, keys=rownames(profile_matrix),
                              keytype="ENSEMBL", column="SYMBOL", multiVals="first"),
        error=function(e) NULL)
      if (!is.null(sym)) {
        keep <- !is.na(sym) & nchar(sym) > 0
        if (sum(keep) > 0) {
          pm <- profile_matrix[keep, , drop=FALSE]
          rownames(pm) <- sym[keep]
          pm <- pm[!duplicated(rownames(pm)), , drop=FALSE]
          profile_matrix <- pm
        }
      }
    }
  }

  pred           <- SingleR::SingleR(test=profile_matrix, ref=ref, labels=ref[[label_col]])
  cluster_labels <- setNames(as.character(pred$labels), colnames(profile_matrix))
  cell_labels    <- cluster_labels[as.character(obj$seurat_clusters)]
  list(labels=cell_labels, method="cluster_aggregate")
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light",style="font-size:0.9em;border-left:3px solid #18BC9C;",
        "Utilise SingleR pour annoter automatiquement les clusters.",
        tags$br(),
        tags$small("IDs Ensembl (ENSG…) convertis automatiquement si org.Hs.eg.db disponible.")),
    selectInput(ns("ref_singler"),"Reference Cellulaire",
      choices=c("Human Primary Cell Atlas"="hpca","Blueprint Encode"="blueprint",
                "ImmGen (Mouse)"="immgen","DICE Immune"="dice")),
    radioButtons(ns("label_level"),"Niveau de Detail",
      choices=c("Main (General)"="main","Fine (Specifique)"="fine"), inline=TRUE),
    actionButton(ns("run_annot"),"Annoter avec SingleR",
                 class="btn-warning w-100",icon=icon("user-tag")),
    hr(),
    div(class="small text-muted",textOutput(ns("annot_status")))
  )
}

mod_sc_annotation_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen=TRUE,
    card_header(div(style="display:flex;justify-content:space-between;align-items:center;",
                    h5("Annotation Automatique (SingleR)",class="mb-0"),
                    downloadButton(ns("dl_annotation"),"Export CSV",class="btn-sm btn-info"))),
    layout_columns(col_widths=c(12),
      card(card_header("UMAP — types cellulaires predits"),
           plotOutput(ns("annot_umap_plot"),height="380px")),
      card(card_header("Table — distribution par cluster"),
           DTOutput(ns("annot_table"))))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_annotation_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    annot_status_rv <- reactiveVal("En attente de l'annotation...")

    observeEvent(input$run_annot, ignoreInit=TRUE, {
      req(global_data$sc_obj)
      obj     <- isolate(global_data$sc_obj)
      refcode <- isolate(input$ref_singler)
      lv      <- isolate(input$label_level)

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Annotation SingleR...", value=0.1)

      tryCatch({
        p$set(0.35, "Chargement référence + vérification IDs...")
        result <- withCallingHandlers(
          .run_singler_safe(obj, refcode, lv),
          warning = function(w) {
            msg <- conditionMessage(w)
            type <- if (grepl("cluster-level|large", msg, ignore.case=TRUE)) "warning"
                    else if (grepl("ENSEMBL|remapp", msg, ignore.case=TRUE)) "message"
                    else "warning"
            showNotification(msg, type=type, duration=7)
            invokeRestart("muffleWarning")
          }
        )

        p$set(0.90, "Mise à jour de l'objet...")
        col_name        <- paste0("SingleR_", refcode, "_", lv)
        obj[[col_name]] <- result$labels
        Idents(obj)     <- result$labels

        global_data$sc_obj   <- obj
        shared_rv$active_tab <- "tab_viz"

        annot_status_rv(paste0("✓ [", result$method, "] — ",
                               length(unique(result$labels)), " types cellulaires"))
        showNotification(paste("✓ Annotation [", result$method, "] —",
                               length(unique(result$labels)), "types"), type="message", duration=5)

      }, error=function(e) {
        annot_status_rv(paste("Erreur:", e$message))
        showNotification(paste("Erreur annotation:", e$message), type="error", duration=10)
      })
    })

    output$annot_status <- renderText({ annot_status_rv() })

    output$annot_umap_plot <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      validate(need(length(singler_cols)>0, "Aucune annotation. Lancez 'Annoter avec SingleR'."))
      col_use <- tail(singler_cols, 1)
      validate(need("umap" %in% names(obj@reductions), "UMAP non calculé."))
      DimPlot(obj, reduction="umap", group.by=col_use, label=TRUE, repel=TRUE, pt.size=0.5) +
        labs(title=paste("SingleR:", col_use)) + theme_minimal() +
        theme(plot.title=element_text(face="bold",size=13))
    })

    output$annot_table <- renderDT({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      validate(need(length(singler_cols)>0, "Aucune annotation."))
      col_use <- tail(singler_cols, 1)
      tbl <- as.data.frame(table(Cluster=meta$seurat_clusters, CellType=meta[[col_use]]))
      tbl <- tbl[tbl$Freq > 0, ]
      tbl <- tbl[order(tbl$Cluster, -tbl$Freq), ]
      datatable(tbl, rownames=FALSE, filter="top",
                options=list(pageLength=15, scrollX=TRUE)) %>%
        formatStyle("Freq", background=styleColorBar(range(tbl$Freq),"#18BC9C"),
                    backgroundSize="98% 88%", backgroundRepeat="no-repeat",
                    backgroundPosition="center")
    })

    output$dl_annotation <- downloadHandler(
      filename = function() paste0("annotation_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(global_data$sc_obj)
        meta         <- global_data$sc_obj@meta.data
        singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
        validate(need(length(singler_cols)>0, "Aucune annotation."))
        write.csv(data.frame(cell_barcode=rownames(meta),
                             seurat_clusters=meta$seurat_clusters,
                             meta[, singler_cols, drop=FALSE],
                             check.names=FALSE),
                  file, row.names=FALSE)
      }
    )
  })
}
