# modules/mod_import_sc.R
# Step-3.6 fixes:
#   - .ensure_10x_features(): writes features.tsv.GZ (Seurat prefers it over genes.tsv.gz)
#   - load_single_cell_data(): checks matrix.mtx exists before Read10X(); fixes add_log&& bug
#   - prepare_seurat_object(): handles SCE, list, sparse/dense matrix

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Helper: CellRanger v2 compat — creates features.tsv.gz from genes.tsv(.gz) ──────────────
.ensure_10x_features <- function(dir_path, log_fn = NULL) {
  log <- function(msg) if (!is.null(log_fn)) log_fn(msg)

  # Already fine if any features file exists
  if (any(file.exists(file.path(dir_path, c("features.tsv", "features.tsv.gz"))))) {
    return(invisible(NULL))
  }

  gene_gz  <- file.path(dir_path, "genes.tsv.gz")
  gene_tsv <- file.path(dir_path, "genes.tsv")

  # Read source genes file
  src <- if (file.exists(gene_gz)) {
    tmp <- tempfile(fileext = ".tsv")
    tryCatch({
      con <- gzfile(gene_gz, "rt"); lines <- readLines(con); close(con)
      writeLines(lines, tmp); tmp
    }, error = function(e) { log(paste("  ⚠ Décompression genes.tsv.gz:", e$message)); NULL })
  } else if (file.exists(gene_tsv)) {
    gene_tsv
  } else {
    return(invisible(NULL))   # nothing we can do
  }

  gdf <- tryCatch(
    read.table(src, sep = "\t", header = FALSE, stringsAsFactors = FALSE, quote = ""),
    error = function(e) { log(paste("  ⚠ Lecture genes.tsv:", e$message)); NULL }
  )
  if (is.null(gdf)) return(invisible(NULL))

  # Ensure 3 columns: ID, Symbol, Type
  if      (ncol(gdf) == 1) { gdf$V2 <- gdf$V1; gdf$V3 <- "Gene Expression" }
  else if (ncol(gdf) == 2) { gdf$V3 <- "Gene Expression" }

  # Write as .gz — Seurat's Read10X looks for features.tsv.gz BEFORE genes.tsv.gz
  feat_gz <- file.path(dir_path, "features.tsv.gz")
  tryCatch({
    gz_con <- gzfile(feat_gz, "wt")
    write.table(gdf, gz_con, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
    close(gz_con)
    log("  ✓ CellRanger v2: genes.tsv → features.tsv.gz créé automatiquement")
  }, error = function(e) log(paste("  ⚠ Création features.tsv.gz:", e$message)))

  invisible(feat_gz)
}

# ── UI ────────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = "Import Single-Cell",
        accordion(
          accordion_panel(
            "Option A: Dossiers Multiples (10X)",
            div(class="alert alert-info", style="font-size:0.85rem;",
                bsicons::bs_icon("info-circle"),
                " Importez plusieurs échantillons pour Harmony."),
            div(class="alert alert-light", style="font-size:0.8rem;",
                bsicons::bs_icon("lightbulb"),
                " Formats acceptés : barcodes.tsv(.gz), features.tsv(.gz) ou genes.tsv(.gz), matrix.mtx(.gz)."),
            shinyDirButton(ns("dir_select"), "📁 Ajouter un Dossier",
                           "Sélectionner dossier contenant matrix.mtx",
                           class="btn-secondary w-100", icon=icon("folder-open")),
            textInput(ns("sample_name"), "Nom de l'échantillon", placeholder="Ex: Patient1"),
            actionButton(ns("btn_add_sample"), "➕ Ajouter à la liste", class="btn-info w-100 mt-2"),
            hr(),
            h6("Échantillons ajoutés:", style="font-weight:bold;"),
            div(style="max-height:200px;overflow-y:auto;border:1px solid #ddd;padding:10px;border-radius:5px;",
                uiOutput(ns("sample_list_display"))),
            actionButton(ns("btn_clear_samples"), "🗑️ Tout Effacer",
                         class="btn-outline-danger btn-sm w-100 mt-2"),
            hr(),
            verbatimTextOutput(ns("path_display"), placeholder=TRUE),
            actionButton(ns("btn_load_dir"), "🚀 Charger Tous les Échantillons",
                         class="btn-success w-100 mt-2", icon=icon("play"))
          ),
          accordion_panel(
            "Option B: Fichiers Multiples (.rds, .h5, .h5ad)",
            div(class="alert alert-info", style="font-size:0.85rem;",
                bsicons::bs_icon("info-circle"), " Importez plusieurs fichiers pour les fusionner."),
            fileInput(ns("file_upload"), "Ajouter Fichier(s)",
                      accept=c(".rds",".h5",".h5ad",".loom"), multiple=TRUE),
            uiOutput(ns("file_list_display")),
            actionButton(ns("btn_load_file"), "🚀 Charger", class="btn-primary w-100", icon=icon("play"))
          ),
          accordion_panel(
            "Option C: Fichier Unique (Classique)",
            fileInput(ns("single_file_upload"), "Charger un seul fichier",
                      accept=c(".rds",".h5",".h5ad",".loom")),
            helpText("Pour un seul échantillon."),
            actionButton(ns("btn_load_single"), "Charger", class="btn-warning w-100")
          )
        )
      ),
      card(
        card_header("Résumé de l'objet chargé"),
        layout_columns(
          value_box(title="Cellules",    value=textOutput(ns("nb_cells")),
                    showcase=bsicons::bs_icon("people"),     theme="primary"),
          value_box(title="Gènes",       value=textOutput(ns("nb_genes")),
                    showcase=bsicons::bs_icon("diagram-3"),  theme="secondary"),
          value_box(title="Échantillons",value=textOutput(ns("nb_samples")),
                    showcase=bsicons::bs_icon("collection"), theme="info"),
          value_box(title="Statut",      value=textOutput(ns("status_obj")),
                    showcase=bsicons::bs_icon("check-circle"),theme="light")
        ),
        card_body(h5("Console de Log", class="text-muted"),
                  verbatimTextOutput(ns("console_log"), placeholder=TRUE))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      logs(paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg, "\n", logs()))
    }

    sample_list <- reactiveVal(list())
    volumes     <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "dir_select", roots = volumes, session = session)

    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      path <- parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) { dir_path(path); add_log(paste("Dossier:", path)) }
    })
    output$path_display <- renderText({
      if (is.null(dir_path())) "Aucun dossier sélectionné" else dir_path()
    })

    observeEvent(input$btn_add_sample, {
      req(dir_path(), input$sample_name)
      if (!nchar(trimws(input$sample_name))) {
        showNotification("⚠️ Nom vide.", type="warning"); return()
      }
      cs <- sample_list()
      if (input$sample_name %in% names(cs)) {
        showNotification("⚠️ Ce nom existe déjà.", type="warning"); return()
      }
      cs[[input$sample_name]] <- dir_path(); sample_list(cs)
      add_log(paste("Échantillon ajouté:", input$sample_name))
    })

    output$sample_list_display <- renderUI({
      s <- names(sample_list())
      if (!length(s)) return(tags$em("Aucun échantillon", style="color:#999;"))
      tags$ul(lapply(s, tags$li))
    })

    observeEvent(input$btn_clear_samples, { sample_list(list()); add_log("Liste effacée") })

    # ── Option A ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_dir, {
      req(sample_list())
      samples <- sample_list()
      add_log(paste("🔄 Import de", length(samples), "dossiers 10X..."))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Chargement...", value=0)
      tryCatch({
        obj_list <- list()
        for (i in seq_along(samples)) {
          sn <- names(samples)[i]; path <- samples[[i]]
          p$set(i/length(samples), detail=sn)
          add_log(paste("  📂 Lecture:", path))
          raw <- load_single_cell_data(path, add_log)
          obj <- prepare_seurat_object(raw, sn)
          obj$orig.ident <- sn; obj_list[[sn]] <- obj
          add_log(paste("    ✓", ncol(obj), "cellules"))
        }
        p$set(0.9, "Fusion...")
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list), project="MultiSample")
        global_data$sc_obj <- merged
        add_log(paste("✅", ncol(merged), "cellules,", length(unique(merged$orig.ident)), "échantillon(s)"))
        showNotification(paste("✅ Import réussi:", ncol(merged), "cellules"), type="message", duration=5)
      }, error=function(e) {
        msg <- paste("❌ Erreur:", conditionMessage(e))
        add_log(msg); showNotification(msg, type="error", duration=10)
      })
    })

    # ── Option B ────────────────────────────────────────────────────────────
    output$file_list_display <- renderUI({
      req(input$file_upload)
      files <- input$file_upload
      tags$ul(style="list-style:none;padding:0;",
        lapply(1:nrow(files), function(i) {
          tags$li(style="padding:4px;border-bottom:1px solid #eee;",
                  "📄 ", files$name[i],
                  tags$small(style="color:#666;", paste0(" (", round(files$size[i]/1024^2,1), " MB)")))
        }))
    })

    observeEvent(input$btn_load_file, {
      req(input$file_upload)
      files <- input$file_upload
      add_log(paste("🔄 Import de", nrow(files), "fichier(s)..."))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message="Chargement...", value=0)
      tryCatch({
        obj_list <- list()
        for (i in 1:nrow(files)) {
          fn <- tools::file_path_sans_ext(files$name[i])
          p$set(i/nrow(files), detail=files$name[i])
          add_log(paste("  📄", files$name[i]))
          raw <- load_single_cell_data(files$datapath[i], add_log)
          obj <- prepare_seurat_object(raw, fn)
          obj$orig.ident <- fn; obj_list[[fn]] <- obj
          add_log(paste("    ✓", ncol(obj), "cellules"))
        }
        p$set(0.9, "Fusion...")
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list), project="MultiFile")
        global_data$sc_obj <- merged
        add_log(paste("✅", ncol(merged), "cellules"))
        showNotification("✅ Import réussi!", type="message", duration=5)
      }, error=function(e) {
        msg <- paste("❌ Erreur:", conditionMessage(e))
        add_log(msg); showNotification(msg, type="error", duration=10)
      })
    })

    # ── Option C ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_single, {
      req(input$single_file_upload)
      add_log("🔄 Import fichier unique...")
      withProgress(message="Chargement...", {
        tryCatch({
          raw <- load_single_cell_data(input$single_file_upload$datapath, add_log)
          obj <- prepare_seurat_object(raw, "SingleSample")
          global_data$sc_obj <- obj
          add_log(paste("✅ Import réussi:", ncol(obj), "cellules"))
          showNotification("✅ Import réussi!", type="message")
        }, error=function(e) {
          msg <- paste("❌ Erreur:", conditionMessage(e))
          add_log(msg); showNotification(msg, type="error", duration=10)
        })
      })
    })

    # ── Outputs ──────────────────────────────────────────────────────────────
    output$nb_cells   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(ncol(global_data$sc_obj), big.mark=",") })
    output$nb_genes   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(nrow(global_data$sc_obj), big.mark=",") })
    output$nb_samples <- renderText({ if(is.null(global_data$sc_obj)) "-" else length(unique(global_data$sc_obj$orig.ident)) })
    output$status_obj <- renderText({
      if(is.null(global_data$sc_obj)) "⚪ Inactif"
      else { n <- length(unique(global_data$sc_obj$orig.ident))
             if(n>1) paste("🟢 Multi (",n,")") else "🟡 Mono" }
    })
    output$console_log <- renderText({ logs() })

    # ── load_single_cell_data ─────────────────────────────────────────────
    load_single_cell_data <- function(path, log_fn = NULL) {
      log <- function(msg) if (!is.null(log_fn)) log_fn(msg)

      # 1. Directory
      if (dir.exists(path)) {
        h5_path <- file.path(path, "filtered_feature_bc_matrix.h5")
        if (file.exists(h5_path)) return(Read10X_h5(h5_path))

        # Only call Read10X when matrix.mtx actually exists
        has_matrix <- any(file.exists(file.path(path, c("matrix.mtx", "matrix.mtx.gz"))))
        if (!has_matrix) {
          # Look for a single .rds inside the directory
          rds <- list.files(path, pattern="\\.rds$", ignore.case=TRUE, full.names=TRUE)
          if (length(rds) == 1) { log(paste("  ℹ RDS dans dossier:", basename(rds))); return(readRDS(rds)) }
          stop(paste0(
            "Dossier sans fichier matrix.mtx(.gz) ni filtered_feature_bc_matrix.h5.\n",
            "Si ce dossier contient un .rds ou .h5ad, utilisez l'Option B/C."))
        }

        # CellRanger v2 compat: create features.tsv.gz if only genes.tsv present
        .ensure_10x_features(path, log_fn)
        return(Read10X(path))
      }

      ext <- tolower(tools::file_ext(path))

      # 2. .rds
      if (ext == "rds") return(readRDS(path))

      # 3. .h5 — BPCells for large files
      if (ext == "h5") {
        if (requireNamespace("BPCells", quietly=TRUE) && file.size(path) > 1e9) {
          mat     <- BPCells::open_matrix_10x_hdf5(path)
          tmp_dir <- tempfile(pattern="bpcells_10x_")
          BPCells::write_matrix_dir(mat=mat, dir=tmp_dir)
          return(BPCells::open_matrix_dir(dir=tmp_dir))
        }
        return(Read10X_h5(path))
      }

      # 4. .h5ad — cascade of converters
      if (ext == "h5ad") {
        if (requireNamespace("BPCells", quietly=TRUE)) {
          try({ mat <- BPCells::open_matrix_anndata_hdf5(path)
                tmp <- tempfile(pattern="bpcells_h5ad_")
                BPCells::write_matrix_dir(mat=mat, dir=tmp)
                return(BPCells::open_matrix_dir(dir=tmp)) }, silent=TRUE)
        }
        if (requireNamespace("zellkonverter", quietly=TRUE)) {
          try({ sce <- zellkonverter::readH5AD(file=path, use_hdf5=TRUE, raw=TRUE)
                if (!"counts" %in% SummarizedExperiment::assayNames(sce))
                  SummarizedExperiment::assay(sce,"counts") <- SummarizedExperiment::assay(sce,SummarizedExperiment::assayNames(sce)[1])
                return(Seurat::as.Seurat(sce, counts="counts", data=NULL)) }, silent=TRUE)
        }
        if (requireNamespace("sceasy", quietly=TRUE)) {
          try({ tmp_rds <- tempfile(fileext=".rds")
                sceasy::convertFormat(path, from="anndata", to="seurat", outFile=tmp_rds)
                return(readRDS(tmp_rds)) }, silent=TRUE)
        }
        stop("Impossible de charger .h5ad. Installez BPCells, zellkonverter ou sceasy.")
      }

      # 5. .loom
      if (ext == "loom") {
        if (!requireNamespace("loomR", quietly=TRUE)) stop("Package 'loomR' requis.")
        lconn <- loomR::connect(path, mode="r"); on.exit(lconn$close())
        return(Seurat::as.Seurat(lconn))
      }

      stop("Format non supporté : ", ext)
    }

    # ── prepare_seurat_object — class detection ───────────────────────────
    prepare_seurat_object <- function(raw, sample_name = NULL) {
      proj <- sample_name %||% "scData"

      if (inherits(raw, "Seurat")) {
        if (!is.null(raw[["RNA"]]) && !inherits(raw[["RNA"]], "Assay5"))
          tryCatch({ raw[["RNA"]] <- as(raw[["RNA"]], "Assay5") }, error=function(e) NULL)
        return(raw)
      }

      if (inherits(raw, "SingleCellExperiment")) {
        mat <- tryCatch({
          cn <- SummarizedExperiment::assayNames(raw)
          SummarizedExperiment::assay(raw, if ("counts" %in% cn) "counts" else cn[1])
        }, error=function(e) NULL)
        if (!is.null(mat)) return(CreateSeuratObject(counts=mat, project=proj))
        return(Seurat::as.Seurat(raw))
      }

      # Multi-modal list (Read10X with multiple modalities)
      if (is.list(raw) && !is.data.frame(raw) && length(raw) > 0) {
        key <- if ("Gene Expression" %in% names(raw)) "Gene Expression" else names(raw)[1]
        return(CreateSeuratObject(counts=raw[[key]], project=proj))
      }

      # Sparse / dense matrix fallback
      return(CreateSeuratObject(counts=raw, project=proj))
    }

  })
}
