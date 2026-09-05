# modules/mod_import_sc.R
# Step-3.6 fixes:
#   - .ensure_10x_features(): writes features.tsv.GZ (Seurat prefers it over genes.tsv.gz)
#   - load_single_cell_data(): checks matrix.mtx exists before Read10X(); fixes add_log&& bug
#   - prepare_seurat_object(): handles SCE, list, sparse/dense matrix
# Step-3.8 fix:
#   - .verify_upload_integrity(): a large upload (e.g. a multi-GB .h5) whose
#     temp-file write fails partway through (typically: the drive R's
#     tempdir() lives on runs out of space mid-upload — observed with
#     TMPDIR defaulting to a nearly-full C: drive even though the app/libs
#     live on D:) used to surface as a cryptic low-level HDF5 C++ stack
#     trace ("H5Fopen(): unable to open file... Iteration failed...") with
#     no indication of the real cause. Browser-reported size (input$file$size)
#     vs actual bytes written to datapath is a cheap, reliable signal for
#     exactly this failure mode -- checked before attempting to open the
#     file at all, replaced with an actionable French message pointing at
#     disk space / TMPDIR instead of the raw HDF5 error.

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

# ── Helper (Step-3.8): detect a truncated/corrupted upload before opening it ────────────────
#' Compare the browser-reported upload size to the actual bytes written to
#' the temp file. A mismatch means the write to disk failed partway through
#' (almost always: destination drive ran out of space) -- catching this here
#' turns an opaque low-level HDF5/Seurat parser crash into an actionable
#' message that names the actual cause and how to fix it.
#'
#' @param datapath  Path to the uploaded temp file (input$file$datapath).
#' @param expected_size  Browser-reported size in bytes (input$file$size).
#' @return list(ok, msg). ok=TRUE means sizes match (or expected_size unknown
#'   -- fileInput doesn't always populate $size reliably, in which case this
#'   check is silently skipped rather than raising a false alarm).
.verify_upload_integrity <- function(datapath, expected_size) {
  if (is.null(expected_size) || is.na(expected_size) || expected_size <= 0)
    return(list(ok = TRUE, msg = NULL))   # nothing to compare against — skip

  actual_size <- tryCatch(file.info(datapath)$size, error = function(e) NA_real_)
  if (is.na(actual_size))
    return(list(ok = FALSE, msg = .tr_plain("Fichier temporaire introuvable après upload — l'écriture a probablement échoué.")))

  if (actual_size < expected_size) {
    gb <- function(b) sprintf("%.2f Go", b / 1024^3)
    return(list(
      ok = FALSE,
      msg = sprintf(
        paste0(
          "\u274c Upload incomplet : %s reçus sur %s attendus. ",
          "Cause la plus probable : l'espace disque du dossier temporaire R (TMPDIR, ",
          "généralement sur le disque C:) est insuffisant pour ce fichier, MÊME SI l'app et ",
          "vos bibliothèques R sont installées ailleurs (ex: D:). ",
          "Solution : libérez de l'espace sur le disque C:, OU redirigez TMPDIR/TMP/TEMP vers ",
          "un disque avec plus d'espace libre via un fichier .Renviron ",
          "(ex: TMPDIR=D:/Rtemp), puis redémarrez la session R et réessayez l'import."
        ),
        gb(actual_size), gb(expected_size)
      )
    ))
  }

  list(ok = TRUE, msg = NULL)
}

# ── UI ────────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = i18n$t("Import Single-Cell"),
        # LOT 4A (V1.x UX): mapping stays in the analysis module (no UI move);
        # remind + native jump to the existing SC "0. Mapping IDs" panel.
        div(class = "alert alert-light", style = "font-size:0.78rem;",
            bsicons::bs_icon("info-circle"), " ",
            i18n$t("Identifiants non-symboles (Ensembl, Entrez, sondes Affymetrix) ? Le mapping d'IDs est disponible à l'étape 0 de l'analyse.")),
        actionButton(ns("goto_mapping"), i18n$t("Aller au mapping des IDs"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2", icon = icon("arrow-right")),
        accordion(
          accordion_panel(
            i18n$t("Option A: Dossiers Multiples (10X)"),
            value = "opt_a",
            div(class = "alert alert-info", style = "font-size:0.85rem;",
                bsicons::bs_icon("info-circle"),
                " ", i18n$t("Importez plusieurs échantillons pour Harmony.")),
            div(class = "alert alert-light", style = "font-size:0.8rem;",
                bsicons::bs_icon("lightbulb"),
                " ", i18n$t("Formats acceptés : barcodes.tsv(.gz), features.tsv(.gz) ou genes.tsv(.gz), matrix.mtx(.gz).")),
            uiOutput(ns("dir_select_ui")),
            textInput(ns("sample_name"), i18n$t("Nom de l'échantillon"), placeholder = "Ex: Patient1"),
            actionButton(ns("btn_add_sample"), i18n$t("➕ Ajouter à la liste"), class = "btn-info w-100 mt-2"),
            hr(),
            h6(i18n$t("Échantillons ajoutés:"), style = "font-weight:bold;"),
            div(style = "max-height:200px;overflow-y:auto;border:1px solid #ddd;padding:10px;border-radius:5px;",
                uiOutput(ns("sample_list_display"))),
            actionButton(ns("btn_clear_samples"), i18n$t("🗑️ Tout Effacer"),
                         class = "btn-outline-danger btn-sm w-100 mt-2"),
            hr(),
            verbatimTextOutput(ns("path_display"), placeholder = TRUE),
            actionButton(ns("btn_load_dir"), i18n$t("🚀 Charger Tous les Échantillons"),
                         class = "btn-success w-100 mt-2", icon = icon("play"))
          ),
          accordion_panel(
            i18n$t("Option B: Fichiers Multiples (.rds, .h5, .h5ad)"),
            value = "opt_b",
            div(class = "alert alert-info", style = "font-size:0.85rem;",
                bsicons::bs_icon("info-circle"), " ", i18n$t("Importez plusieurs fichiers pour les fusionner.")),
            div(class = "alert alert-light", style = "font-size:0.78rem;",
                bsicons::bs_icon("hdd"),
                " ", i18n$t("Fichiers volumineux (> quelques Go) : vérifiez l'espace disque disponible sur le disque où pointe le dossier temporaire de R (TMPDIR), pas seulement celui de l'app.")),
            fileInput(ns("file_upload"), i18n$t("Ajouter Fichier(s)"),
                      accept = c(".rds", ".h5", ".h5ad", ".loom", ".rda", ".RData"), multiple = TRUE),
            uiOutput(ns("file_list_display")),
            actionButton(ns("btn_load_file"), i18n$t("🚀 Charger"), class = "btn-primary w-100", icon = icon("play"))
          ),
          accordion_panel(
            i18n$t("Option C: Fichier Unique (Classique)"),
            value = "opt_c",
            fileInput(ns("single_file_upload"), i18n$t("Charger un seul fichier"),
                      accept = c(".rds", ".h5", ".h5ad", ".loom", ".rda", ".RData")),
            helpText(i18n$t("Pour un seul échantillon.")),
            actionButton(ns("btn_load_single"), i18n$t("Charger"), class = "btn-warning w-100"),
            # .rda/.RData "Inspect & Select" — carte conditionnelle mutualisée
            # (contrat docs/contracts/RDATA_IMPORT_CONTRACT.md)
            rdata_picker_ui(ns("rdata_picker_sc"))
          )
        )
      ),
      card(
        card_header(i18n$t("Résumé de l'objet chargé")),
        layout_columns(
          value_box(title = i18n$t("Cellules"), value = textOutput(ns("nb_cells")),
                    showcase = bsicons::bs_icon("people"), theme = "primary"),
          value_box(title = i18n$t("Gènes"), value = textOutput(ns("nb_genes")),
                    showcase = bsicons::bs_icon("diagram-3"), theme = "secondary"),
          value_box(title = i18n$t("Échantillons"), value = textOutput(ns("nb_samples")),
                    showcase = bsicons::bs_icon("collection"), theme = "info"),
          value_box(title = i18n$t("Statut"), value = textOutput(ns("status_obj")),
                    showcase = bsicons::bs_icon("check-circle"), theme = "light")
        ),
        card_body(h5(i18n$t("Console de Log"), class = "text-muted"),
                  verbatimTextOutput(ns("console_log"), placeholder = TRUE))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────────────────────
mod_import_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      logs(paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg, "\n", logs()))
    }

    # ── .rda/.RData "Inspect & Select" (composant mutualisé) ───────────────
    # Contrat docs/contracts/RDATA_IMPORT_CONTRACT.md : le workspace est
    # inspecté dans un env isolé ; la classe est validée AVANT le commit.
    # L'hôte garde le commit : normalisation prepare_seurat_object puis
    # global_data$sc_obj (précédent des imports existants).
    .RDA_SC_EXPECTED <- c("Seurat", "SingleCellExperiment", "matrix",
                          "dgCMatrix", "dgTMatrix", "data.frame")
    rdata_file_rv <- reactiveVal(NULL)
    rdata_picker_server(
      "rdata_picker_sc",
      file_rv   = rdata_file_rv,
      commit_fn = function(obj, obj_name) {
        if (is.data.frame(obj)) obj <- as.matrix(obj)
        prepared <- prepare_seurat_object(obj, "SingleSample")
        global_data$sc_obj <- prepared
        add_log(paste(.tr("✅ Import réussi:"), ncol(prepared), .tr("cellules"), "—", obj_name))
        showNotification(paste(.tr("✅ Import réussi:"), ncol(prepared), .tr("cellules")),
                         type = "message", duration = 5)
      },
      expected  = .RDA_SC_EXPECTED,
      context   = "import single-cell (.RData)",
      tr        = .tr,
      log_fn    = add_log
    )

    # Dépôt d'un fichier .rda/.RData en Option C : contrôle d'intégrité puis
    # transfert au composant (aperçu + auto-import si objet unique compatible)
    observeEvent(input$single_file_upload, {
      f <- input$single_file_upload
      req(f)
      if (!rdata_is_supported_file(f$datapath)) {
        rdata_file_rv(NULL)
        return()
      }
      integrity <- .verify_upload_integrity(f$datapath, f$size)
      if (!integrity$ok) {
        add_log(paste("❌", integrity$msg))
        showNotification(integrity$msg, type = "error", duration = 10)
        return()
      }
      rdata_file_rv(list(datapath = f$datapath, name = f$name, size = f$size))
    })


    # ── LOT 4A (V1.x UX): native jump to the EXISTING SC mapping panel ──────
    # No UI duplication: select the analysis page (top-level page_navbar via
    # the root session handle) and open the nested accordions hosting the
    # "0. Mapping IDs" panel. Silent no-op if the handles are missing.
    observeEvent(input$goto_mapping, {
      sess <- global_data$session
      req(!is.null(sess))
      nav_select(id = "main_nav", selected = "Analyse Single-Cell", session = sess)
      try(accordion_panel_open(id = "sc-acc_workflow", values = "grp_prep", session = sess), silent = TRUE)
      try(accordion_panel_open(id = "sc-acc_prep", values = "0_mapping", session = sess), silent = TRUE)
    })

    # Re-push translated labels on language switch
    observeEvent(global_data$language, {
      updateTextInput(session, "sample_name", label = .tr("Nom de l'échantillon"))
    }, ignoreInit = TRUE)

    sample_list <- reactiveVal(list())
    volumes     <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "dir_select", roots = volumes, session = session)

    output$dir_select_ui <- renderUI({
      global_data$language
      shinyDirButton(
        ns("dir_select"),
        label = .tr("📁 Ajouter un Dossier"),
        title = .tr("Sélectionner dossier contenant matrix.mtx"),
        class = "btn-secondary w-100",
        icon  = icon("folder-open")
      )
    })

    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      path <- parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) { dir_path(path); add_log(paste(.tr("Dossier:"), path)) }
    })
    output$path_display <- renderText({
      global_data$language
      if (is.null(dir_path())) .tr("Aucun dossier sélectionné") else dir_path()
    })

    observeEvent(input$btn_add_sample, {
      req(dir_path(), input$sample_name)
      if (!nchar(trimws(input$sample_name))) {
        showNotification(.tr("⚠️ Nom vide."), type = "warning"); return()
      }
      cs <- sample_list()
      if (input$sample_name %in% names(cs)) {
        showNotification(.tr("⚠️ Ce nom existe déjà."), type = "warning"); return()
      }
      cs[[input$sample_name]] <- dir_path(); sample_list(cs)
      add_log(paste(.tr("Échantillon ajouté:"), input$sample_name))
    })

    output$sample_list_display <- renderUI({
      s <- names(sample_list())
      if (!length(s)) return(tags$em(.tr("Aucun échantillon"), style = "color:#999;"))
      tags$ul(lapply(s, tags$li))
    })

    observeEvent(input$btn_clear_samples, { sample_list(list()); add_log(.tr("Liste effacée")) })

    # ── Option A ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_dir, {
      req(sample_list())
      samples <- sample_list()
      add_log(paste(.tr("🔄 Import de"), length(samples), .tr("dossiers 10X...")))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Chargement..."), value = 0)
      tryCatch({
        obj_list <- list()
        for (i in seq_along(samples)) {
          sn <- names(samples)[i]; path <- samples[[i]]
          p$set(i/length(samples), detail=sn)
          add_log(paste("  📂", .tr("Lecture:"), path))
          raw <- load_single_cell_data(path, add_log)
          obj <- prepare_seurat_object(raw, sn)
          obj$orig.ident <- sn; obj_list[[sn]] <- obj
          add_log(paste("    ✓", ncol(obj), .tr("cellules")))
        }
        p$set(0.9, .tr("Fusion..."))
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list), project="MultiSample")
        global_data$sc_obj <- merged
        add_log(paste("✅", ncol(merged), .tr("cellules,"), length(unique(merged$orig.ident)), .tr("échantillon(s)")))
        showNotification(paste(.tr("✅ Import réussi:"), ncol(merged), .tr("cellules")), type = "message", duration = 5)
      }, error = function(e) {
        msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
        add_log(msg); showNotification(msg, type = "error", duration = 10)
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
      add_log(paste(.tr("🔄 Import de"), nrow(files), "fichier(s)..."))
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Chargement..."), value = 0)
      tryCatch({
        obj_list <- list()
        for (i in 1:nrow(files)) {
          fn <- tools::file_path_sans_ext(files$name[i])
          p$set(i/nrow(files), detail=files$name[i])

          # Step-3.8: verify the upload actually completed before attempting
          # to open it (see .verify_upload_integrity() docstring above).
          integrity <- .verify_upload_integrity(files$datapath[i], files$size[i])
          if (!integrity$ok) {
            add_log(paste("  ❌", files$name[i], "—", integrity$msg))
            stop(integrity$msg)
          }

          add_log(paste("  📄", files$name[i]))
          raw <- load_single_cell_data(files$datapath[i], add_log)
          obj <- prepare_seurat_object(raw, fn)
          obj$orig.ident <- fn; obj_list[[fn]] <- obj
          add_log(paste("    ✓", ncol(obj), .tr("cellules")))
        }
        p$set(0.9, .tr("Fusion..."))
        merged <- if (length(obj_list)==1) obj_list[[1]] else
          merge(obj_list[[1]], y=obj_list[-1], add.cell.ids=names(obj_list), project="MultiFile")
        global_data$sc_obj <- merged
        add_log(paste("✅", ncol(merged), .tr("cellules")))
        showNotification(.tr("✅ Import réussi:"), type = "message", duration = 5)
      }, error = function(e) {
        msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
        add_log(msg); showNotification(msg, type = "error", duration = 12)
      })
    })

    # ── Option C ────────────────────────────────────────────────────────────
    observeEvent(input$btn_load_single, {
      req(input$single_file_upload)
      if (rdata_is_supported_file(input$single_file_upload$datapath)) {
        # Le contenu .RData est inspecté/importé dès le dépôt du fichier
        # (observeEvent single_file_upload -> composant mutualisé).
        add_log(.tr("Le contenu .RData est inspecté automatiquement dès le dépôt du fichier (carte ci-dessous)."))
        return()
      }
      add_log(paste(.tr("Import fichier unique...")))
      withProgress(message = .tr("Chargement..."), {
        tryCatch({
          # Step-3.8: same upload-integrity check as Option B — this is the
          # path the user's 1M-neurons .h5 import went through when it hit
          # the HDF5 "unable to open file" crash (root cause: TMPDIR on a
          # nearly-full C: drive truncating the ~4.5Go upload mid-write).
          integrity <- .verify_upload_integrity(input$single_file_upload$datapath,
                                                input$single_file_upload$size)
          if (!integrity$ok) {
            add_log(paste("❌", integrity$msg))
            stop(integrity$msg)
          }

          raw <- load_single_cell_data(input$single_file_upload$datapath, add_log)
          obj <- prepare_seurat_object(raw, "SingleSample")
          global_data$sc_obj <- obj
          add_log(paste(.tr("✅ Import réussi:"), ncol(obj), .tr("cellules")))
          showNotification(.tr("✅ Import réussi:"), type = "message")
        }, error = function(e) {
          msg <- paste(.tr("❌ Erreur:"), conditionMessage(e))
          add_log(msg); showNotification(msg, type = "error", duration = 12)
        })
      })
    })

    # ── Outputs ──────────────────────────────────────────────────────────────
    output$nb_cells   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(ncol(global_data$sc_obj), big.mark=",") })
    output$nb_genes   <- renderText({ if(is.null(global_data$sc_obj)) "-" else format(nrow(global_data$sc_obj), big.mark=",") })
    output$nb_samples <- renderText({ if(is.null(global_data$sc_obj)) "-" else length(unique(global_data$sc_obj$orig.ident)) })
    output$status_obj <- renderText({
      global_data$language
      if(is.null(global_data$sc_obj)) .tr("⚪ Inactif")
      else { n <- length(unique(global_data$sc_obj$orig.ident))
             if(n>1) paste("🟢 Multi (",n,")") else "🟡 Mono" }
    })
    output$console_log <- renderText({
      global_data$language
      txt <- logs()
      if (identical(txt, "En attente d'import...")) .tr("En attente d'import...") else txt
    })

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
            .tr_plain("Si ce dossier contient un .rds ou .h5ad, utilisez l'Option B/C.")))
        }

        # CellRanger v2 compat: create features.tsv.gz if only genes.tsv present
        .ensure_10x_features(path, log_fn)
        return(Read10X(path))
      }

      ext <- tolower(tools::file_ext(path))

      # 2. .rds
      if (ext == "rds") return(readRDS(path))

      # 2bis. .rda/.RData — inspecté par le composant mutualisé ; en flux
      # direct (Option B : fusion multi-fichiers), seul un workspace à objet
      # unique est importable ici (sinon message orientant vers l'Option C).
      if (rdata_is_supported_file(path)) {
        env <- rdata_load_env(path)
        on.exit(rdata_free(env), add = TRUE)
        nms <- ls(envir = env)
        if (length(nms) > 1L) {
          stop(paste0(
            "Ce fichier .RData contient ", length(nms),
            " objets. Utilisez l'Option C (Fichier Unique) pour sélectionner ",
            "l'objet à importer, ou exportez les objets souhaités depuis l'aperçu."),
            call. = FALSE)
        }
        log(paste("  ℹ Objet unique détecté :", nms[1]))
        return(rdata_extract_object(env, nms[1]))
      }

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
