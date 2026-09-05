# =============================================================================
# modules/import/mod_geo.R — GEO Import Module
# =============================================================================
# Step-3.6 fix: mod_geo_server(id, global_data) — was (id, shared_rv).
# On confirm: writes global_data$bulk_obj in standard bulk_obj list format,
# transparent to all downstream bulk modules (filter, DE, report, etc.).
# Internal GEO state (accession, suppl files, parsed counts) stays in rv.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── UI ────────────────────────────────────────────────────────────────────────

mod_geo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(i18n$t("Importer depuis GEO"), class = "mt-0 mb-3"),

    # LOT 4A (V1.x UX): GEO feeds the Bulk analysis — mapping stays there;
    # remind + native jump to the existing Bulk "0. Mapping IDs" panel.
    div(class = "alert alert-light", style = "font-size:0.78rem;",
        bsicons::bs_icon("info-circle"), " ",
        i18n$t("Identifiants non-symboles (Ensembl, Entrez, sondes Affymetrix) ? Le mapping d'IDs est disponible à l'étape 0 de l'analyse.")),
    actionButton(ns("goto_mapping"), i18n$t("Aller au mapping des IDs"),
                 class = "btn-outline-secondary btn-sm w-100 mb-2", icon = icon("arrow-right")),

    radioButtons(
      ns("mode"), label = i18n$t("Mode d'import"),
      choices = stats::setNames(c("online", "offline"),
                                c(.tr_plain("Accession GEO (téléchargement automatique)"),
                                  .tr_plain("Fichiers locaux (hors-ligne)"))),
      selected = "online", inline = TRUE
    ),

    # ── Online mode ──────────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] == 'online'", ns("mode")),

      fluidRow(
        column(6,
          textInput(ns("accession"), i18n$t("Accession GEO (GSExxx)"),
                    placeholder = "ex: GSE52778")
        ),
        column(3, br(),
          actionButton(ns("btn_fetch"), i18n$t("Télécharger"),
                       icon = icon("download"), class = "btn-primary mt-1")
        )
      ),

      uiOutput(ns("ui_suppl_selector")),
      uiOutput(ns("ui_online_status"))
    ),

    # ── Offline mode ─────────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] == 'offline'", ns("mode")),

      fileInput(ns("file_counts"),
                i18n$t("Fichier de counts (tsv/csv/txt/xlsx)"),
                accept = c(".tsv", ".csv", ".txt", ".xlsx")),

      fileInput(ns("file_meta"),
                i18n$t("Fichier de métadonnées (optionnel) — series_matrix.txt ou csv/tsv"),
                accept = c(".txt", ".csv", ".tsv", ".gz"))
    ),

    uiOutput(ns("ui_preview")),
    uiOutput(ns("ui_confirm"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_geo_server <- function(id, global_data) {   # FIXED: was (id, shared_rv)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── LOT 4A (V1.x UX): native jump to the EXISTING Bulk mapping panel ────
    # GEO produces a bulk_obj; its ID mapping lives in the Bulk analysis module.
    observeEvent(input$goto_mapping, {
      sess <- global_data$session
      req(!is.null(sess))
      nav_select(id = "main_nav", selected = "Analyse Bulk RNA", session = sess)
      try(accordion_panel_open(id = "bulk-acc_bulk", values = "panel_mapping", session = sess), silent = TRUE)
    })

    observeEvent(global_data$language, {
      updateRadioButtons(session, "mode", label = .tr("Mode d'import"),
        choices = stats::setNames(c("online", "offline"),
                                  c(.tr("Accession GEO (téléchargement automatique)"),
                                    .tr("Fichiers locaux (hors-ligne)"))))
      updateTextInput(session, "accession", label = .tr("Accession GEO (GSExxx)"))
      updateActionButton(session, "btn_fetch", label = .tr("Télécharger"))
    }, ignoreInit = TRUE)

    # Internal GEO state — nothing leaves this rv except via global_data$bulk_obj
    rv <- reactiveValues(
      counts        = NULL,
      metadata      = NULL,
      suppl_files   = NULL,
      suppl_choices = NULL,
      fetch_msg     = NULL,
      fetch_ok      = FALSE,
      accession     = NULL   # was shared_rv$geo_accession
    )

    # ── ONLINE: fetch GEO ─────────────────────────────────────────────────────
    observeEvent(input$btn_fetch, {
      req(nchar(trimws(input$accession)) > 0)

      accession <- toupper(trimws(input$accession))
      rv$accession <- accession             # store locally (was shared_rv$geo_accession)
      rv$fetch_ok  <- FALSE
      rv$fetch_msg <- tags$span(icon("spinner", class = "fa-spin"),
                                .tr(" Téléchargement en cours…"), class = "text-muted")

      result <- tryCatch(.geo_fetch(accession), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))

      if (!result$ok) {
        rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", result$msg, class = "text-danger")
        return()
      }

      rv$suppl_files   <- result$suppl_files
      rv$suppl_choices <- result$suppl_choices
      rv$metadata      <- result$metadata

      if (length(rv$suppl_choices) == 1) {
        loaded <- .load_counts_file(rv$suppl_choices[[1]])
        if (loaded$ok) {
          rv$counts   <- loaded$counts
          rv$fetch_ok <- TRUE
          rv$fetch_msg <- tags$span(icon("circle-check"),
                                    sprintf(" %s : %d gènes × %d échantillons",
                                            accession, nrow(rv$counts), ncol(rv$counts)),
                                    class = "text-success")
        } else {
          rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", loaded$msg, class = "text-danger")
        }
      } else {
        rv$fetch_msg <- tags$span(icon("circle-check"),
                                  sprintf(" %s : %d fichiers supplémentaires trouvés — choisissez ci-dessous.",
                                          accession, length(rv$suppl_choices)),
                                  class = "text-warning")
      }
    })

    # ── Supplementary file selector ───────────────────────────────────────────
    output$ui_suppl_selector <- renderUI({
      req(length(rv$suppl_choices) > 1)
      tagList(
        selectInput(ns("suppl_file"), .tr("Choisir le fichier de counts :"), choices = rv$suppl_choices),
        actionButton(ns("btn_load_suppl"), .tr("Charger ce fichier"),
                     icon = icon("file-import"), class = "btn-outline-primary btn-sm")
      )
    })

    observeEvent(input$btn_load_suppl, {
      req(input$suppl_file)
      loaded <- .load_counts_file(input$suppl_file)
      if (loaded$ok) {
        rv$counts   <- loaded$counts
        rv$fetch_ok <- TRUE
        rv$fetch_msg <- tags$span(icon("circle-check"),
                                  sprintf(" Chargé : %d gènes × %d échantillons",
                                          nrow(rv$counts), ncol(rv$counts)),
                                  class = "text-success")
      } else {
        rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", loaded$msg, class = "text-danger")
      }
    })

    output$ui_online_status <- renderUI({ rv$fetch_msg })

    # ── OFFLINE: local files ──────────────────────────────────────────────────
    observeEvent(input$file_counts, {
      req(input$file_counts)
      loaded <- .load_counts_file(input$file_counts$datapath, orig_name = input$file_counts$name)
      if (loaded$ok) {
        rv$counts   <- loaded$counts
        rv$fetch_ok <- TRUE
      } else {
        showNotification(loaded$msg, type = "error", duration = 8)
        rv$counts   <- NULL
        rv$fetch_ok <- FALSE
      }
    })

    observeEvent(input$file_meta, {
      req(input$file_meta)
      path <- input$file_meta$datapath
      name <- input$file_meta$name

      meta <- tryCatch({
        if (grepl("series_matrix", name, ignore.case = TRUE) ||
            grepl("!Sample_", readLines(path, n = 3, warn = FALSE)[1])) {
          parse_geo_series_matrix(path)
        } else {
          ext <- tolower(tools::file_ext(name))
          if (ext %in% c("csv", "tsv", "txt")) {
            sep <- if (ext == "csv") "," else "\t"
            df  <- read.delim(path, sep = sep, header = TRUE,
                              check.names = FALSE, stringsAsFactors = FALSE)
            if (!is.numeric(df[[1]])) { rownames(df) <- df[[1]]; df <- df[, -1, drop = FALSE] }
            df
          } else NULL
        }
      }, error = function(e) {
        showNotification(paste(.tr("Erreur métadonnées :"), conditionMessage(e)), type = "warning", duration = 8)
        NULL
      })
      rv$metadata <- meta
    })

    # ── Preview ───────────────────────────────────────────────────────────────
    output$ui_preview <- renderUI({
      req(rv$counts)
      counts   <- rv$counts
      meta     <- rv$metadata
      id_type  <- tryCatch(detect_gene_id_type(rownames(counts)), error = function(e) "unknown")

      align_msg <- if (!is.null(meta)) {
        n_match <- sum(colnames(counts) %in% rownames(meta))
        if (n_match == ncol(counts)) {
          tags$span(icon("circle-check"),
                    .t_fmt(.tr(" Métadonnées alignées ({a}/{b})"), a = n_match, b = ncol(counts)),
                    class = "text-success small")
        } else {
          tags$span(icon("triangle-exclamation"),
                    .t_fmt(.tr(" {a}/{b} échantillons alignés"), a = n_match, b = ncol(counts)),
                    class = "text-warning small")
        }
      } else {
        tags$span(icon("circle-info"),
                  .tr(" Pas de métadonnées — inférées depuis les noms de colonnes."),
                  class = "text-muted small")
      }

      tagList(
        hr(),
        h5(.tr("Aperçu")),
        tags$p(tags$b(.tr("Dimensions : ")), sprintf("%d gènes × %d échantillons", nrow(counts), ncol(counts))),
        tags$p(tags$b(.tr("Type d'identifiants : ")), id_type),
        align_msg,
        tags$b(.tr("Counts (5×5) :")),
        tableOutput(ns("tbl_counts_preview"))
      )
    })

    output$tbl_counts_preview <- renderTable({
      req(rv$counts)
      m <- rv$counts[seq_len(min(5, nrow(rv$counts))),
                     seq_len(min(5, ncol(rv$counts))), drop = FALSE]
      as.data.frame(m)
    }, rownames = TRUE)

    # ── Confirm import → write global_data$bulk_obj ───────────────────────────
    output$ui_confirm <- renderUI({
      req(rv$fetch_ok || (!is.null(rv$counts) && input$mode == "offline"))
      tagList(hr(), actionButton(ns("btn_confirm"), .tr("Confirmer l'import"),
                                 icon = icon("check"), class = "btn-success"))
    })

    observeEvent(input$btn_confirm, {
      req(rv$counts)
      counts <- rv$counts
      meta   <- rv$metadata

      # Build/align metadata
      if (is.null(meta) || sum(colnames(counts) %in% rownames(meta)) < ncol(counts)) {
        meta <- data.frame(sample = colnames(counts), row.names = colnames(counts),
                           stringsAsFactors = FALSE)
        showNotification(
          .tr("Métadonnées auto-générées depuis les noms de colonnes. Complétez-les dans l'onglet QC."),
          type = "message", duration = 6
        )
      } else {
        meta <- meta[colnames(counts), , drop = FALSE]
      }
      if (!"sample" %in% colnames(meta)) meta$sample <- rownames(meta)

      # FIXED: write to global_data$bulk_obj (standard bulk_obj format)
      global_data$bulk_obj <- list(
        counts       = counts,
        metadata     = meta,
        project      = rv$accession %||% "GEO_import",
        type         = "bulk",
        timestamp    = Sys.time(),
        import_mode  = "geo",
        gene_id_type = tryCatch(detect_gene_id_type(rownames(counts)), error = function(e) "unknown")
      )

      showNotification(
        sprintf("✅ Import GEO confirmé : %d gènes × %d échantillons",
                nrow(counts), ncol(counts)),
        type = "message", duration = 5
      )
    })

  })
}

# =============================================================================
# Internal helpers (file-scoped, not exported)
# =============================================================================

#' Fetch GEO supplementary files + series_matrix via GEOquery
.geo_fetch <- function(accession) {
  if (!requireNamespace("GEOquery", quietly = TRUE))
    stop("Package 'GEOquery' requis. Installez via BiocManager::install('GEOquery').")

  destdir <- file.path(tempdir(), accession)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  gse <- tryCatch(
    GEOquery::getGEO(accession, destdir = destdir, GSEMatrix = TRUE,
                     AnnotGPL = FALSE, getGPL = FALSE),
    error = function(e) stop("Impossible de récupérer ", accession, " : ", conditionMessage(e))
  )

  meta <- tryCatch({
    pheno <- Biobase::pData(if (is.list(gse)) gse[[1]] else gse)
    informative <- vapply(pheno, function(col) {
      n_uniq <- length(unique(col))
      n_uniq > 1 && n_uniq < nrow(pheno) && !all(grepl("^ftp|^http", col))
    }, logical(1))
    pheno[, informative, drop = FALSE]
  }, error = function(e) NULL)

  supp_files <- tryCatch(
    GEOquery::getGEOSuppFiles(accession, makeDirectory = FALSE, baseDir = destdir),
    error = function(e) stop("Impossible de télécharger les supplémentaires : ", conditionMessage(e))
  )

  candidate_exts <- c("tsv", "csv", "txt", "gz", "xlsx")
  candidate_paths <- rownames(supp_files)[
    tolower(tools::file_ext(rownames(supp_files))) %in% candidate_exts
  ]

  if (length(candidate_paths) == 0)
    stop("Aucun fichier tabulaire trouvé dans les supplémentaires de ", accession,
         ". Utilisez le mode hors-ligne.")

  choices <- setNames(candidate_paths, basename(candidate_paths))
  list(ok = TRUE, suppl_files = candidate_paths, suppl_choices = choices,
       metadata = meta, msg = NULL)
}

#' Load a counts file (auto-detect sep, handle .gz)
.load_counts_file <- function(path, orig_name = NULL) {
  name <- if (!is.null(orig_name)) orig_name else basename(path)

  if (grepl("\\.gz$", name, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = sub("\\.gz$", "", paste0(".", tools::file_ext(name))))
    tryCatch(
      R.utils::gunzip(path, destname = tmp, overwrite = TRUE, remove = FALSE),
      error = function(e) {
        con_in  <- gzcon(file(path, "rb"))
        con_out <- file(tmp, "wb")
        writeBin(readBin(con_in, "raw", n = 1e8), con_out)
        close(con_in); close(con_out)
      }
    )
    path <- tmp
    name <- sub("\\.gz$", "", name, ignore.case = TRUE)
  }

  result <- tryCatch({
    ext <- tolower(tools::file_ext(name))
    df <- switch(ext,
      "csv"  = read.csv(path,  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE),
      "tsv"  = ,
      "txt"  = read.delim(path, header = TRUE, sep = "\t", check.names = FALSE, stringsAsFactors = FALSE),
      "xlsx" = {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("Package 'readxl' requis pour lire les fichiers .xlsx")
        as.data.frame(readxl::read_excel(path, col_names = TRUE))
      },
      stop("Extension non supportée : ", ext)
    )

    if (ncol(df) > 1 && !is.numeric(df[[1]])) {
      rownames(df) <- make.unique(as.character(df[[1]]))
      df <- df[, -1, drop = FALSE]
    }

    num_cols <- vapply(df, is.numeric, logical(1))
    if (!all(num_cols)) {
      df[!num_cols] <- lapply(df[!num_cols], function(col) {
        v <- suppressWarnings(as.numeric(col))
        if (sum(is.na(v)) > sum(is.na(col))) col else v
      })
      num_cols2 <- vapply(df, is.numeric, logical(1))
      if (!all(num_cols2))
        stop(sprintf("%d colonne(s) non numérique(s). Vérifiez que ce fichier est bien une matrice de counts.",
                     sum(!num_cols2)))
    }

    mat <- as.matrix(df)
    list(ok = TRUE, counts = mat, msg = NULL)
  }, error = function(e) list(ok = FALSE, counts = NULL, msg = conditionMessage(e)))

  result
}
