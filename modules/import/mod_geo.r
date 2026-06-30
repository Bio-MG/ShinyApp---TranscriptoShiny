
# =============================================================================
# modules/import/mod_geo.R — GEO Import Module
# =============================================================================
# Depends on: helpers_io.R (parse_geo_series_matrix, detect_gene_id_type)
# Outputs (via shared_rv):
#   shared_rv$counts       — numeric matrix, genes x samples
#   shared_rv$metadata     — data.frame, samples x variables (rownames = sample IDs)
#   shared_rv$data_type    — "bulk"
#   shared_rv$import_source — "geo"
#   shared_rv$geo_accession — character (e.g. "GSE52778")
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────

#' @export
mod_geo_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Importer depuis GEO", class = "mt-0 mb-3"),

    # ── Mode selector ────────────────────────────────────────────────────────
    radioButtons(
      ns("mode"), label = "Mode d'import",
      choices = c(
        "Accession GEO (téléchargement automatique)" = "online",
        "Fichiers locaux (hors-ligne)"                = "offline"
      ),
      selected = "online", inline = TRUE
    ),

    # ── Online mode ──────────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] == 'online'", ns("mode")),

      fluidRow(
        column(6,
          textInput(ns("accession"), "Accession GEO (GSExxx)",
                    placeholder = "ex: GSE52778")
        ),
        column(3, br(),
          actionButton(ns("btn_fetch"), "Télécharger",
                       icon = icon("download"), class = "btn-primary mt-1")
        )
      ),

      uiOutput(ns("ui_suppl_selector")),   # shown when multiple supp files found

      uiOutput(ns("ui_online_status"))
    ),

    # ── Offline mode ─────────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] == 'offline'", ns("mode")),

      fileInput(ns("file_counts"),
                "Fichier de counts (tsv/csv/txt/xlsx)",
                accept = c(".tsv", ".csv", ".txt", ".xlsx")),

      fileInput(ns("file_meta"),
                "Fichier de métadonnées (optionnel) — series_matrix.txt ou csv/tsv",
                accept = c(".txt", ".csv", ".tsv", ".gz"))
    ),

    # ── Preview (shared) ─────────────────────────────────────────────────────
    uiOutput(ns("ui_preview")),

    # ── Confirm import ────────────────────────────────────────────────────────
    uiOutput(ns("ui_confirm"))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

#' @export
mod_geo_server <- function(id, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Internal state ────────────────────────────────────────────────────────
    rv <- reactiveValues(
      counts       = NULL,   # matrix
      metadata     = NULL,   # data.frame
      suppl_files  = NULL,   # character vector — paths to downloaded supp files
      suppl_choices = NULL,  # named vector for selectInput (label → path)
      fetch_msg    = NULL,   # status message (HTML)
      fetch_ok     = FALSE
    )

    # ── ONLINE: fetch GEO ─────────────────────────────────────────────────────
    observeEvent(input$btn_fetch, {
      req(nchar(trimws(input$accession)) > 0)

      accession <- toupper(trimws(input$accession))

      rv$fetch_ok  <- FALSE
      rv$fetch_msg <- tags$span(icon("spinner", class = "fa-spin"),
                                " Téléchargement en cours…", class = "text-muted")

      result <- tryCatch({
        .geo_fetch(accession)
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      })

      if (!result$ok) {
        rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", result$msg,
                                  class = "text-danger")
        return()
      }

      rv$suppl_files   <- result$suppl_files
      rv$suppl_choices <- result$suppl_choices
      rv$metadata      <- result$metadata   # from series_matrix, may be NULL

      # If exactly one counts candidate → load immediately
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
          rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", loaded$msg,
                                    class = "text-danger")
        }
      } else {
        rv$fetch_msg <- tags$span(icon("circle-check"),
                                  sprintf(" %s : %d fichiers supplémentaires trouvés — choisissez ci-dessous.",
                                          accession, length(rv$suppl_choices)),
                                  class = "text-warning")
      }

      shared_rv$geo_accession <- accession
    })

    # ── Supplementary file selector (online, multiple files) ─────────────────
    output$ui_suppl_selector <- renderUI({
      req(length(rv$suppl_choices) > 1)
      tagList(
        selectInput(ns("suppl_file"), "Choisir le fichier de counts :",
                    choices = rv$suppl_choices),
        actionButton(ns("btn_load_suppl"), "Charger ce fichier",
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
        rv$fetch_msg <- tags$span(icon("circle-xmark"), " ", loaded$msg,
                                  class = "text-danger")
      }
    })

    output$ui_online_status <- renderUI({ rv$fetch_msg })

    # ── OFFLINE: local file upload ─────────────────────────────────────────────
    observeEvent(input$file_counts, {
      req(input$file_counts)
      loaded <- .load_counts_file(input$file_counts$datapath,
                                  orig_name = input$file_counts$name)
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
            # first column as rownames if it looks like sample IDs
            if (!is.numeric(df[[1]])) {
              rownames(df) <- df[[1]]; df <- df[, -1, drop = FALSE]
            }
            df
          } else NULL
        }
      }, error = function(e) {
        showNotification(paste("Erreur métadonnées :", conditionMessage(e)),
                         type = "warning", duration = 8)
        NULL
      })
      rv$metadata <- meta
    })

    # ── Preview ───────────────────────────────────────────────────────────────
    output$ui_preview <- renderUI({
      req(rv$counts)
      counts <- rv$counts
      meta   <- rv$metadata

      id_type <- detect_gene_id_type(rownames(counts))

      # Alignment check counts ↔ metadata
      align_msg <- if (!is.null(meta)) {
        n_match <- sum(colnames(counts) %in% rownames(meta))
        if (n_match == ncol(counts)) {
          tags$span(icon("circle-check"),
                    sprintf(" Métadonnées alignées (%d/%d)", n_match, ncol(counts)),
                    class = "text-success small")
        } else {
          tags$span(icon("triangle-exclamation"),
                    sprintf(" %d/%d échantillons alignés entre counts et métadonnées",
                            n_match, ncol(counts)),
                    class = "text-warning small")
        }
      } else {
        tags$span(icon("circle-info"),
                  " Pas de métadonnées — elles seront inférées depuis les noms de colonnes.",
                  class = "text-muted small")
      }

      tagList(
        hr(),
        h5("Aperçu"),
        tags$p(
          tags$b("Dimensions : "),
          sprintf("%d gènes × %d échantillons", nrow(counts), ncol(counts))
        ),
        tags$p(tags$b("Type d'identifiants : "), id_type),
        align_msg,

        if (!is.null(meta)) {
          tagList(
            br(),
            tags$b("Métadonnées (5 premières lignes) :"),
            DT::renderDataTable(
              head(meta, 5), options = list(dom = "t", scrollX = TRUE),
              rownames = TRUE
            ) |> DT::datatable() |> (\(x) NULL)()  # placeholder, use renderUI pattern below
          )
        },

        # Counts head
        tags$b("Counts (5×5) :"),
        tableOutput(ns("tbl_counts_preview"))
      )
    })

    output$tbl_counts_preview <- renderTable({
      req(rv$counts)
      m <- rv$counts[seq_len(min(5, nrow(rv$counts))),
                      seq_len(min(5, ncol(rv$counts))), drop = FALSE]
      as.data.frame(m)
    }, rownames = TRUE)

    # ── Confirm import ────────────────────────────────────────────────────────
    output$ui_confirm <- renderUI({
      req(rv$fetch_ok || (!is.null(rv$counts) && input$mode == "offline"))
      tagList(
        hr(),
        actionButton(ns("btn_confirm"), "Confirmer l'import",
                     icon = icon("check"), class = "btn-success")
      )
    })

    observeEvent(input$btn_confirm, {
      req(rv$counts)

      counts <- rv$counts
      meta   <- rv$metadata

      # Build metadata if missing or misaligned
      if (is.null(meta) || sum(colnames(counts) %in% rownames(meta)) < ncol(counts)) {
        meta <- data.frame(
          sample   = colnames(counts),
          row.names = colnames(counts),
          stringsAsFactors = FALSE
        )
        showNotification(
          "Métadonnées auto-générées depuis les noms de colonnes. Complétez-les dans l'onglet QC.",
          type = "message", duration = 6
        )
      } else {
        # Reorder meta rows to match counts columns
        meta <- meta[colnames(counts), , drop = FALSE]
      }

      # Ensure "sample" column present (mod_bulk_filter expects it)
      if (!"sample" %in% colnames(meta)) meta$sample <- rownames(meta)

      shared_rv$counts        <- counts
      shared_rv$metadata      <- meta
      shared_rv$data_type     <- "bulk"
      shared_rv$import_source <- "geo"

      showNotification(
        sprintf("Import GEO confirmé : %d gènes × %d échantillons",
                nrow(counts), ncol(counts)),
        type = "message", duration = 5
      )
    })

  })
}

# =============================================================================
# Internal helpers (not exported — prefix .)
# =============================================================================

#' Fetch GEO supplementary files + series_matrix via GEOquery
#' Returns list(ok, suppl_files, suppl_choices, metadata, msg)
.geo_fetch <- function(accession) {
  if (!requireNamespace("GEOquery", quietly = TRUE))
    stop("Package 'GEOquery' requis pour l'import en ligne. Installez-le via BiocManager::install('GEOquery').")

  destdir <- file.path(tempdir(), accession)
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  # 1. Download series_matrix for metadata
  gse <- tryCatch(
    GEOquery::getGEO(accession, destdir = destdir, GSEMatrix = TRUE,
                     AnnotGPL = FALSE, getGPL = FALSE),
    error = function(e) stop("Impossible de récupérer ", accession, " : ", conditionMessage(e))
  )

  meta <- tryCatch({
    pheno <- Biobase::pData(if (is.list(gse)) gse[[1]] else gse)
    # keep only informative columns (drop URL-like or all-identical ones)
    informative <- vapply(pheno, function(col) {
      n_uniq <- length(unique(col))
      n_uniq > 1 && n_uniq < nrow(pheno) && !all(grepl("^ftp|^http", col))
    }, logical(1))
    pheno[, informative, drop = FALSE]
  }, error = function(e) NULL)

  # 2. Download supplementary files
  supp_files <- tryCatch(
    GEOquery::getGEOSuppFiles(accession, makeDirectory = FALSE, baseDir = destdir),
    error = function(e) stop("Impossible de télécharger les fichiers supplémentaires : ", conditionMessage(e))
  )

  # Filter to candidate count files (tabular)
  candidate_exts <- c("tsv", "csv", "txt", "gz", "xlsx")
  candidate_paths <- rownames(supp_files)[
    tolower(tools::file_ext(rownames(supp_files))) %in% candidate_exts
  ]

  if (length(candidate_paths) == 0)
    stop("Aucun fichier tabulaire trouvé dans les supplémentaires de ", accession,
         ". Téléchargez manuellement et utilisez le mode hors-ligne.")

  # Build named choices: basename → full path
  choices <- setNames(candidate_paths, basename(candidate_paths))

  list(ok = TRUE, suppl_files = candidate_paths, suppl_choices = choices,
       metadata = meta, msg = NULL)
}


#' Load a counts file (auto-detect sep, handle .gz)
#' Returns list(ok, counts, msg)
.load_counts_file <- function(path, orig_name = NULL) {
  name <- if (!is.null(orig_name)) orig_name else basename(path)

  # Decompress .gz on the fly to a temp file
  if (grepl("\\.gz$", name, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = sub("\\.gz$", "", tools::file_ext(name)))
    tryCatch(
      R.utils::gunzip(path, destname = tmp, overwrite = TRUE, remove = FALSE),
      error = function(e) {
        # fallback: base R
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
      "csv"  = read.csv(path,  header = TRUE, check.names = FALSE,
                         stringsAsFactors = FALSE),
      "tsv"  = ,
      "txt"  = read.delim(path, header = TRUE, sep = "\t",
                          check.names = FALSE, stringsAsFactors = FALSE),
      "xlsx" = {
        if (!requireNamespace("readxl", quietly = TRUE))
          stop("Package 'readxl' requis pour lire les fichiers .xlsx")
        as.data.frame(readxl::read_excel(path, col_names = TRUE))
      },
      stop("Extension non supportée : ", ext)
    )

    # First column as row names if non-numeric
    if (ncol(df) > 1 && !is.numeric(df[[1]])) {
      rownames(df) <- make.unique(as.character(df[[1]]))
      df <- df[, -1, drop = FALSE]
    }

    # All numeric check
    num_cols <- vapply(df, is.numeric, logical(1))
    if (!all(num_cols)) {
      non_num <- sum(!num_cols)
      # Attempt coercion for columns that look like integers stored as character
      df[!num_cols] <- lapply(df[!num_cols], function(col) {
        v <- suppressWarnings(as.numeric(col))
        if (sum(is.na(v)) > sum(is.na(col))) col else v
      })
      num_cols2 <- vapply(df, is.numeric, logical(1))
      if (!all(num_cols2))
        stop(sprintf("%d colonne(s) non numérique(s) après tentative de conversion. Vérifiez que ce fichier est bien une matrice de counts.",
                     sum(!num_cols2)))
    }

    mat <- as.matrix(df)
    list(ok = TRUE, counts = mat, msg = NULL)

  }, error = function(e) {
    list(ok = FALSE, counts = NULL, msg = conditionMessage(e))
  })

  result
}
