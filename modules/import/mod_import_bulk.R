# modules/import/mod_import_bulk.R - Import RNA Bulk
# v3: ajout du mode "One file per sample" (import multi-fichiers bulk)
#     en plus du mode "Merged matrix" existant (inchangé).
#
# Depends on helpers_io.R (sourced by global.R, not defined there):
# infer_metadata_from_names(), preview_metadata_split(), detect_gene_id_type()

# ── Helpers mode per-sample ───────────────────────────────────────────────────

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") ||
      (length(x) == 1 && is.na(x))) y else x
}

#' Guess delimiter from filename (.csv → comma, others → tab).
#' @param filename Uploaded filename.
#' @return Character delimiter.
.guess_delim_per_sample <- function(filename) {
  x <- tolower(sub("\\.gz$", "", basename(filename)))
  if (grepl("\\.csv$", x)) "," else "\t"
}

#' Read one bulk sample file (csv/tsv/txt, optionally .gz).
#' @param path Temp path from fileInput.
#' @param filename Original uploaded filename.
#' @return data.frame with trimmed column names.
.read_per_sample_file <- function(path, filename) {
  src <- if (grepl("\\.gz$", tolower(filename))) gzfile(path, open = "rt") else path
  on.exit({
    if (inherits(src, "connection")) try(close(src), silent = TRUE)
  }, add = TRUE)
  
  df <- tryCatch(
    readr::read_delim(
      file = src,
      delim = .guess_delim_per_sample(filename),
      trim_ws = TRUE,
      show_col_types = FALSE,
      guess_max = 5000,
      progress = FALSE
    ),
    error = function(e) stop(sprintf("Lecture %s : %s", filename, e$message))
  )
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  df
}

#' Fraction of numeric-like values in a vector.
#' @param x Vector.
#' @return Numeric in [0,1].
.numeric_frac <- function(x) {
  y <- suppressWarnings(as.numeric(trimws(as.character(x))))
  mean(!is.na(y))
}

#' Infer sample ID from filename (stem, sanitized).
#' @param filename Uploaded filename.
#' @return Character sample ID.
.infer_sample_id <- function(filename) {
  x <- sub("\\.gz$", "", basename(filename))
  x <- sub("\\.[^.]+$", "", x)
  trimws(gsub("[^A-Za-z0-9._-]+", "_", x))
}

#' Infer condition from filename stem (heuristic).
#' @param filename Uploaded filename.
#' @return Character or NA_character_.
.infer_condition <- function(filename) {
  x <- .infer_sample_id(filename)
  parts <- unlist(strsplit(x, "[._-]+", perl = TRUE))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NA_character_)
  bad <- c("^s[0-9]+$", "^sample[0-9]*$", "^rep[0-9]+$",
           "^r[0-9]+$", "^lane[0-9]+$", "^count[s]?$")
  keep <- parts[!grepl(paste(bad, collapse = "|"), tolower(parts))]
  if (length(keep)) keep[[1]] else NA_character_
}

#' Detect candidate gene-ID and count columns in one file.
#' @param df Raw data.frame.
#' @return Named list: gene_candidates, count_candidates, auto_gene, auto_count.
.detect_columns_per_sample <- function(df) {
  nms <- names(df)
  if (!length(nms)) {
    return(list(gene_candidates = character(0), count_candidates = character(0),
                auto_gene = NA_character_, auto_count = NA_character_))
  }
  
  num_frac   <- vapply(df, .numeric_frac, numeric(1))
  lower_nms  <- tolower(nms)
  
  gene_hits  <- grepl("gene|feature|symbol|ensembl|entrez|geneid|featureid|id$",
                      lower_nms)
  count_hits <- grepl("count|counts|raw|readcount|read_count|numreads|expected",
                      lower_nms)
  
  gene_cands  <- nms[gene_hits]
  if (!length(gene_cands)) gene_cands <- nms[num_frac < 0.5]
  auto_gene   <- gene_cands[1] %||% NA_character_
  
  count_cands <- setdiff(nms[count_hits & num_frac > 0.8], auto_gene)
  if (!length(count_cands)) count_cands <- setdiff(nms[num_frac > 0.8], auto_gene)
  auto_count  <- count_cands[1] %||% NA_character_
  
  list(gene_candidates  = gene_cands,  count_candidates = count_cands,
       auto_gene = auto_gene, auto_count = auto_count)
}

#' Standardize one file to a feature_id + one sample column.
#' @param df Raw data.frame.
#' @param sample_id Name to give the count column.
#' @param gene_col Gene ID column name.
#' @param count_col Count column name.
#' @param dup_threshold Max fraction of duplicated feature IDs allowed.
#' @return list(ok, status, data, id_type, integer_like)
.prepare_one_sample <- function(df, sample_id, gene_col, count_col,
                                dup_threshold = 0.05) {
  fail <- function(msg, id_type = "mixed/unknown")
    list(ok = FALSE, status = msg, data = NULL, id_type = id_type,
         integer_like = NA)
  
  if (!gene_col  %in% names(df)) return(fail("Colonne gene_id introuvable"))
  if (!count_col %in% names(df)) return(fail("Colonne count introuvable"))
  
  feat <- trimws(as.character(df[[gene_col]]))
  cnt  <- suppressWarnings(as.numeric(trimws(as.character(df[[count_col]]))))
  
  if (any(is.na(feat) | !nzchar(feat)))
    return(fail("Gene IDs manquants"))
  if (all(is.na(cnt)))
    return(fail("La colonne count n'est pas numérique"))
  
  out <- data.frame(feature_id = feat, count = cnt,
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  dup_frac <- mean(duplicated(out$feature_id))
  if (dup_frac > dup_threshold)
    return(fail(sprintf("Gene IDs dupliqués : %.1f%% > seuil %.1f%%",
                        100 * dup_frac, 100 * dup_threshold),
                id_type = detect_gene_id_type(feat)))
  
  collapsed <- FALSE
  if (anyDuplicated(out$feature_id)) {
    collapsed <- TRUE
    out <- out |>
      dplyr::group_by(feature_id) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  }
  
  int_like <- all(is.na(out$count) | abs(out$count - round(out$count)) < 1e-8)
  names(out)[2] <- sample_id
  
  list(
    ok          = TRUE,
    status      = if (collapsed) "OK (doublons fusionnés)" else "OK",
    data        = out,
    id_type     = detect_gene_id_type(out$feature_id),
    integer_like = int_like
  )
}

#' Merge list of per-sample tables by feature_id (full join).
#' @param tbls List of data.frames.
#' @return Merged data.frame.
.merge_per_sample_tables <- function(tbls) {
  Reduce(function(a, b) dplyr::full_join(a, b, by = "feature_id"), tbls)
}

#' Validate design readiness (min 2 groups, >=2 replicates, sample match).
#' @param counts_mat Gene x sample matrix.
#' @param metadata data.frame with sample_id + condition columns.
#' @return list(ok, de_ok, messages, metadata)
.validate_design <- function(counts_mat, metadata) {
  msgs <- character(0); ok <- TRUE
  
  if (anyDuplicated(metadata$sample_id)) {
    ok <- FALSE
    msgs <- c(msgs, "sample_id dupliqués dans la métadata.")
  }
  if (!setequal(colnames(counts_mat), metadata$sample_id)) {
    ok <- FALSE
    msgs <- c(msgs, "Colonnes counts ≠ sample_id de la métadata.")
  } else {
    metadata <- metadata[match(colnames(counts_mat), metadata$sample_id), , drop = FALSE]
  }
  
  cond <- as.character(metadata$condition)
  cond[is.na(cond) | !nzchar(trimws(cond))] <- NA_character_
  tab <- table(cond, useNA = "no")
  
  de_ok <- FALSE
  if (length(tab) < 2) {
    msgs <- c(msgs, "1 seule condition : QC/PCA actifs, DE/pathway désactivés.")
  } else if (any(tab < 2)) {
    msgs <- c(msgs, "Au moins un groupe a < 2 réplicats : DE/pathway désactivés.")
  } else if (ok) {
    de_ok <- TRUE
  }
  
  list(ok = ok, de_ok = de_ok, messages = unique(msgs), metadata = metadata)
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

mod_import_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import RNA Bulk",
        
        div(class = "alert alert-info", style = "font-size: 0.85rem;",
            bsicons::bs_icon("info-circle"),
            " Importez des données RNA-Seq en Bulk (matrice de counts + métadonnées)."),
        
        # ── Mode selector ──────────────────────────────────────────────────
        radioButtons(
          ns("bulk_import_mode"),
          label    = "Mode d'import",
          choices  = c("Merged matrix (un seul fichier)" = "merged_matrix",
                       "One file per sample"             = "per_sample"),
          selected = "merged_matrix",
          inline   = TRUE
        ),
        
        accordion(
          # ── Panel 1 : counts (mode merged_matrix) ───────────────────────
          accordion_panel(
            "1. Matrice de Counts",
            icon = icon("table"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              
              fileInput(ns("counts_file"), "Fichier de Counts (CSV/TSV/TXT)",
                        accept = c(".csv", ".tsv", ".txt", ".xlsx")),
              
              helpText("Format attendu : Lignes = Gènes, Colonnes = Échantillons. ",
                       "Un filtrage fin (par réplicat) est disponible à l'étape 1 du module d'analyse — ",
                       "le seuil ci-dessous (Options d'Import) n'est qu'un pré-nettoyage grossier."),
              
              radioButtons(ns("counts_format"), "Format de la matrice",
                           choices  = c("Genes en lignes (standard)" = "rows",
                                        "Genes en colonnes (transposé)" = "cols"),
                           selected = "rows"),
              
              checkboxInput(ns("counts_has_header"),   "La 1ère ligne est un en-tête", value = TRUE),
              checkboxInput(ns("counts_has_rownames"), "La 1ère colonne est le nom des gènes", value = TRUE),
              
              h6("Aperçu de la matrice:", style = "font-weight: bold; margin-top: 10px;"),
              div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                  tableOutput(ns("counts_preview"))),
              
              uiOutput(ns("detected_gene_id_banner"))
            )
          ),
          
          # ── Panel 2 : métadonnées (mode merged_matrix) ──────────────────
          accordion_panel(
            "2. Métadonnées (Optionnel)",
            icon = icon("tags"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              
              fileInput(ns("metadata_file"), "Fichier de Métadonnées (CSV/TSV/TXT)",
                        accept = c(".csv", ".tsv", ".txt", ".xlsx")),
              
              helpText("Format : Lignes = Échantillons, Colonnes = Variables (condition, batch, etc.)."),
              
              checkboxInput(ns("metadata_has_header"),   "La 1ère ligne est un en-tête", value = TRUE),
              checkboxInput(ns("metadata_has_rownames"), "La 1ère colonne est le nom des échantillons", value = TRUE),
              
              h6("Aperçu des métadonnées actives:", style = "font-weight: bold; margin-top: 10px;"),
              div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                  tableOutput(ns("metadata_preview"))),
              
              hr(),
              
              div(
                class = "border rounded p-2",
                style = "background:#f8f9fa;",
                h6("Pas de fichier ? Inférer depuis les noms d'échantillons",
                   style = "font-weight: bold; font-size: 0.85em;"),
                helpText(style = "font-size:0.78em;",
                         "Ex: 'Ctrl_1', 'Ctrl_2', 'KO_1', 'KO_2' → colonnes condition + réplicat. ",
                         "Ignoré si un fichier de métadonnées est fourni ci-dessus."),
                
                textInput(ns("infer_delimiter"), "Délimiteur (regex)", value = "[_-]"),
                actionButton(ns("btn_infer_preview"), "👁 Prévisualiser le découpage",
                             class = "btn-outline-secondary btn-sm w-100"),
                
                uiOutput(ns("infer_preview_ui")),
                uiOutput(ns("infer_colnames_ui")),
                
                actionButton(ns("btn_infer_apply"), "✓ Utiliser ces métadonnées inférées",
                             class = "btn-outline-success btn-sm w-100 mt-2")
              )
            )
          ),
          
          # ── Panel 3 : options d'import ───────────────────────────────────
          accordion_panel(
            "3. Options d'Import",
            icon = icon("cogs"),
            
            textInput(ns("project_name"), "Nom du Projet",
                      value = "BulkRNA_Project", placeholder = "Ex: Study_2024"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              numericInput(ns("min_counts"), "Counts minimum par gène (pré-filtre grossier)",
                           value = 10, min = 0, step = 1),
              helpText("Les gènes avec moins de counts seront filtrés dès l'import.")
            )
          ),
          
          # ── Panel 4 : NEW — one file per sample ─────────────────────────
          accordion_panel(
            "4. Import multi-fichiers (one file per sample)",
            icon = icon("files"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'per_sample'", ns("bulk_import_mode")),
              
              fileInput(
                ns("ps_files"),
                "Fichiers bulk (un fichier = un sample)",
                multiple = TRUE,
                accept   = c(".csv", ".tsv", ".txt", ".gz")
              ),
              
              numericInput(
                ns("ps_dup_threshold"),
                "Seuil max. gene IDs dupliqués (fraction)",
                value = 0.05, min = 0, max = 1, step = 0.01
              ),
              
              hr(),
              
              # Résolution ambiguïté colonnes (rendu dynamique)
              uiOutput(ns("ps_resolve_ui")),
              
              h6("📋 Aperçu des fichiers détectés",
                 style = "font-weight:bold; margin-top:8px;"),
              DT::DTOutput(ns("ps_preview_table")),
              
              hr(),
              
              checkboxInput(
                ns("ps_fill_zero"),
                "✅ J'accepte de remplacer les counts manquants (full join) par 0",
                value = FALSE
              ),
              
              h6("📝 Métadonnées auto-générées (éditables)",
                 style = "font-weight:bold; margin-top:8px;"),
              DT::DTOutput(ns("ps_metadata_dt")),
              
              uiOutput(ns("ps_status_banner"))
            )
          )
        ),
        
        hr(),
        # ── Bouton persistant (inchangé) ───────────────────────────────────
        div(
          style = "background:#eef2f5;padding:10px;border-radius:6px;border:1px solid #d6dde1;",
          uiOutput(ns("readiness_banner")),
          div(id = ns("load_button_container"), style = "margin-top:8px;",
              actionButton(ns("btn_load"), "🚀 Charger les Données",
                           class = "btn-success w-100", icon = icon("play"))
          )
        )
      ),
      
      card(
        card_header("Résumé des Données Bulk"),
        layout_columns(
          value_box(title = "Échantillons",
                    value   = textOutput(ns("nb_samples")),
                    showcase = bsicons::bs_icon("grid-3x3"),
                    theme   = "primary"),
          value_box(title = "Gènes",
                    value   = textOutput(ns("nb_genes")),
                    showcase = bsicons::bs_icon("diagram-3"),
                    theme   = "secondary"),
          value_box(title = "Variables Métadonnées",
                    value   = textOutput(ns("nb_metadata_vars")),
                    showcase = bsicons::bs_icon("tags"),
                    theme   = "info"),
          value_box(title = "Statut",
                    value   = textOutput(ns("status_obj")),
                    showcase = bsicons::bs_icon("check-circle"),
                    theme   = "light")
        ),
        card_body(
          h5("Console de Log", class = "text-muted"),
          verbatimTextOutput(ns("console_log"), placeholder = TRUE)
        )
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

mod_import_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Logger (inchangé) ──────────────────────────────────────────────────
    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      logs(paste0("[", timestamp, "] ", msg, "\n", isolate(logs())))
    }
    
    temp_data <- reactiveValues(is_loaded = FALSE)
    
    # ── Metadata auto-inference state (inchangé) ───────────────────────────
    infer_preview_rv  <- reactiveVal(NULL)
    inferred_metadata <- reactiveVal(NULL)
    
    # ── Per-sample reactive state ──────────────────────────────────────────
    rv_ps <- reactiveValues(
      metadata  = NULL,   # auto-generated metadata data.frame
      preview   = NULL    # preview table (one row per file)
    )
    
    # ── smart_read (inchangé) ──────────────────────────────────────────────
    smart_read <- function(filepath, has_header = TRUE, has_rownames = TRUE) {
      ext <- tolower(tools::file_ext(filepath))
      tryCatch({
        df <- switch(
          ext,
          "csv"  = read.csv(filepath, header = has_header,
                            row.names = if (has_rownames) 1 else NULL,
                            check.names = FALSE, stringsAsFactors = FALSE,
                            fileEncoding = "UTF-8-BOM"),
          "tsv"  = read.delim(filepath, header = has_header,
                              row.names = if (has_rownames) 1 else NULL,
                              sep = "\t", check.names = FALSE,
                              stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
          "txt"  = read.delim(filepath, header = has_header,
                              row.names = if (has_rownames) 1 else NULL,
                              check.names = FALSE, stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8-BOM"),
          "xlsx" = {
            if (!requireNamespace("readxl", quietly = TRUE))
              stop("Package 'readxl' nécessaire pour lire les fichiers .xlsx")
            tmp <- readxl::read_excel(filepath, col_names = has_header)
            tmp <- as.data.frame(tmp, check.names = FALSE, stringsAsFactors = FALSE)
            if (has_rownames) { rownames(tmp) <- tmp[[1]]; tmp <- tmp[, -1, drop = FALSE] }
            tmp
          },
          stop("Format de fichier non supporté")
        )
        if (!is.null(colnames(df))) colnames(df) <- trimws(sub("^\ufeff", "", colnames(df)))
        if (!is.null(rownames(df))) rownames(df) <- trimws(sub("^\ufeff", "", rownames(df)))
        df
      }, error = function(e) stop(paste("Erreur de lecture:", e$message)))
    }
    
    # =========================================================================
    # MODE MERGED_MATRIX — inchangé
    # =========================================================================
    
    counts_reactive <- reactive({
      req(input$counts_file)
      tryCatch({
        add_log("📂 Lecture du fichier de counts...")
        df <- smart_read(input$counts_file$datapath,
                         input$counts_has_header, input$counts_has_rownames)
        if (input$counts_format == "cols") {
          df <- as.data.frame(t(df))
          add_log(" ↻ Matrice transposée (genes étaient en colonnes)")
        }
        if (nrow(df) == 0 || ncol(df) == 0)
          stop("Matrice vide après lecture. Vérifie les options d'en-tête/rownames.")
        add_log(paste("✓ Matrice de counts chargée:", nrow(df), "gènes ×", ncol(df), "échantillons"))
        df
      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture counts:", e$message))
        showNotification(paste("Erreur lecture counts:", e$message), type = "error", duration = 10)
        NULL
      })
    })
    
    metadata_file_reactive <- reactive({
      if (is.null(input$metadata_file)) return(NULL)
      tryCatch({
        add_log("📂 Lecture du fichier de métadonnées...")
        df <- smart_read(input$metadata_file$datapath,
                         input$metadata_has_header, input$metadata_has_rownames)
        add_log(paste("✓ Métadonnées chargées:", nrow(df), "échantillons ×", ncol(df), "variables"))
        df
      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture métadonnées:", e$message))
        showNotification(paste("Erreur métadonnées:", e$message), type = "warning", duration = 10)
        NULL
      })
    })
    
    effective_metadata <- reactive({ metadata_file_reactive() %||% inferred_metadata() })
    
    observe({
      shinyjs::toggleState("btn_infer_preview", condition = is.null(metadata_file_reactive()))
      shinyjs::toggleState("btn_infer_apply",   condition = is.null(metadata_file_reactive()))
    })
    
    observeEvent(input$btn_infer_preview, {
      req(counts_reactive())
      preview <- tryCatch(
        preview_metadata_split(colnames(counts_reactive()), delimiter = input$infer_delimiter),
        error = function(e) {
          showNotification(paste("Erreur découpage:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
      infer_preview_rv(preview)
    })
    
    output$infer_preview_ui <- renderUI({
      preview <- infer_preview_rv()
      if (is.null(preview)) return(NULL)
      seg_mat <- do.call(rbind, lapply(preview$segments, function(s) {
        length(s) <- max(preview$n_seg); s }))
      rownames(seg_mat) <- NULL
      tagList(
        div(class = if (preview$n_seg_consistent) "alert alert-success" else "alert alert-warning",
            style = "font-size:0.78em;padding:6px;margin-top:6px;",
            if (preview$n_seg_consistent) {
              sprintf("✓ Découpage cohérent : %d segment(s) par échantillon.", preview$n_seg[1])
            } else {
              sprintf("⚠️ Nombre de segments incohérent (min=%d, max=%d).",
                      min(preview$n_seg), max(preview$n_seg))
            }),
        renderTable(as.data.frame(seg_mat, stringsAsFactors = FALSE))()
      )
    })
    
    output$infer_colnames_ui <- renderUI({
      preview <- infer_preview_rv()
      req(preview)
      n_seg <- if (preview$n_seg_consistent) preview$n_seg[1] else min(preview$n_seg)
      default_names <- if (n_seg == 2) c("condition", "replicate") else paste0("segment_", seq_len(n_seg))
      tagList(
        h6("Nom de chaque colonne:", style = "font-size:0.82em;font-weight:bold;margin-top:8px;"),
        lapply(seq_len(n_seg), function(i) {
          textInput(ns(paste0("seg_name_", i)), NULL, value = default_names[i],
                    placeholder = paste("Segment", i), width = "100%")
        })
      )
    })
    
    observeEvent(input$btn_infer_apply, {
      req(counts_reactive(), infer_preview_rv())
      samples <- colnames(counts_reactive())
      preview <- infer_preview_rv()
      n_seg   <- if (preview$n_seg_consistent) preview$n_seg[1] else min(preview$n_seg)
      col_names <- vapply(seq_len(n_seg), function(i) {
        v <- input[[paste0("seg_name_", i)]]
        if (is.null(v) || !nzchar(trimws(v))) paste0("segment_", i) else trimws(v)
      }, character(1))
      if (length(unique(col_names)) != length(col_names)) {
        showNotification("⚠️ Les noms de colonnes doivent être uniques.", type = "warning", duration = 6)
        return()
      }
      result <- withCallingHandlers(
        tryCatch(
          infer_metadata_from_names(samples, delimiter = input$infer_delimiter, col_names = col_names),
          error = function(e) {
            showNotification(paste("Erreur:", conditionMessage(e)), type = "error", duration = 8); NULL
          }),
        warning = function(w) {
          showNotification(paste("⚠️", conditionMessage(w)), type = "warning", duration = 8)
          invokeRestart("muffleWarning")
        })
      req(result)
      inferred_metadata(result)
      add_log(paste("✓ Métadonnées inférées:", paste(col_names, collapse = ", ")))
      showNotification("✓ Métadonnées inférées appliquées.", type = "message", duration = 5)
    })
    
    observeEvent(input$counts_file, {
      infer_preview_rv(NULL); inferred_metadata(NULL); temp_data$is_loaded <- FALSE
    })
    observeEvent(input$metadata_file, { temp_data$is_loaded <- FALSE })
    
    output$detected_gene_id_banner <- renderUI({
      df <- counts_reactive(); req(df)
      detected <- tryCatch(detect_gene_id_type(rownames(df)), error = function(e) "unknown")
      if (detected %in% c("symbol", "unknown")) return(NULL)
      label <- switch(detected, ensembl = "Ensembl Gene ID", entrez = "Entrez ID",
                      affy_probe = "Probe Affymetrix", detected)
      div(class = "alert alert-light",
          style = "font-size:0.78em;border-left:3px solid #F39C12;margin-top:6px;",
          bsicons::bs_icon("lightbulb"),
          sprintf(" Identifiants détectés : %s. Convertissez-les via ", label),
          tags$strong("'0. Mapping IDs'"), " dans le module d'analyse RNA Bulk.")
    })
    
    output$counts_preview <- renderTable({
      df <- counts_reactive(); if (is.null(df)) return(NULL)
      head(df[, 1:min(5, ncol(df)), drop = FALSE], 10)
    }, rownames = TRUE)
    
    output$metadata_preview <- renderTable({
      df <- effective_metadata(); if (is.null(df)) return(NULL); head(df, 10)
    }, rownames = TRUE)
    
    # =========================================================================
    # MODE PER_SAMPLE — nouveau
    # =========================================================================
    
    # ── Parse all uploaded files reactively ───────────────────────────────
    ps_raw <- reactive({
      req(input$bulk_import_mode == "per_sample")
      files <- input$ps_files
      req(files)
      
      lapply(seq_len(nrow(files)), function(i) {
        df <- tryCatch(
          .read_per_sample_file(files$datapath[i], files$name[i]),
          error = function(e) NULL
        )
        list(
          file_index = i,
          filename   = files$name[i],
          datapath   = files$datapath[i],
          df         = df,
          sample_id  = .infer_sample_id(files$name[i]),
          detection  = if (!is.null(df)) .detect_columns_per_sample(df) else list(
            gene_candidates = character(0), count_candidates = character(0),
            auto_gene = NA_character_, auto_count = NA_character_)
        )
      })
    })
    
    # ── Dynamic UI for ambiguous column resolution ─────────────────────────
    output$ps_resolve_ui <- renderUI({
      req(input$bulk_import_mode == "per_sample")
      parsed <- ps_raw()
      
      blocks <- lapply(parsed, function(x) {
        need_gene  <- is.na(x$detection$auto_gene)  || length(x$detection$gene_candidates)  > 1
        need_count <- is.na(x$detection$auto_count) || length(x$detection$count_candidates) > 1
        
        if (!need_gene && !need_count) return(NULL)
        
        wellPanel(
          tags$strong(x$filename),
          if (need_gene && length(x$detection$gene_candidates) > 0) {
            selectInput(
              ns(paste0("ps_gene_col_", x$file_index)),
              "Colonne Gene ID",
              choices  = x$detection$gene_candidates,
              selected = x$detection$auto_gene %||% x$detection$gene_candidates[1]
            )
          },
          if (need_count && length(x$detection$count_candidates) > 0) {
            selectInput(
              ns(paste0("ps_count_col_", x$file_index)),
              "Colonne Count",
              choices  = x$detection$count_candidates,
              selected = x$detection$auto_count %||% x$detection$count_candidates[1]
            )
          }
        )
      })
      
      do.call(tagList, Filter(Negate(is.null), blocks))
    })
    
    # ── Preview table (one row per file) ───────────────────────────────────
    ps_preview <- reactive({
      req(input$bulk_import_mode == "per_sample")
      parsed <- ps_raw()
      
      rows <- lapply(parsed, function(x) {
        gene_col  <- input[[paste0("ps_gene_col_",  x$file_index)]] %||% x$detection$auto_gene
        count_col <- input[[paste0("ps_count_col_", x$file_index)]] %||% x$detection$auto_count
        
        status <- if (is.null(x$df))               "Erreur de lecture"
        else if (is.na(gene_col))         "Gene column non résolue"
        else if (is.na(count_col))        "Count column non résolue"
        else                               "OK"
        
        id_type <- if (!is.null(x$df) && !is.na(gene_col) && gene_col %in% names(x$df))
          tryCatch(detect_gene_id_type(x$df[[gene_col]]), error = function(e) "mixed/unknown")
        else "mixed/unknown"
        
        data.frame(
          filename       = x$filename,
          sample_id      = x$sample_id,
          gene_id_column = gene_col  %||% NA_character_,
          count_column   = count_col %||% NA_character_,
          id_type        = id_type,
          status         = status,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      })
      
      prev <- dplyr::bind_rows(rows)
      rv_ps$preview <- prev
      prev
    })
    
    output$ps_preview_table <- DT::renderDT({
      req(input$bulk_import_mode == "per_sample")
      DT::datatable(
        ps_preview(),
        rownames = FALSE,
        options  = list(pageLength = 8, scrollX = TRUE, dom = "tip")
      )
    })
    
    # ── Auto-generate editable metadata whenever preview changes ──────────
    observe({
      req(input$bulk_import_mode == "per_sample")
      prev <- ps_preview()
      req(nrow(prev) > 0)
      
      new_md <- data.frame(
        sample_id = prev$sample_id,
        filename  = prev$filename,
        condition = vapply(prev$filename, .infer_condition, character(1)),
        batch     = NA_character_,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      # Preserve existing manual edits on filename match
      old <- rv_ps$metadata
      if (!is.null(old) && nrow(old)) {
        idx  <- match(new_md$filename, old$filename)
        keep <- !is.na(idx)
        new_md$sample_id[keep]  <- old$sample_id[idx[keep]]
        new_md$condition[keep]  <- old$condition[idx[keep]]
        new_md$batch[keep]      <- old$batch[idx[keep]]
      }
      
      rv_ps$metadata <- new_md
    })
    
    output$ps_metadata_dt <- DT::renderDT({
      req(rv_ps$metadata)
      DT::datatable(
        rv_ps$metadata,
        rownames = FALSE,
        editable = TRUE,
        options  = list(pageLength = 8, scrollX = TRUE)
      )
    })
    
    observeEvent(input$ps_metadata_dt_cell_edit, {
      info <- input$ps_metadata_dt_cell_edit
      x <- rv_ps$metadata; req(x)
      x[info$row, info$col + 1L] <- info$value   # DT éditable 0-indexé côté JS
      rv_ps$metadata <- x
    })
    
    # ── Status / warnings banner ───────────────────────────────────────────
    output$ps_status_banner <- renderUI({
      req(input$bulk_import_mode == "per_sample")
      msgs <- character(0)
      
      prev <- rv_ps$preview
      if (!is.null(prev) && nrow(prev)) {
        bad_files <- prev$filename[prev$status != "OK" &
                                     !grepl("^OK", prev$status)]
        if (length(bad_files))
          msgs <- c(msgs, paste("Fichiers rejetés:", paste(bad_files, collapse = ", ")))
        
        id_types <- unique(prev$id_type[!is.na(prev$id_type) & prev$id_type != "mixed/unknown"])
        if (length(id_types) > 1)
          msgs <- c(msgs, paste("Types d'identifiants mixtes:", paste(id_types, collapse = ", ")))
      }
      
      md <- rv_ps$metadata
      if (!is.null(md) && nrow(md)) {
        cond <- as.character(md$condition)
        cond <- cond[!is.na(cond) & nzchar(trimws(cond))]
        tab  <- table(cond)
        if (length(tab) < 2)
          msgs <- c(msgs, "1 seule condition : QC/PCA actifs, DE/pathway désactivés.")
        else if (any(tab < 2))
          msgs <- c(msgs, "Au moins 1 groupe avec < 2 réplicats : DE/pathway désactivés.")
        if (!is.null(prev) && nrow(prev) > 0) {
          n_entrez <- sum(prev$id_type == "Entrez", na.rm = TRUE)
          if (n_entrez > 0)
            msgs <- c(msgs, sprintf("%d fichier(s) avec IDs Entrez — conversion possible via '0. Mapping IDs'.",
                                    n_entrez))
        }
      }
      
      if (!length(msgs)) return(NULL)
      div(class = "alert alert-warning", style = "font-size:0.82em;margin-top:6px;",
          lapply(msgs, tags$p))
    })
    
    # =========================================================================
    # BOUTON PERSISTANT : readiness_banner + btn_load
    # =========================================================================
    
    output$readiness_banner <- renderUI({
      if (input$bulk_import_mode == "merged_matrix") {
        counts_ok <- !is.null(counts_reactive())
        meta_ok   <- !is.null(effective_metadata())
        div(style = "font-size:0.8em;",
            if (counts_ok) span("✅ Counts chargés",     style = "color:#18BC9C;font-weight:bold;")
            else           span("⚪ Counts manquants",    style = "color:#999;"),
            " · ",
            if (meta_ok) span("✅ Métadonnées prêtes",   style = "color:#18BC9C;font-weight:bold;")
            else         span("⚪ Métadonnées (optionnel)", style = "color:#999;"))
      } else {
        files <- input$ps_files
        prev  <- rv_ps$preview
        n_ok  <- if (!is.null(prev)) sum(grepl("^OK", prev$status)) else 0
        n_tot <- if (!is.null(prev)) nrow(prev) else 0
        
        div(style = "font-size:0.8em;",
            if (!is.null(files)) span(sprintf("✅ %d/%d fichiers OK", n_ok, n_tot),
                                      style = "color:#18BC9C;font-weight:bold;")
            else span("⚪ Aucun fichier",       style = "color:#999;"),
            " · ",
            if (!is.null(rv_ps$metadata)) span("✅ Métadonnées prêtes", style = "color:#18BC9C;font-weight:bold;")
            else                          span("⚪ Métadonnées",         style = "color:#999;"))
      }
    })
    
    observe({
      ready <- if (input$bulk_import_mode == "merged_matrix") {
        !is.null(counts_reactive())
      } else {
        !is.null(input$ps_files) && !is.null(rv_ps$metadata) && nrow(rv_ps$metadata) > 0
      }
      shinyjs::toggleState("btn_load", condition = ready)
      shinyjs::html("btn_load",
                    html = if (isTRUE(temp_data$is_loaded)) "🔄 Mettre à jour les Données"
                    else "🚀 Charger les Données")
    })
    
    # =========================================================================
    # BTN_LOAD — dispatch merged_matrix / per_sample
    # =========================================================================
    
    observeEvent(input$btn_load, {
      
      # ── Branch merged_matrix (code original inchangé) ─────────────────
      if (input$bulk_import_mode == "merged_matrix") {
        req(counts_reactive())
        add_log("🔄 Préparation de l'objet RNA Bulk...")
        showNotification("Traitement des données...", type = "message", duration = NULL, id = ns("progress"))
        
        tryCatch({
          counts   <- counts_reactive()
          metadata <- effective_metadata()
          
          add_log(" → Conversion des valeurs en numérique...")
          counts <- as.data.frame(counts, check.names = FALSE, stringsAsFactors = FALSE)
          for (col in colnames(counts)) {
            if (is.character(counts[[col]]))
              counts[[col]] <- suppressWarnings(as.numeric(counts[[col]]))
          }
          if (anyNA(as.matrix(counts)))
            add_log(" ⚠️ Certaines valeurs ne sont pas numériques (NA après conversion)")
          
          counts_matrix <- as.matrix(counts)
          gene_counts   <- rowSums(counts_matrix, na.rm = TRUE)
          keep_genes    <- gene_counts >= input$min_counts
          counts_matrix <- counts_matrix[keep_genes, , drop = FALSE]
          add_log(paste(" → Gènes filtrés:", sum(!keep_genes), "retirés,",
                        sum(keep_genes), "conservés"))
          
          bulk_obj <- list(counts    = counts_matrix,
                           metadata  = NULL,
                           project   = input$project_name,
                           type      = "bulk",
                           timestamp = Sys.time())
          
          if (!is.null(metadata)) {
            sample_names   <- colnames(counts_matrix)
            metadata_names <- rownames(metadata)
            if (all(sample_names %in% metadata_names)) {
              bulk_obj$metadata <- metadata[sample_names, , drop = FALSE]
              add_log(paste(" ✓ Métadonnées alignées:", ncol(bulk_obj$metadata), "variables"))
            } else {
              bulk_obj$metadata <- data.frame(sample = sample_names, row.names = sample_names)
              add_log(" ⚠️ Métadonnées non alignées - création de métadonnées par défaut")
            }
          } else {
            bulk_obj$metadata <- data.frame(
              sample = colnames(counts_matrix), row.names = colnames(counts_matrix))
            add_log(" → Aucune métadonnée — colonne 'sample' par défaut créée")
          }
          
          global_data$bulk_obj <- bulk_obj
          temp_data$is_loaded  <- TRUE
          add_log(paste("✅ Import réussi!", nrow(counts_matrix), "gènes ×",
                        ncol(counts_matrix), "échantillons"))
          removeNotification(id = ns("progress"))
          showNotification(paste("✅ Import réussi:", ncol(counts_matrix),
                                 "échantillons,", nrow(counts_matrix), "gènes"),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          removeNotification(id = ns("progress"))
          add_log(paste("❌ Erreur lors du chargement:", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
          temp_data$is_loaded <- FALSE
        })
        
        # ── Branch per_sample ───────────────────────────────────────────────
      } else {
        
        parsed   <- ps_raw()
        prev     <- rv_ps$preview
        metadata <- rv_ps$metadata
        
        req(length(parsed) > 0, nrow(prev) == length(parsed), nrow(metadata) == length(parsed))
        
        if (anyDuplicated(metadata$sample_id)) {
          showNotification("❌ sample_id dupliqués dans la métadata.", type = "error")
          return(NULL)
        }
        
        add_log("🔄 Préparation import multi-fichiers bulk...")
        showNotification("Traitement des données...", type = "message",
                         duration = NULL, id = ns("progress"))
        
        result <- tryCatch({
          
          sample_tables <- vector("list", length(parsed))
          int_like_vec  <- logical(length(parsed))
          id_type_vec   <- character(length(parsed))
          
          for (i in seq_along(parsed)) {
            gene_col  <- prev$gene_id_column[i]
            count_col <- prev$count_column[i]
            
            if (is.null(parsed[[i]]$df)) {
              stop(sprintf("Fichier illisible : %s", parsed[[i]]$filename))
            }
            if (is.na(gene_col) || !nzchar(gene_col)) {
              stop(sprintf("Gene column non résolue : %s", parsed[[i]]$filename))
            }
            if (is.na(count_col) || !nzchar(count_col)) {
              stop(sprintf("Count column non résolue : %s", parsed[[i]]$filename))
            }
            
            res <- .prepare_one_sample(
              df            = parsed[[i]]$df,
              sample_id     = metadata$sample_id[i],
              gene_col      = gene_col,
              count_col     = count_col,
              dup_threshold = input$ps_dup_threshold %||% 0.05
            )
            
            if (!isTRUE(res$ok)) stop(sprintf("%s : %s", parsed[[i]]$filename, res$status))
            
            add_log(paste0(" ✓ ", parsed[[i]]$filename, " (", res$status, ")"))
            sample_tables[[i]] <- res$data
            int_like_vec[i]    <- isTRUE(res$integer_like)
            id_type_vec[i]     <- res$id_type
          }
          
          merged     <- .merge_per_sample_tables(sample_tables)
          count_cols <- setdiff(names(merged), "feature_id")
          has_na     <- anyNA(merged[, count_cols, drop = FALSE])
          
          if (has_na && !isTRUE(input$ps_fill_zero)) {
            stop("Valeurs manquantes après full join. Cochez la case de confirmation avant d'importer.")
          }
          if (has_na) {
            for (cc in count_cols) merged[[cc]][is.na(merged[[cc]])] <- 0L
            add_log(" ⚠️ Counts manquants remplacés par 0 (full join)")
          }
          
          counts_mat <- as.matrix(merged[, count_cols, drop = FALSE])
          rownames(counts_mat) <- merged$feature_id
          storage.mode(counts_mat) <- "double"
          
          # Warn for non-integer (DESeq2 mode)
          if (!all(int_like_vec)) {
            bad_samples <- metadata$sample_id[!int_like_vec]
            add_log(paste(" ⚠️ Counts non entiers :", paste(bad_samples, collapse = ", ")))
            showNotification(paste("⚠️ Counts non entiers pour :",
                                   paste(bad_samples, collapse = ", "),
                                   "— DESeq2 nécessite des entiers."),
                             type = "warning", duration = NULL)
          }
          
          # ID type
          unique_types <- unique(id_type_vec[!is.na(id_type_vec)])
          merged_id_type <- if (length(unique_types) == 1) unique_types else "mixed/unknown"
          if (length(unique_types) > 1)
            showNotification(paste("⚠️ Types d'ID mixtes :", paste(unique_types, collapse = ", ")),
                             type = "warning", duration = NULL)
          
          # Validate design
          meta_for_design <- data.frame(
            sample_id = metadata$sample_id,
            condition = metadata$condition,
            batch     = metadata$batch,
            filename  = metadata$filename,
            stringsAsFactors = FALSE, check.names = FALSE
          )
          validation <- .validate_design(counts_mat, meta_for_design)
          
          if (!validation$ok) stop(paste(validation$messages, collapse = " | "))
          
          # Align metadata rownames to match existing bulk_obj structure
          aligned_meta <- as.data.frame(validation$metadata, stringsAsFactors = FALSE)
          rownames(aligned_meta) <- aligned_meta$sample_id
          
          # Add any extra columns from rv_ps$metadata (e.g. GEO-enriched)
          extra_cols <- setdiff(names(metadata), names(aligned_meta))
          if (length(extra_cols)) {
            idx <- match(aligned_meta$sample_id, metadata$sample_id)
            for (cc in extra_cols) aligned_meta[[cc]] <- metadata[[cc]][idx]
          }
          
          list(
            counts_mat    = counts_mat,
            metadata      = aligned_meta,
            id_type       = merged_id_type,
            de_ok         = validation$de_ok,
            val_messages  = validation$messages
          )
        }, error = function(e) {
          removeNotification(id = ns("progress"))
          add_log(paste("❌", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = NULL)
          NULL
        })
        
        if (is.null(result)) { temp_data$is_loaded <- FALSE; return(NULL) }
        
        # ── Commit to global_data (same structure as merged_matrix) ────────
        global_data$bulk_obj <- list(
          counts    = result$counts_mat,
          metadata  = result$metadata,
          project   = input$project_name,
          type      = "bulk",
          timestamp = Sys.time(),
          # Extra slots used by downstream modules
          gene_id_type  = result$id_type,
          de_allowed    = result$de_ok,
          import_mode   = "per_sample"
        )
        
        temp_data$is_loaded <- TRUE
        removeNotification(id = ns("progress"))
        add_log(paste("✅ Import multi-fichiers réussi —",
                      nrow(result$counts_mat), "gènes ×",
                      ncol(result$counts_mat), "échantillons"))
        
        if (length(result$val_messages)) {
          showNotification(paste(result$val_messages, collapse = " | "),
                           type = "warning", duration = 8)
        } else {
          showNotification(
            paste("✅ Import réussi:", ncol(result$counts_mat),
                  "échantillons,", nrow(result$counts_mat), "gènes"),
            type = "message", duration = 5)
        }
      }
    })
    
    # ── Outputs info (inchangés) ───────────────────────────────────────────
    output$nb_samples <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) "-"
      else ncol(global_data$bulk_obj$counts)
    })
    output$nb_genes <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) "-"
      else nrow(global_data$bulk_obj$counts)
    })
    output$nb_metadata_vars <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$metadata)) "-"
      else ncol(global_data$bulk_obj$metadata)
    })
    output$status_obj <- renderText({
      if (is.null(global_data$bulk_obj)) "⚪ Inactif"
      else paste("🟢 Chargé (", format(global_data$bulk_obj$timestamp, "%H:%M:%S"), ")")
    })
    output$console_log <- renderText({ logs() })
  })
}