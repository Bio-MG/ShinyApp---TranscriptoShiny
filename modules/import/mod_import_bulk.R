# modules/mod_import_bulk.R - Import RNA Bulk
# v2: bouton de chargement persistant (hors accordion) + inférence de
#     métadonnées depuis les noms d'échantillons + détection du type
#     d'identifiant de gène (oriente vers l'étape "0. Mapping IDs" du module
#     d'analyse si nécessaire).
#
# Depends on global.R: infer_metadata_from_names(), preview_metadata_split(),
#                       detect_gene_id_type()

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

        accordion(
          accordion_panel(
            "1. Matrice de Counts",
            icon = icon("table"),

            fileInput(ns("counts_file"), "Fichier de Counts (CSV/TSV/TXT)",
                      accept = c(".csv", ".tsv", ".txt", ".xlsx")),

            helpText("Format attendu : Lignes = Gènes, Colonnes = Échantillons. ",
                     "Un filtrage fin (par réplicat) est disponible à l'étape 1 du module d'analyse — ",
                     "le seuil ci-dessous (Options d'Import) n'est qu'un pré-nettoyage grossier."),

            radioButtons(ns("counts_format"), "Format de la matrice",
                         choices = c("Genes en lignes (standard)" = "rows",
                                     "Genes en colonnes (transposé)" = "cols"),
                         selected = "rows"),

            checkboxInput(ns("counts_has_header"), "La 1ère ligne est un en-tête", value = TRUE),
            checkboxInput(ns("counts_has_rownames"), "La 1ère colonne est le nom des gènes", value = TRUE),

            h6("Aperçu de la matrice:", style = "font-weight: bold; margin-top: 10px;"),
            div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                tableOutput(ns("counts_preview"))
            ),

            uiOutput(ns("detected_gene_id_banner"))
          ),

          accordion_panel(
            "2. Métadonnées (Optionnel)",
            icon = icon("tags"),

            fileInput(ns("metadata_file"), "Fichier de Métadonnées (CSV/TSV/TXT)",
                      accept = c(".csv", ".tsv", ".txt", ".xlsx")),

            helpText("Format : Lignes = Échantillons, Colonnes = Variables (condition, batch, etc.)."),

            checkboxInput(ns("metadata_has_header"), "La 1ère ligne est un en-tête", value = TRUE),
            checkboxInput(ns("metadata_has_rownames"), "La 1ère colonne est le nom des échantillons", value = TRUE),

            h6("Aperçu des métadonnées actives:", style = "font-weight: bold; margin-top: 10px;"),
            div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                tableOutput(ns("metadata_preview"))
            ),

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
          ),

          accordion_panel(
            "3. Options d'Import",
            icon = icon("cogs"),

            textInput(ns("project_name"), "Nom du Projet",
                      value = "BulkRNA_Project", placeholder = "Ex: Study_2024"),

            numericInput(ns("min_counts"), "Counts minimum par gène (pré-filtre grossier)",
                         value = 10, min = 0, step = 1),

            helpText("Les gènes avec moins de counts seront filtrés dès l'import.")
          )
        ),

        # ── Bouton de chargement PERSISTANT — toujours visible, peu importe
        #    le panneau d'accordion ouvert (auparavant enterré dans "3. Options
        #    d'Import", invisible si l'utilisateur n'avait pas déplié ce panneau) ──
        hr(),
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
          value_box(
            title = "Échantillons",
            value = textOutput(ns("nb_samples")),
            showcase = bsicons::bs_icon("grid-3x3"),
            theme = "primary"
          ),
          value_box(
            title = "Gènes",
            value = textOutput(ns("nb_genes")),
            showcase = bsicons::bs_icon("diagram-3"),
            theme = "secondary"
          ),
          value_box(
            title = "Variables Métadonnées",
            value = textOutput(ns("nb_metadata_vars")),
            showcase = bsicons::bs_icon("tags"),
            theme = "info"
          ),
          value_box(
            title = "Statut",
            value = textOutput(ns("status_obj")),
            showcase = bsicons::bs_icon("check-circle"),
            theme = "light"
          )
        ),
        card_body(
          h5("Console de Log", class = "text-muted"),
          verbatimTextOutput(ns("console_log"), placeholder = TRUE)
        )
      )
    )
  )
}

mod_import_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Logger avec initialisation explicite
    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      old_logs <- isolate(logs())
      logs(paste0("[", timestamp, "] ", msg, "\n", old_logs))
    }

    temp_data <- reactiveValues(is_loaded = FALSE)

    # ── Metadata auto-inference state (Step "Pas de fichier ?") ─────────────
    infer_preview_rv  <- reactiveVal(NULL)   # output of preview_metadata_split()
    inferred_metadata <- reactiveVal(NULL)   # output of infer_metadata_from_names()

    # --- FONCTION DE LECTURE ROBUSTE ---
    smart_read <- function(filepath, has_header = TRUE, has_rownames = TRUE) {
      ext <- tolower(tools::file_ext(filepath))

      tryCatch({
        df <- switch(
          ext,
          "csv" = read.csv(filepath, header = has_header,
                           row.names = if (has_rownames) 1 else NULL,
                           check.names = FALSE, stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8-BOM"),
          "tsv" = read.delim(filepath, header = has_header,
                             row.names = if (has_rownames) 1 else NULL,
                             sep = "\t", check.names = FALSE,
                             stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8-BOM"),
          "txt" = read.delim(filepath, header = has_header,
                             row.names = if (has_rownames) 1 else NULL,
                             check.names = FALSE,
                             stringsAsFactors = FALSE,
                             fileEncoding = "UTF-8-BOM"),
          "xlsx" = {
            if (!requireNamespace("readxl", quietly = TRUE)) {
              stop("Package 'readxl' nécessaire pour lire les fichiers .xlsx")
            }
            tmp <- readxl::read_excel(filepath, col_names = has_header)
            tmp <- as.data.frame(tmp, check.names = FALSE, stringsAsFactors = FALSE)
            if (has_rownames) {
              rownames(tmp) <- tmp[[1]]
              tmp <- tmp[, -1, drop = FALSE]
            }
            tmp
          },
          stop("Format de fichier non supporté")
        )

        if (!is.null(colnames(df))) colnames(df) <- trimws(sub("^\ufeff", "", colnames(df)))
        if (!is.null(rownames(df))) rownames(df) <- trimws(sub("^\ufeff", "", rownames(df)))

        df
      }, error = function(e) {
        stop(paste("Erreur de lecture:", e$message))
      })
    }

    # --- REACTIVES POUR LES DONNÉES ---
    counts_reactive <- reactive({
      req(input$counts_file)

      tryCatch({
        add_log("📂 Lecture du fichier de counts...")

        df <- smart_read(input$counts_file$datapath,
                         input$counts_has_header,
                         input$counts_has_rownames)

        if (input$counts_format == "cols") {
          df <- as.data.frame(t(df))
          add_log("  ↻ Matrice transposée (genes étaient en colonnes)")
        }

        if (nrow(df) == 0 || ncol(df) == 0) {
          stop("Matrice vide après lecture. Vérifie les options d'en-tête/rownames.")
        }

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
                         input$metadata_has_header,
                         input$metadata_has_rownames)

        add_log(paste("✓ Métadonnées chargées:", nrow(df), "échantillons ×", ncol(df), "variables"))
        df

      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture métadonnées:", e$message))
        showNotification(paste("Erreur métadonnées:", e$message), type = "warning", duration = 10)
        NULL
      })
    })

    # ── Effective metadata: uploaded file takes priority over inference ──────
    effective_metadata <- reactive({
      metadata_file_reactive() %||% inferred_metadata()
    })

    # ── Gray-out the inference block while a metadata file is active ────────
    observe({
      shinyjs::toggleState("btn_infer_preview", condition = is.null(metadata_file_reactive()))
      shinyjs::toggleState("btn_infer_apply",   condition = is.null(metadata_file_reactive()))
    })

    # =========================================================================
    # METADATA AUTO-INFERENCE
    # =========================================================================
    observeEvent(input$btn_infer_preview, {
      req(counts_reactive())
      samples <- colnames(counts_reactive())

      preview <- tryCatch(
        preview_metadata_split(samples, delimiter = input$infer_delimiter),
        error = function(e) {
          showNotification(paste("Erreur découpage:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )
      infer_preview_rv(preview)
    })

    output$infer_preview_ui <- renderUI({
      preview <- infer_preview_rv()
      if (is.null(preview)) return(NULL)

      seg_mat <- do.call(rbind, lapply(preview$segments, function(s) {
        length(s) <- max(preview$n_seg); s
      }))
      rownames(seg_mat) <- NULL

      tagList(
        div(class = if (preview$n_seg_consistent) "alert alert-success" else "alert alert-warning",
            style = "font-size:0.78em;padding:6px;margin-top:6px;",
            if (preview$n_seg_consistent) {
              sprintf("✓ Découpage cohérent : %d segment(s) par échantillon.", preview$n_seg[1])
            } else {
              sprintf("⚠️ Nombre de segments incohérent (min=%d, max=%d) — les segments en trop seront tronqués.",
                      min(preview$n_seg), max(preview$n_seg))
            }),
        renderTable(as.data.frame(seg_mat, stringsAsFactors = FALSE))()
      )
    })

    output$infer_colnames_ui <- renderUI({
      preview <- infer_preview_rv()
      req(preview)
      n_seg <- if (preview$n_seg_consistent) preview$n_seg[1] else min(preview$n_seg)

      default_names <- if (n_seg == 2) c("condition", "replicate")
                       else paste0("segment_", seq_len(n_seg))

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
        if (is.null(v) || nchar(trimws(v)) == 0) paste0("segment_", i) else trimws(v)
      }, character(1))

      if (length(unique(col_names)) != length(col_names)) {
        showNotification("⚠️ Les noms de colonnes doivent être uniques.", type = "warning", duration = 6)
        return()
      }

      result <- withCallingHandlers(
        tryCatch(
          infer_metadata_from_names(samples, delimiter = input$infer_delimiter, col_names = col_names),
          error = function(e) {
            showNotification(paste("Erreur:", conditionMessage(e)), type = "error", duration = 8)
            NULL
          }
        ),
        warning = function(w) {
          showNotification(paste("⚠️", conditionMessage(w)), type = "warning", duration = 8)
          invokeRestart("muffleWarning")
        }
      )
      req(result)

      inferred_metadata(result)
      add_log(paste("✓ Métadonnées inférées depuis les noms d'échantillons:", paste(col_names, collapse = ", ")))
      showNotification("✓ Métadonnées inférées appliquées — visibles dans l'aperçu ci-dessus.",
                       type = "message", duration = 5)
    })

    # ── Reset inference whenever the counts file changes (sample names may differ) ──
    observeEvent(input$counts_file, {
      infer_preview_rv(NULL)
      inferred_metadata(NULL)
      temp_data$is_loaded <- FALSE
    })
    observeEvent(input$metadata_file, {
      temp_data$is_loaded <- FALSE
    })

    # =========================================================================
    # GENE-ID TYPE DETECTION (informational — actual mapping happens in the
    # analysis module's "0. Mapping IDs" step, on the committed bulk_obj$counts)
    # =========================================================================
    output$detected_gene_id_banner <- renderUI({
      df <- counts_reactive()
      req(df)
      detected <- tryCatch(detect_gene_id_type(rownames(df)), error = function(e) "unknown")
      if (detected %in% c("symbol", "unknown")) return(NULL)

      label <- switch(detected, ensembl = "Ensembl Gene ID", entrez = "Entrez ID",
                      affy_probe = "Probe Affymetrix", detected)
      div(class = "alert alert-light", style = "font-size:0.78em;border-left:3px solid #F39C12;margin-top:6px;",
          bsicons::bs_icon("lightbulb"),
          sprintf(" Identifiants détectés : %s (pas des symboles). Convertissez-les via ",
                  label),
          tags$strong("'0. Mapping IDs'"), " dans le module d'analyse RNA Bulk avant l'enrichissement de voies.")
    })

    # --- APERÇUS ---
    output$counts_preview <- renderTable({
      df <- counts_reactive()
      if (is.null(df)) return(NULL)
      head(df[, 1:min(5, ncol(df)), drop = FALSE], 10)
    }, rownames = TRUE)

    output$metadata_preview <- renderTable({
      df <- effective_metadata()
      if (is.null(df)) return(NULL)
      head(df, 10)
    }, rownames = TRUE)

    # --- READINESS BANNER (toujours visible, au-dessus du bouton persistant) ---
    output$readiness_banner <- renderUI({
      counts_ok <- !is.null(counts_reactive())
      meta_ok   <- !is.null(effective_metadata())
      div(style = "font-size:0.8em;",
          if (counts_ok) span("✅ Counts chargés", style = "color:#18BC9C;font-weight:bold;")
          else span("⚪ Counts manquants", style = "color:#999;"),
          " · ",
          if (meta_ok) span("✅ Métadonnées prêtes", style = "color:#18BC9C;font-weight:bold;")
          else span("⚪ Métadonnées (optionnel)", style = "color:#999;"))
    })

    # --- GESTION DU BOUTON DE CHARGEMENT ---
    observe({
      shinyjs::toggleState("btn_load", condition = !is.null(counts_reactive()))
      if (isTRUE(temp_data$is_loaded)) {
        shinyjs::html("btn_load", html = "🔄 Mettre à jour les Données")
      } else {
        shinyjs::html("btn_load", html = "🚀 Charger les Données")
      }
    })

    # --- CHARGEMENT FINAL DES DONNÉES ---
    observeEvent(input$btn_load, {
      req(counts_reactive())

      add_log("🔄 Préparation de l'objet RNA Bulk...")
      showNotification("Traitement des données...", type = "message", duration = NULL, id = ns("progress"))

      tryCatch({
        counts   <- counts_reactive()
        metadata <- effective_metadata()

        add_log("  → Conversion des valeurs en numérique...")
        counts <- as.data.frame(counts, check.names = FALSE, stringsAsFactors = FALSE)
        for (col in colnames(counts)) {
          if (is.character(counts[[col]])) {
            counts[[col]] <- suppressWarnings(as.numeric(counts[[col]]))
          }
        }
        if (anyNA(as.matrix(counts))) {
          add_log("  ⚠️ Certaines valeurs ne sont pas numériques (NA après conversion)")
        }

        counts_matrix <- as.matrix(counts)

        gene_counts <- rowSums(counts_matrix, na.rm = TRUE)
        keep_genes  <- gene_counts >= input$min_counts
        counts_matrix <- counts_matrix[keep_genes, , drop = FALSE]
        add_log(paste("  → Gènes filtrés:", sum(!keep_genes), "retirés,", sum(keep_genes), "conservés"))

        bulk_obj <- list(
          counts    = counts_matrix,
          metadata  = NULL,
          project   = input$project_name,
          type      = "bulk",
          timestamp = Sys.time()
        )

        if (!is.null(metadata)) {
          sample_names   <- colnames(counts_matrix)
          metadata_names <- rownames(metadata)

          if (all(sample_names %in% metadata_names)) {
            bulk_obj$metadata <- metadata[sample_names, , drop = FALSE]
            add_log(paste("  ✓ Métadonnées alignées:", ncol(bulk_obj$metadata), "variables"))
          } else {
            bulk_obj$metadata <- data.frame(sample = sample_names, row.names = sample_names)
            add_log("  ⚠️ Métadonnées non alignées - création de métadonnées par défaut")
          }
        } else {
          bulk_obj$metadata <- data.frame(sample = colnames(counts_matrix), row.names = colnames(counts_matrix))
          add_log("  → Aucune métadonnée fournie/inférée — colonne 'sample' par défaut créée")
        }

        global_data$bulk_obj <- bulk_obj
        temp_data$is_loaded  <- TRUE

        add_log(paste("✅ Import réussi!", nrow(counts_matrix), "gènes ×", ncol(counts_matrix), "échantillons"))
        removeNotification(id = ns("progress"))
        showNotification(
          paste("✅ Import réussi:", ncol(counts_matrix), "échantillons,", nrow(counts_matrix), "gènes"),
          type = "message", duration = 5
        )

      }, error = function(e) {
        removeNotification(id = ns("progress"))
        add_log(paste("❌ Erreur lors du chargement:", e$message))
        showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
        temp_data$is_loaded <- FALSE
      })
    })

    # --- OUTPUTS INFO ---
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
