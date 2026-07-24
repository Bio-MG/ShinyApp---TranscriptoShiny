# =============================================================================
# modules/import/mod_import_spatial.R — Spatial Import (Visium / Xenium / CosMx)
# =============================================================================
# v2 (vignette coverage — Phase 3): added the sketch normalization choice
# (LogNormalize / SCTransform, opt-in) — see R/utils_spatial_io.R::build_sketch()
# for where this is actually applied (bounded to the <= max_sketch cells,
# never the full disk-backed dataset).
#
# Loads raw spatial data then immediately hands off to
# R/utils_spatial_io.R::convert_to_bpcells_and_fov() so global_data$spatial_obj
# is ALWAYS the lightweight list contract (sketch + bpcells_dir + coords),
# never a raw in-RAM Seurat object — see utils_spatial_io.R header.
#
# Visium reuses the existing helpers_io.R::load_spatial_visium() (QC-filtered
# on load, same as before this module existed) — "reutiliser les modules
# existants" project rule. Xenium/CosMx use Seurat's own loaders.
#
# Import itself stays synchronous (withProgress spinner, like
# mod_import_sc.R) — only the heavy downstream analyses (clustering,
# deconvolution, Moran's I) go through mirai/ExtendedTask. SCTransform (when
# selected) also runs synchronously here, on the sketch only — see the
# warning shown in the UI when it's selected. Revisit as an async import if
# raw datasets grow large enough to make this UI-blocking regardless
# (evolutivity hook, unchanged from v1).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── UI ────────────────────────────────────────────────────────────────────
mod_import_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = "Import Spatial",

        radioButtons(ns("technology"), "Technologie",
                     choices = c("Visium (spots)" = "visium",
                                 "Xenium (subcellulaire)" = "xenium",
                                 "CosMx (subcellulaire)" = "cosmx"),
                     selected = "visium"),

        div(class = "alert alert-light", style = "font-size:0.8rem;",
            bsicons::bs_icon("lightbulb"),
            " Selectionnez le dossier racine contenant les fichiers bruts ",
            "(ex: dossier 'outs' pour Visium/Xenium 10X, ou dossier CosMx AtoMx)."),

        textInput(ns("sample_name"), "Nom de l'echantillon", placeholder = "Ex: Tumor_slice1"),

        shinyFiles::shinyDirButton(ns("dir_select"), "\U1F4C1 Choisir le dossier",
                                    "Selectionner le dossier de donnees spatiales",
                                    class = "btn-secondary w-100", icon = icon("folder-open")),
        verbatimTextOutput(ns("path_display"), placeholder = TRUE),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'visium'", ns("technology")),
          hr(),
          numericInput(ns("min_counts"), "nCount_Spatial minimum", 100, min = 0, step = 10),
          numericInput(ns("min_features"), "nFeature_Spatial minimum", 200, min = 0, step = 10)
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] != 'visium'", ns("technology")),
          hr(),
          sliderInput(ns("simplify_tol"), "Tolerance de simplification des polygones",
                      1, 100, 20, step = 1)
        ),

        hr(),
        numericInput(ns("max_sketch"), "Taille max. du sketch (RAM)",
                     50000, min = 5000, max = 100000, step = 5000),

        radioButtons(ns("norm_method"), "Normalisation du sketch",
                     choices = c("LogNormalize (rapide, defaut)" = "lognorm",
                                 "SCTransform (vignette Seurat, plus lourd)" = "sct"),
                     selected = "lognorm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sct'", ns("norm_method")),
          div(class = "alert alert-warning", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-triangle"),
              " SCTransform est significativement plus lourd que LogNormalize et s'execute ",
              "de maniere SYNCHRONE pendant l'import (pas de mirai a cette etape) — reduisez ",
              "la taille du sketch ci-dessus (ex: 10 000-20 000) si l'import devient trop long. ",
              "N'affecte que le sketch (visualisation gene/UMAP) : le clustering spatial ",
              "(BANKSY-lite) et l'indice de Moran restent en LogNormalize rapide sur les ",
              "donnees completes, inchanges.")
        ),

        actionButton(ns("btn_import"), "\U1F680 Importer + convertir (BPCells)",
                     class = "btn-success w-100 mt-2", icon = icon("play"))
      ),

      card(
        card_header("Resume de l'objet spatial charge"),
        layout_columns(
          value_box(title = "Spots / Cellules (total disque)", value = textOutput(ns("nb_total")),
                     showcase = bsicons::bs_icon("grid-3x3"), theme = "primary"),
          value_box(title = "Sketch (RAM)", value = textOutput(ns("nb_sketch")),
                     showcase = bsicons::bs_icon("cpu"), theme = "secondary"),
          value_box(title = "Genes", value = textOutput(ns("nb_genes")),
                     showcase = bsicons::bs_icon("diagram-3"), theme = "info"),
          value_box(title = "Statut", value = textOutput(ns("status_obj")),
                     showcase = bsicons::bs_icon("check-circle"), theme = "light")
        ),
        card_body(h5("Console de Log", class = "text-muted"),
                  verbatimTextOutput(ns("console_log"), placeholder = TRUE))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────
mod_import_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) logs(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n", logs()))

    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "dir_select", roots = volumes, session = session)

    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      path <- shinyFiles::parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) { dir_path(path); add_log(paste("Dossier:", path)) }
    })
    output$path_display <- renderText({
      if (is.null(dir_path())) "Aucun dossier selectionne" else dir_path()
    })

    observeEvent(input$btn_import, {
      req(dir_path())
      sample_name <- if (nchar(trimws(input$sample_name))) trimws(input$sample_name) else basename(dir_path())

      withProgress(message = "Import spatial...", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Lecture des fichiers bruts...")
          raw_obj <- switch(input$technology,
            "visium" = load_spatial_visium(dir_path(), sample_name = sample_name,
                                            min_counts = input$min_counts,
                                            min_features = input$min_features),
            "xenium" = Seurat::LoadXenium(dir_path(), fov = "fov"),
            "cosmx"  = Seurat::LoadNanostring(dir_path(), fov = "fov", assay = "Nanostring"),
            stop("Technologie inconnue.")
          )
          add_log(sprintf("  ✓ Objet brut charge : %d genes x %d %s",
                           nrow(raw_obj), ncol(raw_obj),
                           if (input$technology == "visium") "spots" else "cellules"))

          incProgress(0.3, detail = "Conversion BPCells (disque)...")
          norm_label <- if (input$norm_method == "sct") "SCTransform" else "LogNormalize"
          add_log(sprintf("  Normalisation du sketch : %s", norm_label))
          if (input$norm_method == "sct") {
            incProgress(0.1, detail = "SCTransform sur le sketch (synchrone, peut prendre du temps)...")
          }
          spatial_pkg <- convert_to_bpcells_and_fov(
            raw_obj, dataset_id = sample_name, technology = input$technology,
            simplify_tol = input$simplify_tol %||% 20,
            max_sketch = input$max_sketch,
            norm_method = input$norm_method
          )
          add_log(sprintf("  ✓ BPCells: %s", spatial_pkg$bpcells_dir))
          add_log(sprintf("  ✓ Sketch RAM: %d/%d elements (normalisation: %s)",
                           ncol(spatial_pkg$sketch), spatial_pkg$n_total, norm_label))

          incProgress(0.9, detail = "Finalisation...")
          global_data$spatial_obj <- spatial_pkg

          add_log(sprintf("✅ Import termine : %s (%s)", sample_name, input$technology))
          showNotification(sprintf("✅ Import spatial reussi : %d elements (%d en sketch RAM, %s)",
                                    spatial_pkg$n_total, ncol(spatial_pkg$sketch), norm_label),
                            type = "message", duration = 5)
        }, error = function(e) {
          msg <- paste("❌ Erreur import spatial:", conditionMessage(e))
          add_log(msg); showNotification(msg, type = "error", duration = 10)
        })
      })
    })

    # ── Outputs ────────────────────────────────────────────────────────
    output$nb_total  <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(global_data$spatial_obj$n_total, big.mark = ",") })
    output$nb_sketch  <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(ncol(global_data$spatial_obj$sketch), big.mark = ",") })
    output$nb_genes   <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(nrow(global_data$spatial_obj$sketch), big.mark = ",") })
    output$status_obj <- renderText({
      if (is.null(global_data$spatial_obj)) "⚪ Inactif"
      else {
        # DefaultAssay of the sketch tells us which normalization actually
        # ended up being used (SCTransform can silently fall back to
        # LogNormalize on failure — see build_sketch()) — reflect the truth,
        # not just what was requested.
        norm_used  <- tryCatch(Seurat::DefaultAssay(global_data$spatial_obj$sketch), error = function(e) NA)
        norm_label <- if (identical(norm_used, "SCT")) "SCT" else "LogNorm"
        paste0("🟢 ", global_data$spatial_obj$technology, " (", norm_label, ")")
      }
    })
    output$console_log <- renderText({ logs() })
  })
}
