# mod_spatial_viz.R
# Module d'exploration spatiale avec fond histologique multi-résolution
#
# v2 (audit step 3.9c): "Carte spatiale" tab now shows the interactive
# Plotly map and the static PNG-preview plot SIDE BY SIDE (layout_columns,
# same pattern as the "Vue combinee" tab) instead of stacked, and the
# preview defaults to VISIBLE (was hidden behind an unchecked checkbox) —
# fixes "le plot de previsualisation PNG ne s'affiche pas a cote du cluster
# spatial plotly pour comparer". The checkbox is kept so people with very
# large sketches can hide it again to save render time.

mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Visualisation", width = 320,
      
      uiOutput(ns("sketch_norm_status_ui")),
      
      selectInput(
        ns("color_by"), "Colorer par",
        choices = c(
          "Metrique QC" = "qc",
          "Cluster spatial" = "cluster",
          "Type cellulaire (deconvolution)" = "deconv",
          "Niche spatiale" = "niche",
          "Gene" = "gene"
        ),
        selected = "qc"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'qc'", ns("color_by")),
        selectInput(
          ns("qc_metric"), NULL,
          choices = c("nCount", "nFeature", "pct_mt", "pct_ribo", "log_nCount")
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        selectizeInput(
          ns("gene"), NULL, choices = NULL,
          options = list(
            maxOptions = 3000,
            placeholder = "Rechercher un gene..."
          )
        ),
        # NEW: quick-pick from the Moran's I results (onglet "1. QC &
        # Autocorrelation" > "Genes spatialement variables") -- the small
        # facet grid there is useful to scan many genes at once, but picking
        # ONE gene to inspect full-size/interactively belongs here. Purely
        # a convenience selectInput that feeds the same `gene` field above;
        # NULL/hidden until Moran's I has been run at least once.
        uiOutput(ns("moran_quickpick_ui"))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        checkboxInput(
          ns("scale_alpha_by_expr"),
          "Opacite proportionnelle a l'expression (SpatialFeaturePlot)",
          value = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("scale_alpha_by_expr")),
          sliderInput(
            ns("alpha_range"), "Plage d'opacite (min-max)",
            0, 1, c(0.15, 1), step = 0.05
          )
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'deconv'", ns("color_by")),
        uiOutput(ns("deconv_celltype_ui"))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'cluster'", ns("color_by")),
        checkboxInput(
          ns("show_cluster_labels"),
          "Afficher les labels de cluster sur la carte (vignette: DimPlot label=TRUE)",
          value = FALSE
        )
      ),
      
      sliderInput(ns("pt_radius"), "Taille des points", 1, 20, 6, step = 1),
      sliderInput(ns("pt_opacity"), "Opacite des points (hors mode Gene)", 0.1, 1, 0.85, step = 0.05),
      
      hr(),
      
      checkboxInput(ns("show_histology"), "Afficher l'image histologique (fond de coupe)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_histology")),
        selectInput(
          ns("histology_resolution"), "Résolution du fond",
          choices = NULL,          # sera mis à jour dynamiquement côté serveur
          selected = NULL
        ),
        sliderInput(ns("histology_opacity"), "Opacité de l'image", 0, 1, 0.7, step = 0.05)
      ),
      
      uiOutput(ns("histology_status_ui")),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_histology")),
        downloadButton(
          ns("dl_histology_raw"), "\U0001F41B Fond brut (debug PNG)",
          class = "btn-sm btn-outline-secondary w-100 mb-1"
        )
      ),
      
      div(
        class = "border-top pt-2 mt-2",
        checkboxInput(
          ns("show_static_export_preview"),
          "Afficher l'apercu PNG (a cote de la carte interactive)",
          value = TRUE
        ),
        div(
          class = "text-muted mb-2",
          style = "font-size: 0.7rem;",
          "Meme moteur que le bouton Export PNG ci-dessous. Decochez pour donner ",
          "toute la largeur a la carte interactive Plotly."
        ),
        downloadButton(ns("dl_png"), "Export PNG", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_csv"), "Export CSV (donnees affichees)", class = "btn-sm btn-outline-secondary w-100")
      ),

      # Feedback biologiste ("ajouter la fonction d'ajout au rapport") :
      # sauvegarde la vue ACTUELLE (mode de coloration + parametres) pour
      # qu'elle apparaisse dans l'onglet "7. Export & Rapport" > "Rapport
      # HTML/PDF" > section "Visualisations sauvegardees", MEME apres avoir
      # change d'onglet ou d'echantillon (shared_rv$saved_viz_list est
      # cacheable comme le reste, voir mod_spatial.R). Seule la CONFIG est
      # sauvegardee (pas une image) -- reconstruite a la volee au moment du
      # rendu du rapport via build_saved_viz_df() (R/utils_spatial_report.R),
      # donc toujours a jour avec les derniers calculs si vous relancez une
      # etape ensuite.
      div(
        class = "border-top pt-2 mt-2",
        h6("Ajouter au rapport", style = "font-weight:bold; font-size:0.85rem;"),
        div(class = "text-muted", style = "font-size:0.7rem;",
            "Sauvegarde la vue ACTUELLE (coloration + parametres ci-dessus) pour l'onglet ",
            "\"7. Export & Rapport\", meme apres avoir change d'onglet ou d'echantillon."),
        textInput(ns("viz_save_label"), NULL, placeholder = "Nom de cette vue (ex: Cluster 3 vs stroma)"),
        actionButton(ns("btn_add_to_report"), "\u2795 Ajouter cette vue au rapport",
                     class = "btn-sm btn-outline-primary w-100"),
        uiOutput(ns("saved_viz_list_ui"))
      ),
      
      hr(),
      # v4 (audit step 3.12): DEBUG-ONLY tool, collapsed by default. The
      # real root cause turned out to be the LOWRES background itself
      # (undersized/misaligned depending on Seurat version -- see
      # R/utils_spatial_io.R::extract_histology_image()), now fixed by
      # reading tissue_hires_image.png + scalefactors_json.json directly
      # from disk and defaulting to it. This toggle is kept only as an
      # escape hatch for a dataset/version where that still isn't enough --
      # it is NOT a permanent correction and should not need to be used in
      # normal operation. Scoped to the Plotly-rendering outputs only
      # (spatial_preview_plot, combined_spatial_plot, roi_zoom_plot,
      # roi_context_plot); PNG export, BANKSY clustering and Moran's I are
      # never affected by it.
      tags$details(
        tags$summary(style = "cursor:pointer; font-size:0.72rem; color:#888;",
                     "Outil de diagnostic (debug) : orientation Plotly"),
        div(class = "mt-1",
            checkboxInput(
              ns("plotly_orient_fix"),
              "Corriger l'orientation dans la vue interactive Plotly",
              value = FALSE
            ),
            div(class = "text-muted", style = "font-size:0.65rem;",
                "A utiliser seulement si le fond hires choisi ci-dessus semble encore tourne. ",
                "N'affecte JAMAIS l'export PNG ni le clustering/Moran.")
        )
      ),
      
      hr(),
      bslib::input_task_button(
        ns("btn_compute_umap"),
        "Calculer PCA + UMAP (sketch)",
        icon = icon("chart-line")
      ),
      verbatimTextOutput(ns("umap_progress_text"), placeholder = TRUE)
    ),
    
    navset_card_underline(
      nav_panel(
        "Carte spatiale",
        uiOutput(ns("carte_spatiale_layout_ui"))
      ),
      
      nav_panel(
        "UMAP (non-spatial, sketch)",
        card(
          full_screen = TRUE,
          style = "min-height: 78vh;",
          plotly::plotlyOutput(
            ns("umap_plot"),
            height = "calc(100vh - 220px)"
          )
        )
      ),
      
      nav_panel(
        "Vue combinee (mode expert)",
        div(
          class = "alert alert-light mb-2",
          style = "font-size:0.82rem;",
          uiOutput(ns("combined_help_ui")),
          uiOutput(ns("highlight_debug_ui"))
        ),
        div(
          class = "d-flex flex-wrap gap-2 align-items-end mb-3",
          style = "row-gap: 0.5rem;",
          div(
            style = "min-width: 320px; flex: 1 1 320px;",
            selectizeInput(
              ns("highlight_clusters"),
              "Isoler cluster(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Tous les clusters",
                plugins = list("remove_button")
              )
            )
          ),
          div(
            style = "min-width: 220px;",
            actionButton(
              ns("btn_clear_selection"),
              "Effacer la selection lasso",
              class = "btn btn-outline-secondary"
            )
          ),
          div(
            style = "min-width: 220px;",
            actionButton(
              ns("btn_isolate_roi"),
              "\U0001F50D Isoler cette selection (ROI)",
              class = "btn btn-primary"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header("Carte spatiale liee"),
            plotly::plotlyOutput(
              ns("combined_spatial_plot"),
              height = "72vh"
            )
          ),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header("UMAP lie"),
            plotly::plotlyOutput(
              ns("combined_umap_plot"),
              height = "72vh"
            )
          )
        )
      ),
      
      nav_panel(
        "5. ROI isolee",
        
        div(
          class = "mb-3",
          uiOutput(ns("roi_summary_ui"))
        ),
        
        div(
          class = "container-fluid px-0",
          
          div(
            class = "row g-3 align-items-stretch mb-3",
            
            div(
              class = "col-12 col-xl-6 d-flex",
              card(
                full_screen = TRUE,
                class = "w-100",
                style = "height: min(68vh, 720px); min-height: 520px;",
                card_header("Vue zoomee (ROI)"),
                card_body(
                  class = "p-0",
                  style = "height: calc(min(68vh, 720px) - 52px); min-height: 468px; overflow: hidden;",
                  plotly::plotlyOutput(
                    ns("roi_zoom_plot"),
                    height = "100%"
                  )
                )
              )
            ),
            
            div(
              class = "col-12 col-xl-6 d-flex",
              card(
                full_screen = TRUE,
                class = "w-100",
                style = "height: min(68vh, 720px); min-height: 520px;",
                card_header("Contexte (carte complete)"),
                card_body(
                  class = "p-0",
                  style = "height: calc(min(68vh, 720px) - 52px); min-height: 468px; overflow: hidden;",
                  plotly::plotlyOutput(
                    ns("roi_context_plot"),
                    height = "100%"
                  )
                )
              )
            )
          ),
          
          div(
            class = "row g-3",
            div(
              class = "col-12",
              card(
                card_header("Marqueurs differentiels (ROI vs Reste)"),
                card_body(
                  div(
                    class = "alert alert-light small mb-3",
                    bsicons::bs_icon("info-circle"),
                    " Selectionnez une ROI depuis la Vue combinee, puis lancez le test ",
                    "Wilcoxon asynchrone contre le reste des spots."
                  ),
                  
                  div(
                    class = "d-flex flex-wrap align-items-center gap-2 mb-3",
                    bslib::input_task_button(
                      ns("btn_roi_markers"),
                      "Chercher les marqueurs",
                      icon = icon("magnifying-glass-chart")
                    ),
                    downloadButton(
                      ns("dl_roi_object"),
                      "\U0001F4E5 Telecharger le sous-objet (.rds)",
                      class = "btn-outline-secondary"
                    ),
                    actionButton(
                      ns("btn_clear_roi"),
                      "Vider la ROI",
                      class = "btn-outline-danger"
                    )
                  ),
                  
                  div(
                    class = "bg-light border rounded p-2 mb-3",
                    style = "max-height: 110px; overflow-y: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.75rem;",
                    verbatimTextOutput(
                      ns("roi_markers_progress_text"),
                      placeholder = TRUE
                    )
                  ),
                  
                  DT::DTOutput(ns("roi_markers_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_spatial_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # Helper pour extraire les résolutions disponibles
    # --------------------------------------------------------------------------
    get_available_resolutions <- function(hist_data) {
      if (is.null(hist_data)) return(character(0))
      
      # Cas 1 : structure avec une liste "images"
      if (!is.null(hist_data$images) && is.list(hist_data$images)) {
        return(names(hist_data$images))
      }
      
      # Cas 2 : éléments de premier niveau qui contiennent un raster ou rgba
      candidates <- names(hist_data)[
        vapply(hist_data, function(x) {
          is.list(x) && any(c("raster", "rgba") %in% names(x))
        }, logical(1))
      ]
      if (length(candidates) > 0) return(candidates)
      
      # Cas 3 : noms connus (fallback)
      known <- c("lowres", "hires")
      known <- known[known %in% names(hist_data)]
      if (length(known) > 0) return(known)
      
      character(0)
    }
    
    # --------------------------------------------------------------------------
    # Mise à jour dynamique des choix de résolution
    # --------------------------------------------------------------------------
    observe({
      hist_data <- global_data$spatial_obj$histology
      req(hist_data)
      
      res_choices <- get_available_resolutions(hist_data)
      if (length(res_choices) == 0) {
        # Si aucune résolution trouvée, on garde les valeurs par défaut
        res_choices <- c("hires", "lowres")
      }
      
      # FIX (audit step 3.12) : hires en premier / par defaut -- confirme
      # fiable en test utilisateur (lowres apparaissait systematiquement
      # trop petit/decale, voir R/utils_spatial_io.R::extract_histology_image()
      # pour le diagnostic complet). lowres reste choisissable manuellement.
      if ("hires" %in% res_choices) {
        res_choices <- c("hires", setdiff(res_choices, "hires"))
      }
      
      # Créer des noms lisibles (première lettre en majuscule)
      names(res_choices) <- tools::toTitleCase(res_choices)
      
      updateSelectInput(
        session,
        "histology_resolution",
        choices = res_choices,
        selected = if (input$histology_resolution %in% res_choices) {
          input$histology_resolution
        } else {
          res_choices[1]
        }
      )
    })
    
    # --------------------------------------------------------------------------
    # Layout reactif "Carte spatiale" (feedback UI) : la carte Plotly recupere
    # toute la largeur quand l'apercu PNG (case a cocher, deplacee dans la
    # sidebar) est decoche, au lieu de garder une colonne vide a 5/12. La case
    # elle-meme ne fait plus partie de cette carte -- voir la sidebar.
    # --------------------------------------------------------------------------
    output$carte_spatiale_layout_ui <- renderUI({
      show_png <- isTRUE(input$show_static_export_preview)
      plotly_card <- card(
        full_screen = TRUE,
        style = "min-height: 72vh;",
        card_header("Carte interactive (Plotly)"),
        card_body(
          class = "p-0",
          plotly::plotlyOutput(ns("spatial_preview_plot"), height = if (show_png) "72vh" else "80vh")
        )
      )
      if (!show_png) return(plotly_card)

      layout_columns(
        col_widths = c(7, 5),
        plotly_card,
        card(
          full_screen = TRUE,
          style = "min-height: 72vh;",
          card_header("Aperçu dynamique — export PNG"),
          card_body(
            div(
              class = "text-muted mb-2",
              style = "font-size: 0.72rem;",
              "Même moteur que le bouton Export PNG. ",
              "Le fond est temporairement masqué, sans crash, pendant un changement de résolution invalide."
            ),
            plotOutput(ns("static_export_preview"), height = "calc(72vh - 65px)")
          )
        )
      )
    })

    # --------------------------------------------------------------------------
    # Statut de la normalisation du sketch
    # --------------------------------------------------------------------------
    output$sketch_norm_status_ui <- renderUI({
      req(global_data$spatial_obj$sketch)
      norm_used <- tryCatch(
        Seurat::DefaultAssay(global_data$spatial_obj$sketch),
        error = function(e) NA
      )
      label <- if (identical(norm_used, "SCT")) "SCTransform" else "LogNormalize"
      div(
        class = "alert alert-light",
        style = "font-size:0.7rem;padding:2px 8px;",
        sprintf("Normalisation du sketch : %s", label)
      )
    })
    
    # --------------------------------------------------------------------------
    # Overlay histologique (nouvelle version multi-résolution)
    # --------------------------------------------------------------------------
    histology_overlay <- reactive({
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data)) return(NULL)
      
      requested_resolution <- input$histology_resolution %||% "hires"
      
      hist_img <- tryCatch(
        get_histology_raster(
          hist_data = hist_data,
          resolution = requested_resolution
        ),
        error = function(e) {
          warning(
            "Lecture du fond histologique echouee : ",
            conditionMessage(e),
            call. = FALSE
          )
          NULL
        }
      )
      
      if (
        is.null(hist_img) ||
        is.null(hist_img$raw_raster) ||
        is.null(hist_img$rgba) ||
        is.null(hist_img$dim) ||
        length(hist_img$dim) < 2L
      ) {
        return(NULL)
      }
      
      scale_factor <- hist_img$scale_factor %||% 1
      if (!is.finite(scale_factor) || scale_factor <= 0) scale_factor <- 1
      
      bounds <- list(
        x = c(0, hist_img$dim[2L] / scale_factor),
        y = c(0, hist_img$dim[1L] / scale_factor)
      )
      
      data_uri <- tryCatch({
        raw_png <- png::writePNG(hist_img$rgba)
        
        if (requireNamespace("base64enc", quietly = TRUE)) {
          uri <- base64enc::dataURI(raw_png, mime = "image/png")
        } else if (requireNamespace("jsonlite", quietly = TRUE)) {
          uri <- paste0(
            "data:image/png;base64,",
            jsonlite::base64_enc(raw_png)
          )
        } else {
          NULL
        }
        
        # Nettoyage des retours à la ligne pour Plotly
        if (!is.null(uri)) {
          uri <- gsub("[\r\n[:space:]]+", "", uri)
        }
        
        uri
      }, error = function(e) {
        warning(
          "Encodage data URI histologique echoue : ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      })
      
      list(
        raster_obj = hist_img$raw_raster,
        rgba = hist_img$rgba,
        data_uri = data_uri,
        bounds = bounds,
        resolution = hist_img$resolution,
        scale_factor = scale_factor,
        diag = tryCatch(
          diagnose_histology_raster(hist_img$raw_raster),
          error = function(e) NULL
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Statut de l'histologie (UI)
    # --------------------------------------------------------------------------
    output$histology_status_ui <- renderUI({
      req(global_data$spatial_obj)
      
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data)) {
        return(
          div(
            class = "alert alert-light",
            style = "font-size:0.7rem;padding:2px 6px;",
            "Image histologique indisponible."
          )
        )
      }
      
      ov <- histology_overlay()
      
      if (is.null(ov) || is.null(ov$raster_obj)) {
        return(
          div(
            class = "alert alert-warning",
            style = "font-size:0.7rem;padding:2px 6px;",
            "Le fond histologique n'a pas pu etre prepare pour cette resolution."
          )
        )
      }
      
      if (is.null(ov$data_uri)) {
        return(
          div(
            class = "alert alert-warning",
            style = "font-size:0.7rem;padding:2px 6px;",
            "Fond disponible pour l'export PNG, mais echec de l'encodage Plotly."
          )
        )
      }
      
      div(
        class = "text-muted",
        style = "font-size:0.65rem;padding:2px 6px;",
        sprintf(
          "Fond histologique OK — %s — %d x %d px — %.0f Ko",
          ov$resolution,
          dim(ov$raster_obj)[2L],
          dim(ov$raster_obj)[1L],
          nchar(ov$data_uri) / 1024
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Téléchargement du fond brut (PNG)
    # --------------------------------------------------------------------------
    output$dl_histology_raw <- downloadHandler(
      filename = function() {
        paste0(
          "fond_histologique_",
          input$histology_resolution %||% "hires",
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        hist_data <- global_data$spatial_obj$histology
        
        hist_img <- get_histology_raster(
          hist_data = hist_data,
          resolution = input$histology_resolution %||% "hires"
        )
        
        validate(
          need(
            !is.null(hist_img) && !is.null(hist_img$rgba),
            "Aucune image histologique disponible."
          )
        )
        
        png::writePNG(hist_img$rgba, file)
      }
    )
    
    # --------------------------------------------------------------------------
    # Fonction de construction d'une image Plotly à partir de l'overlay
    # Renvoie un objet image unique (list) ou NULL
    # --------------------------------------------------------------------------
    # ========================================================================
    #  make_plotly_histology_image (remplacé)
    # ========================================================================
    make_plotly_histology_image <- function(hist_ov, opacity = 0.7) {
      if (
        is.null(hist_ov) ||
        is.null(hist_ov$data_uri) ||
        !nzchar(hist_ov$data_uri) ||
        is.null(hist_ov$bounds)
      ) {
        return(NULL)
      }
      
      b <- hist_ov$bounds
      
      xmin <- b$x[1L]
      xmax <- b$x[2L]
      ymin <- b$y[1L]
      ymax <- b$y[2L]
      
      # Les points utilisent y_display = -y.
      # Les coordonnées de l'image Plotly doivent couvrir [-ymax, -ymin].
      # Avec yanchor = "bottom", y est le bord inférieur de l'image.
      list(
        source = hist_ov$data_uri,
        xref = "x",
        yref = "y",
        x = xmin,
        y = -ymax,
        sizex = xmax - xmin,
        sizey = ymax - ymin,
        xanchor = "left",
        yanchor = "bottom",
        sizing = "stretch",
        opacity = opacity,
        layer = "below"
      )
    }
    
    # ========================================================================
    #  compute_spatial_ranges (remplacé)
    # ========================================================================
    compute_spatial_ranges <- function(df_all, hist_ov = NULL, show_hist = FALSE) {
      x_vals <- df_all$x
      y_vals <- -df_all$y
      
      if (
        isTRUE(show_hist) &&
        !is.null(hist_ov) &&
        !is.null(hist_ov$bounds)
      ) {
        xmin <- hist_ov$bounds$x[1L]
        xmax <- hist_ov$bounds$x[2L]
        ymin <- hist_ov$bounds$y[1L]
        ymax <- hist_ov$bounds$y[2L]
        
        # FIX (audit step 3.12): a resolution switch can momentarily produce
        # non-finite or degenerate bounds (0-width/height) while the new
        # histology_overlay() is recomputing -- feeding those into
        # coord_fixed() crashed the static PNG preview (graphics::plot.new
        # error reported by user). Skip the histology contribution to the
        # range for that one frame rather than propagating garbage; the
        # points' own bounds are always used as a safe minimum.
        bounds_ok <- all(is.finite(c(xmin, xmax, ymin, ymax))) && xmax > xmin && ymax > ymin
        if (bounds_ok) {
          x_vals <- c(x_vals, xmin, xmax)
          y_vals <- c(y_vals, -ymax, -ymin)
        }
      }
      
      list(
        x = range(x_vals, na.rm = TRUE),
        y = range(y_vals, na.rm = TRUE)
      )
    }
    
    # --------------------------------------------------------------------------
    # Correctif d'orientation SCOPE PLOTLY UNIQUEMENT (audit step 3.11)
    # --------------------------------------------------------------------------
    # Applies ONLY where called explicitly below (spatial_preview_plot,
    # combined_spatial_df -> combined_spatial_plot/roi_zoom_plot/
    # roi_context_plot). Does NOT touch plot_df() itself, so build_raster_plot()
    # (static_export_preview + dl_png) and $coords (BANKSY/Moran, read
    # directly from global_data$spatial_obj$coords, never from this module)
    # are unaffected -- see sidebar checkbox comment for the diagnosis that
    # led here.
    swap_for_plotly <- function(df) {
      if (!isTRUE(input$plotly_orient_fix)) return(df)
      tmp <- df$x
      df$x <- df$y
      df$y <- tmp
      df
    }
    
    histology_overlay_plotly <- reactive({
      ov <- histology_overlay()
      if (is.null(ov) || !isTRUE(input$plotly_orient_fix)) return(ov)
      b <- ov$bounds
      ov$bounds <- list(x = b$y, y = b$x)
      ov
    })
    
    
    
    # --------------------------------------------------------------------------
    # Safe histology helpers for static ggplot rendering
    # --------------------------------------------------------------------------
    
    is_valid_histology_overlay <- function(hist_ov) {
      if (is.null(hist_ov) || is.null(hist_ov$bounds) || is.null(hist_ov$rgba)) {
        return(FALSE)
      }
      
      b <- hist_ov$bounds
      rgba_dim <- dim(hist_ov$rgba)
      
      if (
        length(rgba_dim) != 3L ||
        rgba_dim[1L] < 1L ||
        rgba_dim[2L] < 1L ||
        rgba_dim[3L] < 3L
      ) {
        return(FALSE)
      }
      
      if (
        is.null(b$x) ||
        is.null(b$y) ||
        length(b$x) != 2L ||
        length(b$y) != 2L
      ) {
        return(FALSE)
      }
      
      all(is.finite(c(b$x, b$y))) &&
        b$x[2L] > b$x[1L] &&
        b$y[2L] > b$y[1L]
    }
    
    #' Convert an RGBA image to a ggplot-compatible raster safely
    #'
    #' @param hist_ov Histology overlay returned by histology_overlay().
    #' @param max_pixels Maximum number of pixels accepted for static rendering.
    #' @return A raster object or NULL.
    safe_static_histology_raster <- function(hist_ov, max_pixels = 25000000L) {
      if (!is_valid_histology_overlay(hist_ov)) {
        return(NULL)
      }
      
      rgba <- hist_ov$rgba
      image_dim <- dim(rgba)
      n_pixels <- as.double(image_dim[1L]) * as.double(image_dim[2L])
      
      # Avoid allocating a very large character raster in RAM.
      if (!is.finite(n_pixels) || n_pixels < 1 || n_pixels > max_pixels) {
        return(NULL)
      }
      
      tryCatch({
        red <- pmin(1, pmax(0, rgba[, , 1L]))
        green <- pmin(1, pmax(0, rgba[, , 2L]))
        blue <- pmin(1, pmax(0, rgba[, , 3L]))
        
        alpha <- if (image_dim[3L] >= 4L) {
          pmin(1, pmax(0, rgba[, , 4L]))
        } else {
          matrix(1, nrow = image_dim[1L], ncol = image_dim[2L])
        }
        
        grDevices::as.raster(
          grDevices::rgb(
            red = as.vector(red),
            green = as.vector(green),
            blue = as.vector(blue),
            alpha = as.vector(alpha)
          )
        ) |>
          matrix(nrow = image_dim[1L], ncol = image_dim[2L])
      }, error = function(e) {
        NULL
      })
    }
    
    #' Guarantee finite non-degenerate coordinate ranges for ggplot/Plotly
    #'
    #' @param x Numeric vector.
    #' @param fallback Numeric length-two fallback range.
    #' @return Numeric length-two range.
    safe_plot_range <- function(x, fallback = c(0, 1)) {
      out <- suppressWarnings(range(x[is.finite(x)], na.rm = TRUE))
      
      if (length(out) != 2L || !all(is.finite(out))) {
        return(fallback)
      }
      
      if (identical(out[1L], out[2L])) {
        delta <- max(abs(out[1L]) * 0.02, 1)
        out <- out + c(-delta, delta)
      }
      
      out
    }
    
    # --------------------------------------------------------------------------
    # Fonction de diagnostic (déjà présente)
    # --------------------------------------------------------------------------
    build_histology_debug_text <- function(hist_ov) {
      if (is.null(hist_ov)) return("Histology overlay = NULL")
      d <- hist_ov$diag
      paste0(
        "Histology debug | data_uri: ", !is.null(hist_ov$data_uri),
        " | raster dims: ", paste(dim(hist_ov$raster_obj), collapse = " x "),
        " | x=[", paste(round(hist_ov$bounds$x, 2), collapse = ", "), "]",
        " | y=[", paste(round(hist_ov$bounds$y, 2), collapse = ", "), "]",
        if (!is.null(d)) {
          sprintf(
            " | pct_near_white=%.1f%% | n_unique_colors=%d | mean_rgb=[%s]",
            d$pct_near_white, d$n_unique_colors, paste(d$mean_rgb, collapse = ",")
          )
        } else ""
      )
    }
    
    # --------------------------------------------------------------------------
    # Mise à jour de la liste des gènes
    # --------------------------------------------------------------------------
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$sketch)
      updateSelectizeInput(
        session,
        "gene",
        choices = rownames(global_data$spatial_obj$sketch),
        server = TRUE
      )
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Raccourci "genes spatialement variables (Moran's I)" -- alimente le
    # champ de recherche de gene ci-dessus a partir des resultats calcules
    # dans l'onglet "1. QC & Autocorrelation". Cache tant que Moran's I n'a
    # jamais ete lance pour l'echantillon actif (shared_rv$moran_results
    # est deja reinitialise au changement d'echantillon par mod_spatial.R).
    # --------------------------------------------------------------------------
    output$moran_quickpick_ui <- renderUI({
      req(shared_rv$moran_results, nrow(shared_rv$moran_results) > 0)
      top <- shared_rv$moran_results[order(-shared_rv$moran_results$moran_i), ]
      top <- top[!is.na(top$gene), , drop = FALSE]
      choices <- stats::setNames(
        top$gene,
        sprintf("%s (I = %.2f)", top$gene, top$moran_i)
      )
      tagList(
        selectInput(
          ns("moran_gene_pick"),
          "Raccourci : gene spatialement variable (indice de Moran)",
          choices = c("\u2014 choisir parmi le top Moran's I \u2014" = "",
                     utils::head(choices, 100))
        ),
        div(class = "text-muted", style = "font-size:0.68rem; margin-top:-8px;",
            "Classes par indice de Moran decroissant (calcule onglet 1).")
      )
    })

    observeEvent(input$moran_gene_pick, {
      req(nzchar(input$moran_gene_pick), global_data$spatial_obj$sketch)
      g <- input$moran_gene_pick
      all_genes <- rownames(global_data$spatial_obj$sketch)
      if (!g %in% all_genes) {
        showNotification(sprintf("Gene '%s' introuvable dans le sketch actuel.", g),
                         type = "warning", duration = 6)
        return()
      }
      # ROBUSTESSE (feedback biologiste : "certains genes ne sont pas
      # synchronises") : pour un selectizeInput server-side, appeler
      # updateSelectizeInput() avec seulement `selected=` ne garantit pas que
      # le client affiche correctement une valeur qu'il n'a encore JAMAIS vue
      # via une recherche ajax (limitation connue de selectize en mode
      # server=TRUE). Re-fournir le jeu de choix COMPLET avec `server=TRUE`
      # a chaque appel enregistre `g` de facon fiable tout en preservant la
      # recherche libre ensuite (meme liste qu'a l'initialisation).
      updateSelectizeInput(session, "gene", choices = all_genes, selected = g, server = TRUE)
      # Le champ "gene" n'etait utile que si color_by == "gene" -- sans ce
      # changement, choisir un gene ici ne se "voyait" nulle part tant que
      # l'utilisateur ne changeait pas MANUELLEMENT "Colorer par" -> "Gene".
      updateSelectInput(session, "color_by", selected = "gene")
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # "Ajouter au rapport" -- sauvegarde la CONFIG de la vue actuelle (pas une
    # image) dans shared_rv$saved_viz_list, cacheable/restaurable par
    # mod_spatial.R comme tout le reste. Rendue par le rapport (onglet 7) via
    # build_saved_viz_df() (R/utils_spatial_report.R).
    # --------------------------------------------------------------------------
    observeEvent(input$btn_add_to_report, {
      label <- trimws(input$viz_save_label %||% "")
      if (!nzchar(label)) label <- sprintf("Vue %s", format(Sys.time(), "%H:%M:%S"))
      cfg <- list(
        color_by = input$color_by, qc_metric = input$qc_metric %||% "nCount",
        gene = input$gene, deconv_celltype = input$deconv_celltype,
        show_cluster_labels = isTRUE(input$show_cluster_labels), created_at = Sys.time()
      )
      current <- shared_rv$saved_viz_list %||% list()
      current[[label]] <- cfg
      shared_rv$saved_viz_list <- current
      showNotification(sprintf("Vue '%s' ajoutee au rapport.", label), type = "message", duration = 4)
      updateTextInput(session, "viz_save_label", value = "")
    })

    output$saved_viz_list_ui <- renderUI({
      lst <- shared_rv$saved_viz_list %||% list()
      if (length(lst) == 0) {
        return(div(class = "text-muted", style = "font-size:0.7rem; margin-top:4px;",
                   "Aucune vue sauvegardee pour cet echantillon."))
      }
      tagList(
        tags$ul(style = "font-size:0.72rem; padding-left:1.1rem; margin-top:4px;",
                lapply(names(lst), function(nm) tags$li(nm))),
        actionLink(ns("btn_clear_saved_viz"), "Vider les vues sauvegardees", style = "font-size:0.7rem;")
      )
    })

    observeEvent(input$btn_clear_saved_viz, {
      shared_rv$saved_viz_list <- list()
    })
    
    
    # --------------------------------------------------------------------------
    # UI du type cellulaire pour la déconvolution
    # --------------------------------------------------------------------------
    output$deconv_celltype_ui <- renderUI({
      req(shared_rv$deconv_props)
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      selectInput(ns("deconv_celltype"), NULL, choices = cts)
    })
    
    # FIX (crash "cannot coerce type 'closure'..."): relancer la deconvolution
    # avec une AUTRE methode change les noms de colonnes (RCTD garde les noms
    # bruts "T_CD4+_1" ; TransferData() les fait passer par la convention
    # Seurat qui remplace "_" par "-", ex "T-CD4+-1") -- input$deconv_celltype
    # cote serveur peut rester bloque sur l'ancienne valeur le temps d'un
    # aller-retour navigateur. Force une selection valide des que les
    # colonnes disponibles changent.
    observeEvent(shared_rv$deconv_props, {
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      if (length(cts) == 0) return()
      if (is.null(input$deconv_celltype) || !input$deconv_celltype %in% cts) {
        updateSelectInput(session, "deconv_celltype", choices = cts, selected = cts[1])
      }
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------------------------------
    # Données à afficher (plot_df)
    # --------------------------------------------------------------------------
    plot_df <- reactive({
      req(global_data$spatial_obj$sketch, global_data$spatial_obj$coords)
      sk_ids <- colnames(global_data$spatial_obj$sketch)
      coords <- global_data$spatial_obj$coords
      df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]
      
      df$value <- tryCatch(
        switch(input$color_by,
               "cluster" = {
                 req(shared_rv$cluster_labels)
                 as.character(shared_rv$cluster_labels[df$id])
               },
               #ajout niche labels
               "niche" = {
                 req(shared_rv$niche_labels)
                 as.character(shared_rv$niche_labels[df$id])
               },
               "deconv" = {
                 req(shared_rv$deconv_props, input$deconv_celltype)
                 req(input$deconv_celltype %in% colnames(shared_rv$deconv_props))
                 m <- match(df$id, shared_rv$deconv_props$id)
                 shared_rv$deconv_props[[input$deconv_celltype]][m]
               },
               "qc" = {
                 req(shared_rv$qc_metrics)
                 m <- match(df$id, shared_rv$qc_metrics$id)
                 shared_rv$qc_metrics[[input$qc_metric]][m]
               },
               "gene" = {
                 req(input$gene)
                 sk <- global_data$spatial_obj$sketch
                 if (!"data" %in% SeuratObject::Layers(sk)) {
                   sk <- Seurat::NormalizeData(sk, verbose = FALSE)
                 }
                 as.numeric(SeuratObject::LayerData(sk, layer = "data")[input$gene, df$id])
               }
        ),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          rep(NA_real_, nrow(df))
        }
      )
      
      df[stats::complete.cases(df[, c("x", "y")]), ]
    })
    
    # --------------------------------------------------------------------------
    # Échelle d'opacité pour l'expression génique
    # --------------------------------------------------------------------------
    scale_alpha_by_value <- function(v, alpha_range = c(0.15, 1)) {
      rng <- suppressWarnings(range(v[is.finite(v)]))
      if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(alpha_range[2], length(v)))
      norm <- (v - rng[1]) / diff(rng)
      norm[!is.finite(norm)] <- 0
      alpha_range[1] + norm * diff(alpha_range)
    }
    
    # --------------------------------------------------------------------------
    # Tri des labels de clusters
    # --------------------------------------------------------------------------
    sort_cluster_labels <- function(x) {
      x <- unique(stats::na.omit(as.character(x)))
      if (length(x) == 0) return(x)
      if (all(grepl("^[0-9]+$", x))) x[order(as.integer(x))] else sort(x)
    }
    
    cluster_palette <- function(cluster_vec) {
      lv <- sort_cluster_labels(cluster_vec)
      if (length(lv) == 0) return(NULL)
      stats::setNames(grDevices::hcl.colors(length(lv), palette = "Dark 3"), lv)
    }
    
    # --------------------------------------------------------------------------
    # Calcul des couleurs selon le mode
    # --------------------------------------------------------------------------
    color_values <- function(df) {
      n <- nrow(df)
      if (n == 0L) return(character(0))
      
      add_alpha <- function(cols, alpha) {
        cols <- as.character(cols)
        alpha <- as.numeric(alpha)
        if (length(alpha) != length(cols)) alpha <- rep(1, length(cols))
        cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        alpha[!is.finite(alpha)] <- 1
        alpha <- pmax(0, pmin(1, alpha))
        
        rgb <- grDevices::col2rgb(cols, alpha = FALSE)
        grDevices::rgb(
          red = rgb[1, ],
          green = rgb[2, ],
          blue = rgb[3, ],
          alpha = round(alpha * 255),
          maxColorValue = 255
        )
      }
      
      if (is.numeric(df$value)) {
        valid <- is.finite(df$value)
        if (!any(valid)) return(rep("#CCCCCC", n))
        
        domain <- range(df$value[valid])
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = domain,
          na.color = "#CCCCCC"
        )
        cols <- as.character(pal(df$value))
        cols[!valid | is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        
        if (identical(input$color_by, "gene") && isTRUE(input$scale_alpha_by_expr)) {
          alpha <- scale_alpha_by_value(df$value, input$alpha_range %||% c(0.15, 1))
          return(add_alpha(cols, alpha))
        }
        return(cols)
      }
      
      vals <- as.character(df$value)
      vals[is.na(vals) | !nzchar(vals)] <- NA_character_
      
      if (identical(input$color_by, "cluster")) {
        pal <- cluster_palette(vals)
        cols <- unname(pal[vals])
        cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        return(cols)
      }
      
      lv <- sort(unique(stats::na.omit(vals)))
      if (length(lv) == 0L) return(rep("#CCCCCC", n))
      
      pal <- stats::setNames(
        grDevices::hcl.colors(length(lv), palette = "Dark 3"),
        lv
      )
      cols <- unname(pal[vals])
      cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
      cols
    }
    
    # --------------------------------------------------------------------------
    # Carte spatiale interactive (Plotly)
    # --------------------------------------------------------------------------
    output$spatial_preview_plot <- plotly::renderPlotly({
      df <- plot_df()
      req(nrow(df) > 0)
      df <- swap_for_plotly(df)
      
      df_plot <- df
      df_plot$y_display <- -df_plot$y
      df_plot$colour <- color_values(df_plot)
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      
      max_preview_cells <- if (isTRUE(show_hist)) 40000L else 150000L
      if (nrow(df_plot) > max_preview_cells) {
        set.seed(1)
        df_plot <- df_plot[sample.int(nrow(df_plot), max_preview_cells), , drop = FALSE]
      }
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      p <- plotly::plot_ly(
        data = df_plot,
        x = ~x,
        y = ~y_display,
        key = ~id,
        type = trace_type,
        mode = "markers",
        marker = list(
          color = ~colour,
          size = input$pt_radius,
          opacity = input$pt_opacity %||% 0.85
        ),
        text = ~paste0(
          "ID: ", id,
          "<br>x: ", round(x, 1),
          "<br>y: ", round(-y_display, 1),
          "<br>Valeur: ", ifelse(is.na(value), "NA",
                                 if (is.numeric(value)) round(value, 3) else as.character(value)
          )
        ),
        hoverinfo = "text",
        source = ns("spatial_preview_src")
      ) |>
        plotly::layout(
          dragmode = "pan",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = spatial_ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = spatial_ranges$y
          )
        )
      
      if (
        identical(input$color_by, "cluster") &&
        isTRUE(input$show_cluster_labels) &&
        is.character(df_plot$value)
      ) {
        centroids <- stats::aggregate(cbind(x, y_display) ~ value, data = df_plot, FUN = mean)
        p <- p |>
          plotly::add_annotations(
            x = centroids$x,
            y = centroids$y_display,
            text = centroids$value,
            showarrow = FALSE,
            font = list(size = 13, color = "#1a1a1a"),
            bgcolor = "rgba(255,255,255,0.65)",
            bordercolor = "#666",
            borderwidth = 0.5
          )
      }
      
      p <- plotly::event_register(p, "plotly_selected")
      p
    })
    
    # --------------------------------------------------------------------------
    # Construction du rendu statique (ggplot) – corrigé pour utiliser le raster
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # Shared static plot builder: preview and exported PNG use the same object
    # --------------------------------------------------------------------------
    
    build_raster_plot <- function(df) {
      validate(
        shiny::need(is.data.frame(df) && nrow(df) > 0L, "Aucune coordonnée spatiale exploitable."),
        shiny::need(
          all(c("x", "y", "value") %in% colnames(df)),
          "Les colonnes x, y ou value sont absentes."
        )
      )
      
      df <- df[
        is.finite(df$x) & is.finite(df$y),
        ,
        drop = FALSE
      ]
      
      validate(
        shiny::need(nrow(df) > 0L, "Aucune coordonnée spatiale finie à afficher.")
      )
      
      use_alpha_scale <- identical(input$color_by, "gene") &&
        isTRUE(input$scale_alpha_by_expr) &&
        is.numeric(df$value)
      
      p <- if (use_alpha_scale) {
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = x, y = -y, color = value, alpha = value)
        )
      } else {
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = x, y = -y, color = value)
        )
      }
      
      hist_ov <- histology_overlay()
      static_raster <- if (isTRUE(input$show_histology)) {
        safe_static_histology_raster(hist_ov)
      } else {
        NULL
      }
      
      show_hist <- !is.null(static_raster) && is_valid_histology_overlay(hist_ov)
      
      if (isTRUE(input$show_histology) && !show_hist) {
        # Deliberately do not stop the preview: spots remain visible while a
        # resolution is being recomputed or when a raster is too large for RAM.
        showNotification(
          "Fond histologique temporairement indisponible pour l'aperçu PNG ; les spots restent affichés.",
          type = "warning",
          duration = 4
        )
      }
      
      if (show_hist) {
        b <- hist_ov$bounds
        
        p <- p + ggplot2::annotation_raster(
          raster = static_raster,
          xmin = b$x[1L],
          xmax = b$x[2L],
          ymin = -b$y[2L],
          ymax = -b$y[1L],
          interpolate = TRUE
        )
      }
      
      point_alpha <- input$pt_opacity %||% 0.85
      
      if (requireNamespace("scattermore", quietly = TRUE)) {
        p <- if (use_alpha_scale) {
          p + scattermore::geom_scattermore(
            pointsize = input$pt_radius
          )
        } else {
          p + scattermore::geom_scattermore(
            pointsize = input$pt_radius,
            alpha = point_alpha
          )
        }
      } else {
        p <- if (use_alpha_scale) {
          p + ggplot2::geom_point(
            size = input$pt_radius / 4
          )
        } else {
          p + ggplot2::geom_point(
            size = input$pt_radius / 4,
            alpha = point_alpha
          )
        }
      }
      
      if (use_alpha_scale) {
        p <- p + ggplot2::scale_alpha_continuous(
          range = input$alpha_range %||% c(0.15, 1),
          guide = "none"
        )
      }
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = if (show_hist) hist_ov else NULL,
        show_hist = show_hist
      )
      
      spatial_ranges$x <- safe_plot_range(spatial_ranges$x)
      spatial_ranges$y <- safe_plot_range(spatial_ranges$y)
      
      p <- p +
        ggplot2::coord_fixed(
          xlim = spatial_ranges$x,
          ylim = spatial_ranges$y,
          expand = FALSE
        ) +
        ggplot2::theme_void()
      
      if (is.numeric(df$value)) {
        p + ggplot2::scale_color_viridis_c(
          na.value = "#CCCCCC"
        )
      } else {
        labels <- sort_cluster_labels(df$value)
        palette <- cluster_palette(labels)
        
        p + ggplot2::scale_color_manual(
          values = palette,
          na.value = "#CCCCCC"
        )
      }
    }
    
    # --------------------------------------------------------------------------
    # Shared static plot reactive
    # --------------------------------------------------------------------------
    
    static_export_plot <- reactive({
      req(isTRUE(input$show_static_export_preview))
      
      df <- plot_df()
      req(nrow(df) > 0L)
      
      tryCatch(
        build_raster_plot(df),
        error = function(e) {
          if (!inherits(e, "shiny.silent.error")) {
            showNotification(
              paste(
                "Aperçu PNG non généré :",
                conditionMessage(e)
              ),
              type = "warning",
              duration = 6
            )
          }
          
          NULL
        }
      )
    })
    
    # --------------------------------------------------------------------------
    # Static PNG preview
    # --------------------------------------------------------------------------
    
    output$static_export_preview <- renderPlot(
      {
        p <- static_export_plot()
        
        validate(
          shiny::need(
            !is.null(p),
            "Préparation de l'aperçu PNG…"
          )
        )
        
        print(p)
      },
      res = 110,
      bg = "white",
      execOnResize = FALSE
    )
    
    # --------------------------------------------------------------------------
    # PNG export: same rendering engine as the on-screen preview
    # --------------------------------------------------------------------------
    
    output$dl_png <- downloadHandler(
      filename = function() {
        paste0(
          "carte_spatiale_",
          input$color_by,
          "_",
          input$histology_resolution %||% "sans_fond",
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        df <- plot_df()
        
        validate(
          shiny::need(nrow(df) > 0L, "Aucune donnée à exporter.")
        )
        
        p <- build_raster_plot(df)
        
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = 8,
          height = 8,
          dpi = 200,
          bg = "white"
        )
      }
    )
    
    # --------------------------------------------------------------------------
    # Téléchargement PNG
    # --------------------------------------------------------------------------
    output$dl_png <- downloadHandler(
      filename = function() paste0("carte_spatiale_", input$color_by, "_", Sys.Date(), ".png"),
      content = function(file) {
        df <- plot_df()
        validate(need(nrow(df) > 0, "Aucune donnee a exporter."))
        ggplot2::ggsave(
          file,
          plot = build_raster_plot(df),
          width = 8,
          height = 8,
          dpi = 200,
          bg = "white"
        )
      }
    )
    
    # --------------------------------------------------------------------------
    # Téléchargement CSV
    # --------------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() paste0("carte_spatiale_", input$color_by, "_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- plot_df()
        validate(need(nrow(df) > 0, "Aucune donnee a exporter."))
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # --------------------------------------------------------------------------
    # UMAP asynchrone
    # --------------------------------------------------------------------------
    log_file <- spatial_log_path(session, "sketch_umap")
    tracker <- create_reactive_tracker(session, log_file)
    
    umap_task <- ExtendedTask$new(function(sketch_path, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement du sketch...", 1, 5)
          sk <- readRDS(sketch_path)
          
          already_sct <- identical(Seurat::DefaultAssay(sk), "SCT")
          
          if (already_sct) {
            write_mirai_log(log_file, "Sketch deja normalise (SCTransform) — HVG/scale.data reutilises.", 2, 5)
          } else {
            write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
            if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
            sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
            sk <- Seurat::ScaleData(sk, verbose = FALSE)
          }
          
          write_mirai_log(log_file, "PCA...", 3, 5)
          sk <- Seurat::RunPCA(sk, npcs = 30, verbose = FALSE)
          
          write_mirai_log(log_file, "UMAP...", 4, 5)
          sk <- Seurat::RunUMAP(sk, dims = 1:30, verbose = FALSE)
          
          write_mirai_log(log_file, "Termine.", 5, 5)
          emb <- as.data.frame(Seurat::Embeddings(sk, "umap"))
          colnames(emb)[1:2] <- c("dim1", "dim2")
          emb$id <- rownames(emb)
          emb
        },
        sketch_path = sketch_path,
        log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(umap_task, "btn_compute_umap")
    
    observeEvent(input$btn_compute_umap, {
      req(global_data$spatial_obj$sketch)
      reset_log(log_file)
      tmp <- tempfile(fileext = ".rds")
      saveRDS(global_data$spatial_obj$sketch, tmp)
      umap_task$invoke(sketch_path = tmp, log_file = log_file)
    })
    
    observeEvent(umap_task$status(), {
      if (umap_task$status() == "success") {
        shared_rv$umap_df <- umap_task$result()
        showNotification("UMAP calcule.", type = "message", duration = 3)
      } else if (umap_task$status() == "error") {
        showNotification("Erreur pendant le calcul UMAP.", type = "error", duration = 10)
      }
    })
    
    output$umap_progress_text <- renderText({
      lines <- tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })
    
    # --------------------------------------------------------------------------
    # Graphique UMAP
    # --------------------------------------------------------------------------
    output$umap_plot <- plotly::renderPlotly({
      req(shared_rv$umap_df)
      emb <- shared_rv$umap_df
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else {
        "sketch"
      }
      
      plotly::plot_ly(
        emb,
        x = ~dim1,
        y = ~dim2,
        color = ~cluster,
        colors = cluster_palette(emb$cluster),
        type = "scattergl",
        mode = "markers",
        marker = list(size = 5, opacity = 0.7)
      ) |>
        plotly::layout(
          margin = list(l = 20, r = 20, t = 20, b = 20),
          autosize = TRUE
        )
    })
    
    # --------------------------------------------------------------------------
    # Mode expert : sélection liée
    # --------------------------------------------------------------------------
    observeEvent(shared_rv$cluster_labels, {
      req(shared_rv$cluster_labels)
      updateSelectizeInput(
        session,
        "highlight_clusters",
        choices = sort_cluster_labels(shared_rv$cluster_labels),
        server = FALSE
      )
    }, ignoreInit = FALSE)
    
    linked_selection <- reactiveVal(character(0))
    
    selected_event_ids <- function(source_id) {
      ev <- tryCatch(
        plotly::event_data(
          event = "plotly_selected",
          source = source_id,
          session = session,
          priority = "event"
        ),
        error = function(e) NULL
      )
      
      if (is.null(ev) || nrow(ev) == 0L || !"key" %in% colnames(ev)) {
        return(character(0))
      }
      
      unique(as.character(ev$key[!is.na(ev$key)]))
    }
    
    observeEvent(
      plotly::event_data(
        event = "plotly_selected",
        source = ns("combined_spatial_src"),
        session = session,
        priority = "event"
      ),
      {
        ids <- selected_event_ids(ns("combined_spatial_src"))
        if (length(ids) > 0L) linked_selection(ids)
      },
      ignoreInit = TRUE
    )
    
    observeEvent(
      plotly::event_data(
        event = "plotly_selected",
        source = ns("combined_umap_src"),
        session = session,
        priority = "event"
      ),
      {
        ids <- selected_event_ids(ns("combined_umap_src"))
        if (length(ids) > 0L) linked_selection(ids)
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$highlight_clusters, {
      selected_clusters <- as.character(input$highlight_clusters %||% character(0))
      
      if (length(selected_clusters) == 0L) {
        linked_selection(character(0))
        return()
      }
      
      req(shared_rv$cluster_labels)
      
      ids <- names(shared_rv$cluster_labels)[
        as.character(shared_rv$cluster_labels) %in% selected_clusters
      ]
      
      linked_selection(unique(as.character(ids)))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_clear_selection, {
      linked_selection(character(0))
      updateSelectizeInput(
        session,
        "highlight_clusters",
        selected = character(0),
        server = FALSE
      )
    })
    
    highlighted_ids <- reactive({
      unique(as.character(linked_selection() %||% character(0)))
    })
    
    apply_highlight_alpha <- function(df) {
      ids <- highlighted_ids()
      
      if (length(ids) == 0L) {
        return(rep(0.85, nrow(df)))
      }
      
      ifelse(df$id %in% ids, 0.95, 0.08)
    }
    
    combined_spatial_df <- reactive({
      df <- plot_df()
      req(nrow(df) > 0L)
      df <- swap_for_plotly(df)  # every consumer of this reactive is Plotly-only
      
      df$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[df$id])
      } else {
        NA_character_
      }
      
      df
    })
    
    combined_umap_df <- reactive({
      req(shared_rv$umap_df)
      emb <- shared_rv$umap_df
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else {
        NA_character_
      }
      emb
    })
    
    output$highlight_debug_ui <- renderUI({
      hl <- highlighted_ids()
      if (length(hl) == 0) return(NULL)
      
      n_spatial <- sum(combined_spatial_df()$id %in% hl)
      n_umap <- if (!is.null(shared_rv$umap_df)) sum(combined_umap_df()$id %in% hl) else NA
      
      div(
        class = "text-info style-sm mt-1",
        style = "font-size:0.75rem;",
        sprintf(
          "\u2139\ufe0f %d cellule(s) selectionnee(s) / isolee(s) (Spatial: %s, UMAP: %s)",
          length(hl),
          paste0(n_spatial, " pts"),
          if (is.na(n_umap)) "non calcule" else paste0(n_umap, " pts")
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Vue combinée : spatial
    # --------------------------------------------------------------------------
    output$combined_spatial_plot <- plotly::renderPlotly({
      df <- combined_spatial_df()
      req(nrow(df) > 0L)
      
      df$y_display <- -df$y
      df$colour <- color_values(df)
      
      alpha_vec <- apply_highlight_alpha(df)
      
      add_alpha <- function(hex_col, alpha) {
        hex_col <- ifelse(is.na(hex_col) | !nzchar(hex_col), "#CCCCCC", hex_col)
        rgb_mat <- grDevices::col2rgb(hex_col, alpha = FALSE)
        
        grDevices::rgb(
          red = rgb_mat[1L, ],
          green = rgb_mat[2L, ],
          blue = rgb_mat[3L, ],
          alpha = pmax(0, pmin(255, round(alpha * 255))),
          maxColorValue = 255
        )
      }
      
      df$colour <- add_alpha(df$colour, alpha_vec)
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) &&
        !is.null(hist_ov$data_uri) &&
        isTRUE(input$show_histology)
      
      # Construction de l'image unique, puis empaquetage dans une liste
      hist_image <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      img <- if (!is.null(hist_image)) list(hist_image) else NULL
      
      ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      trace_type <- if (show_hist) "scatter" else "scattergl"
      
      p <- plotly::plot_ly(
        data = df,
        x = ~x,
        y = ~y_display,
        key = ~id,
        type = trace_type,
        mode = "markers",
        marker = list(
          color = ~colour,
          size = input$pt_radius,
          opacity = 1
        ),
        text = ~paste0(
          "ID : ", id,
          "<br>Valeur : ",
          ifelse(
            is.na(value),
            "NA",
            if (is.numeric(value)) round(value, 3) else as.character(value)
          ),
          "<br>Cluster : ",
          ifelse(is.na(cluster), "NA", cluster)
        ),
        hoverinfo = "text",
        source = ns("combined_spatial_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = img,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = ranges$y
          )
        )
      
      if (
        identical(input$color_by, "cluster") &&
        isTRUE(input$show_cluster_labels) &&
        any(!is.na(df$cluster))
      ) {
        centroids <- stats::aggregate(
          cbind(x, y_display) ~ cluster,
          data = df[!is.na(df$cluster), , drop = FALSE],
          FUN = mean
        )
        
        p <- p |>
          plotly::add_annotations(
            x = centroids$x,
            y = centroids$y_display,
            text = centroids$cluster,
            showarrow = FALSE,
            font = list(size = 11, color = "#1A1A1A"),
            bgcolor = "rgba(255,255,255,0.65)",
            bordercolor = "#666666",
            borderwidth = 0.5
          )
      }
      
      plotly::event_register(p, "plotly_selected")
    })
    
    # --------------------------------------------------------------------------
    # Vue combinée : UMAP
    # --------------------------------------------------------------------------
    output$combined_umap_plot <- plotly::renderPlotly({
      df <- combined_umap_df()
      req(nrow(df) > 0)
      
      pal <- cluster_palette(df$cluster)
      alpha_vec <- apply_highlight_alpha(df)
      
      encode_rgba <- function(hex_col, alpha) {
        hex_col <- ifelse(is.na(hex_col) | !nzchar(hex_col), "#CCCCCC", hex_col)
        rgb_mat <- grDevices::col2rgb(hex_col, alpha = FALSE)
        sprintf(
          "rgba(%d,%d,%d,%.3f)",
          rgb_mat[1, 1], rgb_mat[2, 1], rgb_mat[3, 1], alpha
        )
      }
      
      df$colour <- mapply(function(cl, a) {
        hex <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#CCCCCC"
        encode_rgba(hex, a)
      }, df$cluster, alpha_vec)
      
      p <- plotly::plot_ly(
        source = ns("combined_umap_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          xaxis = list(title = "UMAP_1", zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "UMAP_2", zeroline = FALSE, showgrid = FALSE)
        )
      
      for (cl in sort_cluster_labels(df$cluster)) {
        df_cl <- df[df$cluster == cl, , drop = FALSE]
        p <- p |>
          plotly::add_trace(
            data = df_cl,
            x = ~dim1,
            y = ~dim2,
            key = ~id,
            type = "scattergl",
            mode = "markers",
            name = cl,
            legendgroup = cl,
            marker = list(color = ~colour, size = 5),
            text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
            hoverinfo = "text"
          )
      }
      
      p <- plotly::event_register(p, "plotly_selected")
      p
    })
    
    # --------------------------------------------------------------------------
    # ROI
    # --------------------------------------------------------------------------
    roi_ids <- reactiveVal(NULL)
    
    # FIX (audit step 3.12 -- multi-echantillons dans l'onglet 4): switching
    # the active dataset (mod_spatial.R's selector) already resets shared_rv
    # (QC/clusters/deconv/ROI) and now ALSO restores/clears shared_rv$umap_df
    # centrally (see mod_spatial.R's .cacheable_fields -- feedback biologiste:
    # le sketch-UMAP se perdait a chaque changement d'echantillon). This
    # module's OWN local state -- roi_ids (indices into the PREVIOUS
    # dataset's coordinates) -- is NOT part of that cache (ROI stays tightly
    # coupled to this module's own local reactive state, not safely
    # restorable, see roi_ids below) and is still reset here explicitly.
    observeEvent(global_data$active_spatial_dataset, {
      roi_ids(NULL)
      shared_rv$roi_ids <- NULL
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_isolate_roi, {
      hl <- highlighted_ids()
      if (length(hl) == 0) {
        showNotification("Aucune selection a isoler. Utilisez le lasso.", type = "warning")
        return()
      }
      roi_ids(hl)
      showNotification(sprintf("ROI isolee (%d cellules).", length(hl)), type = "message")
    })
    
    observeEvent(input$btn_clear_roi, {
      roi_ids(NULL)
      shared_rv$roi_ids <- NULL
    })
    
    roi_df <- reactive({
      ids <- roi_ids()
      req(ids, length(ids) > 0)
      df <- combined_spatial_df()
      df[df$id %in% ids, , drop = FALSE]
    })
    
    output$roi_summary_ui <- renderUI({
      ids <- roi_ids()
      if (is.null(ids) || length(ids) == 0) {
        return(
          div(
            class = "alert alert-warning mb-3",
            bsicons::bs_icon("exclamation-triangle"),
            " Aucune ROI isolee."
          )
        )
      }
      
      total <- if (!is.null(global_data$spatial_obj$sketch)) ncol(global_data$spatial_obj$sketch) else NA
      pct <- if (!is.na(total) && total > 0) round(length(ids) / total * 100, 1) else NA
      
      div(
        class = "alert alert-success mb-3 d-flex justify-content-between align-items-center",
        div(
          strong(sprintf("ROI active : %d spots", length(ids))),
          if (!is.na(pct)) sprintf(" — %.1f%%", pct) else ""
        )
      )
    })
    
    output$roi_zoom_plot <- plotly::renderPlotly({
      df_roi <- roi_df()
      req(nrow(df_roi) > 0)
      df_roi$y_display <- -df_roi$y
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(hist_ov = hist_ov, opacity = input$histology_opacity %||% 0.7)
      } else {
        NULL
      }
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      xr <- range(df_roi$x, na.rm = TRUE)
      yr <- range(df_roi$y_display, na.rm = TRUE)
      xdiff <- max(diff(xr), 10)
      ydiff <- max(diff(yr), 10)
      x_range <- c(xr[1] - xdiff * 0.1, xr[2] + xdiff * 0.1)
      y_range <- c(yr[1] - ydiff * 0.1, yr[2] + ydiff * 0.1)
      
      pal <- cluster_palette(df_roi$cluster)
      
      p <- plotly::plot_ly() |>
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = x_range),
          yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, scaleanchor = "x", scaleratio = 1, range = y_range)
        )
      
      for (cl in sort_cluster_labels(df_roi$cluster)) {
        sub_df <- df_roi[df_roi$cluster == cl, , drop = FALSE]
        hex_col <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#CCCCCC"
        p <- p |>
          plotly::add_trace(
            data = sub_df,
            x = ~x,
            y = ~y_display,
            key = ~id,
            type = trace_type,
            mode = "markers",
            name = cl,
            legendgroup = cl,
            marker = list(color = hex_col, size = input$pt_radius),
            text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
            hoverinfo = "text"
          )
      }
      p
    })
    
    output$roi_context_plot <- plotly::renderPlotly({
      df_all <- combined_spatial_df()
      req(nrow(df_all) > 0)
      ids_roi <- roi_ids()
      req(ids_roi)
      
      df_all$y_display <- -df_all$y
      df_all$is_roi <- df_all$id %in% ids_roi
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(hist_ov = hist_ov, opacity = input$histology_opacity %||% 0.7)
      } else {
        NULL
      }
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      spatial_ranges <- compute_spatial_ranges(df_all = df_all, hist_ov = hist_ov, show_hist = show_hist)
      
      p <- plotly::plot_ly() |>
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = spatial_ranges$x),
          yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, scaleanchor = "x", scaleratio = 1, range = spatial_ranges$y)
        )
      
      df_bg <- df_all[!df_all$is_roi, , drop = FALSE]
      if (nrow(df_bg) > 0) {
        p <- p |>
          plotly::add_trace(
            data = df_bg,
            x = ~x,
            y = ~y_display,
            type = trace_type,
            mode = "markers",
            name = "Reste",
            marker = list(color = "rgba(180, 180, 180, 0.3)", size = max(1, input$pt_radius - 2)),
            hoverinfo = "none"
          )
      }
      
      df_fg <- df_all[df_all$is_roi, , drop = FALSE]
      if (nrow(df_fg) > 0) {
        pal <- cluster_palette(df_fg$cluster)
        for (cl in sort_cluster_labels(df_fg$cluster)) {
          sub_df <- df_fg[df_fg$cluster == cl, , drop = FALSE]
          hex_col <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#e41a1c"
          p <- p |>
            plotly::add_trace(
              data = sub_df,
              x = ~x,
              y = ~y_display,
              key = ~id,
              type = trace_type,
              mode = "markers",
              name = paste0("ROI: ", cl),
              marker = list(color = hex_col, size = input$pt_radius + 1),
              text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
              hoverinfo = "text"
            )
        }
      }
      p
    })
    
    # --------------------------------------------------------------------------
    # Marqueurs ROI (asynchrones)
    # --------------------------------------------------------------------------
    roi_log_file <- spatial_log_path(session, "roi_markers")
    roi_tracker <- create_reactive_tracker(session, roi_log_file)
    
    roi_markers_task <- ExtendedTask$new(function(sketch_path, roi_ids, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement du sketch...", 1, 4)
          sk <- readRDS(sketch_path)
          
          write_mirai_log(log_file, "Preparation du test...", 2, 4)
          if (!"data" %in% SeuratObject::Layers(sk)) {
            sk <- Seurat::NormalizeData(sk, verbose = FALSE)
          }
          
          sk$roi_group <- ifelse(colnames(sk) %in% roi_ids, "ROI", "Reste")
          sk$roi_group <- factor(sk$roi_group, levels = c("ROI", "Reste"))
          
          write_mirai_log(log_file, "Calcul FindMarkers...", 3, 4)
          Seurat::Idents(sk) <- "roi_group"
          res <- Seurat::FindMarkers(
            sk,
            ident.1 = "ROI",
            ident.2 = "Reste",
            test.use = "wilcox",
            logfc.threshold = 0.25,
            min.pct = 0.1,
            verbose = FALSE
          )
          
          write_mirai_log(log_file, "Termine.", 4, 4)
          res$gene <- rownames(res)
          res
        },
        sketch_path = sketch_path,
        roi_ids = roi_ids,
        log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(roi_markers_task, "btn_roi_markers")
    
    observeEvent(input$btn_roi_markers, {
      ids <- roi_ids()
      req(ids, length(ids) > 0, global_data$spatial_obj$sketch)
      reset_log(roi_log_file)
      tmp <- tempfile(fileext = ".rds")
      saveRDS(global_data$spatial_obj$sketch, tmp)
      roi_markers_task$invoke(sketch_path = tmp, roi_ids = ids, log_file = roi_log_file)
    })
    
    roi_markers_res <- reactiveVal(NULL)
    
    observeEvent(roi_markers_task$status(), {
      if (identical(roi_markers_task$status(), "success")) {
        result <- roi_markers_task$result()
        roi_markers_res(result)
        
        if (is.null(result) || nrow(result) == 0L) {
          showNotification(
            "Calcul termine, mais aucun gene ne passe les seuils definis.",
            type = "warning",
            duration = 8
          )
        } else {
          showNotification(
            sprintf("%d marqueurs ROI calcules.", nrow(result)),
            type = "message",
            duration = 4
          )
        }
      }
      
      if (identical(roi_markers_task$status(), "error")) {
        showNotification(
          "Erreur lors du calcul des marqueurs ROI. Consultez le journal.",
          type = "error",
          duration = 10
        )
      }
    })
    
    output$roi_markers_progress_text <- renderText({
      lines <- roi_tracker()
      if (length(lines) == 0) return("En attente...")
      paste(lines, collapse = "\n")
    })
    
    output$roi_markers_table <- DT::renderDT({
      res <- roi_markers_res()
      
      validate(
        need(
          !is.null(res) && nrow(res) > 0L,
          "Aucun marqueur a afficher. Lancez le calcul ou ajustez les seuils."
        )
      )
      
      p_cols <- intersect(c("p_val", "p_val_adj"), colnames(res))
      effect_cols <- intersect(c("avg_log2FC", "avg_logFC", "pct.1", "pct.2"), colnames(res))
      
      order_col <- match("p_val_adj", colnames(res))
      if (is.na(order_col)) order_col <- 1L
      
      tbl <- DT::datatable(
        res,
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50),
          scrollX = TRUE,
          autoWidth = TRUE,
          order = list(list(order_col - 1L, "asc"))
        )
      )
      
      if (length(effect_cols) > 0L) {
        tbl <- DT::formatRound(tbl, columns = effect_cols, digits = 3)
      }
      
      if (length(p_cols) > 0L) {
        tbl <- DT::formatSignif(tbl, columns = p_cols, digits = 3)
      }
      
      tbl
    })
    
    output$dl_roi_object <- downloadHandler(
      filename = function() paste0("spatial_roi_subset_", Sys.Date(), ".rds"),
      content = function(file) {
        ids <- roi_ids()
        validate(need(!is.null(ids) && length(ids) > 0, "Aucune ROI selectionnee."))
        sub_obj <- materialize_seurat_subset(global_data$spatial_obj, cellids = ids)
        saveRDS(sub_obj, file)
      }
    )
    
  })
}
