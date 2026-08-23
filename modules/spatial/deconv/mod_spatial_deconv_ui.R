# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv_ui.R — UI builders
# =============================================================================
# Pure UI functions, no server logic. Extracted from the former monolithic
# mod_spatial_deconv.R (Phase 2 refactor).
# =============================================================================

#' Main layout: sidebar (controls) + navset (3 output panels)
mod_spatial_deconv_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Deconvolution cellulaire", width = 380,

      radioButtons(ns("mode"), "Methode",
                   choices = c("Avec reference scRNA-seq (RCTD)" = "rctd",
                               "Transfert d'ancres (Label Transfer, Seurat)" = "labeltransfer",
                               "Sans reference (LDA, type STdeconvolve)" = "stdeconvolve"),
                   selected = "rctd"),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd' || input['%s'] == 'labeltransfer'", ns("mode"), ns("mode")),
        uiOutput(ns("ref_source_picker_ui")),
        conditionalPanel(
          condition = sprintf("!(input['%s'] && input['%s'] == 'shared')", ns("ref_source"), ns("ref_source")),
          fileInput(ns("ref_file"), "Reference scRNA-seq (.rds Seurat, .h5ad, .h5, .loom)",
                    accept = c(".rds", ".h5ad", ".h5", ".loom")),
          uiOutput(ns("ref_status_badge_ui")),
          uiOutput(ns("ref_celltype_col_ui")),
          uiOutput(ns("ref_celltype_summary_ui"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'shared'", ns("ref_source")),
          uiOutput(ns("shared_ref_status_ui"))
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'rctd'", ns("mode")),
        div(class = "alert alert-light", style = "font-size:0.75rem;",
            bsicons::bs_icon("info-circle"),
            " RCTD modelise l'expression par une loi de Poisson resolue par ",
            "programmation quadratique — pic RAM attendu sous ~2 Go. Execute ",
            "en mono-coeur (max_cores=1). Necessite au moins 25 cellules par ",
            "type dans la reference.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'labeltransfer'", ns("mode")),
        radioButtons(ns("lt_norm_method"), "Normalisation",
                     choices = c("LogNormalize (rapide, recommande CPU)" = "lognorm",
                                 "SCTransform (vignette Seurat, plus lent)" = "sct"),
                     selected = "lognorm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sct'", ns("lt_norm_method")),
          numericInput(ns("lt_ncells"), "Cellules pour l'apprentissage SCTransform (ncells)",
                       3000, min = 500, max = 10000, step = 500),
          div(class = "alert alert-warning", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-triangle"),
              " Installez 'glmGamPoi' (BiocManager::install('glmGamPoi')) pour accelerer, ",
              "sinon reduisez plutot le nombre de cellules par type (section reference).")
        ),
        numericInput(ns("lt_npcs"), "Composantes PCA (requete spatiale)", 30, min = 5, max = 50, step = 5)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("mode")),
        numericInput(ns("n_topics"), "Nombre de types cellulaires (K)", 6, min = 2, max = 30, step = 1),
        numericInput(ns("n_top_od"), "Genes surdisperses maximum (vitesse)", 1000, min = 200, max = 3000, step = 100)
      ),

      hr(),
      h6("Aperçu de la reference (UMAP/PCA)", style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          "Verifiez visuellement que les types cellulaires sont bien separes avant de lancer ",
          "RCTD/Label Transfer ci-dessous."),
      selectInput(ns("ref_viz_reduction"), "Reduction", choices = c("UMAP" = "umap", "PCA" = "pca"), selected = "umap"),
      checkboxInput(ns("ref_viz_interactive"), "Vue interactive (Plotly)", value = TRUE),
      bslib::input_task_button(ns("btn_ref_viz"), "Calculer / afficher", icon = icon("chart-line")),
      verbatimTextOutput(ns("ref_viz_progress_text"), placeholder = TRUE),

      bslib::input_task_button(ns("btn_deconv"), "2) Lancer la deconvolution",
                                icon = icon("puzzle-piece")),
      verbatimTextOutput(ns("deconv_progress_text"), placeholder = TRUE)
    ),

    navset_card_underline(
      nav_panel("Proportions par spot/cellule",
                plotOutput(ns("deconv_bar_plot"), height = "550px"),
                DT::DTOutput(ns("deconv_table"))),

      nav_panel("Reference (UMAP/PCA)",
                div(class = "alert alert-light small mb-2",
                    bsicons::bs_icon("info-circle"),
                    " Controles deplaces dans le panneau lateral ('Apercu de la reference')."),
                conditionalPanel(condition = sprintf("input['%s']", ns("ref_viz_interactive")),
                                 plotly::plotlyOutput(ns("ref_viz_plotly"), height = "600px")),
                conditionalPanel(condition = sprintf("!input['%s']", ns("ref_viz_interactive")),
                                 plotOutput(ns("ref_viz_plot"), height = "600px"))),

      nav_panel("Colocalisation (types cellulaires)",
                div(class = "alert alert-light small mb-2",
                    "Correlation entre proportions de types cellulaires par spot/cellule -- deux types ",
                    "fortement correles positivement tendent a co-localiser ; negativement, a s'exclure."),
                plotOutput(ns("deconv_coloc_plot"), height = "520px"))
    )
  )
}