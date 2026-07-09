# =============================================================================
# mod_bulk_de_ui.R — Bulk Child 2: all UI builders (Step-3.6 refactor)
# =============================================================================
# Pure UI-builder functions (no server logic, no reactivity) extracted
# verbatim from the pre-refactor monolithic mod_bulk_de.R. Kept together in
# one file since they are low-complexity and share no state — splitting them
# further would add file-count without reducing real complexity.
#
#   mod_bulk_de_ui(id)              -> sidebar accordion body (Step 2 controls)
#   mod_bulk_de_volcano_ui(id)      -> main panel "Volcano Plot" tab
#   mod_bulk_de_ma_ui(id)           -> main panel "MA-Plot" tab
#   mod_bulk_de_heatmap_ui(id)      -> main panel "Heatmap" tab
#   mod_bulk_de_table_ui(id)        -> main panel "Table DE" tab
#   mod_bulk_de_summary_ui(id)      -> main panel "Resume Up/Down" tab
#   mod_bulk_de_venn_ui(id)         -> main panel "Venn / UpSet" tab
#   mod_bulk_de_multimethod_ui(id)  -> main panel "Multi-methodes" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("condition_col"), "Variable de Condition (principale)", choices = NULL),
    selectizeInput(ns("covariates"), "Covariables additionnelles (optionnel)",
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Ex: batch, sex")),

    verbatimTextOutput(ns("design_formula_preview")),

    uiOutput(ns("de_readiness_check")),

    fluidRow(
      column(6, selectInput(ns("group_ref"),    "Groupe Référence", choices = NULL)),
      column(6, selectInput(ns("group_target"), "Groupe Cible",     choices = NULL))
    ),

    selectInput(ns("de_engine"), "Moteur Statistique", choices = NULL),
    div(style = "display:flex;align-items:center;gap:6px;",
        checkboxInput(ns("shrink_lfc"), "Shrinkage LFC (apeglm) — DESeq2 uniquement", value = TRUE),
        tooltip(bsicons::bs_icon("info-circle"),
               "Réduit les Log2FC artificiellement élevés sur les gènes à faible expression (haute variance d'échantillonnage). Recommandé pour le classement/visualisation ; laissez activé sauf besoin spécifique.")),

    fluidRow(
      column(6, numericInput(ns("lfc_thresh"),  "Seuil |Log2FC|", value = 1,    min = 0, step = 0.1)),
      column(6, numericInput(ns("padj_thresh"), "Seuil p-adj",    value = 0.05, min = 0, max = 1, step = 0.01))
    ),

    textInput(ns("contrast_name"), "Nom du contraste (auto si vide)", placeholder = "Ex: KO_vs_WT"),

    actionButton(ns("run_de"), "🚀 Lancer l'Analyse Différentielle",
                 class = "btn-success w-100", icon = icon("play")),

    uiOutput(ns("pairwise_btn_ui")),

    actionButton(ns("run_multimethod"), "🔬 Comparer DESeq2 / edgeR / limma-voom",
                 class = "btn-outline-warning w-100 mt-1", icon = icon("scale-balanced")),
    helpText("Lance le MÊME contraste (Cible/Référence ci-dessus) avec les 3 moteurs, ",
             "puis calcule un consensus de rang — voir l'onglet \"Multi-méthodes\"."),

    hr(),

    # ── Contraste Ad-hoc (BingleSeq pattern) ──────────────────────────────
    div(
      class = "border rounded p-2 mb-2", style = "background:#fff8e1;",
      checkboxInput(ns("adhoc_mode"), tags$strong("🎯 Contraste Ad-hoc (sélection manuelle d'échantillons)"),
                   value = FALSE),
      conditionalPanel(
        condition = "input.adhoc_mode == true", ns = ns,
        helpText(style = "font-size:0.8em;",
                 "Ignore la colonne condition — reconstruit une métadonnée minimale à la volée."),
        splitLayout(
          checkboxGroupInput(ns("adhoc_group_a"), "Groupe A", choices = NULL),
          checkboxGroupInput(ns("adhoc_group_b"), "Groupe B", choices = NULL)
        ),
        textInput(ns("adhoc_contrast_name"), "Nom du contraste", placeholder = "Ex: KO_vs_WT"),
        uiOutput(ns("adhoc_readiness")),
        actionButton(ns("run_de_adhoc"), "🎯 Lancer l'Analyse (Ad-hoc)",
                    class = "btn-warning w-100", icon = icon("play"))
      )
    ),

    hr(),

    h6("Contrastes calculés:", style = "font-weight:bold;"),
    selectInput(ns("active_contrast_view"), NULL, choices = NULL),

    div(class = "small text-muted mt-1", textOutput(ns("de_status")))
  )
}


# ── UI: Volcano Plot tab ──────────────────────────────────────────────────────

mod_bulk_de_volcano_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header("Volcano Plot"),
    uiOutput(ns("sync_warning_banner")),
    checkboxInput(ns("volcano_interactive"), "📊 Interactif (Plotly — survol pour détails gène)", value = FALSE),
    uiOutput(ns("volcano_manual_palette_ui")),
    uiOutput(ns("volcano_container")),
    downloadButton(ns("dl_volcano_png"), "Export PNG (statique)", class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: MA-Plot tab ────────────────────────────────────────────────────────────

mod_bulk_de_ma_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header("MA-Plot"),
    checkboxInput(ns("ma_interactive"), "📊 Interactif (Plotly — survol pour détails gène)", value = FALSE),
    helpText(style = "font-size:0.8em;",
            "Couleurs (Significatif / Non-sig.) réglables depuis l'onglet Volcano Plot ",
            "(palette \"Manuel\" dans la barre latérale Étape 1)."),
    uiOutput(ns("ma_container")),
    downloadButton(ns("dl_ma_png"), "Export PNG (statique)", class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: Heatmap tab ────────────────────────────────────────────────────────────

mod_bulk_de_heatmap_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "900px",
    card_header("Heatmap"),
    uiOutput(ns("sync_warning_banner_heatmap")),
    fluidRow(
      column(4, numericInput(ns("heatmap_top_n"), "Top N gènes (par p-adj)", value = 30, min = 2, max = 200)),
      column(4, selectizeInput(ns("heatmap_annot"), "Annotation colonnes", choices = NULL,
                               options = list(placeholder = "Aucun", allowEmptyOption = TRUE))),
      column(4, selectInput(ns("heatmap_direction"), "Sous-ensemble",
                            choices = c("Tous (par p-adj)" = "all",
                                        "Significatifs (Up+Down)" = "sig",
                                        "Up-régulés" = "up",
                                        "Down-régulés" = "down",
                                        "Non-significatifs" = "ns")))
    ),
    uiOutput(ns("heatmap_manual_palette_ui")),
    plotOutput(ns("plot_heatmap"), height = "660px"),
    fluidRow(
      column(6, selectInput(ns("heatmap_export_fmt"), "Format export", choices = c("PNG" = "png", "PDF" = "pdf"))),
      column(6, div(style = "margin-top:25px;", downloadButton(ns("dl_heatmap"), "📥 Export Heatmap", class = "btn-sm btn-secondary w-100")))
    )
  )
}


# ── UI: Table DE tab ───────────────────────────────────────────────────────────

mod_bulk_de_table_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header("Table DE"),
    div(
      style = "display:flex;justify-content:flex-end;gap:5px;margin-bottom:5px;",
      downloadButton(ns("dl_de_csv"),   "CSV",   class = "btn-sm btn-primary"),
      downloadButton(ns("dl_de_excel"), "Excel", class = "btn-sm btn-success")
    ),
    DTOutput(ns("table_de"))
  )
}


# ── UI: Résumé Up/Down tab (barchart, all computed contrasts) ────────────────

mod_bulk_de_summary_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "1100px",
    card_header("Résumé Up/Down"),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("info-circle"),
        " Nombre de gènes Up / Down par contraste calculé, selon les seuils |Log2FC| / p-adj ",
        "actuels (Étape 2) — se met à jour en direct si vous changez les seuils, sans recalcul DE."),
    plotOutput(ns("plot_updown"), height = "620px"),
    downloadButton(ns("dl_updown_png"), "Export PNG", class = "btn-sm btn-secondary mt-2"),
    hr(),
    h6("Table récapitulative", style = "font-weight:bold;"),
    DTOutput(ns("table_updown")),
    downloadButton(ns("dl_updown_csv"), "Export CSV", class = "btn-sm btn-info mt-2")
  )
}


# ── UI: Venn/UpSet tab (compare gene sets across contrasts) ──────────────────

mod_bulk_de_venn_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Venn / UpSet"),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #9B59B6;",
       bsicons::bs_icon("info-circle"),
       " Compare les gènes significatifs ENTRE plusieurs contrastes (utile après un run ",
       tags$em("Pairwise auto"), "). Seuils utilisés : ceux du panneau Step 2 (|Log2FC| / p-adj) ",
       "— se mettent à jour en direct si vous les changez, sans recalcul DE."),
    uiOutput(ns("venn_gate_message")),
    fluidRow(
      column(7, selectizeInput(ns("venn_contrasts"), "Contrastes à comparer",
                               choices = NULL, multiple = TRUE,
                               options = list(maxItems = 6, placeholder = "2 à 6 contrastes"))),
      column(5, radioButtons(ns("venn_type"), "Type de diagramme",
                             choices = c("UpSet (recommandé)" = "upset", "Venn (2-4 contrastes)" = "venn"),
                             selected = "upset"))
    ),
    checkboxInput(ns("venn_direction_aware"),
                 "Distinguer Up / Down (chaque contraste devient 2 ensembles)", value = FALSE),
    div(style = "min-width:600px;overflow-x:auto;height:580px;",
       plotOutput(ns("venn_plot"), height = "560px")),
    fluidRow(
      column(6, downloadButton(ns("dl_venn_png"), "Export PNG", class = "btn-sm btn-secondary w-100")),
      column(6, downloadButton(ns("dl_venn_genes_csv"), "Export gènes par intersection (CSV)",
                               class = "btn-sm btn-info w-100"))
    ),
    hr(),
    h6("Table des intersections", style = "font-weight:bold;"),
    div(style = "min-height:200px;overflow-y:auto;",
        DTOutput(ns("venn_intersection_table")))
  )
}


# ── UI: Multi-méthodes tab (getAllDE + rankConsensus + Venn 3-méthodes) ──────

mod_bulk_de_multimethod_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "950px",
    card_header("Multi-méthodes (DESeq2 / edgeR / limma-voom)"),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #F39C12;",
       bsicons::bs_icon("info-circle"),
       " Compare le MÊME contraste (Cible/Référence du panneau Step 2) avec les 3 moteurs ",
       "statistiques disponibles. Le consensus de rang moyenne le classement p-value de ",
       "chaque méthode — un gène cohérent entre méthodes (rang faible, même sens du Log2FC) ",
       "est un candidat plus robuste qu'un gène significatif sur une seule méthode."),

    uiOutput(ns("multimethod_status_ui")),

    navset_tab(
      nav_panel(
        "Venn / UpSet (méthodes)",
        radioButtons(ns("mm_venn_type"), "Type de diagramme",
                    choices = c("UpSet (recommandé)" = "upset", "Venn (2-3 méthodes)" = "venn"),
                    selected = "upset", inline = TRUE),
        plotOutput(ns("mm_venn_plot"), height = "480px"),
        downloadButton(ns("dl_mm_venn_png"), "Export PNG", class = "btn-sm btn-secondary mt-2")
      ),
      nav_panel(
        "Table consensus",
        helpText("Triée par rang moyen (mean_rank) — les gènes les plus consistants entre ",
                 "méthodes apparaissent en premier."),
        DTOutput(ns("mm_consensus_table")),
        downloadButton(ns("dl_mm_consensus_csv"), "Export CSV", class = "btn-sm btn-info mt-2")
      )
    )
  )
}
