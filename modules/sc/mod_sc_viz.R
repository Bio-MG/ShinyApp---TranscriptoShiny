# =============================================================================
# mod_sc_viz.R  —  Child 3: Visualisation (11 types) + Export + Palette SC
# =============================================================================
# Changes vs prev version:
#   [1] sc_discrete_scale() / sc_continuous_scale() — mirror Bulk palette helpers
#   [2] build_sc_viz_plot() — UNIFIED builder: single truth for live + report export
#       (removes duplication that existed between renderPlotly inline code and the
#       previous session's separate helpers_sc.R version)
#   [3] Palette UI: selectInput "Défaut / Okabe-Ito / Viridis / Set2 / Manuel"
#       + dynamic manual colour pickers per group_by level (mirrors Bulk pattern)
#   [4] "📌 Ajouter au Rapport" → shared_rv$report_viz_list (consumed by mod_sc.R)
#   [5] Volcano pickers: fixed stale isolate bug (new reactive on list(sc_obj, group_by))
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a)) ||
                              identical(a, "")) b else a

# ── Colour palette helpers ────────────────────────────────────────────────────
# Mirrors bulk_color_scale() / bulk_annotation_colors() from helpers_bulk.R.
# Kept local so the pair travels with their consumers and helpers_bulk.R is
# not a hard dependency for the SC module alone.

.sc_okabe <- c("#E69F00","#56B4E9","#009E73","#F0E442",
               "#0072B2","#D55E00","#CC79A7","#999999")

#' Discrete ggplot color/fill scale for SC group coloring
#' @param palette "default"|"okabeito"|"viridis"|"set2"|"manual"
#' @param manual_colors Named hex vector (level→color), used when palette=="manual"
#' @param aesthetic "color" or "fill"
#' @return ggplot scale layer or NULL (NULL = let Seurat/ggplot use its defaults)
sc_discrete_scale <- function(palette = "default", manual_colors = NULL,
                               aesthetic = "color") {
  fn_m <- if (aesthetic == "color") scale_color_manual else scale_fill_manual
  fn_v <- if (aesthetic == "color") scale_color_viridis_d else scale_fill_viridis_d
  fn_b <- if (aesthetic == "color") scale_color_brewer   else scale_fill_brewer
  if (identical(palette, "manual") && length(manual_colors) > 0)
    return(fn_m(values = manual_colors))
  switch(palette %||% "default",
    okabeito = fn_m(values = .sc_okabe),
    viridis  = fn_v(option = "turbo"),
    set2     = fn_b(palette = "Set2"),
    NULL)   # "default" → Seurat/ggplot auto-colors
}

#' Continuous ggplot color scale for SC feature expression
sc_continuous_scale <- function(palette = "default", aesthetic = "color") {
  fn <- if (aesthetic == "color") scale_color_viridis_c else scale_fill_viridis_c
  opt <- switch(palette %||% "default",
    okabeito = "plasma", viridis = "plasma", set2 = "viridis", "viridis")
  fn(option = opt)
}

# ── Unified plot builder ──────────────────────────────────────────────────────

#' Build a SC visualization ggplot from a config snapshot.
#'
#' Single source of truth used by BOTH the live render functions (which then
#' wrap with ggplotly or display static) AND the HTML/PDF report (via
#' params$saved_viz_list). Palette is applied via sc_discrete_scale() /
#' sc_continuous_scale() — both NULL-safe (NULL = Seurat/ggplot defaults,
#' which is what "default" palette produces).
#'
#' For the Volcano type, two extra attributes are attached to the returned
#' ggplot so the caller can construct a richer native plotly without re-running
#' FindMarkers:
#'   attr(p, "volcano_markers")  — the markers data.frame
#'   attr(p, "volcano_title")    — the plot title string
#'
#' @param obj     Seurat object.
#' @param cfg     Named list of input values (see .current_cfg() in server).
#' @param sc_palette Character palette name.
#' @param manual_colors Named character vector (level→hex), manual palette only.
#' @return ggplot or patchwork object.
build_sc_viz_plot <- function(obj, cfg, sc_palette = "default", manual_colors = NULL) {
  type    <- cfg$type     %||% "dim"
  pt_size <- as.numeric(cfg$pt_size %||% 0.5)
  grp     <- cfg$group_by %||% "seurat_clusters"

  theme_fn <- switch(cfg$plot_theme %||% "minimal",
    classic = theme_classic(), bw = theme_bw(), void = theme_void(),
    theme_minimal())

  pal_disc <- sc_discrete_scale(sc_palette, manual_colors, "color")
  pal_fill <- sc_discrete_scale(sc_palette, manual_colors, "fill")
  pal_cont <- sc_continuous_scale(sc_palette, "color")

  # NULL-safe scale adder: suppressWarnings silences ggplot2's "replacing
  # existing scale" message (expected when overriding Seurat's built-in scale).
  .add <- function(p, s) if (is.null(s)) p else suppressWarnings(p + s)

  # 1. DimPlot ----------------------------------------------------------------
  if (type == "dim") {
    red <- cfg$reduction %||% "umap"
    if (!red %in% names(obj@reductions)) stop("Réduction non calculée : ", red)
    p <- DimPlot(obj, reduction = red, group.by = grp,
                 label = TRUE, pt.size = pt_size) +
         theme_fn + ggtitle(paste(toupper(red), "\u2014", grp))
    return(.add(p, pal_disc))
  }

  # 2. FeaturePlot ------------------------------------------------------------
  if (type == "feature") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    # multi-gene returns patchwork: scale only safe for single gene
    if (length(valid) == 1L) {
      p <- FeaturePlot(obj, features = valid[1], pt.size = pt_size) + theme_fn
      return(.add(p, pal_cont))
    }
    return(FeaturePlot(obj, features = head(valid, 4), ncol = 2,
                       pt.size = pt_size) + theme_fn)
  }

  # 3. Scatter (enhanced) -----------------------------------------------------
  if (type == "scatter") {
    p <- plot_enhanced_scatter(
           obj, cfg$scatter_gene1, cfg$scatter_gene2, group.by = grp,
           method     = cfg$scatter_cor_method %||% "pearson",
           add_smooth = isTRUE(cfg$scatter_smooth),
           pt.size    = pt_size) + theme_fn
    return(.add(p, pal_disc))
  }

  # 4. Violin -----------------------------------------------------------------
  if (type == "violin") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    if (length(valid) == 1L) {
      p <- plot_violin_enhanced(obj, valid[1], grp,
                                add_boxplot = isTRUE(cfg$violin_boxplot)) + theme_fn
      return(.add(p, pal_fill))
    }
    plots <- lapply(head(valid, 4), function(g)
      .add(VlnPlot(obj, features = g, group.by = grp, pt.size = 0) +
           theme_fn + ggtitle(g), pal_fill))
    return(wrap_plots(plots, ncol = 2))
  }

  # 5. Stacked Violin ---------------------------------------------------------
  if (type == "stacked_violin") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    plots <- lapply(head(valid, 8), function(g)
      .add(VlnPlot(obj, features = g, group.by = grp, pt.size = 0) +
           theme(legend.position = "none",
                 axis.title.x   = element_blank(),
                 axis.text.x    = element_blank()) + ggtitle(g), pal_fill))
    return(wrap_plots(plots, ncol = 1) + theme_fn)
  }

  # 6. Ridge ------------------------------------------------------------------
  if (type == "ridge") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    p <- RidgePlot(obj, features = head(valid, 6), ncol = 2) + theme_fn
    return(.add(p, pal_fill))
  }

  # 7. DotPlot ----------------------------------------------------------------
  if (type == "dot") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    p <- DotPlot(obj, features = head(valid, 20), group.by = grp) + theme_fn +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # DotPlot: color encodes avg expression (continuous), size encodes pct expressed.
    # Apply continuous viridis on color only; size scale untouched.
    opt <- switch(sc_palette %||% "default",
      okabeito = "plasma", viridis = "plasma", set2 = "viridis", "viridis")
    return(suppressWarnings(p + scale_color_viridis_c(option = opt)))
  }

  # 8. Heatmap ----------------------------------------------------------------
  if (type == "heatmap") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop("Aucun gène valide")
    return(DoHeatmap(obj, features = head(valid, 30), group.by = grp) + theme_fn)
  }

  # 9. Correlation Matrix -----------------------------------------------------
  if (type == "correlation_matrix") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (length(valid) < 2) stop("Au moins 2 gènes requis")
    return(plot_correlation_matrix(obj, head(valid, 30), method = "pearson") + theme_fn)
  }

  # 10. Multi-Sample ----------------------------------------------------------
  if (type == "multi_sample") {
    if (length(unique(obj$orig.ident)) < 2) stop("Au moins 2 échantillons requis")
    p <- plot_multi_sample(obj, cfg$multi_gene %||% rownames(obj)[1],
                           cfg$multi_plot_type %||% "violin") + theme_fn
    return(.add(p, pal_fill))
  }

  # 11. Volcano (ggplot — also carries markers as attr for native plotly) ------
  if (type == "volcano") {
    grp_col <- grp
    if (!grp_col %in% colnames(obj@meta.data)) stop("Colonne de groupe introuvable")
    Idents(obj) <- factor(as.character(obj@meta.data[[grp_col]]))
    ident1 <- cfg$volcano_group1 %||% levels(Idents(obj))[1]
    ident2 <- if (is.null(cfg$volcano_group2) ||
                  cfg$volcano_group2 == "rest") NULL else cfg$volcano_group2
    if (!ident1 %in% levels(Idents(obj))) stop("Groupe 1 invalide: ", ident1)

    markers <- tryCatch(
      FindMarkers(obj, ident.1 = ident1, ident.2 = ident2,
                  only.pos = FALSE, min.pct = 0.1, logfc.threshold = 0,
                  verbose = FALSE),
      error = function(e) stop("FindMarkers: ", e$message))
    if (!nrow(markers)) stop("Aucun marqueur trouvé")
    markers$gene <- rownames(markers)

    lfc  <- as.numeric(cfg$volcano_logfc %||% 0.25)
    pval <- as.numeric(cfg$volcano_pval  %||% 0.05)
    markers$status <- dplyr::case_when(
      markers$avg_log2FC >  lfc & markers$p_val_adj < pval ~ "Up",
      markers$avg_log2FC < -lfc & markers$p_val_adj < pval ~ "Down",
      TRUE ~ "NS")

    color_map <- c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
    show_lbl  <- isTRUE(cfg$volcano_show_labels %||% TRUE)
    top_genes <- c(head(markers$gene[markers$status == "Up"],   10),
                   head(markers$gene[markers$status == "Down"], 10))
    markers$label <- ifelse(show_lbl & markers$gene %in% top_genes, markers$gene, "")

    vtitle <- paste0("Volcano: ", ident1, " vs ", ident2 %||% "rest")
    p <- ggplot(markers, aes(x = avg_log2FC,
                              y = -log10(p_val_adj + 1e-300),
                              color = status)) +
         geom_point(alpha = 0.75, size = pt_size) +
         scale_color_manual(values = color_map) +
         geom_vline(xintercept = c(-lfc, lfc), linetype = "dotted", color = "grey40") +
         geom_hline(yintercept = -log10(pval),  linetype = "dotted", color = "grey40") +
         labs(title = vtitle, x = "Log2 Fold Change",
              y = "-log10(P.adj)", color = "Statut") + theme_fn
    if (show_lbl)
      p <- p + geom_text(aes(label = label), size = 2.8, na.rm = TRUE,
                         vjust = -0.6, show.legend = FALSE)

    # Attach markers + title as attributes so the caller can build native plotly
    # without re-running FindMarkers.
    attr(p, "volcano_markers") <- markers
    attr(p, "volcano_title")   <- vtitle
    return(p)
  }

  stop("Type de visualisation non supporté: ", type)
}


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(

    selectInput(
      ns("viz_type"), "Style de Visualisation",
      choices = c(
        "Reduction Dimensionnelle (UMAP/PCA/t-SNE)" = "dim",
        "Feature Plot (Expression sur Reduction)"   = "feature",
        "Correlation Genes (Scatter Ameliore)"      = "scatter",
        "Distribution (Violin)"                     = "violin",
        "Stacked Violin Plot"                        = "stacked_violin",
        "Densite (Ridge Plot)"                       = "ridge",
        "DotPlot"                                   = "dot",
        "Heatmap"                                   = "heatmap",
        "Matrice Correlation"                        = "correlation_matrix",
        "Comparaison Multi-Echantillons"             = "multi_sample",
        "Volcano Plot"                               = "volcano"
      )
    ),

    # Conditional: DimPlot reduction picker
    conditionalPanel(
      condition = "input.viz_type == 'dim'", ns = ns,
      selectInput(ns("viz_reduction"), "Reduction a Visualiser",
                  choices  = c("UMAP"="umap","PCA"="pca","t-SNE"="tsne","Diffusion Map"="dm"),
                  selected = "umap")
    ),

    # Conditional: Scatter gene X / Y
    conditionalPanel(
      condition = "input.viz_type == 'scatter'", ns = ns,
      div(style="border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
          h6("Gene / Feature X", style="font-weight:bold;color:#2C3E50;"),
          selectizeInput(ns("scatter_gene1"), NULL, choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Ex: CD4"))),
      div(style="border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
          h6("Gene / Feature Y", style="font-weight:bold;color:#2C3E50;"),
          selectizeInput(ns("scatter_gene2"), NULL, choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Ex: CD8A"))),
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;",
          radioButtons(ns("scatter_cor_method"), "Methode",
                       choices = c("Pearson"="pearson","Spearman"="spearman"),
                       selected = "pearson", inline = TRUE),
          checkboxInput(ns("scatter_smooth"), "Ligne de tendance", value = TRUE))
    ),

    # Conditional: Multi-sample
    conditionalPanel(
      condition = "input.viz_type == 'multi_sample'", ns = ns,
      selectizeInput(ns("multi_gene"), "Gene a Comparer", choices = NULL, multiple = FALSE),
      radioButtons(ns("multi_plot_type"), "Type",
                   choices = c("Violin"="violin","Box"="box","Jitter"="jitter"),
                   inline = TRUE)
    ),

    # Group-by
    div(style="display:flex;align-items:center;justify-content:space-between;",
        tags$label("Grouper/Colorer par:", class="control-label"),
        tooltip(bsicons::bs_icon("info-circle"), "Variable de métadonnées pour groupement")),
    selectInput(ns("group_by"), NULL, choices = NULL),

    # Gene basket (hidden for scatter / multi_sample / volcano)
    conditionalPanel(
      condition = "input.viz_type != 'scatter' && input.viz_type != 'multi_sample' && input.viz_type != 'volcano'",
      ns = ns,
      div(
        class = "border-bottom pb-2 mb-2",
        selectizeInput(ns("feat_sel"), "Genes a Visualiser",
                       choices = NULL, multiple = TRUE,
                       options = list(maxOptions=5000, placeholder="Ex: CD4, PTPRC", maxItems=10)),
        div(class="small text-muted mb-2", textOutput(ns("gene_selection_status"))),
        actionButton(ns("clear_viz_genes"), "Vider", class="btn-outline-danger btn-sm w-100")
      )
    ),

    # Violin: boxplot overlay
    conditionalPanel(
      condition = "input.viz_type == 'violin'", ns = ns,
      checkboxInput(ns("violin_boxplot"), "Superposer Boxplot", value = FALSE)
    ),

    # Volcano settings
    conditionalPanel(
      condition = "input.viz_type == 'volcano'", ns = ns,
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
          h6("Groupes de Comparaison", style="font-weight:bold;"),
          selectInput(ns("volcano_group1"), "Group 1 (Test)",      choices = NULL),
          selectInput(ns("volcano_group2"), "Group 2 (Reference)", choices = c("All other cells"="rest")),
          helpText("Compare Group 1 vs Group 2 (ou 'rest' pour vs tous les autres)")),
      fluidRow(
        column(6, numericInput(ns("volcano_logfc"), "Log2FC Threshold", value=0.25, step=0.05)),
        column(6, numericInput(ns("volcano_pval"),  "P-adj Threshold",  value=0.05, step=0.001))
      ),
      checkboxInput(ns("volcano_show_labels"), "Afficher labels gènes sig.", value=TRUE),
      actionButton(ns("volcano_add_sig"), "-> Add Sig. Genes to Viz",
                   class="btn-sm btn-success w-100")
    ),

    sliderInput(ns("pt_size"), "Taille points", 0.1, 3, 0.5, 0.1),

    hr(),

    # ── Palette ─────────────────────────────────────────────────────────────
    div(style="display:flex;align-items:center;gap:6px;",
        tags$label("🎨 Palette couleur", class="control-label", style="margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
                "Appliqué aux groupes/clusters colorés. Okabe-Ito = sûre pour daltoniens.")),
    selectInput(ns("sc_palette"), NULL,
                choices = c("Défaut (Seurat/ggplot)" = "default",
                            "Okabe-Ito (daltonien)"  = "okabeito",
                            "Viridis"                = "viridis",
                            "Set2 (ColorBrewer)"     = "set2",
                            "Manuel"                 = "manual")),
    uiOutput(ns("sc_manual_palette_ui"))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_viz_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "750px",
    div(
      class = "card-header bg-light",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5("Visualisation Interactive", class="card-title mb-0"),
        div(
          # "Add to report" basket button
          actionButton(ns("add_to_report"), "📌 Ajouter au Rapport",
                       class="btn-sm btn-outline-dark me-1"),
          # Export popover
          popover(
            trigger = actionButton(ns("plot_settings"), "Personnaliser",
                                   class="btn-sm btn-outline-primary"),
            title = "Export",
            selectInput(ns("plot_theme"), "Thème",
                        choices = c("Minimal"="minimal","Classique"="classic",
                                    "BW"="bw","Vide"="void")),
            numericInput(ns("plot_width"),  "Largeur", 800, min=400, max=2000),
            numericInput(ns("plot_height"), "Hauteur", 600, min=300, max=1500),
            selectInput(ns("export_format"), "Format",
                        choices = c("PNG"="png","PDF"="pdf")),
            downloadButton(ns("export_plot"), "Exporter")
          )
        )
      )
    ),
    div(style="height:650px;overflow:auto;",
        uiOutput(ns("plot_container")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive: ggplot for export ──────────────────────────────────────────
    current_plot <- reactiveVal(NULL)

    # ── Helper: config snapshot (used ONLY for report basket — NOT for renders
    #    to avoid retriggerring expensive computations like FindMarkers when
    #    unrelated inputs change) ─────────────────────────────────────────────
    .current_cfg <- function() {
      list(
        type               = input$viz_type,
        reduction          = input$viz_reduction,
        feat_sel           = input$feat_sel,
        group_by           = input$group_by,
        scatter_gene1      = input$scatter_gene1,
        scatter_gene2      = input$scatter_gene2,
        scatter_cor_method = input$scatter_cor_method,
        scatter_smooth     = input$scatter_smooth,
        multi_gene         = input$multi_gene,
        multi_plot_type    = input$multi_plot_type,
        violin_boxplot     = input$violin_boxplot,
        volcano_group1     = input$volcano_group1,
        volcano_group2     = input$volcano_group2,
        volcano_logfc      = input$volcano_logfc,
        volcano_pval       = input$volcano_pval,
        volcano_show_labels= input$volcano_show_labels,
        pt_size            = input$pt_size,
        plot_theme         = input$plot_theme
      )
    }

    # ── Helper: add genes to viz basket ─────────────────────────────────────
    add_genes_to_viz <- function(genes_input) {
      req(global_data$sc_obj)
      clean <- trimws(unlist(strsplit(genes_input, "[, \n\t]+")))
      clean <- clean[nchar(clean) > 0]
      if (!length(clean)) { showNotification("No valid genes provided", type="warning", duration=3); return(invisible(NULL)) }
      valid   <- intersect(clean, rownames(global_data$sc_obj))
      invalid <- setdiff(clean, valid)
      current <- isolate(input$feat_sel) %||% character(0)
      new_sel <- unique(c(current, valid))
      updateSelectizeInput(session, "feat_sel", selected=new_sel,
                           choices=rownames(global_data$sc_obj), server=TRUE)
      shared_rv$selected_genes <- new_sel
      if (length(valid) > 0) {
        showNotification(sprintf("Added %d gene(s) (%d invalid skipped)", length(valid), length(invalid)),
                         type="message", duration=5)
        shared_rv$active_tab <- "tab_viz"
      } else {
        showNotification(paste("No valid genes. Invalid:", paste(invalid, collapse=", ")),
                         type="error", duration=5)
      }
      invisible(valid)
    }

    # ── Palette: group levels for manual picker ──────────────────────────────
    sc_group_levels <- reactive({
      req(global_data$sc_obj, input$group_by)
      meta <- global_data$sc_obj@meta.data
      if (!input$group_by %in% names(meta)) return(character(0))
      sort(unique(na.omit(as.character(meta[[input$group_by]]))))
    })

    output$sc_manual_palette_ui <- renderUI({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      lvls <- tryCatch(sc_group_levels(), error = function(e) character(0))
      if (!length(lvls)) {
        return(div(class="alert alert-warning", style="font-size:0.8em;",
                   "Sélectionnez une variable 'Grouper par' pour personnaliser les couleurs."))
      }
      ids <- paste0("sc_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(paste("Couleurs manuelles —", input$group_by),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    sc_manual_colors_vec <- reactive({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      lvls <- tryCatch(sc_group_levels(), error = function(e) character(0))
      if (!length(lvls)) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("sc_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    # ── Sync gene basket from sibling modules ────────────────────────────────
    observeEvent(shared_rv$selected_genes, {
      req(global_data$sc_obj)
      genes <- shared_rv$selected_genes
      if (length(genes) > 0)
        updateSelectizeInput(session, "feat_sel", selected=genes,
                             choices=rownames(global_data$sc_obj), server=TRUE)
    }, ignoreInit = TRUE)

    # ── Refresh UI choices on new sc_obj ────────────────────────────────────
    .safe_setnames <- function(values, labels) {
      labels <- as.character(labels); values <- as.character(values)
      n <- length(values)
      if (length(labels) != n) labels <- labels[seq_len(n)]
      setNames(values, labels)
    }

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data

      # Group-by
      valid_cols <- names(meta)[vapply(meta, function(x) is.factor(x)||is.character(x), logical(1))]
      valid_cols <- unique(c("seurat_clusters","orig.ident", valid_cols))
      valid_cols <- valid_cols[valid_cols %in% names(meta)]
      cur_grp    <- isolate(input$group_by)
      sel_grp    <- if (!is.null(cur_grp) && cur_grp %in% valid_cols) cur_grp else "seurat_clusters"
      updateSelectInput(session, "group_by", choices=valid_cols, selected=sel_grp)

      # Gene pickers
      var_features <- tryCatch(VariableFeatures(obj), error=function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "feat_sel",      choices=gene_choices, server=TRUE)
      updateSelectizeInput(session, "scatter_gene1", choices=rownames(obj), server=TRUE)
      updateSelectizeInput(session, "scatter_gene2", choices=rownames(obj), server=TRUE)
      updateSelectizeInput(session, "multi_gene",    choices=gene_choices,  server=TRUE)

      # Reduction picker
      avail_red  <- names(obj@reductions)
      pref_red   <- c("umap","umap_harmony","pca","tsne","dm")
      red_choices <- avail_red[avail_red %in% pref_red]
      if (!length(red_choices)) red_choices <- avail_red
      updateSelectInput(session, "viz_reduction",
                        choices  = red_choices,
                        selected = if ("umap" %in% red_choices) "umap" else red_choices[1])
    }, ignoreInit = TRUE)

    # ── FIX: Volcano group pickers — separate reactive on BOTH sc_obj AND
    #    group_by. The previous version used isolate(input$group_by) inside
    #    the sc_obj-only observer, which read a stale value (the picker was
    #    never properly populated on first load or after group_by changes). ──
    observeEvent(list(global_data$sc_obj, input$group_by), {
      req(global_data$sc_obj, input$group_by)
      meta    <- global_data$sc_obj@meta.data
      grp_col <- input$group_by
      if (!grp_col %in% names(meta)) return(invisible(NULL))

      idents_levels <- tryCatch({
        lvls <- levels(factor(as.character(meta[[grp_col]])))
        lvls[nchar(lvls) > 0]
      }, error = function(e) character(0))

      if (length(idents_levels) < 2) return(invisible(NULL))

      cur_g1 <- isolate(input$volcano_group1)
      updateSelectInput(session, "volcano_group1", choices = idents_levels,
                        selected = if (!is.null(cur_g1) && cur_g1 %in% idents_levels) cur_g1
                                   else idents_levels[1])

      v2_choices <- c("All other cells"="rest",
                      .safe_setnames(idents_levels, paste("Group:", idents_levels)))
      cur_g2 <- isolate(input$volcano_group2)
      updateSelectInput(session, "volcano_group2", choices = v2_choices,
                        selected = if (!is.null(cur_g2) && cur_g2 %in% unname(v2_choices)) cur_g2
                                   else "rest")
    }, ignoreInit = TRUE)

    # ── Plot container router ────────────────────────────────────────────────
    output$plot_container <- renderUI({
      if (isTRUE(input$viz_type %in% c("heatmap", "stacked_violin")))
        plotOutput(ns("plot_static"), height="600px")
      else
        plotlyOutput(ns("plot_interactive"), height="600px")
    })

    # ── Static render (heatmap / stacked_violin) ─────────────────────────────
    output$plot_static <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      type <- input$viz_type
      if (!type %in% c("heatmap", "stacked_violin")) return(NULL)

      cfg <- list(type=type, feat_sel=input$feat_sel, group_by=input$group_by,
                  pt_size=input$pt_size, plot_theme=input$plot_theme)

      p <- tryCatch(
        build_sc_viz_plot(obj, cfg, input$sc_palette, sc_manual_colors_vec()),
        error = function(e)
          ggplot() + annotate("text",x=1,y=1,label=paste("Erreur:", e$message)) + theme_void()
      )
      current_plot(p)
      p
    })

    # ── Interactive render (all other types) ─────────────────────────────────
    output$plot_interactive <- renderPlotly({
      req(global_data$sc_obj, input$group_by)
      obj  <- global_data$sc_obj
      type <- input$viz_type

      if (type %in% c("heatmap","stacked_violin")) return(plotly_empty())

      cfg <- list(
        type               = type,
        reduction          = input$viz_reduction,
        feat_sel           = input$feat_sel,
        group_by           = input$group_by,
        scatter_gene1      = input$scatter_gene1,
        scatter_gene2      = input$scatter_gene2,
        scatter_cor_method = input$scatter_cor_method,
        scatter_smooth     = input$scatter_smooth,
        multi_gene         = input$multi_gene,
        multi_plot_type    = input$multi_plot_type,
        violin_boxplot     = input$violin_boxplot,
        volcano_group1     = input$volcano_group1,
        volcano_group2     = input$volcano_group2,
        volcano_logfc      = input$volcano_logfc,
        volcano_pval       = input$volcano_pval,
        volcano_show_labels= input$volcano_show_labels,
        pt_size            = input$pt_size,
        plot_theme         = input$plot_theme
      )

      p_gg <- tryCatch(
        build_sc_viz_plot(obj, cfg, input$sc_palette, sc_manual_colors_vec()),
        error = function(e)
          ggplot() + annotate("text",x=1,y=1,
                              label=paste("Erreur:", e$message), color="red") + theme_void()
      )
      current_plot(p_gg)   # always set ggplot for export, regardless of display mode

      # Volcano: native plotly for richer hover (reuses markers from attr to
      # avoid a second FindMarkers call)
      if (type == "volcano") {
        markers <- attr(p_gg, "volcano_markers")
        vtitle  <- attr(p_gg, "volcano_title") %||% "Volcano"
        if (!is.null(markers)) {
          lfc       <- as.numeric(input$volcano_logfc %||% 0.25)
          pval      <- as.numeric(input$volcano_pval  %||% 0.05)
          pt_sz     <- as.numeric(input$pt_size %||% 0.5) * 5
          color_map <- c("Up"="#E74C3C","Down"="#2980B9","NS"="#BDC3C7")
          x_rng     <- range(markers$avg_log2FC, na.rm=TRUE)
          return(
            plot_ly(data=markers, x=~avg_log2FC, y=~-log10(p_val_adj+1e-300),
                    type="scatter", mode="markers+text",
                    marker=list(color=~color_map[status], size=pt_sz, opacity=0.75,
                                line=list(width=0)),
                    text=~label, textposition="top center",
                    hovertext=~paste0("<b>",gene,"</b><br>Log2FC: ",round(avg_log2FC,3),
                                      "<br>-log10(padj): ",round(-log10(p_val_adj+1e-300),2),
                                      "<br>Status: ",status),
                    hoverinfo="text") |>
              layout(title=vtitle,
                     xaxis=list(title="Log2 Fold Change", zeroline=TRUE),
                     yaxis=list(title="-log10(P.adj)"),
                     shapes=list(
                       list(type="line",x0=lfc,x1=lfc,y0=0,y1=1,yref="paper",
                            line=list(dash="dot",color="#E74C3C",width=1)),
                       list(type="line",x0=-lfc,x1=-lfc,y0=0,y1=1,yref="paper",
                            line=list(dash="dot",color="#2980B9",width=1)),
                       list(type="line",x0=x_rng[1],x1=x_rng[2],
                            y0=-log10(pval),y1=-log10(pval),
                            line=list(dash="dot",color="#7F8C8D",width=1))
                     ), showlegend=FALSE)
          )
        }
        # Fallback to ggplotly if markers attr missing
        return(tryCatch(suppressWarnings(ggplotly(p_gg, tooltip="text")),
                        error=function(e) plotly_empty()))
      }

      tryCatch(suppressWarnings(ggplotly(p_gg, tooltip="text")),
               error = function(e) plotly_empty())
    })

    # ── Export ───────────────────────────────────────────────────────────────
    output$export_plot <- downloadHandler(
      filename = function() paste0("plot_", Sys.Date(), ".", input$export_format %||% "png"),
      content  = function(file) {
        req(current_plot())
        ggsave(file, plot=current_plot(),
               width  = (input$plot_width  %||% 800) / 100,
               height = (input$plot_height %||% 600) / 100,
               dpi    = 300)
      }
    )

    # ── "📌 Ajouter au Rapport" ──────────────────────────────────────────────
    observeEvent(input$add_to_report, {
      req(global_data$sc_obj)
      cfg   <- .current_cfg()
      title <- paste0(cfg$type, "_", format(Sys.time(), "%H%M%S"))
      current <- shared_rv$report_viz_list %||% list()
      current[[title]] <- cfg
      shared_rv$report_viz_list <- current
      showNotification(paste("📌 Ajouté au rapport:", cfg$type),
                       type="message", duration=3)
    })

    # ── Volcano: add significant genes to viz basket ──────────────────────────
    observeEvent(input$volcano_add_sig, {
      req(current_plot())
      markers <- attr(current_plot(), "volcano_markers")
      if (is.null(markers)) {
        showNotification("Générez d'abord le Volcano Plot.", type="warning"); return()
      }
      lfc  <- as.numeric(input$volcano_logfc %||% 0.25)
      pval <- as.numeric(input$volcano_pval  %||% 0.05)
      sig  <- markers$gene[markers$p_val_adj < pval & abs(markers$avg_log2FC) > lfc]
      if (!length(sig)) {
        showNotification("Aucun gène significatif.", type="warning"); return()
      }
      current <- shared_rv$selected_genes %||% character(0)
      shared_rv$selected_genes <- unique(c(current, sig))
      shared_rv$active_tab     <- "tab_viz"
      showNotification(paste(length(sig), "gènes significatifs ajoutés."), type="message")
    })

    # ── Gene count status ────────────────────────────────────────────────────
    output$gene_selection_status <- renderText({
      n <- length(input$feat_sel %||% character(0))
      if (n == 0) "Aucun gène sélectionné" else paste(n, "gène(s) sélectionné(s)")
    })

    # ── Clear gene selection ─────────────────────────────────────────────────
    observeEvent(input$clear_viz_genes, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      var_features <- tryCatch(VariableFeatures(obj), error=function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "feat_sel", choices=gene_choices,
                           selected=character(0), server=TRUE)
    })

  }) # /moduleServer
}
