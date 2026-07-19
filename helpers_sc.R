# =============================================================================
# helpers_sc.R — Single-cell plotting & analysis helpers (Seurat)
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure functions
# used by the mod_sc_* family (mod_sc_viz.R, mod_sc_corr.R, mod_sc_markers.R,
# mod_sc_trajectory.R). No Shiny reactivity here.
#
# Contents:
#   - Plots      : plot_enhanced_scatter(), plot_violin_enhanced(),
#                  plot_multi_sample(), plot_correlation_matrix(),
#                  plot_trajectory(), plot_gene_correlation_network(),
#                  plot_pseudotime_distribution(), plot_genes_vs_pseudotime()
#   - Analysis   : find_correlated_genes(), calculate_pseudotime(),
#                  subsample_seurat_for_analysis()
#   - DT tables  : build_markers_dt(), build_corr_dt()
#   - Internal   : .get_norm_matrix() (Seurat v4/v5-safe assay extraction)
#
# Pathway enrichment (ORA/GSEA) is shared with bulk — see helpers_pathway.R,
# NOT duplicated here even though it's used by mod_sc_pathways.R too.
#
# Step-3.7 note: the SC visualization builder build_sc_viz_plot(obj, cfg,
# sc_palette, manual_colors) lives ONLY in mod_sc_viz.R now. A duplicate
# 2-arg build_sc_viz_plot(obj, cfg) used to live here too — since app.R
# source()s helpers_sc.R BEFORE mod_sc_viz.R, the later definition always
# silently shadowed this one (same global env), making it 100% dead code.
# Removed to avoid confusion/drift between the two copies.
#
# Depends on: Seurat, ggplot2, igraph, viridis, scales, DT, tidyr.
# =============================================================================







plot_enhanced_scatter <- function(seurat_obj, feature1, feature2, 

                                  group.by = NULL, method = "pearson",

                                  add_smooth = TRUE, pt.size = 1) {

  

  if(!feature1 %in% rownames(seurat_obj) && !feature1 %in% colnames(seurat_obj@meta.data)) {

    stop(paste("Feature non trouvée:", feature1))

  }

  if(!feature2 %in% rownames(seurat_obj) && !feature2 %in% colnames(seurat_obj@meta.data)) {

    stop(paste("Feature non trouvée:", feature2))

  }

  

  if(feature1 %in% rownames(seurat_obj)) {

    data1 <- FetchData(seurat_obj, vars = feature1)[,1]

  } else {

    data1 <- seurat_obj@meta.data[[feature1]]

  }

  

  if(feature2 %in% rownames(seurat_obj)) {

    data2 <- FetchData(seurat_obj, vars = feature2)[,1]

  } else {

    data2 <- seurat_obj@meta.data[[feature2]]

  }

  

  plot_df <- data.frame(

    x = data1,

    y = data2,

    stringsAsFactors = FALSE

  )

  

  if(!is.null(group.by) && group.by %in% colnames(seurat_obj@meta.data)) {

    plot_df$group <- seurat_obj@meta.data[[group.by]]

  } else {

    plot_df$group <- "All Cells"

  }

  

  cor_test <- cor.test(plot_df$x, plot_df$y, method = method)

  cor_value <- round(cor_test$estimate, 3)

  p_value <- format(cor_test$p.value, scientific = TRUE, digits = 3)

  

  stat_label <- paste0(

    toupper(method), " r = ", cor_value, 

    "\np-value = ", p_value

  )

  

  p <- ggplot(plot_df, aes(x = x, y = y, color = group)) +

    geom_point(size = pt.size, alpha = 0.6) +

    labs(

      x = feature1,

      y = feature2,

      title = paste("Corrélation:", feature1, "vs", feature2),

      color = if(!is.null(group.by)) group.by else NULL

    ) +

    theme_minimal() +

    theme(

      plot.title = element_text(face = "bold", size = 14),

      legend.position = "right",

      panel.grid.minor = element_blank()

    )

  

  if(add_smooth) {

    p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", 

                         linetype = "dashed", linewidth = 0.8,

                         aes(group = 1))

  }

  

  p <- p + annotate("text", 

                    x = min(plot_df$x) + 0.1 * diff(range(plot_df$x)),

                    y = max(plot_df$y) - 0.05 * diff(range(plot_df$y)),

                    label = stat_label,

                    hjust = 0, vjust = 1,

                    size = 4, fontface = "bold",

                    color = "black")

  

  return(p)

}



plot_violin_enhanced <- function(seurat_obj, features, group.by = "seurat_clusters",

                                 add_boxplot = TRUE, split.by = NULL) {

  

  valid_features <- intersect(features, rownames(seurat_obj))

  if(length(valid_features) == 0) {

    stop("Aucun gène valide trouvé")

  }

  

  features_use <- head(valid_features, 6)

  

  p <- VlnPlot(

    seurat_obj, 

    features = features_use,

    group.by = group.by,

    split.by = split.by,

    pt.size = 0,

    ncol = min(3, length(features_use))

  )

  

  if(add_boxplot && length(features_use) == 1) {

    data_plot <- FetchData(seurat_obj, vars = c(features_use[1], group.by))

    colnames(data_plot) <- c("expression", "group")

    

    p <- ggplot(data_plot, aes(x = group, y = expression, fill = group)) +

      geom_violin(trim = FALSE, alpha = 0.7, scale = "width") +

      geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA, 

                   alpha = 0.8, coef = 0) +

      labs(title = paste("Expression de", features_use[1]),

           x = group.by, y = "Expression Normalisée") +

      theme_minimal() +

      theme(

        axis.text.x = element_text(angle = 45, hjust = 1),

        legend.position = "none",

        plot.title = element_text(face = "bold")

      )

  }

  

  return(p)

}



plot_multi_sample <- function(seurat_obj, gene, plot_type = "violin") {

  

  if(!gene %in% rownames(seurat_obj)) {

    stop(paste("Gène non trouvé:", gene))

  }

  

  n_samples <- length(unique(seurat_obj$orig.ident))

  if(n_samples < 2) {

    stop("Au moins 2 échantillons requis pour la comparaison")

  }

  

  plot_data <- data.frame(

    expression = FetchData(seurat_obj, vars = gene)[,1],

    sample = seurat_obj$orig.ident,

    cluster = Idents(seurat_obj)

  )

  

  p <- ggplot(plot_data, aes(x = sample, y = expression, fill = sample)) +

    labs(title = paste("Expression de", gene, "par Échantillon"),

         x = "Échantillon", y = "Expression Normalisée") +

    theme_minimal() +

    theme(

      axis.text.x = element_text(angle = 45, hjust = 1),

      legend.position = "none",

      plot.title = element_text(face = "bold", size = 14)

    )

  

  if(plot_type == "violin") {

    p <- p + geom_violin(trim = FALSE, alpha = 0.7) +

      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA)

  } else if(plot_type == "box") {

    p <- p + geom_boxplot(outlier.alpha = 0.3)

  } else if(plot_type == "jitter") {

    p <- p + geom_jitter(width = 0.2, alpha = 0.4, size = 0.5)

  }

  

  return(p)

}



#' Safe normalized matrix extraction (Seurat v4/v5 compatible)

.get_norm_matrix <- function(obj) {

  assay_use <- DefaultAssay(obj)

  mat <- tryCatch(

    GetAssayData(obj, layer = "data", assay = assay_use),

    error = function(e) GetAssayData(obj, slot = "data", assay = assay_use)

  )

  if (inherits(mat, "dgCMatrix")) mat <- as.matrix(mat)

  mat

}






#' Find Correlated Genes

#' @param seurat_obj Objet Seurat

#' @param target_gene Gène cible

#' @param method Méthode de corrélation ("pearson", "spearman")

#' @param threshold Seuil de corrélation (absolue)

#' @param top_n Nombre maximum de gènes à retourner

#' @return data.frame avec gènes corrélés et statistiques

find_correlated_genes <- function(seurat_obj, target_gene, 

                                  method = "pearson", 

                                  threshold = 0.3, 

                                  top_n = 50) {

  

  # Validation

  if(!target_gene %in% rownames(seurat_obj)) {

    stop(paste("Gène non trouvé:", target_gene))

  }

  

  # Extraire expression normalisée

  expr_matrix <- .get_norm_matrix(seurat_obj)

  

  # Convertir en matrice dense si sparse

  if(inherits(expr_matrix, "dgCMatrix")) {

    expr_matrix <- as.matrix(expr_matrix)

  }

  

  # Expression du gène cible

  target_expr <- as.numeric(expr_matrix[target_gene, ])

  

  # Calculer corrélations avec tous les autres gènes

  all_genes <- rownames(expr_matrix)

  all_genes <- setdiff(all_genes, target_gene)

  

  # Pour performance, limiter aux gènes variables si > 5000 gènes

  if(length(all_genes) > 5000) {

    var_genes <- VariableFeatures(seurat_obj)

    if(length(var_genes) > 0) {

      all_genes <- intersect(all_genes, var_genes)

    }

  }

  

  # FIX: Calcul avec gestion des ex-aequos

  cor_results <- lapply(all_genes, function(g) {

    gene_expr <- as.numeric(expr_matrix[g, ])

    

    # Éviter gènes avec variance nulle

    if(sd(gene_expr) == 0 || sd(target_expr) == 0) {

      return(data.frame(cor = 0, pval = 1))

    }

    

    # Utiliser exact=FALSE pour éviter warning avec ex-aequos

    test <- tryCatch({

      cor.test(target_expr, gene_expr, method = method, exact = FALSE)

    }, error = function(e) {

      list(estimate = 0, p.value = 1)

    })

    

    data.frame(

      cor = as.numeric(test$estimate),

      pval = as.numeric(test$p.value)

    )

  })

  

  # Combiner résultats

  cor_df <- do.call(rbind, cor_results)

  cor_df$gene <- all_genes

  cor_df$abs_correlation <- abs(cor_df$cor)

  colnames(cor_df)[1:2] <- c("correlation", "p_value")

  

  # Filtrer par seuil

  cor_df <- cor_df[cor_df$abs_correlation >= threshold, ]

  

  # Ajuster p-values

  if(nrow(cor_df) > 0) {

    cor_df$p_adj <- p.adjust(cor_df$p_value, method = "BH")

  } else {

    return(data.frame(

      gene = character(0),

      correlation = numeric(0),

      p_value = numeric(0),

      abs_correlation = numeric(0),

      p_adj = numeric(0)

    ))

  }

  

  # Trier et limiter

  cor_df <- cor_df[order(-cor_df$abs_correlation), ]

  cor_df <- head(cor_df, top_n)

  

  return(cor_df)

}



#' Correlation Matrix Plot

#' @param seurat_obj Objet Seurat

#' @param features Vecteur de gènes (max 50)

#' @param method Méthode corrélation

#' @return ggplot heatmap

plot_correlation_matrix <- function(seurat_obj, features, method = "pearson",

                                    low_color = "#2166AC", mid_color = "white",

                                    high_color = "#B2182B") {

  

  # Validation

  features <- intersect(features, rownames(seurat_obj))

  if(length(features) < 2) {

    stop("Au moins 2 gènes requis")

  }

  

  # Limiter à 50 gènes pour performance

  if(length(features) > 50) {

    warning("Limitation à 50 gènes pour performance")

    features <- head(features, 50)

  }

  

  # Extraire expression

  expr_matrix <- .get_norm_matrix(seurat_obj)

  

  # Sous-ensemble et transposer

  expr_subset <- as.matrix(t(expr_matrix[features, ]))

  

  # Calculer matrice corrélation

  cor_matrix <- cor(expr_subset, method = method, use = "pairwise.complete.obs")

  

  # Convertir pour ggplot

  cor_melt <- reshape2::melt(cor_matrix)

  colnames(cor_melt) <- c("Gene1", "Gene2", "Correlation")

  

  # Plot

  p <- ggplot(cor_melt, aes(x = Gene1, y = Gene2, fill = Correlation)) +

    geom_tile(color = "white", size = 0.5) +

    geom_text(aes(label = sprintf("%.2f", Correlation)), 

              size = 3, color = "black") +

    scale_fill_gradient2(

      low = low_color, mid = mid_color, high = high_color,

      midpoint = 0, limits = c(-1, 1),

      name = "Corrélation"

    ) +

    labs(

      title = paste("Matrice de Corrélation (", method, ")", sep = ""),

      x = NULL, y = NULL

    ) +

    theme_minimal() +

    theme(

      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),

      axis.text.y = element_text(size = 9),

      plot.title = element_text(face = "bold", size = 14),

      panel.grid = element_blank(),

      legend.position = "right"

    ) +

    coord_fixed()

  

  return(p)

}



#' Trajectory Analysis (k-NN shortest path)

#' @param seurat_obj Objet Seurat

#' @param reduction Réduction à utiliser ("umap", "pca")

#' @param root_cells Indices des cellules racines (optionnel)

#' @return Objet Seurat avec $pseudotime

calculate_pseudotime <- function(seurat_obj, reduction = "umap", root_cells = NULL) {
  if (!reduction %in% names(seurat_obj@reductions))
    stop(paste("Réduction", reduction, "non trouvée"))
  
  embeddings <- Embeddings(seurat_obj, reduction = reduction)
  if (!requireNamespace("igraph", quietly = TRUE)) stop("Package igraph requis")
  
  dist_mat <- as.matrix(dist(embeddings))
  k <- min(10, ncol(seurat_obj) - 1)
  edge_list <- c()
  for (i in 1:nrow(dist_mat)) {
    neighbors <- order(dist_mat[i, ])[2:(k+1)]
    for (j in neighbors) edge_list <- c(edge_list, i, j)
  }
  
  g <- igraph::graph(edges = edge_list, n = ncol(seurat_obj), directed = FALSE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  # FIX: a k-NN graph on well-separated clusters (small test datasets) can be
  # disconnected. igraph::distances() then returns Inf across components,
  # which used to corrupt the min/max normalization below -- every reachable
  # cell collapsed near 0 and the rest became NaN (empty distribution plot,
  # trajectory colored by a single value). Now: root cell is picked within
  # its own component, cells outside it get NA (excluded downstream by
  # ggplot's na.rm) instead of a corrupted value.
  comp <- igraph::components(g)
  if (comp$no > 1) {
    warning(sprintf(
      "Graphe de trajectoire deconnecte (%d composantes) -- pseudotemps calcule uniquement sur la composante de la cellule racine ; les autres cellules recoivent NA.",
      comp$no
    ))
  }
  
  if (is.null(root_cells)) {
    main_comp  <- which.max(comp$csize)
    comp_cells <- which(comp$membership == main_comp)
    centrality <- igraph::closeness(g, vids = comp_cells)
    root_cell  <- comp_cells[which.max(centrality)]
  } else {
    root_cell <- root_cells[1]
  }
  
  root_comp <- comp$membership[root_cell]
  same_comp <- which(comp$membership == root_comp)
  
  pseudotime <- rep(NA_real_, ncol(seurat_obj))
  d   <- igraph::distances(g, v = root_cell, to = same_comp)[1, ]
  rng <- range(d, finite = TRUE)
  pseudotime[same_comp] <- if (diff(rng) > 0) (d - rng[1]) / diff(rng) else 0
  
  seurat_obj$pseudotime <- pseudotime
  seurat_obj
}



#' Plot Trajectory

#' @param seurat_obj Objet Seurat avec pseudotime

#' @param reduction Réduction à visualiser

#' @param color_by Variable de coloration ("pseudotime" ou metadata)

#' @return ggplot object

plot_trajectory <- function(seurat_obj, reduction = "umap", color_by = "pseudotime") {

  if (!reduction %in% names(seurat_obj@reductions))

    stop(paste("Réduction", reduction, "non trouvée"))

  

  embeddings <- Embeddings(seurat_obj, reduction = reduction)

  color_val <- if (color_by %in% colnames(seurat_obj@meta.data)) {

    seurat_obj@meta.data[[color_by]]

  } else {

    warning(paste("Colonne", color_by, "introuvable"))

    rep("unknown", ncol(seurat_obj))

  }

  

  plot_data <- data.frame(x = embeddings[, 1], y = embeddings[, 2], value = color_val)

  

  p <- ggplot(plot_data, aes(x = x, y = y, color = value)) +

    geom_point(size = 1.5, alpha = 0.7) +

    labs(title = paste("Trajectory —", toupper(reduction)),

         x = paste0(toupper(reduction), "_1"),

         y = paste0(toupper(reduction), "_2"),

         color = color_by) +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14), legend.position = "right")

  

  if (color_by == "pseudotime" && is.numeric(plot_data$value))

    p <- p + scale_color_viridis_c(option = "plasma")

  p

}



#' Gene correlation network plot (igraph), factored out of mod_sc_corr.R for

#' reuse by the Single-Cell report — one source of truth for the network

#' rendering so the in-app plot and the exported report never diverge.

#'

#' @param corr_df data.frame from find_correlated_genes() (gene, correlation, abs_correlation).

#' @param target_gene Character, the reference gene at the center of the network.

#' @param top_n Number of top-correlated genes to display as nodes.

plot_gene_correlation_network <- function(corr_df, target_gene, top_n = 20) {

  top <- head(corr_df, top_n)

  edges <- data.frame(

    from        = target_gene,

    to          = top$gene,

    weight      = top$abs_correlation,

    correlation = top$correlation

  )

  g <- igraph::graph_from_data_frame(edges, directed = FALSE)



  edge_colors <- ifelse(edges$correlation > 0, "#27AE60", "#E74C3C")

  edge_widths <- scales::rescale(edges$weight, to = c(1, 5))

  layout      <- igraph::layout_with_fr(g)

  node_sizes  <- ifelse(igraph::V(g)$name == target_gene, 15, 8)

  node_colors <- ifelse(igraph::V(g)$name == target_gene, "#3498DB", "#95A5A6")



  par(mar = c(0, 0, 2, 0))

  plot(

    g,

    layout              = layout,

    vertex.size         = node_sizes,

    vertex.color        = node_colors,

    vertex.label.cex    = 0.7,

    vertex.label.color  = "black",

    vertex.label.family = "sans",

    edge.color          = edge_colors,

    edge.width          = edge_widths,

    main                = paste("Reseau de Correlation -", target_gene),

    edge.curved         = 0.2

  )

  legend("bottomleft",

         legend = c("Correlation positive", "Correlation negative", "Gene cible"),

         col    = c("#27AE60", "#E74C3C", "#3498DB"),

         pch    = c(15, 15, 19), pt.cex = c(2, 2, 2.5),

         bty = "n", cex = 0.9)

}



#' Standardized FindAllMarkers results DT table (shared by mod_sc_markers.R

#' and the future Single-Cell HTML/PDF report — same rationale as

#' build_de_results_dt: one source of truth for table formatting).

#'

#' @param df Markers data.frame (gene, cluster, avg_log2FC, p_val_adj, pct.1, pct.2).

#' @return A DT::datatable object.

build_markers_dt <- function(df) {

  df_display <- data.frame(

    Gene    = df$gene,

    Cluster = df$cluster,

    Log2FC  = round(df$avg_log2FC, 3),

    P_adj   = format(df$p_val_adj, scientific = TRUE, digits = 3),

    Pct.1   = round(df$pct.1 * 100, 1),

    Pct.2   = round(df$pct.2 * 100, 1),

    stringsAsFactors = FALSE

  )

  DT::datatable(df_display, filter = "top", rownames = FALSE,

                options = list(pageLength = 15, scrollX = TRUE)) %>%

    DT::formatStyle(

      "Log2FC",

      backgroundColor = DT::styleColorBar(range(df_display$Log2FC), "lightblue"),

      backgroundSize  = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center"

    ) %>%

    DT::formatStyle("P_adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

}



#' Standardized gene-correlation results DT table (Step-3.7) — mirrors

#' build_markers_dt(). Shared by mod_sc_corr.R's live table AND the new

#' "Table des Corrélations" report section (sc_report_template.Rmd), so the
#' exported table always matches what was shown live.

#'

#' @param df data.frame from find_correlated_genes() (gene, correlation, p_value, abs_correlation, p_adj).

#' @return A DT::datatable object.

build_corr_dt <- function(df) {

  df_display <- data.frame(

    Gene        = df$gene,

    Correlation = round(df$correlation, 3),

    `|r|`       = round(df$abs_correlation, 3),

    P_value     = format(df$p_value, scientific = TRUE, digits = 2),

    P_adj       = format(df$p_adj, scientific = TRUE, digits = 2),

    check.names = FALSE

  )

  DT::datatable(df_display, selection = list(mode = "none"), filter = "top", rownames = FALSE,

                options = list(pageLength = 15, scrollX = TRUE)) %>%

    DT::formatStyle("Correlation",

                    background = DT::styleColorBar(range(df_display$Correlation), "lightblue"),

                    backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",

                    backgroundPosition = "center") %>%

    DT::formatStyle("P_adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

}



#' Pseudotime distribution plot (density per cluster) — Step-3.7: factored

#' out of mod_sc_trajectory.R so the live plot, its PNG/PDF export, AND the

#' HTML/PDF report all call the exact same code (previous version had 3

#' near-identical ggplot blocks copy-pasted across renderPlot()/downloadHandler,

#' and the downloadHandler ones were broken — they called `output$xxx()` which

#' is not how you retrieve a renderPlot's underlying ggplot object).

#'

#' @param seurat_obj Seurat object with a $pseudotime + $seurat_clusters column.

#' @return ggplot object.

plot_pseudotime_distribution <- function(seurat_obj) {

  if (!"pseudotime" %in% colnames(seurat_obj@meta.data))

    stop("Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'.")



  df <- data.frame(

    pseudotime = seurat_obj@meta.data$pseudotime,

    cluster    = as.character(seurat_obj@meta.data$seurat_clusters)

  )

  df <- df[!is.na(df$pseudotime), ]   # cells outside the root component (disconnected graph)

  if (nrow(df) == 0) stop("Distribution non disponible (pseudotemps NA pour toutes les cellules).")



  ggplot(df, aes(x = pseudotime, fill = cluster)) +

    geom_density(alpha = 0.6) +

    scale_fill_viridis_d(option = "turbo") +

    labs(title = "Distribution du Pseudotemps par Cluster",

         x = "Pseudotemps", y = "Densité", fill = "Cluster") +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 13))

}



#' Gene expression vs pseudotime plot (Step-3.7) — factored out of

#' mod_sc_trajectory.R, single source of truth reused by the live render

#' (no more "Tracer" button — updates as soon as genes are picked), its

#' PNG/PDF export, AND the new "Gènes vs Pseudotemps" report section.

#'

#' @param seurat_obj Seurat object with a $pseudotime column already computed.

#' @param genes Character vector of gene symbols to trace (max 8 kept).

#' @param smooth_method "loess", "gam", or "lm".

#' @return ggplot object (faceted, one panel per gene).

plot_genes_vs_pseudotime <- function(seurat_obj, genes, smooth_method = "loess") {

  if (!"pseudotime" %in% colnames(seurat_obj@meta.data))

    stop("Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'.")



  valid_genes <- intersect(genes, rownames(seurat_obj))

  valid_genes <- head(valid_genes, 8)

  if (length(valid_genes) == 0) stop("Aucun gène valide sélectionné")



  expr_df   <- FetchData(seurat_obj, vars = c("pseudotime", valid_genes))

  expr_long <- tidyr::pivot_longer(expr_df, cols = -pseudotime,

                                   names_to = "gene", values_to = "expression")



  ggplot(expr_long, aes(x = pseudotime, y = expression, color = gene)) +

    geom_point(alpha = 0.3, size = 0.6) +

    geom_smooth(method = smooth_method, se = TRUE, linewidth = 0.9, na.rm = TRUE) +

    facet_wrap(~gene, scales = "free_y", ncol = 2) +

    scale_color_viridis_d(option = "turbo") +

    labs(title = "Expression génique le long du Pseudotemps",

         x = "Pseudotemps", y = "Expression Normalisée") +

    theme_minimal() +

    theme(legend.position = "none",

          plot.title = element_text(face = "bold", size = 13),

          strip.text = element_text(face = "bold"))

}



#' Cap the number of cells used for a heavy per-cell computation
#' (FindAllMarkers, find_correlated_genes, ...) — RAM/CPU safety net for large
#' Single-Cell objects on a 32Go CPU-only machine.
#'
#' Stratifies by `group_col` (default: cluster) rather than a flat random
#' subsample of the whole object, so rare cell populations are not wiped out
#' by a global cap. Falls back to a flat/global subsample if `group_col`
#' doesn't exist in the object's metadata (e.g. correlation subsampling by
#' "orig.ident" on a single-sample object).
#'
#' @param obj Seurat object.
#' @param max_per_group Max cells to keep per level of `group_col`.
#'   `Inf`, `NA`, or `<= 0` disables subsampling entirely (returns obj as-is).
#' @param group_col Metadata column to stratify by (default "seurat_clusters").
#' @param seed Random seed, for reproducibility across re-runs of the same step.
#' @return list(object, n_before, n_after, was_subsampled)
subsample_seurat_for_analysis <- function(obj, max_per_group = Inf,
                                          group_col = "seurat_clusters", seed = 1) {
  n_before <- ncol(obj)

  if (is.null(max_per_group) || is.na(max_per_group) || !is.finite(max_per_group) || max_per_group <= 0) {
    return(list(object = obj, n_before = n_before, n_after = n_before, was_subsampled = FALSE))
  }

  set.seed(seed)

  if (group_col %in% colnames(obj@meta.data)) {
    groups <- obj@meta.data[[group_col]]
    keep_cells <- unlist(lapply(split(Cells(obj), groups), function(cells) {
      if (length(cells) > max_per_group) sample(cells, max_per_group) else cells
    }), use.names = FALSE)
  } else {
    all_cells  <- Cells(obj)
    keep_cells <- if (length(all_cells) > max_per_group) sample(all_cells, max_per_group) else all_cells
  }

  n_after <- length(keep_cells)
  if (n_after >= n_before) {
    return(list(object = obj, n_before = n_before, n_after = n_before, was_subsampled = FALSE))
  }

  list(object = subset(obj, cells = keep_cells),
       n_before = n_before, n_after = n_after, was_subsampled = TRUE)
}



#' Remap a Seurat object's RNA assay rownames (Ensembl/Entrez) to gene symbols
#'
#' Mirrors remap_gene_ids_to_symbol() (helpers_io.R, used for Bulk) but is
#' RAM-safe for large sparse SC matrices: duplicate-symbol collapsing is done
#' via sparse indicator-matrix multiplication (t(G) %*% counts), never
#' densifying the (genes x cells) matrix -- unlike the Bulk version which
#' densifies (`mode(mat) <- "numeric"`), acceptable there because bulk
#' matrices are small. Rebuilds a FRESH Seurat object from raw counts only;
#' any existing normalization/PCA/UMAP/clusters must be recomputed after
#' (same constraint as Bulk Step-0 mapping vs Step-1 filter/VST) -- this MUST
#' run BEFORE "1. Pipeline".
#'
#' @param obj Seurat object (RNA assay, raw counts).
#' @param from_type "ensembl" or "entrez".
#' @param organism "human" or "mouse".
#' @param collapse_method "sum" (recommended) or "max_mean".
#' @param strip_version Strip Ensembl ".N" version suffix before mapping.
#' @return list(object, n_mapped, n_unmapped, n_collapsed)
remap_seurat_ids_to_symbol <- function(obj, from_type = "ensembl", organism = "human",
                                       collapse_method = "sum", strip_version = TRUE) {
  if (!from_type %in% c("ensembl", "entrez")) {
    stop("from_type doit etre 'ensembl' ou 'entrez' pour Single-Cell.")
  }
  orgdb <- if (organism == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop("Package 'org.Hs.eg.db' requis.")
    org.Hs.eg.db::org.Hs.eg.db
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop("Package 'org.Mm.eg.db' requis.")
    org.Mm.eg.db::org.Mm.eg.db
  }
  from_key <- switch(from_type, ensembl = "ENSEMBL", entrez = "ENTREZID")
  
  counts <- tryCatch(
    GetAssayData(obj, assay = "RNA", layer = "counts"),
    error = function(e) GetAssayData(obj, assay = "RNA", slot = "counts")
  )
  ids_clean <- rownames(counts)
  if (strip_version && from_type == "ensembl") ids_clean <- gsub("\\.[0-9]+$", "", ids_clean)
  
  expected_pattern <- switch(from_type, ensembl = "^ENS(MUS)?G[0-9]{6,}", entrez = "^[0-9]+$")
  sample_ids <- head(ids_clean[!is.na(ids_clean)], 200)
  pct_match  <- if (length(sample_ids) > 0) mean(grepl(expected_pattern, sample_ids)) else 0
  if (pct_match < 0.05) {
    stop(sprintf(
      "Vos identifiants ne ressemblent pas a des ID '%s' (%.0f%% correspondent). Exemple : '%s'.",
      from_type, pct_match * 100, if (length(sample_ids) > 0) sample_ids[1] else "?"
    ))
  }
  
  map_df <- tryCatch(
    AnnotationDbi::select(orgdb, keys = ids_clean, keytype = from_key, columns = "SYMBOL"),
    error = function(e) stop("Echec du mapping d'identifiants : ", conditionMessage(e))
  )
  map_df <- map_df[!is.na(map_df$SYMBOL), ]
  map_df <- map_df[!duplicated(map_df[[from_key]]), ]
  id_to_symbol <- setNames(map_df$SYMBOL, map_df[[from_key]])
  
  mapped_symbols <- id_to_symbol[ids_clean]
  keep       <- !is.na(mapped_symbols)
  n_mapped   <- sum(keep)
  n_unmapped <- sum(!keep)
  if (n_mapped == 0) stop("Aucun gene n'a pu etre converti en symbole. Verifiez organisme/type source.")
  
  mat  <- counts[keep, , drop = FALSE]
  syms <- unname(mapped_symbols[keep])
  n_before_collapse <- nrow(mat)
  uniq_syms <- unique(syms)
  
  if (collapse_method == "sum") {
    # Sparse-safe collapse: G is a (n_genes x n_unique_symbols) 0/1 indicator;
    # t(G) %*% mat sums duplicate-symbol rows without ever densifying `mat`.
    G <- Matrix::sparseMatrix(
      i = seq_along(syms), j = match(syms, uniq_syms), x = 1,
      dims = c(length(syms), length(uniq_syms))
    )
    new_mat <- Matrix::t(G) %*% mat
    rownames(new_mat) <- uniq_syms
  } else {
    mean_expr  <- Matrix::rowMeans(mat)
    ord        <- order(syms, -mean_expr)
    mat_ord    <- mat[ord, , drop = FALSE]
    syms_ord   <- syms[ord]
    keep_first <- !duplicated(syms_ord)
    new_mat    <- mat_ord[keep_first, , drop = FALSE]
    rownames(new_mat) <- syms_ord[keep_first]
  }
  
  meta_keep <- obj@meta.data[, intersect("orig.ident", colnames(obj@meta.data)), drop = FALSE]
  new_obj   <- CreateSeuratObject(counts = new_mat, meta.data = meta_keep, project = Project(obj))
  if ("orig.ident" %in% colnames(meta_keep)) new_obj$orig.ident <- meta_keep$orig.ident
  
  list(object = new_obj, n_mapped = n_mapped, n_unmapped = n_unmapped,
       n_collapsed = n_before_collapse - nrow(new_mat))
}
