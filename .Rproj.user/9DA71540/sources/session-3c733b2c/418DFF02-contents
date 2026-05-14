# global.R v2.2.txt
# Charge les librairies et définit les options globales

# --- 1. MEMOIRE & OPTIONS ---
options(future.globals.maxSize = 10000 * 1024^2)
options(shiny.maxRequestSize = 5000 * 1024^2)

# --- 2. PACKAGES ---
required_packages <- c(
  "shiny", "bslib", "Seurat", "ggplot2", "dplyr", "DT", "patchwork", "viridis",
  "plotly", "bsicons", "future", "shinyFiles", "SingleR", "celldex", 
  "SingleCellExperiment", "harmony", "destiny", "fs", "igraph", "Matrix", "reshape2","shinyjs"
) 

bioc_packages <- c("DESeq2", "edgeR", "ComplexHeatmap")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    warning(paste("Paquet manquant :", pkg))
  }
}

has_deseq2 <- requireNamespace("DESeq2", quietly = TRUE)
has_edger <- requireNamespace("edgeR", quietly = TRUE)

if (require("future", quietly = TRUE)) {
  plan(multisession, workers = max(1, parallel::detectCores() - 2))
}
# OPTIMISATION PARALLÉLISATION 
if(require("future", quietly = TRUE)) {
  plan("multisession", workers = max(1, parallel::detectCores() - 2))
  options(future.globals.maxSize = 10000 * 1024^2)  # 10GB
}


my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C3E50",
  secondary = "#18BC9C",
  "enable-gradients" = TRUE
)

clean_mem <- function() { gc() }

# --- 3. FONCTIONS UTILITAIRES SINGLE-CELL ---
load_single_cell_data <- function(path) {
  if (dir.exists(path)) {
    return(Seurat::Read10X(data.dir = path))
  } 
  
  ext <- tolower(tools::file_ext(path))
  switch(ext,
         "h5" = {
           if (requireNamespace("SeuratDisk", quietly = TRUE) && 
               SeuratDisk::ValidateFile(path, type = "h5Seurat")) {
             return(SeuratDisk::LoadH5Seurat(path))
           } else {
             return(Seurat::Read10X_h5(path))
           }
         },
         "h5ad" = {
           if (requireNamespace("SeuratDisk", quietly = TRUE)) {
             Convert(path, dest = "h5seurat", overwrite = TRUE)
             return(SeuratDisk::LoadH5Seurat(gsub(".h5ad", ".h5seurat", path)))
           } else {
             stop("Package 'SeuratDisk' requis pour .h5ad")
           }
         },
         "loom" = {
           if (requireNamespace("SeuratDisk", quietly = TRUE)) {
             return(SeuratDisk::Connect(path))
           } else {
             stop("Package 'SeuratDisk' requis pour .loom")
           }
         },
         "rds" = {
           return(readRDS(path))
         },
         stop("Format non supporté : ", ext))
}

prepare_seurat_object <- function(obj, project_name = "Project") {
  if (!inherits(obj, "Seurat")) {
    obj <- CreateSeuratObject(counts = obj, project = project_name)
  }
  if (grepl("^5", packageVersion("Seurat"))) {
    tryCatch({
      obj <- JoinLayers(obj)
    }, error = function(e) NULL) 
  }
  return(obj)
}

# --- 4. FONCTIONS UTILITAIRES BULK RNA ---
read_bulk_matrix <- function(file_path, has_header = TRUE, has_rownames = TRUE) {
  ext <- tolower(tools::file_ext(file_path))
  
  df <- switch(ext,
               "csv" = read.csv(file_path, header = has_header, 
                                row.names = if(has_rownames) 1 else NULL, 
                                check.names = FALSE, stringsAsFactors = FALSE),
               "tsv" = read.delim(file_path, header = has_header, 
                                  row.names = if(has_rownames) 1 else NULL, 
                                  sep = "\t", check.names = FALSE, stringsAsFactors = FALSE),
               "txt" = read.delim(file_path, header = has_header, 
                                  row.names = if(has_rownames) 1 else NULL, 
                                  check.names = FALSE, stringsAsFactors = FALSE),
               "xlsx" = {
                 if (requireNamespace("readxl", quietly = TRUE)) {
                   df_temp <- readxl::read_excel(file_path, col_names = has_header)
                   if (has_rownames) {
                     rownames(df_temp) <- df_temp[[1]]
                     df_temp <- df_temp[, -1, drop = FALSE]
                   }
                   as.data.frame(df_temp)
                 } else {
                   stop("Package 'readxl' nécessaire pour lire les fichiers .xlsx")
                 }
               },
               stop("Format de fichier non supporté : ", ext)
  )
  
  return(df)
}

prepare_bulk_object <- function(counts_matrix, metadata = NULL, project_name = "BulkRNA") {
  if (!all(sapply(counts_matrix, is.numeric))) {
    stop("La matrice de counts doit contenir uniquement des valeurs numériques.")
  }
  
  counts_mat <- as.matrix(counts_matrix)
  
  bulk_obj <- list(
    counts = counts_mat,
    metadata = metadata,
    project = project_name,
    type = "bulk"
  )
  
  if (is.null(metadata)) {
    bulk_obj$metadata <- data.frame(
      sample = colnames(counts_mat),
      row.names = colnames(counts_mat)
    )
  }
  
  return(bulk_obj)
}

# --- 5. FONCTIONS UTILITAIRES SPATIAL ---
load_spatial_visium <- function(visium_dir, sample_name = "Spatial_Sample", 
                                min_counts = 100, min_features = 200) {
  if (!dir.exists(file.path(visium_dir, "spatial"))) {
    stop("Dossier 'spatial' introuvable dans ", visium_dir)
  }
  
  spatial_obj <- tryCatch({
    Load10X_Spatial(
      data.dir = visium_dir,
      filename = "filtered_feature_bc_matrix.h5",
      assay = "Spatial",
      slice = sample_name,
      filter.matrix = TRUE
    )
  }, error = function(e) {
    if (dir.exists(file.path(visium_dir, "filtered_feature_bc_matrix"))) {
      Load10X_Spatial(
        data.dir = visium_dir,
        assay = "Spatial",
        slice = sample_name,
        filter.matrix = TRUE
      )
    } else {
      stop("Impossible de charger les données Visium : ", e$message)
    }
  })
  
  spatial_obj$orig.ident <- sample_name
  
  spatial_obj <- subset(spatial_obj, 
                        subset = nCount_Spatial >= min_counts & 
                          nFeature_Spatial >= min_features)
  
  return(spatial_obj)
}

prepare_spatial_object <- function(obj) {
  if (!inherits(obj, "Seurat")) {
    stop("L'objet doit être un objet Seurat.")
  }
  
  if (!"Spatial" %in% names(obj@assays) && length(obj@images) == 0) {
    warning("L'objet ne contient pas de données spatiales détectées.")
  }
  
  if (grepl("^5", packageVersion("Seurat"))) {
    tryCatch({
      obj <- JoinLayers(obj)
    }, error = function(e) NULL)
  }
  
  return(obj)
}

# --- 6. FONCTIONS AVANCÉES SINGLE-CELL VISUALIZATION (v2.1) ---

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

# --- 7. FONCTIONS AVANCÉES v2.2: PATHWAY & TRAJECTORY ---

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
plot_correlation_matrix <- function(seurat_obj, features, method = "pearson") {
  
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
      low = "#2166AC", mid = "white", high = "#B2182B",
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

#' Pathway Enrichment Analysis (Robust v3)
#' @param genes Vecteur de gènes (Symbols)
#' @param organism Organisme ("human", "mouse")
#' @param database Base de données ("GOBP", "KEGG", "Reactome")
#' @param pval_cutoff Seuil p-value
#' @return data.frame avec pathways enrichis
run_pathway_enrichment <- function(genes, organism = "human",
                                   database = "GOBP",
                                   pval_cutoff = 0.05) {
  
  # 1. Check Core Dependencies
  if (!requireNamespace("clusterProfiler", quietly = TRUE))
    stop("Package 'clusterProfiler' requis. Installez-le via BiocManager.")
  
  library(clusterProfiler)
  
  # 2. Prepare Organism Database & Convert Genes
  gene_entrez <- NULL
  
  if (organism == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))
      stop("Package 'org.Hs.eg.db' requis pour l'analyse humaine.")
    library(org.Hs.eg.db)
    orgdb <- org.Hs.eg.db
    
  } else if (organism == "mouse") {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))
      stop("Package 'org.Mm.eg.db' requis pour l'analyse souris.")
    library(org.Mm.eg.db)
    orgdb <- org.Mm.eg.db
    
  } else {
    stop("Organisme non supporté (choisir 'human' ou 'mouse')")
  }
  
  # Clean input genes: remove NAs, empty strings, trim whitespace
  genes_clean <- unique(trimws(genes[!is.na(genes) & nchar(trimws(genes)) > 0]))
  
  if (length(genes_clean) == 0) {
    stop("Aucun gène valide fourni après nettoyage.")
  }
  
  # Attempt conversion Symbol -> Entrez ID
  # Note: bitr can fail if keys are invalid. We wrap it in tryCatch.
  tryCatch({
    gene_entrez <- bitr(
      genes_clean, 
      fromType = "SYMBOL", 
      toType   = "ENTREZID", 
      OrgDb    = orgdb
    )
  }, error = function(e) {
    # If bitr fails completely, return empty
    warning(paste("Erreur lors de la conversion des gènes:", e$message))
    gene_entrez <<- data.frame(SYMBOL=character(0), ENTREZID=character(0))
  })
  
  # Check if any genes were successfully converted
  if (is.null(gene_entrez) || nrow(gene_entrez) == 0) {
    stop("Aucun gène n'a pu être converti en Entrez ID. Vérifiez que les noms de gènes sont des symboles officiels (ex: 'TP53', 'Actb') et correspondent à l'organisme sélectionné.")
  }
  
  ids <- gene_entrez$ENTREZID
  
  # 3. Run Enrichment based on Database
  enrich_result <- NULL
  
  if (database == "GOBP") {
    enrich_result <- enrichGO(
      gene          = ids,
      OrgDb         = orgdb,
      ont           = "BP",
      pAdjustMethod = "BH",
      pvalueCutoff  = pval_cutoff,
      qvalueCutoff  = 0.2,
      readable      = FALSE # Keep as Entrez for consistency, or TRUE for Symbols
    )
    
  } else if (database == "KEGG") {
    # KEGG requires organism code (hsa/mmu)
    org_code <- if (organism == "human") "hsa" else "mmu"
    
    # Check if KEGGREST is available for newer clusterProfiler versions
    if (!requireNamespace("KEGGREST", quietly = TRUE)) {
      warning("KEGGREST recommandé pour KEGG. Installation suggérée.")
    }
    
    enrich_result <- enrichKEGG(
      gene          = ids,
      organism      = org_code,
      pAdjustMethod = "BH",
      pvalueCutoff  = pval_cutoff
    )
    
  } else if (database == "Reactome") {
    if (!requireNamespace("ReactomePA", quietly = TRUE))
      stop("Package 'ReactomePA' requis pour l'analyse Reactome.")
    
    library(ReactomePA)
    
    # ReactomePA::enrichPathway expects organism name exactly as per its DB
    # Usually "human" or "mouse" works if the DB is loaded.
    enrich_result <- enrichPathway(
      gene          = ids,
      organism      = organism, 
      pAdjustMethod = "BH",
      pvalueCutoff  = pval_cutoff
    )
  } else {
    stop("Base de données non supportée (GOBP/KEGG/Reactome)")
  }
  
  # 4. Format Output
  if (is.null(enrich_result) || nrow(as.data.frame(enrich_result)) == 0) {
    return(data.frame(
      ID          = character(0),
      Description = character(0),
      p.adjust    = numeric(0),
      Count       = integer(0),
      GeneRatio   = character(0)
    ))
  }
  
  res_df <- as.data.frame(enrich_result)
  return(res_df)
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
  
  if (is.null(root_cells)) {
    centrality <- igraph::closeness(g)
    root_cell <- which.max(centrality)
  } else {
    root_cell <- root_cells[1]
  }
  
  pseudotime <- igraph::distances(g, v = root_cell, to = igraph::V(g))[1, ]
  pseudotime <- (pseudotime - min(pseudotime)) / (max(pseudotime) - min(pseudotime))
  seurat_obj$pseudotime <- pseudotime
  return(seurat_obj)
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