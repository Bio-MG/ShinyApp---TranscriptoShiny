# global.R v2.2.txt

# Charge les librairies et définit les options globales



# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)



# --- 2. PACKAGES ---

required_packages <- c(

  "shiny", "bslib", "Seurat", "ggplot2", "dplyr", "DT", "patchwork", "viridis",

  "plotly", "bsicons", "future", "shinyFiles", "SingleR", "celldex", 

  "SingleCellExperiment", "harmony", "destiny", "fs", "igraph", "Matrix", "reshape2",

  "shinyjs", "circlize", "rmarkdown", "zip"

) 



bioc_packages <- c("DESeq2", "edgeR", "limma", "ComplexHeatmap")



for (pkg in required_packages) {

  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {

    warning(paste("Paquet manquant :", pkg))

  }

}



has_deseq2 <- requireNamespace("DESeq2", quietly = TRUE)

has_edger <- requireNamespace("edgeR", quietly = TRUE)

has_limma <- requireNamespace("limma", quietly = TRUE)



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



#' Gene Set Enrichment Analysis (GSEA, pre-ranked) — complements run_pathway_enrichment()

#'

#' Unlike ORA (run_pathway_enrichment), GSEA does not require an arbitrary

#' significance threshold: it ranks ALL tested genes by a continuous score

#' (here, signed -log10(p) * sign(log2FC), the standard ranking metric) and

#' tests for enrichment along that full ranking. More statistically robust

#' when few genes pass a hard significance cutoff.

#'

#' @param de_results data.frame with at least columns: gene, log2FoldChange, pvalue.

#' @param organism "human" or "mouse".

#' @param database "GOBP", "KEGG", or "Reactome".

#' @param pval_cutoff p-value cutoff passed to the GSEA call.

#' @return data.frame: ID, Description, setSize, enrichmentScore, NES, pvalue, p.adjust, ...

run_gsea_enrichment <- function(de_results, organism = "human",

                                 database = "GOBP", pval_cutoff = 0.05) {



  if (!requireNamespace("clusterProfiler", quietly = TRUE))

    stop("Package 'clusterProfiler' requis. Installez-le via BiocManager.")

  library(clusterProfiler)



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



  required_cols <- c("gene", "log2FoldChange", "pvalue")

  missing_cols <- setdiff(required_cols, colnames(de_results))

  if (length(missing_cols) > 0) {

    stop("Colonnes manquantes dans les résultats DE pour GSEA : ", paste(missing_cols, collapse = ", "))

  }



  de_clean <- de_results[!is.na(de_results$pvalue) & !is.na(de_results$log2FoldChange), ]

  de_clean <- de_clean[!is.na(de_clean$gene) & nchar(trimws(de_clean$gene)) > 0, ]

  if (nrow(de_clean) < 10) {

    stop("Trop peu de gènes valides (", nrow(de_clean), ") après nettoyage pour GSEA.")

  }



  gene_entrez <- tryCatch({

    AnnotationDbi::select(orgdb, keys = unique(de_clean$gene), keytype = "SYMBOL", columns = "ENTREZID")

  }, error = function(e) {

    stop("Erreur lors de la conversion des gènes pour GSEA : ", conditionMessage(e))

  })

  gene_entrez <- gene_entrez[!is.na(gene_entrez$ENTREZID), ]

  gene_entrez <- gene_entrez[!duplicated(gene_entrez$SYMBOL), ]



  de_merged <- merge(de_clean, gene_entrez, by.x = "gene", by.y = "SYMBOL")

  if (nrow(de_merged) < 10) {

    stop("Aucun gène n'a pu être converti en Entrez ID pour GSEA. Vérifiez l'organisme sélectionné.")

  }



  # Standard pre-ranking metric: signed -log10(p), tie-broken by averaging

  # duplicated Entrez IDs (can happen when multiple symbols map to one gene).

  de_merged$rank_metric <- -log10(pmax(de_merged$pvalue, 1e-300)) * sign(de_merged$log2FoldChange)

  de_merged <- stats::aggregate(rank_metric ~ ENTREZID, data = de_merged, FUN = mean)



  ranked <- sort(setNames(de_merged$rank_metric, de_merged$ENTREZID), decreasing = TRUE)



  gsea_result <- if (database == "GOBP") {

    clusterProfiler::gseGO(geneList = ranked, OrgDb = orgdb, ont = "BP",

                           pvalueCutoff = pval_cutoff, pAdjustMethod = "BH", verbose = FALSE)

  } else if (database == "KEGG") {

    org_code <- if (organism == "human") "hsa" else "mmu"

    clusterProfiler::gseKEGG(geneList = ranked, organism = org_code,

                             pvalueCutoff = pval_cutoff, pAdjustMethod = "BH", verbose = FALSE)

  } else if (database == "Reactome") {

    if (!requireNamespace("ReactomePA", quietly = TRUE))

      stop("Package 'ReactomePA' requis pour l'analyse Reactome.")

    library(ReactomePA)

    ReactomePA::gsePathway(geneList = ranked, organism = organism,

                          pvalueCutoff = pval_cutoff, pAdjustMethod = "BH", verbose = FALSE)

  } else {

    stop("Base de données non supportée pour GSEA (GOBP/KEGG/Reactome)")

  }



  res_df <- as.data.frame(gsea_result)

  if (nrow(res_df) == 0) {

    return(data.frame(ID = character(0), Description = character(0), setSize = integer(0),

                      enrichmentScore = numeric(0), NES = numeric(0),

                      pvalue = numeric(0), p.adjust = numeric(0)))

  }

  # Normalize column naming to stay compatible with build_pathway_dt() /

  # plot_pathway_barplot() / plot_pathway_dotplot(), which expect "Count"

  # and "GeneRatio" — GSEA uses "setSize" instead, so we alias it.

  res_df$Count     <- res_df$setSize

  res_df$GeneRatio <- paste0(res_df$setSize, "/", length(ranked))



  # Attach the raw gseaResult S4 object as an attribute (additive — does not

  # change the data.frame contract for existing callers). enrichplot::gseaplot2()

  # needs this raw object (with @geneList, @result, etc.) to draw the running

  # enrichment-score curve; consumers that only need the table can ignore it.

  attr(res_df, "gsea_obj") <- gsea_result

  res_df

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



# --- 8. BULK RNA-SEQ — MOTEUR D'ANALYSE DIFFERENTIELLE ---



#' Filter low-count genes from a bulk counts matrix

#'

#' @param counts_matrix Numeric matrix, genes in rows, samples in columns.

#' @param min_count Minimum total count across all samples to keep a gene.

#' @param min_samples Minimum number of samples reaching `min_count_per_sample`.

#' @param min_count_per_sample Per-sample count threshold used with `min_samples`.

#' @return Filtered numeric matrix (same column order, fewer rows).

filter_bulk_counts <- function(counts_matrix, min_count = 10,

                                min_samples = 1, min_count_per_sample = 1) {

  keep_total <- rowSums(counts_matrix, na.rm = TRUE) >= min_count

  keep_n     <- rowSums(counts_matrix >= min_count_per_sample, na.rm = TRUE) >= min_samples

  out <- counts_matrix[keep_total & keep_n, , drop = FALSE]

  if (nrow(out) == 0) stop("Aucun gène ne passe le filtre — seuils trop stricts.")

  out

}



#' Build a DESeqDataSet and (optionally) fit DESeq()

#'

#' @param counts_matrix Integer-like counts matrix (genes x samples).

#' @param metadata Sample metadata; rownames must match colnames(counts_matrix).

#' @param design_formula Character, e.g. "~ condition" or "~ batch + condition".

#' @param run_deseq Logical — if FALSE, returns the unfit DDS (used for blind VST).

#' @return A DESeqDataSet (fit or unfit depending on `run_deseq`).

build_dds <- function(counts_matrix, metadata, design_formula = "~ condition",

                       run_deseq = TRUE) {

  if (!requireNamespace("DESeq2", quietly = TRUE)) {

    stop("Le package 'DESeq2' est requis pour cette analyse.")

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  if (length(common) < 2) {

    stop("Moins de 2 échantillons communs entre la matrice de counts et les métadonnées.")

  }

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  # Convert character design columns to factor explicitly, BEFORE

  # DESeqDataSetFromMatrix — silences the "converting to factors" warning

  # and gives us control over level ordering (first level = reference).

  design_terms <- all.vars(stats::as.formula(design_formula))

  for (term in design_terms) {

    if (term %in% colnames(metadata) && is.character(metadata[[term]])) {

      metadata[[term]] <- factor(metadata[[term]])

    }

  }



  if (any(counts_matrix < 0, na.rm = TRUE)) {

    stop("La matrice de counts contient des valeurs négatives — DESeq2 requiert des counts bruts.")

  }

  if (any(counts_matrix != round(counts_matrix), na.rm = TRUE)) {

    warning("Valeurs non-entières détectées — arrondi automatique (DESeq2 requiert des counts entiers).")

    counts_matrix <- round(counts_matrix)

  }



  dds <- DESeq2::DESeqDataSetFromMatrix(

    countData = counts_matrix,

    colData   = metadata,

    design    = stats::as.formula(design_formula)

  )

  if (run_deseq) dds <- DESeq2::DESeq(dds, quiet = TRUE)

  dds

}



#' Detect if a covariate is fully confounded with the main condition

#'

#' DESeq2's design matrix loses full rank when a covariate level maps to

#' exactly one condition level (and vice-versa) — this produces a cryptic

#' "model matrix is not full rank" error deep inside DESeq(). This check

#' runs BEFORE fitting, so the UI can show a clear, actionable message

#' instead of a low-level matrix algebra error.

#'

#' @param metadata Sample metadata data.frame.

#' @param condition_col Character, name of the main condition column.

#' @param covariate_col Character, name of the covariate column to check.

#' @return Logical — TRUE if covariate_col is fully confounded with condition_col.

check_design_confounding <- function(metadata, condition_col, covariate_col) {

  if (!condition_col %in% colnames(metadata) || !covariate_col %in% colnames(metadata)) {

    return(FALSE)

  }

  tbl <- table(

    factor(metadata[[condition_col]]),

    factor(metadata[[covariate_col]])

  )

  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(FALSE)



  # Confounded when every covariate level is observed in only one condition

  # level (cross-tab has exactly one non-zero cell per covariate column).

  nonzero_per_covariate_level <- colSums(tbl > 0)

  all(nonzero_per_covariate_level == 1)

}



#' Validate a full design (condition + optional covariates) before fitting

#'

#' @param metadata Sample metadata data.frame.

#' @param condition_col Character, main condition column.

#' @param covariates Character vector of additional covariate columns (may be empty).

#' @return Character vector of human-readable problem descriptions (empty if none).

validate_bulk_design <- function(metadata, condition_col, covariates = character(0)) {

  problems <- character(0)



  if (condition_col %in% colnames(metadata)) {

    n_na <- sum(is.na(metadata[[condition_col]]))

    if (n_na > 0) {

      problems <- c(problems, sprintf(

        "%d échantillon(s) ont une valeur manquante (NA) dans '%s'.", n_na, condition_col

      ))

    }

    grp_counts <- table(metadata[[condition_col]])

    if (any(grp_counts < 2)) {

      problems <- c(problems, sprintf(

        "Groupe(s) avec un seul réplicat dans '%s' : %s — résultats statistiquement peu fiables.",

        condition_col, paste(names(grp_counts)[grp_counts < 2], collapse = ", ")

      ))

    }

  }



  for (cov in covariates) {

    if (check_design_confounding(metadata, condition_col, cov)) {

      problems <- c(problems, sprintf(

        "La covariable '%s' est entièrement confondue avec '%s' — son effet ne peut pas être estimé séparément (design non plein rang). Retirez-la ou vérifiez votre plan d'expérience.",

        cov, condition_col

      ))

    }

  }



  problems

}



#' Extract a (shrunk) contrast result table from a fitted DESeqDataSet

#'

#' @param dds Fitted DESeqDataSet (post-DESeq()).

#' @param condition_col Column name used for the contrast.

#' @param group_target Numerator level ("treatment").

#' @param group_ref Denominator level ("reference").

#' @param shrink Apply apeglm/normal LFC shrinkage when possible.

#' @param alpha Significance threshold passed to results().

#' @return data.frame: gene, baseMean, log2FoldChange, pvalue, padj, ...

extract_deseq2_contrast <- function(dds, condition_col, group_target, group_ref,

                                     shrink = TRUE, alpha = 0.05) {

  contrast_vec <- c(condition_col, group_target, group_ref)

  res <- DESeq2::results(dds, contrast = contrast_vec, alpha = alpha)



  if (shrink) {

    coef_name    <- paste0(condition_col, "_", group_target, "_vs_", group_ref)

    avail_coefs  <- DESeq2::resultsNames(dds)

    res <- tryCatch({

      if (coef_name %in% avail_coefs) {

        DESeq2::lfcShrink(dds, coef = coef_name, res = res, type = "apeglm", quiet = TRUE)

      } else {

        DESeq2::lfcShrink(dds, contrast = contrast_vec, res = res, type = "normal", quiet = TRUE)

      }

    }, error = function(e) res)  # fallback silencieux sur le résultat non-shrunk

  }



  out <- as.data.frame(res)

  out$gene <- rownames(out)

  out <- out[order(out$padj), ]

  rownames(out) <- NULL

  out

}



#' edgeR fallback differential expression (2-group comparison)

run_edger_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref) {

  if (!requireNamespace("edgeR", quietly = TRUE)) stop("Package 'edgeR' requis.")

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  grp  <- factor(metadata[[condition_col]], levels = c(group_ref, group_target))

  keep <- !is.na(grp)

  if (sum(keep) < 4) stop("Trop peu d'échantillons valides pour edgeR (minimum 4 recommandé).")



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]), group = grp[keep])

  y      <- edgeR::calcNormFactors(y)

  design <- stats::model.matrix(~grp[keep])

  y      <- edgeR::estimateDisp(y, design)

  fit    <- edgeR::glmQLFit(y, design)

  qlf    <- edgeR::glmQLFTest(fit, coef = 2)



  res <- edgeR::topTags(qlf, n = Inf)$table

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]  <- "log2FoldChange"

  colnames(res)[colnames(res) == "PValue"] <- "pvalue"

  colnames(res)[colnames(res) == "FDR"]    <- "padj"

  res[order(res$padj), ]

}



#' limma-voom fallback differential expression (2-group comparison)

run_limma_voom_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref) {

  missing_pkgs <- c(

    if (!requireNamespace("limma", quietly = TRUE)) "limma",

    if (!requireNamespace("edgeR", quietly = TRUE)) "edgeR"

  )

  if (length(missing_pkgs) > 0) {

    stop("Package(s) manquant(s) pour limma-voom : ", paste(missing_pkgs, collapse = ", "),

         ". Vérifiez .libPaths() — installé mais peut-être dans une autre librairie R.")

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  grp  <- factor(metadata[[condition_col]], levels = c(group_ref, group_target))

  keep <- !is.na(grp)

  if (sum(keep) < 4) stop("Trop peu d'échantillons valides pour limma-voom (minimum 4 recommandé).")



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]))

  y      <- edgeR::calcNormFactors(y)

  design <- stats::model.matrix(~grp[keep])

  v      <- limma::voom(y, design)

  fit    <- limma::eBayes(limma::lmFit(v, design))



  res <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "P")

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]     <- "log2FoldChange"

  colnames(res)[colnames(res) == "P.Value"]   <- "pvalue"

  colnames(res)[colnames(res) == "adj.P.Val"] <- "padj"

  res[order(res$padj), ]

}



#' Dispatch DE engine call

run_bulk_de_dispatch <- function(engine, counts_matrix, metadata, condition_col,

                                  group_target, group_ref, dds = NULL, shrink = TRUE) {

  switch(engine,

    deseq2 = {

      if (is.null(dds)) stop("DESeqDataSet manquant pour le moteur DESeq2.")

      extract_deseq2_contrast(dds, condition_col, group_target, group_ref, shrink = shrink)

    },

    edger = run_edger_de(counts_matrix, metadata, condition_col, group_target, group_ref),

    limma = run_limma_voom_de(counts_matrix, metadata, condition_col, group_target, group_ref),

    stop("Moteur DE non supporté : ", engine)

  )

}



#' Normalize DE result column names + fill baseMean if missing (edgeR/limma)

.normalize_de_cols <- function(df, counts_for_basemean = NULL) {

  if (is.null(df) || nrow(df) == 0) return(df)

  if (!"gene" %in% colnames(df))           df$gene <- rownames(df)

  if (!"log2FoldChange" %in% colnames(df)) df$log2FoldChange <- NA_real_

  if (!"pvalue" %in% colnames(df))         df$pvalue <- NA_real_

  if (!"padj" %in% colnames(df))           df$padj   <- NA_real_

  if (!"baseMean" %in% colnames(df)) {

    df$baseMean <- if (!is.null(counts_for_basemean)) {

      rowMeans(counts_for_basemean[df$gene, , drop = FALSE], na.rm = TRUE)

    } else NA_real_

  }

  df

}



#' Variance-stabilizing matrix for PCA/heatmap (falls back to log2 for small n)

get_vst_matrix <- function(dds) {

  # Guard 1: too few samples for blind dispersion estimation.

  if (ncol(dds) < 4) {

    return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

  }

  # Guard 2: vst() requires >= nsub genes (default 1000) for the

  # subsampling-based dispersion fit. Small gene panels (e.g. targeted

  # capture panels, custom panels < 1000 genes) must use the direct

  # variance-stabilizing transformation instead (slower, but correct

  # regardless of gene count — no subsampling involved).

  nsub <- min(1000, nrow(dds))

  if (nrow(dds) < 1000) {

    vsd <- tryCatch(

      DESeq2::varianceStabilizingTransformation(dds, blind = TRUE),

      error = function(e) {

        warning("VST direct a échoué (", conditionMessage(e),

                "), repli sur log2(counts normalisés + 1).")

        NULL

      }

    )

    if (is.null(vsd)) return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

    return(SummarizedExperiment::assay(vsd))

  }

  vsd <- DESeq2::vst(dds, blind = TRUE, nsub = nsub)

  SummarizedExperiment::assay(vsd)

}



#' Bulk PCA plot colored/shaped by metadata

plot_bulk_pca <- function(vst_matrix, metadata, color_by = NULL, shape_by = NULL, ntop = 500) {

  rv   <- apply(vst_matrix, 1, var)

  ntop <- min(ntop, nrow(vst_matrix))

  sel  <- order(rv, decreasing = TRUE)[seq_len(ntop)]

  pca  <- prcomp(t(vst_matrix[sel, , drop = FALSE]), scale. = FALSE)

  pct  <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)



  df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], sample = colnames(vst_matrix))



  # Defensive normalisation: treat NULL/NA/""/length!=1 all the same way, and

  # never let an ambiguous condition reach an `if`/`&&` — R >= 4.3 errors hard

  # ("missing value where TRUE/FALSE needed" or "length(0) in coercion to

  # 'logical(1)'") on exactly this class of bug, e.g. `shape_by != color_by`

  # when one side is NULL, or a stray length-0/NA value slips through from

  # an upstream selectInput.

  .clean_scalar <- function(x) {

    if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) NULL else x

  }

  color_by <- .clean_scalar(color_by)

  shape_by <- .clean_scalar(shape_by)



  has_color <- !is.null(color_by) && isTRUE(color_by %in% colnames(metadata))

  has_shape <- !is.null(shape_by) && isTRUE(shape_by %in% colnames(metadata)) &&

               isTRUE(is.null(color_by) || shape_by != color_by)



  if (has_color) df$color <- as.character(metadata[df$sample, color_by])

  if (has_shape) df$shape <- as.character(metadata[df$sample, shape_by])



  p <- ggplot(df, aes(x = PC1, y = PC2, label = sample))

  if (has_color && has_shape) {

    p <- p + geom_point(aes(color = color, shape = shape), size = 3.5, alpha = 0.85)

  } else if (has_color) {

    p <- p + geom_point(aes(color = color), size = 3.5, alpha = 0.85)

  } else {

    p <- p + geom_point(size = 3.5, alpha = 0.85, color = "#2C3E50")

  }

  p + geom_text(size = 3, vjust = -1, check_overlap = TRUE) +

    labs(title = "PCA — Échantillons Bulk RNA-seq",

         x = paste0("PC1 (", pct[1], "%)"), y = paste0("PC2 (", pct[2], "%)"),

         color = if (has_color) color_by else NULL,

         shape = if (has_shape) shape_by else NULL) +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' Volcano plot for bulk DE results

plot_volcano_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05, top_label = 10) {

  res_df <- res_df[!is.na(res_df$padj), ]

  res_df$status <- dplyr::case_when(

    res_df$padj < padj_thresh & res_df$log2FoldChange >  lfc_thresh ~ "Up",

    res_df$padj < padj_thresh & res_df$log2FoldChange < -lfc_thresh ~ "Down",

    TRUE ~ "NS"

  )

  ord      <- order(res_df$padj)

  up_lbl   <- head(res_df$gene[ord][res_df$status[ord] == "Up"],   top_label)

  down_lbl <- head(res_df$gene[ord][res_df$status[ord] == "Down"], top_label)

  res_df$label <- ifelse(res_df$gene %in% c(up_lbl, down_lbl), res_df$gene, NA)



  ggplot(res_df, aes(x = log2FoldChange, y = -log10(padj), color = status)) +

    geom_point(alpha = 0.7, size = 1.6) +

    scale_color_manual(values = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")) +

    geom_vline(xintercept = c(-lfc_thresh, lfc_thresh), linetype = "dashed", color = "grey40") +

    geom_hline(yintercept = -log10(padj_thresh), linetype = "dashed", color = "grey40") +

    geom_text(aes(label = label), size = 2.8, na.rm = TRUE, vjust = -0.6, show.legend = FALSE) +

    labs(title = "Volcano Plot — Analyse Différentielle",

         x = "Log2 Fold Change", y = "-Log10(P-adj)", color = "Statut") +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' MA-plot for bulk DE results

plot_ma_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05) {

  res_df <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]

  res_df$sig <- res_df$padj < padj_thresh & abs(res_df$log2FoldChange) > lfc_thresh

  ggplot(res_df, aes(x = log10(baseMean + 1), y = log2FoldChange, color = sig)) +

    geom_point(alpha = 0.6, size = 1.4) +

    scale_color_manual(values = c(`TRUE` = "#E74C3C", `FALSE` = "#BDC3C7"), guide = "none") +

    geom_hline(yintercept = 0, color = "grey30") +

    labs(title = "MA-Plot", x = "Log10(Expression Moyenne + 1)", y = "Log2 Fold Change") +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 14))

}



#' Heatmap of selected genes (ComplexHeatmap if available, ggplot fallback otherwise)

plot_heatmap_bulk <- function(vst_matrix, genes, metadata, annotation_col = NULL, scale_rows = TRUE) {

  genes <- intersect(genes, rownames(vst_matrix))

  if (length(genes) < 2) stop("Au moins 2 gènes requis pour la heatmap.")

  mat <- vst_matrix[genes, , drop = FALSE]

  if (scale_rows) mat <- t(scale(t(mat)))



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    ann <- NULL

    if (!is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      ann <- ComplexHeatmap::HeatmapAnnotation(group = metadata[colnames(mat), annotation_col])

    }

    return(ComplexHeatmap::Heatmap(mat, name = "Z-score", top_annotation = ann,

                                   show_row_names = nrow(mat) <= 60,

                                   column_title = "Heatmap — Gènes Différentiels"))

  }

  melted <- reshape2::melt(mat, varnames = c("gene", "sample"))

  ggplot(melted, aes(x = sample, y = gene, fill = value)) +

    geom_tile() +

    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, name = "Z-score") +

    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

}



#' Sample-to-sample correlation heatmap (QC diagnostic, computed on VST matrix)

#'

#' High diagnostic value, near-zero extra cost: detects mislabeled samples,

#' batch outliers, or unexpected duplicates BEFORE the user spends time

#' interpreting a differential expression result built on bad input.

#' Reuses the already-computed VST matrix — no extra heavy computation.

#'

#' @param vst_matrix Matrix, VST-transformed expression (genes x samples).

#' @param metadata Sample metadata, for optional column annotation.

#' @param annotation_col Character, metadata column used to annotate samples (optional).

#' @param method Correlation method, "pearson" (default, standard for VST data) or "spearman".

#' @return A ComplexHeatmap object if available, otherwise a ggplot fallback.

plot_sample_correlation_heatmap <- function(vst_matrix, metadata = NULL,

                                             annotation_col = NULL, method = "pearson") {

  if (ncol(vst_matrix) < 2) stop("Au moins 2 échantillons requis pour la corrélation.")

  cor_mat <- cor(vst_matrix, method = method, use = "pairwise.complete.obs")



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    ann <- NULL

    if (!is.null(metadata) && !is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      ann <- ComplexHeatmap::HeatmapAnnotation(

        group = metadata[colnames(cor_mat), annotation_col]

      )

    }

    return(ComplexHeatmap::Heatmap(

      cor_mat, name = paste0(toupper(substring(method, 1, 1)), substring(method, 2)),

      top_annotation     = ann,

      col                = circlize::colorRamp2(c(min(cor_mat), 1), c("white", "#2C3E50")),

      show_row_names     = TRUE,

      show_column_names  = TRUE,

      column_title       = "Corrélation Inter-Échantillons (QC)",

      cell_fun = function(j, i, x, y, width, height, fill) {

        grid::grid.text(sprintf("%.2f", cor_mat[i, j]), x, y, gp = grid::gpar(fontsize = 8))

      }

    ))

  }



  melted <- reshape2::melt(cor_mat, varnames = c("sample1", "sample2"))

  ggplot(melted, aes(x = sample1, y = sample2, fill = value)) +

    geom_tile() +

    geom_text(aes(label = sprintf("%.2f", value)), size = 2.8, color = "black") +

    scale_fill_gradient(low = "white", high = "#2C3E50", name = toupper(method)) +

    labs(title = "Corrélation Inter-Échantillons (QC)", x = NULL, y = NULL) +

    theme_minimal() +

    theme(axis.text.x = element_text(angle = 45, hjust = 1),

          plot.title  = element_text(face = "bold", size = 14))

}





# --- 9. PATHWAY VISUALISATION HELPERS (PARTAGÉS sc + bulk) ---



#' Top-N pathway barplot (shared by mod_sc_pathways.R and mod_bulk.R)

plot_pathway_barplot <- function(df, db_label = "", top_n = 15) {

  df_top <- head(df, top_n)

  df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))

  ggplot(df_top, aes(x = Count, y = Description, fill = -log10(p.adjust))) +

    geom_bar(stat = "identity", width = 0.7) +

    scale_fill_viridis_c(option = "plasma", direction = -1) +

    labs(title = paste("Top", top_n, "Pathways -", db_label),

         x = "Nombre de gènes", y = NULL, fill = "-log10(P-adj)") +

    theme_minimal(base_size = 12) +

    theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),

          legend.position = "right", panel.grid.major.y = element_blank())

}



#' Pathway dotplot (shared)

plot_pathway_dotplot <- function(df, db_label = "", top_n = 20) {

  df_top <- head(df, top_n)

  df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))

  if ("GeneRatio" %in% colnames(df_top)) {

    df_top$GeneRatioNum <- sapply(df_top$GeneRatio, function(r) {

      parts <- strsplit(as.character(r), "/")[[1]]

      if (length(parts) == 2) as.numeric(parts[1]) / as.numeric(parts[2]) else NA

    })

  } else {

    df_top$GeneRatioNum <- df_top$Count / max(df_top$Count, na.rm = TRUE)

  }

  ggplot(df_top, aes(x = GeneRatioNum, y = Description, color = -log10(p.adjust), size = Count)) +

    geom_point(alpha = 0.85) +

    scale_color_viridis_c(option = "magma", direction = -1) +

    labs(title = paste("Dotplot Pathways -", db_label),

         x = "Ratio de gènes", y = NULL, color = "-log10(P-adj)", size = "Nb gènes") +

    theme_minimal(base_size = 12) +

    theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),

          legend.position = "right")

}



#' Pathway results DT table (shared)

build_pathway_dt <- function(df) {

  cols_available <- intersect(c("ID", "Description", "p.adjust", "Count", "GeneRatio"), colnames(df))

  df_display <- df[, cols_available, drop = FALSE]

  colnames(df_display) <- c("ID", "Description", "P-adj", "Nb Gènes", "Ratio")[seq_along(cols_available)]

  DT::datatable(df_display, filter = "top", rownames = FALSE,

                options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip"),

                extensions = "Buttons") %>%

    DT::formatStyle("P-adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

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



#' Standardized DE results DT table (shared by mod_bulk.R Shiny render AND the

#' bulk HTML/PDF report). Centralizing this fixes a real bug: the report's

#' previous inline call to DT::datatable() omitted rownames = FALSE, so the

#' first column displayed auto-numbered row indices (1..n) instead of gene

#' symbols (which live in the "gene" column, since extract_deseq2_contrast()

#' resets rownames to NULL). One function, used everywhere, prevents this

#' divergence from reappearing.

#'

#' @param df DE results data.frame (must have gene, baseMean, log2FoldChange, pvalue, padj).

#' @return A DT::datatable object.

build_de_results_dt <- function(df) {

  df_display <- data.frame(

    Gene     = df$gene,

    BaseMean = round(df$baseMean, 1),

    Log2FC   = round(df$log2FoldChange, 3),

    PValue   = format(df$pvalue, scientific = TRUE, digits = 3),

    Padj     = format(df$padj,   scientific = TRUE, digits = 3),

    stringsAsFactors = FALSE

  )

  DT::datatable(df_display, filter = "top", rownames = FALSE,

                options = list(pageLength = 15, scrollX = TRUE)) %>%

    DT::formatStyle("Padj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                       c("darkgreen", "green", "orange", "red")))

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





# --- 10. BULK RNA-SEQ — INFÉRENCE MÉTADONNÉES & MAPPING D'IDENTIFIANTS (1.2) ---



#' Infer sample-level metadata by splitting sample names on a delimiter

#'

#' GEO almost never ships clean tabular metadata — but sample names usually

#' encode the experimental design (e.g. "MW1_cornea_mock_1"). This replaces

#' manual/LLM-assisted metadata reconstruction with a deterministic,

#' inspectable split.

#'

#' @param sample_names Character vector of sample/column names from the counts matrix.

#' @param delimiter Regex delimiter to split on (default underscore or hyphen).

#' @param col_names Character vector naming each resulting segment. Length must match

#'   the number of segments produced by the split (after trimming to the minimum

#'   segment count across all samples, to handle slightly irregular naming).

#' @return data.frame, rownames = sample_names, one column per segment.

infer_metadata_from_names <- function(sample_names, delimiter = "[_-]", col_names = NULL) {

  segments <- strsplit(sample_names, delimiter)

  n_seg <- sapply(segments, length)



  if (length(unique(n_seg)) > 1) {

    min_n <- min(n_seg)

    warning(sprintf(

      "Nombre de segments inégal entre échantillons (min=%d, max=%d). Troncature à %d segments — vérifiez le résultat.",

      min_n, max(n_seg), min_n

    ))

    segments <- lapply(segments, function(s) head(s, min_n))

    n_seg <- min_n

  } else {

    n_seg <- n_seg[1]

  }



  mat <- do.call(rbind, segments)

  mat <- as.data.frame(mat, stringsAsFactors = FALSE)



  if (is.null(col_names)) {

    col_names <- paste0("segment_", seq_len(n_seg))

  } else if (length(col_names) != n_seg) {

    stop(sprintf("col_names doit avoir %d éléments (segments détectés), reçu %d.", n_seg, length(col_names)))

  }

  colnames(mat) <- col_names

  rownames(mat) <- sample_names



  # Auto-detect purely numeric segments (e.g. replicate number) and convert

  for (cn in colnames(mat)) {

    if (all(grepl("^[0-9]+$", mat[[cn]]))) mat[[cn]] <- as.integer(mat[[cn]])

  }

  mat

}



#' Preview a metadata-from-names split without committing — feeds the live UI preview

#'

#' @param sample_names Character vector of sample names.

#' @param delimiter Regex delimiter.

#' @param n_preview Number of samples to preview.

#' @return List: segments (list of character vectors), n_seg_consistent (logical), n_seg (integer vector).

preview_metadata_split <- function(sample_names, delimiter = "[_-]", n_preview = 5) {

  segments <- strsplit(head(sample_names, n_preview), delimiter)

  n_seg    <- sapply(segments, length)

  list(segments = segments, n_seg_consistent = length(unique(n_seg)) == 1, n_seg = n_seg)

}



#' Detect the likely gene ID type from a vector of row identifiers

#'

#' @param gene_ids Character vector (rownames of the counts matrix).

#' @return One of "symbol", "ensembl", "entrez", "affy_probe", "unknown".

detect_gene_id_type <- function(gene_ids) {

  sample_ids <- head(gene_ids[!is.na(gene_ids)], 50)

  if (length(sample_ids) == 0) return("unknown")



  pct_match <- function(pattern) mean(grepl(pattern, sample_ids))



  if (pct_match("^ENSG[0-9]{11}") > 0.7 || pct_match("^ENSMUSG[0-9]{11}") > 0.7) return("ensembl")

  if (pct_match("^[0-9]+$") > 0.7) return("entrez")

  if (pct_match("^[0-9]+(_[a-z]_)?_at$") > 0.5) return("affy_probe")

  if (pct_match("^[A-Za-z0-9.-]+$") > 0.7 && pct_match("^[0-9]+$") < 0.3) return("symbol")

  "unknown"

}



#' Convert counts matrix row identifiers to gene symbols

#'

#' @param counts_matrix Matrix, genes in rows (any supported ID type).

#' @param from_type One of "ensembl", "entrez", "affy_probe" (output of detect_gene_id_type()).

#' @param organism "human" or "mouse".

#' @param collapse_method How to merge counts when multiple original IDs map to the

#'   same symbol: "sum" (recommended for counts) or "max_mean" (keep the ID with the

#'   highest mean expression, discard the rest — useful for probe-level redundancy).

#' @return List: matrix (remapped, deduplicated), n_mapped, n_unmapped, n_collapsed.

remap_gene_ids_to_symbol <- function(counts_matrix, from_type, organism = "human",

                                      collapse_method = "sum") {

  if (!from_type %in% c("ensembl", "entrez", "affy_probe")) {

    stop("from_type doit être 'ensembl', 'entrez' ou 'affy_probe'.")

  }



  orgdb <- if (organism == "human") {

    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop("Package 'org.Hs.eg.db' requis.")

    org.Hs.eg.db::org.Hs.eg.db

  } else {

    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop("Package 'org.Mm.eg.db' requis.")

    org.Mm.eg.db::org.Mm.eg.db

  }



  from_key <- switch(from_type,

    ensembl    = "ENSEMBL",

    entrez     = "ENTREZID",

    affy_probe = if (organism == "human") "PROBEID" else stop("Mapping de probes Affymetrix non supporté pour la souris dans ce module — fournissez un fichier d'annotation de plateforme dédié.")

  )



  ids_clean <- rownames(counts_matrix)



  # ── Pre-flight sanity check ────────────────────────────────────────────────

  # AnnotationDbi::select() throws a low-level, untranslated error ("None of

  # the keys entered are valid keys for 'ENSEMBL'") when NONE of the supplied

  # identifiers look anything like the chosen from_type — typically because

  # the data isn't gene-level at all (e.g. "eye_count"-style custom row IDs),

  # or the wrong from_type was left selected. Catch this BEFORE the call with

  # an actionable French message instead of the raw Bioconductor text.

  expected_pattern <- switch(from_type,

    ensembl    = "^ENS(MUS)?G[0-9]{6,}",

    entrez     = "^[0-9]+$",

    affy_probe = "^[0-9]+(_[a-z]_)?_at$"

  )

  sample_ids <- head(ids_clean[!is.na(ids_clean)], 200)

  pct_match  <- if (length(sample_ids) > 0) mean(grepl(expected_pattern, sample_ids)) else 0

  if (pct_match < 0.05) {

    stop(sprintf(

      paste0(

        "Vos identifiants ne ressemblent pas à des ID '%s' (seulement %.0f%% correspondent au format attendu, ex: %s). ",

        "Causes probables : (1) vos données n'utilisent pas d'identifiants de gènes standards — dans ce cas, ",

        "ignorez cette étape facultative ; ou (2) le type source sélectionné ne correspond pas à vos données. ",

        "Exemple d'identifiant trouvé dans vos données : '%s'."

      ),

      from_type, pct_match * 100, expected_pattern,

      if (length(sample_ids) > 0) sample_ids[1] else "?"

    ), call. = FALSE)

  }



  map_df <- tryCatch({

    AnnotationDbi::select(orgdb, keys = ids_clean, keytype = from_key, columns = "SYMBOL")

  }, error = function(e) {

    if (grepl("not valid keys|None of the keys", conditionMessage(e), ignore.case = TRUE)) {

      stop(sprintf(

        paste0(

          "Aucun de vos identifiants n'est reconnu comme '%s' chez l'organisme '%s'. ",

          "Causes probables : (1) l'organisme sélectionné ne correspond pas à vos données — essayez l'autre option ; ",

          "ou (2) vos données n'utilisent pas d'identifiants de gènes standards, auquel cas ignorez cette étape facultative."

        ),

        from_type, organism

      ), call. = FALSE)

    }

    stop("Échec du mapping d'identifiants : ", conditionMessage(e),

         ". Vérifiez que l'organisme sélectionné correspond bien à vos données.", call. = FALSE)

  })



  map_df <- map_df[!is.na(map_df$SYMBOL), ]

  map_df <- map_df[!duplicated(map_df[[from_key]]), ]  # one symbol per original ID

  id_to_symbol <- setNames(map_df$SYMBOL, map_df[[from_key]])



  mapped_symbols <- id_to_symbol[ids_clean]

  n_unmapped <- sum(is.na(mapped_symbols))

  n_mapped   <- sum(!is.na(mapped_symbols))



  keep <- !is.na(mapped_symbols)

  mat  <- counts_matrix[keep, , drop = FALSE]

  syms <- mapped_symbols[keep]



  n_before_collapse <- nrow(mat)

  if (collapse_method == "sum") {

    agg <- stats::aggregate(as.data.frame(mat), by = list(symbol = syms), FUN = sum)

    rn  <- agg$symbol

    mat <- as.matrix(agg[, -1, drop = FALSE])

    rownames(mat) <- rn

  } else {

    # max_mean: keep highest-expressed probe/ID per symbol, discard the rest

    mean_expr <- rowMeans(mat)

    ord <- order(syms, -mean_expr)

    mat <- mat[ord, , drop = FALSE]

    syms_ord <- syms[ord]

    keep_first <- !duplicated(syms_ord)

    mat <- mat[keep_first, , drop = FALSE]

    rownames(mat) <- syms_ord[keep_first]

  }



  list(

    matrix      = mat,

    n_mapped    = n_mapped,

    n_unmapped  = n_unmapped,

    n_collapsed = n_before_collapse - nrow(mat)

  )

}

