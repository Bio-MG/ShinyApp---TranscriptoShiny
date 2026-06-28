# =============================================================================
# helpers_pathway.R — Pathway enrichment (ORA + GSEA), shared sc + bulk
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — single source
# of truth for pathway analysis, called by BOTH mod_sc_pathways.R and
# mod_bulk_pathways.R so the two domains never diverge in methodology.
#
# Contents:
#   - run_pathway_enrichment() : ORA (GO/KEGG/Reactome) via clusterProfiler
#   - run_gsea_enrichment()    : pre-ranked GSEA (GO/KEGG/Reactome)
#   - plot_pathway_barplot(), plot_pathway_dotplot(), build_pathway_dt()
#
# Depends on: clusterProfiler, org.Hs.eg.db / org.Mm.eg.db, ReactomePA
# (KEGG/Reactome only), KEGGREST (optional), ggplot2, viridis, DT.
# =============================================================================




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
