# =============================================================================
# mod_bulk_report.R  —  Bulk Child 4: Rapport HTML/PDF + Export Script R reproductible
# =============================================================================
# State contract (shared_rv) READ ONLY:
#   vst_mat, contrasts, active_contrast, pathway_results, pca_color_by,
#   pca_palette, pca_manual_colors, volcano_role_colors, lfc_thresh,
#   padj_thresh, heatmap_top_n, pathway_db,
#   active_condition_col   <- Step-3.0: mirrors DE condition column name
# =============================================================================

mod_bulk_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
        "Rapport autonome (PCA, QC, Volcano, Heatmap, Pathways) — partageable sans R."),

    textInput(ns("report_title"), "Titre du rapport",
              value = "Analyse RNA-seq Bulk", placeholder = "Ex: Projet — Cov2 vs Mock"),
    textInput(ns("report_subtitle"), "Sous-titre (optionnel)", placeholder = "Ex: GSE164073"),
    textAreaInput(ns("report_notes"), "Notes / Commentaires (markdown supporté)",
                  rows = 3, placeholder = "Ex: Fichier GSE164073, donneur D3."),

    checkboxGroupInput(ns("report_sections"), "Sections à inclure",
      choices = c("PCA" = "pca", "QC Échantillons" = "qc",
                  "Volcano + MA-Plot" = "volcano", "Heatmap Top Gènes" = "heatmap",
                  "Table DE complète" = "table", "Pathway Enrichment" = "pathway"),
      selected = c("pca", "qc", "volcano", "heatmap", "table", "pathway")),

    radioButtons(ns("report_format"), "Format de sortie",
      choices = c("HTML interactif" = "html", "PDF statique" = "pdf", "Les deux (.zip)" = "both"),
      selected = "html"),

    conditionalPanel(condition = "input.report_format != 'pdf'", ns = ns,
      checkboxInput(ns("report_interactive"),
                    "Graphiques interactifs (PCA, Volcano) — HTML uniquement", value = TRUE)),
    div(class = "small text-muted", "Le PDF requiert LaTeX (tinytex::install_tinytex())."),

    downloadButton(ns("dl_report"), "\U0001f4c4 G\u00e9n\u00e9rer le Rapport",
                   class = "btn-dark w-100 mt-2"),
    hr(),
    div(class = "alert alert-light", style = "font-size:0.82em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("code-slash"),
        " Export script R reproductible — snapshot param\u00e8tres actuels."),
    downloadButton(ns("dl_r_script"), "\U0001f9fe Export Script R Reproductible",
                   class = "btn-outline-secondary w-100"),
    div(class = "small text-muted mt-1", textOutput(ns("report_status")))
  )
}

mod_bulk_report_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggleState("dl_report",   condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_r_script", condition = !is.null(shared_rv$filtered_counts))
    })

    output$report_status <- renderText({
      if (is.null(shared_rv$vst_mat)) "Lancez d'abord l'\u00e9tape 1 (Filtrage & VST)."
      else "Pr\u00eat \u2014 s\u00e9lectionnez les sections puis cliquez sur 'G\u00e9n\u00e9rer Rapport'."
    })

    # ── HTML / PDF Report ──────────────────────────────────────────────────
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html = "html", pdf = "pdf", both = "zip")
        paste0("rapport_bulk_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(shared_rv$vst_mat)
        ac        <- shared_rv$active_contrast
        de_results <- if (!is.null(ac) && ac %in% names(shared_rv$contrasts))
                        shared_rv$contrasts[[ac]] else NULL
        padj_now  <- shared_rv$padj_thresh %||% 0.05
        lfc_now   <- shared_rv$lfc_thresh  %||% 1
        all_c     <- shared_rv$contrasts %||% list()
        all_c_sum <- if (length(all_c) == 0) NULL else
                       summarize_contrasts_updown(all_c, lfc_now, padj_now, ac)

        template_path <- file.path("modules", "bulk", "bulk_report_template.Rmd")
        tmp_rmd <- file.path(tempdir(), "bulk_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite = TRUE)

        render_params <- list(
          vst_mat               = shared_rv$vst_mat,
          metadata              = global_data$bulk_obj$metadata,
          de_results            = de_results,
          contrast_name         = ac,
          all_contrasts_summary = all_c_sum,
          all_contrasts         = all_c,
          pathway_results       = shared_rv$pathway_results,
          pathway_db            = shared_rv$pathway_db %||% "GOBP",
          sections              = input$report_sections %||% character(0),
          pca_color_by          = if (nzchar(shared_rv$pca_color_by %||% "")) shared_rv$pca_color_by else NULL,
          pca_palette           = shared_rv$bulk_palette %||% "default",
          pca_manual_colors     = shared_rv$pca_manual_colors,
          role_colors           = shared_rv$volcano_role_colors,
          heatmap_top_n         = shared_rv$heatmap_top_n %||% 30,
          lfc_thresh            = lfc_now,
          padj_thresh           = padj_now,
          report_title          = input$report_title %||% "Analyse RNA-seq Bulk",
          report_subtitle       = input$report_subtitle %||% "",
          report_notes          = input$report_notes %||% "",
          interactive           = isTRUE(input$report_interactive) && input$report_format != "pdf"
        )

        withProgress(message = "G\u00e9n\u00e9ration du rapport...", value = 0.2, {
          formats_needed <- switch(input$report_format,
            html = "html_document", pdf = "pdf_document",
            both = c("html_document", "pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail = paste("Rendu", fmt))
            out_path <- file.path(tempdir(), paste0("bulk_report_", as.integer(Sys.time())))
            res <- tryCatch(
              rmarkdown::render(input = tmp_rmd, output_format = fmt, output_file = out_path,
                                params = render_params, envir = new.env(parent = globalenv()), quiet = TRUE),
              error = function(e) {
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)), type = "error", duration = 12)
                NULL
              })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (length(out_files) == 0) stop("Aucun format g\u00e9n\u00e9r\u00e9.")
          else if (length(out_files) == 1) file.copy(out_files[1], file, overwrite = TRUE)
          else zip::zip(file, files = out_files, mode = "cherry-pick")
        })
      }
    )

    # ── Script R reproductible ─────────────────────────────────────────────
    output$dl_r_script <- downloadHandler(
      filename = function() paste0("analyse_bulk_", Sys.Date(), ".R"),
      content  = function(file) {
        ac    <- shared_rv$active_contrast %||% ""
        parts <- strsplit(ac, "_vs_")[[1]]
        target <- if (length(parts) >= 2) paste(parts[-length(parts)], collapse = "_") else "GroupA"
        ref    <- if (length(parts) >= 2) parts[length(parts)] else "GroupB"
        writeLines(
          .bulk_r_script_text(
            n_genes       = if (!is.null(shared_rv$filtered_counts)) nrow(shared_rv$filtered_counts) else "?",
            n_samp        = if (!is.null(shared_rv$filtered_counts)) ncol(shared_rv$filtered_counts) else "?",
            lfc           = shared_rv$lfc_thresh  %||% 1,
            padj          = shared_rv$padj_thresh %||% 0.05,
            contrast_name = ac,
            condition_col = shared_rv$active_condition_col %||% "condition",
            group_target  = target,
            group_ref     = ref,
            palette_colors = shared_rv$volcano_role_colors
          ),
          file
        )
      }
    )

  }) # /moduleServer
}

# ── Helper: generate reproducible R script (Step-3.0: MA, Heatmap, Pathway) ──
#' @param n_genes,n_samp Dataset dimensions (informational, in header comment).
#' @param lfc,padj Thresholds mirrored from shared_rv at click time.
#' @param condition_col The actual metadata column name used in the DE design.
#' @param palette_colors Named vector c(Up=, Down=, NS=) for volcano colours.
.bulk_r_script_text <- function(n_genes, n_samp, lfc, padj,
                                 contrast_name, condition_col,
                                 group_target, group_ref, palette_colors) {
  rc   <- palette_colors %||% c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
  date <- format(Sys.Date(), "%Y-%m-%d")
  cond <- condition_col %||% "condition"

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny
# G\u00e9n\u00e9r\u00e9 le : ', date, '
# Contraste  : ', contrast_name, ' (', group_target, ' vs ', group_ref, ')
# Dataset    : ', n_genes, ' g\u00e8nes \u00d7 ', n_samp, ' \u00e9chantillons
# Condition  : ', cond, '
# =============================================================================

library(DESeq2); library(ggplot2); library(dplyr)

# \u2500\u2500 0. Donn\u00e9es \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
counts   <- readRDS("counts_raw.rds")   # matrix: genes x samples (raw integers)
metadata <- readRDS("metadata.rds")     # data.frame: rownames = colnames(counts)

# \u2500\u2500 1. Filtrage \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
filtered <- counts[rowSums(counts, na.rm=TRUE) >= 10 & rowSums(counts >= 1, na.rm=TRUE) >= 1, ]
cat(sprintf("%d genes retained\\n", nrow(filtered)))

# \u2500\u2500 2. VST + PCA \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
dds_b   <- DESeq2::DESeqDataSetFromMatrix(filtered, metadata, design = ~1)
dds_b   <- DESeq2::estimateSizeFactors(dds_b)
vst_mat <- SummarizedExperiment::assay(DESeq2::vst(dds_b, blind = TRUE))

rv  <- apply(vst_mat, 1, var)
sel <- order(rv, decreasing = TRUE)[seq_len(min(500L, nrow(vst_mat)))]
pca <- prcomp(t(vst_mat[sel, ]), scale. = FALSE)
pct <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
df_pca <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], sample=rownames(pca$x))
ggplot(df_pca, aes(PC1, PC2, label=sample)) +
  geom_point(size=3) + geom_text(size=3, vjust=-0.8) +
  labs(title="PCA", x=paste0("PC1 (",pct[1],"%)"), y=paste0("PC2 (",pct[2],"%)")) +
  theme_minimal()

# QC \u2014 corr\u00e9lation inter-\u00e9chantillons
if (requireNamespace("pheatmap", quietly=TRUE))
  pheatmap::pheatmap(cor(vst_mat), main="QC \u2014 Corr\u00e9lation inter-\u00e9chantillons",
                     color=colorRampPalette(c("white","#2C3E50"))(50), fontsize=8)

# \u2500\u2500 3. DESeq2 \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
CONDITION_COL <- "', cond, '"
GROUP_TARGET  <- "', group_target, '"
GROUP_REF     <- "', group_ref, '"
LFC_THRESH    <- ', sprintf("%.3f", lfc), '
PADJ_THRESH   <- ', sprintf("%.4f", padj), '

metadata[[CONDITION_COL]] <- relevel(factor(metadata[[CONDITION_COL]]), ref=GROUP_REF)
dds <- DESeq2::DESeqDataSetFromMatrix(filtered, metadata, design=as.formula(paste("~", CONDITION_COL)))
dds <- DESeq2::DESeq(dds, quiet=TRUE)
coef_name <- paste0(CONDITION_COL, "_", GROUP_TARGET, "_vs_", GROUP_REF)
res <- tryCatch(
  DESeq2::lfcShrink(dds, coef=coef_name, type="apeglm", quiet=TRUE),
  error=function(e) DESeq2::results(dds, contrast=c(CONDITION_COL, GROUP_TARGET, GROUP_REF))
)
res_df       <- as.data.frame(res); res_df$gene <- rownames(res_df)
res_df       <- res_df[order(res_df$padj), ]
cat(sprintf("%d significant genes\\n",
    sum(res_df$padj < PADJ_THRESH & abs(res_df$log2FoldChange) > LFC_THRESH, na.rm=TRUE)))

# \u2500\u2500 4. Volcano \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
res_v <- res_df[!is.na(res_df$padj), ]
res_v$status <- dplyr::case_when(
  res_v$padj < PADJ_THRESH & res_v$log2FoldChange >  LFC_THRESH ~ "Up",
  res_v$padj < PADJ_THRESH & res_v$log2FoldChange < -LFC_THRESH ~ "Down",
  TRUE ~ "NS")
lbl <- c(head(res_v$gene[res_v$status=="Up"],10), head(res_v$gene[res_v$status=="Down"],10))
res_v$label <- ifelse(res_v$gene %in% lbl, res_v$gene, NA_character_)
ggplot(res_v, aes(log2FoldChange, -log10(padj+1e-300), color=status)) +
  geom_point(alpha=0.7, size=1.6) +
  scale_color_manual(values=c(Up="', rc["Up"], '", Down="', rc["Down"], '", NS="', rc["NS"], '")) +
  geom_text(aes(label=label), size=2.8, vjust=-0.6, na.rm=TRUE, show.legend=FALSE) +
  geom_vline(xintercept=c(-LFC_THRESH,LFC_THRESH), linetype="dashed", color="grey40") +
  geom_hline(yintercept=-log10(PADJ_THRESH), linetype="dashed", color="grey40") +
  labs(title="Volcano", subtitle=paste(GROUP_TARGET,"vs",GROUP_REF),
       x="Log2FC", y="-log10(padj)", color="Status") + theme_minimal(base_size=13)

# \u2500\u2500 5. MA-Plot \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
res_ma <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]
res_ma$sig <- res_ma$padj < PADJ_THRESH & abs(res_ma$log2FoldChange) > LFC_THRESH
ggplot(res_ma, aes(log10(baseMean+1), log2FoldChange, color=sig)) +
  geom_point(alpha=0.6, size=1.4) +
  scale_color_manual(values=c("TRUE"="', rc["Up"], '","FALSE"="', rc["NS"], '"), guide="none") +
  geom_hline(yintercept=0, color="grey30") +
  labs(title="MA-Plot", x="Log10(BaseMean+1)", y="Log2FC") + theme_minimal()

# \u2500\u2500 6. Heatmap top g\u00e8nes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# install.packages("pheatmap")  si n\u00e9cessaire
top_genes <- intersect(head(res_df$gene, 30), rownames(vst_mat))
if (length(top_genes) >= 2 && requireNamespace("pheatmap", quietly=TRUE)) {
  mat_z <- t(scale(t(vst_mat[top_genes, ])))
  ann   <- metadata[colnames(mat_z), CONDITION_COL, drop=FALSE]
  pheatmap::pheatmap(mat_z, annotation_col=ann, scale="none",
                     main=paste("Top", length(top_genes), "g\u00e8nes (p-adj)"), fontsize_row=8)
}

# \u2500\u2500 7. Pathway ORA (optionnel) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# BiocManager::install(c("clusterProfiler","org.Hs.eg.db"))
if (requireNamespace("clusterProfiler",quietly=TRUE) && requireNamespace("org.Hs.eg.db",quietly=TRUE)) {
  library(clusterProfiler); library(org.Hs.eg.db)
  sig_g  <- res_df$gene[!is.na(res_df$padj) & res_df$padj < PADJ_THRESH &
                           abs(res_df$log2FoldChange) > LFC_THRESH]
  entrez <- tryCatch(bitr(sig_g,fromType="SYMBOL",toType="ENTREZID",OrgDb=org.Hs.eg.db),
                     error=function(e) NULL)
  if (!is.null(entrez) && nrow(entrez) >= 10) {
    ora <- enrichGO(gene=entrez$ENTREZID, OrgDb=org.Hs.eg.db, ont="BP",
                    pAdjustMethod="BH", pvalueCutoff=0.05, readable=TRUE)
    if (!is.null(ora) && nrow(as.data.frame(ora)) > 0) {
      barplot(ora, showCategory=15, title="GO BP (ORA)")
      write.csv(as.data.frame(ora), paste0("pathways_GOBP_",Sys.Date(),".csv"), row.names=FALSE)
    }
  }
}

# \u2500\u2500 8. Export CSV \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
out <- paste0("DE_",CONDITION_COL,"_",GROUP_TARGET,"_vs_",GROUP_REF,"_",Sys.Date(),".csv")
write.csv(res_df, out, row.names=FALSE)
cat("Results saved to:", out, "\\n")
')
}
