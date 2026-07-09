# =============================================================================
# mod_bulk_report.R  —  Bulk Child 4: Rapport HTML/PDF + Export Script R reproductible
# =============================================================================
# State contract (shared_rv) READ ONLY:
#   vst_mat, contrasts, active_contrast, pathway_results, pca_color_by,
#   pca_palette, pca_manual_colors, volcano_role_colors, lfc_thresh,
#   padj_thresh, heatmap_top_n, pathway_db,
#   active_condition_col   <- Step-3.0: mirrors DE condition column name
#   multimethod_de, multimethod_consensus <- Step-3.5: mirrored by mod_bulk_de.R
# =============================================================================

#' Locate bulk_report_template.Rmd robustly (Step-3.6 fix)
#'
#' The previous version hardcoded a single relative path
#' (`modules/bulk/bulk_report_template.Rmd`), which only resolves when the
#' app's working directory is EXACTLY the project root at render time (true
#' for the normal `shiny::runApp()` from the project folder, but not
#' guaranteed for every deployment/launch method — e.g. Shiny Server app
#' directories, `rsconnect`, or running from a different `wd`). A missing
#' template previously surfaced as a cryptic `file.copy()`/`rmarkdown::render()`
#' error with no actionable message. This tries a small set of realistic
#' candidates and fails with a clear, French, actionable message otherwise.
#'
#' @return Character path to the template file.
.find_bulk_report_template <- function() {
  candidates <- unique(c(
    file.path("modules", "bulk", "bulk_report_template.Rmd"),
    file.path(getwd(), "modules", "bulk", "bulk_report_template.Rmd"),
    file.path(dirname(getwd()), "modules", "bulk", "bulk_report_template.Rmd")
  ))
  hit <- Filter(file.exists, candidates)
  if (length(hit) == 0) {
    stop(
      "\u274c Template 'bulk_report_template.Rmd' introuvable. Chemins essay\u00e9s :\n  - ",
      paste(candidates, collapse = "\n  - "),
      "\nV\u00e9rifiez que l'application est lanc\u00e9e depuis la racine du projet ",
      "(shiny::runApp() depuis le dossier contenant app.R).",
      call. = FALSE
    )
  }
  hit[[1]]
}

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

    # Step-3.5: choix de mise en page pour la section "Toutes les paires"
    # (Volcano/MA/Heatmap pairwise) — grille compacte ou 1 plot pleine page.
    radioButtons(ns("pairwise_layout"), "Mise en page \"Toutes les paires\" (Volcano/MA/Heatmap)",
      choices = c("Petits multiples compilés (grille)" = "grid",
                  "Grand format (1 par contraste)" = "full"),
      selected = "grid", inline = TRUE),

    radioButtons(ns("report_format"), "Format de sortie",
      choices = c("HTML interactif" = "html", "PDF statique" = "pdf", "Les deux (.zip)" = "both"),
      selected = "html"),

    conditionalPanel(condition = "input.report_format != 'pdf'", ns = ns,
      checkboxInput(ns("report_interactive"),
                    "Graphiques interactifs (PCA, Volcano, MA individuel) — HTML uniquement", value = TRUE)),
    div(class = "small text-muted", "Le PDF requiert LaTeX (tinytex::install_tinytex())."),

    downloadButton(ns("dl_report"), "\U0001f4c4 G\u00e9n\u00e9rer le Rapport",
                   class = "btn-dark w-100 mt-2"),
    hr(),
    div(class = "alert alert-light", style = "font-size:0.82em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("code-slash"),
        " Export script R reproductible (.zip) \u2014 contient le script + counts_raw.rds + metadata.rds. ",
        "Pour l'ex\u00e9cuter : d\u00e9compressez le .zip, ouvrez R/RStudio dans CE dossier ",
        "(ou faites setwd() dessus), puis source(\"analyse_bulk_....R\") ou ex\u00e9cutez-le ligne par ligne."),
    downloadButton(ns("dl_r_script"), "\U0001f9fe Export Script R Reproductible (.zip)",
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
        # Step-3.5: horodatage à la seconde — évite l'écrasement si plusieurs
        # exports sont générés le même jour (Sys.Date() seul ne suffisait pas).
        paste0("rapport_bulk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
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

        template_path <- tryCatch(.find_bulk_report_template(), error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 15)
          NULL
        })
        req(template_path)
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
          # Step-3.5: Multi-méthodes (consensus DESeq2/edgeR/limma) — miroir
          # écrit par mod_bulk_de.R après un run "🔬 Comparer".
          multimethod_de        = shared_rv$multimethod_de,
          multimethod_consensus = shared_rv$multimethod_consensus,
          # Step-3.5: choix de mise en page pour "Toutes les paires".
          pairwise_layout       = input$pairwise_layout %||% "grid",
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
            # Step-3.5 FIX: tempfile() avec fileext explicite -- l'ancienne
            # version encodait les secondes fractionnaires (%OS3) DANS le nom
            # de base, qui contient donc un "." (ex: "..._143022.456").
            # rmarkdown interprete CE point comme l'extension du fichier et
            # n'ajoute PAS le vrai ".html"/".pdf" -- inoffensif pour un export
            # simple (Shiny renomme via downloadHandler$filename), mais casse
            # le mode "Les deux (.zip)" : les 2 fichiers dans l'archive
            # n'avaient plus d'extension reconnaissable et n'etaient plus
            # ouvrables directement une fois decompresses.
            ext_i    <- if (fmt == "html_document") "html" else "pdf"
            out_path <- tempfile(pattern = paste0("bulk_report_", ext_i, "_"), fileext = paste0(".", ext_i))
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
      # Step-3.5: horodatage à la seconde — même logique que dl_report.
      filename = function() paste0("analyse_bulk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$bulk_obj)
        ac    <- shared_rv$active_contrast %||% ""
        parts <- strsplit(ac, "_vs_")[[1]]
        target <- if (length(parts) >= 2) paste(parts[-length(parts)], collapse = "_") else "GroupA"
        ref    <- if (length(parts) >= 2) parts[length(parts)] else "GroupB"

        script_text <- .bulk_r_script_text(
          n_genes       = if (!is.null(shared_rv$filtered_counts)) nrow(shared_rv$filtered_counts) else "?",
          n_samp        = if (!is.null(shared_rv$filtered_counts)) ncol(shared_rv$filtered_counts) else "?",
          lfc           = shared_rv$lfc_thresh  %||% 1,
          padj          = shared_rv$padj_thresh %||% 0.05,
          contrast_name = ac,
          condition_col = shared_rv$active_condition_col %||% "condition",
          group_target  = target,
          group_ref     = ref,
          palette_colors = shared_rv$volcano_role_colors,
          all_contrast_names = names(shared_rv$contrasts),
          pathway_mode  = shared_rv$pathway_mode %||% "ora"
        )

        # ── Bundle companion data (Step-3.5 fix) ──────────────────────────
        # The script reads `counts_raw.rds` / `metadata.rds` from its own
        # working directory (see section 0 of the generated script) — these
        # were never actually produced before, making the script impossible
        # to run standalone. RAW (unfiltered, pre-VST) counts are used, since
        # the script re-does its own filtering — same matrix mod_bulk_filter
        # would start from (mapped IDs if Step-0 mapping was applied).
        tmp_dir <- tempfile("bulk_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        # Step-3.5: le nom du .R interne au zip est lui aussi horodaté à la
        # seconde — cohérent avec le nom du .zip, et évite toute confusion
        # si l'utilisateur extrait plusieurs zips dans le même dossier.
        script_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_bulk_", script_stamp, ".R"))
        writeLines(script_text, script_path)

        raw_counts <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
        saveRDS(raw_counts, file.path(tmp_dir, "counts_raw.rds"))
        saveRDS(global_data$bulk_obj$metadata, file.path(tmp_dir, "metadata.rds"))

        zip::zip(file, files = c(script_path,
                                 file.path(tmp_dir, "counts_raw.rds"),
                                 file.path(tmp_dir, "metadata.rds")),
                 mode = "cherry-pick")
      }
    )

  }) # /moduleServer
}

# ── Helper: generate reproducible R script (Step-3.0: MA, Heatmap, Pathway;
#    Step-3.0b: pairwise section when the app computed >1 contrast) ─────────
#' @param n_genes,n_samp Dataset dimensions (informational, in header comment).
#' @param lfc,padj Thresholds mirrored from shared_rv at click time.
#' @param condition_col The actual metadata column name used in the DE design.
#' @param palette_colors Named vector c(Up=, Down=, NS=) for volcano colours.
#' @param all_contrast_names Names of every contrast in shared_rv$contrasts —
#'   when length > 1 (pairwise-auto or repeated manual runs), an extra
#'   "3bis. Pairwise" section is generated computing/exporting every OTHER
#'   pair by reusing the same fitted `dds` (results()/lfcShrink only, no
#'   refit) — keeps the exported script a faithful, complete reproduction of
#'   the session rather than just the single active contrast.
.bulk_r_script_text <- function(n_genes, n_samp, lfc, padj,
                                 contrast_name, condition_col,
                                 group_target, group_ref, palette_colors,
                                 all_contrast_names = NULL, pathway_mode = "ora") {
  rc   <- palette_colors %||% c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
  date <- format(Sys.Date(), "%Y-%m-%d")
  cond <- condition_col %||% "condition"

  # ── Pairwise block (only when the app has >1 stored contrast) ────────────
  other_names <- setdiff(all_contrast_names %||% character(0), contrast_name)
  pairwise_block <- ""
  if (length(other_names) > 0) {
    pair_list_lines <- vapply(other_names, function(nm) {
      parts_nm <- strsplit(nm, "_vs_")[[1]]
      tgt <- paste(parts_nm[-length(parts_nm)], collapse = "_")
      rf  <- parts_nm[length(parts_nm)]
      sprintf('  list(target = "%s", ref = "%s")', tgt, rf)
    }, character(1))
    pairwise_block <- paste0(
'
# \u2500\u2500 3bis. Pairwise \u2014 autres contrastes calcul\u00e9s dans l\'app (', length(other_names), ' paire(s) suppl.) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# R\u00e9utilise le M\u00cAME ajustement DESeq2 "dds" (design ~ ', cond, ') pour chaque paire \u2014
# results()/lfcShrink() seulement ; DESeq() n\'est PAS relanc\u00e9.
other_pairs <- list(
', paste(pair_list_lines, collapse = ",\n"), '
)
all_de <- list()
all_de[[paste0(GROUP_TARGET, "_vs_", GROUP_REF)]] <- res_df
for (pr in other_pairs) {
  coef_p <- paste0(CONDITION_COL, "_", pr$target, "_vs_", pr$ref)
  res_p <- tryCatch({
    r0 <- DESeq2::results(dds, contrast = c(CONDITION_COL, pr$target, pr$ref))
    if (coef_p %in% DESeq2::resultsNames(dds)) {
      DESeq2::lfcShrink(dds, coef = coef_p, res = r0, type = "apeglm", quiet = TRUE)
    } else {
      DESeq2::lfcShrink(dds, contrast = c(CONDITION_COL, pr$target, pr$ref), res = r0, type = "normal", quiet = TRUE)
    }
  }, error = function(e) DESeq2::results(dds, contrast = c(CONDITION_COL, pr$target, pr$ref)))
  df_p <- as.data.frame(res_p); df_p$gene <- rownames(df_p); df_p <- df_p[order(df_p$padj), ]
  all_de[[paste0(pr$target, "_vs_", pr$ref)]] <- df_p
  write.csv(df_p, paste0("DE_", pr$target, "_vs_", pr$ref, "_", Sys.Date(), ".csv"), row.names = FALSE)
}

# R\u00e9sum\u00e9 Up/Down \u2014 TOUS les contrastes (actif + pairwise)
summary_rows <- lapply(names(all_de), function(nm) {
  d <- all_de[[nm]]
  sig <- !is.na(d$padj) & d$padj < PADJ_THRESH & abs(d$log2FoldChange) > LFC_THRESH
  data.frame(Contraste = nm, n_testes = nrow(d), n_sig = sum(sig),
             n_up = sum(sig & d$log2FoldChange > 0), n_down = sum(sig & d$log2FoldChange < 0))
})
summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, paste0("summary_updown_pairwise_", Sys.Date(), ".csv"), row.names = FALSE)
print(summary_df)
'
    )
  }

  # ── Pathway section: ORA (significant genes) or GSEA (full ranked list) ──
  pathway_section <- if (identical(pathway_mode, "gsea")) {
'# \u2500\u2500 7. Pathway GSEA (optionnel, sans seuil) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# BiocManager::install(c("clusterProfiler","org.Hs.eg.db","enrichplot"))
if (requireNamespace("clusterProfiler",quietly=TRUE) && requireNamespace("org.Hs.eg.db",quietly=TRUE)) {
  library(clusterProfiler); library(org.Hs.eg.db)
  de_gsea <- res_df[!is.na(res_df$padj) & !is.na(res_df$log2FoldChange) & !is.na(res_df$pvalue), ]
  gene_entrez <- tryCatch(
    AnnotationDbi::select(org.Hs.eg.db, keys=unique(de_gsea$gene), keytype="SYMBOL", columns="ENTREZID"),
    error=function(e) NULL)
  if (!is.null(gene_entrez)) {
    gene_entrez <- gene_entrez[!is.na(gene_entrez$ENTREZID) & !duplicated(gene_entrez$SYMBOL), ]
    de_m <- merge(de_gsea, gene_entrez, by.x="gene", by.y="SYMBOL")
    de_m$rank_metric <- -log10(pmax(de_m$pvalue, 1e-300)) * sign(de_m$log2FoldChange)
    de_m <- stats::aggregate(rank_metric ~ ENTREZID, data=de_m, FUN=mean)
    ranked <- sort(setNames(de_m$rank_metric, de_m$ENTREZID), decreasing=TRUE)
    if (length(ranked) >= 10) {
      gsea_res <- gseGO(geneList=ranked, OrgDb=org.Hs.eg.db, ont="BP",
                        pvalueCutoff=0.05, pAdjustMethod="BH", verbose=FALSE)
      if (!is.null(gsea_res) && nrow(as.data.frame(gsea_res)) > 0) {
        write.csv(as.data.frame(gsea_res), paste0("pathways_GSEA_GOBP_",Sys.Date(),".csv"), row.names=FALSE)
        if (requireNamespace("enrichplot", quietly=TRUE))
          print(enrichplot::gseaplot2(gsea_res, geneSetID=1, title=as.data.frame(gsea_res)$Description[1]))
      }
    }
  }
}
'
  } else {
'# \u2500\u2500 7. Pathway ORA (optionnel) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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
'
  }

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny
# G\u00e9n\u00e9r\u00e9 le : ', date, '
# Contraste  : ', contrast_name, ' (', group_target, ' vs ', group_ref, ')
# Dataset    : ', n_genes, ' g\u00e8nes \u00d7 ', n_samp, ' \u00e9chantillons
# Condition  : ', cond, '
# =============================================================================

library(DESeq2); library(ggplot2); library(dplyr)

# \u2500\u2500 0. Donn\u00e9es \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
counts   <- readRDS("counts_raw.rds")   # matrix: genes x samples (raw integers)
metadata <- readRDS("metadata.rds")     # data.frame: rownames = colnames(counts)

# \u2500\u2500 1. Filtrage \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
filtered <- counts[rowSums(counts, na.rm=TRUE) >= 10 & rowSums(counts >= 1, na.rm=TRUE) >= 1, ]
cat(sprintf("%d genes retained\\n", nrow(filtered)))

# \u2500\u2500 2. VST + PCA \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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

# \u2500\u2500 3. DESeq2 \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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
', pairwise_block, '
# \u2500\u2500 4. Volcano \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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

# \u2500\u2500 5. MA-Plot \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
res_ma <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]
res_ma$sig <- res_ma$padj < PADJ_THRESH & abs(res_ma$log2FoldChange) > LFC_THRESH
ggplot(res_ma, aes(log10(baseMean+1), log2FoldChange, color=sig)) +
  geom_point(alpha=0.6, size=1.4) +
  scale_color_manual(values=c("TRUE"="', rc["Up"], '","FALSE"="', rc["NS"], '"), guide="none") +
  geom_hline(yintercept=0, color="grey30") +
  labs(title="MA-Plot", x="Log10(BaseMean+1)", y="Log2FC") + theme_minimal()

# \u2500\u2500 6. Heatmap top g\u00e8nes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# install.packages("pheatmap")  si n\u00e9cessaire
top_genes <- intersect(head(res_df$gene, 30), rownames(vst_mat))
if (length(top_genes) >= 2 && requireNamespace("pheatmap", quietly=TRUE)) {
  mat_z <- t(scale(t(vst_mat[top_genes, ])))
  ann   <- metadata[colnames(mat_z), CONDITION_COL, drop=FALSE]
  pheatmap::pheatmap(mat_z, annotation_col=ann, scale="none",
                     main=paste("Top", length(top_genes), "g\u00e8nes (p-adj)"), fontsize_row=8)
}


', pathway_section, '

# \u2500\u2500 7bis. edgeR / limma-voom (optionnel, m\u00eame contraste) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# D\u00e9comment\u00e9 sur demande \u2014 relance le M\u00eame contraste avec un moteur diff\u00e9rent,
# pour comparaison manuelle avec les r\u00e9sultats DESeq2 ci-dessus.
# ---------------------------------------------------------------------------
# if (requireNamespace("edgeR", quietly=TRUE)) {
#   grp    <- factor(metadata[[CONDITION_COL]], levels=c(GROUP_REF, GROUP_TARGET))
#   y      <- edgeR::DGEList(counts=round(filtered), group=grp)
#   y      <- edgeR::calcNormFactors(y)
#   design <- stats::model.matrix(~grp)
#   y      <- edgeR::estimateDisp(y, design)
#   fit    <- edgeR::glmQLFit(y, design)
#   qlf    <- edgeR::glmQLFTest(fit, coef=2)
#   res_edger <- edgeR::topTags(qlf, n=Inf)$table
#   res_edger$gene <- rownames(res_edger)
#   write.csv(res_edger, paste0("DE_edgeR_", CONDITION_COL, "_", Sys.Date(), ".csv"), row.names=FALSE)
# }
# if (requireNamespace("limma", quietly=TRUE) && requireNamespace("edgeR", quietly=TRUE)) {
#   grp2    <- factor(metadata[[CONDITION_COL]], levels=c(GROUP_REF, GROUP_TARGET))
#   y2      <- edgeR::calcNormFactors(edgeR::DGEList(counts=round(filtered)))
#   design2 <- stats::model.matrix(~grp2)
#   v       <- limma::voom(y2, design2)
#   fit2    <- limma::eBayes(limma::lmFit(v, design2))
#   res_limma <- limma::topTable(fit2, coef=2, number=Inf, sort.by="P")
#   res_limma$gene <- rownames(res_limma)
#   write.csv(res_limma, paste0("DE_limma_", CONDITION_COL, "_", Sys.Date(), ".csv"), row.names=FALSE)
# }

# \u2500\u2500 8. Export CSV \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
out <- paste0("DE_",CONDITION_COL,"_",GROUP_TARGET,"_vs_",GROUP_REF,"_",Sys.Date(),".csv")
write.csv(res_df, out, row.names=FALSE)
cat("Results saved to:", out, "\\n")
')
}
