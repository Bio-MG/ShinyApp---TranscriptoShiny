# =============================================================================
# helpers_io.R — Multi-format data loading + gene-ID / metadata mapping
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure I/O and
# identifier-mapping helpers shared across the Import modules
# (mod_import_sc.R, mod_import_bulk.R, mod_import_spatial.R). No Shiny
# reactivity here: every function below is a plain R function operating on
# paths/data.frames, callable and testable outside the app.
#
# Contents:
#   - Single-cell  : load_single_cell_data(), prepare_seurat_object()
#   - Bulk         : read_bulk_matrix(), prepare_bulk_object()
#   - Spatial      : load_spatial_visium(), prepare_spatial_object()
#   - Bulk metadata: infer_metadata_from_names(), preview_metadata_split(),
#                    parse_geo_series_matrix()
#   - Gene IDs     : detect_gene_id_type(), remap_gene_ids_to_symbol()
#
# Depends on: Seurat, tools (base). remap_gene_ids_to_symbol() additionally
# needs org.Hs.eg.db / org.Mm.eg.db (AnnotationDbi) — checked at call time.
# =============================================================================





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




load_spatial_visium <- function(visium_dir, sample_name = "Spatial_Sample",
                                min_counts = 100, min_features = 200) {
  if (!dir.exists(file.path(visium_dir, "spatial"))) {
    stop("Dossier 'spatial' introuvable dans ", visium_dir)
  }
  
  # --- FIX: détection automatique du nom de fichier .h5 ---
  h5_files    <- list.files(visium_dir, pattern = "\\.h5$", full.names = FALSE)
  h5_filename <- head(h5_files[grepl("filtered_feature_bc_matrix", h5_files)], 1)
  
  spatial_obj <- tryCatch({
    if (length(h5_filename) == 1) {
      Load10X_Spatial(
        data.dir      = visium_dir,
        filename      = h5_filename,   # <-- plus de hardcode
        assay         = "Spatial",
        slice         = sample_name,
        filter.matrix = TRUE
      )
    } else {
      stop("Aucun fichier *filtered_feature_bc_matrix.h5 detecte.")
    }
  }, error = function(e) {
    if (dir.exists(file.path(visium_dir, "filtered_feature_bc_matrix"))) {
      Load10X_Spatial(data.dir = visium_dir, assay = "Spatial",
                      slice = sample_name, filter.matrix = TRUE)
    } else {
      stop("Impossible de charger les donnees Visium : ", e$message)
    }
  })
  
  spatial_obj$orig.ident <- sample_name
  subset(spatial_obj, subset = nCount_Spatial >= min_counts & nFeature_Spatial >= min_features)
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



#' Parse a GEO "series_matrix.txt" file into a clean sample x variable metadata table
#'
#' GEO series_matrix.txt files (downloaded directly from a GSE page, no GEOquery/
#' internet access needed) embed per-sample metadata as repeated
#' "!Sample_characteristics_chN" tab-separated lines (one line per characteristic,
#' one quoted "key: value" cell per sample) plus a "!Sample_geo_accession" line
#' giving the GSM IDs in the same column order as the expression/counts matrix.
#' This replaces manual/LLM-assisted extraction (see metadata_GSE52778.csv-style
#' files) with a deterministic local parse — biologist drops the raw file GEO
#' gives them, no curation step required.
#'
#' @param filepath Path to a *_series_matrix.txt file.
#' @return data.frame, one row per sample (rownames = GSM accession), one column
#'   per "!Sample_characteristics_chN" key (+ "title" if present).
parse_geo_series_matrix <- function(filepath) {
  raw_lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)

  split_quoted_tsv <- function(line) {
    parts <- strsplit(line, "\t")[[1]][-1]            # drop the "!Sample_xxx" tag itself
    trimws(gsub('^"|"$', "", parts))
  }

  geo_line   <- raw_lines[startsWith(raw_lines, "!Sample_geo_accession")]
  title_line <- raw_lines[startsWith(raw_lines, "!Sample_title")]
  char_lines <- raw_lines[grepl("^!Sample_characteristics_ch[0-9]+", raw_lines)]

  if (length(geo_line) == 0) {
    stop(paste0(
      "Pas une ligne '!Sample_geo_accession' trouvee. Ce fichier n'est probablement ",
      "pas un series_matrix.txt GEO valide (ou c'est en fait la matrice de counts — ",
      "elle s'importe via 'Option B/C', pas ici)."
    ))
  }

  geo_acc <- split_quoted_tsv(geo_line[1])
  n_samples <- length(geo_acc)
  if (n_samples == 0) stop("Aucun echantillon (GSM) trouve dans ce series_matrix.txt.")

  meta <- data.frame(row.names = geo_acc)

  if (length(title_line) > 0) {
    meta$title <- split_quoted_tsv(title_line[1])[seq_len(n_samples)]
  }

  used_keys <- character(0)
  for (ln in char_lines) {
    vals <- split_quoted_tsv(ln)[seq_len(n_samples)]
    has_kv <- grepl(":", vals)
    key <- if (any(has_kv)) trimws(sub(":.*$", "", vals[has_kv][1])) else "characteristic"
    vals_clean <- ifelse(has_kv, trimws(sub("^[^:]*:", "", vals)), vals)

    key <- make.unique(c(used_keys, key))[length(used_keys) + 1]
    used_keys <- c(used_keys, key)
    meta[[key]] <- vals_clean
  }

  meta
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

    # rowsum() is vastly faster than aggregate() on large matrices (no
    # data.frame round-trip) — same fix pattern as smart_read()'s duplicate
    # GeneID collapse in mod_import_bulk.R.

    mat_num <- mat

    suppressWarnings(mode(mat_num) <- "numeric")

    mat <- rowsum(mat_num, group = syms, reorder = FALSE, na.rm = TRUE)

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
