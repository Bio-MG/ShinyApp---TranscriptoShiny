# =============================================================================
# R/sc/sc_velocity.R — RNA Velocity validation, I/O & plotting
# (extracted from helpers_sc.R, Block 7 refactor)
# =============================================================================
# Pure functions: no Shiny reactivity. Called by:
#   - modules/sc/mod_sc_velocity.R (UI module)
#
# Depends on: Matrix, ggplot2
# Sourced in app.R AFTER helpers_io.R, BEFORE modules/sc/*.R
# =============================================================================

#' Detect matrix orientation from explicit identifiers
#'
#' @param mat Matrix with row and column identifiers.
#' @param seurat_cells Cell barcodes from the Seurat object.
#' @param seurat_genes Gene identifiers from the Seurat object.
#' @param orientation User-selected orientation.
#' @return One of "genes_x_cells" or "cells_x_genes".
#' @export
detect_velocity_orientation <- function(
    mat,
    seurat_cells = NULL,
    seurat_genes = NULL,
    orientation = c(
        "auto_strict",
        "genes_x_cells",
        "cells_x_genes"
    )
) {
    orientation <- match.arg(orientation)
    mat <- as.matrix(mat)

    if (is.null(rownames(mat)) || is.null(colnames(mat))) {
        stop(
            "Orientation impossible : les matrices doivent posseder des ",
            "identifiants de lignes et de colonnes."
        )
    }

    if (orientation != "auto_strict") {
        return(orientation)
    }

    if (is.null(seurat_cells) || is.null(seurat_genes)) {
        stop(
            "Orientation automatique impossible sans identifiants ",
            "de cellules et de genes Seurat."
        )
    }

    row_cell_overlap <- sum(rownames(mat) %in% seurat_cells)
    col_cell_overlap <- sum(colnames(mat) %in% seurat_cells)
    row_gene_overlap <- sum(rownames(mat) %in% seurat_genes)
    col_gene_overlap <- sum(colnames(mat) %in% seurat_genes)

    genes_x_cells_score <- row_gene_overlap + col_cell_overlap
    cells_x_genes_score <- col_gene_overlap + row_cell_overlap

    if (genes_x_cells_score == cells_x_genes_score) {
        stop(
            "Orientation ambigue : les deux orientations restent plausibles. ",
            "Choisissez explicitement Genes x cellules ou Cellules x genes."
        )
    }

    if (genes_x_cells_score > cells_x_genes_score) {
        return("genes_x_cells")
    }

    "cells_x_genes"
}

#' Validate matrix identifiers
#'
#' @param mat Matrix to validate.
#' @param label Human-readable matrix label.
#' @return Invisible TRUE or stops with an informative error.
#' @export
validate_velocity_identifiers <- function(
    mat,
    label = "velocity matrix"
) {
    if (is.null(rownames(mat)) || is.null(colnames(mat))) {
        stop(
            "Identifiants absents dans ",
            label,
            " : rownames et colnames sont obligatoires."
        )
    }

    duplicated_rows <- unique(rownames(mat)[duplicated(rownames(mat))])
    duplicated_cols <- unique(colnames(mat)[duplicated(colnames(mat))])

    if (length(duplicated_rows) > 0L) {
        stop(
            "Identifiants de lignes dupliques dans ",
            label,
            " : ",
            paste(head(duplicated_rows, 10L), collapse = ", "),
            "."
        )
    }

    if (length(duplicated_cols) > 0L) {
        stop(
            "Identifiants de colonnes dupliques dans ",
            label,
            " : ",
            paste(head(duplicated_cols, 10L), collapse = ", "),
            "."
        )
    }

    invisible(TRUE)
}

#' Normalize cell barcodes without silently creating collisions
#'
#' @param ids Cell barcodes.
#' @param strip_suffix Logical, remove terminal -1/-2 style suffix.
#' @return Normalized barcodes.
#' @export
normalize_velocity_cell_barcodes <- function(
    ids,
    strip_suffix = FALSE
) {
    ids <- as.character(ids)

    if (!isTRUE(strip_suffix)) {
        return(ids)
    }

    normalized <- sub("-[0-9]+$", "", ids)
    collisions <- unique(normalized[duplicated(normalized)])

    if (length(collisions) > 0L) {
        stop(
            "Le retrait des suffixes de barcodes cree des collisions : ",
            paste(head(collisions, 10L), collapse = ", "),
            ". Desactivez cette option ou corrigez les identifiants."
        )
    }

    normalized
}

#' Normalize gene identifiers without silently creating collisions
#'
#' @param ids Gene identifiers.
#' @param strip_version Logical, remove terminal Ensembl version suffix.
#' @return Normalized gene identifiers.
#' @export
normalize_velocity_gene_ids <- function(
    ids,
    strip_version = FALSE
) {
    ids <- as.character(ids)

    if (!isTRUE(strip_version)) {
        return(ids)
    }

    normalized <- sub("\\.[0-9]+$", "", ids)
    collisions <- unique(normalized[duplicated(normalized)])

    if (length(collisions) > 0L) {
        stop(
            "Le retrait des versions de genes cree des collisions : ",
            paste(head(collisions, 10L), collapse = ", "),
            ". Desactivez cette option ou corrigez les identifiants."
        )
    }

    normalized
}

#' Validate spliced and unspliced matrices
#'
#' @param spliced Spliced matrix.
#' @param unspliced Unspliced matrix.
#' @param ambiguous Optional ambiguous matrix.
#' @param seurat_cells Seurat cell barcodes.
#' @param seurat_genes Seurat gene identifiers.
#' @param orientation Matrix orientation.
#' @param strip_cell_suffix Remove cell barcode suffixes.
#' @param strip_gene_version Remove gene version suffixes.
#' @param allow_low_overlap Allow overlap < 80 percent.
#' @return Validated velocity structure and alignment report.
#' @export
validate_velocity_matrices <- function(
    spliced,
    unspliced,
    ambiguous = NULL,
    seurat_cells = NULL,
    seurat_genes = NULL,
    orientation = "auto_strict",
    strip_cell_suffix = FALSE,
    strip_gene_version = FALSE,
    allow_low_overlap = FALSE
) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
        stop("Le package Matrix est requis pour valider les matrices.")
    }

    if (!is.matrix(spliced) && !inherits(spliced, "Matrix")) {
        stop("La matrice spliced doit etre une matrice numerique.")
    }

    if (!is.matrix(unspliced) && !inherits(unspliced, "Matrix")) {
        stop("La matrice unspliced doit etre une matrice numerique.")
    }

    if (!identical(dim(spliced), dim(unspliced))) {
        stop(
            "Les matrices spliced et unspliced doivent avoir les memes dimensions."
        )
    }

    validate_velocity_identifiers(spliced, "spliced")
    validate_velocity_identifiers(unspliced, "unspliced")

    if (!identical(rownames(spliced), rownames(unspliced)) ||
        !identical(colnames(spliced), colnames(unspliced))) {
        stop(
            "Les identifiants des matrices spliced et unspliced ",
            "doivent etre identiques et dans le meme ordre."
        )
    }

    if (!is.null(ambiguous)) {
        if (!identical(dim(spliced), dim(ambiguous))) {
            stop(
                "La matrice ambiguous doit avoir les memes dimensions ",
                "que spliced et unspliced."
            )
        }

        validate_velocity_identifiers(ambiguous, "ambiguous")

        if (!identical(rownames(spliced), rownames(ambiguous)) ||
            !identical(colnames(spliced), colnames(ambiguous))) {
            stop(
                "Les identifiants de la matrice ambiguous doivent etre ",
                "identiques a ceux de spliced."
            )
        }
    }

    detected_orientation <- detect_velocity_orientation(
        mat = spliced,
        seurat_cells = seurat_cells,
        seurat_genes = seurat_genes,
        orientation = orientation
    )

    if (detected_orientation == "cells_x_genes") {
        spliced <- Matrix::t(spliced)
        unspliced <- Matrix::t(unspliced)
        if (!is.null(ambiguous)) {
            ambiguous <- Matrix::t(ambiguous)
        }
    }

    cell_ids_original <- colnames(spliced)
    gene_ids_original <- rownames(spliced)

    cell_ids <- normalize_velocity_cell_barcodes(
        cell_ids_original,
        strip_suffix = strip_cell_suffix
    )

    gene_ids <- normalize_velocity_gene_ids(
        gene_ids_original,
        strip_version = strip_gene_version
    )

    if (anyDuplicated(cell_ids)) {
        stop(
            "Les identifiants de cellules deviennent dupliques apres normalisation."
        )
    }

    if (anyDuplicated(gene_ids)) {
        stop(
            "Les identifiants de genes deviennent dupliques apres normalisation."
        )
    }

    if (!is.null(seurat_cells)) {
        seurat_cells <- as.character(seurat_cells)
        # Raw overlap is computed BEFORE any normalization so the provenance
        # report can show both raw and normalized agreement (BUG 5/BUG 6).
        seurat_cells_raw <- seurat_cells
        raw_cell_overlap <- length(intersect(cell_ids_original, seurat_cells_raw)) /
            max(1L, length(seurat_cells_raw))

        seurat_cells_normalized <- normalize_velocity_cell_barcodes(
            seurat_cells,
            strip_suffix = strip_cell_suffix
        )

        if (anyDuplicated(seurat_cells_normalized)) {
            stop(
                "Les cellules Seurat deviennent dupliquees apres normalisation."
            )
        }

        matched_cells <- intersect(
            seurat_cells_normalized,
            cell_ids
        )

        n_cells_input <- length(cell_ids)
        n_cells_matched <- length(matched_cells)
        overlap <- n_cells_matched / max(1L, length(seurat_cells_normalized))

        if (overlap < 0.8 && !isTRUE(allow_low_overlap)) {
            stop(
                "Recouvrement des cellules inferieur a 80 pourcent : ",
                sprintf("%.1f", 100 * overlap),
                " pourcent. Activez explicitement l'option ",
                "'Forcer l'alignement faible' pour continuer."
            )
        }

        cell_order <- match(
            seurat_cells_normalized,
            cell_ids
        )

        keep_cells <- !is.na(cell_order)

        spliced <- spliced[, cell_order[keep_cells], drop = FALSE]
        unspliced <- unspliced[, cell_order[keep_cells], drop = FALSE]

        if (!is.null(ambiguous)) {
            ambiguous <- ambiguous[, cell_order[keep_cells], drop = FALSE]
        }

        aligned_cells <- seurat_cells[keep_cells]
    } else {
        aligned_cells <- cell_ids_original
        n_cells_input <- length(cell_ids)
        n_cells_matched <- length(cell_ids)
        overlap <- NA_real_
        raw_cell_overlap <- NA_real_
    }

    if (!is.null(seurat_genes)) {
        seurat_genes <- as.character(seurat_genes)
        seurat_genes_normalized <- normalize_velocity_gene_ids(
            seurat_genes,
            strip_version = strip_gene_version
        )

        if (anyDuplicated(seurat_genes_normalized)) {
            stop(
                "Les genes Seurat deviennent dupliques apres normalisation."
            )
        }

        gene_order <- match(
            seurat_genes_normalized,
            gene_ids
        )

        keep_genes <- !is.na(gene_order)

        if (!any(keep_genes)) {
            stop(
                "Aucun gene velocity ne correspond aux genes de l'objet Seurat."
            )
        }

        spliced <- spliced[gene_order[keep_genes], , drop = FALSE]
        unspliced <- unspliced[gene_order[keep_genes], , drop = FALSE]

        if (!is.null(ambiguous)) {
            ambiguous <- ambiguous[gene_order[keep_genes], , drop = FALSE]
        }

        aligned_genes <- seurat_genes[keep_genes]
    } else {
        aligned_genes <- gene_ids_original
    }

    # update rownames/colnames to reflect aligned Seurat identifiers for downstream consistency
    rownames(spliced) <- aligned_genes
    rownames(unspliced) <- aligned_genes
    colnames(spliced) <- aligned_cells
    colnames(unspliced) <- aligned_cells
    if (!is.null(ambiguous)) {
        rownames(ambiguous) <- aligned_genes
        colnames(ambiguous) <- aligned_cells
    }

    list(
        spliced = spliced,
        unspliced = unspliced,
        ambiguous = ambiguous,
        cell_names = aligned_cells,
        gene_names = aligned_genes,
        orientation = detected_orientation,
        cell_match_mode = if (isTRUE(strip_cell_suffix)) "suffix" else "exact",
        gene_match_mode = if (isTRUE(strip_gene_version)) "version" else "exact",
        strip_cell_suffix = isTRUE(strip_cell_suffix),
        strip_gene_version = isTRUE(strip_gene_version),
        velocity_source = "import",
        velocity_method = "precomputed",
        n_cells_input = n_cells_input,
        n_genes_input = length(gene_ids),
        n_cells_matched = ncol(spliced),
        n_genes_matched = nrow(spliced),
        n_cells_missing = if (!is.null(seurat_cells)) {
            length(seurat_cells) - ncol(spliced)
        } else {
            0L
        },
        n_genes_missing = if (!is.null(seurat_genes)) {
            length(seurat_genes) - nrow(spliced)
        } else {
            0L
        },
        n_cell_collisions = 0L,
        n_gene_collisions = 0L,
        warnings = character(0),
        overlap = overlap,
        raw_cell_overlap = raw_cell_overlap,
        normalized_cell_overlap = overlap,
        allow_low_overlap = isTRUE(allow_low_overlap)
    )
}

#' Read a combined RNA velocity RDS object
#'
#' @param path RDS file path.
#' @return Named velocity input list.
#' @export
read_velocity_rds <- function(path) {
    object <- readRDS(path)

    allowed_names <- c(
        "spliced",
        "unspliced",
        "ambiguous",
        "cell_names",
        "gene_names",
        "velocity_source",
        "velocity_method",
        "orientation",
        "clusters",
        "umap_embedding",
        "umap_velocity",
        "embedding_velocity",
        "umap_vectors",
        "vectors",
        "embedding_reduction",
        "stored_reduction",
        "reduction",
        "velocity_metadata",
        "alignment"
    )

    if (!is.list(object) || is.null(names(object))) {
        stop(
            "Le fichier RDS velocity doit contenir une liste nommee avec ",
            "spliced et unspliced."
        )
    }

    observed_names <- names(object)
    unexpected_names <- setdiff(observed_names, allowed_names)

    if (length(unexpected_names) > 0L) {
        stop(
            "Noms inattendus dans le RDS velocity : ",
            paste(unexpected_names, collapse = ", "),
            ". Noms observes : ",
            paste(observed_names, collapse = ", "),
            "."
        )
    }

    if (!all(c("spliced", "unspliced") %in% observed_names)) {
        stop(
            "Le RDS velocity doit contenir exactement les elements ",
            "'spliced' et 'unspliced'."
        )
    }

    validate_velocity_rds_metadata(object)

    object
}

#' Validate metadata fields of a combined RNA velocity RDS object
#'
#' Structural checks only — no inference, no transposition, no padding.
#'
#' @param velocity_input Named list returned by read_velocity_rds().
#' @return Invisible TRUE or stops with an informative French error.
#' @export
validate_velocity_rds_metadata <- function(velocity_input) {
    if (is.null(velocity_input) || !is.list(velocity_input)) {
        stop("Entrée velocity invalide : liste nommee attendue.")
    }

    .assert_optional_single_string <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        if (!is.character(value) || length(value) != 1L || is.na(value)) {
            stop(sprintf(
                "%s doit etre une chaine de caracteres unique.",
                field
            ))
        }
        invisible(TRUE)
    }

    .assert_optional_string_vector <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        if (!is.character(value) || anyNA(value)) {
            stop(sprintf("%s doit etre un vecteur de chaines sans NA.", field))
        }
        invisible(TRUE)
    }

    .assert_two_col_numeric_matrix <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        value <- as.matrix(value)
        if (!is.numeric(value) || ncol(value) != 2L) {
            stop(sprintf(
                "%s doit etre une matrice numerique avec exactement deux colonnes.",
                field
            ))
        }
        if (is.null(rownames(value))) {
            stop(sprintf(
                "%s doit posseder des noms de lignes (barcodes de cellules).",
                field
            ))
        }
        if (anyDuplicated(rownames(value))) {
            stop(sprintf(
                "%s contient des noms de lignes dupliques.",
                field
            ))
        }
        invisible(TRUE)
    }

    .assert_optional_single_string(
        velocity_input$velocity_source,
        "'velocity_source'"
    )
    .assert_optional_single_string(
        velocity_input$velocity_method,
        "'velocity_method'"
    )

    if (!is.null(velocity_input$orientation) &&
        !velocity_input$orientation %in%
            c("genes_x_cells", "cells_x_genes", "auto_strict")) {
        stop(paste(
            "'orientation' doit valoir 'genes_x_cells', 'cells_x_genes'",
            "ou 'auto_strict'."
        ))
    }

    .assert_optional_string_vector(velocity_input$gene_names, "'gene_names'")
    .assert_optional_string_vector(velocity_input$cell_names, "'cell_names'")

    # clusters: optional, one value per velocity cell (column axis by
    # convention genes_x_cells; row axis when orientation == cells_x_genes).
    if (!is.null(velocity_input$clusters)) {
        spliced_dim <- dim(velocity_input$spliced)
        n_cells_rds <- if (identical(velocity_input$orientation, "cells_x_genes")) {
            spliced_dim[1L]
        } else {
            spliced_dim[2L]
        }
        if (length(velocity_input$clusters) != n_cells_rds) {
            stop(sprintf(
                "'clusters' doit contenir une valeur par cellule velocity (%d attendues, %d fournies).",
                n_cells_rds, length(velocity_input$clusters)
            ))
        }
    }

    # Coordinates: partial alignment is allowed, padding is forbidden.
    .assert_two_col_numeric_matrix(
        velocity_input$umap_embedding,
        "'umap_embedding'"
    )

    # Velocity vectors in embedding space: same structural contract.
    # umap_embedding is deliberately NOT routed through this check as a
    # vector — coordinates and dx/dy remain separate concepts.
    .assert_two_col_numeric_matrix(velocity_input$umap_velocity, "'umap_velocity'")
    .assert_two_col_numeric_matrix(
        velocity_input$embedding_velocity,
        "'embedding_velocity'"
    )

    invisible(TRUE)
}

.read_lines_maybe_gz <- function(path) {
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
        gzfile(path, open = "rt")
    } else {
        file(path, open = "rt")
    }
    on.exit(close(con), add = TRUE)
    readLines(con, warn = FALSE)
}

#' Read RNA velocity Matrix Market inputs
#'
#' Requires the three files (matrix, barcodes, features/genes). Gzipped
#' barcode/feature files are supported transparently. The sparse Matrix
#' object returned by Matrix::readMM() is preserved as-is (never densified).
#' When the feature file contains MULTIPLE columns (standard 10x layout:
#' gene_id <tab> gene_symbol <tab> ...), an explicit `feature_column` MUST be
#' supplied — the first column is never assumed to be the identifier used by
#' the Seurat object. Duplicate identifiers stop validation (no make.unique).
#'
#' @param matrix_path Matrix Market file (.mtx or .mtx.gz).
#' @param barcode_path Cell barcode file.
#' @param feature_path Gene/feature file.
#' @param feature_column Integer column index in the feature file that holds
#'   the identifiers matching the Seurat rownames. Required when the feature
#'   file has more than one column; ignored for single-column files.
#' @return Named sparse matrix with genes in rows and cells in columns.
#' @export
read_velocity_mtx <- function(
    matrix_path,
    barcode_path,
    feature_path,
    feature_column = NULL
) {
    if (!file.exists(matrix_path) ||
        !file.exists(barcode_path) ||
        !file.exists(feature_path)) {
        stop(
            "Les trois fichiers sont obligatoires : matrice, barcodes ",
            "et features/genes."
        )
    }

    mat <- Matrix::readMM(
      if (grepl("\\.gz$", matrix_path, ignore.case = TRUE)) {
          gzfile(matrix_path, open = "rt")
      } else {
          matrix_path
      }
    )
    barcodes_raw <- trimws(.read_lines_maybe_gz(barcode_path))
    barcodes <- barcodes_raw[nzchar(barcodes_raw)]
    features_raw <- trimws(.read_lines_maybe_gz(feature_path))
    features_raw <- features_raw[nzchar(features_raw)]

    split_fields <- strsplit(features_raw, "\t", fixed = TRUE)
    n_detected_cols <- unique(vapply(split_fields, length, integer(1)))

    if (length(n_detected_cols) > 1L) {
        # Ragged feature file — refuse rather than guess a per-row column.
        stop(
            "Fichier features/genes irregulier : nombre de colonnes ",
            "variable entre les lignes. Corrigez le fichier ou fournissez ",
            "un TSV tabule homogene."
        )
    }

    n_feature_cols <- if (length(n_detected_cols) == 0L) 1L else n_detected_cols

    feature_column_int <- NULL
    if (!is.null(feature_column) && !is.na(feature_column)) {
        feature_column_int <- suppressWarnings(as.integer(feature_column))
        if (is.na(feature_column_int) || feature_column_int < 1L ||
            feature_column_int > n_feature_cols) {
            stop(sprintf(
                "Colonne d'identifiants invalide : %s (le fichier features ",
                "contient %d colonne(s)).",
                paste(feature_column, collapse = ","), n_feature_cols
            ))
        }
    } else if (n_feature_cols > 1L) {
        stop(sprintf(
            paste0("Le fichier features/genes contient %d colonnes. ",
                   "Indiquez explicitement quelle colonne correspond aux ",
                   "identifiants de l'objet Seurat (champ 'Colonne ",
                   "d'identifiants features'). La premiere colonne n'est ",
                   "jamais supposee correcte."),
            n_feature_cols
        ))
    } else {
        feature_column_int <- 1L
    }

    features <- vapply(
        split_fields,
        function(fields) fields[feature_column_int],
        character(1)
    )

    if (nrow(mat) != length(features) ||
        ncol(mat) != length(barcodes)) {
        stop(
            "Les dimensions de la matrice MTX ne correspondent pas aux ",
            "fichiers features/barcodes."
        )
    }

    rownames(mat) <- features
    colnames(mat) <- barcodes

    validate_velocity_identifiers(mat, "MTX velocity")

    mat
}

#' Plot an RNA velocity phase portrait
#'
#' Descriptive scatter of spliced vs unspliced counts for one gene. ALL cells
#' are plotted, including zero-count cells (no filtering, no padding). No
#' trend line of any kind is drawn: no geom_smooth(), no lm, no loess, no
#' induction/repression curve.
#'
#' @param velocity_result Validated velocity result.
#' @param gene Gene identifier.
#' @param max_cells Maximum cells plotted.
#' @return A ggplot object.
#' @export
plot_velocity_phase_portrait <- function(
    velocity_result,
    gene,
    max_cells = 50000L
) {
    if (!gene %in% rownames(velocity_result$spliced) ||
        !gene %in% rownames(velocity_result$unspliced)) {
        stop(
            "Le gene selectionne est absent des matrices spliced et unspliced : ",
            gene
        )
    }

    spliced_values <- as.numeric(
        velocity_result$spliced[gene, ]
    )

    unspliced_values <- as.numeric(
        velocity_result$unspliced[gene, ]
    )

    plot_data <- data.frame(
        unspliced = unspliced_values,
        spliced = spliced_values,
        cell_barcode = colnames(velocity_result$spliced),
        stringsAsFactors = FALSE
    )

    if (nrow(plot_data) > max_cells) {
        set.seed(1L)
        plot_data <- plot_data[
            sample.int(nrow(plot_data), max_cells),
            ,
            drop = FALSE
        ]
    }

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = unspliced, y = spliced)
    ) +
        ggplot2::geom_point(
            alpha = 0.35,
            size = 0.7,
            colour = "#3B82F6"
        ) +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = "Transcrits unspliced",
            y = "Transcrits spliced",
            title = paste("Phase portrait :", gene),
            subtitle = paste0(
                "Nuage spliced / unspliced — visualisation descriptive, ",
                "pas un modele de transcription."
            )
        )
}

#' Validate precomputed two-dimensional velocity vectors
#'
#' @param vectors Numeric matrix with one row per aligned cell.
#' @param cell_names Expected aligned cell barcodes.
#' @param selected_reduction Selected reduction name.
#' @param stored_reduction Reduction stored with the vectors.
#' @return Validated vector matrix.
#' @export
validate_precomputed_velocity_vectors <- function(
    vectors,
    cell_names,
    selected_reduction,
    stored_reduction = NULL
) {
    if (is.null(vectors)) {
        stop("Aucun vecteur velocity pre-calcule n'est disponible.")
    }

    vectors <- as.matrix(vectors)

    if (!is.numeric(vectors) ||
        ncol(vectors) != 2L) {
        stop(
            "Les vecteurs velocity pre-calcules doivent avoir exactement ",
            "deux colonnes numeriques."
        )
    }

    if (nrow(vectors) != length(cell_names)) {
        stop(
            "Le nombre de vecteurs velocity ne correspond pas au nombre ",
            "de cellules alignees."
        )
    }

    if (is.null(rownames(vectors))) {
        stop(
            "Les vecteurs velocity doivent posseder des noms de lignes ",
            "correspondant aux cellules."
        )
    }

    if (anyDuplicated(rownames(vectors))) {
        stop("Les vecteurs velocity contiennent des noms de lignes dupliques.")
    }

    if (!identical(rownames(vectors), as.character(cell_names))) {
        stop(
            "L'ordre des cellules des vecteurs velocity ne correspond pas ",
            "a l'ordre des cellules Seurat."
        )
    }

    if (!is.null(stored_reduction) &&
        !identical(
            as.character(stored_reduction),
            as.character(selected_reduction)
        )) {
        stop(
            "Les vecteurs velocity ont ete calcules pour la reduction '",
            stored_reduction,
            "' et ne peuvent pas etre affiches dans '",
            selected_reduction,
            "'."
        )
    }

    if (any(!is.finite(vectors))) {
        stop(
            "Les vecteurs velocity contiennent des valeurs NA, NaN ou infinies."
        )
    }

    vectors
}

#' Align a partial velocity embedding to validated velocity cells
#'
#' Partial embeddings are allowed and never padded: cells without stored
#' coordinates are simply reported as missing (n_embedding_missing). The
#' embedding is NOT transposed and is NOT interpreted as velocity vectors.
#'
#' @param embedding Numeric two-column matrix.
#' @param cell_names Validated velocity cell barcodes.
#' @param selected_reduction Selected reduction name.
#' @param stored_reduction Optional reduction stored with embedding.
#' @return A list with aligned embedding and counts.
#' @export
align_velocity_embedding <- function(
    embedding,
    cell_names,
    selected_reduction = "umap",
    stored_reduction = NULL
) {
    embedding <- as.matrix(embedding)

    if (!is.numeric(embedding) ||
        ncol(embedding) != 2L) {
        stop(
            "L'embedding doit avoir exactement deux colonnes numeriques."
        )
    }

    if (is.null(rownames(embedding))) {
        stop("L'embedding doit posseder des noms de lignes.")
    }

    if (anyDuplicated(rownames(embedding))) {
        stop("L'embedding contient des cellules dupliquees.")
    }

    if (!is.null(stored_reduction) &&
        !identical(
            as.character(stored_reduction),
            as.character(selected_reduction)
        )) {
        stop(
            "L'embedding correspond a la reduction '",
            stored_reduction,
            "' et non a '",
            selected_reduction,
            "'."
        )
    }

    cell_names <- as.character(cell_names)
    matched <- intersect(cell_names, rownames(embedding))

    if (!length(matched)) {
        stop(
            "Aucune cellule de l'embedding ne correspond aux cellules velocity."
        )
    }

    ordered_embedding <- embedding[
        match(matched, rownames(embedding)),
        ,
        drop = FALSE
    ]

    ordered_cells <- cell_names[
        match(matched, cell_names)
    ]

    rownames(ordered_embedding) <- ordered_cells

    list(
        embedding = ordered_embedding,
        cell_names = ordered_cells,
        n_velocity_cells = length(cell_names),
        n_embedding_cells = nrow(embedding),
        n_embedding_matched = nrow(ordered_embedding),
        n_embedding_missing = length(cell_names) -
            nrow(ordered_embedding)
    )
}

#' Velocity object fingerprint
#'
#' @param obj Seurat object.
#' @return Character fingerprint.
#' @export
velocity_object_fingerprint <- function(obj) {
    if (is.null(obj)) return(NULL)
    paste(
        ncol(obj),
        paste(head(colnames(obj), 20L), collapse = "|"),
        paste(tail(colnames(obj), 20L), collapse = "|"),
        sep = "::"
    )
}

#' Build velocity provenance export data
#'
#' @param velocity_result Validated velocity result.
#' @param gene Selected gene.
#' @param selected_reduction Selected reduction.
#' @return Data frame for CSV export.
#' @export
build_velocity_provenance_export <- function(
    velocity_result,
    gene,
    selected_reduction = NA_character_
) {
    if (!gene %in% rownames(velocity_result$spliced) ||
        !gene %in% rownames(velocity_result$unspliced)) {
        stop("Gene absent des matrices velocity : ", gene)
    }

    n <- ncol(velocity_result$spliced)

    data.frame(
        cell_barcode = colnames(velocity_result$spliced),
        gene = rep(gene, n),
        spliced = as.numeric(velocity_result$spliced[gene, ]),
        unspliced = as.numeric(velocity_result$unspliced[gene, ]),
        velocity_source = rep(
            velocity_result$velocity_source %||% NA_character_,
            n
        ),
        velocity_method = rep(
            velocity_result$velocity_method %||% "precomputed",
            n
        ),
        matrix_orientation = rep(
            velocity_result$orientation %||% NA_character_,
            n
        ),
        cell_match_mode = rep(
            velocity_result$cell_match_mode %||% NA_character_,
            n
        ),
        gene_match_mode = rep(
            velocity_result$gene_match_mode %||% NA_character_,
            n
        ),
        n_cells_matched = rep(
            velocity_result$n_cells_matched %||% n,
            n
        ),
        n_genes_matched = rep(
            velocity_result$n_genes_matched %||%
                nrow(velocity_result$spliced),
            n
        ),
        raw_cell_overlap = rep(
            velocity_result$raw_cell_overlap %||% NA_real_,
            n
        ),
        normalized_cell_overlap = rep(
            velocity_result$normalized_cell_overlap %||%
                velocity_result$overlap %||% NA_real_,
            n
        ),
        suffix_stripping = rep(
            isTRUE(velocity_result$strip_cell_suffix),
            n
        ),
        version_stripping = rep(
            isTRUE(velocity_result$strip_gene_version),
            n
        ),
        allow_low_overlap = rep(
            isTRUE(velocity_result$allow_low_overlap),
            n
        ),
        selected_reduction = rep(
            selected_reduction %||% NA_character_,
            n
        ),
        embedding_cells_matched = rep(
            velocity_result$embedding_alignment$n_embedding_matched %||%
                NA_integer_,
            n
        ),
        embedding_cells_missing = rep(
            velocity_result$embedding_alignment$n_embedding_missing %||%
                NA_integer_,
            n
        ),
        timestamp = rep(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            n
        ),
        stringsAsFactors = FALSE
    )
}

#' Plot precomputed velocity embedding (coordinates + optional arrows)
#'
#' Coordinates and velocity vectors are deliberately separate inputs:
#' `vectors = NULL` renders the coordinate scatter ONLY (no arrows are ever
#' fabricated from coordinates). When vectors are supplied, arrows are drawn
#' only for cells present in BOTH layers, matched by exact barcode rownames
#' (no silent reordering; partial coverage is allowed and reported).
#'
#' @param embeddings Two-column numeric matrix/data.frame of cell coordinates,
#'   with cell barcodes as rownames.
#' @param vectors Optional validated 2-column dx/dy matrix with cell-barcodes
#'   as rownames (must pass validate_precomputed_velocity_vectors() upstream).
#' @param max_cells Maximum cells displayed.
#' @return ggplot object.
#' @export
plot_velocity_embedding <- function(embeddings, vectors = NULL, max_cells = 5000L) {
    embeddings <- as.matrix(embeddings)
    if (!is.numeric(embeddings) || ncol(embeddings) != 2L) {
        stop("L'embedding d'affichage doit avoir exactement deux colonnes numeriques.")
    }
    if (is.null(rownames(embeddings))) {
        stop("L'embedding d'affichage doit posseder des noms de lignes (barcodes).")
    }

    df <- data.frame(
        x = as.numeric(embeddings[, 1]),
        y = as.numeric(embeddings[, 2]),
        xend = NA_real_,
        yend = NA_real_,
        stringsAsFactors = FALSE
    )
    rownames(df) <- rownames(embeddings)

    if (!is.null(vectors)) {
        vectors <- as.matrix(vectors)
        m <- match(rownames(embeddings), rownames(vectors))
        ok <- !is.na(m)
        df$xend[ok] <- df$x[ok] + as.numeric(vectors[m[ok], 1])
        df$yend[ok] <- df$y[ok] + as.numeric(vectors[m[ok], 2])
    }
    has_arrow <- !is.na(df$xend)

    if (nrow(df) > max_cells) {
        set.seed(1L)
        keep_idx <- sample.int(nrow(df), max_cells)
        df <- df[keep_idx, , drop = FALSE]
        has_arrow <- has_arrow[keep_idx]
    }

    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(size = 0.5, alpha = 0.5, colour = "grey60") +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = "Dimension 1", y = "Dimension 2",
            title = "Vecteurs velocity pre-calcules (UMAP)",
            subtitle = if (any(has_arrow)) {
                sprintf("Fleches = deplacement predit (%d/%d cellules couvertes)",
                        sum(has_arrow), nrow(df))
            } else {
                "Coordonnees uniquement — aucun vecteur velocity pre-calcule fourni."
            }
        )

    if (any(has_arrow)) {
        p <- p + ggplot2::geom_segment(
            data = df[has_arrow, , drop = FALSE],
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "cm")),
            linewidth = 0.25, alpha = 0.6, colour = "#3B82F6"
        )
    }

    p
}
