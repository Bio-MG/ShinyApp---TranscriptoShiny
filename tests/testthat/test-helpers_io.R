# =============================================================================
# test-helpers_io.R — pure-function tests for helpers_io.R
# =============================================================================
# Scope: gene-ID/organism detection, Visium(-HD) directory-mode resolution,
# GEO series_matrix parsing, Slide-seq file detection, delimited-table
# auto-sniffing. All of these are pure (filesystem/text in, data out) and
# need no Bioconductor/Seurat package — only base R + a scratch tempdir().
# remap_gene_ids_to_symbol() / load_spatial_*() are out of scope: they need
# org.Hs.eg.db/org.Mm.eg.db or hdf5r and a real 10x-style dataset on disk.
# =============================================================================

source_project_file("R/core/io_helpers.R")

# ---------------------------------------------------------------------------
# detect_gene_id_type()
# ---------------------------------------------------------------------------
test_that("detect_gene_id_type recognizes Ensembl human IDs", {
  ids <- paste0("ENSG", sprintf("%011d", 1:20))
  expect_identical(detect_gene_id_type(ids), "ensembl")
})

test_that("detect_gene_id_type recognizes Ensembl mouse IDs", {
  ids <- paste0("ENSMUSG", sprintf("%011d", 1:20))
  expect_identical(detect_gene_id_type(ids), "ensembl")
})

test_that("detect_gene_id_type recognizes Entrez IDs", {
  ids <- as.character(1:20)
  expect_identical(detect_gene_id_type(ids), "entrez")
})

test_that("detect_gene_id_type recognizes Affymetrix probe IDs", {
  ids <- c("1007_s_at", "1053_at", "117_at", "121_at", "1255_g_at")
  expect_identical(detect_gene_id_type(ids), "affy_probe")
})

test_that("detect_gene_id_type recognizes gene symbols", {
  ids <- c("TP53", "ACTB", "GAPDH", "MYC", "EGFR")
  expect_identical(detect_gene_id_type(ids), "symbol")
})

test_that("detect_gene_id_type returns 'unknown' for empty/non-gene input", {
  expect_identical(detect_gene_id_type(character(0)), "unknown")
  expect_identical(detect_gene_id_type(c(NA, NA)), "unknown")
})

# ---------------------------------------------------------------------------
# detect_organism_from_ids()
# ---------------------------------------------------------------------------
test_that("detect_organism_from_ids picks human/mouse from Ensembl prefixes", {
  expect_identical(detect_organism_from_ids(paste0("ENSG", sprintf("%011d", 1:10))), "human")
  expect_identical(detect_organism_from_ids(paste0("ENSMUSG", sprintf("%011d", 1:10))), "mouse")
})

test_that("detect_organism_from_ids returns NA when ambiguous (below 50% either way)", {
  ids <- c("TP53", "ACTB", "GAPDH")   # plain symbols, no Ensembl prefix at all
  expect_true(is.na(detect_organism_from_ids(ids)))
})

test_that("detect_organism_from_ids returns NA for empty input", {
  expect_true(is.na(detect_organism_from_ids(character(0))))
})

# ---------------------------------------------------------------------------
# Visium / Visium HD directory-mode resolution
# ---------------------------------------------------------------------------
test_that("get_visium_import_mode returns 'visium' for a plain classic layout", {
  d <- tempfile("visium_classic_"); dir.create(file.path(d, "spatial"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  expect_identical(get_visium_import_mode(d), "visium")
})

test_that("get_visium_import_mode returns 'visium_hd_binned' when binned_outputs/ exists", {
  d <- tempfile("visium_hd_"); dir.create(file.path(d, "binned_outputs"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  expect_identical(get_visium_import_mode(d), "visium_hd_binned")
})

test_that("get_visium_import_mode returns 'visium_hd_flat' for a feature-slice .h5 export", {
  d <- tempfile("visium_flat_"); dir.create(file.path(d, "spatial"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  file.create(file.path(d, "sample_spatial.h5"))
  expect_identical(get_visium_import_mode(d), "visium_hd_flat")
  expect_true(is_visium_hd_flat_dir(d))
  expect_true(is_visium_hd_dir(d))
})

test_that("is_visium_hd_flat_dir is FALSE without a spatial/ folder even with a matching .h5 name", {
  d <- tempfile("visium_no_spatial_"); dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  file.create(file.path(d, "sample_spatial.h5"))
  expect_false(is_visium_hd_flat_dir(d))
})

test_that("list_visium_hd_bin_sizes only reports 8/16um bins that actually contain a .h5 file", {
  d <- tempfile("visium_hd_bins_")
  dir.create(file.path(d, "binned_outputs", "square_008um"), recursive = TRUE)
  dir.create(file.path(d, "binned_outputs", "square_016um"), recursive = TRUE)
  dir.create(file.path(d, "binned_outputs", "square_002um"), recursive = TRUE)  # no .h5 -> excluded
  on.exit(unlink(d, recursive = TRUE))
  file.create(file.path(d, "binned_outputs", "square_008um", "filtered_feature_bc_matrix.h5"))
  # square_016um deliberately left WITHOUT a .h5 -> should be excluded too
  sizes <- list_visium_hd_bin_sizes(d)
  expect_identical(sizes, 8L)
})

test_that("list_visium_hd_bin_sizes returns integer(0) when binned_outputs/ is absent", {
  d <- tempfile("visium_plain_"); dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  expect_identical(list_visium_hd_bin_sizes(d), integer(0))
})

# ---------------------------------------------------------------------------
# infer_metadata_from_names() / preview_metadata_split()
# ---------------------------------------------------------------------------
test_that("infer_metadata_from_names splits consistent sample names and types numeric segments", {
  samples <- c("MW1_cornea_mock_1", "MW1_cornea_treated_2", "MW1_lens_mock_3")
  out <- infer_metadata_from_names(samples, col_names = c("batch", "tissue", "treatment", "rep"))
  expect_identical(rownames(out), samples)
  expect_identical(out$tissue, c("cornea", "cornea", "lens"))
  expect_true(is.integer(out$rep))   # purely-numeric segment auto-converted
})

test_that("infer_metadata_from_names truncates + warns on inconsistent segment counts", {
  samples <- c("A_B_1", "A_B_C_2")   # 3 segments vs 4 segments -> truncated to min = 3
  expect_warning(out <- infer_metadata_from_names(samples), "segments")
  expect_equal(ncol(out), 3)
})

test_that("infer_metadata_from_names errors if col_names length doesn't match segment count", {
  expect_error(
    infer_metadata_from_names(c("A_B_C"), col_names = c("only_one")),
    "col_names doit avoir"
  )
})

test_that("preview_metadata_split flags inconsistent segment counts without erroring", {
  samples <- c("A_B_1", "A_B_C_2", "A_B_3")
  res <- preview_metadata_split(samples)
  expect_false(res$n_seg_consistent)
  expect_length(res$segments, 3)
})

# ---------------------------------------------------------------------------
# parse_geo_series_matrix()
# ---------------------------------------------------------------------------
test_that("parse_geo_series_matrix extracts GSM accessions, title, and characteristics", {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf))
  writeLines(c(
    '!Sample_title\t"WT rep1"\t"KO rep1"',
    '!Sample_geo_accession\t"GSM1"\t"GSM2"',
    '!Sample_characteristics_ch1\t"genotype: WT"\t"genotype: KO"',
    '!Sample_characteristics_ch1\t"age: 8wk"\t"age: 8wk"'
  ), tf)
  meta <- parse_geo_series_matrix(tf)
  expect_identical(rownames(meta), c("GSM1", "GSM2"))
  expect_identical(meta$title, c("WT rep1", "KO rep1"))
  expect_identical(meta$genotype, c("WT", "KO"))
  expect_identical(meta$age, c("8wk", "8wk"))
})

test_that("parse_geo_series_matrix errors on a non-GEO file with an actionable message", {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf))
  writeLines(c("gene\tsample1\tsample2", "TP53\t10\t20"), tf)
  expect_error(parse_geo_series_matrix(tf), "series_matrix")
})

test_that("parse_geo_series_matrix de-duplicates repeated characteristic keys", {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf))
  writeLines(c(
    '!Sample_geo_accession\t"GSM1"\t"GSM2"',
    '!Sample_characteristics_ch1\t"tissue: liver"\t"tissue: liver"',
    '!Sample_characteristics_ch2\t"tissue: repeat1"\t"tissue: repeat2"'
  ), tf)
  meta <- parse_geo_series_matrix(tf)
  expect_true(all(c("tissue", "tissue.1") %in% colnames(meta)))
})

# ---------------------------------------------------------------------------
# .read_delimited_table() — separator + header auto-detection
# ---------------------------------------------------------------------------
test_that(".read_delimited_table detects a comma-separated file with a header", {
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf))
  writeLines(c("barcode,x,y", "AAAC,1.5,2.5", "AAAG,3.5,4.5"), tf)
  df <- .read_delimited_table(tf)
  expect_identical(colnames(df), c("barcode", "x", "y"))
  expect_equal(nrow(df), 2)
})

test_that(".read_delimited_table detects a HEADERLESS tab-separated file (Slide-seq style)", {
  tf <- tempfile(fileext = ".tsv")
  on.exit(unlink(tf))
  # 2nd/3rd fields look numeric on the first line -> treated as headerless
  writeLines(c("AAAC\t1.5\t2.5", "AAAG\t3.5\t4.5"), tf)
  df <- .read_delimited_table(tf)
  expect_identical(colnames(df), c("V1", "V2", "V3"))
  expect_equal(nrow(df), 2)
})

test_that(".read_delimited_table falls back to the other separator if the guessed one is absent", {
  # .txt defaults to tab-guess, but content is actually comma-separated
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf))
  writeLines(c("barcode,x,y", "AAAC,1,2"), tf)
  df <- .read_delimited_table(tf)
  expect_equal(ncol(df), 3)
})

# ---------------------------------------------------------------------------
# Slide-seq file detection: .find_slideseq_location_file() / .find_slideseq_counts()
# ---------------------------------------------------------------------------
test_that(".find_slideseq_location_file finds a prefixed alignedXYCoords file (not anchored)", {
  files <- c("/d/Puck_Num_01_alignedXYCoords.tsv", "/d/Puck_Num_01_expression_matrix.mtx")
  hit <- .find_slideseq_location_file(files)
  expect_identical(hit, "/d/Puck_Num_01_alignedXYCoords.tsv")
})

test_that(".find_slideseq_location_file returns NA when nothing matches", {
  files <- c("/d/random_file.txt", "/d/other.csv")
  expect_true(is.na(.find_slideseq_location_file(files)))
})

test_that(".find_slideseq_counts prefers a prefixed 10x-style triplet in the SAME directory", {
  files <- c(
    "/d/Puck_01_matrix.mtx", "/d/Puck_01_barcodes.tsv", "/d/Puck_01_genes.tsv"
  )
  res <- .find_slideseq_counts(files)
  expect_identical(res$kind, "mtx")
  expect_identical(res$mtx, "/d/Puck_01_matrix.mtx")
  expect_identical(res$barcodes, "/d/Puck_01_barcodes.tsv")
  expect_identical(res$features, "/d/Puck_01_genes.tsv")
})

test_that(".find_slideseq_counts prefers the SHALLOWEST directory when a triplet is duplicated in a nested dir", {
  files <- c(
    "/d/matrix.mtx", "/d/barcodes.tsv", "/d/genes.tsv",
    "/d/compress/matrix.mtx", "/d/compress/barcodes.tsv", "/d/compress/genes.tsv"
  )
  res <- .find_slideseq_counts(files)
  expect_identical(dirname(res$mtx), "/d")
})

test_that(".find_slideseq_counts falls back to a dense DGE table when no mtx triplet exists", {
  files <- c("/d/MappedDGEForR.csv")
  res <- .find_slideseq_counts(files)
  expect_identical(res$kind, "dge")
  expect_identical(res$path, "/d/MappedDGEForR.csv")
})

test_that(".find_slideseq_counts errors clearly when a *matrix.mtx exists but siblings are missing", {
  files <- c("/d/matrix.mtx")   # no barcodes.tsv/genes.tsv anywhere
  expect_error(.find_slideseq_counts(files), "barcodes.tsv")
})

test_that(".find_slideseq_counts errors clearly when nothing usable is found at all", {
  files <- c("/d/readme.txt")
  expect_error(.find_slideseq_counts(files), "Aucune matrice de comptage")
})

test_that("is_slideseq_dir is TRUE only when BOTH counts and locations are present", {
  d <- tempfile("slideseq_"); dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  file.create(file.path(d, "matrix.mtx"), file.path(d, "barcodes.tsv"), file.path(d, "genes.tsv"))
  expect_false(is_slideseq_dir(d))   # counts present, but no location file yet
  file.create(file.path(d, "BeadLocationsForR.csv"))
  expect_true(is_slideseq_dir(d))
})

# ---------------------------------------------------------------------------
# .detect_slideseq_feature_column()
# ---------------------------------------------------------------------------
test_that(".detect_slideseq_feature_column returns 2 for a standard 2-column features file", {
  tf <- tempfile(fileext = ".tsv")
  on.exit(unlink(tf))
  writeLines(c("ENSG001\tTP53", "ENSG002\tACTB"), tf)
  expect_equal(.detect_slideseq_feature_column(tf), 2L)
})

test_that(".detect_slideseq_feature_column returns 1 for a single gene-symbol column", {
  tf <- tempfile(fileext = ".tsv")
  on.exit(unlink(tf))
  writeLines(c("TP53", "ACTB"), tf)
  expect_equal(.detect_slideseq_feature_column(tf), 1L)
})

test_that(".detect_slideseq_feature_column errors on a missing file", {
  expect_error(.detect_slideseq_feature_column(tempfile()), "introuvable")
})
