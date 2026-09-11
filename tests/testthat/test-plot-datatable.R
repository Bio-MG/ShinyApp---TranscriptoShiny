# =============================================================================
# test-plot-datatable.R — PLOT-S3 : wrapper DT harmonise ts_datatable()
# =============================================================================
# Objectif central : ZERO CHANGEMENT DE COMPORTEMENT.
#   - buttons = FALSE par defaut (48 des 49 tables de l'app n'en ont aucun)
#     => le helper NE pose NI `extensions` NI `dom`
#   - page_length est OBLIGATOIRE : il n'existe aucune valeur neutre
#     (10 x22, 15 x15, 8 x5, 20 x3, 6 x1)
# =============================================================================
suppressWarnings(suppressPackageStartupMessages(library(DT)))

source_project_file("R/plotting/datatable.R")

.tsd_df <- function() {
  data.frame(gene = c("A", "B", "C"), padj = c(0.0001, 0.02, 0.4),
             stringsAsFactors = FALSE)
}

# --- 1. Contrat du resolveur -------------------------------------------------

test_that("surface publique figee", {
  expect_setequal(
    ts_datatable_public_api(),
    c("ts_datatable", "ts_datatable_buttons",
      "ts_datatable_page_lengths", "ts_datatable_public_api")
  )
  expect_false(anyDuplicated(ts_datatable_public_api()) > 0)
})

test_that("page_length est OBLIGATOIRE — aucune valeur par defaut n'est neutre", {
  df <- .tsd_df()
  e <- tryCatch(ts_datatable(df), error = function(e) e)
  expect_s3_class(e, "plot_datatable_error")
  expect_identical(e$state, "invalid_page_length")
  expect_match(conditionMessage(e), "obligatoire")
  expect_match(conditionMessage(e), "10, 15, 8, 20 et 6")
  # il ne doit PAS y avoir de valeur par defaut dans la signature
  expect_true(is.name(formals(ts_datatable)$page_length) ||
              is.symbol(formals(ts_datatable)$page_length))
})

test_that("page_length invalide => invalid_page_length", {
  df <- .tsd_df()
  for (bad in list(NULL, NA, 0, -5, "15", c(10, 15))) {
    e <- tryCatch(ts_datatable(df, page_length = bad), error = function(e) e)
    expect_s3_class(e, "plot_datatable_error")
    expect_identical(e$state, "invalid_page_length")
  }
})

test_that("les tailles de page reelles de l'app sont toutes connues", {
  expect_setequal(ts_datatable_page_lengths(), c(6L, 8L, 10L, 15L, 20L))
})

test_that("donnees invalides => invalid_data ; matrix/liste sont coercés", {
  e <- tryCatch(ts_datatable("pas une table", page_length = 10),
                error = function(e) e)
  expect_s3_class(e, "plot_datatable_error")
  expect_identical(e$state, "invalid_data")

  m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
  expect_s3_class(ts_datatable(m, page_length = 10), "datatables")
  expect_s3_class(ts_datatable(list(a = 1:3), page_length = 10), "datatables")
})

test_that("buttons non logique => invalid_buttons", {
  df <- .tsd_df()
  for (bad in list("oui", NA, c(TRUE, FALSE))) {
    e <- tryCatch(ts_datatable(df, page_length = 10, buttons = bad),
                  error = function(e) e)
    expect_s3_class(e, "plot_datatable_error")
    expect_identical(e$state, "invalid_buttons")
  }
})

# --- 2. GARANTIE zero changement de comportement ------------------------------

test_that("appel nu = strictement identique au DT::datatable() d'origine", {
  df <- .tsd_df()
  ref <- DT::datatable(df, filter = "top", rownames = FALSE,
                       options = list(pageLength = 15, scrollX = TRUE))
  got <- ts_datatable(df, page_length = 15)

  # memes options, et RIEN de plus
  expect_identical(got$x$options$pageLength, ref$x$options$pageLength)
  expect_identical(got$x$options$scrollX,    ref$x$options$scrollX)
  expect_null(got$x$options$dom)
  expect_null(got$x$options$buttons)
  expect_identical(got$x$options, ref$x$options)
  # aucune extension
  expect_true(is.null(got$x$extensions) || length(got$x$extensions) == 0L)
  expect_identical(got$x$filter, "top")
  expect_false(isTRUE(got$x$rownames))
})

test_that("buttons = FALSE ne pose NI extensions NI dom (48 sites sur 49)", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 8)
  expect_null(got$x$options$dom)
  expect_true(is.null(got$x$extensions) || length(got$x$extensions) == 0L)
})

test_that("buttons = TRUE reproduit exactement le site pathways (dom + Buttons)", {
  df <- .tsd_df()
  ref <- DT::datatable(df, filter = "top", rownames = FALSE,
                       options = list(pageLength = 10, scrollX = TRUE,
                                      dom = "Bfrtip"),
                       extensions = "Buttons")
  got <- ts_datatable(df, page_length = 10, buttons = TRUE)
  expect_identical(got$x$options$dom, "Bfrtip")
  expect_identical(got$x$options$pageLength, 10)
  expect_identical(got$x$options$scrollX, TRUE)
  expect_true("Buttons" %in% unlist(got$x$extensions))
  # aucun jeu de boutons nomme : DT applique ses boutons par defaut
  expect_null(got$x$options$buttons)
  expect_identical(got$x$options$dom, ref$x$options$dom)
})

test_that("filename_base ajoute les 4 boutons nommes (opt-in, jamais par defaut)", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 10, buttons = TRUE,
                      filename_base = "pathways")
  expect_identical(got$x$options$dom, "Bfrtip")
  btns <- got$x$options$buttons
  expect_type(btns, "list")
  flat <- unlist(btns)
  expect_true("copy"  %in% flat)
  expect_true("print" %in% flat)
  expect_true("csv"   %in% flat)
  expect_true("excel" %in% flat)
  expect_true("pathways" %in% flat)
})

test_that("dom / extensions explicites ont la priorite sur buttons", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 10, buttons = TRUE, dom = "tip")
  expect_identical(got$x$options$dom, "tip")
})

test_that("les parametres de forme sont respectes et les ... transmis", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 20, filter = "none",
                      rownames = TRUE, scroll_x = FALSE,
                      selection = list(mode = "none"))
  expect_identical(got$x$options$pageLength, 20)
  expect_identical(got$x$options$scrollX, FALSE)
  expect_identical(got$x$filter, "none")
  # pas d'erreur => les `...` (selection, etc.) ont bien ete acceptes
  expect_s3_class(got, "datatables")
})

test_that("ts_datatable_buttons() est deterministe", {
  b <- ts_datatable_buttons("x")
  expect_identical(b, ts_datatable_buttons("x"))
  expect_identical(b[[1]], "copy")
  expect_identical(b[[3]]$filename, "x")
  expect_identical(b[[4]]$extend, "excel")
})

# --- 3. Anti-regression -------------------------------------------------------

.tsd_migrated <- c(
  "R/bulk/bulk_helpers.R",
  "R/core/pathway_helpers.R",
  "R/sc/sc_helpers.R",
  "modules/bulk_de/mod_bulk_de_multimethod.R",
  "modules/bulk_de/mod_bulk_de_venn.R",
  "modules/spatial/mod_spatial_cluster.R"
)

test_that("PLOT-S3 : les 6 sites canoniques passent par ts_datatable()", {
  for (rel in .tsd_migrated) {
    path <- file.path(ts_project_root(), rel)
    code <- readLines(path, warn = FALSE, encoding = "UTF-8")
    code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
    src <- paste(code, collapse = "\n")
    # On cible precisement la forme canonique migree, pas n'importe quelle
    # table du fichier (certains fichiers contiennent d'autres tables, de forme
    # differente, volontairement laissees hors du perimetre PLOT-S3).
    pat <- paste0(
      "(?:DT::)?datatable\\s*\\(\\s*[A-Za-z0-9_.$]+\\s*,\\s*",
      "filter\\s*=\\s*.top.\\s*,\\s*rownames\\s*=\\s*FALSE\\s*,\\s*",
      "options\\s*=\\s*list\\(\\s*pageLength\\s*=\\s*[0-9]+\\s*,\\s*",
      "scrollX\\s*=\\s*TRUE")
    hits <- grepl(pat, src, perl = TRUE)
    expect_false(hits,
      paste0(rel, " : une table de forme canonique appelle encore datatable() ",
             "directement au lieu de ts_datatable()"))
  }
})

test_that("les valeurs de pageLength historiques sont preservees", {
  # 15 / 10 / 15 / 15 / 15 / 20 — aucune normalisation
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "R/bulk/bulk_helpers.R")), collapse = "\n"),
               "ts_datatable\\(df_display, page_length = 15\\)")
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "R/core/pathway_helpers.R")), collapse = "\n"),
               "ts_datatable\\(df_display, page_length = 10, buttons = TRUE\\)")
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "modules/spatial/mod_spatial_cluster.R")), collapse = "\n"),
               "ts_datatable\\(shared_rv\\$cluster_markers, page_length = 20\\)")
})
