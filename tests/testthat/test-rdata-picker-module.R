# =============================================================================
# test-rdata-picker-module.R — Composant mutualisé mod_rdata_picker
# =============================================================================
# Pattern : shiny::testServer sur le module serveur (fixtures .rda dans
# tempdir, commit_fn enregistreur). Le flux SC hôte est testé en intégration
# (mod_import_sc_server) pour le chemin auto-import objet unique.
# =============================================================================

source_project_file("R/core/io_helpers.R")   # %||%
suppressPackageStartupMessages({
  library(shiny)       # testServer, reactiveVal
  library(bslib)       # layout_columns
  library(DT)
  library(bsicons)
  library(shinyFiles)  # getVolumes (mod_import_sc)
  library(Seurat)      # prepare_seurat_object (attaché par global.R en prod)
})
source_project_file("R/core/rdata_io.R")
source_project_file("modules/import/mod_rdata_picker.R")

.tmpdir <- tempfile(pattern = "rdata_picker_")
dir.create(.tmpdir, showWarnings = FALSE)

mat <- matrix(1:12, 3, 4)
single_path <- file.path(.tmpdir, "single_mat.rda")
save(mat, file = single_path)

grob <- grid::rectGrob()
single_bad_path <- file.path(.tmpdir, "single_grob.rda")
save(grob, file = single_bad_path)

meta_df <- data.frame(a = 1:3)
other <- "texte"
multi_path <- file.path(.tmpdir, "multi.rda")
save(mat, meta_df, other, file = multi_path)

# Enregistreur de commits (l'hôte doit stop() en cas d'échec de commit)
.new_recorder <- function() {
  env <- new.env(parent = emptyenv())
  env$commits <- list()
  env$commit_fn <- function(obj, obj_name) {
    env$commits[[length(env$commits) + 1L]] <<- list(obj = obj, name = obj_name)
  }
  env
}

# ── Composant seul ───────────────────────────────────────────────────────────
test_that("picker: objet unique compatible -> commit automatique", {
  rec <- .new_recorder()
  logs <- character(0)
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix"),
    log_fn = function(m) logs <<- c(logs, m)
  ), expr = {
    file_rv(list(datapath = single_path, name = "single_mat.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 1L)
    expect_identical(rec$commits[[1L]]$name, "mat")
    expect_identical(dim(rec$commits[[1L]]$obj), c(3L, 4L))
  })
  expect_true(any(grepl("mat", logs)))
})

test_that("picker: objet unique incompatible -> preview forcée, pas de commit", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("Seurat", "matrix"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = single_bad_path, name = "single_grob.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 0L)
    # la carte de preview est rendue malgré l'objet unique
    card <- session$output$picker_card
    expect_true(is.character(card) || is.list(card))
  })
})

test_that("picker: multi-objets -> preview + import sur sélection exacte", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix", "data.frame"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = multi_path, name = "multi.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 0L)

    # Import sans sélection -> refus (notification), pas de commit
    session$setInputs(import_btn = 1L)
    expect_length(rec$commits, 0L)

    # Sélection d'une ligne (la table multi : mat=1, meta_df=2, other=3)
    session$setInputs(preview_table_rows_selected = 1L)
    session$setInputs(import_btn = 2L)
    expect_length(rec$commits, 1L)
    expect_identical(rec$commits[[1L]]$name, "mat")

    # Sélection du data.frame : classe acceptée -> commit
    session$setInputs(preview_table_rows_selected = 2L)
    session$setInputs(import_btn = 3L)
    expect_length(rec$commits, 2L)
    expect_identical(rec$commits[[2L]]$name, "meta_df")
  })
})

test_that("picker: changement de fichier libère l'ancien env (rdata_free)", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)
  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = single_path, name = "a.rda", size = 1))
    session$flushReact()
    expect_length(rec$commits, 1L)
    file_rv(NULL)
    session$flushReact()
    file_rv(list(datapath = multi_path, name = "b.rda", size = 1))
    session$flushReact()
    # pas de commit auto en multi ; les commits précédents ne se dupliquent pas
    expect_length(rec$commits, 1L)
  })
})

# ── Intégration hôte SC (mod_import_sc_server) ───────────────────────────────
# L'auto-import d'un .rda à objet unique traverse le composant puis
# prepare_seurat_object -> global_data$sc_obj (précédent commit des imports).
source_project_file("modules/import/mod_import_sc.R")

test_that("hôte SC: .rda objet unique (matrice) -> global_data$sc_obj Seurat", {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL

  testServer(mod_import_sc_server, args = list(global_data = gd), expr = {
    session$setInputs(single_file_upload = list(
      name = "single_mat.rda", size = as.integer(file.info(single_path)$size),
      datapath = single_path
    ))
    session$flushReact()
    expect_false(is.null(gd$sc_obj))
    expect_s4_class(gd$sc_obj, "Seurat")
    expect_equal(ncol(gd$sc_obj), 4L)
    expect_equal(nrow(gd$sc_obj), 3L)
  })
})

test_that("hôte SC: .rda multi-objets en Option C -> preview, pas de commit direct", {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL

  testServer(mod_import_sc_server, args = list(global_data = gd), expr = {
    session$setInputs(single_file_upload = list(
      name = "multi.rda", size = as.integer(file.info(multi_path)$size),
      datapath = multi_path
    ))
    session$flushReact()
    expect_null(gd$sc_obj)
  })
})

unlink(.tmpdir, recursive = TRUE)
