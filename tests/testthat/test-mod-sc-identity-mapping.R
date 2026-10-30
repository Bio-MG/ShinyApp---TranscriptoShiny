# =============================================================================
# test-mod-sc-identity-mapping.R — modules/sc/mod_sc_communication.R (3E)
# =============================================================================
# Item 3E du packet round-7 (O28 batch B5) : surface de validation propose/
# valide du sous-onglet « Harmonisation identites » (spéc §2.17 de
# docs/proposals/V1X_SC_QA_AUDIT_FIXES_PROPOSAL.md).
#
# DEUX niveaux de test, cibles uniquement :
#   1. Verrous structurels (pattern B3 test-mod-sc-communication-ui.R) :
#      la surface existe, appelle le moteur PUR, applique la garde import_only,
#      et les textes historiques du module sont préservés verbatim.
#   2. testServer de bout en bout (pattern B1 : session$setInputs émule le
#      navigateur ; les calls UI non implémentés par MockShinySession —
#      showNotification/showModal/removeModal — sont shimmés dans globalenv,
#      posés PUIS restaurés). Chemin complet : import CellPhoneDB (fixture
#      temporaire) → proposition → pré-remplissage → application confirmée.
#
# ⚠️ shiny 1.14 : sous testServer, les update* ne déplacent PAS input$<id> —
# ici aucun assert ne repose sur un input déplacé par update* (B1/B3).
# ⚠️ Limite documentée (B3) : le rendu DT/JS n'est pas exécutable headless —
# les assertions portent sur l'ETAT (shared_rv$communication_result, passé par
# référence) et sur les notifications capturées, jamais sur le DOM.
# =============================================================================

Sys.setenv(OMNIPATHR_LOGFILE = "none")

source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/error_state.R")
source_project_file("tools/pkg_skeleton/R/datatable.R")
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")
source_project_file("tools/pkg_skeleton/R/sc_communication.R")
source_project_file("tools/pkg_skeleton/R/sc_communication_input.R")
source_project_file("tools/pkg_skeleton/R/sc_communication_views.R")
source_project_file("modules/sc/mod_sc_communication.R")

suppressPackageStartupMessages(library(shiny))

# ── Harnais : les 4 sous-modules contextuels sont HORS PÉRIMÈTRE 3E ─────────
# spatial/trajectory/velocity/perturbation : leurs serveurs enregistrent des
# observers eager qui lisent des inputs jamais rendus sous MockShinySession
# (ex. mod_sc_communication_perturbation.R:86 `switch(input$pert_target, ...)`
# sur NULL → « EXPR must be a length 1 vector ») et cassent le flush de
# testServer — c'est précisément la limite documentée par B3
# (test-mod-sc-communication-ui.R). La surface 3E n'en dépend pas : no-op.
# (assign en globalenv : les closures du module y résolvent leurs symboles.)
assign("mod_sc_communication_spatial_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_trajectory_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_velocity_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_perturbation_server",
       function(...) invisible(NULL), envir = globalenv())

.MSCC_FILE <- "modules/sc/mod_sc_communication.R"

# ── Shim des calls UI non implémentés par MockShinySession ─────────────────
# showNotification/showModal/removeModal sont appelés NUS par le module — la
# résolution part de globalenv : le shim capte et avale, puis est restauré.
# ⚠️ BINDING : le box est créé AVANT l'exécution du code shimmé et passé en
# paramètre — un pattern `box <- .with_shims(testServer(...{box$...}))`
# évaluerait le bloc AVANT l'assignation de `box` (résolution vers une
# closure étrangère : « object of type 'closure' is not subsettable »).
.b5_install_shims <- function() {
  box <- new.env(parent = emptyenv())
  box$notifs <- list()
  box$modals <- 0L
  olds <- lapply(c("showNotification", "showModal", "removeModal"), get0,
                 envir = globalenv())
  assign("showNotification", function(ui, type = "default", duration = 5,
                                      session = getDefaultReactiveDomain(), ...) {
    box$notifs <- c(box$notifs, list(list(ui = as.character(ui), type = type)))
    invisible(NULL)
  }, envir = globalenv())
  assign("showModal", function(ui, session = getDefaultReactiveDomain(), ...) {
    box$modals <- box$modals + 1L
    invisible(NULL)
  }, envir = globalenv())
  assign("removeModal", function(session = getDefaultReactiveDomain(), ...) {
    invisible(NULL)
  }, envir = globalenv())
  list(box = box, olds = olds,
       restore = function() {
         invisible(mapply(function(nm, old) assign(nm, old, envir = globalenv()),
                          c("showNotification", "showModal", "removeModal"),
                          olds, USE.NAMES = FALSE))
       })
}

# ── Fixtures : objet Seurat + tables CellPhoneDB (labels divergents) ────────
# Objet : identites "CD4 T cells" / "B" / "NK cells".
# Table : labels "CD4_T_Cells" (casse+ponctuation — proposition attendue),
# "B" et "NK cells" (exact). UNE seule suggestion, JAMAIS appliquée d'office.
.b5_obj <- function() {
  cells <- paste0("C", 1:6)
  ids <- c("CD4 T cells", "CD4 T cells", "B", "B", "NK cells", "NK cells")
  counts <- Matrix::Matrix(
    matrix(seq_len(10 * 6) %% 5L, nrow = 10L,
           dimnames = list(paste0("G", 1:10), cells)),
    sparse = TRUE
  )
  SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(cell_type = ids, row.names = cells)
  )
}

.b5_means <- function() {
  data.frame(
    id_cp_interaction = c("CII-1", "CII-2", "CII-3"),
    interacting_pair = c("IL7|IL7R", "CD40|CD40", "CCL5|CCR5"),
    partner_a = c("IL7", "CD40", "CCL5"),
    partner_b = c("IL7R", "CD40", "CCR5"),
    gene_a = c("IL7", "CD40", "CCL5"),
    gene_b = c("IL7R", "CD40", "CCR5"),
    receptor_a = c(FALSE, FALSE, FALSE),
    receptor_b = c(TRUE, TRUE, TRUE),
    "CD4_T_Cells|B" = c(0.3, 0.0, 0.1),
    "B|NK cells" = c(0.1, 0.5, 0.0),
    "NK cells|NK cells" = c(0.0, 0.0, 0.2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.b5_write_fixture <- function() {
  dir <- tempfile("b5-cpdb-")
  dir.create(dir, recursive = TRUE)
  means_path <- file.path(dir, "means.txt")
  write.table(.b5_means(), means_path, sep = "\t", quote = FALSE,
              row.names = FALSE)
  list(means = means_path)
}

# ═══ Niveau 1 — verrous structurels (pattern B3) ════════════════════════════

test_that("3E : la surface propose/valide existe dans le sous-onglet Harmonisation", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Les deux boutons de la surface, dans le nav_panel existant :
  expect_match(src, "comm_propose_identities", fixed = TRUE)
  expect_match(src, "comm_apply_identities", fixed = TRUE)
  # DT éditable restreint à la colonne à valider :
  expect_match(src, "output$comm_proposals <- DT::renderDataTable", fixed = TRUE)
  expect_match(src, 'which(colnames(p) == "identity_validee") - 1L', fixed = TRUE)
  # Confirmation explicite (modal) avant application :
  expect_match(src, "comm_apply_confirm", fixed = TRUE)
  # Le moteur PUR est appelé (proposition + application) :
  expect_match(src, "propose_communication_identity_mapping(", fixed = TRUE)
  expect_match(src, "apply_communication_identity_mapping(", fixed = TRUE)
})

test_that("3E : garde import_only — la surface est refusée sur la voie calculée", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "if (!isTRUE(r$provenance$import_only))", fixed = TRUE)
  expect_match(src,
               "Mapping interactif disponible pour les resultats importes uniquement.",
               fixed = TRUE)
})

test_that("3E : signal visible quand l'exact match laisse des labels non mappés", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "if (harm$summary$n_labels_unmatched > 0L)", fixed = TRUE)
  expect_match(src,
               "propositions de correspondance dans le sous-onglet « Harmonisation identites ».",
               fixed = TRUE)
  # La notification est de type warning (visible, distincte du succès) :
  expect_match(src, 'type = "warning", duration = 8', fixed = TRUE)
})

test_that("3E : aucun effet sans validation — pré-remplissage limité aux propositions", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Seules les suggestions sont pré-copiées ; exact/ambigu/unmatched restent vides :
  expect_match(src, 'p$identity_validee <- ifelse(p$status == "proposed",',
               fixed = TRUE)
  # Les propositions en attente ne survivent ni à un reset ni à un dépôt :
  expect_match(src, "prop_rv(NULL)", fixed = TRUE)
  # Le chemin historique (exact match) est inchangé dans le moteur :
  eng <- paste(readLines(file.path(
    ts_project_root(), "tools/pkg_skeleton/R/sc_communication.R"),
    warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(eng, "Défaut NULL : comportement HISTORIQUE strict", fixed = TRUE)
})

test_that("3E : les textes historiques du module sont préservés verbatim", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCC_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "Harmonisation par exact match uniquement : aucun renommage de populations, labels sans correspondance listes integralement.",
               fixed = TRUE)
  expect_match(src, "Harmonisation identites", fixed = TRUE)
  # Coordination 3D : la bannière Limites reste hors de portée de B5 (une
  # seule carte synthétique, cf. test-mod-sc-communication-ui.R).
})

# ═══ Niveau 2 — testServer de bout en bout ═════════════════════════════════

test_that("3E surface : import → proposition pré-remplie → application confirmée", {
  obj <- .b5_obj()
  fx <- .b5_write_fixture()
  global_data <- shiny::reactiveValues(sc_obj = obj, i18n = NULL, language = "fr")
  shared_rv <- shiny::reactiveValues(communication_result = NULL)

  shims <- .b5_install_shims()
  on.exit(shims$restore(), add = TRUE)
  box <- shims$box

  run <- function(box) testServer(mod_sc_communication_server,
    args = list(id = "communication", global_data = global_data,
                shared_rv = shared_rv),
    {
      session$setInputs(comm_source = "cellphonedb",
                        comm_identity_column = "cell_type")
      session$setInputs(comm_cpdb_means = list(
        name = "means.txt", datapath = fx$means, size = 100L,
        type = "text/plain"))
      # Diagnostic d'harnais : le fileInput simulé doit round-tripper.
      expect_identical(input$comm_cpdb_means$datapath, fx$means)
      session$setInputs(comm_import = 1L)

      # 1. Import : exact match historique — 1 label sans correspondance.
      res0 <- shared_rv$communication_result
      expect_true(is.list(res0))
      expect_identical(res0$identity_summary$n_labels_unmatched, 1L)
      expect_identical(res0$identity_summary$n_labels_matched, 2L)
      m0 <- res0$identity_mapping
      expect_false(m0$matched[m0$label == "CD4_T_Cells"])
      # 3 colonnes means × 3 paires = 9 lignes canoniques ; le label divergent
      # est sender sur 3 lignes, toutes a NA (exact match historique).
      expect_identical(nrow(res0$canonical_table), 9L)
      expect_true(all(is.na(res0$canonical_table$sender_mapped[
        res0$canonical_table$sender == "CD4_T_Cells"])))

      # 2. Signal visible (constat 3E) : notification warning distincte.
      types <- vapply(box$notifs, `[[`, character(1), "type")
      expect_true("warning" %in% types,
                  info = paste(vapply(box$notifs, `[[`, character(1), "ui"),
                               collapse = " | "))

      # 3. Proposition : calcul PUR, pré-rempli pour la suggestion seulement.
      session$setInputs(comm_propose_identities = 1L)

      # 4. Application CONFIRMÉE (modal ouverte puis confirmée via setInputs —
      #    le bouton de la modal n'existe pas dans le mock, l'observateur si).
      session$setInputs(comm_apply_identities = 1L)
      expect_identical(box$modals, 1L)
      session$setInputs(comm_apply_confirm = 1L)

      res1 <- shared_rv$communication_result
      expect_identical(res1$identity_summary$n_user_mapped, 1L)
      m1 <- res1$identity_mapping
      expect_identical(m1$source[m1$label == "CD4_T_Cells"], "user_approved")
      expect_identical(m1$rule[m1$label == "CD4_T_Cells"], "user_validated")
      expect_identical(m1$matched_identity[m1$label == "CD4_T_Cells"],
                       "CD4 T cells")
      tab1 <- res1$canonical_table
      expect_true(all(tab1$sender_mapped[tab1$sender == "CD4_T_Cells"] ==
                        "CD4 T cells"))
      # Les roles exacts restent intacts (aucune surcharge) :
      expect_true(all(tab1$receiver_mapped[tab1$receiver == "B"] == "B"))
      expect_true(all(tab1$receiver_mapped[tab1$receiver == "NK cells"] ==
                        "NK cells"))
      expect_true(all(tab1$sender_mapped[tab1$sender == "NK cells"] ==
                        "NK cells"))
      # Les paires exactes restent exactes (aucune surcharge) :
      expect_identical(m1$source[m1$label == "B"], "exact")
      expect_identical(m1$source[m1$label == "NK cells"], "exact")
      # Trace visible de l'application :
      expect_true(any(grepl("Mapping d'identites applique",
                            vapply(box$notifs, `[[`, character(1), "ui"),
                            fixed = TRUE)))
    }
  )
  run(box)
})

test_that("3E surface : rien n'est appliqué sans validation explicite", {
  obj <- .b5_obj()
  fx <- .b5_write_fixture()
  global_data <- shiny::reactiveValues(sc_obj = obj, i18n = NULL, language = "fr")
  shared_rv <- shiny::reactiveValues(communication_result = NULL)

  shims <- .b5_install_shims()
  on.exit(shims$restore(), add = TRUE)
  box <- shims$box

  run <- function(box) testServer(mod_sc_communication_server,
    args = list(id = "communication", global_data = global_data,
                shared_rv = shared_rv),
    {
      session$setInputs(comm_source = "cellphonedb",
                        comm_identity_column = "cell_type")
      session$setInputs(comm_cpdb_means = list(
        name = "means.txt", datapath = fx$means, size = 100L,
        type = "text/plain"))
      session$setInputs(comm_import = 1L)
      res0 <- shared_rv$communication_result
      expect_true(is.list(res0))

      # Application SANS avoir ouvert/validé la moindre paire : la modal
      # n'est PAS ouverte, un warning explicite est émis (garde 3C — jamais de
      # req() silencieux) et le résultat déposé reste strictement l'exact
      # match historique.
      session$setInputs(comm_apply_identities = 1L)
      expect_identical(box$modals, 0L)
      uis <- vapply(box$notifs, `[[`, character(1), "ui")
      expect_true(any(grepl("Aucune proposition en attente", uis, fixed = TRUE)))
      res1 <- shared_rv$communication_result
      expect_identical(res1$identity_mapping, res0$identity_mapping)
      expect_identical(res1$identity_summary$n_user_mapped, 0L)
      expect_null(res1$identity_summary$user_mapping)
    }
  )
  run(box)
})
