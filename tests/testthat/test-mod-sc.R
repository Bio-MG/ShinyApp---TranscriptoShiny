# =============================================================================
# test-mod-sc.R — tests for modules/sc/mod_sc.R
# =============================================================================
# 29ᵉ incrément de la dette de conventions (2026-09-18, §2co).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur les assertions de classe tant que les deux `stop()` ne
# portent pas `class = "mod_sc_error"`.
#
# 🔴 **CLASSE `mod_sc_error`, ET NON `sc_error` — décision doctrinale mesurée.**
# Le mapping mécanique de `modules/` (retirer le préfixe `mod_`, garder le
# domaine : `mod_sc_pseudobulk.R` → `sc_pseudobulk_error`) donnerait ici
# **`sc_error`** — soit EXACTEMENT le nom que `CONVENTIONS.md` §7 (:334-340)
# **interdit** : « **`sc_helpers_error`** — et **non** `sc_error`, trop large :
# il prétendrait désigner « l'erreur single-cell » générique alors que
# `sc_multi_error`, `sccoda_error`, `milo_error` et `velocity_validation_error`
# coexistent ». Le dépôt a donc **deux** branches dans §7 : (a) le fichier a un
# domaine unique ⇒ `class = "<domaine>_error"` ; (b) il n'en a pas ⇒ **la classe
# porte le FICHIER** (`R/sc/sc_helpers.R` → `sc_helpers_error`).
# `mod_sc.R` est de la branche (b) : son en-tête le déclare « **Parent Router
# Module** », et il empile des observateurs **sans domaine commun** (langue,
# onglet actif, panier de viz, auto-pipeline) puis l'export du rapport. ⇒ La
# classe porte le fichier : **`mod_sc_error`**.
# ⚠️ **Portée de cette décision** : elle vaut pour les **3 routeurs parents**
# (`mod_sc.R`, `mod_bulk.R`, `mod_spatial.R`) — les deux autres portent encore
# des sites C10 (`mod_bulk.R:427`) et devaient donc être tranchés *avant* d'y
# toucher. Elle est consignée dans `CONVENTIONS.md` §7 pour ne pas être
# re-litigée au prochain lot.
#
# 🟢 **LOT « QUI PAIE DEUX FOIS »** : `mod_sc.R` porte **DEUX** sites C10
# (625 et 707), tous deux dans le **MÊME** `content = function(file)` du
# `downloadHandler` du rapport (`output$dl_report`, **L621 → L711**).
# ⇒ un seul fichier converti = **−2** sur C10.
#
# ⚠️ **Extraction AST : le `content` est un ARGUMENT NOMMÉ**, pas un top-level.
# On cherche donc les appels `downloadHandler(...)` et on prend l'élément
# `content` dont le `deparse()` contient la garde — jamais « le 1ᵉʳ trouvé » :
# le fichier en contient **plusieurs** (`dl_report`, `dl_sc_r_script`, …).
#
# ⚠️ **`file.exists()` est le CŒUR de la garde du site 625** : c'est lui qu'on
# mocke. Il est **enregistré** (témoin de traversée) car le contrôle de borne
# en a besoin.
#
# 🔴 **`withProgress()` DOIT être bouchonné — mesuré.** Sa signature réelle est
# `(expr, min, max, value, message, detail, style, session, env, quoted)` : le
# bloc positionnel du module tombe donc sur **`expr`**, le PREMIER formel. Mais
# hors session Shiny il lève `'session' is not a ShinySession object`. On le
# remplace par `function(expr, ...) expr` — retourner `expr` force le bloc à
# s'exécuter. (C'est une **primitive d'UI**, comme `showNotification`, pas le
# sujet du test.)
#
# ⚠️ **`%||%` est REDÉFINI localement** (cf. `R/core/io_helpers.R:47`) : le
# `content` en fait un usage intensif dans `render_params`. ⚠️ Le dépôt en
# contient **deux définitions DIVERGENTES** (`io_helpers.R` teste aussi
# `length(a) == 0`, `R/plotting/palettes.R` non) — on reproduit celle de
# `io_helpers.R`.
#
# ⚠️ **`req()` et `isolate()` sont RÉELS** (`shiny::`), pas mockés : mesuré,
# `req()` passe hors contexte réactif dès que ses arguments sont truthy.
#
# ⚠️ **C16 / §2bn sans objet sur les deux sites** : messages à **UN SEUL**
# argument et entièrement statiques ⇒ **aucun `paste0()`**. On assère les
# messages **ENTIERS** (§2bx.3).
#
# 🔴 **LIMITE DOCUMENTÉE — le site 707 n'a PAS de contrôle de borne.** Franchir
# sa garde exige que `out_files` soit NON VIDE, donc qu'un `rmarkdown::render()`
# **réussisse** (`out_files <- c(out_files, res)`), et `rmarkdown::render` est
# appelé via `::` ⇒ **non mockable**. Le contrôle n'est donc pas réalisable à
# coût raisonnable. ⚠️ **L'assertion d'erreur, elle, n'est PAS vacante** : la
# classe n'apparaît QUE si notre `stop()` converti s'est exécuté — ce qui EST la
# preuve de joignabilité. Ce qui manque est la preuve de **conditionnalité**.
# =============================================================================

source_project_file("modules/sc/mod_sc.R")

.MSCR_FILE <- "modules/sc/mod_sc.R"

# --- Extraction AST : l'argument `content` du downloadHandler qui porte la garde
# Harnais partage `helper-ast.R` (§2dg) : derniere des **14** copies du depot.
# ⚠️ Le needle porte sur le TEXTE du `content`, pas sur l'appel englobant : on
# collecte donc l'appel `downloadHandler` puis on filtre sur son `content`.
.mscr_content_expr <- function(needle) {
  p     <- ts_ast_parse(.MSCR_FILE)
  calls <- ts_ast_find_all(p, function(x) {
    if (!ts_ast_is_call_to(x, "downloadHandler")) return(FALSE)
    l  <- as.list(x)
    nm <- names(l)
    if (is.null(nm)) return(FALSE)
    for (i in seq_along(l)) {                    # (1) par INDEX
      if (!identical(nm[i], "content")) next
      if (grepl(needle, ts_ast_deparse(l[[i]]), fixed = TRUE)) return(TRUE)
    }
    FALSE
  })
  for (x in calls) {
    l  <- as.list(x)
    nm <- names(l)
    for (i in seq_along(l)) {
      if (!is.null(nm) && identical(nm[i], "content")) {
        d <- ts_ast_deparse(l[[i]])
        if (grepl(needle, d, fixed = TRUE)) return(l[[i]])
      }
    }
  }
  stop("aucun downloadHandler(content=) contenant '", needle, "'", call. = FALSE)
}

.MSCR_NEEDLE <- "Template introuvable"

# --- Environnement enfant -----------------------------------------------------
# `exists_fun(path)` décide de `file.exists()` ; `rec` enregistre les étapes
# (témoin de traversée) ; `file_copy_throws` sert au contrôle de borne du 625.
.mscr_env <- function(exists_fun, report_format, rec = NULL, file_copy_throws = FALSE) {
  e <- new.env(parent = globalenv())
  e$.tr       <- function(s) s
  e$.tr_plain <- function(s) s
  # cf. R/core/io_helpers.R:47 (l'autre definition du depot omet `length(a) == 0`)
  e$`%||%`    <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req       <- shiny::req       # REEL (mesure : truthy hors reactif OK)
  e$isolate   <- shiny::isolate   # REEL
  e$state_get <- function(state, field) NULL
  e$showNotification <- function(...) invisible(NULL)
  # Primitive d'UI : hors session Shiny elle leve (mesure) ⇒ bouchon.
  e$withProgress <- function(expr, ...) expr
  e$file.exists <- function(...) {
    path <- list(...)[[1]]
    v <- exists_fun(path)
    if (!is.null(rec)) rec(paste0("file.exists:", basename(path), "=", v))
    v
  }
  e$file.copy <- function(...) {
    if (!is.null(rec)) rec("file.copy")
    if (file_copy_throws) stop("__arret_volontaire__")
    TRUE
  }
  e$global_data <- list(sc_obj = "FAKE_OBJ", language = "fr", i18n = NULL)
  e$shared_rv   <- list()
  e$input <- list(report_format = report_format, report_sections = character(0),
                  report_title = "T", report_subtitle = "S", report_notes = "N",
                  report_interactive = FALSE)
  e
}

# ⚠️ `eval()` rend la FONCTION ; il faut l'APPELER (§2cn.3).
.mscr_call <- function(envir) {
  tryCatch({
    f <- eval(.mscr_content_expr(.MSCR_NEEDLE), envir = envir)
    f(tempfile(fileext = ".html"))
    NULL
  },
  error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSCR_TPL_MSG <- "Template introuvable : reports/sc_report_template.Rmd"
.MSCR_FMT_MSG <- "Aucun format généré."

# ---------------------------------------------------------------------------
# Site 625 — template de rapport absent
# ---------------------------------------------------------------------------
test_that("mod_sc : template absent ⇒ classe (site 625)", {
  steps <- character(0)
  res <- .mscr_call(.mscr_env(function(p) FALSE, "html",
                              rec = function(s) steps <<- c(steps, s)))

  expect_false(is.null(res))                                  # erreur levée…
  expect_true("mod_sc_error" %in% res$class)                      # …avec NOTRE classe
  # Message a UN SEUL argument, entierement statique ⇒ message ENTIER (§2bx.3).
  expect_identical(res$msg, .MSCR_TPL_MSG)
  # Temoin de traversee : `file.exists()` a bien ete evalue (garde atteinte).
  expect_true(any(grepl("sc_report_template", steps)))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 625) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde template est conditionnelle : present ⇒ pas notre classe", {
  steps <- character(0)
  # `file.exists` -> TRUE ⇒ on FRANCHIT la garde. On arrete juste APRES, sur
  # `file.copy` (L627), pour ne pas partir dans un vrai `rmarkdown::render`.
  res <- .mscr_call(.mscr_env(function(p) TRUE, "html",
                              rec = function(s) steps <<- c(steps, s),
                              file_copy_throws = TRUE))

  # ⚠️ Temoin de traversee NON VACUANT (§2cj.3) : `file.copy` est appele L627,
  # APRES la garde L624 ⇒ son enregistrement PROUVE qu'on l'a franchie. Sans
  # lui, l'absence de notre classe serait vraie aussi si le code avait echoue
  # AVANT (p. ex. dans `req()`).
  expect_true("file.copy" %in% steps)
  expect_false(is.null(res))
  expect_false("mod_sc_error" %in% res$class)
  expect_true(grepl("__arret_volontaire__", res$msg, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Site 707 — aucun format de rapport n'a pu être généré
# ---------------------------------------------------------------------------
test_that("mod_sc : aucun format genere ⇒ classe (site 707)", {
  steps <- character(0)
  # `file.exists` -> TRUE pour le template (franchit la garde 625), FALSE pour
  # l'i18n (evite un `jsonlite::fromJSON` inutile).
  # `report_format` INCONNU => `switch()` rend NULL => la boucle ne tourne pas
  # => `out_files` reste vide => la garde 707 tire.
  res <- .mscr_call(.mscr_env(function(p) !grepl("translation.json", p), "INCONNU",
                              rec = function(s) steps <<- c(steps, s)))

  expect_false(is.null(res))
  expect_true("mod_sc_error" %in% res$class)
  expect_identical(res$msg, .MSCR_FMT_MSG)
  # Temoin : on a franchi la garde 625 et telecharge le template avant d'arriver
  # au 707. ⚠️ Aucun controle de BORNE ici : franchir 707 exige un
  # `rmarkdown::render()` REUSSI, non mockable (cf. en-tete).
  expect_true("file.copy" %in% steps)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_sc.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_sc.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSCR_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_sc.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# 2J (O28, batch B3) : largeur de la sidebar paramétrable
# ---------------------------------------------------------------------------
# Constat 2J : à HEAD, `width = 420` figé (preuve mod_sc.R:571 — 30,7 % d'un
# écran 1366 px). Correctif : option globale `ts.sc_sidebar_width` ou argument
# `sidebar_width` de mod_sc_ui(), bornée [240, 720] px. Le mécanisme réactif
# complet (toggle ◀/▶ + offcanvas) est le constat 2L — batch B4, PAS ici.

test_that("2J : .sc_sidebar_width — defaut 420, option globale, bornes [240, 720]", {
  expect_identical(.sc_sidebar_width(), 420L)
  expect_identical(.sc_sidebar_width(360), 360L)
  expect_identical(.sc_sidebar_width("500"), 500L)
  # Bornes : trop petit / trop grand -> clamp, jamais un layout cassé.
  expect_identical(.sc_sidebar_width(100), 240L)
  expect_identical(.sc_sidebar_width(9999), 720L)
  # Invalide -> repli 420 (jamais NA transmis à sidebar(width=)).
  expect_identical(.sc_sidebar_width(NA), 420L)
  expect_identical(.sc_sidebar_width("abc"), 420L)
  expect_identical(.sc_sidebar_width(c(300, 500)), 420L)
  # L'option globale est le canal de paramétrisation (app.R n'est pas modifié) :
  old <- options(ts.sc_sidebar_width = 360)
  on.exit(options(old), add = TRUE)
  expect_identical(.sc_sidebar_width(), 360L)
})

test_that("2J verrou source : mod_sc_ui accepte sidebar_width et la transmet à width", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src,
               "mod_sc_ui <- function(id, sidebar_width = .sc_sidebar_width())",
               fixed = TRUE)
  expect_false(grepl("width[[:space:]]*=[[:space:]]*420[,)]", src),
               label = "width = 420 figé subsiste (constat 2J)")
})

# ---------------------------------------------------------------------------
# 2K (O28, batch B3) : repliage des panneaux d'accordéon après validation
# ---------------------------------------------------------------------------
# Constat 2K : à HEAD, `git grep accordion_panel_close` sur modules/ + tools/
# renvoyait 0 hit — les panneaux restaient ouverts après validation (spéc
# §2.22). Fonction pure `.sc_acc_panels_to_close()` + observateur à signature
# (le repli ne tire qu'au CHANGEMENT d'état : jamais de combat réactif avec un
# panneau que l'utilisateur a rouvert).
#
# ⚠️ Limite documentée (même piège que le reste du fichier) : le message
# `accordion_panel_close` est consommé par le widget JS — sous testServer()
# l'input ne bouge pas (leçon B1) ; la preuve comportementale du message ne
# peut pas être levée ici. Ce lot prouve : (a) la correspondance état →
# panneaux (fonction pure), (b) le verrou source de l'observateur.

test_that("2K : .sc_acc_panels_to_close — tronc pipeline + marqueurs, jamais partiel", {
  # Rien de validé -> rien à replier :
  expect_length(.sc_acc_panels_to_close(FALSE, FALSE, FALSE, FALSE), 0L)
  # Tronc partiel (ex. QC seul, ou QC+Norm sans clusters) -> Pipeline ouvert :
  expect_length(.sc_acc_panels_to_close(TRUE, FALSE, FALSE, FALSE), 0L)
  expect_length(.sc_acc_panels_to_close(TRUE, TRUE, FALSE, FALSE), 0L)
  # Tronc QC + Normalisation + Clustering validé -> « 1. Pipeline » replié :
  got <- .sc_acc_panels_to_close(TRUE, TRUE, TRUE, FALSE)
  expect_identical(got$acc_prep, "1_pipeline")
  expect_false("acc_analyse" %in% names(got))
  # Marqueurs posés (module manuel OU auto-pipeline) -> « 4. Marqueurs » replié :
  got2 <- .sc_acc_panels_to_close(FALSE, FALSE, FALSE, TRUE)
  expect_identical(got2$acc_analyse, "4_markers")
  expect_false("acc_prep" %in% names(got2))
  # Les deux à la fois :
  got3 <- .sc_acc_panels_to_close(TRUE, TRUE, TRUE, TRUE)
  expect_setequal(names(got3), c("acc_prep", "acc_analyse"))
})

test_that("2K verrou source : repli via accordion_panel_close au changement de signature + pastille", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Le repli passe par l'API bslib documentée (jamais un hack CSS) :
  expect_match(src,
               "bslib::accordion_panel_close(ns(acc_id), values = to_close[[acc_id]])",
               fixed = TRUE)
  # Garde anti-combat réactif : un seul repli par signature d'état.
  expect_match(src, "identical(sig, acc_close_signature())", fixed = TRUE)
  # Pastille de résumé dans l'en-tête du panneau « 1. Pipeline » (spéc §2.22) :
  expect_match(src, "acc_pipeline_resume_badge", fixed = TRUE)
  expect_match(src, "\u2705 Pipeline valid\u00e9 \u2014 %s cellules conserv\u00e9es",
               fixed = TRUE)
})

# ── B4 (O28, 2026-10-30) : 2A — les 17 onglets « Résultats » en 5 nav_menu ───
# Re-sweep 1920×1080 (RE_SWEEP_1920_2026-10-30.md) : barre 2 lignes / 100 px
# (couche L2 du goulot 2I) ; après regroupement spéc §2.6 : 1 ligne / 42 px.
# Les `value` sont INCHANGÉS — l'état active_tab (mod_sc_server:867) et les
# nav_select existants (3C corr, drive-watcher tabmap) les visent ; le lock
# drive-watcher `value = "<tab>", mod_sc_<m>_output_ui` reste contigu.
test_that("2A : les 17 onglets Résultats sont regroupés en 5 nav_menu thématiques (spéc §2.6)", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # 5 pôles, libellés de la spéc §2.6 (le caret des menus est dessiné par
  # bslib — pas de « ▾ » dans les clés i18n) :
  for (grp in c("Exploration", "Différentiel & DA", "Réseaux & Signalisation",
                "Dynamique", "Contrôle & Rapports")) {
    expect_match(src, sprintf('nav_menu(i18n$t("%s")', grp), fixed = TRUE, info = grp)
  }
  # Exactement 5 nav_menu dans le fichier (pas de regroupement partiel) :
  expect_identical(lengths(regmatches(src, gregexpr("nav_menu(i18n$t(", src,
                                                    fixed = TRUE))), 5L)
  # Le navset Résultats garde son id (état + tests drive-watcher) :
  expect_match(src, 'id = ns("main_tabs")', fixed = TRUE)
  # Les 17 `value` sont inchangés — aucun rename silencieux :
  for (v in c("tab_viz", "tab_table", "tab_pseudobulk", "tab_annotation",
              "tab_rarity", "tab_correlation", "tab_pathway", "tab_trajectory",
              "tab_velocity", "tab_communication", "tab_da_design", "tab_da_milo",
              "tab_da_sccoda", "tab_da_cross", "tab_report_consolide",
              "tab_qc", "tab_summary")) {
    expect_match(src, sprintf('value = "%s"', v), fixed = TRUE, info = v)
  }
  # Chaque pôle porte son membership de la spéc §2.6 (contigüité value+UI,
  # même motif que le lock drive-watcher) :
  expect_match(src, 'value = "tab_viz", mod_sc_viz_output_ui', fixed = TRUE)
  expect_match(src, 'value = "tab_communication", mod_sc_communication_output_ui', fixed = TRUE)
  expect_match(src, 'value = "tab_report_consolide", mod_sc_report_consolidated_output_ui', fixed = TRUE)
})


# =============================================================================
# B2 (O28, 2026-10-31) — items 1E et 3A
# =============================================================================
# 1E (spéc §2.5) : garde contextuelle single-sample — onglets VISIBLES, libellé
# 🔒 dynamique + carte d'état vide actionnable (pourquoi + voie réelle + CTA
# deep-link import), contenu arbitré par l'opérateur (DECISIONS.md row O28 (b),
# non édulcorable). 3A (spéc §2.11 + live audit §5.1) : lignes de seuil sur les
# violons + rappel des seuils appliqués + compte « retirées » complet.

test_that("1E : les 3 onglets DA gardent leurs value et passent par les uiOutput gardés", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Les `value` sont verrouillés par les contrats freeze + le test 2A — la
  # garde ne touche QUE la présentation :
  for (v in c("tab_da_milo", "tab_da_sccoda", "tab_da_cross")) {
    expect_match(src, sprintf('value = "%s"', v), fixed = TRUE, info = v)
  }
  # Canvas gardé (carte 🔒 vs UI réelle) + libellé dynamique :
  for (pfx in c("da_milo", "da_sccoda", "da_cross")) {
    expect_match(src, sprintf('uiOutput(ns("%s_guarded"))', pfx), fixed = TRUE, info = pfx)
    expect_match(src, sprintf('uiOutput(ns("%s_tab_title"))', pfx), fixed = TRUE, info = pfx)
  }
})

test_that("1E : la carte porte le contenu arbitré O28(b) — pourquoi + voie réelle + CTA", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # POURQUOI (non négociable — O28(b) : ≥ 2 échantillons réels par condition ;
  # les cellules ne sont pas des réplicats) :
  expect_match(src, "au moins 2 \u00e9chantillons R\u00c9ELS par condition", fixed = TRUE)
  expect_match(src, "Les cellules ne sont pas des r\u00e9plicats", fixed = TRUE)
  expect_match(src, "aucune pseudoreplication tol\u00e9r\u00e9e", fixed = TRUE)
  # LA VOIE RÉELLE EXISTANTE (import multi-échantillons, parse A_1/B2, table
  # design éditable) :
  expect_match(src, "A_1 / B2", fixed = TRUE)
  expect_match(src, "Ouvrir l'\u00e9tape 0.5", fixed = TRUE)
  # CTA deep-link import : saut racine par le sélecteur BS5 STABLE data-value
  # (nav_select("main_nav") est un no-op depuis un module — makeScope préfixe
  # sendInputMessage ; shinyjs n'est pas activé dans app.R).
  expect_match(src, '.sc_root_tab_js("Single-Cell")', fixed = TRUE)
  expect_match(src, 'data-value', fixed = TRUE)
  # CTA design in-module (accordéon de CETTE sidebar) :
  expect_match(src, 'values = "0_metadata"', fixed = TRUE)
})

test_that("1E : l'éligibilité consomme le verdict moteur (aucun seuil re-enseigné)", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # verdict canonique (sc_abundance_design.R:131-134) — pas de re-validation locale :
  expect_match(src, "da_design_status_is_valid(des$status)", fixed = TRUE)
  # Les serveurs DA restent montés une fois pour toutes (contrats freeze
  # milo/sccoda/cross) — la garde ne les remonte pas :
  for (srv in c("mod_sc_da_milo_server", "mod_sc_da_sccoda_server",
                "mod_sc_da_cross_server")) {
    expect_match(src, srv, fixed = TRUE, info = srv)
  }
})

test_that("3A : lignes de seuil — nFeature min/max + percent.mt, JAMAIS nCount (non filtré)", {
  applied <- list(source = "auto", min_genes = 100, max_genes = 8000, max_mt = 20,
                  n_removed = 800L, n_remaining = 1900L)
  ln <- .sc_qc_threshold_lines(applied)
  expect_false(is.null(ln))
  expect_setequal(unique(ln$feature), c("nFeature_RNA", "percent.mt"))
  expect_identical(sort(ln$y), c(20, 100, 8000))
  expect_identical(sum(ln$feature == "nFeature_RNA"), 2L)  # min ET max

  # Un run manuel (source != "auto") ne produit AUCUNE ligne — ses seuils ne
  # sont pas devinables ici (mod_sc_pipeline.R hors périmètre) : honnêteté.
  expect_null(.sc_qc_threshold_lines(list(source = "manual")))
  expect_null(.sc_qc_threshold_lines(NULL))
  # Seuils non numériques (QC sauté) : lignes droppées, jamais NaN tracé :
  expect_null(.sc_qc_threshold_lines(list(source = "auto", min_genes = NA_real_)))
})

test_that("3A : formes canoniques du rappel (applied / manual / skipped / none)", {
  expect_identical(.sc_qc_recall_lines(NULL)$kind, "none")
  expect_identical(.sc_qc_recall_lines(list(source = "manual"))$kind, "manual")
  expect_identical(.sc_qc_recall_lines(list(source = "auto", min_genes = NA_real_))$kind,
                   "skipped")
  r <- .sc_qc_recall_lines(list(source = "auto", min_genes = 100, max_genes = 8000,
                                max_mt = 20, n_removed = 800, n_remaining = 1900))
  expect_identical(r$kind, "applied")
  expect_identical(r$min_genes, 100)
  expect_identical(r$max_genes, 8000)
  expect_identical(r$max_mt, 20)
  expect_identical(r$n_removed, 800)
  expect_identical(r$n_remaining, 1900)
})

test_that("3A verrou source : rappel QC dans l'onglet + log non tronqué + capture aux lancements", {
  src <- paste(readLines(file.path(ts_project_root(), .MSCR_FILE),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Rappel dans le canvas QC (UX-1 : « les seuils appliqués ne sont rappelés
  # nulle part ») :
  expect_match(src, 'uiOutput(ns("qc_threshold_recall"))', fixed = TRUE)
  # Compte complet (UX-1) : le conteneur force le repli des lignes longues :
  expect_match(src, 'class = "sc-auto-log-wrap"', fixed = TRUE)
  expect_match(src, "white-space: pre-wrap", fixed = TRUE)
  # Capture aux lancements du pipeline auto (UI confirm, voie directe, drive)
  # + confirmation par le snapshot moteur (sc_pipeline.R:237-245) :
  expect_match(src, ".sc_qc_capture_applied(params)", fixed = TRUE)
  expect_match(src, ".sc_qc_capture_applied(sc_ap_normalize_params(as.list(input)))",
               fixed = TRUE)
  expect_match(src, ".sc_qc_capture_applied(sc_ap_normalize_params(drive_inputs))",
               fixed = TRUE)
  expect_match(src, "res$qc_snapshot", fixed = TRUE)
  # Purge honnête au changement de dataset :
  expect_match(src, "qc_applied_rv(NULL)", fixed = TRUE)
})
