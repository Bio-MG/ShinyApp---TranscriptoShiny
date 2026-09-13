#!/usr/bin/env Rscript
# =============================================================================
# tools/check_conventions.R — garde statique des conventions de code
#                             TranscriptoShiny ("Cerberus")
# =============================================================================
# Base R uniquement (aucune dépendance), comme tools/check_duplication.R : la
# garde doit pouvoir tourner sur n'importe quel poste avant un merge, sans
# étape d'installation.
#
# Ce que ce fichier fait : il rend MECANIQUES les conventions écrites dans
# `docs/CONVENTIONS.md` (qui fait elle-même référence aux règles dures de
# `AGENTS.md`). Une convention non vérifiable n'est qu'un vœu : chaque règle
# documentée porte donc un ID C1..C12 repris ci-dessous et dans le doc.
#
# USAGE
#   Rscript tools/check_conventions.R [--strict] [--no-git]
#
#   --strict   : les AVERTISSEMENTS font échouer la garde (exit 1).
#   --no-git   : n'interroge pas git (règle C3 partielle : existence seule).
#
# SÉVÉRITÉ
#   ERREUR  -> enfreint une règle dure : la garde sort en 1. Le dépôt doit
#              rester à ZÉRO erreur (toutes les ERREUR sont actuellement
#              vertes ; une régression est donc immédiatement visible).
#   AVERT.  -> dette connue et mesurée (plafonds relevés à chaque session
#              dans docs/CONVENTIONS.md §12). Ne casse pas la garde sauf
#              --strict. Sert à interdire l'AGGRAVATION : relancer la garde
#              après un chantier et comparer les compteurs.
#
# EXIT CODE : 0 si aucune ERREUR (et, avec --strict, aucun AVERT.),
#             1 sinon.
# =============================================================================

# ---------------------------------------------------------------------------
# Utilitaires de lecture : on travaille sur le code, pas sur le texte brut.
# ---------------------------------------------------------------------------

#' Retire (approximativement) le contenu des chaînes et les commentaires de
#' fin de ligne. Volontairement simple — ce n'est pas un vrai tokenizer R —
#' mais suffisant pour ne pas signaler un `library(Seurat)` qui n'existe que
#' DANS le texte d'un script reproductible généré (cas réel :
#' R/sc/sc_export.R), ni un `tr("...")` cité dans un commentaire.
.strip_strings_and_comments <- function(line) {
  line <- gsub('"([^"\\\\]|\\\\.)*"', '""', line, perl = TRUE)
  line <- gsub("'([^'\\\\]|\\\\.)*'", "''", line, perl = TRUE)
  sub("#.*$", "", line)
}

#' Une ligne est-elle un commentaire pur ?
.is_comment_line <- function(line) grepl("^\\s*#", line)

.read_code_lines <- function(path) {
  raw <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
  data.frame(
    line_no = seq_along(raw),
    raw     = raw,
    code    = vapply(raw, .strip_strings_and_comments, character(1),
                     USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

.collect_files <- function(roots, ext = "R") {
  pattern <- paste0("\\.", ext, "$")
  files <- character(0)
  for (root in roots) {
    if (!dir.exists(root)) {
      if (file.exists(root)) { files <- c(files, root); next }
      next
    }
    files <- c(files, list.files(root, pattern = pattern, full.names = TRUE,
                                 recursive = TRUE))
  }
  unique(normalizePath(files, winslash = "/", mustWork = FALSE))
}

# ---------------------------------------------------------------------------
# Moteur de rapport
# ---------------------------------------------------------------------------
.REPORT <- new.env(parent = emptyenv())
.REPORT$errors <- list()
.REPORT$warns  <- list()

.add <- function(severity, rule, file, line, detail) {
  entry <- list(rule = rule, file = file, line = line, detail = detail)
  if (identical(severity, "ERROR")) {
    .REPORT$errors[[length(.REPORT$errors) + 1L]] <- entry
  } else {
    .REPORT$warns[[length(.REPORT$warns) + 1L]] <- entry
  }
  invisible(NULL)
}

#' NB : la racine du projet contient des parenthèses (« … (git work) … ») —
#' elle ne peut PAS être injectée dans une expression régulière sans
#' échappement. D'où startsWith() + substr() au lieu de sub().
.rel <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  b <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (startsWith(p, b)) sub("^/", "", substr(p, nchar(b) + 1L, nchar(p))) else p
}

# =============================================================================
# C1 — `R/modules/` ne doit pas exister (règle : R/ = logique pure)
# C2 — aucun symbole Shiny dans R/
# C3 — toute cible de source() existe ET est versionnée
# C4 — aucun setwd() dans R/ ou modules/
# C5 — aucun browser() oublié
# =============================================================================
.SHINY_SYMBOLS <- c("input\\$", "output\\$", "session\\$", "observeEvent\\(",
                    "renderPlot\\(", "renderUI\\(", "renderTable\\(",
                    "downloadHandler\\(", "reactiveVal\\(", "reactive\\(")

#' Réactivité dure : jamais dans R/, quel que soit le contexte. Le lookbehind
#' évite les faux positifs du type `report_input$type` (champ de liste) pris
#' pour le `input$` de Shiny — piège réellement rencontré sur ce dépôt.
.SHINY_HARD <- c(
  "observeEvent\\(", "observe\\(", "renderPlot\\(", "renderUI\\(", "renderTable\\(",
  "renderText\\(", "renderDataTable\\(", "renderImage\\(", "downloadHandler\\(",
  "reactive\\(", "(?<![A-Za-z0-9_.])output\\$"
)

#' Exception ASSUMÉE et documentée (docs/CONVENTIONS.md §3.2) : `R/core/state.R`
#' est la couche d'état transversale — son rôle EST de fabriquer les schémas
#' `reactiveValues` partagés. Partout ailleurs dans R/, c'est une erreur.
.STATE_LAYER <- c("R/core/state.R", "R/sc/sc_state.R")
.SHINY_STATE_FACTORIES <- c("reactiveValues\\(", "reactiveVal\\(")

#' Injection explicite : `input` / `session` passés en PARAMÈTRE sont assumés
#' (run_sc_auto_pipeline(input, ..., session), .safe_plot_render(session, ...)).
#' Accédés autrement (variable libre, environnement global) -> ERREUR.
.SHINY_SOFT <- c(
  input   = "(?<![A-Za-z0-9_.])input\\$",
  session = "(?<![A-Za-z0-9_.])session\\$"
)

#' Solde des parenthèses d'un fragment de code (ouvertes - fermées).
.paren_balance <- function(txt) {
  sum(unlist(gregexpr("\\(", txt, fixed = TRUE)) > 0) -
    sum(unlist(gregexpr("\\)", txt, fixed = TRUE)) > 0)
}

#' Extrait la liste des paramètres d'une signature `f <- function(a, b = 1)`.
#' Gère les signatures écrites sur plusieurs lignes.
.parse_formals <- function(sig) {
  inner <- sub("^.*function\\s*\\(", "", sig, perl = TRUE)
  inner <- sub("\\).*$", "", inner, perl = TRUE)
  parts <- strsplit(inner, ",")[[1]]
  trimws(sub("\\s*=.*$", "", parts))
}

check_c1_r_modules <- function() {
  if (dir.exists(file.path("R", "modules"))) {
    .add("ERROR", "C1", "R/modules", NA_integer_,
         "R/modules/ existe — R/ est la couche de logique pure (AGENTS.md règle 9).")
  }
}

check_c2_shiny_in_r <- function(r_files) {
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    formals <- character(0)

    for (i in seq_len(nrow(ann))) {
      ln <- ann$code[i]

      # -- fonction englobante : on lit la signature par ÉQUILIBRAGE des
      #    parenthèses à partir de `function(`, pas "jusqu'à la prochaine ligne
      #    contenant )" — cette dernière heuristique se coinçait dès qu'une
      #    signature contenait un `#` ou une chaîne avec une parenthèse, et
      #    laissait `collecting` bloqué sur des centaines de lignes.
      m_fun <- regexpr("function\\s*\\(", ln, perl = TRUE)
      if (m_fun > 0 &&
          grepl("^[A-Za-z_.][A-Za-z0-9_.]*\\s*(<-|=)\\s*function\\s*\\(", ln, perl = TRUE)) {
        sig <- substring(ln, m_fun)
        j <- i
        while (j < nrow(ann) && .paren_balance(sig) > 0L) {
          j <- j + 1L
          sig <- paste0(sig, " ", ann$code[j])
        }
        formals <- .parse_formals(sig)
      }

      is_state_layer <- .rel(f) %in% .STATE_LAYER

      for (sym in .SHINY_HARD) {
        if (grepl(sym, ln, perl = TRUE)) {
          .add("ERROR", "C2", .rel(f), ann$line_no[i],
               sprintf("réactivité Shiny dans R/ (`%s`) — la réactivité appartient à modules/ (règle 9).",
                       gsub("\\\\", "", sym)))
        }
      }
      if (!is_state_layer) {
        for (sym in .SHINY_STATE_FACTORIES) {
          if (grepl(sym, ln, perl = TRUE)) {
            .add("ERROR", "C2", .rel(f), ann$line_no[i],
                 sprintf("%s hors de la couche d'état — seul %s fabrique des conteneurs réactifs.",
                         gsub("\\\\", "", sym), .STATE_LAYER))
          }
        }
      }

      for (nm in names(.SHINY_SOFT)) {
        if (grepl(.SHINY_SOFT[[nm]], ln, perl = TRUE) && !(nm %in% formals)) {
          .add("ERROR", "C2", .rel(f), ann$line_no[i],
               sprintf("`%s$` utilisé dans R/ sans être un paramètre de la fonction — injecter `%s` en argument (pattern assumé) ou déplacer l'appel dans modules/.",
                       nm, nm))
        }
      }
    }
  }
}

check_c3_source_targets <- function(use_git = TRUE) {
  scan_roots <- c("app.R", "global.R", "R", "modules", "config")
  files <- character(0)
  for (root in scan_roots) {
    if (dir.exists(root)) {
      files <- c(files, list.files(root, pattern = "\\.R$", full.names = TRUE,
                                   recursive = TRUE))
    } else if (file.exists(root)) {
      files <- c(files, root)
    }
  }
  targets <- character(0)
  origins <- character(0)
  for (f in unique(files)) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    m <- regmatches(ann$raw, regexec('source\\("([^"]+)"\\)', ann$raw, perl = TRUE))
    for (i in seq_along(m)) {
      if (length(m[[i]]) >= 2) {
        targets <- c(targets, m[[i]][2])
        origins <- c(origins, sprintf("%s:%d", .rel(f), ann$line_no[i]))
      }
    }
  }
  if (!length(targets)) return(invisible(NULL))
  keep <- !duplicated(targets)
  targets <- targets[keep]; origins <- origins[keep]

  for (i in seq_along(targets)) {
    tgt <- targets[i]
    if (!file.exists(tgt)) {
      .add("ERROR", "C3", tgt, NA_integer_,
           sprintf("source(\"%s\") pointe vers un fichier ABSENT (%s).", tgt, origins[i]))
      next
    }
    if (!use_git) next
    # Un fichier ignoré par git ET non suivi est absent d'un clone neuf :
    # l'app ne pourra pas être sourcée. (Cas réel corrigé le 2026-09-13 :
    # R/plotting/complex_heatmap.R était dans .gitignore alors que app.R le source.)
    ignored <- suppressWarnings(
      system2("git", c("check-ignore", "--no-index", "-q", shQuote(tgt)),
              stdout = FALSE, stderr = FALSE)
    )
    tracked <- suppressWarnings(
      system2("git", c("ls-files", "--error-unmatch", shQuote(tgt)),
              stdout = FALSE, stderr = FALSE)
    )
    if (identical(ignored, 0L) && !identical(tracked, 0L)) {
      .add("ERROR", "C3", .rel(tgt), NA_integer_,
           sprintf("sourcé par %s mais EXCLU du versionnage (.gitignore) — un clone neuf ne pourra pas sourcer l'app.",
                   origins[i]))
    }
  }
}

check_c4_setwd <- function(files) {
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    hits <- which(grepl("setwd\\(", ann$code, perl = TRUE))
    for (i in hits) {
      .add("ERROR", "C4", .rel(f), ann$line_no[i],
           "setwd() — le répertoire courant doit rester la racine du projet (renv).")
    }
  }
}

check_c5_browser <- function(files) {
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    hits <- which(grepl("\\bbrowser\\s*\\(", ann$code, perl = TRUE))
    for (i in hits) {
      .add("ERROR", "C5", .rel(f), ann$line_no[i], "browser() laissé dans le code.")
    }
  }
}

# =============================================================================
# C6 — pas de library()/require() au chargement dans R/ (dette connue)
# C7 — toute clé tr("...") existe dans i18n/translation.json
# C8 — chaque contrat gelé a un test (freeze test)
# C9 — chaque fichier de R/ a un fichier de test
# C10 — erreurs : errorCondition(class=<domaine>_error) ou call. = FALSE
# C11 — primitives parallèles interdites
# C12 — en-tête de fichier documentaire
# =============================================================================
check_c6_library_in_r <- function(r_files) {
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    # profondeur d'accolades : on ne cible que le niveau TOP-LEVEL (ce qui
    # s'exécute au source(), et donc attache le package globalement).
    depth <- 0L
    for (i in seq_len(nrow(ann))) {
      ln <- ann$code[i]
      if (depth == 0L && grepl("(^|[^A-Za-z0-9_.])(library|require)\\s*\\(", ln, perl = TRUE)) {
        .add("WARN", "C6", .rel(f), ann$line_no[i],
             "library()/require() au top-level de R/ — préférer requireNamespace() + :: (un package attaché au source() masque des fonctions de l'app).")
      }
      opens  <- lengths(regmatches(ln, gregexpr("\\{", ln, fixed = TRUE)))
      closes <- lengths(regmatches(ln, gregexpr("\\}", ln, fixed = TRUE)))
      depth <- max(0L, depth + opens - closes)
    }
  }
}

#' Décode les échappements \uXXXX d'une chaîne (base R, sans jsonlite).
#' `s` est un vecteur : la fonction doit rester vectorielle (elle est appelée
#' sur les 2000+ clés d'un coup), d'où le vapply() et le `repeat` par élément.
.unescape_r <- function(s) {
  vapply(s, function(x) {
    if (length(x) == 0 || is.na(x)) return(NA_character_)
    repeat {
      m <- regmatches(x, regexpr("\\\\u[0-9a-fA-F]{4}", x, perl = TRUE))
      if (!length(m)) break
      x <- sub("\\\\u[0-9a-fA-F]{4}", intToUtf8(strtoi(substring(m, 3L), 16L)), x)
    }
    x
  }, character(1), USE.NAMES = FALSE)
}

check_c7_i18n_keys <- function(all_files) {
  json_path <- file.path("i18n", "translation.json")
  if (!file.exists(json_path)) {
    .add("ERROR", "C7", json_path, NA_integer_, "fichier de traduction absent.")
    return(invisible(NULL))
  }
  raw <- paste(readLines(json_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fr_matches <- regmatches(raw, gregexpr('"fr"\\s*:\\s*"([^"\\\\]|\\\\.)*"', raw, perl = TRUE))[[1]]
  if (!length(fr_matches)) {
    .add("ERROR", "C7", json_path, NA_integer_, "aucune entrée {fr, en} trouvée — format inattendu.")
    return(invisible(NULL))
  }
  # Les littéraux JSON sont décodés par R lui-même (eval/parse) : c'est le
  # seul moyen fiable d'aligner `\"`, `\n`, `\u00e9` côté JSON avec ce que
  # produit réellement tr("...") côté R — sinon toute clé contenant une
  # apostrophe échappée ou un guillemet serait faussement signalée manquante.
  fr_literals <- sub('^"fr"\\s*:\\s*', "", fr_matches, perl = TRUE)
  fr_keys <- unique(vapply(fr_literals, function(lit) {
    decoded <- tryCatch(eval(parse(text = lit)), error = function(e) NULL)
    if (is.null(decoded) || length(decoded) != 1L || !is.character(decoded)) {
      decoded <- .unescape_r(gsub('^"|"$', "", lit))
    }
    decoded
  }, character(1), USE.NAMES = FALSE))

  used <- list()
  for (f in all_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    for (i in seq_len(nrow(ann))) {
      if (.is_comment_line(ann$raw[i])) next
      m <- regmatches(ann$raw[i],
                      gregexpr('\\b(?:tr|i18n\\$t)\\s*\\(\\s*"([^"\\\\]|\\\\.)*"',
                               ann$raw[i], perl = TRUE))[[1]]
      for (hit in m) {
        lit <- sub('^\\b(?:tr|i18n\\$t)\\s*\\(\\s*', "", hit, perl = TRUE)
        key <- tryCatch(.unescape_r(eval(parse(text = lit))),
                        error = function(e) NA_character_)
        if (is.na(key)) next
        if (is.null(used[[key]])) used[[key]] <- character(0)
        used[[key]] <- c(used[[key]], sprintf("%s:%d", .rel(f), ann$line_no[i]))
      }
    }
  }
  missing_keys <- names(used)[!names(used) %in% fr_keys]
  for (k in missing_keys) {
    .add("ERROR", "C7", .rel(used[[k]][1]), NA_integer_,
         sprintf("clé i18n absente de translation.json : \"%s\" (ajouter via tools/add_i18n_keys.R).",
                 substr(k, 1L, 80L)))
  }
  invisible(length(used))
}

check_c8_contract_tests <- function() {
  contracts <- list.files(file.path("docs", "contracts"), pattern = "\\.md$",
                          full.names = TRUE)
  if (!length(contracts)) return(invisible(NULL))
  tests <- list.files(file.path("tests", "testthat"), pattern = "\\.R$",
                      full.names = TRUE)
  test_blob <- if (length(tests)) {
    tolower(paste(vapply(tests, function(t)
      paste(readLines(t, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      character(1)), collapse = "\n"))
  } else ""

  for (cpath in contracts) {
    base <- tools::file_path_sans_ext(basename(cpath))
    token <- tolower(sub("_CONTRACT$", "", base))
    if (!nzchar(test_blob) || !grepl(token, test_blob, fixed = TRUE)) {
      .add("WARN", "C8", .rel(cpath), NA_integer_,
           sprintf("contrat `%s` : aucun test ne le mentionne (contrat-first exige code + freeze test + doc).",
                   basename(cpath)))
    }
  }
}

#' Convention de nommage RÉELLE du dépôt : `R/bulk/bulk_gsva.R` se teste dans
#' `tests/testthat/test-bulk-gsva.R` (domaine en préfixe, `_` -> `-`), et non
#' `test-bulk_gsva.R`. Les deux formes sont acceptées pour ne pas pénaliser
#' l'historique.
check_c9_r_tests <- function(r_files) {
  for (f in r_files) {
    base <- tools::file_path_sans_ext(basename(f))
    dashed <- gsub("_", "-", base)
    domain <- basename(dirname(f))
    candidates <- c(
      file.path("tests", "testthat", paste0("test-", base, ".R")),
      file.path("tests", "testthat", paste0("test-", dashed, ".R")),
      file.path("tests", "testthat", paste0("test-", domain, "-", base, ".R")),
      file.path("tests", "testthat", paste0("test-", domain, "-", dashed, ".R"))
    )
    if (!any(file.exists(candidates))) {
      .add("WARN", "C9", .rel(f), NA_integer_,
           sprintf("aucun test éponyme (%s) — règle 5 : tout fichier de R/ est livré avec ses tests.",
                   paste(basename(candidates), collapse = " ou ")))
    }
  }
}

check_c10_error_style <- function(r_files) {
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    for (i in seq_len(nrow(ann))) {
      ln <- ann$code[i]
      if (!grepl("(^|[^A-Za-z0-9_.])stop\\s*\\(", ln, perl = TRUE)) next
      if (grepl("errorCondition", ln, fixed = TRUE)) next          # forme maison
      if (grepl("call\\.\\s*=\\s*FALSE", ln, perl = TRUE)) next     # forme explicite
      if (grepl("^\\s*stop\\(\\)", ln, perl = TRUE)) next           # stop() nu
      .add("WARN", "C10", .rel(f), ann$line_no[i],
           "stop() sans errorCondition(class=<domaine>_error) ni call. = FALSE (dette héritée ; obligatoire pour tout code neuf).")
    }
  }
}

check_c11_parallel <- function(files) {
  patterns <- c(
    "enableWGCNAThreads" = "WGCNA::enableWGCNAThreads() est interdit sous Windows (règle transverse).",
    "mclapply"           = "mclapply() = fork, non fiable sous Windows — utiliser mirai (règle 8).",
    "makeCluster"        = "makeCluster() = second framework parallèle — un seul pool : mirai (règle 8).",
    "MulticoreParam"     = "BiocParallel::MulticoreParam : autorisé UNIQUEMENT sous Unix (SerialParam sous Windows) — vérifier le garde-fou."
  )
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    for (p in names(patterns)) {
      hits <- which(grepl(p, ann$code, fixed = TRUE))
      for (i in hits) {
        .add("WARN", "C11", .rel(f), ann$line_no[i], patterns[[p]])
      }
    }
  }
}

check_c12_headers <- function(r_files) {
  for (f in r_files) {
    first <- tryCatch(readLines(f, n = 3L, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    if (!length(first) || !any(grepl("^\\s*#", first))) {
      .add("WARN", "C12", .rel(f), NA_integer_,
           "pas d'en-tête commenté (rôle du fichier, chantier d'origine, garde-fous).")
    }
  }
}

# ---------------------------------------------------------------------------
# Rapport final
# ---------------------------------------------------------------------------
run_check <- function(strict = FALSE, use_git = TRUE) {
  r_files  <- .collect_files("R")
  m_files  <- .collect_files("modules")
  all_code <- c(r_files, m_files, "app.R", "global.R")
  all_code <- all_code[file.exists(all_code)]

  cat(sprintf("[check_conventions] %d fichier(s) R/ — %d fichier(s) modules/ — git: %s\n",
              length(r_files), length(m_files), if (use_git) "oui" else "non"))

  check_c1_r_modules()
  check_c2_shiny_in_r(r_files)
  check_c3_source_targets(use_git)
  check_c4_setwd(c(r_files, m_files))
  check_c5_browser(all_code)
  check_c6_library_in_r(r_files)
  check_c7_i18n_keys(all_code)
  check_c8_contract_tests()
  check_c9_r_tests(r_files)
  check_c10_error_style(r_files)
  check_c11_parallel(all_code)
  check_c12_headers(r_files)

  rules <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12")

  cat("\n--------------------------------------------------------------------\n")
  cat(sprintf("%-5s %-8s %s\n", "RÈGLE", "NIVEAU", "DESCRIPTION"))
  cat("--------------------------------------------------------------------\n")
  desc <- c(
    C1  = "R/modules/ ne doit pas exister (R/ = logique pure)",
    C2  = "aucun symbole Shiny dans R/",
    C3  = "toute cible de source() existe et est versionnée",
    C4  = "aucun setwd() dans R/ ou modules/",
    C5  = "aucun browser() oublié",
    C6  = "pas de library()/require() au top-level de R/ (dette)",
    C7  = "toute clé tr() existe dans i18n/translation.json",
    C8  = "chaque contrat gelé est référencé par un test (dette)",
    C9  = "chaque fichier de R/ a son fichier de test (dette)",
    C10 = "stop() classé (errorCondition) ou call. = FALSE (dette)",
    C11 = "primitives parallèles à vérifier (mirai uniquement)",
    C12 = "en-tête commenté dans chaque fichier de R/ (dette)"
  )
  lvl <- setNames(rep("ERREUR", length(rules)), rules)
  lvl[c("C6", "C8", "C9", "C10", "C11", "C12")] <- "AVERT."
  for (r in rules) {
    n <- sum(vapply(.REPORT$errors, function(e) identical(e$rule, r), logical(1))) +
      sum(vapply(.REPORT$warns, function(e) identical(e$rule, r), logical(1)))
    cat(sprintf("%-5s %-8s %-55s %s\n", r, lvl[[r]], desc[[r]],
                if (n == 0) "OK (0)" else sprintf("%d signalement(s)", n)))
  }
  cat("--------------------------------------------------------------------\n")

  if (length(.REPORT$errors)) {
    cat("\n-- ERREURS (bloquantes) --\n")
    for (e in .REPORT$errors) {
      loc <- if (is.na(e$line)) .rel(e$file) else sprintf("%s:%d", .rel(e$file), e$line)
      cat(sprintf("  %-4s %s\n       %s\n", e$rule, loc, e$detail))
    }
  }
  if (length(.REPORT$warns)) {
    cat(sprintf("\n-- AVERTISSEMENTS (dette mesurée, %d) — premiers signalements --\n",
                length(.REPORT$warns)))
    for (e in head(.REPORT$warns, 25L)) {
      loc <- if (is.na(e$line)) .rel(e$file) else sprintf("%s:%d", .rel(e$file), e$line)
      cat(sprintf("  %-4s %s\n       %s\n", e$rule, loc, e$detail))
    }
    if (length(.REPORT$warns) > 25L) {
      cat(sprintf("  ... et %d autre(s) — relancer avec --strict pour tout lister.\n",
                  length(.REPORT$warns) - 25L))
    }
  }

  n_err <- length(.REPORT$errors)
  n_warn <- length(.REPORT$warns)
  cat(sprintf("\n---- Résumé : %d erreur(s), %d avertissement(s) ----\n", n_err, n_warn))
  blocking <- n_err + (if (strict) n_warn else 0L)
  invisible(if (blocking > 0) 1L else 0L)
}

# ---------------------------------------------------------------------------
# Point d'entrée — ne s'exécute qu'en `Rscript` (jamais au source()).
# ---------------------------------------------------------------------------
if (identical(environment(), globalenv()) && sys.nframe() == 0L &&
    !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0) {
  argv <- commandArgs(trailingOnly = TRUE)
  status <- run_check(strict = "--strict" %in% argv,
                      use_git = !("--no-git" %in% argv))
  quit(status = status, save = "no")
}
