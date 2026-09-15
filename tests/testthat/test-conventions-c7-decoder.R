# =============================================================================
# test-conventions-c7-decoder.R — cas NÉGATIF de la règle C7
# =============================================================================
# Règle du dépôt : une règle statique réécrite doit être éprouvée sur un cas
# NÉGATIF. Un garde qui affiche « 0 erreur » ne prouve rien si on ne l'a jamais
# vu passer au rouge.
#
# Défaut réel corrigé le 2026-09-15 (cause racine prouvée octet par octet) :
# sous `LC_CTYPE=C` — ce que Git Bash exporte via `LC_ALL=C.UTF-8`, nom que R ne
# reconnaît PAS sous Windows — `parse()` convertit le texte en encodage natif
# avant de le lire et remplace tout caractère non-ASCII par la chaîne
# LITTÉRALE « <U+00E9> » (7 octets ASCII) :
#
#     eval(parse(text = '"3c. Réseau"'))  ->  "3c. R<U+00E9>seau"
#
# Toute clé accentuée ou emoji était donc déclarée ABSENTE de
# translation.json à tort : 218 fausses erreurs C7 sous `C`, 71 sous
# `French_France.1252` (les emoji restent hors CP1252), 0 sous `fr_FR.UTF-8`.
# Le verdict de la porte de merge dépendait de la locale de l'appelant.
#
# Ce fichier vérifie les DEUX directions :
#   1. une clé PRÉSENTE n'est PAS signalée — que le code l'écrive en \uXXXX,
#      en \UXXXXXXXX ou en vrai UTF-8 (les trois formes existent au dépôt) ;
#   2. une clé réellement ABSENTE est TOUJOURS signalée, et elle seule (le
#      garde n'est pas devenu aveugle) ;
# et il le fait sous une locale non-UTF-8, pour prouver l'indépendance.
# =============================================================================

source_project_file("tools/check_conventions.R")

.C7_PRESENT_ACCENT <- "3c. R\u00e9seau de co-expression (WGCNA)"
.C7_PRESENT_EMOJI  <- "\U0001F41B Fond brut (debug PNG)"
.C7_ABSENT         <- "CLE VOLONTAIREMENT ABSENTE DE LA FIXTURE"

#' Fixture : un translation.json en VRAI UTF-8 (comme celui du dépôt, qui
#' n'utilise AUCUN échappement \uXXXX) + un fichier de code couvrant les trois
#' formes d'écriture réellement présentes au dépôt :
#'   - échappement \uXXXX        (majorité des modules)
#'   - échappement \UXXXXXXXX    (emoji)
#'   - caractère UTF-8 littéral  (R/, quelques modules)
.c7_write_fixture <- function() {
  dir <- tempfile("c7fix_")
  dir.create(dir, recursive = TRUE)
  dir.create(file.path(dir, "i18n"), showWarnings = FALSE)

  json <- c(
    "{",
    '  "languages": ["fr", "en"],',
    '  "translation": [',
    sprintf('    { "fr": "%s", "en": "3c. Co-expression network (WGCNA)" },',
            .C7_PRESENT_ACCENT),
    sprintf('    { "fr": "%s", "en": "%s Raw background (debug PNG)" },',
            .C7_PRESENT_EMOJI, .C7_PRESENT_EMOJI),
    '    { "fr": "S\u00e9lectionnez une colonne", "en": "Select a column" }',
    "  ]",
    "}"
  )
  con <- file(file.path(dir, "i18n", "translation.json"), open = "wt",
              encoding = "UTF-8")
  writeLines(json, con)
  close(con)

  code <- c(
    # 1) accent écrit en échappement R (l'antislash doit être LITTÉRAL)
    'x1 <- i18n$t("3c. R\\u00e9seau de co-expression (WGCNA)")',
    # 2) accent écrit en vrai UTF-8
    sprintf('x2 <- i18n$t("%s")', .C7_PRESENT_ACCENT),
    # 3) emoji écrit en échappement \UXXXXXXXX
    'y <- i18n$t("\\U0001F41B Fond brut (debug PNG)")',
    # 4) clé réellement absente (le cas NÉGATIF)
    sprintf('z <- i18n$t("%s")', .C7_ABSENT)
  )
  writeLines(code, file.path(dir, "fixture_code.R"), useBytes = TRUE)

  list(dir = dir,
       json = file.path(dir, "i18n", "translation.json"),
       code = file.path(dir, "fixture_code.R"))
}

test_that("C7 : le verdict ne depend PAS de la locale (C vs locale ambiante)", {
  fx <- .c7_write_fixture()
  on.exit(unlink(fx$dir, recursive = TRUE), add = TRUE)

  old <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)

  resultats <- list()
  for (loc in unique(c("C", old))) {
    got <- suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", loc),
                                     error = function(e) ""))
    if (!nzchar(got)) next
    resultats[[loc]] <- .i18n_key_sets(fx$json, fx$code)
  }
  expect_gte(length(resultats), 1L)

  for (loc in names(resultats)) {
    sets <- resultats[[loc]]
    info <- function(txt) paste0("locale ", loc, " : ", txt)
    defined <- enc2utf8(sets$defined)

    # 1. Les clés PRÉSENTES sont reconnues des deux côtés.
    expect_true(.C7_PRESENT_ACCENT %in% defined,
                info = info("cle accentuee non decodee cote JSON"))
    expect_true(.C7_PRESENT_EMOJI %in% defined,
                info = info("emoji non decode cote JSON"))
    expect_true(.C7_PRESENT_ACCENT %in% names(sets$used),
                info = info("cle accentuee non decodee cote R"))
    expect_true(.C7_PRESENT_EMOJI %in% names(sets$used),
                info = info("emoji non decode cote R"))

    # 2. CAS NÉGATIF : la clé absente est TOUJOURS détectée, et elle seule.
    #    (Une clé présente signalée manquante ferait échouer ceci aussi.)
    expect_identical(sets$missing, .C7_ABSENT,
                     info = info("detection de la cle absente"))
  }

  # 3. Verdict IDENTIQUE quelle que soit la locale.
  if (length(resultats) > 1L) {
    expect_identical(resultats[[1L]]$missing, resultats[[2L]]$missing)
    expect_setequal(resultats[[1L]]$defined, resultats[[2L]]$defined)
  }
})

test_that("C7 : parse() nu est sensible a la locale, le decodeur du garde non", {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
  got <- suppressWarnings(tryCatch(Sys.setlocale("LC_CTYPE", "C"),
                                   error = function(e) ""))
  skip_if(!identical(got, "C"), "locale C indisponible sur ce poste")

  lit <- paste0('"', .C7_PRESENT_ACCENT, '"')   # littéral JSON en vrai UTF-8
  attendu <- enc2utf8(.C7_PRESENT_ACCENT)

  # Sous locale C le défaut est bien présent : on documente le rouge...
  brut <- eval(parse(text = lit))
  if (!identical(enc2utf8(brut), attendu)) {
    expect_match(brut, "<U+00E9>", fixed = TRUE)
  }
  # ...et le chemin du garde doit malgré tout rendre la bonne clé.
  expect_identical(.decode_json_literals(lit), attendu)
})
