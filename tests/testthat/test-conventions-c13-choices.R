# =============================================================================
# test-conventions-c13-choices.R — cas NÉGATIF de la règle C13
# =============================================================================
# Règle du dépôt : une règle statique réécrite doit être éprouvée sur un cas
# NÉGATIF. Un garde qui affiche « 0 erreur » ne prouve rien si on ne l'a jamais
# vu passer au rouge.
#
# Défaut réel (2026-09-15, onglets « Réseau » des voies, bulk ET SC) :
#
#     choices = c("emap" = .tr("Voies ↔ voies (similarité de gènes)"), ...)
#
# Shiny AFFICHE LE NOM d'un vecteur nommé et RENVOIE LA VALEUR dans `input$id`
# (`?radioButtons`). Le motif ci-dessus affichait donc littéralement
# « emap » / « cnet » dans une app française, et faisait arriver le texte
# traduit complet dans `input$network_mode` ⇒ `match.arg(mode, c("emap","cnet"))`
# échouait à TOUS les coups. STATUS §2au avait conclu « non reproduit, input
# transitoire » : c'était une erreur de raisonnement, pas une panne rare.
# Motif correct : `setNames(valeur, libellé)`.
#
# Ce fichier vérifie les TROIS directions :
#   1. le motif inversé est SIGNALÉ sur une fixture (le rouge existe) ;
#   2. le motif correct `setNames()` et un vecteur nommé HORS `choices` ne sont
#      PAS des faux positifs ;
#   3. les deux modules réels sont conformes — c'est ce test qui ÉCHOUAIT avant
#      le correctif.
# =============================================================================

source_project_file("tools/check_conventions.R")

#' Fixture ASCII pure (aucun caractère non-ASCII ne doit atteindre un parse()).
.c13_write_fixture <- function() {
  dir <- tempfile("c13fix_")
  dir.create(dir, recursive = TRUE)

  # a) le motif INVERSE — celui du défaut réel
  bad <- c(
    "ui <- function() {",
    "  radioButtons(ns(\"network_mode\"), \"Type de reseau\",",
    "               choices = c(\"emap\" = .tr(\"Voies voies\"),",
    "                           \"cnet\" = .tr(\"Voies genes\")),",
    "               inline = TRUE)",
    "}"
  )
  # b) le motif CORRECT — setNames(valeur, libelle)
  good <- c(
    "ui <- function() {",
    "  radioButtons(ns(\"network_mode\"), \"Type de reseau\",",
    "               choices = stats::setNames(c(\"emap\", \"cnet\"),",
    "                                         c(.tr(\"Voies voies\"),",
    "                                           .tr(\"Voies genes\"))),",
    "               inline = TRUE)",
    "}"
  )
  # c) un vecteur nomme AILLEURS que dans un `choices` : HORS perimetre de C13.
  #    `c("EN" = .tr("Hello"))` y est legitime — le nom est une cle stable et la
  #    valeur est le texte affiche. La regle ne vise que le contrat de `choices`.
  other <- c(
    "labels <- c(\"EN\" = .tr(\"Hello\"), \"FR\" = .tr(\"Bonjour\"))"
  )

  writeLines(bad,   file.path(dir, "bad.R"),   useBytes = TRUE)
  writeLines(good,  file.path(dir, "good.R"),  useBytes = TRUE)
  writeLines(other, file.path(dir, "other.R"), useBytes = TRUE)
  dir
}

test_that("C13 : le motif inverse est signale, les motifs legitimes ne le sont pas", {
  dir <- .c13_write_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  bad   <- .c13_find_hits(file.path(dir, "bad.R"))
  good  <- .c13_find_hits(file.path(dir, "good.R"))
  other <- .c13_find_hits(file.path(dir, "other.R"))

  # CAS NEGATIF : sans ceci, « 0 erreur » sur le depot ne prouverait rien.
  expect_gt(length(bad), 0L)
  expect_length(good, 0L)
  expect_length(other, 0L)
})

test_that("C13 : le motif inverse soumet le libelle a input$, pas la valeur", {
  skip_if_not_installed("shiny")
  tr <- function(x) x

  bad <- as.character(shiny::radioButtons(
    "m", "t", choices = c("emap" = tr("Voies voies")), inline = TRUE))
  good <- as.character(shiny::radioButtons(
    "m", "t", choices = stats::setNames(c("emap"), tr("Voies voies")), inline = TRUE))

  # Ce que Shiny ecrit dans input$m : la VALEUR du vecteur nomme.
  expect_match(bad,  'value="Voies voies"', fixed = TRUE)
  expect_match(good, 'value="emap"',        fixed = TRUE)
})

test_that("C13 : les onglets Reseau (bulk + SC) soumettent bien emap / cnet", {
  for (f in c("modules/sc/mod_sc_pathways.R",
              "modules/bulk/mod_bulk_pathways.R")) {
    hits <- .c13_find_hits(file.path(ts_project_root(), f))
    expect_length(hits, 0L)
  }
})
