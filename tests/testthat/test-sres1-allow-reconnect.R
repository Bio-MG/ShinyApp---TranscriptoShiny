# =============================================================================
# test-sres1-allow-reconnect.R -- GARDE S-RES-1 : reconnexion client (O30)
# =============================================================================
# POURQUOI CE FICHIER
#
# Carte section 5 #14 / O30 (DECISIONS.md:107) : S-RES-1 (STATUS.md 2f-bis,
# ROADMAP.md #14) est applicable sans attendre S-RES-2. Propriete visee :
# chaque session Shiny invite le navigateur a TOUJOURS tenter une reconnexion
# (veille PC : WebSocket rompu, processus R vivant).
#
# MECANISME CORRIGE (decision d'escalade du run, 2026-10-31) : la ligne
# enregistree dans STATUS.md:567, shinyOptions(allowReconnect = "force"), est
# INERTE sur shiny 1.14.0 (renv.lock:16212-16213) : AUCUN code de shiny ne lit
# cette option (preuves : deparsage integral de la namespace + generateurs R6 +
# pages d'aide ; ShinySession$initialize n'envoie que config = workerId /
# sessionId / user ; main amont R/shiny.R:99,1423,1427 et tags v1.7.1/v1.8.1
# identiques ; NEWS.md credite session$allowReconnect(TRUE) comme point
# d'entree de la fonctionnalite). Cote client, la reconnexion n'est arme que
# par le message serveur "allowReconnect" (shiny.js:6935-6944) et ne retente
# que si la valeur vaut "force" (shiny.js:6596). Le mecanisme effectif est
# donc session$allowReconnect("force") en PREMIERE instruction de server()
# (cf. ?session : 'If "force", then the client browser will always attempt to
# reconnect'), une ligne, cote serveur, comme prevu par la fiche.
#
# QU'EST ASSERTABLE HEADLESS (honnetete du garde) : le message WebSocket reel
# (private$write(toJSON(list(allowReconnect = value)))) n'est PAS observable
# sans navigateur ; testServer() no-oppe session$allowReconnect avec un
# avertissement (shiny::makeWarnNoops sur MockShinySession). Conformement a la
# convention du depot (test-app-sourcing.R : app.R n'est jamais source dans la
# suite), le garde verifie LEXICALEMENT : (1) l'appel existe, exactement une
# fois, dans le corps de server() ; (2) c'est la premiere instruction de
# server() -- chaque session l'execute au demarrage, avant onStop() et tout
# travail lourd ; (3) la forme inerte shinyOptions(allowReconnect = ...)
# n'est pas reintroduite. Le comportement runtime repose sur la doc shiny
# ?session (cette partie n'est pas re-testee ici).
#
# ASCII pur : un garde ne doit pas dependre de la locale (CONVENTIONS C7).
# =============================================================================

.sres1_lines <- function() {
  readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
}

# Corps de server() : de la ligne "server <- function(input, output, session) {"
# jusqu'a la premiere accolade fermante en colonne 1 (app.R:560 -> app.R:1605,
# mesure 2026-10-31 ; les fermetures imbriquees sont indentes, seul le "}"
# final est en colonne 1). NULL si la definition est absente, non unique ou
# jamais fermee -- jamais un faux "corps vide".
.sres1_server_body <- function(lines) {
  start <- grep("server <- function(input, output, session) {", lines,
                fixed = TRUE)
  if (length(start) != 1L) return(NULL)
  rest <- lines[(start + 1L):length(lines)]
  end_rel <- grep("^}", rest)[1L]
  if (is.na(end_rel)) return(NULL)
  rest[seq_len(end_rel - 1L)]
}

# Instructions effectives du corps : non vides, non commentaire. Le commentaire
# de provenance S-RES-1 est volontairement ignore ici (c'est la ligne d'APPEL
# qui doit etre la premiere instruction executee par chaque session).
.sres1_stmt_lines <- function(body) {
  t <- trimws(body)
  t[nzchar(t) & !startsWith(t, "#")]
}

# --- Le detecteur lui-meme (cas negatif, obligatoire -- convention
# --- test-app-sourcing.R "the detector itself works") ------------------------
test_that("the S-RES-1 detector itself works (negative case)", {
  fake <- c(
    "server <- function(input, output, session) {",
    "  ",
    "  # commentaire",
    "  onStop(f)",
    "  x <- 1",
    "}",
    "shinyApp(ui, server)"
  )
  body <- .sres1_server_body(fake)
  expect_false(is.null(body))
  expect_identical(body, fake[2:5])
  expect_identical(.sres1_stmt_lines(body), c("onStop(f)", "x <- 1"))
  # jamais fermee au top-level => NULL (pas de faux corps vide)
  expect_null(.sres1_server_body(c(
    "server <- function(input, output, session) {", "  x <- 1")))
  # definition absente => NULL
  expect_null(.sres1_server_body(character(0)))
})

# --- Le garde S-RES-1 ---------------------------------------------------------
test_that("S-RES-1: server() arms reconnect via session$allowReconnect force, first statement", {
  body <- .sres1_server_body(.sres1_lines())
  expect_false(is.null(body),
               info = paste("definition server() introuvable, non unique ou",
                            "jamais fermee dans app.R -- garde S-RES-1 non",
                            "evaluable"))
  hits <- grep('session\\$allowReconnect\\(\\s*"force"\\s*\\)', body)
  expect_identical(
    length(hits), 1L,
    info = paste('session$allowReconnect("force") doit apparaitre exactement',
                 "une fois dans server() (chaque session doit armer la",
                 "reconnexion au demarrage) -- appels observes :",
                 length(hits)))
  expect_true(
    startsWith(trimws(.sres1_stmt_lines(body)[1L]),
               'session$allowReconnect("force")'),
    info = paste("la premiere instruction de server() doit etre",
                 'session$allowReconnect("force") (avant onStop() et tout',
                 "travail lourd, sinon la reconnexion n'est pas armee au",
                 "demarrage de session) -- trouve en tete :",
                 shQuote(.sres1_stmt_lines(body)[1L])))
  expect_true(
    any(grepl("S-RES-1", body)),
    info = paste("commentaire de provenance S-RES-1 (section 5 #14 / O30)",
                 "manquant dans server()"))
})

# --- Anti-regression : la forme inerte ne doit pas revenir --------------------
test_that("S-RES-1: the inert shinyOptions(allowReconnect=...) form is not reintroduced", {
  # CODE seul : les commentaires sont exclus (le commentaire de provenance
  # app.R cite justement la forme inerte pour documenter la correction) ;
  # les lignes code sont collees par "\n" pour que \\s* detecte aussi un
  # appel eclate sur deux lignes.
  t <- trimws(.sres1_lines())
  code <- paste(t[nzchar(t) & !startsWith(t, "#")], collapse = "\n")
  expect_false(
    grepl("shinyOptions\\(\\s*allowReconnect", code),
    info = paste("shinyOptions(allowReconnect = ...) est INERTE sur shiny",
                 "1.14.0 (renv.lock:16212) : aucun code de shiny ne lit cette",
                 "option (verification 2026-10-31 : namespace, generateurs R6,",
                 "pages d'aide, main amont, v1.7.1/v1.8.1). Ne pas reintroduire",
                 "la forme enregistree STATUS.md:567 -- le mecanisme effectif",
                 'est session$allowReconnect("force").'))
})
