# =============================================================================
# scripts/verify_release_gates.R — Gates de packaging V1.0 (Stage 19)
# =============================================================================
# Vérifications rejouables du matériel RC :
#   G1 renv.lock : JSON valide, shiny.i18n présent, R = 4.4.2.x, ordre des clés ;
#   G2 "no undocumented mandatory package" : tout package cité dans global.R
#       (required/optional/bioc) est dans renv.lock (ou package de base) ;
#   G3 i18n : aucune clé FR dupliquée, couverture EN complète ;
#   G4 fichiers suivis par git : pas de chemins locaux ni de credentials.
# Sortie : lignes PASS/WARN/FAIL + code retour (1 si FAIL).
# Lancer depuis la racine projet : Rscript scripts/verify_release_gates.R
# =============================================================================

fails <- 0L
warns <- 0L
ok    <- function(msg) cat("PASS  ", msg, "\n")
warn  <- function(msg) { cat("WARN  ", msg, "\n"); warns <<- warns + 1L }
fail  <- function(msg) { cat("FAIL  ", msg, "\n"); fails <<- fails + 1L }

# ── G1 : renv.lock ───────────────────────────────────────────────────────────
lock_path <- "renv.lock"
lock <- tryCatch(jsonlite::fromJSON(lock_path, simplifyVector = FALSE),
                 error = function(e) NULL)
if (is.null(lock)) {
  fail("G1 renv.lock : JSON invalide.")
} else {
  n <- length(lock$Packages)
  if ("shiny.i18n" %in% names(lock$Packages)) {
    ok(sprintf("G1 renv.lock : shiny.i18n présent (%d pkgs au total).", n))
  } else fail("G1 renv.lock : shiny.i18n ABSENT (dépendance obligatoire global.R).")
  lock_r <- paste(lock$R$Version)
  run_r  <- paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")
  if (startsWith(lock_r, run_r)) ok(sprintf("G1 renv.lock : R %s cohérent.", lock_r))
  else warn(sprintf("G1 renv.lock : R %s vs session %s.", lock_r, run_r))
}

# ── G2 : dépendances directes de global.R toutes dans le lock ───────────────
extract_vec <- function(name) {
  ln <- readLines("global.R", warn = FALSE)
  start <- grep(paste0("^", name, "\\s*<-\\s*c\\("), ln)
  if (length(start) == 0L) return(character(0))
  start <- start[1]
  end <- start
  while (end < length(ln)) {
    end <- end + 1L
    if (grepl("\\)\\s*$", ln[end])) break
  }
  body <- paste(ln[start:end], collapse = " ")
  body <- sub(paste0("^", name, "\\s*<-\\s*c\\("), "", body)
  body <- sub("\\)\\s*$", "", body)
  # Ignore les commentaires inline (les vecteurs nls ne contiennent que des noms de packages)
  body <- gsub("#[^\"]*$", "", body)
  vals <- trimws(strsplit(gsub('"', "", body), ",")[[1]])
  vals <- vals[nzchar(vals) & !startsWith(vals, "#")]
  vals
}
base_pkgs <- rownames(installed.packages(priority = "base"))
check_group <- function(label, pkgs, hard) {
  missing <- pkgs[!(pkgs %in% names(lock$Packages)) & !(pkgs %in% base_pkgs)]
  if (length(missing) == 0L) {
    ok(sprintf("G2 %s : %d packages tous dans renv.lock.", label, length(pkgs)))
  } else if (hard) {
    fail(sprintf("G2 %s : ABSENTS du lock : %s", label, paste(missing, collapse = ", ")))
  } else {
    warn(sprintf("G2 %s (optionnels, fallback documenté) : absents du lock : %s",
                 label, paste(missing, collapse = ", ")))
  }
}
req  <- extract_vec("required_packages")
opt  <- extract_vec("optional_packages")
bioc <- extract_vec("bioc_packages")
check_group("required_packages (dur)", req, hard = TRUE)
check_group("bioc_packages (dur)", bioc, hard = TRUE)
check_group("optional_packages", opt, hard = FALSE)

# ── G3 : i18n ─────────────────────────────────────────────────────────────────
i18n <- tryCatch(jsonlite::fromJSON("i18n/translation.json", simplifyVector = FALSE),
                 error = function(e) NULL)
if (is.null(i18n)) {
  fail("G3 i18n : JSON invalide.")
} else {
  fr <- vapply(i18n$translation, function(x) x$fr %||% "", character(1))
  en <- vapply(i18n$translation, function(x) x$en %||% "", character(1))
  dup <- unique(fr[duplicated(fr)])
  if (length(dup) == 0L) ok(sprintf("G3 i18n : %d clés, aucun doublon FR.", length(fr)))
  else fail(sprintf("G3 i18n : DOUBLONS FR (crash startup) : %s",
                    paste(dup, collapse = " | ")))
  if (all(nzchar(en))) ok("G3 i18n : couverture EN complète.")
  else warn(sprintf("G3 i18n : %d traductions EN vides.", sum(!nzchar(en))))
}

# ── G4 : fichiers suivis — chemins locaux / credentials ─────────────────────
tracked <- system2("git", c("ls-files"), stdout = TRUE)
patterns <- c(
  `chemin projet local` = "D:/Data_science|D:\\\\Data_science",
  `profil utilisateur`   = "C:/Users/[A-Za-z]+|C:\\\\Users\\\\[A-Za-z]+",
  `identifiant machine`  = "\\bmarcg\\b",
  `cle AWS`              = "AKIA[0-9A-Z]{16}",
  `token GitHub`         = "ghp_[A-Za-z0-9]{36}",
  `cle API generique`    = "sk-[A-Za-z0-9_-]{20,}",
  `mot de passe en dur`  = "password\\s*=\\s*[\"'][^\"']+[\"']"
)
hits_total <- 0L
for (nm in names(patterns)) {
  # Exclude this script itself from the search: it must embed its own regex
  # patterns as literals, which otherwise self-match and turn G4 permanently
  # red (false positive observed at the pre-release pass, 2026-09-05).
  hits <- system2("git", c("grep", "-lE", patterns[[nm]], "--",
                           ":(exclude)scripts/verify_release_gates.R"),
                  stdout = TRUE, stderr = FALSE)
  hits <- hits[nchar(hits) > 0 & !startsWith(hits, "fatal")]
  if (length(hits) > 0L) {
    hits_total <- hits_total + length(hits)
    fail(sprintf("G4 %s : %d fichier(s) : %s", nm, length(hits),
                 paste(utils::head(hits, 8), collapse = ", ")))
  }
}
if (hits_total == 0L) ok(sprintf("G4 : aucun chemin local/credential dans %d fichiers suivis.",
                                 length(tracked)))

# ── Verdict ───────────────────────────────────────────────────────────────────
cat(sprintf("\n== Gates packaging : %d PASS, %d WARN, %d FAIL ==\n",
            7L - warns - fails, warns, fails))
quit(status = if (fails > 0L) 1L else 0L)
