# =============================================================================
# tools/check_writers.R — une AUTRE session ecrit-elle le meme arbre ?
# =============================================================================
# Contexte (paye le 2026-09-16) : deux sessions Workbuddy ont ecrit le meme
# arbre en parallele. Consequences mesurees : une section de ROADMAP.md
# remplacee en silence, et deux suites de tests lancees en meme temps
# (contention du port Chrome) qui ont produit un faux « 5406 PASS / 2 SKIP »
# la ou l'arbre tranquille donne « 5408 PASS / 1 SKIP ».
#
# Workbuddy n'a PAS de verrou de session. Ce script est le detecteur le moins
# couteux qui ne demande AUCUNE cooperation de l'autre session : il regarde
# ce que l'autre session laisse forcement derriere elle (mtimes, reflog).
#
# Usage :
#   Rscript tools/check_writers.R                 # diagnostic (defaut : 30 min)
#   Rscript tools/check_writers.R --minutes=5     # fenetre plus courte
#   Rscript tools/check_writers.R --register      # (re)pose MON battement
#
# Le battement est OPT-IN : il ne sert que si les DEUX sessions l'appellent.
# Sans hook, la detection repose sur les mtimes et le reflog, qui suffisent.
#
# Sortie : stdout. Code de sortie 0 = aucun ecrivain concurrent, 1 = au moins
# un (utilisable dans un `if`).  Rscript peut sortir en 139 au teardown : lire
# la SORTIE, pas le code (piege connu du depot).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[1]])
}

MINUTES <- suppressWarnings(as.numeric(arg_value("minutes", 30)))
if (is.na(MINUTES) || MINUTES <= 0) MINUTES <- 30
REGISTER <- "--register" %in% args

# Dossiers lourds ou non pertinents : jamais parcourus.
SKIP <- c("renv", "python_env_sccoda", ".git", "node_modules", "QC", ".Rproj.user")

# Racines scannees : le code et la documentation, pas les donnees.
SCAN <- c("R", "modules", "tests", "tools", "config", "i18n", "www",
          "scripts", "mcp.examples", "docs")
ROOT_FILES <- c("app.R", "global.R", "CHANGELOG.md", "AGENTS.md",
                "renv.lock", "CONVENTIONS.md")

HB_DIR <- file.path(".workbuddy-ai", "sessions")
fmt <- function(t) format(t, "%Y-%m-%d %H:%M:%S")

# --- 1. Battement de session (opt-in) ---------------------------------------
self_id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())

if (REGISTER) {
  dir.create(HB_DIR, recursive = TRUE, showWarnings = FALSE)
  writeLines(fmt(Sys.time()), file.path(HB_DIR, paste0(self_id, ".hb")))
  cat("Battement pose : ", file.path(HB_DIR, paste0(self_id, ".hb")), "\n", sep = "")
}

foreign <- character(0)
if (dir.exists(HB_DIR)) {
  hb <- list.files(HB_DIR, pattern = "\\.hb$", full.names = TRUE)
  hb <- hb[basename(hb) != paste0(self_id, ".hb")]
  if (length(hb)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(hb), units = "mins"))
    foreign <- hb[age <= MINUTES]
  }
}

# --- 2. Fichiers modifies recemment (le detecteur principal) -----------------
paths <- character(0)
for (d in SCAN) {
  if (dir.exists(d)) {
    paths <- c(paths, list.files(d, recursive = TRUE, full.names = TRUE))
  }
}
paths <- c(paths, ROOT_FILES[file.exists(ROOT_FILES)])
# `full.names = TRUE` rend le filtre exact (jamais de regex : la racine
# contient des parentheses).
paths <- paths[!vapply(strsplit(paths, "/", fixed = TRUE),
                       function(p) any(p %in% SKIP), logical(1))]

mt <- file.mtime(paths)
recent <- paths[!is.na(mt) & as.numeric(difftime(Sys.time(), mt, units = "mins")) <= MINUTES]
recent <- recent[order(file.mtime(recent), decreasing = TRUE)]

# --- 3. Reflog : ce que l'arbre a enregistre, et quand -----------------------
reflog <- suppressWarnings(
  tryCatch(system2("git", c("reflog", "--date=iso", "-8"),
                   stdout = TRUE, stderr = FALSE), error = function(e) character(0))
)

# --- Rapport ----------------------------------------------------------------
cat("=== check_writers.R — fenetre ", MINUTES, " min — ", fmt(Sys.time()), " ===\n\n", sep = "")

cat("1. BATTEMENTS DE SESSION ETRANGERS\n")
if (!dir.exists(HB_DIR)) {
  cat("   (aucun dossier de battements : mecanisme opt-in non utilise)\n")
} else if (!length(foreign)) {
  cat("   aucun\n")
} else {
  for (f in foreign) cat("   ACTIF : ", basename(f), " (", fmt(file.mtime(f)), ")\n", sep = "")
}

cat("\n2. FICHIERS MODIFIES DANS LES ", MINUTES, " DERNIERES MINUTES : ", length(recent), "\n", sep = "")
if (length(recent)) {
  for (i in seq_len(min(25L, length(recent)))) {
    cat("   ", fmt(file.mtime(recent[[i]])), "  ", recent[[i]], "\n", sep = "")
  }
  if (length(recent) > 25L) cat("   ... et ", length(recent) - 25L, " autre(s)\n", sep = "")
} else {
  cat("   aucun\n")
}

cat("\n3. DERNIERS MOUVEMENTS DE L'ARBRE (reflog)\n")
if (!length(reflog)) {
  cat("   (reflog illisible)\n")
} else {
  for (l in reflog) cat("   ", l, "\n", sep = "")
}

cat("\n--- VERDICT ---\n")
if (length(foreign)) {
  cat("ECRIVAIN CONCURRENT : ", length(foreign),
      " battement(s) etranger(s) actif(s). NE PAS editer de doc partage,\n", sep = "")
  cat("NE PAS lancer de suite de tests, NE PAS committer un etat intermediaire.\n")
  cat("Consequence connue : tout chiffre mesure maintenant est FAUX.\n")
} else if (length(recent)) {
  cat("Aucun battement etranger, mais ", length(recent), " fichier(s) modifie(s) dans la fenetre.\n", sep = "")
  cat("Si vous n'en etes pas l'auteur : une autre session ecrit. Verifier les horaires\n")
  cat("ci-dessus contre votre propre historique avant d'editer un doc partage.\n")
} else {
  cat("Aucun ecrivain concurrent detecte : l'arbre est a vous.\n")
}

invisible(if (length(foreign)) 1L else 0L)
