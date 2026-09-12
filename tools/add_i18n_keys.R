# tools/add_i18n_keys.R — append {fr, en} entries to i18n/translation.json
# ONLY for keys not already present (duplicate FR keys crash app startup —
# guarded by test-i18n-integrity.R). Idempotent.
# Usage: Rscript tools/add_i18n_keys.R
json_path <- file.path(getwd(), "i18n", "translation.json")
j <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

# key (fr) -> en
new_entries <- list(
  c("Scores par \u00e9chantillon (GSVA / ssGSEA)",
    "Per-sample scores (GSVA / ssGSEA)"),
  c("Attribue \u00e0 chaque \u00e9chantillon un score par voie \u2014 PCA et heatmaps par voie m\u00eame sans contraste. Exige la matrice VST (\u00e9tape 1) et un fichier .gmt (nom<TAB>description<TAB>g\u00e8nes...). Les jeux dont moins de 20 % des g\u00e8nes sont retrouv\u00e9s sont rejet\u00e9s (d\u00e9calage d'identifiants).",
    "Assigns each sample a per-pathway score — PCA and per-pathway heatmaps even without any contrast. Requires the VST matrix (step 1) and a .gmt file (name<TAB>description<TAB>genes...). Gene sets with less than 20% of their genes found are rejected (identifier mismatch)."),
  c("M\u00e9thode de scoring", "Scoring method"),
  c("Fichier de jeux de g\u00e8nes (.gmt)", "Gene sets file (.gmt)"),
  c("Taille min voie", "Min pathway size"),
  c("Taille max voie", "Max pathway size"),
  c("Lancer Scores par \u00e9chantillon", "Run per-sample scores"),
  c("En attente \u2014 lancez d'abord le Filtrage & VST (\u00e9tape 1).",
    "Waiting \u2014 run Filtering & VST (step 1) first."),
  c("\u2713 {n} voies scor\u00e9es x {m} \u00e9chantillons [ {meth} ]",
    "\u2713 {n} pathways scored x {m} samples [ {meth} ]"),
  c("Scores de voies par \u00e9chantillon...", "Per-sample pathway scoring..."),
  c("\u2713 {n} voies scor\u00e9es (ssGSEA/GSVA) sur {m} \u00e9chantillons.",
    "\u2713 {n} pathways scored (ssGSEA/GSVA) on {m} samples."),
  c("\u26a0\ufe0f Fournissez un fichier .gmt (jeux de g\u00e8nes).",
    "\u26a0\ufe0f Please provide a .gmt file (gene sets)."),
  c("Erreur GMT:", "GMT error:"),
  c("Erreur scores:", "Scores error:"),
  c("\u26a0\ufe0f {n} jeu(x) de g\u00e8nes rejet\u00e9(s) \u2014 voir le d\u00e9tail dans l'onglet Scores.",
    "\u26a0\ufe0f {n} gene set(s) rejected \u2014 see details in the Scores tab."),
  c("{n} jeu(x) rejet\u00e9(s) \u2014 recouvrement < {pct} % ou taille hors bornes. D\u00e9tail :",
    "{n} rejected set(s) \u2014 overlap < {pct}% or size out of bounds. Details:"),
  c("Colorer la PCA par", "Color PCA by"),
  c("Voies affich\u00e9es (heatmap)", "Pathways shown (heatmap)"),
  c("Export CSV (scores)", "CSV export (scores)"),
  c("Export RDS (r\u00e9sultat complet)", "RDS export (full result)"),
  c("Scores par \u00e9chantillon", "Per-sample scores"),
  c("PCA des scores de voies (par \u00e9chantillon)", "Pathway-scores PCA (per sample)"),
  c("M\u00e9thode : %s \u2014 %d voies", "Method: %s \u2014 %d pathways"),
  c("Erreur:", "Error:"),
  c("Score", "Score"),
  c("Scores de voies", "Pathway scores"),
  c("Fournissez un fichier .gmt (jeux de g\u00e8nes).", "Provide a .gmt file (gene sets).")
)

fr_existing <- vapply(j$translation, function(x) x$fr %||% "", character(1))
added <- 0L
for (e in new_entries) {
  if (!e[1] %in% fr_existing) {
    j$translation <- c(j$translation, list(list(fr = e[1], en = e[2])))
    fr_existing <- c(fr_existing, e[1])
    added <- added + 1L
  }
}
jsonlite::write_json(j, json_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("entries added:", added, "| total:", length(j$translation), "\n")

# Post-check: no duplicate FR keys, all en non-empty
fr <- vapply(j$translation, function(x) x$fr %||% "", character(1))
en <- vapply(j$translation, function(x) x$en %||% "", character(1))
stopifnot(length(unique(fr)) == length(fr), all(nzchar(en)))
cat("integrity OK (no duplicates, all en non-empty)\n")
