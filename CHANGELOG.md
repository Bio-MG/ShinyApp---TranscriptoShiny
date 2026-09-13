# Changelog

Tous les changements notables de TranscriptoShiny (« Cerberus ») sont documentés ici.
Format inspiré de [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/) ;
versionnement [SemVer](https://semver.org/lang/fr/). Une étape = un commit sur `main`.

> ℹ️ **Trou de maintenance assumé** : entre `V1.x-D` (2026-09-06) et l'entrée
> ci-dessous, plusieurs jalons ont été livrés **sans entrée de changelog**
> (PLOT-Q1..Q5, PLOT-S1..S5, Bulk V2 / batch-QC, correctif Milo, STAT-Q1..Q4).
> Leur état fait foi dans **`docs/STATUS.md`** §1 et §2. Ce fichier reprend à
> partir de `STAT-S1`.

## [V1.x — CCC 7–8 route (b)] — 2026-09-13 — Import de rangs LIANA

Premier jalon d'**interopération communication cellule–cellule** (`a88577f`).
**Aucune dépendance nouvelle, `renv.lock` intouché**, aucun calcul d'inférence
dans l'application : la route (b) importe une table **agrégée produite par
LIANA hors de l'app**.

### Ajouté
- **`parse_liana_import(tab, rank_column, aggregation_mode, source_file)`** —
  convertit une table agrégée LIANA vers la table canonique de communication.
  Ni le mode d'agrégation ni la colonne de rang ne sont déduits du fichier :
  les deux sont **déclarés explicitement** (aucun défaut implicite).
- **`communication_rank_fields()`** = `rank`, `rank_direction`,
  `rank_aggregation_mode` ; **`communication_rank_aggregation_modes()`** =
  `specificity`, `magnitude`. Surface **séparée** des 12 champs contractuels.
- **`"liana"`** ajouté à `communication_supported_sources()`.
- **`provenance$is_external_consensus`** — TRUE quand la table porte un agrégat
  **inter-méthodes** calculé par LIANA (`mean_rank`, `aggregate_rank`).
- **QC des rangs** : `n_rank_out_of_range`, `n_rank_missing` (0 pour les sources
  sans rang).
- **UI** : 4ᵉ route dans le sélecteur de source existant + panneau conditionnel
  (mode d'agrégation **sans sélection par défaut**, colonne de rang proposée
  depuis l'en-tête du fichier). **+8** clés i18n (2367 → **2375**).
- `tests/testthat/test-sc-communication-liana.R` — **73** assertions.

### Modifié
- `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md` — §1, §2, §4, §5, §6, §9,
  §10 : source `liana`, champs de mesure de rang, nuance « consensus importé ≠
  consensus calculé », évolution **additive** documentée. **Même commit** que le
  code et les tests de gel (contract-first).
- `tests/testthat/test-communication-contract-freeze.R` — gels étendus +
  `parse_liana_` ajouté au ban des consommateurs rapport.

### Notes
- **`aggregate_rank` → `p_value`** : c'est une p-value (*Robust Rank
  Aggregation*, `min(p) × k`), pas un score de communication.
- **`score` reste `NA`** sur cette route : un rang n'est pas un score (échelle
  **et** direction différentes).
- **`rank_direction = "lower_is_better"`** : dans LIANA, rang 1 = meilleur —
  c'est l'**inverse** de `prob` (CellChat).
- **Les colonnes `.complex` ne sont jamais découpées** : un complexe n'a pas de
  découpage univoque, le deviner serait inventer une donnée.
- **Non-régression** : CellChat et CellPhoneDB **ne gagnent aucune colonne de
  rang** et leur résultat est inchangé (test dédié) — les parseurs existants
  n'ont pas été touchés.
- **Risque connu, non traité** : les vues exploratoires supposent un score
  orienté « plus grand = meilleur » ; sur une source de rangs, échelles et
  filtre « score minimum » peuvent être inversés. À traiter **vue par vue**.

## [V1.x — Maintenance] — 2026-09-13 — Conventions de code, i18n, versionnage

Passe transversale **sans changement de comportement** : aucune méthode
statistique, aucun paramètre par défaut, aucun tracé modifié.

### Ajouté
- **`docs/CONVENTIONS.md`** — une seule source de vérité pour les conventions
  de code : arborescence et responsabilités, `R/` pur (avec l'exception
  assumée de la couche d'état), nommage, contract-first, erreurs classées,
  i18n, paramètres déclarés, async/cache/provenance, tests. Chaque règle porte
  un identifiant **C1..C12**.
- **`tools/check_conventions.R`** — garde statique en **base R uniquement**
  (même esprit que `check_duplication.R`, exécutable sans installation) :
  vérifie C1..C12. État mesuré : **0 erreur**, **324 avertissements** de dette
  (C6 = 16 `library()` au top-level de `R/`, C9 = 37 fichiers sans test
  éponyme, C10 = 270 `stop()` non classés, C11 = 1 `MulticoreParam` sous garde
  Unix). Exit 0 = vert ; `--strict` fait échouer sur la dette.

### Corrigé
- **🐛 Un fichier sourcé n'avait jamais été commité.** `R/plotting/complex_heatmap.R`
  (cœur de PLOT-S4, marqué livré) était absent de `HEAD` et exclu par le
  `.gitignore` local, alors que `app.R:64` le source : **l'application ne
  démarrait que sur ce poste**. Fichier sorti du `.gitignore` et **commité
  pour la première fois** ; la règle **C3** interdit toute récidive (toute
  cible de `source()` doit exister **et** être versionnée).
- **Dette i18n soldée** : **101 clés** utilisées par `tr()` / `i18n$t()` mais
  absentes de `i18n/translation.json` ont été ajoutées avec leur traduction
  anglaise via `tools/add_i18n_keys.R` (idempotent). Total **2367 clés**,
  **0 clé manquante** (C7), aucune entrée `en` vide — un utilisateur en
  anglais ne retombe plus sur du français non traduit.

### Documentation
- `docs/ROADMAP.md` — nouveau **§2.0 « ordre d'actionnabilité »** daté
  (arbitrage utilisateur 2026-09-13 : CCC 7–8 → CCC 9 → STAT-S2/S3 →
  NEW-1..3 → UX → 4E-4), contrats **24** (au lieu de 13/18), flux B aligné sur
  le parking tranché, décisions 4/5/8 mises à jour, **décisions 9 et 10**
  ajoutées (route CCC 7–8 ; dette de conventions).
- `docs/ROADMAP_HANDOFF_NEXT.md` — **ré-écrit** : le prompt MD-1 (consommé)
  est retiré, l'étape courante devient **CCC 7–8 route (b)** (import de
  résultats LIANA sans dépendance) avec ancres re-vérifiées dans le dépôt.
- `docs/STATUS.md` — §2g corrigé (le « double jeu SC » n'était plus « non
  démarré » : **MD-4** l'a livré), §2e parking CCC tranché, **§2z** = cette
  passe.
- `AGENTS.md` — règles dures **10** (conventions = doc + garde) et **11**
  (jamais gitignorer un fichier sourcé).

> Rappel : les jalons `MD-1`→`MD-4`, `4F-EXT`, `PLOT-S1..S5`, Bulk V2 M2–M5
> n'ont **pas** d'entrée propre ici — leur état fait foi dans `docs/STATUS.md`.

## [V1.x — STAT-S1] — 2026-09-12 — Correction de batch ComBat-seq

Retrait d'un **effet de lot technique** avant l'analyse différentielle Bulk,
par **ComBat-seq** (`sva`) plutôt que le ComBat classique : il agit sur les
**comptages bruts** (modèle binomial négatif) et son paramètre `group=`
**protège la condition biologique** — un ComBat sur matrice transformée peut
l'effacer. Contrat gelé `docs/contracts/BATCH_CORRECTION_CONTRACT.md`
(freeze : `test-bulk-batch-correction-contract-freeze.R`).

### Ajouté
- **Noyau pur** `R/bulk/batch_correction.R` :
  `bulk_batch_correction_public_api()` (inventaire gelé) ;
  `bulk_assert_raw_counts()` — **miroir exact** de
  `bulk_assert_transformed_matrix()` (`bulk_batch_qc.R`) : **même seuil de
  0,95** de fraction entière, appliqué **dans le sens inverse** (ce que l'une
  accepte, l'autre le refuse) ; `bulk_batch_correction_design()` — réutilise
  `bulk_batch_design_check()` (cross-table, **colinéarité lot/condition**) et
  décide `can_apply` / `use_group` ; `bulk_batch_correction_label()` —
  provenance **préfixée** (`"<normalisation> + ComBat-seq (batch : X ; groupe :
  Y)"`), jamais écrasée ; `run_combat_seq()` — enveloppe **paresseuse**
  (`requireNamespace("sva")` à l'appel, **jamais** au `source`) ; 
  `plot_batch_correction_pca()` — compose **deux** tracés `plot_bulk_pca()` via
  `patchwork` (`tr` **en dernier**, piège de signature PLOT-S1/S2).
- **5 états d'erreur classés gelés** (`bulk_batch_correction_error`) :
  `invalid_input`, `not_raw_counts`, `degenerate_batch`, `missing_dependency`,
  `compute_failed` — le freeze test **compte** les `state = "…"` du source pour
  interdire l'inflation silencieuse de cette surface.
- **UI** — section repliable « Correction de batch (optionnel) — ComBat-seq »
  dans l'onglet **QC Batch** de `mod_bulk_filter.R` : sélection de la colonne de
  lot, choix de la condition à préserver, diagnostic PCA **avant / après**. La
  copie « pristine » des comptages vit **dans le module** (`bc_pristine`) — 
  **aucune clé de `shared_rv` ajoutée** ; le pipeline ne change **que sur clic
  explicite** (idempotent), et les contrastes déjà calculés sont invalidés.
- **Dépendance** : `sva` ajouté à **`bioc_packages`** — ⚠️ **pas** à
  `required_packages` : l'application **démarre sans `sva`**, la fonctionnalité
  échoue alors proprement en erreur classée `missing_dependency` (message FR
  avec le remède).
- **Seuil déclaré** `config/thresholds.R` :
  `TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH <- 2L`.
- **i18n** : **+15** clés `{fr, en}` (2084 → 2099, 0 doublon).
- Tests : `test-bulk-batch-correction.R` (**57** assertions) + freeze test
  dédié (**69** assertions) → **126 PASS / 0 FAIL / 0 ERROR**.

### Mesuré
- **Critère d'acceptation quantifié** (la fiche proposait une lecture visuelle,
  non testable) : sur comptages binomials négatifs sur-dispersés à **effet de
  lot multiplicatif propre à chaque gène**, le **R² du lot sur PC1** passe de
  **0,74 → 0,23** (< 50 % de l'initial) et l'**écart entre conditions est
  préservé** (> 70 %). `dimnames` conservés.
- Porte de duplication : **0 erreur / 3 avertissements** (baseline).
- `app.R` se source de bout en bout (UI + server construits) — vérifié.

### Limite connue (gelée au contrat §11.1)
- Un **décalage additif uniforme sur tous les gènes n'est PAS corrigé** :
  ComBat-seq le lit comme un **effet de profondeur de séquençage** et l'absorbe
  par son offset. C'est statistiquement correct, mais c'est un **piège de
  fixture** — un test fige ce comportement pour qu'il ne soit pas « corrigé »
  par erreur.

### Notes
- ⚠️ Le contrat vit sous `docs/`, **gitignoré** : le freeze test lit donc
  l'arbre, pas git. Sur un **clone neuf**, ce test échoue (contrat absent) alors
  que le code est intact — voir la décision 4 de `docs/ROADMAP.md` §5.
- ⚠️ `sva` est installé mais **pas encore snapshoté dans `renv.lock`** (snapshot
  différé, règle du dépôt) — dégradation propre si absent.

## [V1.x-D] — 2026-09-06 — Perturbation IN SILICO (roadmap CCC avancée, Phase 4)

Simulation de perturbation sur le **réseau INFÉRÉ** importé (suppression /
atténuation d'un ligand, récepteur, interaction ligand->récepteur ou
population) avec quantification des effets de **premier ordre** sur les
paires et les nœuds, à partir des scores IMPORTÉS — jamais recalculés, aucune
propagation réseau modélisée (anti-feature-creep). Contrat
`docs/contracts/COMMUNICATION_PERTURBATION_CONTRACT.md` (freeze :
`test-communication-perturbation-contract-freeze.R`).

### Ajouté
- **Noyau pur** `R/sc/sc_communication_perturbation.R` :
  `build_communication_perturbation()` (cibles
  `communication_perturbation_targets()` = ligand / receptor / interaction /
  sender / receiver ; valeur EXACTE issue de la table — absente = erreur FR
  classée avec liste des valeurs disponibles ; mode `remove` ou `attenuate`
  avec facteur strictement (0,1)) ; deltas par paire (score_total,
  delta_fraction, affected) et par nœud (sortant + entrant) ; résumés
  baseline/perturbé (NA sans scores — jamais 0 fabriqué) ; la table
  canonique n'est JAMAIS modifiée (copie d'affichage).
- **GARDE ABSOLU** : étiquette « IN SILICO PERTURBATION » portée par le
  résultat (`params$label`, provenance `label = "in_silico_not_ko"`), les
  avertissements, le sous-titre de chaque figure et une colonne de chaque
  export — jamais « KO », « effet biologique » ni « causal ». Effets de
  PREMIER ORDRE explicitement énoncés (NETWORK PERTURBATION ≠ DOWNSTREAM
  TRANSCRIPTIONAL SIMULATION, jamais conflation).
- **Vues** : `plot_communication_perturbation_delta()` (barres divergentes
  top N), `plot_communication_perturbation_nodes()` (baseline vs perturbé
  par nœud) ; `build_communication_perturbation_export()` (traçabilité +
  étiquette par ligne).
- **Module** `modules/sc/mod_sc_communication_perturbation.R` — onglet
  « Perturbation (in silico) » dans le panneau Communication existant,
  bandeau permanent, choix de valeur synchronisé avec la table (filtres du
  panneau honorés), calcul sur bouton explicite, péremption vérifiée,
  CSV/PNG/PDF tracés.
- Tests : `test-sc-communication-perturbation.R` (72 assertions) + freeze
  test dédié.

### Corrigé
- `app.R` : lignes `source()` V1.x-A..D réparées (des insertions par
  sous-chaîne avaient concaténé le commentaire de la ligne précédente sur
  chaque nouvelle ligne et dupliqué trajectory/velocity ; commentaires un
  par ligne, doublons supprimés, lignes perturbation restaurées — commit
  `62510b9`).

### Parking (reste de la roadmap CCC avancée)
Phases 5-6 (ligand→target / NicheNet-like — exigent un réseau prior et une
proposition 4D-3 avec le contrat d'entrée de l'app upstream), Phase 7-8
(OmniPath / LIANA — packages nouveaux, renv.lock justifié ; import de
RÉSULTATS LIANA externes possible en extension du contrat Stage 11 avec
ajout simultané code+freeze+doc), Phase 9 (rare-cell — auditer le
chevauchement Milo), Phase 10 (gallery — règle « aucune dépendance graph
nouvelle ») — voir `docs/ROADMAP_CCC_ADVANCED.md`.

## [V1.x-A/B/C] — 2026-09-06 — Contextes CCC (roadmap CCC avancée, Phases 1-3)

Extension de la Communication cellule-cellule par trois **contextes dérivés**
(consommateurs purs du résultat canonique Stage 11/12 — table gelée intacte,
aucun score recalculé, aucune inférence) : roadmap `docs/ROADMAP_CCC_ADVANCED.md`,
mandat utilisateur « go on, implement phases ». Trois onglets DANS le panneau
Communication existant (aucun nouveau panneau latéral) ; erreurs classées
`communication_context_error` ; provenance PRODUITE au calcul avec
`parent_analysis_id` (chaîne CCC → contexte traçable) ; WIP utilisateur
préservé (staging partiel de app.R).

### Ajouté
- **Contexte spatial (V1.x-A, `1d5d0ab`)** : `R/sc/sc_communication_spatial.R`
  — distances de centroïdes + NN croisées (chunks mémoire bornés) par paire,
  fractions à portée d'un rayon EXPLICITE (jamais inféré), enrichissement par
  permutation OPT-IN (seed fixée, null = permutation des étiquettes) ;
  source de coordonnées DÉCLARÉE (bundle spatial courant ou réduction 2D —
  « distances de projection, NON physiques ») ; la distance = CONTRAINTE
  spatiale, jamais une preuve de communication. Contrat
  `COMMUNICATION_SPATIAL_CONTRACT.md` + freeze test dédié.
- **Contexte trajectoire (V1.x-B, `da84e28`)** :
  `R/sc/sc_communication_trajectory.R` — composition des populations et
  expression ligands/récepteurs le long du pseudo-temps par bins quantiles
  (dégradation comptabilisée, jamais forcée) ; mode PAR LIGNÉE slingshot
  (jamais de collapse) ; gènes absents / cellules sans pseudo-temps comptés
  jamais imputés ; score importé CONSTANT par paire, jamais décliné par bin ;
  `communication_fetch_expression_matrix()` (extraction « data » bornée aux
  gènes, branche layer/slot explicite). Contrat
  `COMMUNICATION_TRAJECTORY_CONTRACT.md` + freeze test dédié.
- **Contexte vélocité (V1.x-C, `4903829`)** :
  `R/sc/sc_communication_velocity.R` — magnitude L2 des vecteurs de vitesse
  PRÉCALCULÉS importés (contrat Stage 10 consommé, jamais ré-inféré) par
  population et paire ; états gracieux `unavailable_no_vectors` /
  `insufficient_overlap` (aucun vecteur substitué, aucune fabrication) ;
  seuil `TS_VELOCITY_OVERLAP_MIN` consommé du config/thresholds.R.
  Contrat `COMMUNICATION_VELOCITY_CONTRACT.md` + freeze test dédié.
- **Modules d'orchestration** : `mod_sc_communication_{spatial,trajectory,
  velocity}.R` montés par le module Communication — calcul sur bouton
  explicite, péremption vérifiée avant rendu/export, CSV/PNG/PDF tracés
  (`analysis_id` + `parent_analysis_id` + timestamp par ligne).
- Tests : `test-sc-communication-{spatial,trajectory,velocity}.R` +
  3 freeze tests + fixtures partagées
  (`helper-communication-context-fixtures.R`).

### Parking (Phases 4-10 de la roadmap)
Perturbation in silico, Ligand→receptor→target, NicheNet-like, OmniPath,
LIANA, rare-cell, gallery — requièrent 4D-3 (contrat d'entrée app upstream
non gelé), des dépendances nouvelles (renv.lock justifié) ou une proposition
scientifique dédiée ; voir `docs/ROADMAP_CCC_ADVANCED.md` §4.

## [V1.1.0-rc] — 2026-09-05 — Import .rda/.RData « Inspect & Select »

Support des fichiers `.rda`/`.RData` (workspaces `save.image()`, listes
mixtes) selon le paradigme **« Inspecter d'abord, importer ensuite »** :
contrat `docs/contracts/RDATA_IMPORT_CONTRACT.md` (gel :
`test-rdata-contract-freeze.R`). Zéro changement sur les chemins `.rds`,
`.h5`, `.h5ad`, `.loom` existants.

### Ajouté
- **Noyau pur** `R/core/rdata_io.R` : `rdata_load_env()` (environnement
  isolé `parent = emptyenv()`, jamais globalenv), `rdata_describe_objects()`
  (Nom / Classe / Dimensions / Taille / Type), `rdata_classify_object()`
  (codes gelés, AFFICHAGE UNIQUEMENT — jamais d'auto-guess),
  `rdata_extract_object()`, `rdata_assert_class()` (validation AVANT écriture
  dans `global_data`), `rdata_export_selection()`, `rdata_free()`.
- **Composant Shiny mutualisé** `modules/import/mod_rdata_picker.R` :
  carte de preview (DT multi-sélection) avec « Importer l'objet sélectionné »
  (exactement 1 ligne) et « Exporter la sélection (.RData) » — bundle .RData
  téléchargé via le navigateur, SANS écriture dans `global_data`.
- **Exploration imbriquée + .rds** : les listes nommées sont APLATIES en
  chemins explorables (`objet$enfant$...`, profondeur 3 — cas type
  `data_humanSkin$data$NL` d'un workspace CellChat tutorial) ; les `.rds`
  (même contenant une liste) sont explorés comme les `.rda` ;
  « Exporter l'objet sélectionné (.rds) » sauvegarde n'importe quelle
  feuille en monofile `.rds` (remplace le workflow manuel
  `load()` → `CreateSeuratObject` → `saveRDS`) ; `rdata_flatten_env()`,
  `rdata_extract_path()`, `rdata_read_file_env()`, `rdata_export_paths()`.
- **Import Single-Cell** : Options B/C acceptent `.rda`/`.RData` ; objet
  unique compatible → auto-import via `prepare_seurat_object()` ; workspace
  multi-objets → carte de preview (choisir 1 objet à importer, ou en exporter
  plusieurs vers un .RData).
- **Import Bulk** : `.rda` accepté pour les slots comptages ET métadonnées
  (2 instances du composant, libellés « Utiliser comme matrice de
  comptages » / « Utiliser comme métadonnées » ; transposition gérée) ;
  un même workspace peut alimenter les deux slots.
- **Référence spatiale** (`read_reference_scrna`) : `.rda` objet unique
  (Seurat / liste counts+meta / matrice) ; multi-objets → erreur orientante.
- **Communication / Vélocité** : `parse_cellchat_object()` et
  `read_velocity_rds()` acceptent un workspace `.rda` à objet unique
  (multi-objets → erreur orientante vers l'aperçu SC).
- **Snapshot de session** (app.R) : `.rda` accepté (objet unique = snapshot).
- **Config** : `TS_IMPORT_RDA_EXTENSIONS`, `TS_IMPORT_RDA_WARN_MB` (500 —
  avertissement mémoire, jamais un blocage). **i18n** : 24 clés FR/EN.
- **Tests** : `test-core-rdata.R`, `test-rdata-contract-freeze.R`,
  `test-rdata-picker-module.R` (testServer), `test-rda-spatial-bulk.R`,
  `test-rda-comm-velocity.R`.

## [V1.1.0-rc] — 2026-09-05 (vague UX/UI, mandat utilisateur — candidat pré-release)

Refonte UX/UI fondée sur l'audit utilisateur (6 frictions, see
`docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md`) : **zéro changement de
comportement scientifique**, contrats figés intouchés, IDs de modules
inchangés, migration par lots reversibles (un lot = un commit).

### Ajouté
- **Paradigme pipeline mutualisé** : le pipeline auto (1 clic) est le
  **premier panneau de l'accordéon (0.)** dans les trois domaines (SC, Bulk,
  Spatial), avec badge de paradigme en tête de chaque sidebar (« auto dispo » /
  « mode guidé » / « auto async »).
- **Bulk** : étape « 1. Pipeline Bulk — Contrôle qualité & filtrage » ;
  vue « Résumé Pipeline Bulk » (champs `shared_rv` existants uniquement,
  aucun nouveau calcul) ; dernier panneau = « 4. Livrables — Rapport & Script R ».
- **Spatial** : conteneur standard `layout_sidebar` — étapes numérotées en
  accordéon à gauche (une étape ouverte à la fois), résultats à droite ;
  dataset + statut des daemons toujours visibles dans la sidebar ; le module
  pipeline est scindé (contrôles dans l'accordéon, résumé dans le navset
  droit, même namespace → serveur inchangé).
- **Import** : rappel « mapping des IDs » + bouton natif « Aller au mapping
  des IDs » dans Import Single-Cell/Bulk/GEO (saut via `page_navbar(id="main_nav")`
  + `accordion_panel_open` ; aucun panneau déplacé, aucune UI dupliquée).
- **GEO** : renommé « Source publique (GEO) » (source de données, pas une
  4e modalité), phrase d'orientation ; placé en DERNIER du menu Import
  (préférence utilisateur du 2026-09-05).
- **i18n** : 36 clés FR/EN (glossaire double libellé : Pseudobulk, Vélocité
  ARN, Niches spatiales… + tooltips Moran/Milo/scCODA). Clés figées 8b→9b
  conservées (gate d'intégrité vert).
- **Test fonctionnel** `test-sc-auto-pipeline.R` : run complet de
  `run_sc_auto_pipeline` sur fixture minima (QC → PCA → clustering → UMAP →
  t-SNE → marqueurs → trajectoire → commit) + chemin d'échec gracieux.

### Modifié
- **SC** : accordéon plat (17 panneaux) regroupé en 5 sections parent
  (Préparation / Analyse / Dynamique / Abondance cellulaire / Livrables) ;
  les 4 panneaux DA 8c–8f sont nidifiés en un panneau « Abondance
  différentielle » à onglets internes (A. design — B. Milo/scCODA —
  C. vues croisées ; gating inchangé). Valeurs de panneaux préservées.

### Corrigé
- **Crash Spatial** : le binding accordéon bslib retourne un **vecteur de tous
  les panneaux ouverts** (`multiple=TRUE` par défaut) — l'observer de sync
  crashait par indexation récursive dès l'ouverture d'un 2e panneau.
  Correctif : `multiple = FALSE` (une étape à la fois, comme l'ancien navset
  horizontal) + défense `tail(1)`.
- **Gate G4** (`scripts/verify_release_gates.R`) : faux positif permanent
  depuis le Stage 19 (le script se matchait lui-même sur ses propres regex de
  chemins locaux) ; exclusion de soi via pathspec git.

### Vérification (pré-release)
- Suite complète : **1873 assertions PASS / 0 FAIL / 0 ERROR / 0 SKIP**
  (référence V1.0 : 1858 + 15 du nouveau test fonctionnel).
- Duplication : 0 erreur / 3 warnings pré-existants. Boot headless HTTP 200.
- Gates packaging : 6 PASS / 1 WARN / 0 FAIL. e2e shinytest2 : 13 PASS.

## [Post-V1.0] — 2026-09-04 (polish mandat utilisateur — revue des manquants)

Revue des fonctionnalités manquantes demandée après la V1.0 : les rapports ne
restitution pas les domaines ajoutés depuis leur création.

### Rapport Rmd Single-Cell (panneau 9)
- **Nouvelles sections** (tables pures, restituées du résultat canonique tel
  quel — aucune re-exécution, aucun recalcul de figure) : **Vitesse ARN**
  (statut, dimensions alignées, analysis_id), **Communication cellulaire**
  (méthode source, compteurs d'import, table canonique en extrait), **DA**
  (design expérimental + éligibilités, Milo — voisinages avec disclaimer
  « niveau voisinage », scCODA — effets crédibles avec disclaimer « pas des
  p-values »). Opt-in via les cases « Sections » du panneau 9 (aucun changement
  des rapports par défaut) ; paramètres canoniques exposés depuis
  `shared_rv` (expositions Stage 17 + pseudobulk).

### Rapport consolidé 4F (panneau 9b)
- **Domaines ajoutés au contrat (9 → 11)** : `pseudobulk` (exposition additive
  `shared_rv$pseudobulk_result` du panneau 4b — moteur, contraste, table DE)
  et `correlation` (gène cible + table de corrélations, déjà dans l'état
  partagé). Bundle : `tables/correlation.csv`, `tables/pseudobulk_de.csv`.
  Contrat + test de freeze + tests fonctionnels mis à jour simultanément.
- i18n : 10 clés FR/EN ajoutées (libellés de sections + disclaimers
  scientifiques).

### Restants (parking V1.x, décision requise)
- Rapports Rmd Bulk/Spatial : périmètre déjà complet par domaine (contrôlé).
- Compilation 4F des états Bulk/Spatial : proposition (extension contrat).
- 4D-3 / 4E-4 : inchangés (voir `UPGRADE_AND_COMPATIBILITY.md` §3).

## [V1.0.0] — 2026-09-04 (Stage 20)

Déclaration V1.0 : toutes les exigences de la roadmap vérifiées et prouvées
(`docs/release/V1_0_DECLARATION.md`) — contrats figés, provenance active,
rapports compilés, exports traçables, limitations documentées, gates RC passées.

### Vérification finale
- Suite complète : **1837 PASS / 0 FAIL / 0 ERROR / 0 SKIP / 16 warnings bénins**.
- Gates packaging : 6 PASS / 1 WARN documenté / 0 FAIL ; duplication 0 erreur ;
  lancement headless HTTP 200.
- Tags : `v1.0.0-rc.1` → `v1.0.0` (locaux ; push après confirmation).

## [V1.0.0-rc.1] — 2026-09-04 (Release Candidate, Stage 19)

Première version formelle candidate à la V1.0. Macro-fonctionnalités livres :
plateforme multi-omique 3 domaines (Single-Cell, Bulk RNA, Spatial), vitesse ARN
(3B), communication cellulaire (4D), abondance différentielle (4E), rapport
consolidé + reproductibilité (4F), durcissement release.

### Ajouts — Analyse Single-Cell
- **Vitesse ARN (Stages 8–10)** : import mtx/rds validé, 9 états de validité,
  empreinte d'objet v2, contrat résultat figé (`VELOCITY_RESULT_CONTRACT.md`),
  visualisations consommatrices pures + exports PNG/PDF/CSV.
- **Communication cellulaire (Stages 11–12)** : import CellChat/CellPhoneDB
  (schémas stricts, jamais de devinette), table canonique 12 champs,
  harmonisation d'identités par correspondance EXACTE, QC d'import, vues
  exploratoires (dotplot, heatmap pathways, réseau circulaire ggplot2 pur),
  centralité descriptive, filtres avec provenance. Génération depuis données
  brutes = proposition 4D-3 (parking).
- **Abondance différentielle (Stages 13–16)** : validation de design
  expérimental bloquant la pseudoreplication (les cellules ne sont pas des
  réplicas biologiques) ; Milo 4E-1 (voisinages, seed appliquée) ; scCODA 4E-2
  (composition par échantillon, environnement Python explicite, diagnostic de
  convergence en pur R) ; vues croisées 4E-3 descriptives (7 catégories de
  concordance figées, aucune p-value de consensus).
- **Rapport consolidé 4F (Stage 17)** : compilateur d'état canonique + de
  provenance (aucune ré-exécution) — collecteur, validateur à 7 états (les
  sections sans provenance sont refusées), rendu HTML autonome sans pandoc,
  bundle d'export projet (manifeste, tables fidèles, script R reproductible,
  session info).

### Ajouts — Socle & release
- **Durcissement (Stage 18)** : matrice 14 catégories × domaines
  (`docs/release/HARDENING_MATRIX.md`), tests de stress (rapport à 1200 entrées
  de provenance), baseline performance synthétique, limitations connues
  documentées.
- **Packaging RC (Stage 19)** : `scripts/verify_release_gates.R` (lock valide,
  aucune dépendance obligatoire non verrouillée, i18n sans doublon, aucun
  chemin local/credential dans les fichiers suivis) ; renv.lock complété des
  dépendances obligatoires manquantes **shiny.i18n 0.3.0, shinyWidgets 0.9.1,
  shinycssloaders 1.1.0** (419 packages au total) ; test de non-régression
  i18n (un doublon de clé FR fait crasher le démarrage).

### Corrections
- Onglet « 3. Visualisation » : le Bloc 3 « Dynamique & Écosystème » passe de
  « à venir » à « disponible » (panneaux 8 → 9b livrés) — la carte annonçait
  des fonctionnalités existantes comme futures.
- Clé i18n dupliquée « Verdict » (crash au démarrage via shiny.i18n) —
  détectée par le gate de lancement headless, corrigée avant commit.

### Sécurité / données
- Aucune donnée brute embarquée dans les rapports par défaut ; exports =
  résumés et tables de résultats uniquement ; noms de fichiers sources
  originaux seulement (jamais de chemins locaux) dans la provenance.

## [Versions antérieures — non étiquetées]
- Chrysalis 2A–2F : socle `R/core/` (state, validation, provenance, jobs,
  caching, io/pathway helpers) avec tests ; gate de duplication.
- Phases 1–7 historiques : import SC/Bulk/Spatial, pipeline SC (QC, HVG,
  PCA/UMAP/t-SNE, clustering, sketch, BPCells), marqueurs, corrélations,
  pathways, trajectoire, annotation SingleR, pseudobulk, rapports Rmd par
  domaine, spatial (QC, clustering, déconvolution RCTD/STdeconvolve, niches,
  Moran, multi-échantillons).
