# CONTRAT MOTEUR — CELLCHAT NATIF (Path B)

V1.x · créé le 2026-09-14 · jalon **CC-2** (`STATUS.md` §2ao) · décision
utilisateur : `docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md` §8 et
`STATUS.md` §2al.

> **Toute modification doit passer simultanément** par : le code
> (`R/sc/sc_communication_engine.R`), le test
> **`tests/testthat/test-sc-communication-engine.R`** **et** ce document.
>
> *Nommage* : ce contrat fait partie des 6 dont les assertions de gel vivent
> dans le **test éponyme du domaine** (comme `BULK_DOSE_RESPONSE`,
> `BULK_PATTERN`, `PLOT_DATATABLE`, `PLOT_EXPORT`, `PLOT_HEATMAP`) et non dans
> un `test-<sujet>-contract-freeze.R`. Écart de nommage assumé — la règle est
> qu'**aucun contrat ne soit sans test** (garde **C8** = 0).
>
> **Ce contrat ne remplace rien.** La voie **Path A** (import d'un résultat
> CSV/TSV produit hors de l'application) reste entière, inchangée et
> impérative — volonté explicite de l'utilisateur. L'**export** des résultats
> après calcul reste également disponible.

---

## 1. Champ d'application

Ce contrat couvre **l'exécution de CellChat dans l'application** et la
réduction immédiate de son résultat aux **12 champs canoniques** du contrat
Stage 11 (`COMMUNICATION_RESULT_CONTRACT.md` §4).

| Bout de la chaîne | Producteur | Fichier externe ? |
|---|---|---|
| **Entrée** (matrice normalisée + symboles + étiquettes) | `build_cellchat_input()` — contrat `CELLCHAT_INPUT_CONTRACT.md`, **inchangé** | aucun |
| **Résultat** (paires LR, `prob`, `pathway`) | **ce contrat** — `run_cellchat()` | aucun |

Hors périmètre, explicitement :

- **toute comparaison inter-condition** — le moteur produit un résultat **par
  objet Seurat** (une seule condition). Une comparaison passe par la **porte
  DA** (décision Phase 9). `mergeCellChat()` / `compareCellChat()` ne sont
  **jamais** appelés ;
- **les figures CellChat** (`netVisual_*`) — les plots restent des
  **consommateurs purs** de la table canonique (motif Stage 9). Aucun objet
  circlize / ComplexHeatmap / grid du moteur n'entre dans l'application ;
- **les seuils de performance** (plafond de cellules, de clusters, mode
  RAM-safe, ETA) — **benchmark d'abord, contrat ensuite** (§7).

---

## 2. Dépendance — justification

| Point | Valeur mesurée |
|---|---|
| Source | **GitHub uniquement** (`jinworks/CellChat`) — ni CRAN, ni Bioconductor |
| Version retenue | ligne **v2** (`2.2.0.9001`) ; `v3 = SpatialCellChat` **exclu** (chantier spatial séparé) |
| Épinglage | par **SHA** — un remote GitHub n'est reproductible que par son SHA |
| Empreinte mesurée | 39 dépendances directes, **32 déjà dans `renv.lock`** ; **11 paquets à installer** + `CellChat` (détail et correction de l'estimation initiale : `STATUS.md` §2ao.1) |
| Compilation | **oui** — `LinkingTo: Rcpp, RcppEigen` ⇒ Rtools requis à l'installation |
| Démarrage de l'app | **jamais requis** — `requireNamespace("CellChat")` paresseux + erreur classée `missing_dependency` avec guidage d'installation |

⚠️ **Statut du pin au 2026-09-15** : **non abouti** (jalon **CC-1**).
`renv::install("jinworks/CellChat")` n'a rien installé après ~30 min bien que
Rtools 4.4 soit détecté. Le moteur est donc livré mais **non exécutable** :
`run_cellchat()` lève `missing_dependency`, l'application démarre normalement et
Path A continue de fonctionner. La présente section décrit la **cible** ; CC-1
viendra la satisfaire (détail : `STATUS.md` §2ao.4).

Précédent de pin par SHA : déjà **5 paquets GitHub** au lockfile (`BPCells`,
`spacexr`, `STdeconvolve`, `SeuratDisk`, `schard`). Ce n'est donc pas une
nouveauté de classe, seulement une surface de reproductibilité **double** : le
SHA du paquet **et** la base `CellChatDB`.

---

## 3. Résultat produit — `run_cellchat()`

`run_cellchat()` **délègue** la finalisation à
`finalize_communication_result()` (contrat Stage 11) avec
`computation = "engine"`. Le moteur ne fabrique **aucun** champ de résultat qui
lui serait propre : la forme de l'objet est **identique** des deux voies.

| Champ | Contenu |
|---|---|
| tout le schéma Stage 11 | inchangé (`type`, `status`, `source_method = "cellchat"`, `canonical_table`, `identity_column`, `identity_mapping`, `identity_summary`, `column_mapping`, `qc`, `input_summary`, `object_identity`, `warnings`, `timestamp_utc`) |
| `analysis_id` | **`"sc-communication-engine"`** (distinct de `sc-communication-import`) |
| `provenance$method` | `"cellchat"` (vs `"import_cellchat"` sur la voie import) |
| `provenance$import_only` | **`FALSE`** — c'est le marqueur qui distingue un calcul d'un import |
| `provenance$computation_path` | `"B"` (vs `"A"`) |
| `provenance$engine` / `engine_version` / `engine_sha` | identité du moteur |
| `provenance$database` / `database_version` | base LR utilisée |
| `engine` | `list(engine, engine_version, engine_sha, database, database_version, seed, nboot, n_populations, n_pathways_significant, n_interactions)` |
| `engine_path` | `"B"` |

### 3.1 Les 12 champs canoniques — cible commune

`sender` · `receiver` · `ligand` · `receptor` · `interaction` · `pathway` ·
`score` · `p_value` · `p_adjusted` · `source_method` · `source_file` ·
`source_cell_identity_level`

Extraction depuis l'objet CellChat (**structure réelle**, vérifiée sur le code
amont `R/modeling.R`) :

| Champ canonique | Source dans l'objet CellChat |
|---|---|
| `sender` / `receiver` | `dimnames(net$prob)[[1]]` / `[[2]]` (groupes) |
| `ligand` / `receptor` / `pathway` | `object@LR$LRsig` — colonnes `ligand`, `receptor`, `pathway_name`, appariées par **nom d'interaction exact** |
| `interaction` | `paste(ligand, receptor, sep = " -> ")` — dérivé, comme sur la voie import |
| `score` | `net$prob` (valeurs non nulles) |
| `p_value` | `net$pval` (même forme), sinon `NA` |
| `p_adjusted` | **`NA`** — jamais fabriqué (§6.4) |
| `source_method` | `"cellchat"` |
| `source_file` | `NA` — aucun fichier externe n'est lu |
| `source_cell_identity_level` | colonne d'identité déclarée (posée par l'harmonisation) |

⚠️ **Le nom d'interaction CellChat n'est jamais découpé.** `interaction_name`
peut contenir plusieurs sous-unités (`L_R1_R2`) : le deviner serait inventer
une donnée. D'où la résolution par `@LR$LRsig`.

---

## 4. Paramètres déclarés

| Paramètre | Défaut | Règle |
|---|---|---|
| `seed` | **aucun — obligatoire** | CellChat permute (`nboot`) pour ses p-values. La graine est un **paramètre du calcul**, jamais un `set.seed()` global en session Shiny. Un run sans graine tracée n'est pas reproductible ⇒ erreur `invalid_parameters`. |
| `nboot` | `TS_CELLCHAT_NBOOT_DEFAULT` (100) | Nom **réel** du paramètre de `computeCommunProb()`. ⚠️ **`nPerm` n'existe pas** — piège signalé par la proposition §9.4, vérifié sur la signature amont. |
| `species` / `group_by` / `data` | issus de `cellchat_input` | Jamais redemandés ni reconstruits. |
| `TS_CELLCHAT_MIN_GROUPS` | 2 | CellChat n'infère rien avec moins de deux populations. |

---

## 5. États de validité — `cellchat_engine_states()`

Erreurs classées `cellchat_engine_error`, état lu par
`cellchat_engine_error_state()` :

- `valid` — résultat canonique produit.
- `missing_dependency` — paquet `CellChat` absent. **État prévu** : message
  avec la commande d'installation et le rappel que Path A reste disponible.
- `invalid_input` — objet `cellchat_input` non conforme, ou base non résolvable.
- `invalid_parameters` — graine absente, `nboot` invalide, < 2 populations.
- `engine_failure` — CellChat a échoué : le message d'origine est **remonté tel
  quel**, jamais traduit ni masqué.
- `no_interactions` — aucune probabilité non nulle, ou tout supprimé au QC.
  Aucun résultat canonique n'est produit (jamais un résultat vide trompeur).

---

## 6. Invariants d'ingénierie (gelés)

### 6.1 Moteur éphémère

```text
etat reactif = resultat canonique  !=  moteur externe
```

L'objet `cellchat` (base LR + tableaux 3-D) n'est **jamais** stocké : ni dans
un `reactiveValues`, ni dans le résultat, ni dans l'état partagé. La référence
est retirée dès l'extraction faite. Vérifié **statiquement** (aucune
affectation d'un tel objet hors du fichier moteur) et **dynamiquement** (classe
du résultat et de ses champs).

### 6.2 `future` séquentiel à l'intérieur du job

`computeCommunProb()` peut demander un plan `future`. L'application utilise
**mirai** (un seul pool — règle du dépôt) : un plan non séquentiel lancé
**dans** un daemon créerait des workers imbriqués. Le moteur force donc
`future::plan(future::sequential)` pour la durée du calcul et **restaure** le
plan précédent à la sortie.

### 6.3 `updateCellChatDB()` interdit

La base ne doit jamais changer silencieusement entre deux runs : deux
résultats portant la même `database_version` doivent rester comparables. Aucun
appel à `updateCellChatDB()` n'existe dans le dépôt — garanti par le test de
gel.

### 6.4 `p_adjusted` reste `NA`

Aucun export standard de CellChat ne produit de p-value ajustée au niveau LR.
La fabriquer serait inventer une donnée (règle 1) : le champ reste `NA`,
exactement comme sur la voie import. Le niveau **pathway** est couvert par
`computeCommunProbPathway()` (slot `netP`), ce qui rend l'onglet « Heatmap
pathways » fonctionnel au moins comme sur Path A.

### 6.5 Reproductibilité — identité d'analyse

Deux runs identiques doivent être reconnaissables. `seed` + `database_version`
**ne suffisent pas**. L'identité est **dérivée** de la provenance existante —
jamais dupliquée (règle 3, audit n°3 §10.2) :

| Élément | Fourni par |
|---|---|
| `analysis_id`, `seed`, `parameters` | `new_provenance_entry()` — existe |
| `input_fingerprint` | `dataset_hash` + `hash_exact` — existe |
| `dataset_dims` | `new_provenance_entry()` — existe |
| **`engine_sha`** | **nouveau** — SHA du remote GitHub |
| **`database_version`** | **nouveau** — version de `CellChatDB` |

`cellchat_analysis_identity(result)` **dérive** les premiers et **ajoute** les
deux derniers. Aucun champ de provenance n'est recopié deux fois.
`database_fingerprint` (hash du contenu de la base) est **reporté en V2** —
non bloquant (§10.3 de la proposition).

---

## 7. Aucun seuil de performance avant benchmark

Deux expertises externes se contredisent d'un facteur ~3 sur la RAM à 60 000
cellules. Règle du dépôt, payée trois fois (`base_size`, `pageLength`,
`plotly`) : **re-mesurer avant de planifier**. Sont donc **interdits** dans ce
contrat jusqu'à mesure :

- tout ETA affiché (« 60 000 cellules → 15 min ») ;
- tout plafond de cellules ou de clusters ;
- tout « mode RAM-safe ».

Seront affichés à la place, après mesure uniquement : `n_cells`, `n_groups`,
interactions candidates, paramètre de permutation — puis un état
**OK / WARNING / BLOCK**.

---

## 8. Surface publique — `cellchat_engine_public_api()`

`cellchat_engine_available` · `cellchat_engine_error_state` ·
`cellchat_engine_states` · `cellchat_engine_summary` ·
`cellchat_analysis_identity` · `run_cellchat` · `cellchat_engine_public_api`

Helpers internes (préfixés d'un point) : `.CELLCHAT_ENGINE_STATES`,
`.cellchat_engine_stop`, `.cellchat_engine_require`,
`.cellchat_engine_identity`, `.cellchat_engine_db`,
`.cellchat_engine_extract`.

---

## 9. Compatibilité

- **Nouveau fichier** `R/sc/sc_communication_engine.R` (ajout — la règle de gel
  de dossiers autorise les ajouts). Sourcé dans `app.R` **après**
  `R/sc/sc_communication_input.R`.
- `R/sc/sc_communication.R` : **un seul argument additif**
  (`computation = c("import", "engine")`, défaut `"import"`). Aucun appel
  existant ne change de comportement — le freeze test Stage 11 qui assère
  `import_only = TRUE` continue de passer.
- `CELLCHAT_INPUT_CONTRACT.md` : **inchangé**.
- `COMMUNICATION_RESULT_CONTRACT.md` : §1 et §10 mis à jour (le calcul dans
  l'application existe désormais, en plus de l'import).
- `config/defaults.R` : `TS_CELLCHAT_NBOOT_DEFAULT`,
  `TS_CELLCHAT_SEED_DEFAULT`, `TS_CELLCHAT_MIN_GROUPS`.
- **Aucun module modifié** dans ce jalon (l'UI Path B est le jalon **CC-5**).
- Le cache (`cerberus_cache_*`) n'est **pas** étendu : `communication` n'est
  pas une portée autorisée (règle 8).
