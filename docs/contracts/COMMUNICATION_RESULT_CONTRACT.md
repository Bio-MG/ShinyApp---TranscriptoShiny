# CONTRAT DE RESULTAT — COMMUNICATION CELLULE-CELLULE (IMPORT)

Stage 11 (4D-1) · figé à la création · **étendu en CCC 7–8 route (b)** (source
`liana`, champs de mesure de rang, `is_external_consensus`) · toute
modification doit passer **simultanément** par : le code
(`R/sc/sc_communication.R`), le test de freeze
(`tests/testthat/test-communication-contract-freeze.R`) et ce document.

## 1. Champ d'application

Ce contrat couvre **l'import et la validation uniquement** de tables de
résultats de communication cellule-cellule déjà produites hors de
l'application (CellChat, CellPhoneDB, LIANA). Aucun réseau, chord/circle plot
ni inference de pathway (Stage 12) ; aucun matching flou de labels.

> **Nuance du 2026-09-14 (jalon moteur CellChat, `STATUS.md` §2ao).** La phrase
> historique « aucun calcul CellChat/CellPhoneDB/LIANA n'est effectué dans
> l'application » reste **vraie pour ce fichier** — la voie « Path A » n'exécute
> toujours rien — mais elle n'est plus vraie **de l'application** : CellChat
> peut désormais être exécuté nativement (voie « Path B », contrat
> **`CELLCHAT_ENGINE_CONTRACT.md`**). Les deux voies produisent **le même objet
> de résultat**, aux deux marqueurs près : `provenance$import_only`
> (`TRUE` / `FALSE`) et `analysis_id`
> (`sc-communication-import` / `sc-communication-engine`).
> **Path A reste impératif et inchangé** — Path B est un ajout, jamais un
> remplacement.

Principe directeur : les schémas de chaque source sont traités par des
parseurs séparés — **jamais de conversion supposée entre colonnes
incompatibles**. Chaque mapping de colonne est résolu de façon déterministe
(alias explicites) et enregistré dans `column_mapping`.

## 2. Resultat canonique — `finalize_communication_result()`

Objet produit par `finalize_communication_result()`, consommé via
`assert_communication_result()` par tout consommateur (visualisations,
exports, futur rapport). Champs du contrat :

| Champ | Contenu |
|---|---|
| `type` | `"cell_cell_communication"` (constant) |
| `status` | état de validité technique (voir §3) |
| `source_method` | `"cellchat"` ou `"cellphonedb"` — **un seul par résultat** |
| `canonical_table` | table canonique (12 champs, §4) + colonnes d'origine utiles + `sender_mapped`/`receiver_mapped`/`duplicate_interaction` |
| `identity_column` | colonne de métadonnées Seurat choisie explicitement |
| `identity_mapping` | data.frame `label → matched_identity` (exact match uniquement) |
| `identity_summary` | compteurs d'harmonisation (n_labels_matched, unmatched_labels, ambiguïtés de casse…) |
| `column_mapping` | mapping `canonical_field → colonne source` |
| `qc` | compteurs QC produits à l'import (lignes droppées, auto-interactions, duplications, p-values hors [0,1]…) |
| `input_summary` | `list(source_method, files, n_rows_input, n_rows_canonical)` — fichiers : **noms originaux uniquement**, jamais de chemins locaux |
| `object_identity` | `list(fingerprint, method, seurat_dims)` — empreinte **v2 réutilisée de `velocity_object_fingerprint()`**, jamais dupliquée |
| `warnings` | avertissements produits à l'import (harmonisation, QC), jamais reconstruits |
| `provenance` | entrée `new_provenance_entry()` enrichie (`analysis_type = "cell_cell_communication"`, `import_only = TRUE`, `is_external_consensus` = TRUE si la source a fourni un agrégat inter-méthodes — voir §6.7) |
| `analysis_id` | `"sc-communication-import"` |
| `timestamp_utc` | horodatage UTC ISO-8601 de l'import |

## 3. États de validité — `communication_validity_states()`

- `valid` — table canonique importée, identités harmonisées sur la colonne
  choisie, QC terminé. **Validité technique uniquement** : aucune validité
  biologique n'est impliquée.
- `invalid_input` — fichier absent/illisible/vide/type inattendu.
- `invalid_schema` — colonnes requises de la source absentes ou inexploitables
  (p. ex. `interacting_pair` sans séparateur `|` unique).
- `invalid_identity_mapping` — **aucun** label sender/receiver ne correspond à
  la colonne choisie (bloquant : la colonne ne décrit probablement pas les
  populations de la source).
- `stale_against_current_seurat_object` — état dérivé à l'affichage
  (`communication_result_is_stale()`) : le résultat correspond à un objet
  Seurat antérieur. Un résultat périmé n'est jamais affiché ni exporté.

Les états `invalid_*` lèvent des erreurs classées
`communication_import_error` (état structuré via `communication_error_state()`)
; **aucun résultat canonique n'est produit** dans ces cas.

## 4. Table canonique — `communication_contract_fields()`

12 champs contractuels (les champs absents d'une source restent `NA`, jamais
fabriqués) :

`sender` · `receiver` · `ligand` · `receptor` · `interaction` · `pathway` ·
`score` · `p_value` · `p_adjusted` · `source_method` · `source_file` ·
`source_cell_identity_level`

Champs dérivés (toujours clairement séparés des champs contractuels) :

- `interaction` = `paste(ligand, receptor, sep = " -> ")` ;
- `sender_mapped` / `receiver_mapped` : **exact match uniquement** contre la
  colonne choisie — jamais de renommage silencieux de population ; les labels
  sans correspondance restent tels quels (`*_mapped` = NA) et sont listés
  intégralement ;
- `duplicate_interaction` : clé `sender|receiver|ligand|receptor` dupliquée —
  lignes **conservées et flagées, jamais fusionnées silencieusement**.

### Champs de MESURE DE RANG — `communication_rank_fields()` (CCC 7–8 route (b))

Colonnes **additionnelles**, présentes **uniquement** quand la source fournit
une mesure ordinale (aujourd'hui : la source `liana`). Elles ne font
volontairement **pas** partie des 12 champs contractuels : ceux-ci sont
**exigés** de toute source (`finalize_communication_result()` échoue si l'un
manque), alors qu'un rang n'existe ni chez CellChat ni chez CellPhoneDB. Les y
ajouter forcerait ces deux sources à émettre des colonnes `NA` et **modifierait
leurs résultats** — interdit par la règle 1 (zéro changement sur l'existant).
**Un consommateur DOIT donc tester leur présence.**

| Champ | Contenu |
|---|---|
| `rank` | valeur ordinale importée (`mean_rank`, `{méthode}.rank` ou `aggregate_rank` selon le choix déclaré de l'utilisateur) |
| `rank_direction` | `"lower_is_better"` — dans LIANA, rang 1 = meilleur : c'est l'**inverse** de `prob` (CellChat) |
| `rank_aggregation_mode` | `"specificity"` ou `"magnitude"` (`communication_rank_aggregation_modes()`) — **choix explicite** de l'utilisateur, aucun défaut implicite ; les deux modes répondent à des questions différentes et ne sont **pas** comparables entre eux |

QC associé (compteurs `qc`, jamais corrigés) : `n_rank_out_of_range`,
`n_rank_missing`. Borne haute = nombre de lignes ; borne **basse = 0 et non 1**,
car `aggregate_rank` est une p-value RRA sur `[0,1]` et peut légitimement être
importée comme colonne de rang — mieux vaut sous-signaler que produire un faux
positif sur un fichier valide.

## 5. Sources supportées (v1) — `communication_supported_sources()`

- `cellchat` : table exportée CellChat (`source`/`target`/`ligand`/`receptor`/
  `prob`, `pathway`/`pathway_name` optionnel, `pval`/`padj` optionnels).
  Colonnes d'origine utiles conservées (`ligand.group`, `receptor.group`,
  `pathway_name`).
- `cellchat` (route objet, Stage 12) : `parse_cellchat_object()` lit un objet
  CellChat sauvegardé (.rds) produit HORS de l'application et **extrait** ses
  résultats déjà calculés (`net$prob`, `net$pval` optionnel) — **aucune
  méthode CellChat n'est relancée, aucun score ré-agrégé** au-delà du
  remodelage déterministe des valeurs non nulles.
  Forme de `net$prob` : array 3-D **[groupe source, groupe cible,
  `interaction_name`]** — mesurée sur CellChat 2.2.0.9001 (3 × 3 × 109, et
  0/109 noms d'interaction ne contiennent `|`).
  **Délégation (règle 3)** : l'extraction est faite par
  `.cellchat_engine_extract()`, la SEULE fonction qui connaît la structure
  interne de CellChat — commune au moteur Path B et à cet import. Il n'existe
  donc pas de seconde lecture de `net$prob` dans l'application.
  `ligand`, `receptor` et `pathway` sont **résolus** via `@LR$LRsig` quand
  l'objet le porte (ils ne sont plus systématiquement NA : changement de
  comportement assumé au 2026-09-15, il aligne les deux voies) ;
  `p_adjusted` reste NA (non produit par CellChat au niveau net).
  Accepte l'objet S4 ou une liste nommée `net`/`LR` de forme identique (route
  testable). La forme `[ligand, récepteur, "sender|receiver"]` documentée
  avant le 2026-09-15 était **fictive** — aucune version de CellChat ne la
  produit — elle est désormais **refusée**, jamais interprétée à tort.
- `cellphonedb` : `means.txt` (requis) + `pvalues.txt` (optionnel), format v2 :
  colonne `interacting_pair` (`ligand|receptor`, séparateur `|` **exactement
  une fois**) et une colonne par paire `sender|receiver` (convention
  CellPhoneDB partner_a|partner_b). P-values rapprochées par
  (interacting_pair, colonne de paire) ; paires absentes → `p_value` = NA.
  `p_adjusted` n'existe pas dans ce format → NA + avertissement explicite.
- `liana` **(CCC 7–8 route (b))** : table **agrégée** produite par LIANA
  **hors de l'application** (`liana_aggregate()` / `rank_aggregate()`,
  `get_ranks = TRUE`, `get_agrank = TRUE`). L'application n'exécute **aucune**
  méthode CCC, ne recalcule **aucun** rang et ne fabrique **aucun** score.
  - **Identité** : `source` / `target` (populations) ; `ligand` / `receptor`
    **décomplexifiés** en priorité, sinon `ligand.complex` / `receptor.complex`
    repris **tels quels** — jamais découpés (un complexe n'a pas de découpage
    univoque : le deviner serait inventer une donnée).
  - **`aggregate_rank` → `p_value`** : c'est une **p-value** produite par
    *Robust Rank Aggregation* (`min(p) × k`, `p_i = pbeta(r_i, i, k−i+1)`),
    pas un score de communication.
  - **`score` reste `NA`** : convertir un rang en score serait une erreur de
    catégorie (échelle et direction différentes).
  - `pathway` et `p_adjusted` restent `NA` (le format agrégé ne les expose pas)
    avec avertissement explicite.
  - **Deux paramètres déclarés obligatoires** : la colonne de rang à importer
    (`rank_column`) et le mode d'agrégation (`aggregation_mode`) — ni l'un ni
    l'autre n'est déductible de façon fiable du contenu du fichier.
  - Échecs classés : `invalid_input` (mode ou colonne de rang manquant/inconnu,
    table absente ou vide), `invalid_schema` (`sender`/`receiver` ou
    ligand/récepteur ou colonne de rang absents).

**Hors périmètre de la route import (décision 2026-09-02) :** générer des
résultats de communication depuis des données brutes (10x mtx/.h5/.h5ad/.rds
d'expression) exigerait d'exécuter CellChat/CellPhoneDB — calcul lourd reporté
à une étape explicite, jamais introduit par la route import.

> **Cette étape explicite existe depuis le 2026-09-14** : le moteur natif
> CellChat (`R/sc/sc_communication_engine.R`, contrat
> `CELLCHAT_ENGINE_CONTRACT.md`, décision `STATUS.md` §2al). Elle ne change
> rien à la présente section : l'**import** ne génère toujours rien, et il
> reste la voie de secours et d'interopérabilité.

## 6. Garde-fous scientifiques (Stage 11, hérités pour la Stage 12)

1. Validité technique ≠ validité biologique : les libellés
   (`communication_status_labels()`) l'énoncent explicitement.
2. Un résultat ne mélange jamais deux sources : les scores de sources
   différentes ne sont **pas comparables** sur une échelle commune.
3. Aucun score, p-value ou pathway n'est recalculé, imputé ou inféré.
4. Provenance PRODUITE à l'import (règle 7 AGENTS.md), consolidée au rapport
   (Stage 17) — jamais reconstruite après coup.
5. Mapping d'identités explicite et reviewable (`identity_mapping`) ; décision
   enregistrée, jamais une conversion implicite.
6. Ligne droppee (sender/receiver/ligand/receptor vide) = comptée avant/après
   (`qc`), jamais supprimée silencieusement.
7. **Consensus importé ≠ consensus calculé (CCC 7–8).** LIANA produit lui-même
   des agrégats **inter-méthodes** (`mean_rank`, `aggregate_rank`).
   L'application **n'agrège jamais** : elle importe et **marque**. Le résultat
   porte donc `provenance$is_external_consensus = TRUE` dès qu'au moins une
   valeur agrégée par la source est présente (`aggregate_rank` importé dans
   `p_value`, ou colonne de rang `mean_rank` / `aggregate_rank`). La règle 2
   reste intacte : il n'y a toujours **qu'une seule** `source_method` par
   résultat. Cette nuance doit rester écrite ici — sans quoi un lecteur futur
   conclurait que la règle a été contournée.
8. **Un rang n'est pas un score.** Sur la source `liana`, `score` reste `NA` et
   la mesure ordinale vit dans `rank`, avec `rank_direction =
   "lower_is_better"` : toute comparaison ou échelle de couleur doit **inverser
   explicitement** la direction. Les vues exploratoires (§8) doivent traiter ce
   cas vue par vue.

## 7. Exports (Stage 11)

- `build_communication_import_summary()` : résumé à 1 ligne (CSV).
- `build_communication_identity_mapping_export()` : table d'harmonisation
  (CSV), tracée par `analysis_id`.
- `communication_export_filename()` : `<kind>_<analysis_id>_<date>.<ext>`.
- Résultat canonique complet en RDS (module).
- Périmètre des fichiers sources enregistré : noms originaux uniquement.

## 8. Vues exploratoires (Stage 12 — `R/sc/sc_communication_views.R`)

Fichier ADJONCTION (l'API de `sc_communication.R` reste gelée telle quelle) ;
sa surface publique est gelée via `communication_views_public_api()` :

- `communication_apply_filters()` — filtres d'AFFICHAGE (score_min,
  p_value_max, pathways, senders, receivers, include_self) ; la table du
  résultat n'est **jamais modifiée** ; sémantique explicite : une ligne sans
  valeur pour un champ filtré est retirée et **comptabilisée** ; les filtres
  sender/receiver portent sur le nœud coalescent (identité harmonisée sinon
  label brut).
- `plot_communication_dotplot()` / `plot_communication_pathway_heatmap()` /
  `plot_communication_circle()` — consommatrices pures (`assert_communication_result`,
  garde de péremption) ; type de score + méthode source affichés sur CHAQUE
  figure ; p-values étiquetées « importées » ; aucune causalité suggérée ;
  résultat vide filtré → message explicite, jamais un graphique trompeur ;
  pathways manquants exclus et comptés (jamais imputés) ; auto-interactions
  non dessinables sur le cercle, comptées.
- `build_communication_centrality()` — quantités dérivées du réseau
  (degrés, totaux de scores importés) étiquetées **descriptives, pas un
  contrôle biologique**.
- `build_communication_filter_provenance()` — entrée de provenance PRODUITE
  à l'export avec les filtres figés dans les paramètres (`analysis_id =
  "sc-communication-explore"`, `import_only = FALSE`) ; appendue par le
  module, jamais reconstruite.
- `build_communication_filtered_export()` — chaque ligne porte `analysis_id`,
  horodatage de base et `applied_filters` (export reproductible).

Implémentation : ggplot pur (courbes de Bézier quadratiques échantillonnées)
— **aucune dépendance nouvelle** (pas d'igraph/ggraph/circlize).

## 9. Surface publique — `communication_public_api()`

Le test de freeze (`test-communication-contract-freeze.R`) refuse toute
fonction top-level non préfixée d'un point hors de cette liste :

`communication_contract_fields`, `communication_supported_sources`,
`communication_validity_states`, `communication_status_labels`,
`communication_status_is_valid`, `communication_error_state`,
`assert_communication_result`, `communication_public_api`,
`communication_rank_fields`, `communication_rank_aggregation_modes`,
`parse_cellchat_import`, `parse_cellphonedb_import`,
`parse_cellchat_object`, `parse_liana_import`,
`harmonize_communication_identities`, `communication_import_qc`,
`finalize_communication_result`, `communication_result_is_stale`,
`build_communication_import_summary`,
`build_communication_identity_mapping_export`,
`communication_export_filename`

Le fichier de vues (Stage 12, §8) expose en outre
`communication_views_public_api()`, gelé de la même façon.

Helpers internes (réservés à ce fichier) : `.COMMUNICATION_STATUS_STATES`,
`.communication_stop`, `.COMMUNICATION_FIELD_ALIASES`,
`.communication_pick_column`, `.communication_is_blank`,
`.communication_coerce_numeric`, `.communication_object_fingerprint`,
`.COMMUNICATION_RANK_DIRECTION_LOWER`.

## 10. Compatibilité

- Nouveau fichier `R/sc/sc_communication.R` (ajout — aucun déplacement).
- Nouveau module `modules/sc/mod_sc_communication.R` monté dans `mod_sc.R`
  (panneau `8b. Communication (import)` + onglet résultat) — IDs de module
  nouveaux, aucun existant modifié.
- Aucune dépendance nouvelle : lecture CSV/TSV via `utils::read.*`, empreinte
  réutilisée de velocity, provenance réutilisée de `R/core/provenance.R`.
- `run_job()` / `cerberus_cache_*` : **aucun appel** — l'import n'est pas un
  calcul lourd et `communication` n'est pas une portée de cache autorisée.

### CCC 7–8 route (b) — évolution **additive** (2026-09-13)

- Source `liana` ajoutée à `communication_supported_sources()` : un parseur de
  plus, **aucun parseur existant modifié**.
- Trois fonctions ajoutées à la surface publique
  (`communication_rank_fields`, `communication_rank_aggregation_modes`,
  `parse_liana_import`) ; aucun nom existant renommé ou retiré.
- Deux compteurs QC ajoutés (`n_rank_out_of_range`, `n_rank_missing`),
  renseignés **uniquement** quand la colonne `rank` est présente — les autres
  sources restent à 0.
- Un argument optionnel ajouté à `finalize_communication_result()`
  (`external_consensus = FALSE`) : les appels existants sont inchangés.
- **Aucune dépendance nouvelle, `renv.lock` intouché** (la route (b) importe
  un résultat produit à l'extérieur — c'est tout son intérêt).
- **Non-régression** : les imports CellChat et CellPhoneDB produisent un
  résultat **identique** (les 12 champs contractuels et leurs valeurs ne
  changent pas ; les champs de rang ne leur sont pas ajoutés).
