# STATUS.md — Source de vérité unique de l'état des chantiers

> **Point d'entrée / navigation** : `docs/ROADMAP.md` (orchestrateur) — il dit
> quoi lire et dans quel ordre. Ce fichier-ci ne décrit que l'**état**.
>
> **Ce fichier fait foi.** Les roadmaps décrivent l'*intention* ; ce fichier
> décrit l'*état réel* (ce qui est commité, ce qui est ouvert, ce qui est
> bloqué). En cas de contradiction entre une roadmap et ce fichier, **ce
> fichier gagne** — puis corrigez la roadmap.
>
> Créé 2026-09-10 pour éliminer la dérive documentaire : quatre roadmaps
> portaient des statuts contradictoires (détails §6 « Dérive corrigée »).
> Mis à jour à la fin de chaque session, dans le même commit que le travail.

**Branche** : `main` (travail direct sur main, décision utilisateur).
**Dernier commit fonctionnel de référence** : `2cd39b4` — feat(plots): PLOT-S3
`ts_datatable()` (wrapper DT harmonisé, 6 sites).
**Dernière consolidation documentaire** : commit `docs(status):` — création de
`docs/STATUS.md` et `docs/ROADMAP.md` (index + état), dé-obsolescence des
roadmaps (voir `git log --oneline -- docs/`).

> **▶ PROCHAINE ÉTAPE** (transfert **Workbuddy**, tranché par l'utilisateur
> le 2026-09-13 — cf. §2y) : **Phase 7–8 CCC (interop OmniPath/LIANA) =
> DÉBLOQUÉE** — choisir la route (renv.lock justifié OU import de résultats
> LIANA externes sans dépendance) ; **Phase 9 CCC (rare cells) = VALIDÉE, à
> prévoir** (audit chevauchement Milo d'abord) ; **Phase 5–6 CCC = GELÉE
> SANS SUITE** (pas d'import fastP côté 4D-3 — ne plus re-proposer).
> **Convention amendée** : « nouvelle dépendance = renv.lock justifié »
> n'est **pas immuable** — une dépendance est acceptable si efficience sans
> régression, justification documentée obligatoire (inscrit dans AGENTS.md
> §5 et `docs/ROADMAP_CCC_ADVANCED.md` §4). État livré : MD-1→MD-4 ✅ +
> 4F-EXT ✅ (cf. §2t–§2x). **Le MCP local fonctionne bien (confirmé
> utilisateur 2026-09-13)** ; l'utilisateur peut prendre en charge
> certaines tâches manuellement (ex. push, vérifs) pour gagner du temps.
>
> **PLOT-S6 est CLOS** (décision utilisateur du 2026-09-12) : il était **déjà
> satisfait par l'arbre** — le routeur `renderUI` statique/interactif existait
> déjà (`modules/sc/mod_sc_viz.R:635`) et Spatial est déjà plotly. Le repli
> **PLOT-S6′** n'est **pas retenu** (§2o). Le volet « présentation » est terminé.
> ⚠️ **Décision ouverte** : déployer les boutons d'export DT sur les ~43 tables
> restantes + normaliser `pageLength` = changement **visible** → jalon dédié
> (`docs/contracts/PLOT_DATATABLE_CONTRACT.md` §6).
>
> **Commits locaux non poussés (compté 2026-09-12 soir, avant le commit MD-1
> ci-dessous)** : 10 en avance sur `origin/main` — les 4 cités ici
> historiquement (`612eddf`, `0912c24`, `2cd39b4`, STAT-S1) plus la campagne
> Bulk V2 M2–M5 et sa documentation (`081bc6a`…`9d38ec7`). Le push reste à
> **la main de l'utilisateur**. La suite complète n'a plus **aucun
> échec** (hors segfault de sortie, §2l, sans impact sur les résultats).
>
> **Serveur MCP local** (`scripts/mcp_server.R` + `mcp.examples/`) : vérifié
> fonctionnel le 2026-09-12 (§2p) — mais **hors de toute roadmap**.
>
> **Livré :** **MD-1 ✅** (conteneur `bulk_datasets` — jeux bulk nommés pour la
> comparaison multi-jeux, 3 producteurs, contrat gelé, cf. §2t), **MD-2 ✅**
> (comparaison multi-jeux — volcanos à échelle partagée, recouvrement DEGs,
> concordance de direction, contrat §10, cf. §2u), **MD-3 ✅** (pont
> pseudobulk → `bulk_datasets`, producteur `pseudobulk`, cf. §2v), **MD-4 ✅**
> (conteneur `sc_datasets` — double jeu SC modes 1/2, décision 5 clôturée,
> cf. §2w), **4F-EXT ✅** (rapport consolidé consomme `bulk_multi_comparison`
> — 12ᵉ domaine, cf. §2x), **STAT-S1 ✅**
> (ComBat-seq — onglet QC Batch du Filtrage, contrat
> gelé, 126 assertions, cf. §2q), **PLOT-S1 ✅** (`ts_theme()` partagé +
> propagation Bulk/SC/Spatial, 44 sites), **PLOT-S2 ✅** (`ts_export_plot()`, 25
> sites), **PLOT-S3 ✅** (`ts_datatable()`, 6 sites), **PLOT-S4 ✅**
> (`ts_complex_heatmap()` — 3 heatmaps ComplexHeatmap unifiées, cf. §2m),
> **PLOT-S5 ✅** (SVG exposé dans les 5 sélecteurs d'export, cf. §2n), **4D-3 —
> chemin de données ✅** (10X/Seurat → entrée CellChat, sans dépendance
> nouvelle), **Bulk V2 / batch-QC ✅** (commité par l'utilisateur `aa92f24`,
> puis **fini** `612eddf` — erreur de test corrigée, dette i18n soldée, cf.
> §5bis), **Bulk V2 M2–M5 ✅ — chantier COMPLET** (scores de voies par
> échantillon GSVA/ssGSEA/PLAGE/zscore, signatures cellulaires Hallmark/
> PROGENy/DoRothEA/RDS local avec garde « scores relatifs », WGCNA safe-mode,
> survie KM+Cox — 4 contrats gelés, 555 nouvelles assertions, roadmap dédiée
> `docs/ROADMAP_BULK_V2.md`, cf. §2r), **Milo — 16 échecs ✅
> RÉSOLUS** (cause racine : fuite d'option `Matrix.warnDeprecatedCoerce` par
> `GSVA::.onLoad()`, cf. §2k).
>
> Rapports de stage 6 sections : `docs/ROADMAP_HANDOFF_STAGE_PLOT_S1.md`,
> `docs/ROADMAP_HANDOFF_STAGE_CELLCHAT_INPUT.md` et
> **`docs/ROADMAP_HANDOFF_STAGE_STAT_S1.md`** (dernier livré). Handoff :
> `docs/ROADMAP_HANDOFF_NEXT.md`. Rien à fournir côté données.

---

## 1. Chantiers livrés (ne pas refaire)

| Chantier | Preuve | Date |
|---|---|---|
| TREE CERBERUS Stages 8–20 (V1.0) | `c08ce99` → `301905f` ; tags locaux `v1.0.0-rc.1`, `v1.0.0` | 2026-09-04 |
| Refonte UX/UI V1.x — lots 0, 1, 2, 3A, 4A, 6A, 5 | `037203e..3ec9e6f` (8 commits) + vague 2 `812d028..876cf57` | 2026-09-05 |
| CCC V1.x-A — contexte spatial | `1d5d0ab` | 2026-09-06 |
| CCC V1.x-B — contexte trajectoire | `da84e28` | 2026-09-06 |
| CCC V1.x-C — contexte vélocité | `4903829` | 2026-09-06 |
| CCC V1.x-D — perturbation in silico | `c1f1fb1` + `62510b9` | 2026-09-06 |
| Import RDA/RDS — exploration de listes imbriquées | `38be355` | 2026-09-06 |
| **PLOT-Q1..Q5 — ggrepel, dpi 300, export PDF, sous-titres stats** | **`4dee553`** | **2026-09-10** |
| **PLOT-S1 — `ts_theme()` partagé + propagation Bulk / SC / Spatial** | **`8295fde`** + **`dcaa1b2`** | **2026-09-10** |
| **4D-3 — chemin de données 10X/Seurat → entrée CellChat** | **`6bf18ca`** | **2026-09-10** |
| **Bulk V2 / batch-QC (+ compare SC)** | **`aa92f24`** (commité + poussé par l'utilisateur) | **2026-09-11** |

## 2. Chantiers ouverts, par ordre d'actionnabilité

### 2a. Prêts à démarrer — présentation (`ROADMAP_presentation_stats.md` §3)

Aucun blocage. Séquençables indépendamment du reste.

| ID | Contenu | Dépend de | Effort |
|---|---|---|---|
| ✅ PLOT-S1 | `ts_theme()` — thème + base_size partagés Bulk/SC/Spatial | — | M |
| ✅ PLOT-S2 | `ts_export_plot()` — helper dpi/format unifié (25 sites) | PLOT-S1 | M |
| ✅ PLOT-S3 | `ts_datatable()` — wrapper DT harmonisé (6 sites) | — | M |
| ✅ PLOT-S4 | `ts_complex_heatmap()` — 3 heatmaps ComplexHeatmap unifiées | PLOT-S1/S2/S3 | L |
| ✅ PLOT-S5 | Export SVG (`svglite`) — exposé dans les 5 sélecteurs d'export | PLOT-S2 | S |
| ✅ PLOT-S6 | Toggle plotly étendu à SC/Spatial — **déjà satisfait par l'arbre** : le routeur `renderUI` demandé existe (`mod_sc_viz.R:635`) et Spatial est déjà plotly | — | **0** |
| ❌ PLOT-S6′ | *(résiduel)* repli **statique** optionnel côté SC — **non retenu** : décision utilisateur du 2026-09-12 = clore PLOT-S6, ne pas ouvrir le repli (§2o) | — | S |

> ⚠️ **PLOT-S1 — piège vérifié le 2026-09-10** : la roadmap propose
> `ts_theme(..., base_size = 12)`, mais **le défaut de ggplot2 est 11** et
> `base_size` est **déjà codé en dur à 20 endroits avec des valeurs divergentes**
> (15×12, 1×13, 1×11, 1×15 ; le reste — dont le switch SC et les 5 thèmes Bulk —
> hérite de 11). Il n'existe donc **aucun défaut neutre** : chaque site migré doit
> **conserver sa valeur actuelle**. Détail et stratégies (A) / (B) :
> `docs/ROADMAP_HANDOFF_NEXT.md` §3 et §5 décision 1.

### 2b. Statistiques (`ROADMAP_presentation_stats.md` §4)

| ID | Contenu | Effort |
|---|---|---|
| ✅ STAT-Q1 | Choix méthode correction multiple (BH/BY/bonferroni/holm) | S |
| ✅ STAT-Q2 | Afficher `lfcSE` (déjà calculé par DESeq2) | S |
| ✅ STAT-Q3 | Note outliers Cook's distance | S |
| ✅ STAT-Q4 | Export Excel pour les pathways | S |
| ✅ STAT-S1 | **ComBat-seq** (`sva`) — onglet « QC Batch » du Filtrage, contrat gelé — **2026-09-12** (§2q) | L |
| STAT-S2 | Réseau d'enrichissement (`emapplot`/`cnetplot`) | M |
| STAT-S3 | Pattern/profile clustering (kmeans MVP, Mfuzz en v2) | L |

### 2c. Nouveaux modules (`ROADMAP_presentation_stats.md` §6)

| ID | Contenu | Prérequis |
|---|---|---|
| NEW-1 | Dose-réponse / time-course (`drc`) | — |
| NEW-2 | Fusionner des jeux de données | ✅ **prérequis levé** — `STAT-S1` livré le 2026-09-12 (`run_combat_seq()` disponible) |
| NEW-3 | Réseau PCSF / interactome | **Backlog conditionnel** — interactome local à évaluer vs contrainte offline |

### 2d. Propositions V1.x (parking officiel)

Source : `docs/release/UPGRADE_AND_COMPATIBILITY.md` §3.

| ID | Contenu | État |
|---|---|---|
| 4D-3 | CellChat depuis données brutes | 🟡 **Chemin de données livré** (`6bf18ca`) — reste le contrat upstream + la dépendance CellChat, voir §3 |
| 4E-4 | Exécution asynchrone de la DA | ⏸ Décision de pool à prendre (§4) |
| 4F-ext | Rapport consolidé étendu Bulk/Spatial | Prêt (aucun blocage technique) |
| Cache | Élargissement du cache | ❌ **Non demandé** — règle 8, ne pas étendre |

### 2e. CCC avancée — parking **tranché** le 2026-09-13 (`ROADMAP_CCC_ADVANCED.md` §4)

| Phase | Contenu | État / condition de déblocage |
|---|---|---|
| 5 | Ligand→receptor→target | ❌ **GELÉE SANS SUITE** (2026-09-13) — pas d'import fastP côté 4D-3 ; l'entrée démontrée est la matrice 10X. **Ne plus re-proposer** |
| 6 | NicheNet-like scoring | ❌ **GELÉE SANS SUITE** (2026-09-13) — idem |
| 7 | OmniPath | 🟢 **DÉBLOQUÉE** — route (a) `OmnipathR` = `renv.lock` justifié, **ou** route (b) import de résultats LIANA externes (colonnes `.rank`) **sans dépendance** — extension du contrat Stage 11. **Recommandation : (b)** |
| 8 | LIANA | 🟢 **DÉBLOQUÉE** — idem ; comparaison en **rangs/recouvrement uniquement**, **jamais de score consensus** |
| 9 | Rare-cell annotator | 🟡 **VALIDÉE — à prévoir** : auditer **d'abord** le chevauchement avec Milo (Stage 14, règle 3 : aucun moteur dupliqué) |
| 10 | Gallery d'étendue | ⏸ Tient tant que « aucune dépendance graph nouvelle » (Stage 12) — même assouplissement que 7–8 possible, non activé |

Convention amendée (2026-09-13) : « nouvelle dépendance = `renv.lock`
justifié » **n'est pas immuable** — une dépendance est acceptable si elle
apporte une efficience **sans régression**, justification documentée
obligatoire (inscrit dans `AGENTS.md` §5, `ROADMAP_CCC_ADVANCED.md` §4 et
`docs/CONVENTIONS.md` §10).

### 2f. UX/UI — lots restants

Lots 0/1/2/3A/4A/6A/5 **exécutés** (preuve §1). Restent :

- **Options 3B, 4B, 6B** — jamais tranchées (les variantes A ont été retenues).
- **Libellés techniques des modules enfants Spatial** (Moran, niches…) —
  volontairement intouchés lors des passes UX.

### 2g. Single-Cell — « double jeu de données » — ✅ FONDATION LIVRÉE (MD-4, 2026-09-13)

Intention utilisateur : deux fichiers chargés, avec **trois relations
déclarées** — analyses séparées à paramètres **partagés** (mode 1), analyses
séparées à paramètres **distincts** (mode 2), ou **fusion** quand ce sont des
réplicats. Détail dans `docs/ROADMAP.md` §4 (« Principe double jeu de
données »).

*État réel (corrigé le 2026-09-13 — l'ancien texte disait « non démarré »)* :
**MD-4 est livré** (`654ab59`) — conteneur `sc_datasets` (`R/sc/sc_multi.R`,
contrat gelé `docs/contracts/SC_MULTI_CONTRACT.md`, plafond
`TS_SC_MULTI_MAX_DATASETS = 5L`), relation **déclarée**
`standalone` / `shared_params` (mode 1) / `distinct_params` (mode 2),
producteurs import + gestion (`modules/sc/mod_sc_datasets.R`) ; voir **§2w**.

Ce qui reste : l'**usage** des modes 1/2 à l'intérieur des panneaux
d'analyse (appliquer les paramètres partagés ou distincts au moment du
traitement). C'est une évolution sur la fondation livrée — proposition
explicite avant exécution, pas un correctif. L'import multiple continue de
fusionner par défaut (`merge()` + `add.cell.ids`, Harmony auto ≥ 2
échantillons) : comportement **inchangé**.

### 2h. Bulk V2 / batch-QC — ✅ COMPLET (M1→M5) + roadmap dédiée créée

**Historique** : commité et poussé le 2026-09-11 (`aa92f24` — « bulk V2.0, sc
compare »), puis fini (`612eddf`). **Le 2026-09-12, les jalons M2→M5 de la
mission Bulk V2 sont livrés** (§2r étendu ci-dessous + commits `081bc6a`,
`a4c67c4`, `0054a89`, `a024e94`) — la recommandation « créer une roadmap
dédiée » est appliquée : **`docs/ROADMAP_BULK_V2.md`** (Flux E documenté,
découvertes d'environnement, restes ouverts).

Règles techniques héritées du brief (STATUS.md §2h) désormais **gelées par
tests** dans chaque contrat : GSVA via `gsvaParam`/`ssgseaParam` avec
`BPPARAM` sur `gsva()` (jamais le constructeur) ; `SerialParam` sous Windows
(jamais `MulticoreParam`) ; **jamais** `enableWGCNAThreads` ;
`fitExtractVarPartModel` plafonné 2000 gènes ; decoupleR signatures locales
uniquement (réseau sortant = 0) ; porte de recouvrement < 20 % après strip
Ensembl `.1/.2` ; N < 15 = arrêt dur WGCNA ; survie médiane/quartiles
uniquement (aucun cutpoint optimal) ; ≥ 10 événements.

Seuils `config/thresholds.R` : tous les `TS_BULK_*` consommés par les
domaines (jamais codés en dur). Packages optionnels installés dans la
bibliothèque renv (GSVA, WGCNA, variancePartition, decoupleR, survminer,
msigdbr, impute) — **`renv.lock` non modifié** (décision utilisateur).

---

### 2i. Dette i18n héritée de STAT-Q — ✅ SOLDÉE (7 clés)

**Ouverte le 2026-09-10, fermée le 2026-09-11.** Les 7 clés ont été ajoutées à
`i18n/translation.json` (entrées `{fr, en}`), avec leurs traductions
anglaises. JSON toujours valide (2080 entrées). Plus aucune chaîne française
ne fuit dans l'UI anglaise pour ces libellés.

```
"Méthode de correction (p-adj)"                    -> "Multiple-testing correction (p-adj)"
"S'applique aux tests DE (DESeq2/edgeR/limma) …"   -> "Applies to DE tests (DESeq2/edgeR/limma) …"
"Export Excel"                                     -> "Excel export"
"{n} gène(s) exclu(s) du test (outlier Cook's …)"  -> "{n} gene(s) excluded from the test (Cook's …)"
"{n} gène(s) non exprimé(s)"                       -> "{n} gene(s) not expressed"
"{n} gène(s) écarté(s) par le filtrage indépendant …" -> "{n} gene(s) discarded by independent filtering …"
"Taille de police (base)"                          -> "Base font size"
```

*Note* : 6 clés venaient de STAT-Q, la 7ᵉ (`Taille de police (base)`) du slider
`base_size` ajouté par PLOT-S1. Les autres libellés de thème existaient déjà
(`Thème`, `Minimal`, `Classique`, `BW`, `Vide`). Aucun changement de code n'était
requis — le code appelait déjà ces clés.

---

### 2j. `docs/` est gitignoré — ✅ DÉCISION ASSUMÉE (avec une réserve)

**Découvert le 2026-09-10, tranché par l'utilisateur le 2026-09-11.**

`.gitignore:36` contient `docs/`. Conséquence vérifiée : `git ls-files "*.md"`
ne renvoie que **5 fichiers** (`CHANGELOG.md`, `README.md`, `RENV_SETUP.md`,
`docs/ROADMAP.md`, `docs/STATUS.md`). Les **21 autres documents** du dossier —
tous les contrats gelés de `docs/contracts/` et tous les rapports de stage —
sont **non suivis**.

**Décision de l'utilisateur : c'est volontaire.** Les documents sont des notes
de travail **locales**, destinées à l'agent qui travaille sur cette machine.
La règle d'ignore n'est **pas** modifiée.

**Réserve (avis, non appliqué)** — le choix est défendable pour les *rapports de
stage* et le *handoff*, qui sont des notes de session. Il l'est moins pour
`docs/contracts/` : ces fichiers ne sont pas des notes, ce sont des **contrats
gelés** que les tests de gel vérifient (par ex.
`test-bulk-batch-qc-contract-freeze.R` échoue si
`docs/contracts/BULK_BATCH_QC_CONTRACT.md` n'est plus synchronisé). Conséquence
concrète : sur un clone neuf, **ces tests échouent** faute de fichier, et
`git revert` ne peut pas restaurer un contrat sans historique.

**Compromis proposé, à valider** : laisser `docs/` ignoré, mais ajouter une
exception pour les contrats uniquement —

```gitignore
docs/
!docs/contracts/
!docs/contracts/**
```

Garde les notes locales, rend les contrats traçables. **Non appliqué** —
décision de l'utilisateur.

### 2k. ✅ Santé de la suite — 16 échecs Milo **RÉSOLUS** (cause racine : fuite d'option GSVA)

**Ouvert le 2026-09-11, résolu le 2026-09-12.** Les 16 échecs (tous Milo)
n'étaient **pas** un bug de Milo : Milo en était la **victime**.

| Fichier | Échecs (avant) | Après |
|---|---|---|
| `test-sc-milo-contract.R` | 8 | **0** |
| `test-sc-milo-views.R` | 4 | **0** |
| `test-milo-contract-freeze.R` | 3 | **0** |
| `test-sc-da-cross-views.R` | 1 | **0** |

**Chaîne causale complète (vérifiée par traçage de pile à l'exécution) :**

1. `bulk_provenance_session_packages()` (`R/bulk/bulk_provenance.R`) charge les
   namespaces de 13 packages pour **lire leurs versions** — dont `GSVA`.
2. **`GSVA::.onLoad()` est, mot pour mot,
   `function(libname, pkgname) { options(Matrix.warnDeprecatedCoerce = 2) }`**
   — et ne restaure **jamais** l'option. Charger GSVA laisse donc un **état
   global** derrière lui, pour tout le reste de la session.
3. Avec `Matrix.warnDeprecatedCoerce = 2`, `Matrix::Matrix.DeprecatedCoerce()`
   exécute `oop <- options(warn = 2L)` **avant** d'appeler `.Deprecated()` → la
   dépréciation devient une **erreur fatale** (d'où le « *(converted from
   warning)* » du message).
4. Milo appelle `miloR::calcNhoodDistance()`, qui fait
   `lapply(value, function(X) as(X, "dgCMatrix"))` sur des `dgTMatrix` →
   `as(<dgTMatrix>, "dgCMatrix")` est déprécié (Matrix 1.7-5) → erreur.
5. Le `tryCatch` de `run_milo_da()` la convertit en `compute_failed`
   (`milo_error`).

**C'est donc un vrai bug inter-modules de production, pas un artefact de test** :
importer un jeu bulk suffisait à rendre **toute** dépréciation Matrix fatale
dans l'application entière.

**Correction** — `bulk_provenance_session_packages()` relève l'option avant le
chargement et la **restaure à l'identique** (absente → retirée). Une fonction
qui ne fait que *constater* des versions ne doit laisser aucun état global.
Test de non-régression ajouté dans `test-bulk-provenance.R` (40 → 42 assertions).

**Pourquoi les tests passaient en isolation** : sans passage préalable par
`bulk_provenance_session_packages()`, GSVA n'est jamais chargé → l'option reste
absente (défaut `NA`) → Matrix prend la branche **`message()`** (bénigne, avalée
par `suppressMessages`) au lieu de la branche erreur. Le bug n'apparaît donc que
dans le contexte de la suite complète (ordre alphabétique : `bulk` avant `milo`).

**Validation — suite complète du 2026-09-12** : `test_dir()` avec
`SummaryReporter(max_reports = 500L)` → **0 échec** (aucune section `Failed`),
**32 avertissements** (tous bénins : « built under R 4.4.3 » et « Coercing to
dgCMatrix »), `EXIT=139` = le segfault de sortie `igraph` du §2l, **sans effet
sur les résultats** (le bilan est imprimé avant le crash). C'est le **nouveau
baseline**, contre 16 échecs avant correction.

**Leçon** : une fonction utilitaire qui appelle `requireNamespace()` sur une
liste de packages hérite des `.onLoad()` de **tous** leurs graphes de
dépendances. C'est un vecteur d'effets de bord globaux — à encadrer.

> ⚠️ **Piège d'outillage (toujours valable)** : `testthat::test_dir()` s'arrête
> de *rapporter* après 10 échecs (« Maximum number of 10 failures reached »)
> mais **continue d'exécuter**. Pour un bilan honnête :
> `testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter$new(max_reports = 500L))`

### 2l. ⚠️ SEGFAULT en sortie de session R — `igraph` (préexistant, **non corrigé**)

**Mesuré le 2026-09-12.** Un processus R qui a chargé `igraph` **segfault en
sortie** (exit code 139), même sans exécuter la moindre fonction d'igraph.

```
Rscript --vanilla -e 'library(igraph)'      # => Segmentation fault (139)
Rscript --vanilla -e 'cat("no igraph\n")'   # => 0   (témoin)
```

**Isolé :** c'est **igraph** (et non miloR, ni renv, ni le projet) :
`scran` et `scater` crashent aussi (ils dépendent d'igraph) ; `edgeR`,
`BiocNeighbors`, `SingleCellExperiment`, `SummarizedExperiment`, `beachmat`,
`Rcpp` sortent proprement.

**Stratégies de sortie testées — toutes échouent** : `q("no", runLast = FALSE)`,
`detach("package:igraph", unload = TRUE)`, `gc()` explicite. Les deux
installations d'igraph (renv 2.3.3 **et** système 2.2.1) crashent.

**Piste** : les deux binaires igraph ont été compilés **sous R 4.4.3** alors que
R est en **4.4.2** — un décalage ABI au déchargement de la DLL est le suspect
le plus probable. **Correctif = action sur l'environnement** (réinstaller un
binaire igraph pour R 4.4.2, ou passer R en 4.4.3) → **à trancher par
l'utilisateur**, non appliqué.

**Impact réel : faible.** Le résumé testthat est **imprimé avant** le crash
(« ══ DONE ══ » puis segfault) → les résultats sont valides ; seul le **code de
sortie** du processus est faux. Dans l'app Shiny, R ne sort qu'à l'arrêt de
l'app, après écriture des sorties.

> 🔁 **CORRECTION (2026-09-12, mesuré lors de l'audit du serveur MCP §2p) :
> l'attribution à `igraph` est INCOMPLÈTE — ce n'est pas igraph qui est en
> cause, c'est une classe de paquets.**
>
> Testé un par un, `Rscript -e 'library(<pkg>)'` (renv du projet actif),
> code de sortie :
>
> | Paquet | Sortie | Paquet | Sortie |
> |---|---|---|---|
> | `igraph` | **139** | `jsonlite` | **0** |
> | `rlang` | **139** | `R6` | **0** |
> | `shiny` | **139** | *(aucun `library()`)* | **0** |
> | `mcptools` | **139** | | |
> | `btw` | **139** | | |
> | `ellmer` | **139** | | |
>
> `mcptools` + `btw` chargés → **`igraph` n'est PAS dans `loadedNamespaces()`**
> et le processus sort quand même en 139. Le déclencheur n'est donc pas igraph
> *en particulier*. L'hypothèse ABI du paragraphe précédent (binaires compilés
> sous R 4.4.3 vs R 4.4.2) reste la piste la plus plausible, mais **le
> correctif doit viser l'environnement R dans son ensemble, pas igraph seul**.
> ⚠️ **Conséquence pour le serveur MCP (§2p)** : un client MCP peut
> interpréter un code de sortie 139 comme un **crash** (journal d'erreur,
> redémarrage du serveur). Le serveur fonctionne (§2p) — c'est bien un défaut
> de *teardown*, pas de service.

---

### 2m. ✅ PLOT-S4 — heatmap ComplexHeatmap unifiée (`ts_complex_heatmap()`)

**Livré le 2026-09-12** (effort **L**, dernier jalon de la série
`plot-shared-helpers`). Nouveau fichier `R/plotting/complex_heatmap.R` +
contrat gelé `docs/contracts/PLOT_HEATMAP_CONTRACT.md` + tests
`tests/testthat/test-plot-complex-heatmap.R` (103 assertions).

**Le problème** : trois implémentations ComplexHeatmap divergentes
(`plot_heatmap_bulk`, `plot_sample_correlation_heatmap`,
`build_sc_hierarchical_heatmap`) — **et aucune des trois n'était testée**.

**Mesure préalable** : les trois divergent sur **9 axes** (`name`, rampe, noms
de lignes/colonnes, titre, `cell_fun`, clustering, mode de dessin, repli sans
ComplexHeatmap, traduction du titre). Aucune valeur n'est neutre → chaque
wrapper passe la sienne **explicitement** (même discipline que `base_size` en
PLOT-S1 et `page_length` en PLOT-S3).

**Décision : zéro changement de comportement.** Les 3 sites deviennent de fins
wrappers. Deux écarts sont **volontairement préservés** :
`plot_sample_correlation_heatmap` ne dessine toujours pas (elle rend l'objet,
l'appelant fait `print()`) → elle reste **non protégée** contre « figure margins
too large » ; et le `name` SC reste `"Z-score"` non traduit. Deux autres constats
sont documentés sans être corrigés : le repli `GetAssayData(slot=)` est *defunct*
depuis SeuratObject 5.0, et **`subtitle` de `plot_heatmap_bulk()` n'est lu nulle
part** — PLOT-Q3 est donc un **no-op sur la heatmap** (il fonctionne sur
Volcano/MA). Cf. contrat §6.

**Nouveautés** : choix **distance/méthode** de clustering, **`k_row`**
(`row_split`, découpage du dendrogramme de lignes en k groupes), et annotations
**multiples** (`col_meta`/`row_meta` en liste nommée ou `data.frame`).

**Vérification** : le test **reconstruit la construction historique** et compare
propriété par propriété (matrice, `name`, titre, affichage des noms, **couleurs
de rampe échantillonnées**, clustering, annotation) — les rampes sont comparées
par leurs couleurs, deux `colorRamp2` créés séparément n'étant jamais
`identical()`. Suite ciblée : **0 échec** (`bulk-helpers`, `sc-helpers`,
`plot-theme`, `plot-datatable`, `plot-export`, `stats-quickwins`,
`bulk-provenance`, `plot-complex-heatmap`). Gate de duplication : **0 erreur /
3 avertissements** (baseline inchangée).

**e2e `shinytest2`** : **13 tests, 0 échec, 0 avertissement** (bulk 4, sc 5,
spatial 4) — l'app démarre et navigue les 3 domaines sans erreur Shiny. Les
**garde-fous de dérive de namespace** ont été étendus aux **6 nouveaux inputs**
(`bulk-de-heatmap_clust_distance` / `_clust_method` / `_k_row` et
`sc-viz-hier_clust_distance` / `_clust_method` / `_k_row`) : un renommage
accidentel côté UI serait désormais détecté au lieu de retomber silencieusement
sur le `%||%` du serveur.

**i18n** : 3 clés ajoutées pour les nouveaux contrôles + 1 clé **résiduelle**
(`"Apparence des graphiques"`, utilisée par PLOT-S1 mais jamais reportée dans
`translation.json` → fuite de français dans l'UI anglaise, corrigée ici).
`translation.json` passe de **2080 à 2084 entrées**, toujours valide, 0 doublon.

**⚠️ Non commité** : sur décision utilisateur (cf. en-tête).

---

### 2n. ✅ PLOT-S5 — SVG exposé dans les 5 sélecteurs d'export

**Décision utilisateur (2026-09-12) : « exposer SVG partout »** — changement
**visible** assumé (une entrée de menu en plus), donc jalon dédié.

**Le constat qui a changé la donne** : la fiche
(`ROADMAP_presentation_stats.md` §3) annonce « nouvelle dépendance légère ».
**C'est faux** — `svglite` **2.2.2 était déjà installé ET déjà présent dans
`renv.lock`**, et la branche `format == "svg"` de `ts_export_plot()` **existait
déjà** (PLOT-S2). Le vrai problème : **la branche était INATTEIGNABLE depuis
l'application** — sur 31 appels à `ts_export_plot()`, aucun ne passait
`format = "svg"`, et aucun sélecteur UI ne proposait `SVG`.

**Livré**

- **Source unique des choix** : `ts_export_format_choices_ui()`
  (`R/plotting/export.R`) dérive le menu de `TS_EXPORT_FORMATS`. Avant, chacun
  des 5 sélecteurs codait son propre `c("PNG"="png","PDF"="pdf")` — c'est
  exactement ce qui a laissé `volcano_export_fmt` dériver jusqu'à devenir mort.
  Une **garde structurelle** du test vérifie que les 5 consomment ce helper.
- **Les 5 sélecteurs proposent désormais SVG** : `volcano_export_fmt`,
  `ma_export_fmt`, `heatmap_export_fmt` (`mod_bulk_de_ui.R`), `traj_export_fmt`
  (`mod_sc_trajectory.R`), `export_format` (`mod_sc_viz.R`).
- **2 sites à device brut complétés** — ComplexHeatmap n'est pas un ggplot,
  `ggsave()` ne s'y applique pas : `mod_bulk_de_viz.R` (heatmap, 9×8 in) et le
  cas `heatmap_hier` de `mod_sc_viz.R` reçoivent une branche
  `svglite::svglite()`. Branches `png`/`pdf` inchangées.
- **`svglite` déclaré** dans `required_packages` (`global.R`) : la branche ne
  retombe plus silencieusement sur `png`. **`renv.lock` inchangé** — aucune
  dépendance réellement nouvelle. 3 listes disjointes, 0 doublon,
  40 requis / 5 optionnels / 7 Bioc.
- **i18n** : le libellé du sélecteur Trajectoire passe de
  « Format export plots (PNG/PDF) » à « … (PNG/PDF/SVG) » (clé FR + valeur EN
  mises à jour dans `translation.json` ; l'ancienne clé n'était référencée
  qu'ici). Les **libellés de choix** restent techniques et **non traduits**
  (« PNG » / « PDF (vectoriel) » / « SVG (vectoriel) ») — ce ne sont pas des
  clés i18n, comportement inchangé depuis PLOT-Q4.

**🐞 Bug corrigé au passage — `volcano_export_fmt` était un contrôle MORT** :
déclaré dans l'UI (`mod_bulk_de_ui.R`) mais **référencé nulle part**, et
`dl_volcano_png` codait l'extension `.png` en dur → choisir « PDF (vectoriel) »
produisait **quand même un PNG**. Le contrôle pilote désormais le nom de fichier
**et** le device. (Antérieur à PLOT-S5, probablement PLOT-Q4.)

**Écarts assumés, non corrigés** (diff volontairement limité au format) :
libellés unifiés sur « PDF (vectoriel) » (2 sites disaient déjà ça, 3 disaient
« PDF » — valeurs inchangées) ; `dl_heatmap` garde son `dev.off()` sans
`on.exit` (pré-existant) ; `dpi = 300` passé aussi aux devices vectoriels, où
`ggsave()` l'ignore. Détail : `docs/contracts/PLOT_EXPORT_CONTRACT.md` §5bis.

**Vérification** : `plot-export` **103 assertions / 0 échec** (dont un test qui
dessine un **vrai ComplexHeatmap sur un device svglite**) ; gate de duplication
**0 erreur / 3 avertissements** (baseline inchangée).

---

### 2o. ⚠️ PLOT-S6 — **prémisse périmée : le jalon est déjà satisfait** (mesuré 2026-09-12)

**Aucun code écrit.** Mesure préalable exigée par `ROADMAP.md` §3.2 (« l'arbre
fait foi ») → la fiche `ROADMAP_presentation_stats.md` §3 décrit un travail
**déjà présent dans le dépôt**.

| Ce que la fiche annonce | Réalité mesurée |
|---|---|
| « créer un `renderUI` qui bascule `plotOutput` ↔ `plotlyOutput`, appliqué à 2-3 vues SC » | **`output$plot_container` (`modules/sc/mod_sc_viz.R:635-642`) EST ce routeur** |
| FeaturePlot / corrélation de gènes « à rendre interactifs » | **déjà `plotlyOutput(ns("plot_interactive"))`** (branche `else`, l. 640-641) |
| SC — volcano | **déjà plotly, plus riche que Bulk** (tooltip natif construit en R, l. 698-720) |
| Spatial « déjà interactive » (concession de la fiche) | **confirmé** : `spatial_preview_plot` = `renderPlotly` (l. 1109) + 5 `plotlyOutput` (l. 226/280/289/321/339) |
| dépendance `plotly` à ajouter ? | **déjà requise** (`global.R:49`) — rien à toucher dans `renv.lock` |

**Le vrai écart est l'inverse de la fiche** : le SC est déjà *interactif par
défaut* (bascule **automatique par type de vue**, non pilotée par
l'utilisateur). Ce qui manque, c'est de pouvoir **revenir au statique** — comme
le font `volcano_interactive` / `ma_interactive` côté Bulk
(`mod_bulk_de_ui.R:120` et `:142`). Jalon résiduel proposé : **PLOT-S6′**
(effort **S**).

⚠️ **Si PLOT-S6′ est retenu, le défaut doit rester l'INTERACTIF**
(`value = TRUE`, à l'inverse de Bulk) : aujourd'hui l'utilisateur SC reçoit du
plotly sans rien demander — un défaut à `FALSE` serait un changement visuel
silencieux, interdit par la règle dure n°1.

📌 **Motif systémique — 3ᵉ prémisse périmée d'affilée** : `base_size` (PLOT-S1),
`pageLength` (PLOT-S3), `plotly` (PLOT-S6). Les fiches §3 datent de l'audit
initial ; l'arbre a évolué depuis. **Toute fiche §3 doit être re-mesurée avant
d'être planifiée.** Détail et commandes : `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md`.

---

### 2p. Serveur MCP local (`scripts/mcp_server.R`) — ✅ fonctionnel, ⚠️ hors roadmap

Ajouté par l'utilisateur le 2026-09-12. Serveur **MCP stdio** pour ce projet
(`posit-dev/mcptools` + sous-ensemble léger de `btw` : outils `docs` + `pkg`).
**N'appartient à aucune roadmap** — même situation que Bulk V2 en son temps
(§2h).

**Vérifié le 2026-09-12** :

- `Rscript scripts/mcp_server.R --check` → **`mcp_server check: OK`**,
  `renv_active: TRUE`, `mcptools 1.0.2`, `btw 1.5.0`.
- Transport **stdio uniquement** (aucun port réseau) — conforme à la contrainte
  local-first/offline du dépôt.
- Fichiers : `scripts/mcp_server.R` (118 l.), `mcp.examples/` (5 snippets +
  README). Tous **non suivis par git**.

**⚠️ Deux points ouverts** :

1. **`renv.lock` désynchronisé.** `mcptools`, `btw`, `ellmer` (+ leurs
   dépendances) sont installés dans `renv/library/…` (448 paquets) mais
   **absents de `renv.lock`** (419 entrées) → `renv::status()` reste
   « out-of-sync ». Sur un **clone neuf**, le serveur ne démarrera pas. La règle
   du dépôt (« aucun changement de `renv.lock` sans justification documentée »)
   impose une décision : soit les snapshoter avec justification, soit assumer
   que l'outillage MCP est **local à cette machine**.
2. **Segfault de sortie** — le processus sort en **139** (§2l), et la mesure
   montre que **ce n'est pas propre à igraph** (voir la note ajoutée en §2l).

**Câblage WorkBuddy** : `~/.workbuddy-ai/mcp.json` créé le 2026-09-12
(`mcpServers.transcriptoshiny-r-btw`). Le serveur **ne s'active pas
automatiquement** — il faut le « Trust » depuis la gestion des connecteurs.
`mcp.examples/workbuddy_template.json` passe d'« UNVERIFIED » à **format
vérifié**.

### 2q. ✅ STAT-S1 — correction de batch **ComBat-seq** (`sva`)

**Livré le 2026-09-12** (effort **L**, 1ᵉʳ jalon du volet statistique
`STAT-S1..S3`). Onglet **« QC Batch » du Filtrage** (`mod_bulk_filter.R`) :
section repliable « Correction de batch (optionnel) — ComBat-seq ».

**Fichiers** (contract-first, règle §4 de `ROADMAP.md`) :

| Rôle | Fichier |
|---|---|
| Logique pure | `R/bulk/batch_correction.R` (nouveau, ~330 l.) |
| Tests fonctionnels | `tests/testthat/test-bulk-batch-correction.R` (**57** assertions) |
| Test de gel | `tests/testthat/test-bulk-batch-correction-contract-freeze.R` (**69** assertions) |
| Contrat gelé | `docs/contracts/BATCH_CORRECTION_CONTRACT.md` (11 sections) |
| Seuil déclaré | `config/thresholds.R` → `TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH <- 2L` |
| Dépendance | `global.R` → `"sva"` ajouté à **`bioc_packages`** (⚠️ **pas** `required_packages`) |
| Câblage | `app.R` (source), `modules/bulk/mod_bulk_filter.R`, `i18n/translation.json` (**+15** clés) |

**Décisions de conception** :

- **ComBat-seq, pas ComBat classique** : agit sur les **comptages bruts** (modèle
  binomial négatif) et accepte `group=` — la condition biologique est donc
  **préservée**, contrairement au ComBat sur matrice transformée qui peut
  l'effacer. C'est la raison du choix.
- **Garde `bulk_assert_raw_counts()` = miroir exact** de
  `bulk_assert_transformed_matrix()` (`bulk_batch_qc.R`) : **même seuil de 0,95**
  de fraction entière, appliqué dans le sens inverse. Le test de gel vérifie la
  symétrie (ce que l'une accepte, l'autre le refuse).
- **Import paresseux** de `sva` (`requireNamespace` au moment de l'appel,
  **jamais** au `source`) → l'app **démarre sans `sva`** ; la fonctionnalité
  échoue alors proprement en erreur classée `missing_dependency`.
- **Zéro duplication** : réutilise `bulk_batch_design_check()` (plan
  d'expérience / colinéarité), `plot_bulk_pca()` (les deux tracés PCA) et
  `patchwork::wrap_plots()`. Aucune logique de plan ni de tracé n'est réécrite.
- **5 états d'erreur gelés** : `invalid_input`, `not_raw_counts`,
  `degenerate_batch`, `missing_dependency`, `compute_failed` (classe
  `bulk_batch_correction_error`). Le test de gel **interdit l'inflation
  silencieuse** (il compte les `state = "…"` du source).
- **Provenance** : `bulk_batch_correction_label()` **préfixe** la normalisation
  déclarée (`"<normalisation> + ComBat-seq (batch : X ; groupe : Y)"`) — jamais
  d'écrasement (principe §4 de `AGENTS.md`).
- **Idempotence** : la copie « pristine » des comptages filtrés vit **dans le
  module** (`bc_pristine`), pas dans `shared_rv` — **aucune clé d'état partagé
  ajoutée**. Le pipeline ne change que sur **clic explicite**.

**Critère d'acceptation (mesuré)** : sur un jeu simulé à comptages binomials
négatifs sur-dispersés et **effet de lot multiplicatif propre à chaque gène**,
le **R² du lot sur PC1** passe de **0,74 → 0,23** (< 50 % de l'initial), et
l'écart entre conditions est **préservé** (> 70 %). Le test vérifie en plus que
la matrice de sortie **conserve `dimnames`**.

⚠️ **Limite gelée, documentée au contrat §11.1** : un **décalage additif
uniforme sur tous les gènes n'est PAS corrigé** — ComBat-seq le lit comme un
**effet de profondeur de séquençage** et l'absorbe par son offset. C'est
**correct** statistiquement (ce n'est pas un effet de lot), mais c'est un piège
de test : la 1ʳᵉ version du fixture échouait pour cette raison. Un test
fonctionnel **fige** ce comportement pour qu'il ne soit pas « corrigé » par
erreur plus tard.

**Portes franchies** :

| Vérification | Résultat |
|---|---|
| Tests ciblés (fonctionnel + gel) | **126 PASS / 0 FAIL / 0 ERROR / 0 SKIP / 0 WARN** |
| `app.R` se source de bout en bout (UI + server construits) | ✅ `SMOKE_SOURCED: TRUE` |
| `sva` dans `bioc_packages` (et **pas** `required_packages`) | ✅ mesuré |
| Porte de duplication (`tools/check_duplication.R … --fail-on-warning`) | **0 erreur / 3 avertissements** = baseline exacte |

> ⚠️ **Note de méthode — le contrôle « doc ↔ code » du test de gel lit l'arbre,
> pas git.** `docs/` est gitignoré (§2j) : le contrat
> `docs/contracts/BATCH_CORRECTION_CONTRACT.md` **n'est pas versionné**, comme
> les 17 autres. Le test `expect_match(doc, "\`nom\`")` vérifie donc le fichier
> présent sur le disque. Conséquence : sur un **clone neuf**, ce test échoue
> (contrat absent) alors que le code est intact — c'est le point soulevé par la
> **décision 4** de `ROADMAP.md` §5, désormais **matérialisé par un test**.

### 2r. ✅ Bulk V2 M2–M5 — chantier COMPLET (scores par échantillon, signatures, WGCNA, survie)

**Livré le 2026-09-12** (chantier Flux E, mission Bulk V2 — roadmap dédiée
`docs/ROADMAP_BULK_V2.md` créée, recommandation §2h appliquée).

| Jalon | Contenu | Contrat gelé | Commit | Assertions |
|---|---|---|---|---|
| M2 | Scores de voies **par échantillon** (gsva/ssgsea/plage/zscore, GSVA 2.x) — onglet « Scores par échantillon » du module Pathways, stockage `bulk_obj$pathways$per_sample` | `BULK_GSVA_CONTRACT.md` | `081bc6a` | 166 |
| M3 | **Signatures cellulaires** (Hallmark/PROGENy/DoRothEA/**RDS local**) + GARDE « scores relatifs, jamais cytométrique » (résultat + chaque ligne d'export + alerte UI permanente) — panneau 3b, `bulk_obj$pathways$signatures` | `BULK_SIGNATURES_CONTRACT.md` | `a4c67c4` | 108 |
| M4 | **WGCNA safe-mode** (power pickSoftThreshold, blockwiseModules TOM borné, bicor MEs/traits) — panneau 3c + onglet WGCNA | `BULK_WGCNA_CONTRACT.md` | `0054a89` | 158 |
| M5 | **Survie** (KM médiane/quartiles, Cox univariés BH) — panneau 3d + onglet Survie, onglets VERROUILLÉS sans temps/statut valides ou < 10 événements | `BULK_SURVIVAL_CONTRACT.md` | `a024e94` | 123 |

**Fichiers** (contract-first à chaque jalon) : `R/bulk/{bulk_gsva,
bulk_signatures,bulk_wgcna,bulk_survival}.R` + leurs tests fonctionnels et
tests de gel + `modules/bulk/{mod_bulk_signatures,mod_bulk_wgcna,
mod_bulk_survival}.R` + section onglet dans `mod_bulk_pathways.R`. Sources
`app.R` câblées à chaque jalon. Seuils ajoutés : `TS_BULK_GSVA_OVERLAP_MIN`.
i18n : +107 clés au total (2204 entrées, integrity OK).

**Gardes mission vérifiés par les tests de gel** : counts bruts refusés
partout (garde M1 réutilisée, re-classée par domaine) ; `BPPARAM` sur
`GSVA::gsva()` et jamais au constructeur ; Windows → `SerialParam` (jamais
MulticoreParam) ; `enableWGCNAThreads`/`allowWGCNAThreads` ABSENTS du code
WGCNA (disable avant chaque calcul) ; N < 15 = arrêt dur WGCNA (testé N=6) ;
HVG bornées [2000, 5000] ; maxBlockSize = 5000 ; decoupleR signatures locales
(réseau sortant = 0, gel structurel) ; découpes survie médiane/quartiles avec
REFUS du cutpoint optimal ; ≥ 10 événements ; provenance PRODUITE à chaque
calcul (`new_provenance_entry`).

**Découvertes d'environnement** (détail + correctifs dans
`docs/ROADMAP_BULK_V2.md` §3) : quirk d'attache WGCNA 1.74 (blockwiseModules
namespacé échoue — attach/detach systématique) ; WGCNA renvoie `fitIndices` ;
decoupleR 2.12 = gènes en ROWS ; variancePartition 1.36.3 vs lme4 récent
(repli R pur documenté) ; msigdbr >= 26 = cache au premier appel.

**GSVA/WGCNA/decoupleR/survminer/msigdbr = dépendances OPTIONNELLES à
l'exécution** (`requireNamespace` à l'appel) : l'app démarre sans elles,
échec propre classé. Installées dans la bibliothèque renv le 2026-09-12 —
**`renv.lock` NON modifié** (décision utilisateur, cf. §2p pour le schéma).

**Vérification finale (2026-09-12, suite COMPLÈTE tous domaines, post-correctif
`5ae0b8b`)** : **0 FAIL / 0 ERROR / 0 SKIP — 3760 PASS** (71 fichiers, runner
crash-resilient `tools/run_full_suite.R`, bilan par fichier flushé) ; gate de
duplication **0 erreur / 3 avertissements** = baseline exacte ; boot headless
`HTTP 200` avec tous les modules câblés ; i18n 2204 entrées 0 doublon.

**§2k — RÉCURRENCE détectée puis re-fermée pendant cette campagne** : le
NOUVEAU domaine `bulk_gsva.R` chargeait GSVA en direct (requireNamespace au
calcul) → `GSVA::.onLoad()` reposait `Matrix.warnDeprecatedCoerce = 2` sans
le restaurer → les 16 échecs Milo réapparaissaient en suite complète (le
baseline vert du matin précédait M2, l'effet de bord n'avait pas pu se
manifester). Correctif `5ae0b8b` : option relevée/restaurée dans
`compute_pathway_scores()` + test de non-régression dédié dans
`test-bulk-gsva.R`. Leçon §2k généralisée : tout nouveau chargement de
package dans un domaine doit encadrer l'option (gelé par test ici).

### 2s. Serveur MCP (§2p) — recommandation sur `renv.lock` + diagnostic ABI

**Ne pas snapshoter `mcptools`/`btw`/`ellmer` dans le `renv.lock`
principal.** Ce ne sont jamais des dépendances de `app.R`/`global.R` (aucun
`library()`/`::` applicatif) — seulement des outils de dev pour l'agent.
Les mélanger au lockfile de reproductibilité scientifique ferait dériver ce
fichier à chaque mise à jour de `mcptools` sans rapport avec une analyse.

**Recommandé** : `renv::activate(profile = "dev")` (bibliothèque/lockfile
dev séparés), ou a minima documenter dans `scripts/mcp_server.R` que ces
paquets sont locaux à la machine et à installer manuellement sur un
nouveau clone. **À valider par l'utilisateur** (décision 7 de
`ROADMAP.md` §5). *(Note : le §2r proposé initialement pour cette
sous-section était déjà pris par Bulk V2 M2–M5 — renuméroté §2s.)*

**Diagnostic ABI §2l/§2p — exécuté le 2026-09-12 (soir), hypothèse
RÉFUTÉE**. L'hypothèse était un décalage massif « bibliothèque compilée
sous 4.4.3 vs interpréteur 4.4.2 ». Mesure réelle (`installed.packages()
[, c("Package","Built")]` + `find.package()`) : la bibliothèque renv
active est majoritairement Built **R 4.4.2**, comme l'interpréteur —
`rlang`, `igraph`, `shiny`, `mcptools`, `btw`, `ellmer`, `R6` = 4.4.2.
**Seul `jsonlite` est Built R 4.4.3** (direction risquée : binaire plus
récent que l'interpréteur), cas isolé. Le segfault de sortie §2l n'est
donc **pas** expliqué par un décalage ABI global. Candidat de correction
ciblé si besoin : réinstaller `jsonlite` sous 4.4.2 — **décision
utilisateur, ne pas faire sans accord** (conséquence renv à documenter
avant). **Connexion à ZCode posée le 2026-09-12 (soir)** :
`.zcode/config.json` créé (portée workspace, clé `mcp.servers`, schéma
strict `stdio` — gitignoré, non commité), format `mcp.examples/
zcode_template.json` passé de UNVERIFIED à **vérifié** ; auto-connexion à
l'ouverture du workspace, statut à contrôler dans **Settings → MCP** après
redémarrage. Santé serveur re-vérifiée le soir même (`mcp_server check:
OK`).

### 2t. ✅ MD-1 — conteneur `bulk_datasets` (fondation multi-dataset Bulk)

**Livré le 2026-09-12 (soir)** (jalon MD-1 de `docs/ROADMAP_MULTI_DATASET.md`,
fusion des décisions 8 et 5 — priorité utilisateur `++`). Contrat gelé :
`docs/contracts/BULK_MULTI_CONTRACT.md` (9 sections). Rapport :
`docs/ROADMAP_HANDOFF_STAGE_MD_1.md`.

**Le piège re-vérifié AVANT tout code** (comme l'exigeait le handoff) : le
mode « one file per sample » de `mod_import_bulk.R` fait bien un full-join
en **UN SEUL** `bulk_obj` (`.merge_per_sample_tables()` →
`global_data$bulk_obj <- list(..., import_mode = "per_sample")`) — MD-1 est
donc bien nécessaire. La prémisse du handoff était correcte cette fois (4e
occurrence du motif « re-mesurer avant de planifier », §2o — mesurée et
confirmée avant le code).

| Rôle | Fichier |
|---|---|
| Logique pure | `R/bulk/bulk_multi.R` (nouveau — check_label/check_obj/capture_pipeline/register/remove/get/summary) |
| Tests fonctionnels | `tests/testthat/test-bulk-multi.R` (**95** assertions) |
| Test de gel | `tests/testthat/test-bulk-multi-contract-freeze.R` (**156** assertions) |
| Module de gestion | `modules/bulk/mod_bulk_datasets.R` (nouveau) — panneau « Multi-jeux — Datasets enregistrés » du module Bulk (producteur `pipeline_save`) |
| Label à l'import | `modules/import/mod_import_bulk.R` — champ **optionnel** `multi_label` + `.register_multi_dataset()` aux **2** points de commit (producteur `import`, échec = alerte non bloquante) |
| Conteneur | `app.R` — init `bulk_datasets = list()` à côté de `bulk_obj`, snapshot session, restore `%||% list()` (snapshots anciens), reset `confirm_reset` |
| Seuil déclaré | `config/thresholds.R` → `TS_BULK_MULTI_MAX_DATASETS <- 20L` (RAM 32 Go) |
| i18n | `translation.json` **+17 clés** (2221 entrées, 0 doublon) + liste idempotente `tools/add_i18n_keys.R` |

**Design — 3 producteurs pour le conteneur** : `import` (état brut, label
optionnel à l'import) · `pipeline_save` (état traité : `bulk_obj` + les
**12 champs de pipeline figés** de `bulk_multi_pipeline_fields()` — contrasts,
filtered_counts, vst_mat, pathway_results… ; JAMAIS `dds_blind`/`dds_full`/
`counts_original`, objets volumineux non comparables) · `pseudobulk`
(réservé MD-3, aucun code en MD-1).

**Gardes gelés par le test de gel** : zéro mutation de `bulk_obj`/`shared_rv`
(lecture seule) ; **isolation par copie** (muter la source après
enregistrement ne change jamais l'entrée — testé) ; **zéro référence à
`bulk_datasets` dans le pipeline existant** (17 fichiers du pipeline vérifiés,
garde §2.3 du contrat) ; labels uniques (refus `duplicate_label` sauf
`overwrite = TRUE` explicite) ; plafond `capacity_exceeded` ; logique pure
(0 symbole Shiny dans `bulk_multi.R` — regex gelées) ; 7 états d'erreur
classés `bulk_multi_error` (inflation interdite) ; app.R 4 ancres (init /
snapshot / restore / reset).

**Écart assumé vs la fiche initiale du jalon** (corrigé dans
`ROADMAP_MULTI_DATASET.md`) : la fiche limitait l'UI à
`mod_import_bulk.R (mode additif)` ; la livraison y ajoute le module de
gestion `mod_bulk_datasets.R` (câblé dans `mod_bulk.R`) car l'acceptation du
handoff exige « un second dataset importé, **traité (Filtrage → DE) et
stocké** » — l'état traité vit dans `shared_rv` (portée `mod_bulk.R`),
l'import seul ne peut produire que l'état brut. `bulk_obj` reste le slot
« actif » ; **l'activation d'un jeu enregistré vers `bulk_obj` n'existe pas
en MD-1** (hors périmètre, candidat MD-2+/MD-4).

**Portes franchies** : tests ciblés **251 PASS / 0 FAIL / 0 ERROR / 0 SKIP**
(fonctionnel 95 + gel 156) ; duplication gate **0 erreur / 3 avertissements**
= baseline exacte ; `app.R` se source de bout en bout (`SMOKE_SOURCED:
TRUE`) ; i18n 2221 entrées 0 doublon. **Vérification finale — suite COMPLÈTE
tous domaines (73 fichiers, runner `tools/run_full_suite.R`)** : **0 FAIL /
0 ERROR / 0 SKIP — 4011 PASS** (= 3760 baseline + 251 nouvelles assertions,
comptes exacts) ; e2e shinytest2 bulk **4 PASS** (app démarre et navigue le
domaine Bulk avec le nouveau panneau). C'est le **nouveau baseline**.

### 2u. ✅ MD-2 — comparaison multi-jeux (`mod_bulk_multi.R`, contrat §10)

**Livré le 2026-09-12 (nuit)** (jalon MD-2 de `docs/ROADMAP_MULTI_DATASET.md`).
Extension du contrat `BULK_MULTI_CONTRACT.md` (**§10** nouveau — code +
freeze test + doc simultanément) + **3 états d'erreur** ajoutés à l'ensemble
gelé du domaine (`insufficient_datasets`, `no_common_contrast`,
`no_significant_genes` — `bulk_multi_error_states()` passe de 7 à 10, tests
MD-1 adaptés dans le même commit). Rapport :
`docs/ROADMAP_HANDOFF_STAGE_MD_2.md`.

**Mesure préalable** : les contrastes stockés dans `pipeline$contrasts` sont
des data.frames dont `.normalize_de_cols()` garantit les colonnes `gene`,
`log2FoldChange`, `padj` — donc `plot_volcano_bulk()`,
`build_contrast_gene_sets()` et `build_contrast_intersection_dt()`
consomment les entrées du conteneur **telles quelles**. MD-2 est une
**composition** de helpers existants, pas un nouveau moteur.

| Rôle | Fichier |
|---|---|
| Logique pure | `R/bulk/bulk_multi_compare.R` (nouveau — entry_contrasts/common_contrasts/deg_gene_sets/volcano_scales/volcano_panel/concordance/run_comparison) |
| Tests fonctionnels | `tests/testthat/test-bulk-multi-compare.R` (**64** assertions) |
| Test de gel | `tests/testthat/test-bulk-multi-compare-contract-freeze.R` (**98** assertions) |
| Module | `modules/bulk/mod_bulk_multi.R` (nouveau) — onglet « Comparaison multi-jeux » du module Bulk (contrôles + 4 sous-onglets) |
| Câblage | `mod_bulk.R` (1 nav_panel + 1 appel serveur), `app.R` (source ×2, snapshot/restore `%||% NULL`/reset de `bulk_multi_comparison`) |
| i18n | `translation.json` **+21 clés** (2242 entrées, 0 doublon) |

**Ce que fait la comparaison** : ≥ 2 jeux enregistrés éligibles (pipeline
capturé) + **un contraste commun** + des **seuils uniques** appliqués à
tous les jeux (les seuils stockés restent affichés en transparence,
colonnes `stored_*`) → (1) **volcanos côte à côte à échelle partagée**
(`bulk_multi_volcano_scales()` calcule des limites globales — un `padj`
nul (Inf) est exclu du calcul d'échelle — appliquées via `coord_cartesian`)
assemblés par `patchwork::wrap_plots()` ; (2) **recouvrement des DEGs**
(UpSet `plot_upset_contrasts()` + table `build_contrast_intersection_dt()`) ;
(3) **concordance de direction** par paires (Jaccard Up/Down + % même
direction) ; (4) table de détail par dataset. Résultat écrit **en plat** dans
`global_data$bulk_multi_comparison` (pattern `spatial_multi_integration`,
consommable par le futur 4F-ext) — **sans dupliquer** les data.frames de
résultats (le rendu relit `bulk_datasets`).

**Gardes gelés** : lecture seule (ni `bulk_datasets`, ni `bulk_obj`, ni
`shared_rv`) ; réutilisation stricte — le source cite les 4 helpers et le
test interdit toute redéfinition ; pureté Shiny (mêmes regex que §2.7) ;
snapshots antérieurs tolérés (`%||% NULL`) ; dataset supprimé depuis le
calcul → message propre au rendu (pas de crash).

**Portes franchies** : tests ciblés **417 PASS / 0 FAIL / 0 ERROR / 0 SKIP**
(bulk-multi 95 + gel 156→160 + compare 64 + gel compare 98) ; duplication
gate **0 erreur / 3 avertissements** = baseline exacte ; `SMOKE_SOURCED:
TRUE` ; i18n 2242 entrées 0 doublon. **Vérification finale — suite COMPLÈTE
tous domaines (75 fichiers, runner `tools/run_full_suite.R`)** : **0 FAIL /
0 ERROR / 0 SKIP — 4177 PASS** (= 4011 baseline + 162 pour les 2 nouveaux
fichiers + 4 pour le gel MD-1 étendu aux 10 états, comptes exacts) ;
e2e shinytest2 bulk **4 PASS** (app démarre et navigue le domaine Bulk avec
le nouvel onglet). C'est le **nouveau baseline**.

### 2v. ✅ MD-3 — pont pseudobulk → `bulk_datasets` (producteur `pseudobulk`)

**Livré le 2026-09-12 (nuit)** (jalon MD-3 de `docs/ROADMAP_MULTI_DATASET.md`).
Aucun nouveau fichier de logique : le pont est une **composition pure** de
l'API gelée MD-1 (`bulk_multi_check_label()` → `bulk_multi_capture_pipeline()`
sur une liste partielle → `bulk_multi_register(producer = "pseudobulk")`),
câblée dans le module SC existant. Contrat mis à jour (§2.8 + §6 producteur 3
— code + freeze test + doc simultanément), rapport :
`docs/ROADMAP_HANDOFF_STAGE_MD_3.md`.

| Rôle | Fichier |
|---|---|
| Câblage UI + serveur | `modules/sc/mod_sc_pseudobulk.R` — section « 3. Envoi vers la comparaison multi-jeux Bulk » : label libre (défaut `pseudobulk_<cible>_vs_<réf>`, ne remplit jamais un champ saisi) + bouton « Envoyer vers comparaison Bulk » |
| Entrée produite | `obj = list(counts, metadata)` (comptages agrégés + metadata façon Bulk) ; pipeline capturé = contraste unique `<cible>_vs_<réf>` + seuils du run, autres champs `NULL` → **éligible à la comparaison MD-2 dès l'envoi** |
| Test fonctionnel | `tests/testthat/test-bulk-multi-pseudobulk-bridge.R` (**39** assertions — entrée conforme, résumé, éligibilité + `bulk_multi_run_comparison()` contre une entrée bulk, overwrite, `invalid_obj`, isolation par copie) |
| Test de gel | `test-bulk-multi-contract-freeze.R` — ancre contrat §2.8/§6 mise à jour + garde « consommateur mince » sur `mod_sc_pseudobulk.R` (appels API, `producer = "pseudobulk"`, interdiction d'écriture sur `bulk_obj`/`shared_rv$contrasts`) |
| i18n | `translation.json` **+9 clés** (2251 entrées, 0 doublon) |

**Gardes** : lecture du seul état `pb$` du module — `bulk_obj` et
`shared_rv$contrasts` jamais écrits (gelé par test) ; échec d'enregistrement
= notification d'erreur, jamais d'interruption ; sans résultat DE =
avertissement ; re-pousser le même label = mise à jour explicite
(`overwrite = TRUE`, `registered_at` conservé) ; aucun changement app.R
(`bulk_datasets` déjà snapshot/restore/reset par MD-1).

**Portes franchies** : tests ciblés bulk-multi **464 PASS / 0 FAIL**
(bulk-multi 95 + gel 168 + compare 64 + gel compare 98 + pont 39 = comptes
exact par fichier) ; duplication gate **0 erreur / 3 avertissements** = baseline ;
`SMOKE_SOURCED: TRUE` ; i18n 2251 entrées 0 doublon ; suite complète cf.
§6 du rapport MD-3.

### 2w. ✅ MD-4 — conteneur `sc_datasets` & double jeu SC (modes 1/2, décision 5)

**Livré le 2026-09-12 (nuit)** (jalon MD-4 de
`docs/ROADMAP_MULTI_DATASET.md` — **roadmap MD complète**). Miroir du
pattern `bulk_datasets` appliqué au domaine SC, avec une différence assumée :
**pas de capture de pipeline** (l'état pipeline SC vit dans l'objet Seurat ;
les paramètres sont des inputs de module) — à la place, chaque entrée porte
une **relation déclarée** (`standalone` | `shared_params` [mode 1] |
`distinct_params` [mode 2]) = annotation de provenance, jamais appliquée
mécaniquement. Contrat gelé `docs/contracts/SC_MULTI_CONTRACT.md`
(code + freeze test + doc simultanément). Rapport :
`docs/ROADMAP_HANDOFF_STAGE_MD_4.md`.

| Rôle | Fichier |
|---|---|
| Logique pure | `R/sc/sc_multi.R` (nouveau — register/remove/get/summary/relations, 7 états `sc_multi_error`) |
| Seuil config | `config/thresholds.R` — `TS_SC_MULTI_MAX_DATASETS = 5L` (chaque entrée = un objet Seurat **complet**) |
| Producteur import | `mod_import_sc.R` — panneau « Multi-datasets SC (double jeu) » (label optionnel + relation) ; helper `.register_sc_multi_dataset()` aux **4 points de commit** ; échec = alerte, import jamais bloqué ; sans label = comportement inchangé |
| Producteur gestion | `modules/sc/mod_sc_datasets.R` (nouveau) — câblé dans `mod_sc.R` (section Préparation) : label, relation, enregistrement (`producer = "pipeline_save"`, overwrite explicite), résumé, suppression |
| Câblage app.R | source ×2 ; init `sc_datasets = list()` ; snapshot ; restore `%||% list()` ; reset |
| Tests | `test-sc-multi.R` + `test-sc-multi-contract-freeze.R` — **180 PASS / 0 FAIL** |
| i18n | `translation.json` **+15 clés** (2266 entrées, 0 doublon) |

**Gardes gelés** : zéro écriture sur `sc_obj` (le pipeline SC existant ne
référence **jamais** `sc_datasets` — garde de non-régression testée par grep
sur 33 fichiers SC préexistants) ; pureté Shiny de `sc_multi.R` (regex) ;
plafond conteneur + repli config ; labels uniques (overwrite explicite) ;
isolation (remplacer `sc_obj` ne change jamais une entrée).

**Pièges i18n rencontrés et soldés** (cf. rapport §1.1) : avec
`i18n$use_js()`, `i18n$t()` à l'UI retourne un shiny.tag — les noms de
choix de `selectInput` exigent `.tr_plain()` ; `c("label" = "valeur")` avec
des appels de fonction ne parse pas — `setNames()` requis.

**Portes franchies** : sc-multi **180 PASS / 0 FAIL / 0 ERROR / 0 SKIP** ;
i18n 2266 entrées 0 doublon ; duplication gate **0 erreur /
3 avertissements** = baseline ; `SMOKE_SOURCED: TRUE` ; **suite COMPLÈTE
tous domaines (80 fichiers, runner `tools/run_full_suite.R`)** : **0 FAIL /
0 ERROR — 4484 PASS / 1 SKIP** (skip préexistant dans `test-mod-geo.R`,
hors périmètre MD) ; e2e shinytest2 bulk **4 PASS** / import **5 PASS** /
sc **5 PASS** (avec le nouveau panneau datasets au boot) / spatial
**4 PASS**. Correctif e2e en cours de jalon : le résumé du conteneur ne
doit PAS passer par `validate(need())` (émet un `.shiny-output-error`
visible au boot — table vide à la place). C'est le **nouveau baseline**.

### 2x. ✅ 4F-EXT — le rapport consolidé consomme `bulk_multi_comparison` (12ᵉ domaine)

**Livré le 2026-09-13** (parking V1.x consommé — promis par le rapport MD-2
§5.2 : « la structure §10.4 est déjà plate et sérialisable »). Le rapport
consolidé 4F ajoute une **12ᵉ section figée** `bulk_multi_comparison` :
résumé descriptif de la dernière comparaison multi-jeux bulk (datasets,
contraste, seuils, `ran_at`), verdict `valid_legacy` à **libellé dédié**
« auto-daté (ran_at) », `analysis_id = "bulk-multi-compare"`, et **3 tables**
de plus dans le bundle (`bulk_multi_per_dataset.csv`,
`bulk_multi_concordance.csv`, `bulk_multi_intersection.csv`). Contrat
`CONSOLIDATED_REPORT_CONTRACT.md` mis à jour (code + freeze test + doc
simultanément). Rapport : `docs/ROADMAP_HANDOFF_STAGE_4F_EXT.md`.

| Rôle | Fichier |
|---|---|
| Collecteur | `report_collector.R` — catégorie **domaines "global"** (`.report_global_domains`) : résultat plat lu dans `global_data$bulk_multi_comparison` via le **nouveau paramètre optionnel `global_data`** (défaut NULL = comportement d'origine strictement conservé) ; garde de forme (`per_dataset` + `concordance` non vides + `ran_at` — résultat partiel ⇒ section absente, jamais « réparée ») |
| Validateur | `report_validator.R` — domaine global ⇒ `valid_legacy` avec libellé dédié (traçabilité par `ran_at`, pas par la provenance partagée) ; **aucun nouvel état** (7 états inchangés) |
| Rendu + bundle | `report_render.R` (libellé de section) ; `report_bundle.R` (+3 tables, copies fidèles) |
| Module 9b | `mod_sc_report_consolidated.R` — transmet `global_data` au collecteur (1 ligne) |
| Tests | freeze (domaines 12, symbole gelé, ancres contrat) + fonctionnel (collecte/verdict/bundle/rendu, comportement d'origine sans `global_data`) — **166 PASS / 0 FAIL** sur le filtre `report` |
| i18n | aucune clé nouvelle (libellés rapport = texte FR figé, convention du domaine) |

**Choix documentés** : `sc_datasets` (MD-4) n'est **volontairement pas**
consommé — état de stockage, pas un résultat d'analyse (le rapport compile
des analyses) ; `bulk_multi_comparison` n'est PAS un domaine « à contrat »
au sens 4F (pas de provenance partagée ni d'empreinte v2 — résultat
cross-datasets non lié à l'objet SC courant).

**Portes franchies** : filtre `report` **166 PASS / 0 FAIL** ; duplication
gate **0 erreur / 3 avertissements** = baseline ; `SMOKE_SOURCED: TRUE` ;
suite complète : **0 FAIL / 0 ERROR — 4511 PASS / 1 SKIP** (skip = ping LIVE
conditionnel `test-mod-geo.R`, hors périmètre). C'est le **nouveau
baseline**.

### 2y. Décisions utilisateur 2026-09-13 — parking CCC tranché + transfert Workbuddy

Consignées dans `docs/ROADMAP_CCC_ADVANCED.md` §4 (parking des phases) et
dans le HANDOFF V1.x (`docs/ROADMAP_HANDOFF_STAGE_11_20.md`). Décret :

1. **Phase 5–6 CCC (targets / NicheNet-like) : GELÉE et CLASSÉE SANS
   SUITE** — pas d'import fastP côté 4D-3. PRÉCISION (confirmation
   utilisateur 2026-09-13) : l'application upstream fastP existe et gère
   les fichiers bruts (fastq) **côté utilisateur**, mais ses sorties ne
   sont PAS importées dans Cerberus — la phase n'a donc pas d'alimentation
   en données ; et pour CCC elle-même, l'entrée démontrée par 4D-3 est la
   matrice 10X (`CELLCHAT_INPUT_CONTRACT.md`, `6bf18ca`), pas le fastq.
   Ne plus la re-proposer (sauf demande explicite contraire = NOUVELLE
   proposition type 4D-3, pas une réouverture).
2. **Phases 7–8 CCC (interop OmniPath / LIANA) : DÉBLOQUÉES** — deux
   routes possibles : (a) OmnipathR/liana = ajout `renv.lock` justifié ;
   (b) route d'IMPORT de résultats LIANA externes sans dépendance
   (extension du contrat Stage 11 — `communication_supported_sources` —
   code + freeze test + doc simultanément ; comparaison inter-méthodes en
   **rangs/recouvrement uniquement**, jamais de score consensus).
3. **Convention amendée (à inscrire partout où elle s'applique)** : la
   règle « nouvelle dépendance = renv.lock justifié » **n'est pas
   immuable** — une dépendance nouvelle n'est pas à exclure si elle gagne
   en **efficience sans régression** ; la justification documentée reste
   obligatoire. Inscrite dans AGENTS.md §5 et `ROADMAP_CCC_ADVANCED.md` §4.
4. **Phase 9 CCC (rare cells) : VALIDÉE — à prévoir** : commencer par
   l'audit de chevauchement avec Milo (Stage 14, règle 3 : aucun moteur
   dupliqué) avant toute proposition.

**Transfert d'agent** : la suite revient à **Workbuddy**. Notes
d'environnement confirmées par l'utilisateur le 2026-09-13 : le **serveur
MCP local fonctionne bien** (cf. §2s-bis — santé vérifiée ; template de
connexion `mcp.examples/`) ; **l'utilisateur peut prendre en charge
certaines tâches manuellement** (push, vérifications, arbitrages) pour
gagner du temps — lui demander ce qu'il préfère garder avant d'automatiser.

### 2z. Passe de maintenance 2026-09-13 — conventions, i18n, versionnage

Passe transversale (hors jalon produit) lancée depuis Workbuddy : « remettre
les fichiers à jour, documenter et appliquer les conventions ».

| Action | Résultat mesuré |
|---|---|
| **`docs/CONVENTIONS.md` créé** | conventions de code C1..C12, une seule source ; référencé par `AGENTS.md` et `docs/ROADMAP.md` |
| **`tools/check_conventions.R` créé** | garde base-R (comme `check_duplication.R`) : **0 erreur**, **324 avertissements** de dette (C6 = 16, C9 = 37, C10 = 270, C11 = 1). Exit 0 = vert |
| **Dette i18n soldée** | 101 clés manquantes ajoutées via `tools/add_i18n_keys.R` (idempotent) → **2367 clés**, `tr()` : **0 clé manquante** (C7). Aucune entrée `en` vide (intégrité OK) |
| **🐛 P0 versionnage corrigé** | `R/plotting/complex_heatmap.R` (cœur de **PLOT-S4**, marqué livré en §2m) n'avait **jamais été commité** : absent de `HEAD`, exclu par le `.gitignore` local, alors que `app.R:64` le source — **l'app ne pouvait pas démarrer depuis un clone**. Fichier sorti du `.gitignore` et suivi par git (premier commit) ; verrouillé par la règle **C3** (toute cible de `source()` doit exister **et** être versionnée). ⚠️ Précision : le `.gitignore` lui-même **n'est pas versionné** (dé-tracqué en `7e67581`) — le risque était donc « poste courant + toute copie du `.gitignore` », pas « clone nu » |
| **Docs remises à jour** | `ROADMAP.md` (§2.0 ordre d'actionnabilité daté, 24 contrats au lieu de 13/18, flux B 2026-09-13, décisions 4/5/8 mises à jour + décisions 9 et 10 ajoutées) ; `ROADMAP_HANDOFF_NEXT.md` **ré-écrit** (prompt MD-1 retiré, étape = CCC 7–8, ancres re-vérifiées) ; `STATUS.md` §2g corrigé (le « double jeu SC » n'était plus « non démarré » : MD-4 l'a livré) ; `ROADMAP_MULTI_DATASET.md` §5 repointé |

**Dette résiduelle assumée** (avertissements, plafonds — ils ne doivent pas
augmenter) : 16 `library()`/`require()` au top-level de `R/` (C6), 37
fichiers de `R/` sans test éponyme (C9 — beaucoup sont couverts sous un autre
nom), 270 `stop()` non classés (C10), 1 `BiocParallel::MulticoreParam` sous
garde Unix (C11). Chantier de réduction proposé à l'arbitrage : décision 10
dans `docs/ROADMAP.md` §5.

### 2aa. ✅ CCC 7–8 route (b) — import de rangs LIANA (`a88577f`, 2026-09-13)

Premier jalon d'**interopération CCC**. L'utilisateur a tranché les 4 décisions
de la proposition (`docs/proposals/CCC_7_8_LIANA_IMPORT_PROPOSAL.md`) par
« suis ta recommandation » → options **B+C** appliquées telles quelles.

| Élément | Détail |
|---|---|
| **Route** | **(b)** : import d'une table **agrégée LIANA** produite **hors** de l'application. **Aucune dépendance nouvelle, `renv.lock` intouché**, aucun calcul d'inférence dans l'app |
| **Livrable** | `parse_liana_import(tab, rank_column, aggregation_mode, source_file)` + `communication_rank_fields()` + `communication_rank_aggregation_modes()` ; `"liana"` ajouté à `communication_supported_sources()` |
| **Sémantique** | `aggregate_rank` (p-value **RRA**) → `p_value` ; `score` reste **`NA`** (un rang n'est pas un score) ; `rank_direction = "lower_is_better"` (dans LIANA rang 1 = meilleur : **l'inverse** de `prob`) |
| **Champs de rang** | `rank`, `rank_direction`, `rank_aggregation_mode` — surface **séparée** des 12 champs contractuels : ceux-ci sont *exigés* de toute source, un rang n'existe pas chez CellChat/CellPhoneDB → les y mettre forcerait des colonnes `NA` et **modifierait leurs résultats** (règle 1) |
| **Consensus externe** | `mean_rank`/`aggregate_rank` sont des agrégats **inter-méthodes calculés par LIANA**. L'app n'agrège **jamais** : elle importe et **marque** (`provenance$is_external_consensus`). Nuance écrite au contrat §6.7 |
| **Mode d'agrégation** | **choix obligatoire**, sans défaut : `specificity` et `magnitude` répondent à des questions différentes et ne sont pas comparables entre elles |
| **UI** | 4ᵉ route dans le `radioButtons` existante + `conditionalPanel` (**onglet**, jamais un nouveau panneau latéral) ; colonnes de rang proposées depuis l'en-tête du fichier |
| **Tests** | `test-sc-communication-liana.R` (**73** assertions) + gels étendus ; communication : **684 PASS / 0 FAIL** ; i18n : 15 PASS |
| **Gardes** | conventions **0 erreur** / 324 avertissements (plafonds inchangés) ; duplication **0 erreur** / 3 avertissements ; `SMOKE_SOURCED: TRUE` |
| **Suite complète** | **`failed=0 passed=4609 error=0 skipped=1`** (80 fichiers). Référence : 4511 PASS / 1 SKIP → **+98 PASS** = exactement les assertions ajoutées (73 du nouveau fichier + ~25 du freeze étendu). **Aucune régression.** |
| **Contrat** | `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md` §1, §2, §4, §5, §6, §9, §10 — **même commit** que le code et les gels |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_CCC_7_8.md` (6 sections) |

**Non-régression** : CellChat et CellPhoneDB **ne gagnent aucune colonne de
rang** et leur résultat est inchangé (test dédié) — les parseurs existants n'ont
pas été touchés d'une ligne.

**Risque identifié, non traité dans ce jalon** (voir le rapport §4) : les vues
exploratoires du Stage 12 supposent un score orienté « plus grand = meilleur ».
Sur une source de **rangs**, les échelles de couleur et le filtre « Score
minimum » peuvent être **inversés/trompeurs**. À traiter **vue par vue** avant
d'exposer la route LIANA comme pleinement supportée.

**⚠️ Découverte annexe (pré-existante, non corrigée)** : `git ls-files
docs/contracts/` renvoie **0** fichier pour **24** contrats sur disque — aucun
contrat n'est versionné, donc **un clone neuf ne peut pas passer la suite**
(tous les freeze tests font `expect_true(file.exists("docs/contracts/…"))`).
Même classe de bug que `complex_heatmap.R` (§2z), mais portant sur les 24
contrats. Non corrigé : force-ajouter 24 fichiers relève de la **décision
ouverte n°4** (`docs/` versionné ou non).

### 2ab. 🟡 CCC 9 — périmètre TRANCHÉ (question 1) + proposition écrite (2026-09-13)

Le blocage documenté en §2y (point 4) et dans `ROADMAP_HANDOFF_NEXT.md` §2 est
levé : la phase 9 (« rare-cell annotator ») n'était **spécifiée nulle part**,
et l'audit Milo (`docs/proposals/CCC_9_RARE_CELLS_MILO_AUDIT.md`) avait montré
que trois questions différentes mènent à trois jalons différents.

| Élément | Détail |
|---|---|
| **Décision utilisateur (2026-09-13)** | La phase 9 est la **question 1** — « quelles **populations annotées** sont rares ? » |
| **Nature du jalon** | **Descriptif, mono-condition** — **aucun graphe kNN**, aucune affirmation différentielle. L'audit Milo devient **sans objet** pour ce jalon (l'audit §7.3 l'avait prévu) |
| **Règle 3 (moteur dupliqué)** | Satisfaite **trivialement** : compter les cellules par niveau d'une colonne est une primitive de base R, pas un moteur. **Rien à réutiliser de miloR** (voisinages non requis) et **rien à dupliquer** |
| **Recouvrements apparents écartés** | (i) table d'identités du **design DA** (`R/sc/sc_abundance_design.R:464-487`) — portée « design », **gardée par Stage 13**, répond à « testable ? » pas « rare ? » → **ne pas réutiliser** ; (ii) tableau croisé `mod_sc_annotation.R:307` (cluster × type, UI) → **laisser intact** (règle 1) |
| **Porte Stage 13** | **Inapplicable** — la sortie ne porte **aucune** affirmation différentielle ; motif **écrit dans le contrat** (exigence de l'audit) + champ `descriptive_only` dans la provenance |
| **Seuil de rareté** | **Choix déclaré obligatoire**, aucun défaut implicite (`config/` ne fournit que les **règles autorisées**) — même discipline que le mode d'agrégation de CCC 7–8. ⚠️ **Interdit** de réutiliser `TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE` (confusion sémantique + défaut implicite) |
| **Proposition produite** | `docs/proposals/CCC_9_POPULATION_RARITY_PROPOSAL.md` — **9 éléments V1.x** couverts (§2–§10), **5 décisions** à trancher (§12) |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_CCC_9_PROPOSAL.md` (6 sections) |
| **Code / contrat / test** | **Aucun** — session de proposition (condition d'acceptation du handoff §4) |

**Décisions en attente** (à trancher avant toute implémentation, §12 de la
proposition) : (1) jalon bien descriptif ; (2) seuil = choix déclaré obligatoire ;
(3) rapport consolidé 4F **inchangé** (12 domaines figés — recommandé) ;
(4) unification ultérieure de la table cluster × type (tâche séparée) ;
(5) nom du type canonique `sc_population_rarity` / `analysis_id`
`sc-population-rarity`.

**Si l'utilisateur préfère un jalon sans arbitrage préalable** : `STAT-S2`
(réseau d'enrichissement, `enrichplot` déjà présent) est prêt — `ROADMAP.md`
§2.0, rang 2.

**Rappel de périmètre** : CCC 5–6 restent **gelées sans suite** ; la route (a)
de CCC 7–8 reste une **décision séparée**, non demandée. Les questions 2
(rareté par voisinage) et 3 (rareté × communication) de la phase 9 ne sont
**pas** retenues par la décision du 2026-09-13.

### 2ac. ✅ CCC 9 — rareté par population annotée IMPLÉMENTÉE (2026-09-13)

Suite directe du §2ab : les 5 décisions de la proposition (§12) sont appliquées
telles quelles (jalon descriptif ; seuil = choix déclaré obligatoire ; rapport
consolidé 4F inchangé — option A ; unification cluster × type reportée à une
tâche séparée ; type canonique `sc_population_rarity` / `analysis_id`
`sc-population-rarity`).

| Élément | Détail |
|---|---|
| **Moteur pur** | `R/sc/sc_population_rarity.R` — `compute_population_rarity(meta, identity_column, rule_type, threshold, sample_column, seurat_obj)` + surface publique gelée (12 fonctions). `table()`/base R : règle 3 satisfaite trivialement, aucune primitive miloR |
| **Contrat** | `docs/contracts/POPULATION_RARITY_CONTRACT.md` — gelé ; porte Stage 13 **inapplicable, motif écrit** (§1.1) + `descriptive_only = TRUE` dans la provenance ; corollaire contraignant : toute comparaison entre conditions repasse par la porte DA |
| **Aucun seuil implicite** | `config/defaults.R` : `TS_POPULATION_RARITY_RULES` (règles autorisées) + `TS_POPULATION_RARITY_MIN_CELLS_TOTAL` (plancher de garde 50) — **jamais** un seuil de rareté ; `TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE` non réutilisé (garde au contrat §1.3) |
| **UI** | onglet **dans** le panneau SC existant (`mod_sc.R` : accordion « 2b. Rareté par population » + `nav_panel` sortie) — jamais un nouveau panneau latéral ; champ seuil part **vide** (`value = NA`) |
| **Rapport** | section optionnelle du rapport SC (`reports/sc_report_template.Rmd`), **consomme** `shared_rv$population_rarity_result` — aucune ré-exécution ; rapport consolidé 4F **inchangé** (12 domaines figés) |
| **États** | `valid` / `valid_with_warnings` / `unavailable_single_population` (`is_rare = NA`, pas une erreur) / `invalid_input` (erreurs classées, FR, `call. = FALSE`) |
| **Tests** | `test-sc-population-rarity.R` **98 PASS** + freeze **83 PASS** + i18n 15 PASS ; gardes : conventions **0 erreur / 324 avert.** (plafond), duplication **0 erreur / 3 avert.** (préexistants) ; suite complète : **`failed=0 passed=4788 error=0 skipped=2`** — les 2 SKIP = le SKIP de référence (GEO live smoke) + `test-shinytest2-bulk` **flake chromote** (« Chrome debugging port not open »), repassé **seul : 4/4 PASS** → effectif **4790 PASS / 0 FAIL / 1 SKIP** (référence 4609 PASS / 1 SKIP → **+181 PASS** = exactement les 181 assertions du jalon). **Aucune régression.** |
| **Boot** | headless `runApp()` → **HTTP 200** |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_CCC_9_IMPL.md` (6 sections) |

**Reste ouvert (inchangé)** : les questions 2 et 3 de la phase 9 ne sont pas
retenues ; CCC 5–6 gelées ; route (a) de CCC 7–8 = décision séparée ;
`docs/STATUS.md` §2ab rappelle que **24 contrats** (dont le nouveau
`POPULATION_RARITY_CONTRACT.md`) ne sont pas versionnés dans git — décision
ouverte n°4.


---

## 3. 4D-3 — décision et contrat d'entrée upstream

> **✅ Question du format : TRANCHÉE DANS LE CODE (2026-09-10, `6bf18ca`).**
> `R/sc/sc_communication_input.R` convertit 10X/Seurat → objet d'entrée
> CellChat, **sans aucune dépendance nouvelle** (`createCellChat()` n'est pas
> appelé). Contrat gelé : `docs/contracts/CELLCHAT_INPUT_CONTRACT.md`.
> Rapport : `docs/ROADMAP_HANDOFF_STAGE_CELLCHAT_INPUT.md`.
>
> **Analyse de faisabilité (historique)** :
> `docs/proposals/CCC_DATA_PATH_ASSESSMENT.md` — conclusion : le format 10X
> n'est **pas** le blocage (c'est déjà l'entrée naturelle de CellChat, et
> l'app sait déjà lire 10X). Pas besoin de fastq pour tester.

**Nouveau (2026-09-10).** Ce qui restait une *évaluation* est devenu du code
exécutable. La question posée — « des données 10X (gènes, matrice, features…)
peuvent-elles être converties en une entrée que CellChat consomme ? » — a une
réponse **oui, démontrée** :

- `cellchat_input_from_matrix(data, features, labels, species, …)` →
  produit `{data (gènes × cellules, normalisée, symboles), labels, species,
  provenance, fingerprint}` — exactement les 3 vraies exigences de CellChat
  (expression normalisée, **symboles**, étiquettes de population) + 2 décisions
  déclarées (espèce, contrat upstream).
- `build_cellchat_input(obj, group_by, assay, layer, …)` → wrapper Seurat.
- Détection 10X : table `features.tsv` (1/2/3 colonnes), symboles Ensembl
  (strip `.1/.2`), doublons de symboles (`sum` | `first`), garde-fou
  « la matrice ressemble à des comptes bruts » (→ `log_normalize = TRUE`
  ou erreur explicite).
- 4 états d'erreur classés : `invalid_input`, `invalid_features`,
  `invalid_labels`, `invalid_species`.

**Ce qui reste bloquant n'est donc PLUS le format** :

| Blocage | État |
|---|---|
| Format 10X → CellChat | ✅ **Résolu dans le code** |
| Étiquettes de population | ✅ Gérées (garde-fou + `min_cells_per_group`) |
| Dépendance CellChat dans `renv.lock` | ⛔ Toujours absente — **aucun appel CellChat n'est fait** |
| Contrat d'entrée de l'app upstream | ⛔ Toujours non gelé — voir ci-dessous |

**Décision (2026-09-10)** : l'application upstream (fasta/fastq bruts → données
pour Cerberus) est **en cours de développement par l'utilisateur**. Décision
retenue : **ne pas démarrer les phases CCC 5–6 ni 4D-3 maintenant.**

*Raison* : ce qui bloque n'est pas l'avancement de l'upstream mais le fait que
son **contrat d'entrée n'est pas gelé**. Construire contre une cible mouvante
produirait du code à jeter — et la règle du dépôt (« ne PAS démarrer avant que
le contrat d'entrée upstream soit connu ») l'interdit explicitement.

**Piste pour avancer sans attendre** (détail dans l'assessment §5) : importer
des **résultats CCC produits à l'extérieur** (CellChat/CellPhoneDB tournés hors
app) = extension du contrat Stage 11, sans fastq, sans contrat upstream, sans
dépendance nouvelle. Permet de valider toute la chaîne d'analyse/visualisation
CCC sur des données réelles dès maintenant.

**Ce qu'il faut obtenir de l'app upstream pour débloquer** (à figer côté
upstream, puis recopier ici) :

1. **Formats de sortie** — quels fichiers, quelles extensions (`.h5ad`, `.rds`,
   matrices mtx + barcodes/features, autre ?).
2. **Identifiants** — convention de nommage des gènes (symboles, Ensembl, mixte)
   et des cellules/échantillons. Impacte `harmonize_communication_identities()`.
3. **Métadonnées** — quelles colonnes sont garanties présentes (condition,
   échantillon, batch, type cellulaire ?) et sous quels noms.
4. **Granularité** — la sortie est-elle déjà agrégée par type cellulaire
   (entrée CellChat) ou par cellule ?
5. **Stabilité** — les sorties sont-elles versionnées / stables entre runs ?
6. **Périmètre** — l'upstream fait-il le QC/filtrage, ou Cerberus doit-il le
   refaire ?

Tant que ces six points ne sont pas figés, toute proposition CCC 5–6 reste au
parking.

## 4. 4E-4 — décision de pool à prendre (pas technique, décisionnelle)

Trois options, aucune tranchée :

| Option | Coût | Risque |
|---|---|---|
| Pool mirai dédié au SC (non-Spatial) | Moyen | Deux pools à gérer |
| Pool applicatif unique partagé | Faible | Contention avec Spatial |
| Rester synchrone (V1.0, assumé) | Nul | DA longue bloque l'UI |

Contrainte actée : **scCODA exclu** de l'async tant que reticulate/TensorFlow
tourne dans le processus appelant (chaque daemon = processus Python
supplémentaire). Milo est le candidat réaliste.

---

## 5. État de l'arbre de travail

**✅ Arbre PROPRE au 2026-09-11.** Le chantier Bulk V2 / batch-QC listé
ci-dessous a été **commité et poussé** par l'utilisateur (`aa92f24` —
« bulk V2.0, sc compare », 14 fichiers, +1488). Il n'y a donc **plus** de WIP
en attente ; cette section ne décrit plus que l'historique.

Anciennement non commité au 2026-09-10 (chantier Bulk V2 / batch-QC, cf. §2h) :

```
 M app.R, config/thresholds.R, i18n/translation.json,
 M modules/bulk/mod_bulk.R, modules/bulk/mod_bulk_filter.R,
 M modules/import/mod_import_sc.R, modules/sc/mod_sc.R,
 M modules/sc/mod_sc_da_design.R
?? R/bulk/bulk_batch_qc.R, R/bulk/bulk_provenance.R,
?? tests/testthat/{.smoke_bulk_v2,test-bulk-batch-qc*,
                    test-bulk-provenance}.R
```

`docs/kanban_roadmap.html` est **non suivi par git** (nouveau, ajouté par
l'utilisateur) — à ajouter au suivi quand souhaité.

### 5bis. Pourquoi Bulk V2 était « verrouillé » — et ce qui restait

Question posée le 2026-09-11. Réponse : il y avait **trois** raisons, dont une
seule était réellement technique.

| # | Raison | Nature | Résolution |
|---|---|---|---|
| 1 | **1 erreur de test** — `test-bulk-batch-qc.R` « plots : consommateurs purs ggplot » échouait : `could not find function "ggplot"` | **Technique, bloquante** | `plot_bulk_varpart()` / `plot_bulk_batch_scree()` appelaient `ggplot()`, `labs()`, `theme()` **nus**. Corrigé : appels préfixés `ggplot2::` + garde de gel ajoutée (le fichier doit tourner **sans** ggplot2 attaché) |
| 2 | **Aucune roadmap** — chantier sans document de pilotage (§2h) | Organisationnelle | Toujours ouvert : lui écrire une roadmap dédiée |
| 3 | **Dette i18n non soldée** — les 7 clés devaient partir avec le commit Bulk V2 | Dette déclarée | Soldée : les 7 clés ont été ajoutées à `i18n/translation.json` (§2i) |

**Corrections apportées dans le commit de finition :**
- `R/bulk/bulk_batch_qc.R` — appels ggplot2 préfixés ; `theme_minimal(base_size = 12)` → `ts_theme("minimal", 12)` (site exclu de PLOT-S1 parce que le fichier était alors en WIP — **zéro changement visuel**, 12 conservé explicitement).
- `tests/testthat/test-bulk-batch-qc.R` — source `R/plotting/theme.R` ; `library(ggplot2)` uniquement pour `plot_scree_bulk()` (fonction **préexistante** qui appelle encore `ggplot()` nu). **38 → 40 assertions, 1 erreur → 0.**
- `tests/testthat/test-bulk-batch-qc-contract-freeze.R` — nouvelle garde « aucun appel ggplot2 nu » (74 → 75 assertions).

---

## 6. Dérive corrigée (historique de ce fichier)

Contradictions relevées le 2026-09-10 et leur résolution :

1. **Compte `dpi = 150`** — `ROADMAP_presentation_stats.md` annonçait 15
   occurrences ; le dépôt en contenait **14**. Le dépôt fait foi ; les 14 sont
   traitées (`4dee553`). *Cause : comptage à la main lors de l'audit initial.*
2. **`ROADMAP.md`** — la roadmap présentation dit s'auto-committer en
   `ROADMAP.md` à la racine : ce fichier **n'existe pas**. La roadmap vit en
   `docs/ROADMAP_presentation_stats.md`. Le kanban vit en
   `docs/kanban_roadmap.html`. **Aucun renommage décidé** — l'emplacement
   `docs/` est cohérent avec les trois autres roadmaps.
3. **Statut UX contradictoire** — `V1X_UX_REFACTOR_PROPOSAL.md` affichait
   toujours « aucune exécution avant validation utilisateur » / « Aucun code
   écrit » alors que 8 lots étaient livrés et commités. Corrigé par bandeau en
   tête du fichier (voir commit de session).
4. **Comptes de tests divergents** — trois chiffres circulaient (1822 dans
   `AGENTS.md`, 1858 puis 1873 dans le handoff). Cause probable : sessions de
   validation ne testant que les fichiers modifiés. **Ne jamais citer un
   compte de tests comme référence sans relancer la suite complète** — un
   compte partiel n'est pas une référence de non-régression.
5. **`kanban_roadmap.html` sans statut persistant** — la carte s'annonçait
   « statuts en mémoire, pas de stockage persistant », donc PLOT-Q livré
   réapparaissait en « backlog » à chaque rechargement. Corrigé : statuts
   initiaux semés depuis ce fichier (`DEFAULT_STATUS`, PLOT-Q1..Q5 = done).
   **Pas de persistance des clics** (décision) : le kanban est un miroir
   *visuel* en lecture seule de ce fichier — s'il mémorisait les clics, un clic
   périmé pourrait masquer une mise à jour de `STATUS.md`. « Réinitialiser »
   restaure l'état du dépôt.

## 7. Structure documentaire — point à trancher

`docs/` est **volontairement gitignoré** (décision utilisateur) : roadmaps et
instructions d'agents restent locales et non suivies. C'est défendable pour la
documentation de pilotage.

**Exception à considérer** : `docs/contracts/*.md` (13 contrats gelés). La
règle du dépôt (`AGENTS.md`) impose qu'un changement de contrat soit livré
**code + freeze test + doc simultanément**. Si le contrat n'est pas versionné,
cette règle est mécaniquement inapplicable. C'est le seul endroit où le
`.gitignore` contredit une règle écrite du dépôt. Décision en attente.

Note : `docs/STATUS.md` et `docs/ROADMAP.md` ont été **forcés** dans le suivi
git (`git add -f`) car ils constituent l'index et l'état — mais tout le reste
de `docs/` reste ignoré. `docs/kanban_roadmap.html` reste **non suivi**.
