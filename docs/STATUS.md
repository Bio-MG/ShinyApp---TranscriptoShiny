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
> ✅ **Décision close** (2026-09-14) : boutons d'export DT généralisés +
> `pageLength = 15` normalisé — jalon **DT-EXPORT livré** (§2ai, contrat
> `PLOT_DATATABLE_CONTRACT.md` §6 option B ; aperçus/QC exclus).
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

### 2ad. ✅ STAT-S2 — réseau d'enrichissement (`emapplot`/`cnetplot`) (2026-09-13)

Jalon flux A, **rang 2** de l'ordre d'actionnabilité (`ROADMAP.md` §2.0) —
fiche **re-mesurée** avant planification (règle du 2026-09-12) : constats
confirmés (ORA sans objet brut, `enrichplot` 1.26.6 présent, onglets
Barplot/Dotplot/Table des deux côtés). Livraison le même jour que CCC 9
(commits séparés, un jalon = un commit).

| Élément | Détail |
|---|---|
| **Moteur** | `plot_pathway_network(df, db_label, top_n, mode, tr)` dans `R/core/pathway_helpers.R` — consomme l'objet brut porté par l'**attribut additif** `enrich_obj` (même pattern que `gsea_obj`) ; `mode = "emap"` (similarité de gènes entre voies, via `pairwise_termsim` + `emapplot`) ou `"cnet"` (voies ↔ gènes). Étiqueté **descriptif** |
| **Attribut additif** | `run_pathway_enrichment()` attache `attr(res_df, "enrich_obj")` — contrat data.frame des appelants inchangé, zéro comportement modifié |
| **UI** | onglet **« Réseau »** dans `mod_bulk_pathways.R` ET `mod_sc_pathways.R` (radio emap/cnet, `top_n` 2–100, export PNG 300 dpi côté bulk, messages d'aide quand aucun résultat) |
| **Dépendance** | **aucune** — `enrichplot` déjà dans `renv.lock` (1.26.6, `pairwise_termsim` présent) |
| **Tests** | `test-pathway-helpers.R` : **16 PASS / 0 FAIL** dont 5 nouveaux blocs — happy path avec un **vrai `enrichGO` hors-ligne** (GO:0007049 depuis `org.Hs.egGO2ALLEGS`) |
| **i18n** | 8 clés FR/EN (2375 → 2410 entrées cumulées) ; `test-i18n-integrity.R` vert |
| **Gardes** | conventions **0 erreur / 324 avert.** ; duplication **0 erreur / 3 avert.** ; suite complète : **0 FAIL / 0 ERROR** (voir §2ac pour le flake chromote documenté) |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_STAT_S2.md` (6 sections) |

**Suivant dans l'ordre d'actionnabilité** : **STAT-S3** (clustering de profils,
kmeans MVP, effort L) — fiche à re-mesurer avant planification ; puis NEW-1..3
prêts, UX 3B/4B/6B et 4E-4 en attente d'arbitrage.

### 2ae. ✅ STAT-S3 — clustering de profils (kmeans MVP) (2026-09-13)

Jalon flux A, **rang 3** de l'ordre d'actionnabilité — fiche **re-mesurée**
avant planification ; **adaptation assumée** : moteur dans `R/bulk/` (et non
`R/core/` comme écrit dans la fiche) — l'arbre actuel place les moteurs bulk
dans `R/bulk/` (bulk_wgcna.R, bulk_gsva.R, bulk_survival.R). **Sur demande
utilisateur : tests ciblés uniquement, pas de suite complète ce jalon.**

| Élément | Détail |
|---|---|
| **Moteur pur** | `R/bulk/bulk_pattern.R` — `run_pattern_clustering(vst_mat, metadata, group_column, genes, k, seed, …)` : moyenne VST par groupe → **z-score par gène** → `stats::kmeans()` ; descriptif, aucune p-value |
| **Contrat** | `docs/contracts/BULK_PATTERN_CONTRACT.md` gelé ; type `bulk_pattern_clusters`, `analysis_id` `"bulk-pattern-clusters"` ; erreurs classées `bulk_pattern_error` ; surface publique figée (8 fonctions) |
| **Tout est déclaré** | `group_column`, liste de gènes (up/down/all_sig, convention pathways), **k** (2..12, plafond `TS_PATTERN_KMEANS_MAX_K`, aucun défaut métier), `seed` tracée (défaut UI 15) ; exclusions comptabilisées (gènes absents/constants, échantillons NA) |
| **Module** | `modules/bulk/mod_bulk_pattern.R` — accordion « 3e. Clustering de profils » + onglet sortie (profils moyens par cluster, table gènes→clusters, export CSV) ; `shared_rv$pattern_result` |
| **V2 floue (Mfuzz)** | non retenue (dépendance nouvelle, aucun besoin exprimé) — écrit au contrat §8 |
| **Tests** | `test-bulk-pattern.R` **31 PASS / 0 FAIL** (100 % hors-ligne) ; ciblés : i18n 15, e2e `shinytest2-bulk` 4, freezes signatures/survie (app.R) 68+80, wgcna 52 ; gardes : conventions **0 erreur / 324 avert.** (1 clé i18n détectée manquante puis ajoutée — flux normal), duplication **0 erreur / 3 avert.** ; boot **HTTP 200** |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_STAT_S3.md` (6 sections) |

**Suivant** : **NEW-1** (dose-réponse / time-course, `drc`) — re-mesurer la
fiche avant planification ; `NEW-2` prérequis levé ; `NEW-3` backlog
conditionnel ; UX 3B/4B/6B et 4E-4 en attente d'arbitrage.

### 2af. ✅ NEW-1 — dose-réponse / time-course (drc) (2026-09-13)

Jalon flux A, rang 4 de l'ordre d'actionnabilité — fiche re-mesurée avant
planification. **Première dépendance nouvelle depuis l'amendement du
2026-09-13** : `drc` 3.0-1, autorisée par l'utilisateur (« ok pour la prochaine
étape »), justification documentée au contrat §2. **Tests ciblés uniquement**
(consigne utilisateur en vigueur, pas de suite complète).

| Élément | Détail |
|---|---|
| **Moteur pur** | `R/bulk/dose_response.R` — `run_dose_response()` : `drc::drm()` par gène (LL.4/W1.4/W2.4/BC.4), EC50 = exp(e), pseudo-R², grille de courbe + IC 95 % ; descriptif, aucune p-value |
| **Dépendance** | `drc` + `multcomp`/`sandwich`/`TH.data`/`plotrix`/`mvtnorm` : **419 → 425 entrées renv.lock, insertion chirurgicale** (pas de `renv::snapshot()` — les packages MCP restent exclus) ; JSON revalidé ; `psych`/`magic`/`meboot`/`mctest` (Suggests) non installés |
| **Contrat** | `docs/contracts/BULK_DOSE_RESPONSE_CONTRACT.md` gelé ; erreurs classées `bulk_dose_error` ; surface publique figée (9 fonctions) |
| **Gardes métier** | dose numérique **déclarée** ; **strictement positive** (log(dose) — temps à 0 = décalage déclaré requis) ; ≥ 4 doses distinctes ; plafond 200 gènes (troncature signalée) ; échecs par gène comptabilisés (`fit_ok`, `message`), `compute_failed` si aucun convergent |
| **Module** | `modules/bulk/mod_bulk_dose_response.R` — accordion « 3f. Dose–réponse / time-course » + onglet sortie (courbe + ruban IC, table EC50, exports CSV/PNG) ; sources up/down/all_sig triées par p.adjust ; `shared_rv$dose_result` |
| **Tests** | `test-bulk-dose-response.R` **38 PASS / 0 FAIL** (Hill synthétique EC50 = 1 retrouvé, montants + descendants, échecs comptabilisés) ; ciblés : e2e shinytest2-bulk 4, freezes app.R 68+80, bulk-pattern 31, wgcna 52, i18n 15 ; gardes : conventions **0 erreur / 324 avert.** (plafond ; 1 avert. C9 transient corrigé au nom de fichier), duplication **0 erreur / 3 avert.** ; boot **HTTP 200** |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_NEW_1.md` (6 sections) |

**Suivant** : **NEW-2 ✅ livré le 2026-09-13** (fusion de jeux bulk — cf.
§2ag) ; NEW-3 backlog conditionnel ; UX 3B/4B/6B et 4E-4 en attente
d'arbitrage.

### 2ag. ✅ NEW-2 — fusion de jeux bulk (Merge Data) (2026-09-13)

Jalon flux A, rang 5 de l'ordre d'actionnabilité — fiche **re-mesurée** avant
planification. **Écart assumé documenté** : la fiche §6 proposait d'étendre
`mod_import_bulk.R` (écrite avant MD-1..MD-4) ; livré dans la famille
« Multi-jeux » du module Bulk, consommant le conteneur `bulk_datasets`, le
produit étant chargé comme **jeu actif** (`bulk_obj`) — comportement d'un
import, sans aucun changement au contrat BULK_MULTI_CONTRACT (pas de nouveau
producteur). **Aucune dépendance nouvelle, `renv.lock` intouché, aucun seuil
`TS_` nouveau.** **Tests ciblés uniquement** (consigne utilisateur en
vigueur ; suite complète non lancée — consigné).

| Élément | Détail |
|---|---|
| **Moteur pur** | `R/bulk/bulk_merge.R` — `bulk_merge_run()` : intersection **exacte** des gènes, union-fill des métadonnées, renommage complet des jeux en collision de noms d'échantillons (jamais silencieux), colonne lot déclarée `dataset_origin`, ComBat-seq **optionnel** (STAT-S1 réutilisé : `run_combat_seq` + `bulk_batch_correction_design`, condition préservée via `group=`) |
| **Contrat** | `docs/contracts/BULK_MERGE_CONTRACT.md` gelé ; 5 états `bulk_merge_error` (`invalid_input`, `insufficient_datasets`, `no_common_genes`, `invalid_metadata`, `design_not_applicable`) ; erreurs des domaines réutilisés propagent avec leur classe d'origine |
| **Module** | `modules/bulk/mod_bulk_merge.R` — accordion « Multi-jeux — Fusion de jeux » + onglet « Fusion de jeux » : aperçus AVANT exécution (gènes communs, plan ComBat), table par dataset, renommages, **PCA avant/après** colorée par jeu d'origine (chaîne canonique `build_dds`→`estimateSizeFactors`→`get_vst_matrix` + `plot_batch_correction_pca`) |
| **Câblage** | `app.R` (2 sources), `mod_bulk.R` (`panel_merge` + `tab_bulk_merge` + serveur) ; le module **n'écrit que** `global_data$bulk_obj` (ne reçoit pas `shared_rv` — garde du test de gel) |
| **Gardes gelés** | pureté Shiny du moteur ; réutilisation stricte (MD-1/STAT-S1 cités, jamais redéfinis) ; module sans écriture conteneur/comparaison ; ancres app.R + mod_bulk.R ; **aucune constante `TS_` nouvelle** ; sync code ↔ contrat ; clés i18n |
| **Tests** | `test-bulk-merge.R` + freeze : **228 PASS / 0 FAIL / 0 WARN** (dont ComBat réel : R² du lot sur PC1 chute, condition préservée) ; voisines : **716 PASS** (bulk-multi ×4, batch-correction ×2, dose, pattern, provenance, i18n) ; freeze WGCNA 106 ; e2e shinytest2-bulk **4 PASS** |
| **Gardes** | conventions **0 erreur / 324 avert.** (baseline exacte — 16 C10 introduits puis éliminés par refactor du constructeur d'erreur) ; duplication **0 erreur / 3 avert.** ; boot headless **HTTP 200** ; i18n **2495 entrées** 0 doublon (+37) |
| **Rapport de stage** | `docs/ROADMAP_HANDOFF_STAGE_NEW_2.md` (6 sections) |

**Reste ouvert** : NEW-3 (backlog conditionnel — interactome local à évaluer
vs contrainte offline) ; UX 3B/4B/6B et 4E-4 en attente d'arbitrage ; boutons
d'export DT (~43 tables) = jalon dédié à créer. Le volet flux A
(`ROADMAP_presentation_stats.md`) est **TERMINÉ** (PLOT-Q/S, STAT-Q/S,
NEW-1..2 livrés ; NEW-3 non planifié).

### 2ah. 🔎 Check-up NEW-2 + politique de tests (2026-09-14)

Séance de reprise — aucune fonctionnalité nouvelle.

- **Check-up NEW-2 (post-livraison)** : tests ciblés relancés —
  `test-bulk-merge.R` (97 PASS) + `test-bulk-merge-contract-freeze.R`
  (131 PASS), **0 FAIL / 0 WARN**. Gates : conventions **0 erreur**
  (324 avert., plafond inchangé). **Suite complète volontairement non
  lancée.**
- **Nouvelle règle — politique de tests (décision utilisateur 2026-09-14)** :
  tests **ciblés par défaut** (seuls les fichiers concernés par le changement
  + gates conventions/duplication) ; la **suite complète ne tourne qu'en fin
  de version** — après accumulation de plusieurs fonctionnalités, avant un
  tag release/RC. Consignée dans `AGENTS.md` §1 (Test policy) et
  `ROADMAP.md` §2.0 (commit `867bb39`).

### 2ai. ✅ DT-EXPORT — boutons d'export DT généralisés + pageLength normalisé (2026-09-14)

Jalon dédié créé conformément au contrat `PLOT_DATATABLE_CONTRACT.md` §6
(**option B**) et au rang 9 de `ROADMAP.md` §2.0 — **changement visible
assumé** (boutons + pageLength 15 sur les tables de résultats). Lancé à
défaut d'arbitrage (l'utilisateur n'a pas tranché entre NEW-3 / UX 3B/4B/6B /
4E-4 / export DT ; ce jalon était le seul sans décision bloquante ; NEW-3
reste interdit sans besoin utilisateur concret — sa fiche l'impose).

| Élément | Détail |
|---|---|
| **Wrapper** | `R/plotting/datatable.R` — `buttons` par défaut = `TS_DT_BUTTONS_DEFAULT` (**TRUE** désormais) ; **nouveau paramètre `extra_options`** (liste nommée fusionnée dans `options` avec priorité maximale — canal unique pour `language`, `lengthMenu`, `order`… ; nouvelle erreur classée `invalid_extra_options`) |
| **Config** | `config/defaults.R` — `TS_DT_BUTTONS_DEFAULT = TRUE`, nouvelle constante `TS_DT_PAGE_LENGTH_DEFAULT = 15L` ; `TS_DT_PAGE_LENGTHS` inchangée (historique) |
| **Migration** | **73 sites** `ts_datatable()` au total : ~47 appels directs `DT::datatable()` migrés dans `modules/` (bulk/import/sc/spatial) + sites canoniques `R/` (`bulk_helpers`, `sc_helpers`, `pathway_helpers`) ; **zéro appel direct restant** (garde ajoutée au test de gel) ; `pageLength = 15L` normalisé sur les tables de résultats |
| **Exception aperçus** | tables d'aperçu/QC (`dom "t"/"tip"`, tables QC `pageLength 5/6/8`, placeholders) : `buttons = FALSE` explicite, page_length d'origine conservée — écart documenté entre « ~43 tables » et ~49 sites |
| **Contrat** | `PLOT_DATATABLE_CONTRACT.md` §4 (API + constantes), §5 (`invalid_extra_options`), §6 (décision livrée), §7–§8 mis à jour — code + test de gel + doc simultanés |
| **Tests** | `test-plot-datatable.R` réécrit : **80 PASS / 0 FAIL** (défaut boutonné, constante suivie, aperçus opt-out, extra_options fusion/erreur, garde zéro appel direct) ; voisines vertes : bulk-helpers 49, sc-helpers 39, pathway-helpers 16, merge-freeze 131, bulk-multi-freeze 168, sc-multi-freeze 110, rarity-freeze 83, i18n 15 |
| **Gardes** | conventions **0 erreur / 324 avert.** (plafond inchangé) ; duplication **0 erreur / 3 avert.** ; boot headless **HTTP 200** (port 4893) ; grep final : plus aucun `DT::datatable(` vivant hors wrapper |
| **Traçabilité export** | chaque table de résultat a un `filename_base` stable (ex. `sc_markers_table`, `milo_da_table`, `spatial_roi_markers`…) → fichiers exportés nommés |

**Points d'attention** : (1) `mod_sc_markers.R` — les libellés FR du DT
(`language`) sont restaurés via `extra_options` (l'ancien `options=` direct
n'est plus possible sans collision de `formals`) ; (2) `mod_spatial_viz.R`
(`spatial_roi_markers`) : `lengthMenu`/`autoWidth`/`order` passent par
`extra_options` ; (3) suite complète **non lancée** (politique du 2026-09-14 :
full suite en fin de version uniquement).

### 2aj. 🔧 Retour feedback utilisateur + consignation session (2026-09-14)

Trois correctifs issus du retour utilisateur (commit `4ed66ba`) :

| # | Symptôme | Cause racine | Correctif |
|---|---|---|---|
| 1 | Littéral `<span class="i18n" data-key="ex : ...">` affiché sous « Label multi-datasets SC » (et 11 sites cousins) | `placeholder = i18n$t(...)` : hors session, `i18n$t()` retourne un **tag HTML** (shim client), échappé comme valeur d'attribut | `placeholder = .tr_plain(...)` sur 12 sites (import SC/Bulk, merge, datasets, dose-response, communication, pseudobulk) — placeholders restent FR (pas de live-switch, philosophie `.tr_plain`) |
| 2 | Boutons « Aller au mapping des IDs » sans effet (import SC / import bulk / GEO) | nav_panels d'analyse titrés `tagList(emoji, i18n$t(...))` **sans `value` explicite** ⇒ bslib génère une valeur interne (`"🔬 \nAnalyse Single-Cell"`) que `nav_select(selected=)` ne matchait jamais | `value = "tab_sc" / "tab_bulk" / "tab_spatial"` sur les 3 nav_panels (app.R) + retarget des 3 handlers ; e2e `click_nav_by_text` inchangé (clique par texte) |
| 3 | Console (app sans données) : `Error in graphics::plot.new: figure margins too large` sur `output$sc-rarity-rarity_plot` | `shiny:::startPNG()` appelle **lui-même** `plot.new()` à l'ouverture du device, AVANT d'évaluer l'expression — device rendu dans un conteneur replié (taille ≈ 0) ⇒ erreur indépendante du `req()` | plancher 200px sur les fonctions `width`/`height` du `renderPlot` de `mod_sc_rarity.R` (affichage inchangé) |

Vérifications : parse OK (11 fichiers), i18n 15 / rarity-freeze 83 /
merge-freeze 131 PASS, boot headless **HTTP 200** (port 4895).

**⚠️ Travail EN COURS non commité (apparu après `4ed66ba`, PAS de cette
session — user ou session agent parallèle) :** `modules/import/
mod_import_sc.R` diff ~85 lignes (`.ensure_10x_features()` — compat
CellRanger / genes.tsv colonne unique) + `tests/manual/
test_ensure_10x_features.R` (non suivi). **Non poussé.** La prochaine
session doit le mettre au point avec son auteur avant tout commit (ne pas
l'écraser, ne pas le commitifier aveuglément).

**PROCHAINE SESSION (consignation) :** 1) **suite complète** (~14 min,
`test_dir("tests/testthat")`) — seuil « fin de version » atteint : CCC 9,
STAT-S2/S3, NEW-1/2, DT-EXPORT et ces correctifs se sont accumulés depuis la
référence Stage 18 ; 2) arbitrages restants : **4E-4 (DA async, pool mirai
partagé recommandé)**, UX **6B** éventuel (3B/4B non recommandés, NEW-3
parqué sans besoin concret) ; 3) baseline DT-EXPORT enregistrée : wrapper
`buttons = TRUE` par défaut, `extra_options`, `page_length = 15L`, garde
« zéro appel direct » dans `test-plot-datatable.R` (80 PASS).

### 2ak. 🔎 Reprise de session — suite complète de fin de version + dérive documentaire corrigée (2026-09-14)

Session de reprise : **aucune fonctionnalité nouvelle**. Exécution de la
première action consignée en §2aj (suite complète, seuil « fin de version »
atteint), puis correction de la dérive documentaire relevée à cette occasion.

| Élément | Détail |
|---|---|
| **Suite complète** | `tools/run_full_suite.R` — **87 fichiers**, `BILAN: failed=0 passed=5101 error=0 skipped=2` (19:31:49 → 20:08:53). Le code de sortie **139 est le segfault de teardown documenté** (§2l), **pas** un échec : le runner flushe par fichier et le BILAN est bien écrit |
| **SKIP 1 (attendu)** | `test-mod-geo.R` — smoke GEO live, **identique** à la référence §2ac (même fichier, même compte) |
| **SKIP 2 (flake)** | `test-shinytest2-import.R` — `skip=1 pass=3` : **flake chromote** (`handle_read_frame error: websocketpp.transport:7 (End of File)`, ~15 min de stall sur ce seul fichier) ; **repassé seul → `fail=0 pass=5 err=0 skip=0`**, exactement la référence |
| **Baseline effective** | **5103 PASS / 0 FAIL / 0 ERROR / 1 SKIP** — référence §2ac = 4790 PASS / 0 FAIL / 1 SKIP → **+313 PASS, 0 régression** (le SKIP restant est le smoke GEO live) |
| **Gates** | conventions **0 erreur / 324 avert.** (plafond inchangé) ; duplication **0 erreur / 3 avert.** (les 3 préexistants) |
| **Dérive « contrats »** | `docs/contracts/` contient **28** contrats ; `ROADMAP.md` (§1, §4, §5), `CONVENTIONS.md` §6 et cette §7 annonçaient **24** — et §7 disait encore **13** (chiffre du 2026-09-10). Les quatre sites sont corrigés à **28** |
| **Nommage des freeze tests** | **24** contrats ont un `test-<sujet>-contract-freeze.R` éponyme ; **5** (`BULK_DOSE_RESPONSE`, `BULK_PATTERN`, `PLOT_DATATABLE`, `PLOT_EXPORT`, `PLOT_HEATMAP`) portent leurs assertions de gel **dans le test principal** du domaine. Écart de **nommage** documenté (`ROADMAP.md` §4, `CONVENTIONS.md` §6) ; garde **C8 = 0** → aucun contrat sans test |
| **Référence cassée** | `BULK_DOSE_RESPONSE_CONTRACT.md` citait `tests/testthat/test-bulk-dose.R` (**inexistant**) → corrigé en `test-bulk-dose-response.R` (idem en-tête du fichier de test ; re-mesuré : **38 PASS / 0 FAIL**) |
| **`ROADMAP.md`** | bandeau §2 « étape courante » **réécrit** (les 4 items qu'il listait comme à faire sont tous livrés) ; §1 : 3 entrées d'index périmées corrigées (handoff consommé, « dernier rapport livré », proposition CCC 9 « en attente de validation ») ; §5 : **décision 12** ajoutée (travail non commité) |
| **`AGENTS.md`** | §1 en-tête : point d'entrée de session mis à jour ; §4 : baseline de suite complète remplacée par la mesure du 2026-09-14 |
| **Archivage** | `docs/archive/` **créé** (2026-09-14) — ⚠️ **rien n'est supprimé**, seulement déplacé : `ROADMAP_HANDOFF_NEXT rev2.md` (auto-déclaré fusionné), `ROADMAP_BULK_V2_STATS.md` (auto-déclaré caduque, **ses 3 références repointées** : `ROADMAP.md`, `ROADMAP_MULTI_DATASET.md` ×2), `app.log`, `app.tex`, `conversation-export.md`, `_e2e_out.txt`. Nouvelle entrée d'index dans `ROADMAP.md` §1 |
| **Handoff ré-écrit** | `docs/ROADMAP_HANDOFF_NEXT.md` (**3ᵉ** ré-écriture) : il listait CCC 7–8 / phase 9 comme « prochaine étape » alors que **les deux sont livrés**. Nouveau contenu = les **3 arbitrages en attente**, ancres **4E-4 re-vérifiées** (`R/core/jobs.R:50`, `R/spatial/spatial_async.R:112/232/294`, `modules/sc/mod_sc_da_milo.R:185`), critères d'acceptation, prompt à coller. Ancienne version **archivée** (`docs/archive/ROADMAP_HANDOFF_NEXT_2026-09-13_CCC9.md`) |
| **Kanban remis à jour** | `docs/kanban_roadmap.html` — miroir de ce fichier, **figé au 2026-09-12** : `DEFAULT_STATUS` complété (STAT-S2/S3, NEW-1/2, DT-EXPORT), **carte DT-EXPORT ajoutée**, descriptions datées + références de commit, en-tête/pied corrigés (**22 tâches + 7 baseline**, 17/22 done). Syntaxe JS validée (`node --check`) |
| **Bloqué / en attente** | **4E-4** (pool), **UX 3B/4B/6B**, **NEW-3** (interdit sans besoin concret) = arbitrages utilisateur ; `.ensure_10x_features` non commité = à coordonner avec son auteur (§2aj). ➡️ **Les trois arbitrages ont été rendus le même jour : voir §2am** |

**Points d'attention** : (1) le **flake chromote est récurrent** (déjà vu en
§2ac) — il ne produit pas de FAIL mais ~15 min de stall ; un `skip=1` sur un
fichier `test-shinytest2-*` doit donc être **repassé seul** avant d'être lu
comme une régression ; (2) l'arbre **n'est pas propre** (travail d'un autre
auteur, §2aj + `ROADMAP.md` §5 décision 12) — le commit de cette session
(`63c31a1`) l'**exclut** délibérément ; (3) **aucun** code produit n'a changé
ici : uniquement de la documentation et la baseline ; (4) le **handoff** et le
**kanban** restent **non versionnés** (décision ouverte n°4) — un clone neuf ne
les aura pas, pas plus que les **28** contrats ; (5) l'**audit sc
multi-échantillon / bulk** annoncé par l'utilisateur **n'existe pas dans le
dépôt** (§2am.4) — à fournir avant toute planification sur ce sujet.

### 2al. 🟢 DÉCISION — Moteur CellChat : choix B retenu, état A **conservé** (2026-09-14)

**Arbitrage utilisateur rendu le 2026-09-14** — met fin à l'incertitude notée en
§2ak et tranche la proposition
`docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md`.

| # | Point | Décision |
|---|---|---|
| 1 | **Choix B** — exécuter CellChat dans l'application | ✅ **Retenue** |
| 2 | Nouvelle dépendance | ✅ **Acceptée** si justifiée (amendement 2026-09-13) |
| 3 | **Conserver l'import CSV/TSV externe (état A)** | ✅ **Impératif** — B est un **ajout**, pas un remplacement |
| 4 | **Conserver l'export** des résultats après calcul | ✅ **Impératif** |
| 5 | Version | v1/v2 (`jinworks/CellChat`) ; **v3 `SpatialCellChat` exclu** |

L'utilisateur indique que le calcul dans l'application était **sa volonté
initiale** pour CellChat ; l'import externe reste la voie de secours et
l'interopérabilité avec des résultats produits ailleurs.

**Règle des deux voies** (à ne pas perdre) : les **12 champs canoniques** du
contrat Stage 11 restent la **cible commune**. Le moteur doit produire
**exactement** ce que l'import produit — toute divergence entre les deux voies
est un **bug**, pas une variante.

**Corrections factuelles établies en préparant la décision** :

| Affirmation | Verdict |
|---|---|
| « `CellChat` = dépendance Bioconductor » | ❌ **Faux** — GitHub-only (`devtools::install_github("jinworks/CellChat")`). Corrigé dans **3 docs** le 2026-09-14 |
| « Ce serait la première dépendance GitHub du projet » | ❌ **Faux** — `renv.lock` en a déjà **5** : `BPCells` 0.3.1, `spacexr` 2.2.1, `STdeconvolve` 1.3.2, `SeuratDisk` 0.0.0.9021, `schard` 1.1.0 |
| « Dépendance lourde » | ⚠️ **À nuancer** — `ComplexHeatmap`, `circlize`, `igraph`, `future`/`future.apply`, `cluster`/`doParallel`/`foreach` **déjà présents** ; manquent `NMF` + transitifs, `collapse`, `ggalluvial` → **+8 à +12** sur 425 |
| « Les tableaux 3-D de l'objet sont le goulot RAM » | ❌ **Faux** — `net$prob` est indexé par **groupes** : 20 clusters × ~2 000 LR ≈ **6 Mo** par tableau. Le goulot est la **matrice d'entrée**, pas le résultat |
| « Sous-ensemble LR » présenté comme astuce | ⚠️ **Déjà une étape officielle** — `CellChat::subsetData()`. À **réutiliser**, pas à réimplémenter |
| GPU 5070 Ti | ⛔ **Inutile** — CellChat est 100 % CPU, sans support CUDA |

⚠️ **Deux expertises externes se contredisent sur la RAM** (facteur ~3 à 60 000
cellules : « > 16 Go, risque d'OOM » contre « 32 Go confortables jusqu'à ~100 000
cellules »). **Aucun chiffre ne doit être figé dans le contrat** : le jalon
comporte une **étape de mesure** (durée + RAM sur un jeu 10X public, puis un jeu
moyen) **avant** tout seuil — `max_clusters`, mode RAM-safe, plafond de cellules.
Règle du dépôt déjà payée trois fois (`base_size`, `pageLength`, `plotly`) :
**re-mesurer avant de planifier.**

**Suite** : contraintes d'ingénierie, ordre de travail et arbitrage complet des
trois expertises dans `docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md` §3, §7,
§9, §10.

**Renforcement (audit externe n°3, même date)** :

1. **Terminologie** : **Path A** (import externe) / **Path B** (moteur natif) —
   « état A / état B » sous-entendait un remplacement, ce que la décision exclut.
2. **`analysis_identity` : déjà couverte à ~80 %** — vérifié dans
   `R/core/provenance.R:115` : `new_provenance_entry()` porte déjà `analysis_id`,
   `seed` (**paramètre natif**), `parameters` (→ `nPerm`, `database_version`),
   `dataset_hash`/`hash_exact`/`dataset_dims` (= **l'`input_fingerprint`**) et
   `versions`. ⇒ **étendre, ne pas dupliquer** (règle 3). Seuls **`engine_sha`**
   et **`database_fingerprint`** sont réellement nouveaux ; le second est
   **reporté en V2** (non bloquant pour le MVP). CellChat devient ainsi le
   **premier cas d'implémentation** de l'identité d'analyse transversale.
3. **Aucun ETA, aucun seuil arbitraire** (« clusters > X ») avant benchmark :
   afficher des **faits** (`n_cells`, `n_groups`, interactions candidates), pas
   des prédictions.
4. Ne **jamais** écrire « CellChat est compatible BPCells » : BPCells = stockage
   **résident**, jusqu'au point où CellChat exige une matrice en mémoire.

---

### 2am. ⚖️ Arbitrages utilisateur — 4E-4, UX, NEW-3 (2026-09-14)

**Arbitrages rendus le 2026-09-14** sur les trois points restés ouverts en
§2ak. Ils **débloquent** le jalon 4E-4 et le jalon NEW-3, et **tranchent**
partiellement le volet UX.

| # | Point | Décision de l'utilisateur | Effet |
|---|---|---|---|
| 1 | **4E-4** — exécution async de la DA | **(b) pool applicatif partagé** | 🟢 **DÉBLOQUÉ** |
| 2 | **UX 3B / 4B / 6B** | **4B retenue** ; 3B mise en doute ; 6B non tranchée | 🟡 voir ci-dessous |
| 3 | **NEW-3** — réseau PCSF / interactome | **Interactome embarqué** ; téléchargement en ligne acceptable **si local-first** | 🟢 **DÉBLOQUÉ** |

#### 2am.1 — 4E-4 : pool applicatif partagé, **avec réserve explicite**

Décision : **(b) pool applicatif**. Un seul pool mirai partagé par
l'application (conformité avec la règle « un seul pool de workers : mirai »).

⚠️ **Réserve de l'utilisateur, à ne pas perdre** — citation :

> « même si le pool mirai est pas mal si l'on a à gérer plusieurs échantillons
> exemple, 3 réplicats × 2 conditions (CONTRÔLE et traitement) »

**Conséquence d'ingénierie** : l'implémentation **(b) ne doit pas fermer la
porte au parallélisme par échantillon**. Le cas d'usage nommé est
**6 échantillons** (3 réplicats × 2 conditions Contrôle / Traitement) traités
**en parallèle**. La conception doit donc :

1. router la DA par le pool **partagé** (choix par défaut, règle du dépôt) ;
2. **réserver** la possibilité d'un **pool dédié** (dimensionné, cycle de vie
   propre) pour les exécutions multi-échantillons — sans l'implémenter dans le
   même jalon si elle n'est pas nécessaire ;
3. ne **pas** introduire de second mécanisme de workers (jamais
   `MulticoreParam`, jamais `enableWGCNAThreads()` — règles dures inchangées).

**Point d'ancrage déjà vérifié** (`docs/ROADMAP_HANDOFF_NEXT.md`) :
`R/core/jobs.R:50` `run_job()` ; `R/spatial/spatial_async.R:112,232,294` ;
`modules/sc/mod_sc_da_milo.R:185` ; `R/sc/sc_abundance_milo.R:277`.
Exclusion maintenue : **scCODA** reste hors du périmètre async tant que
reticulate/TensorFlow s'exécute dans le processus appelant.

#### 2am.2 — UX : 4B retenue, 3B jugée non nécessaire — **état MESURÉ**

L'utilisateur écrit : « UX 3B (ok réellement nécessaire ?) ou 4B (ok, FAIT) ».
**Mesure faite dans l'arbre le 2026-09-14** (pas d'hypothèse) :

| Item | État mesuré | Preuve |
|---|---|---|
| **UX 3A** | ✅ **Livrée** | `modules/import/mod_geo.R:25-30` |
| **UX 4A** | ✅ **Livrée** | `modules/import/mod_geo.R:90-96` (bouton de saut LOT 4A) |
| **UX 6A** | ✅ **Livrée** | — |
| **UX 3B** | ⬜ Non livrée | — |
| **UX 4B** | ⬜ **Non livrée** | l'accordéon de mapping est toujours `modules/sc/mod_sc.R:50-52` |
| **UX 6B** | ⬜ Non livrée | — |

⚠️ **Écart levé le 2026-09-14 (même jour)** : l'utilisateur indique « aucune
idée pour 4B, je ne sais plus ce que c'est → **gèle** », et « 4A pourquoi pas
mais **gèle aussi, non prioritaire** ». **Décision finale : tout le volet UX
est GELÉ**, l'écart 4A/4B devient **sans objet** (aucune des deux ne sera
faite dans la vague en cours).

**Rappel de ce que sont ces options** (consigné ici pour ne pas avoir à le
redemander — source `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md` §LOTS 3/4/6) :

| Option | Contenu | État |
|---|---|---|
| **3A** | Section « Livrables » mise en scène dans chaque domaine | ✅ Livrée |
| **3B** | `nav_menu` global « 📄 Rapports » dans `app.R` (re-câblage serveur, e2e lourds) | 🧊 **GELÉE** — non nécessaire (3A suffit) |
| **4A** | Rappel contextuel + bouton de saut dans les onglets d'import (mapping = étape 0) | ✅ Livrée — 🧊 **gelée, non prioritaire** |
| **4B** | **Migration réelle** de l'accordéon mapping vers `mod_import_sc` / `mod_import_bulk` (chaînage réactif modifié = zone à risque règle 1) | 🧊 **GELÉE** |
| **6A** | Onglet GEO renommé « Source publique (GEO) », placé en tête du menu Import | ✅ Livrée |
| **6B** | UI GEO intégrée comme accordéon DANS les onglets SC/Bulk (l'onglet GEO disparaît) | 🧊 **GELÉE** |

**Conséquence** : la refonte UX **ne reprend pas**. Les lots 3/4/6 sont clos
en l'état (A livrées, B gelées). Toute reprise exigera une **décision
utilisateur explicite et nouvelle** — ne pas re-proposer 3B/4B/6B de sa propre
initiative (règle déjà appliquée à CCC 5–6).

#### 2am.3 — NEW-3 : interactome embarqué, **local-first**

Décision : **interactome embarqué** dans l'application. Le téléchargement en
ligne (« comme GEO ») est **acceptable en complément**, « du moment qu'une
solution **local first** existe ».

**Contraintes dérivées** :

1. Un chemin **100 % hors ligne** doit exister : l'interactome est livré avec
   l'application (données/paquet), **aucun réseau n'est requis** pour produire
   un résultat.
2. Le téléchargement en ligne, s'il est ajouté, est un **enrichissement
   optionnel** : échec réseau ⇒ repli silencieux sur l'embarqué, jamais
   d'erreur bloquante.
3. **Mesurer le poids** de l'interactome avant de l'embarquer (règle du dépôt :
   re-mesurer avant de planifier). Le choix de la source (STRING, BioGRID,
   OmniPath…) dépend du volume et de la licence — **à arbitrer sur chiffres**,
   pas à l'intuition.
4. Le backlog conditionnel est levé : NEW-3 n'est plus « interdit sans besoin
   utilisateur concret » — le besoin est exprimé.

#### 2am.4 — Point ouvert : audit annoncé, **absent du dépôt**

L'utilisateur annonce : « POUR LA SUITE j'ai un audit sc multi échantillon et
bulk. »

**Vérification faite le 2026-09-14** : **aucun fichier d'audit sc
multi-échantillon / bulk n'existe dans le dépôt.** Le seul audit présent est
`docs/ROADMAP_HANDOFF_STAGE_CCC_9_AUDIT.md`, qui porte sur la rareté par
population (CCC 9) et **ne concerne pas** ce sujet.

➡️ **Fourni par l'utilisateur le 2026-09-14 au soir** (deux textes en texte
libre). **Vérifié affirmations par affirmations contre le code** →
`docs/AUDIT_VERIFICATION_BULK_SC.md` et **`§2an`** ci-dessous.

### 2an. 🔎 Audit externe Bulk/SC — VÉRIFIÉ contre le code (2026-09-14)

Deux textes d'audit externe reçus le 2026-09-14 (passage 1 : SC / pseudobulk /
bulk multi / comparaison ; passage 2 : Bulk DE / import / logique d'erreur).
**Vérification affirmations par affirmations**, avec `fichier:ligne` :
rapport complet → **`docs/AUDIT_VERIFICATION_BULK_SC.md`**.

| Verdict | Nb |
|---|---:|
| ✅ Confirmée | **20** |
| ⚠️ Confirmée + nuance | **4** |
| ❌ Infirmée | **0** |
| ➕ Trouvée en vérifiant (absente de l'audit) | **5** |

**L'audit est fiable** — aucune affirmation inventée, et ses P0 sont les bons.

#### Les 4 P0 confirmés

1. **`merged_matrix` laisse entrer une matrice non-counts dans le DE** —
   aucune garde (`modules/import/mod_import_bulk.R:881-930`) alors que
   `per_sample` a `.validate_design()` stricte
   (`R/bulk/bulk_import_engine.R:135-163`) ; `build_dds()` se contente d'un
   `warning()` + `round()` (`R/bulk/bulk_helpers.R:129-135`).
2. **`merged_matrix` remplace des métadonnées non alignées par des métadonnées
   artificielles** — test unidirectionnel `all(sample %in% metadata)` puis
   repli destructeur, **sans notification Shiny** (`mod_import_bulk.R:910-917`).
3. **`dds_full` peut devenir stale** — la clé de cache est la **formule seule**
   (« cheap heuristic », commentaire du code) :
   `mod_bulk_de_multimethod.R:72-85`.
4. **NA `sample_id` silencieusement exclus du pseudobulk** — le commentaire
   l'avoue : `mod_sc_pseudobulk.R:110`.

#### ⚠️ Les 4 nuances (là où l'audit est incomplet)

1. **Le correctif existe déjà.** L'audit veut créer
   `assert_raw_count_matrix_for_de()` → **`bulk_assert_raw_counts()` existe**
   (`R/bulk/batch_correction.R:67-104`, bloque NA / négatif / `frac_int <=
   0.95`). Elle n'est simplement **pas câblée sur le DE**. Idem pour
   `analysis_id` / `input_fingerprint` → **`new_provenance_entry()`**
   (`R/core/provenance.R:115`) porte déjà `analysis_id`, `dataset_hash`,
   `hash_exact`, `dataset_dims`, `parameters`, `versions`, `seed`. → **étendre,
   ne pas dupliquer** (règle 3 ; même conclusion que CellChat, §2al).
2. **L'invalidation n'est pas totalement absente.** `re-filtrage → contrastes`
   (`mod_bulk_filter.R:281-289`, avec notification), `auto-pipeline →
   contrastes` (`mod_bulk.R:325`) et `pseudobulk → de_result`
   (`mod_sc_pseudobulk.R:320`) **sont** câblées. Ce qui manque :
   **mapping → aval** et **vst → WGCNA/pathways/signatures**. Le défaut réel
   est l'**irrégularité**, pas l'inexistence.
3. **`rankConsensus()` est documenté, pas accidentel** — le roxygen dit
   explicitement que les `padj` NA sont exclus (`bulk_helpers.R:546-548`). Le
   défaut est de **traçabilité dans le résultat**, pas d'ignorance.
4. **Le type de gène est déjà détecté** — `detect_gene_id_type()`
   (`R/core/io_helpers.R:1016`) est utilisé à 4 endroits. Ce qui manque est le
   **contrôle au moment de la comparaison**, pas la détection.

#### ➕ Les 5 problèmes trouvés en vérifiant (absents de l'audit)

| # | Constat | Preuve | Gravité |
|---|---|---|---|
| N1 | `round()` silencieux **aussi** côté edgeR/limma, **sans `warning`** | `R/bulk/bulk_helpers.R:300` | 🔴 |
| N2 | **Aucun contrôle de design n'est bloquant** — simple bannière ; le bouton s'active dès que `filtered_counts` existe | `mod_bulk_de_engine.R:217-229` + `mod_bulk_de_run.R:32` | 🔴 |
| N3 | Colonne de condition absente ⇒ **zéro problème remonté** | `R/core/validation.R:52` | 🟠 |
| N4 | Le chemin DE ne produit **aucune provenance** (0 occurrence dans `modules/bulk_de/`) | grep | 🔴 |
| N5 | Le bug `intersect(covariates, …)` est en **3** endroits, pas 2 | `validation.R:70`, edgeR, limma | 🟠 |

#### Position

L'audit recommande un **thin vertical slice sur Bulk DE** (pas de gros
refactor) — **accord total**, cohérent avec la règle 1. Ordre proposé en 8
jalons (DE-GUARD → META-IDENTITY → COVARIATES-BLOCK → RANK-BLOCK →
SHRINK-TRACE → CONTRAST-IDENTITY → INVALIDATION-GRAPH → PSEUDOBULK-FIX) :
`docs/AUDIT_VERIFICATION_BULK_SC.md` §5.

⚠️ **Réserve de méthode** : vérification **statique**. Avant toute correction,
**écrire le test qui échoue** — sinon on risque de « corriger » une branche
inatteignable en pratique.

---

### 2ao. ✅ Moteur CellChat natif — CHANTIER CLOS, CC-1..CC-5 livrés (2026-09-14 → 2026-09-15)

Ouverture du chantier décidé en §2al (choix **B** retenu, Path A **conservé**).
Ordre de travail : `docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md` §7. Suivi
opérationnel : `docs/ROADMAP.md` **§2.0bis**.

#### 2ao.1 — Mesure préalable : l'empreinte réelle corrige l'estimation §2.2

La proposition annonçait « **+8 à +12** » entrées de lockfile. **Mesuré contre
le `DESCRIPTION` amont réel** (`jinworks/CellChat` master, version
**2.2.0.9001** — ligne v2, conforme à la décision §8 point 5 ; `v3
SpatialCellChat` bien exclu) :

| Mesure | Valeur |
|---|---|
| Dépendances directes réelles (`Depends` + `Imports` + `LinkingTo`) | **39** |
| Déjà dans `renv.lock` | **32** ✅ |
| Manquantes (directes) | **7** — `collapse`, `ggalluvial`, `ggnetwork`, `ggpubr`, `NMF`, `sna`, `methods` |
| Fermeture transitive hors lock | **+10** — `coda`, `corrplot`, `ggsci`, `ggsignif`, `gridBase`, `network`, `polynom`, `registry`, `rngtools`, `rstatix`, `statnet.common` |
| **Total hors lock** | **17** (dont 9 paquets **base/recommendés** — `graphics`, `grDevices`, `grid`, `methods`, `parallel`, `splines`, `stats`, `tools`, `utils` — que renv n'enregistre jamais) |

**Deux lectures, et elles ne disent pas la même chose :**

1. **La thèse de fond de la proposition est CONFIRMÉE** — la chaîne lourde est
   déjà payée : `ComplexHeatmap` 2.22.0, `circlize`, `igraph`, `Rcpp`,
   **`RcppEigen`**, `RSpectra`, `FNN`, `BiocNeighbors`, `future`/
   `future.apply`, `reticulate`, `plotly`, `shiny`, `bslib`… sont **déjà** au
   lock. « Dépendance lourde » était bien une nuance juste.
2. **Le chiffre, lui, était faux** — et pour une raison instructive : il ne
   comptait que les paquets *que l'auteur connaissait*. À périmètre mesuré,
   **11 paquets à installer** + `CellChat`. Parmi les 17 manquants, **6 sont
   déjà installés dans la bibliothèque mais non enregistrés** (`corrplot`,
   `ggpubr`, `ggsci`, `ggsignif`, `polynom`, `rstatix`) — ils arrivent avec
   `GSVA`/`WGCNA`/`survminer`, pas avec CellChat.
   ⇒ **4ᵉ occurrence de la règle « re-mesurer avant de planifier »**
   (`base_size`, `pageLength`, `plotly`, maintenant `CellChat`).

⚠️ **Point d'attention reporté** : `LinkingTo: Rcpp, RcppEigen` ⇒ **CellChat
doit être compilé** (Rtools requis). Rtools **4.4** est présent
(`C:\RBuildTools\4.4`, `x86_64-w64-mingw32.static.posix`), mais **n'est pas dans
le `PATH`** au démarrage de R — à positionner avant tout `renv::install()`.

#### 2ao.2 — ⛔ Interdiction nouvelle : jamais de `renv::snapshot()` global

`renv::status()` mesuré le 2026-09-14 → **`synchronized = FALSE`** :

- **20 paquets en dérive de version** lockfile ≠ bibliothèque (`igraph` 2.2.1 vs
  **2.3.3**, `future` 1.75.0 vs 1.70.0, `bit64`, `bslib`, `xml2`, `sf`…) ;
- **48 paquets « utilisés mais non enregistrés »** — `WGCNA`, `GSVA`, `sva`,
  `survminer`, `Hmisc`, `msigdbr`, `decoupleR`, `variancePartition`… ;
- 1 paquet GitHub en dérive de référence (`spacexr` : `HEAD` vs `master`).

Un `renv::snapshot()` capturerait **toute cette dette**, étrangère au jalon, dans
le commit du moteur CellChat. **Règle du jalon : insertion chirurgicale**
(précédent NEW-1 / `drc`, insertion manuelle 419 → 425). La dérive constatée est
consignée ici ; elle **n'est pas** corrigée dans ce jalon.

#### 2ao.3 — Jalons

| # | Contenu | État |
|---|---|---|
| **CC-1** | Épinglage `CellChat` par **SHA** + insertion chirurgicale au lockfile | ✅ **LIVRÉ** (réserve d'installation : §2ao.5) |
| **CC-2** | Contrat gelé `docs/contracts/CELLCHAT_ENGINE_CONTRACT.md` | ✅ **LIVRÉ** |
| **CC-3** | `R/sc/sc_communication_engine.R` — `run_cellchat()`, réduction immédiate aux 12 champs | ✅ **LIVRÉ** |
| **CC-4** | Test `tests/testthat/test-sc-communication-engine.R` (éponyme C9 + assertions de gel) + gardes | ✅ **LIVRÉ** |
| **CC-5** | Module UI Path B + export conservé | ✅ **LIVRÉ** — voir §2ao.6 |

#### 2ao.4 — Bilan de la session (2026-09-14 → 2026-09-15)

**Livré dans le même commit** (contract-first : code + test + doc) :

| Fichier | Rôle |
|---|---|
| `R/sc/sc_communication_engine.R` | moteur natif, ~470 lignes |
| `tests/testthat/test-sc-communication-engine.R` | 53 assertions + 1 skip (run réel, exige CellChat) |
| `docs/contracts/CELLCHAT_ENGINE_CONTRACT.md` | **29ᵉ contrat gelé** |
| `config/defaults.R` | `TS_CELLCHAT_NBOOT_DEFAULT`, `TS_CELLCHAT_SEED_DEFAULT`, `TS_CELLCHAT_MIN_GROUPS` |
| `app.R` | `source()` du moteur **après** `sc_communication_input.R` |
| `R/sc/sc_communication.R` | **un** argument additif `computation = c("import","engine")` (défaut `"import"`) |
| `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md` | §1 et §5 requalifiés (le calcul dans l'app existe, l'import ne génère toujours rien) |

📌 **Application minimale de la décision ouverte n°4** (`ROADMAP.md` §5) :
`docs/` reste non versionné **sauf** `STATUS.md`, `ROADMAP.md`,
`CONVENTIONS.md` — et, dans ce commit, les **deux contrats concernés par le
jalon** (`CELLCHAT_ENGINE_CONTRACT.md`, `COMMUNICATION_RESULT_CONTRACT.md`),
ajoutés en `git add -f`. Sans cela la règle contract-first serait
mécaniquement inapplicable (le doc ne peut pas être dans le même commit).
Précédent : `PLOT_DATATABLE_CONTRACT.md` est déjà suivi. **Réversible** :
`git rm --cached` suffit à revenir en arrière.

**Gardes au vert, à la baseline exacte** : conventions **0 erreur / 324 avert.**
(C9 revenues à **37** après correction — voir ci-dessous) ; duplication
**0 erreur / 3 avert.** ; tests ciblés **270 assertions, 0 échec**.

**Trois pièges payés en chemin** :

1. **`nboot`, pas `nPerm`.** La proposition §9.4 signalait le risque sans le
   lever ; vérifié sur la signature amont de `computeCommunProb()` : le
   paramètre de permutation s'appelle **`nboot`** (défaut 100) et la graine
   **`seed.use`** (défaut 1L). CellChat appelle lui-même `set.seed(seed.use)` en
   interne ⇒ la graine est bien un **paramètre du calcul**. Le test de gel
   interdit désormais l'apparition de `nPerm`.
2. **Le nom d'interaction n'est pas découpable.** Les colonnes
   `ligand`/`receptor`/`pathway` sont lues dans `object@LR$LRsig` (appariement
   exact sur `interaction_name`), jamais par `strsplit()` — un
   `interaction_name` peut contenir plusieurs sous-unités (`L_R1_R2`).
3. **C9 a augmenté à 38** au premier passage : le fichier moteur n'avait pas de
   test **éponyme**. Corrigé en adoptant le motif déjà documenté pour 5
   contrats (assertions de gel dans le test éponyme du domaine) — le fichier
   s'appelle donc `test-sc-communication-engine.R`, pas
   `test-cellchat-engine-contract-freeze.R`.

**🔎 Constat annexe (à traiter hors jalon).** `parse_cellchat_object()`
(`R/sc/sc_communication.R:342`) suppose que `net$prob` est indexé
`[ligand, receptor, "sender|receiver"]`. La structure **réelle** de CellChat
est `[source, target, interaction_name]` (vérifié : `dimnames(Prob) <-
list(levels(group), levels(group), rownames(pairLRsig))`). La route « objet S4 »
de Path A échouerait donc sur un vrai objet CellChat (les noms de paires ne
contiennent pas `|`). Non corrigé ici — c'est un gel de Stage 11, à ouvrir
comme jalon distinct avec son test qui échoue d'abord.

**CC-1 non abouti — état précis (important pour la reprise).**

| Étape | Résultat |
|---|---|
| Rtools 4.4 | ✅ présent et détecté (`C:\RBuildTools\4.4`, `has_build_tools = TRUE`) — ⚠️ **absent du `PATH`** au démarrage de R : à ajouter `…\x86_64-w64-mingw32.static.posix\bin` **et** `…\usr\bin` |
| Les **11 dépendances** à installer | ✅ **installées** dans la bibliothèque du projet (`collapse` 2.1.8 — compilé depuis les sources en 2,3 min ; `NMF` 0.28, `sna` 2.8, `network` 1.20.0, `statnet.common` 4.13.0, `ggnetwork` 0.5.14, `ggalluvial` 0.12.6, `gridBase` 0.4-7, `registry` 0.5-1, `rngtools` 1.5.2, `coda` 0.19-4.1) |
| Compilation de **CellChat** | ✅ le C++ (Rcpp/RcppEigen) **compile** — `CellChat.dll` produite |
| Installation de **CellChat** | ❌ **échec** : `ERROR: lazy loading failed for package 'CellChat'` après « moving datasets to lazyload DB » |
| `renv.lock` | **intact** (425 entrées) — volontairement non touché |

**Deux causes écartées, une reste ouverte :**

1. ❌ Ce n'est **pas** la résolution des dépendances : installées une par une,
   elles aboutissent en 2,5 min. ❌ Ce n'est **pas** un import manquant : les
   **38 imports** de CellChat se chargent tous (`loadNamespace` OK sur chacun).
2. ⚠️ **Non diagnostiqué** : `R CMD INSTALL` **n'affiche pas** le message
   d'erreur R sous Windows (seul « ERROR: lazy loading failed » apparaît ; ni
   `stdout=TRUE, stderr=TRUE`, ni `--verbose` ne le capturent). La piste à
   ouvrir est donc l'exécution manuelle de l'étape de lazy-load
   (`tools:::.install_packages` / `loadNamespace` sur le répertoire `00new`).

**Trois pièges d'outillage à retenir** (coût réel : ~1 h) :

- `renv::install("<user>/<repo>")` **se bloque sans aucune activité disque** sur
  ce poste — deux tentatives, ~30 min chacune, rien d'installé. La cause est
  vraisemblablement le **prompt d'identifiants git** (`gitcreds` : un
  `gitcreds-stderr-*` apparaît puis plus rien) : `remotes` passe par git, qui
  attend une saisie impossible en `Rscript` non interactif.
- `capture.output(..., file = log)` **bufferise tout** : une installation longue
  paraît « bloquée » alors qu'elle progresse. Toujours laisser la sortie visible.
- `R.home("bin")` vaut `…/bin/x64` sous Windows : `file.path(R.home("bin"),
  "Rcmd.exe")` est correct, `…/bin/Rcmd.exe` n'existe pas.

#### 2ao.5 — CC-1 ABOUTI : pin, installation tolérée, insertion lockfile (2026-09-15)

**Cause racine trouvée** (elle manquait au §2ao.4) : sur ce poste, **tout**
processus R qui charge `dplyr`, `ggplot2`, `igraph`, `NMF` ou `ggnetwork` sort
en **139** — mais **au teardown**, c'est-à-dire **après** avoir fait son travail
(contrôle : `stats`, `Matrix` et `circlize` sortent en 0). Or `R CMD INSTALL`
lance un **sous-processus R** pour le lazy-load ; celui-ci écrit
`R/CellChat.rdb`, puis meurt en 139 ⇒ INSTALL conclut « lazy loading failed » et
**supprime** un paquet pourtant complet.

| Hypothèse | Verdict |
|---|---|
| Paquets construits sous R 4.4.3 alors que R tourne en 4.4.2 | ❌ **infirmée** : `Matrix` est construit sous 4.4.3 **et** sort en 0 ; `igraph` est construit sous 4.4.2 **et** sort en 139 |

**Contournement retenu** : `R CMD INSTALL --no-lock --no-clean-on-error
--no-test-load` conserve le paquet complet ; `tools:::.install_package_namespace_info()`
écrit ensuite le `Meta/nsInfo.rds` manquant (imports/exports/dynlibs/S3methods).

⚠️ **Réserve assumée** : l'arbre `Meta/` reste incomplet (`data.rds`, index
d'aide). Le paquet **fonctionne** — version 2.2.0.9001, 19 formals de
`computeCommunProb` dont `nboot` et `seed.use`, base LR 3 233 interactions, S4
`new("CellChat")` + `slotNames` OK — mais une réinstallation propre suppose de
lever le segfault de teardown.

**Verrouillage par SHA** : `git ls-remote` → `75253cd0…358f`, téléchargé en
tarball (`CellChat` est absent de `available.packages()` : ni CRAN ni
Bioconductor, confirmé). Les champs `Remote*`/`GithubSHA1` ont été reportés dans
le `DESCRIPTION` installé — ce que `remotes` aurait écrit — pour que
`engine_sha` soit traçable. Valeur lue par le moteur :
`75253cd0c9e68410e6e721a6d3a0419a1d7e358f`.

**Insertion lockfile PUREMENT ADDITIVE** : 425 → **438** entrées, **+535 / −0
lignes**, 0 changement de version.

⚠️ **Piège payé — `renv::record()` n'est pas chirurgical** : il ajoute bien les
13 entrées **mais réécrit** 10 entrées préexistantes (`colorspace`, `drc`,
`mgcv`, `multcomp`, `mvtnorm`, `nanonext`, `plotrix`, `sandwich`, `shinyFiles`,
`shinycssloaders`) — du churn sur le travail d'un jalon antérieur. Procédure
retenue : récupérer le **texte** produit par renv pour les 13 entrées,
restaurer `renv.lock` depuis HEAD, puis réinsérer les blocs à leur position
alphabétique. Bilan final : **0 suppression**.

⚠️ **Isolation** : `ggpubr` (dépendance directe) est résolu depuis la
bibliothèque **système** `R-4.4.2/library`, pas depuis la bibliothèque renv du
projet. Enregistré au lockfile, mais l'isolation n'est pas totale tant qu'il
n'est pas installé côté projet.

**Trois pièges de plus, découverts en exécutant ENFIN le moteur :**

1. **`CellChatDB.human$version` n'existe pas** (NULL, vérifié). La version est
   portée par **chaque ligne** de `interaction$version`, et la base est
   **mixte** (`CellChatDB v1` **+** `CellChatDB v2`). `.cellchat_engine_db()`
   lisait `db[["version"]]` ⇒ `database_version` serait resté **NA** sans que
   rien ne l'indique. Corrigé : valeurs distinctes réellement présentes,
   jointes par `"; "`.
2. **`utils::packageDescription()` ne voit pas les champs `Remote*`** : il lit
   de préférence `Meta/package.rds`, alors que `read.dcf()` sur le
   `DESCRIPTION` en voit 29 dont `RemoteSha`. Corrigé par un repli `read.dcf()`.
3. **Un jeu trop pauvre fait échouer CellChat** sur « subscript out of bounds »,
   parce qu'il annonce « 0 highly variable ligand-receptor pairs ». Le moteur
   détecte désormais `nrow(LR$LRsig) == 0` **avant** l'appel et lève
   `no_interactions` avec un message qui dit ce qui manque. Le test jouet
   correspondant a été ajouté — un fixture de 47 gènes ne produit **aucune**
   paire, un fixture avec de vrais gènes de signalisation en produit **109**.

**Vérifications** : `renv::lockfile_read()` → 438 entrées, CellChat
`Source: GitHub` + SHA ; gardes **0 erreur / 324 avert.** et **0 erreur /
3 avert.** ; `test-sc-communication-engine.R` : **74 assertions, 0 échec,
0 skip** — le bloc « run réel » n'est plus skippé et valide
`net$prob = [source, target, interaction_name]`.

**Conséquence produit** : le moteur est livré mais **non exécutable** —
`run_cellchat()` lève `missing_dependency` (état prévu, avec guidage
d'installation), l'application démarre normalement et **Path A continue de
fonctionner**. Les dépendances déjà installées ne sont **pas** enregistrées dans
`renv.lock` : on n'épingle pas des orphelins sans le paquet qui les requiert.

**Invariants à ne pas perdre** (proposition §3, §9, §10) : Path A **conservé** ·
export **conservé** · `build_cellchat_input()` **inchangé** · 12 champs
canoniques = **contrat commun aux deux voies** · objet moteur **jamais** dans un
`reactiveValues` · `future::plan("sequential")` **à l'intérieur** du job mirai ·
graine **explicite et tracée** · `database_version` **tracée** ·
`updateCellChatDB()` **interdit** · comparaison inter-condition **hors moteur**
(porte DA) · aucun ETA ni seuil de clusters **avant benchmark**.

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
| Dépendance CellChat dans `renv.lock` | ⛔ Toujours absente — **aucun appel CellChat n'est fait** — 🟢 mais **décision rendue** (§2al, 2026-09-14) : à ajouter, épinglée par **SHA** (GitHub, **pas** Bioconductor) |
| Contrat d'entrée de l'app upstream | ⛔ Toujours non gelé — voir ci-dessous |

**Décision (2026-09-10)** : l'application upstream (fasta/fastq bruts → données
pour Cerberus) est **en cours de développement par l'utilisateur**. Décision
retenue : **ne pas démarrer les phases CCC 5–6 ni 4D-3 maintenant.**

> ⚠️ **Qualification du 2026-09-14 (décision §2al).** Cette interdiction est
> **exacte pour la voie « ingestion d'artefacts upstream »** — on y lit des
> fichiers produits par l'app upstream, son contrat est donc décisif — et
> **inopérante pour la voie « moteur »** : `build_cellchat_input(obj)` consomme
> l'**objet Seurat déjà en mémoire**, aucun artefact upstream n'entre en jeu.
> Le moteur est donc débloqué par la **décision de dépendance** (§2al), **pas**
> par le contrat upstream. Lire le paragraphe ci-dessus comme visant la voie
> ingestion.

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

> ⚖️ **TRANCHÉ LE 2026-09-14 — voir §2am.1.** Option retenue : **(b) pool
> applicatif unique partagé**, avec la **réserve explicite** de l'utilisateur :
> un **pool mirai dédié** reste pertinent pour traiter **plusieurs échantillons
> en parallèle** (ex. **3 réplicats × 2 conditions** = 6 échantillons
> Contrôle / Traitement). ⇒ L'implémentation ne doit **pas fermer la porte** au
> parallélisme par échantillon, mais n'introduit **pas** de second mécanisme de
> workers (jamais `MulticoreParam`, jamais `enableWGCNAThreads()`).

Trois options, l'une tranchée :

| Option | Coût | Risque | Verdict |
|---|---|---|---|
| Pool mirai dédié au SC (non-Spatial) | Moyen | Deux pools à gérer | 🟡 **Réserve** — reste pertinent pour le multi-échantillons |
| Pool applicatif unique partagé | Faible | Contention avec Spatial | ✅ **RETENUE** |
| Rester synchrone (V1.0, assumé) | Nul | DA longue bloque l'UI | ❌ Écartée |

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

#### 2ao.6 — CC-5 LIVRÉ : UI « calculer dans l'application » (2026-09-15)

Le moteur était exécutable mais **inexposable** : aucune action ne permettait
de lancer un calcul. `modules/sc/mod_sc_communication.R` gagne une **5ᵉ
source** : `cellchat_engine` — « Calculer dans l'application (CellChat) ».

| Élément | Choix | Raison |
|---|---|---|
| `comm_engine_species` | **sans défaut** (`selected = character(0)`) | La base LR en découle et ne se devine pas ⇒ choix **déclaré**, bloquant si absent. Même motif que le mode LIANA. |
| `comm_engine_seed` / `comm_engine_nboot` | valeurs `TS_CELLCHAT_*` | Visibles et tracées : la graine est un paramètre du calcul. |
| `comm_compute` | bouton dédié | Le bouton « Importer et valider » est **masqué** pour cette source (il exigerait un fichier inexistant) ; la branche import s'en protège aussi explicitement. |

**Règle des deux voies, côté UI** : les deux chemins déposent leur résultat via
le **même** `.store_result()`. Aucune vue (DotPlot, heatmap pathways, réseau,
centralité) ni aucun export (CSV/RDS) n'a eu à changer — **c'est la preuve
mécanique que le moteur produit bien la forme canonique**.

**Dépendance paresseuse** : si `cellchat_engine_available()` est `FALSE`, le
message donne le remède et rappelle que l'import reste disponible. Les erreurs
du moteur sont affichées **avec leur état** (`[etat : no_interactions]`…).

⚠️ **Invariant gelé reformulé, pas affaibli.**
`test-communication-contract-freeze.R` interdisait `CellChatDB` dans le module :
ce grep interdisait aussi le simple **libellé** du choix d'espèce
(« Humain (CellChatDB.human) »). Reformulé en énumérant les vrais interdits
(`library(`, `computeCommunProb`, `CellChatDB$`, `CellChat::`) et en vérifiant
que le nom de la base n'apparaît que dans des libellés (**2 occurrences**).

**Vérifications** : 388 tests du domaine communication (dont **53 assertions UI
nouvelles**), 0 échec, 0 skip ; i18n **intègre** (13 clés, aucune dupliquée) ;
gardes **0 erreur / 324 avert.** et **0 erreur / 3 avert.** — et surtout
« aucun bloc dupliqué détecté », qui valide la factorisation `.store_result()`.

**Chantier CLOS** : CC-1 → CC-5 tous livrés.

#### 2ao.7 — DÉFAUT CORRIGÉ : `parse_cellchat_object()` ne savait pas lire un VRAI objet (2026-09-15)

**Découverte** : après CC-5, deux extracteurs de `net$prob` coexistaient avec
des **hypothèses incompatibles** — `.cellchat_engine_extract()`
(`[source, target, interaction_name]`, la forme réelle) et
`parse_cellchat_object()` (`[ligand, récepteur, "sender|receiver"]`).

**Mesuré** sur un objet CellChat 2.2.0.9001 réel (jamais supposé) :

| Mesure | Valeur |
|---|---|
| `dim(net$prob)` | **3 × 3 × 109** = `[groupe source, groupe cible, interaction_name]` |
| noms de dim3 contenant `\|` | **0 / 109** (`CXCL1_ACKR1`, `TGFB1_TGFBR1_TGFBR2`…) |
| `parse_cellchat_object()` sur cet objet | ❌ `invalid_schema` — « 109 nom(s) de paire sans separateur '\|' unique » |
| `.cellchat_engine_extract()` sur le **même** objet | ✅ 981 lignes, senders/receivers/ligands/pathways corrects |

**Conséquence** : la route « importer un objet CellChat (.rds) » était cassée
pour **tout objet réel**. Elle échouait bruyamment (aucune donnée fabriquée)
mais en accusant le fichier de l'utilisateur. Aucun test ne l'avait vu : ils
passaient tous par un **stub de la forme fictive**, jamais par un objet réel —
la forme fictive était même **acceptée silencieusement**.

**Correctif (règle 3 — étendre, ne pas dupliquer)** : `parse_cellchat_object()`
**délègue** l'extraction à `.cellchat_engine_extract()`, qui devient la SEULE
lecture de `net$prob` de l'application. Un accesseur tolérant
(`.cellchat_engine_slot()`) permet de traiter l'objet S4 **ou** une liste
nommée `net`/`LR` (route de test, forme désormais identique à la réalité). Les
états du moteur sont **traduits** dans le vocabulaire d'import
(`no_interactions` → `invalid_input`, sinon `invalid_schema`).

**Changement de comportement assumé** : `pathway` n'est plus systématiquement
NA sur cette route (résolu via `@LR$LRsig`). C'est un alignement des deux
voies : `COMMUNICATION_RESULT_CONTRACT.md` §5 a donc été modifié **dans le
même commit** que le code et que son test de gel.

**Garde ajoutée** : `test-communication-contract-freeze.R` interdit le retour
du découpage fictif (`pairs_split`, `dimnames(prob)[[`). Le découpage sur `|`
reste **légitime** pour CellPhoneDB (`interacting_pair`) — l'interdit vise la
relecture de `net$prob` hors du moteur.

**Piège récurrent confirmé (3ᵉ occurrence)** : un contrat « documenté » +
« testable » + « testé » peut être **faux** tant qu'aucun test ne l'exerce
contre le **réel**. Une fixture est une assertion de plus, pas une preuve.

### 2ap. 🐛 HOTFIX — `ns` introuvable dans deux serveurs de modules (2026-09-15)

**Symptôme** (console, à l'affichage du panneau Réseau) :

```text
116: renderUI [modules/sc/mod_sc_pathways.R#231]
=> impossible de trouver la fonction "ns"
```

**Cause** : `ns` est lié par `NS(id)` dans les fonctions **UI** uniquement. Un
serveur `moduleServer(id, function(input, output, session) {...})` évalue son
corps dans l'environnement de la fonction englobante, où `ns` **n'existe pas**.
Un `renderUI` qui appelle `ns()` lève donc à l'exécution — **et seulement quand
la branche s'affiche** : invisible au démarrage, invisible aux tests qui ne
rendent pas l'UI.

**Deux occurrences** (la seconde trouvée par balayage systématique, pas par le
rapport de bug) :

| Fichier | Serveur | `renderUI` fautif |
|---|---|---|
| `modules/sc/mod_sc_pathways.R` | `mod_sc_pathways_server` | `network_ui` (réseau STAT-S2), 3 appels |
| `modules/sc/mod_sc.R` | `mod_sc_server` | `multisample_overview_ui`, 4 appels |

**Correctif** : `ns <- session$ns` en tête des deux serveurs. Le motif
`session$ns(...)` (déjà employé ailleurs dans le dépôt, ex.
`mod_sc_da_design.R`) y échappe également — il reste donc valide.

**Garde ajoutée** — `C15` dans `test-release-hardening.R` : balayage **statique**
de `modules/` ; tout serveur appelant `ns()` doit le lier. Elle attrape la
**classe** entière, pas ces deux occurrences. **Cas négatif vérifié** : la garde
détecte bien les deux versions committées d'avant correctif.

**Leçon** — même famille que §2ao.7 : ce qui n'est pas exercé **au moment où ça
casse** passe inaperçu, quel que soit le nombre de tests verts. Ici le
déclencheur est l'affichage d'une branche d'UI ; là c'était un objet réel. Un
test statique de la classe est le seul filet qui tienne dans les deux cas.

### 2aq. 🔒 Herméticité renv — mesure, garde, et trou restant (2026-09-15)

**Nouveau garde** : `tools/check_renv_hermeticity.R`. Pour chaque paquet du
lock, il vérifie **où il se résout** (`find.package()`, sans charger). Sortie
`0` si hermétique. Il ne charge aucun paquet ⇒ il échappe au segfault de
teardown 139 qui rend `renv::status()` illisible en sortie standard.

**Mesure initiale : 7 paquets hors bibliothèque projet**, pas un seul — tous
résolus depuis `D:/Data_science/R-4.4.2/library` :

`AsioHeaders`, `chromote`, `ggpubr`, `pingr`, `shiny.i18n`, `shinytest2`,
`websocket`

⚠️ `shiny.i18n` est une dépendance **d'exécution** (l'i18n de l'application),
pas un détail de test. Ce n'était pas « juste ggpubr ».

**Versions vérifiées IDENTIQUES au lock pour les 7** avant d'agir : déplacer
ne change donc aucun comportement. 6 installés par
`renv::install(..., lock = FALSE)` (lockfile **inchangé**, md5 identique).

**`ggpubr` a résisté — et pourquoi** : aucun binaire Windows pour la version
du lock **1.0.0** (seul **0.6.3** est distribué en binaire). renv doit donc
construire depuis les sources ⇒ `ERROR: lazy loading failed` : le segfault de
teardown déjà rencontré en CC-1. Même contournement :
`Rcmd INSTALL --no-clean-on-error --no-test-load` puis
`tools:::.install_package_namespace_info()` (127 exports,
`loadNamespace("ggpubr")` OK).
👉 Leçon : `renv::install()` échoue **en silence sur la cause** — il ne dit pas
« pas de binaire », il dit « installation failed ».

**Après correction : 426 / 426 hermétiques, 0 erreur.**

**IL RESTE UN TROU PLUS GROS — mesuré, NON corrigé (jalon distinct).**
**11 paquets utilisés par le code sont ABSENTS de `renv.lock`** :

`decoupleR`, `GSVA`, `loomR`, `msigdbr`, `sceasy`, `survminer`, `sva`,
`variancePartition`, `WGCNA` — et **`dorothea` / `progeny` non installés du
tout**.

`renv::restore()` sur une machine propre ne les poserait pas. Les enregistrer
impose la procédure chirurgicale (§2ao.2 : jamais de `renv::snapshot()`
global ; `renv::record()` n'est **pas** chirurgical).

À noter : `renv::status()` liste aussi des paquets en **dérive de version**
(bibliothèque ≠ lock, ex. `igraph`, `bslib`, `future`, `sf`) et un
`CellChat [ref: main != master]` — la référence et non le SHA. Compte
**re-mesuré à 14** au §2ar (le « 22 » antérieur était périmé).

### 2ar. Fermeture de dépendances du lock — 10 trous pré-existants, puis 35, puis 0 (2026-09-15)

**Le défaut n'a pas été introduit ici : il était déjà là.** Un lockfile n'est
restaurable que si la **fermeture** `Depends`/`Imports`/`LinkingTo` de ses
paquets y figure aussi. Mesure, à chaque étape :

| État | Paquets | Trous de fermeture |
|---|---|---|
| `HEAD` | 438 | **10** |
| après enregistrement des 9 utilisés-non-déclarés | 447 | **35** (+24) |
| après fermeture complète | **482** | **0** |

Les **10 trous de `HEAD`** venaient de paquets **déjà** dans le lock :
`ggpubr` → `ggsci`, `ggsignif`, `polynom`, `rstatix` ; `miloR` →
`ggbeeswarm`, `pracma` ; `jsonlite` → `RcppML`. Autrement dit
**`renv::restore()` ne tenait pas déjà sur une machine propre avant ce jalon**.
Enregistrer les 9 paquets a mécaniquement ajouté **24** trous (leurs propres
dépendances) : c'est le comportement attendu, pas une régression.

**Corrigé** — 44 paquets enregistrés (447 → 482) et **15 ramenés** de
`R-4.4.2/library` vers la bibliothèque du projet (`beeswarm`, `corrplot`,
`ggbeeswarm`, `ggsci`, `ggsignif`, `Hmisc`, `htmlTable`, `litedown`,
`markdown`, `polynom`, `preprocessCore`, `RcppML`, `rstatix`,
`SpatialExperiment`, `vipor`). Les 4 paquets Bioconductor passés de la forme
minoritaire `Source: Repository` + `Repository: BioCsoft` (4 occurrences,
écrite par `renv::record()`) à la forme **canonique** `Source: Bioconductor` +
`Repository: "Bioconductor 3.20"` (67 occurrences, et ce qu'écrit
`renv::snapshot()`).
👉 **470 / 470 hermétiques, 0 hors projet, 0 trou de fermeture, 0 erreur**
(2 avertissements = `dorothea`, `progeny`, optionnels non installés).

**Garde étendu (§4)** — `tools/check_renv_hermeticity.R` remonte désormais la
fermeture via `read.dcf` (base R, **aucun paquet chargé** ⇒ échappe au 139) et
compte comme erreur tout paquet de la fermeture absent du lock. Il mesure la
vraie propriété — *« `restore()` tient-il ? »* — au lieu de *« chaque entrée
existe-t-elle ? »*. **Vérifié sur un cas négatif** : le lock de `HEAD` est
signalé à 10 trous.

**Deux notes techniques réutilisables :**
1. **Enregistrer un paquet GitHub hors réseau.** `renv::record("owner/repo@sha")`
   **bloque** (4 min 27 sans résultat, résolution de remote distante) et
   `renv::record("sceasy")` répond `failed to resolve remote`. Solution :
   `renv:::renv_snapshot_description(<chemin du paquet installé>)` — la fonction
   que renv emploie lui-même, hors réseau, et **garantie identique** à ce qu'un
   `snapshot()` écrirait. Elle **écarte** les champs hérités `Github*`
   (`GithubRepo`, `GithubUsername`, `GithubRef`, `GithubSHA1`, `GithubHost`) et
   ne garde que les `Remote*`.
2. **`git diff --numstat` peut mentir sur les suppressions.** Insérer un gros
   bloc fait mal aligner le diff de **Myers** : ce jalon affichait `1631 23`
   alors qu'**aucun** bloc pré-existant n'était modifié. Vérification fiable :
   `git diff --diff-algorithm=patience --numstat` (`1608 0`), **puis**
   comparaison **bloc par bloc** des `Packages` entre `HEAD` et le fichier
   (438 → 482, **0 disparu, 0 modifié**). La règle « 0 suppression » du §2ao.2
   reste bonne mais **le moyen de la vérifier** doit être le diff `patience` ou
   la comparaison de blocs, pas le diff par défaut.

**⚠️ Reste ouvert (jalon distinct) — 14 dérives de version, TOUTES pré-existantes.**
`bbotk`, `bit64`, `bslib`, `class`, `future`, `hexbin`, `igraph` (lock 2.2.1 /
installé 2.3.3), `mlr3learners`, `nnet`, `sf`, `spatstat.explore`,
`spatstat.geom`, `spatstat.random`, `xml2`. Vérifié : leur version au lock est
**identique à `HEAD`** — ce jalon n'en a touché aucune. C'est ce que
`renv::status()` signale encore (`synchronized: FALSE`), et non plus un paquet
manquant. Les corriger suppose de **réinstaller** ces 14 aux versions du lock
(ou de mettre le lock à jour), donc **re-mesurer la suite complète** ensuite :
jalon distinct, non ouvert.

### 2as. 🟢 Porte de merge RÉPARÉE — les 218 erreurs C7 étaient un artefact de LOCALE (2026-09-15)

**Chantier demandé** : deux chantiers — (a) faire décoder les `\uXXXX` par C7
avec un cas négatif, (b) solder « ~46 clés i18n manquantes + le mojibake ».
**Résultat mesuré : (a) était mal diagnostiqué et est corrigé ; (b) est VIDE —
il n'y a ni clé manquante, ni mojibake.** Les deux symptômes avaient la même
cause unique, et ce n'était pas celle qui avait été écrite.

#### Cause racine (prouvée octet par octet, pas déduite)

C7 ne comparait **pas** du texte brut : il décodait déjà les échappements. Le
défaut est ailleurs — `parse()` convertit le texte en **encodage natif** avant
de le lire. Sous une locale non-UTF-8, un caractère non-ASCII est remplacé par
la chaîne **littérale** `<U+00E9>` (7 octets ASCII) :

```
eval(parse(text = '"3c. Réseau"'))  ->  "3c. R<U+00E9>seau"   # sous LC_CTYPE=C
```

`LC_CTYPE=C` n'est pas un cas tordu : c'est ce que **Git Bash exporte**
(`LANG=C.UTF-8`, `LC_ALL=C.UTF-8` — un nom que R **ne reconnaît pas** sous
Windows, d'où le repli silencieux sur `C`).

Mesure avant correctif, sur le dépôt réel :

| Locale | Erreurs C7 |
|---|---|
| `C` | **218** |
| `French_France.1252` | **71** (les emoji restent hors CP1252) |
| `fr_FR.UTF-8` | **0** |

⇒ **La porte de merge était verte ou rouge selon la locale de l'appelant**, et
le chiffre documenté (`0`) était celui d'une locale UTF-8.

#### Chantier (b) — VIDE, et c'est le résultat de la mesure

- **Clés manquantes : 0.** 2179 clés distinctes utilisées vs 2508 définies.
  Les « 243 brutes / 46 après décodage » d'une analyse antérieure étaient
  **toutes** des faux positifs : les 71 qui survivaient sous locale française
  étaient les clés **emoji** (hors CP1252), pas des clés absentes.
- **Mojibake : 0.** `i18n/translation.json` contient **0** occurrence de `Ã`
  (octets `C3 83`), **0** caractère de remplacement `U+FFFD`, **0**
  `SÃ©lectionnez` — et **21** `Sélectionnez` corrects. Le « mojibake
  `SÃ©lectionnez` » observé la veille était le **rendu console** d'une chaîne
  déjà corrompue en mémoire par `parse()`, pas le contenu du fichier.

⇒ Aucune clé à ajouter, aucun octet à réparer. **Le chantier (b) n'existe pas.**

#### Correctif (a)

`tools/check_conventions.R` :
- **`.asciify_non_ascii()`** (nouveau) : convertit tout caractère non-ASCII en
  `\uXXXX` / `\UXXXXXXXX` **avant** `parse()`. La substitution est sans
  ambiguïté — un caractère non-ASCII n'est jamais membre d'une séquence
  d'échappement, celles-ci étant ASCII par construction.
- **`enc2utf8()` des deux côtés** de la comparaison (clés du JSON et clés du
  code), pour que le résultat ne dépende plus de la locale qui a produit les
  chaînes.
- **`.i18n_key_sets()`** extrait de `check_c7_i18n_keys()` : la décision devient
  testable sur une fixture, ce qu'un contrôle en ligne sur le dépôt ne permet
  pas (le dépôt doit rester à 0).

#### Vérifications

| Contrôle | Résultat |
|---|---|
| C7 sous `C`, `French_France.1252`, `fr_FR.UTF-8` | **0 / 0 / 0** (verdict identique) |
| Diff de la sortie complète de la garde, hors bloc C7 | **aucune différence** — tous les compteurs inchangés (C6=16, C9=37, C10=270, C11=1, total 324 avert.) |
| **Cas négatif — fixture** (`tests/testthat/test-conventions-c7-decoder.R`) | 15 assertions vertes ; l'ancien décodeur dérive (2 / 1 / 0 faux positifs selon la locale), le correctif reste à **1** (la clé volontairement absente) dans les trois locales |
| **Cas négatif — bout en bout** | clé factice injectée dans `modules/bulk/mod_bulk.R` ⇒ **exactement 1 erreur C7** (sortie 1), puis restauration du fichier **octet pour octet** (`git status` propre, `diff` identique à la sauvegarde) |

#### ⚠️ Découverte incidente — la suite de tests exige une locale UTF-8

Sous `LC_CTYPE=C` **ou** `French_France.1252`, R ne peut pas **parser**
certaines sources UTF-8 du dépôt :

```
R/sc/sc_communication_perturbation.R:436:73: unexpected invalid token
    ... scale_colour_manual(values = c(Baseline = "grey55", Perturbé = "#D6604D")
```

Seule `fr_FR.UTF-8` passe (`Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")`). Le
fichier est du UTF-8 **valide** (lu sans erreur par `readLines(encoding="UTF-8")`) ;
c'est le couple `parse(file=)` + locale non-UTF-8 qui échoue. **Conséquence :
toute exécution de la suite depuis Git Bash est impossible en l'état** — c'est
un point d'environnement à traiter, indépendant de ce jalon.

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

**Exception à considérer** : `docs/contracts/*.md` (**28** contrats gelés au
2026-09-14 — le chiffre « 13 » de cette section datait du 2026-09-10). La
règle du dépôt (`AGENTS.md`) impose qu'un changement de contrat soit livré
**code + freeze test + doc simultanément**. Si le contrat n'est pas versionné,
cette règle est mécaniquement inapplicable. C'est le seul endroit où le
`.gitignore` contredit une règle écrite du dépôt. Décision en attente.

Note : `docs/STATUS.md` et `docs/ROADMAP.md` ont été **forcés** dans le suivi
git (`git add -f`) car ils constituent l'index et l'état — mais tout le reste
de `docs/` reste ignoré. `docs/kanban_roadmap.html` reste **non suivi**.
