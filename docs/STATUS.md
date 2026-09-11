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
**Dernier commit fonctionnel de référence** : `4dee553` — feat(plots): ggrepel
volcano, 300dpi, pdf export, stat subtitles (PLOT-Q1..Q5).
**Dernière consolidation documentaire** : commit `docs(status):` — création de
`docs/STATUS.md` et `docs/ROADMAP.md` (index + état), dé-obsolescence des
roadmaps (voir `git log --oneline -- docs/`).

> **▶ PROCHAINE ÉTAPE** : **PLOT-S2** (`ts_export_plot()` — helper d'export
> unifié dpi/format).
>
> **Tout est poussé.** `HEAD = aa92f24` (« bulk V2.0, sc compare ») —
> **0 commit d'avance sur `origin/main`**, arbre de travail **propre**.
>
> **Livré :** **PLOT-S1 ✅** (`ts_theme()` partagé + propagation Bulk/SC/Spatial,
> 44 sites), **4D-3 — chemin de données ✅** (10X/Seurat → entrée CellChat, sans
> dépendance nouvelle), **Bulk V2 / batch-QC ✅** (commité par l'utilisateur,
> puis **fini** — erreur de test corrigée, dette i18n soldée, cf. §5bis).
>
> Rapports de stage 6 sections : `docs/ROADMAP_HANDOFF_STAGE_PLOT_S1.md` et
> `docs/ROADMAP_HANDOFF_STAGE_CELLCHAT_INPUT.md`. Handoff :
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
| PLOT-S2 | `ts_export_plot()` — helper dpi/format unifié | PLOT-S1 (conseillé) | M |
| PLOT-S3 | `ts_datatable()` — boutons DT harmonisés | — | M |
| PLOT-S4 | Heatmap unifiée "publication-ready" | PLOT-S1/S2/S3 | L |
| PLOT-S5 | Export SVG (`svglite`) | PLOT-S2 | S |
| PLOT-S6 | Toggle plotly étendu à SC/Spatial | — | M |

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
| STAT-S1 | ComBat-seq (dépendance `sva`) dans Filtrage | L |
| STAT-S2 | Réseau d'enrichissement (`emapplot`/`cnetplot`) | M |
| STAT-S3 | Pattern/profile clustering (kmeans MVP, Mfuzz en v2) | L |

### 2c. Nouveaux modules (`ROADMAP_presentation_stats.md` §6)

| ID | Contenu | Prérequis |
|---|---|---|
| NEW-1 | Dose-réponse / time-course (`drc`) | — |
| NEW-2 | Fusionner des jeux de données | STAT-S1 (réutilise `run_combat_seq`) |
| NEW-3 | Réseau PCSF / interactome | **Backlog conditionnel** — interactome local à évaluer vs contrainte offline |

### 2d. Propositions V1.x (parking officiel)

Source : `docs/release/UPGRADE_AND_COMPATIBILITY.md` §3.

| ID | Contenu | État |
|---|---|---|
| 4D-3 | CellChat depuis données brutes | 🟡 **Chemin de données livré** (`6bf18ca`) — reste le contrat upstream + la dépendance CellChat, voir §3 |
| 4E-4 | Exécution asynchrone de la DA | ⏸ Décision de pool à prendre (§4) |
| 4F-ext | Rapport consolidé étendu Bulk/Spatial | Prêt (aucun blocage technique) |
| Cache | Élargissement du cache | ❌ **Non demandé** — règle 8, ne pas étendre |

### 2e. CCC avancée — phases parkées (`ROADMAP_CCC_ADVANCED.md` §4)

| Phase | Contenu | Condition de déblocage |
|---|---|---|
| 5 | Ligand→receptor→target | ⛔ Contrat d'entrée upstream (§3) — **le format d'entrée, lui, n'est plus une question ouverte** : il est implémenté et testé, cf. §3 |
| 6 | NicheNet-like scoring | ⛔ Contrat d'entrée upstream (§3) — idem |
| 7 | OmniPath | Nouvelle dépendance (renv.lock justifié) — **variante sans dépendance** : import de résultats LIANA externes (colonnes `.rank`), extension du contrat Stage 11 |
| 8 | LIANA | Idem ; comparaison en rangs/recouvrement uniquement, **jamais de score consensus** |
| 9 | Rare-cell annotator | Auditer d'abord le chevauchement avec Milo (Stage 14) |
| 10 | Gallery d'étendue | Tient tant que « aucune dépendance graph nouvelle » (Stage 12) |

### 2f. UX/UI — lots restants

Lots 0/1/2/3A/4A/6A/5 **exécutés** (preuve §1). Restent :

- **Options 3B, 4B, 6B** — jamais tranchées (les variantes A ont été retenues).
- **Libellés techniques des modules enfants Spatial** (Moran, niches…) —
  volontairement intouchés lors des passes UX.

### 2g. Single-Cell — « double jeu de données » (direction produit, non démarré)

Intention utilisateur : deux fichiers chargés, avec **trois relations
déclarées** — analyses séparées à paramètres **partagés**, analyses séparées à
paramètres **distincts**, ou **fusion** quand ce sont des réplicats.
Détail dans `docs/ROADMAP.md` §4 (« Principe double jeu de données »).

*État réel* : l'app **fusionne toujours** les imports multiples
(`merge()` + `add.cell.ids` dans `mod_import_sc.R`, chaque import devenant un
`orig.ident` ; Harmony automatique dès ≥ 2 échantillons). Les modes
« séparés » n'existent pas. C'est une **évolution**, pas un correctif — donc
proposition explicite avant exécution (règle du dépôt).

### 2h. Bulk V2 / batch-QC — ✅ commité, ⚠️ toujours sans roadmap

**Commité et poussé le 2026-09-11** (`aa92f24` — « bulk V2.0, sc compare »),
puis fini (voir §5bis). Il n'appartient toujours à **aucune des quatre
roadmaps** : c'est un chantier sans document de pilotage.
Les seuils associés existent bien dans `config/thresholds.R`
(`TS_BULK_VARPART_MAX_GENES`, `TS_BULK_GSVA_*`, `TS_BULK_WGCNA_*`,
`TS_BULK_SURV_MIN_EVENTS`, …) et le contrat
`docs/contracts/BULK_BATCH_QC_CONTRACT.md` est écrit.

**Recommandation** : lui créer sa propre roadmap avant de reprendre, sinon il
restera le seul chantier sans état traçable. Règles techniques déjà connues
à ne pas oublier (extraites du prompt de session antérieur) : GSVA via
`gsvaParam`/`ssgseaParam` avec `BPPARAM` sur `gsva()` et non le constructeur,
`SnowParam` sous Windows, **jamais** `enableWGCNAThreads`
(`allowWGCNAThreads(nThreads = 1)` si WGCNA tourne sous mirai), plafonner
`fitExtractVarPartModel` à 2000–5000 HVG, decoupleR en signatures RDS locales
uniquement avec assertion réseau sortant = 0, porte de recouvrement de jeux
de gènes < 20 % après strip des suffixes Ensembl `.1/.2`.

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
