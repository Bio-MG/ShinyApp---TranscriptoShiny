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

> **▶ PROCHAINE ÉTAPE** : **STAT-Q1 → STAT-Q4 : ✅ TERMINÉ** (padj configurable,
> `lfcSE`, note Cook, Excel pathways) — voir
> **`docs/ROADMAP_HANDOFF_STAGE_STAT_Q.md`** (rapport de stage 6 sections) et
> `docs/ROADMAP_HANDOFF_NEXT.md` (handoff). Suite possible : `PLOT-S*`
> (`stat-quickwins` est fait, `plot-shared-helpers` peut démarrer). Rien à
> fournir côté données.

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

## 2. Chantiers ouverts, par ordre d'actionnabilité

### 2a. Prêts à démarrer — présentation (`ROADMAP_presentation_stats.md` §3)

Aucun blocage. Séquençables indépendamment du reste.

| ID | Contenu | Dépend de | Effort |
|---|---|---|---|
| PLOT-S1 | `ts_theme()` — thème + base_size partagés Bulk/SC/Spatial | — | M |
| PLOT-S2 | `ts_export_plot()` — helper dpi/format unifié | PLOT-S1 (conseillé) | M |
| PLOT-S3 | `ts_datatable()` — boutons DT harmonisés | — | M |
| PLOT-S4 | Heatmap unifiée "publication-ready" | PLOT-S1/S2/S3 | L |
| PLOT-S5 | Export SVG (`svglite`) | PLOT-S2 | S |
| PLOT-S6 | Toggle plotly étendu à SC/Spatial | — | M |

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
| 4D-3 | CellChat depuis données brutes | ⛔ **BLOQUÉ upstream** — voir §3 |
| 4E-4 | Exécution asynchrone de la DA | ⏸ Décision de pool à prendre (§4) |
| 4F-ext | Rapport consolidé étendu Bulk/Spatial | Prêt (aucun blocage technique) |
| Cache | Élargissement du cache | ❌ **Non demandé** — règle 8, ne pas étendre |

### 2e. CCC avancée — phases parkées (`ROADMAP_CCC_ADVANCED.md` §4)

| Phase | Contenu | Condition de déblocage |
|---|---|---|
| 5 | Ligand→receptor→target | ⛔ Contrat d'entrée upstream (§3) |
| 6 | NicheNet-like scoring | ⛔ Contrat d'entrée upstream (§3) |
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

### 2h. Bulk V2 / batch-QC — ⚠️ sans roadmap

Travail en cours **non commité** dans l'arbre (voir §5). Il n'appartient à
aucune des quatre roadmaps : c'est un chantier sans document de pilotage.
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

### 2i. Dette i18n héritée de STAT-Q — 6 clés manquantes

**Ouverte le 2026-09-10.** `i18n/translation.json` est verrouillé par le WIP
Bulk V2 (§5), donc les clés introduites par STAT-Q n'ont pas pu y être ajoutées.
En attendant elles retombent sur le texte FR (convention « clé = texte FR ») :
**l'UI française est correcte, l'UI anglaise affichera du français pour ces
6 chaînes.**

```
"Méthode de correction (p-adj)"
"S'applique aux tests DE (DESeq2/edgeR/limma) et à l'enrichissement. Pour DESeq2, changer la méthode recalcule p-adj à partir du modèle déjà ajusté — sans réajustement."
"Export Excel"
"{n} gène(s) exclu(s) du test (outlier Cook's distance)"
"{n} gène(s) non exprimé(s)"
"{n} gène(s) écarté(s) par le filtrage indépendant (faible expression)"
```

**Correctif** : les ajouter dans le même commit que le WIP Bulk V2 (ce fichier
est déjà ouvert). Aucun changement de code n'est requis — le code les appelle
déjà. Détail : `docs/ROADMAP_HANDOFF_STAGE_STAT_Q.md` §1ter et §4.

---

## 3. 4D-3 — décision et contrat d'entrée upstream

> **Analyse de faisabilité détaillée** :
> `docs/proposals/CCC_DATA_PATH_ASSESSMENT.md` — conclusion : le format 10X
> n'est **pas** le blocage (c'est déjà l'entrée naturelle de CellChat, et
> l'app sait déjà lire 10X). Pas besoin de fastq pour tester. Les vrais
> blocages sont : étiquettes de population, contrat upstream, dépendance
> CellChat absente de `renv.lock`.

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

## 5. État de l'arbre de travail (WIP à ne pas committer sans accord)

Non commité au 2026-09-10 (chantier Bulk V2 / batch-QC, cf. §2h) :

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
