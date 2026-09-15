# ROADMAP.md — Orchestrateur des chantiers

> **Point d'entrée unique.** Ce fichier ne décrit ni l'état, ni les règles, ni
> le détail des tâches : il dit **quoi lire, dans quel ordre, et où aller**.
>
> | Question | Fichier |
> |---|---|
> | Où en est-on ? (état réel, commits) | **`docs/STATUS.md`** — source de vérité |
> | Quelles sont les règles non négociables ? | **`AGENTS.md`** (racine, local) |
> | Comment écrire le code ? | **`docs/CONVENTIONS.md`** (+ `tools/check_conventions.R`, garde mécanique) |
> | Quelle est la tâche détaillée ? | roadmaps thématiques (§1) |
> | Vue visuelle de l'avancement | `docs/kanban_roadmap.html` |
>
> **Règle d'or** : en cas de contradiction entre une roadmap et `STATUS.md`,
> **`STATUS.md` gagne** — puis corrigez la roadmap.

Créé 2026-09-10 · Emplacement `docs/` volontaire (cohérent avec les autres
roadmaps et avec le `.gitignore` : la documentation de pilotage reste locale).

---

## 1. Carte des documents

| Document | Rôle | Quand le lire |
|---|---|---|
| `AGENTS.md` | Règles dures, environnement, commandes, baseline de tests | **Toujours, en premier** |
| `docs/CONVENTIONS.md` | Conventions de code (C1..C12) + garde `tools/check_conventions.R` | Avant d'écrire du code ; relancer la garde avant tout merge |
| `docs/STATUS.md` | État réel : livré / ouvert / bloqué + refs de commit | Au début et à la fin de chaque session |
| `docs/ROADMAP_presentation_stats.md` | Plots & stats : PLOT-Q/S, STAT-Q/S, NEW-1..3 | Pour tout travail présentation/statistique |
| `docs/ROADMAP_CCC_ADVANCED.md` | CCC avancée, phases 1–10 + conditions de déblocage | Pour tout travail communication |
| `docs/ROADMAP_HANDOFF_STAGE_11_20.md` | Stages 8–20 (V1.0, livré) + **HANDOFF V1.x** | Historique V1.0 ; entrées V1.x |
| `docs/ROADMAP_HANDOFF_NEXT.md` | **Prochaine session** : les **3 arbitrages** (4E-4 / UX 3B-4B-6B / NEW-3) — ⚖️ **tous rendus le 2026-09-14** (`STATUS.md` §2am) ; ancres re-vérifiées, prompt à coller. ➡️ **Périmé sur son objet** : voir désormais **§2am.1 (4E-4)** et **§2am.3 (NEW-3)** | **Au démarrage de la prochaine session** — à ré-écrire sur les jalons **débloqués** |
| `docs/ROADMAP_MULTI_DATASET.md` | Design MD-1..MD-4 (multi-pipeline Bulk + pseudobulk + SC double jeu, fusion décisions 8+5) | Avant tout travail multi-dataset |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_Q.md` | Rapport de stage PLOT-Q (6 sections) | Exemple du format de rapport attendu |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_Q.md` | Rapport de stage STAT-Q (6 sections) | Historique du volet statistique |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` | Mesure PLOT-S6 (6 sections) — **prémisse périmée, jalon déjà satisfait par l'arbre** | Avant de planifier PLOT-S6 / PLOT-S6′ |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_S1.md` | Rapport de stage STAT-S1 (6 sections) — **ComBat-seq** | Historique — le rapport de stage le plus récent est `ROADMAP_HANDOFF_STAGE_NEW_2.md` (2026-09-14) |
| `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md` | Conception des lots UX (⚠️ état corrigé en tête) | Avant toute reprise UX |
| `docs/proposals/CCC_DATA_PATH_ASSESSMENT.md` | 10X → CellChat : faisabilité et chemins | Avant 4D-3 / phases 5–6 |
| `docs/proposals/CCC_7_8_LIANA_IMPORT_PROPOSAL.md` | **Proposition CCC 7–8 route (b)** : import de rangs LIANA, 9 éléments V1.x, 4 décisions — ✅ **tranchées et livrées** (`a88577f`) | Référence de conception (état réel : `STATUS.md` §2aa) |
| `docs/proposals/CCC_9_RARE_CELLS_MILO_AUDIT.md` | **Audit Milo (règle 3) de la phase 9** : aucun moteur à dupliquer, mais Milo ne sert pas la question mono-condition ; périmètre non spécifié dans le dépôt | Avant toute reprise de la phase 9 |
| `docs/proposals/CCC_9_POPULATION_RARITY_PROPOSAL.md` | **Proposition CCC 9 — question 1** (rareté par population annotée) : jalon descriptif, 9 éléments V1.x, 5 décisions — ✅ **livrée** (`8c1a969`) : les 5 décisions §12 ont été appliquées telles quelles | Référence de conception (état réel : `STATUS.md` §2ac) |
| `docs/release/UPGRADE_AND_COMPATIBILITY.md` | §3 = parking officiel V1.x (4D-3, 4E-4, 4F) | Pour les propositions V1.x |
| `docs/contracts/*.md` | **29** contrats gelés (contract-first) — ⚠️ non versionnés (`docs/` gitignoré) | Avant de toucher un domaine gelé |
| `docs/contracts/CELLCHAT_ENGINE_CONTRACT.md` | **29ᵉ** contrat (2026-09-15) — moteur CellChat natif, jalons **CC-1..CC-5** ; **CC-1..CC-4 livrés** (CellChat 2.2.0.9001 épinglé par SHA, moteur exécutable), reste **CC-5** (UI Path B) | Avant tout travail sur le moteur CellChat |
| `docs/kanban_roadmap.html` | Tableau visuel (lecture seule, miroir de `STATUS.md`) | Démonstration / vue d'ensemble |
| `docs/archive/` | **Archives locales** (créé le 2026-09-14) : fiches caduques, handoffs consommés, artefacts de build obsolètes. ⚠️ **Rien n'est supprimé**, seulement déplacé — toute référence pointe le nouveau chemin | Quand une fiche semble manquer |
| `docs/AUDIT_VERIFICATION_BULK_SC.md` | **Audit externe Bulk/SC VÉRIFIÉ contre le code** (2026-09-14) : 20 confirmées, 4 nuancées, 0 infirmée, **5 trouvées en vérifiant**. Bilan + ordre en 8 jalons — `STATUS.md` §2an | **Avant tout travail Bulk DE / pseudobulk / multi** |
| `CHANGELOG.md` (racine, suivi par git) | Historique des livraisons | Après un commit notable |

---

## 2. Les quatre flux de travail

> **▶ Étape courante (mise à jour 2026-09-14 — cf. `STATUS.md` §2aj/§2ak)** :
> le flux multi-dataset est **terminé** (`MD-1` → `MD-4` ✅ + `4F-EXT` ✅) et
> **tous les jalons débloqués** de l'ordre d'actionnabilité du 2026-09-13 sont
> **livrés** : CCC 7–8 route (b) (`a88577f`), CCC 9 question 1 (`8c1a969`),
> STAT-S2 (`1555283`), STAT-S3 (`e832ec7`), NEW-1 (`b7e0379`), NEW-2
> (`5a64855`), puis **DT-EXPORT** (`f644b55`) et les **correctifs du retour
> feedback** (`4ed66ba`).
>
> **⚖️ Arbitrages rendus le 2026-09-14 — `STATUS.md` §2am** : les trois items
> ci-dessous sont **tranchés**. Deux jalons sont donc **débloqués** (4E-4,
> NEW-3) ; le volet UX reste partiellement ouvert :
>
> 1. ~~**4E-4** — exécution async de la DA~~ → 🟢 **(b) pool applicatif
>    partagé**, *avec réserve* : un pool mirai **dédié** reste pertinent pour
>    plusieurs **échantillons** en parallèle (3 réplicats × 2 conditions) —
>    `STATUS.md` §2am.1 ;
> 2. **UX 3B / 4B / 6B** → 🧊 **TOUT LE VOLET UX EST GELÉ** (décision
>    2026-09-14) : 3A/4A/6A livrées, **3B/4B/6B gelées**, 4A elle-même
>    « gelée, non prioritaire ». L'écart 4A/4B est **sans objet** —
>    `STATUS.md` §2am.2 ;
> 3. ~~**NEW-3** (réseau PCSF)~~ → 🟢 **interactome embarqué** ; téléchargement
>    en ligne acceptable **si local-first** — `STATUS.md` §2am.3. Le besoin
>    utilisateur est exprimé ⇒ le backlog conditionnel est **levé**.
>
> ✅ **Audit reçu et VÉRIFIÉ le 2026-09-14** (« audit sc multi échantillon et
> bulk », annoncé plus tôt) : deux passages, confrontés au code
> affirmations par affirmations → **20 confirmées / 4 nuancées / 0 infirmée /
> 5 problèmes supplémentaires trouvés**. Rapport :
> `docs/AUDIT_VERIFICATION_BULK_SC.md` ; bilan : `STATUS.md` **§2an**.
> **Le correctif P0 le plus coûteux est en fait déjà écrit** :
> `bulk_assert_raw_counts()` existe et n'est pas câblé sur le DE — **étendre,
> ne pas dupliquer** (règle 3).
>
> Deux points **hors arbitrage** restent ouverts :
>
> - **Travail non commité d'un autre auteur** — `.ensure_10x_features()`
>   (`modules/import/mod_import_sc.R` + `tests/manual/`) : à mettre au point
>   **avec son auteur** avant tout commit ; ne pas l'écraser, ne pas le
>   committer aveuglément (`STATUS.md` §2aj).
> - **Suite complète de fin de version** — seuil atteint (cf. §2.0, « politique
>   de tests ») ; référence mesurée consignée en `STATUS.md` §2ak.
>
> **CCC phases 5–6 : GELÉES SANS SUITE** (pas d'import fastP côté 4D-3) — ne
> plus les re-proposer. Détail : `docs/ROADMAP_CCC_ADVANCED.md` §4.
>
> **Passe de maintenance du 2026-09-13** (hors jalon produit, déjà faite) :
> `docs/CONVENTIONS.md` créé + garde `tools/check_conventions.R` (C1..C12),
> dette i18n soldée (101 clés ajoutées, C7 = 0 manquante) et
> `R/plotting/complex_heatmap.R` sorti du `.gitignore` (il est sourcé par
> `app.R` — un clone neuf ne pouvait pas démarrer).
>
> Le volet « présentation » (`plot-shared-helpers`) est **terminé et clos** :
> PLOT-S1..S5 livrés ; **PLOT-S6 était déjà satisfait par l'arbre** (mesure du
> 2026-09-12, `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md`) et le repli **PLOT-S6′
> n'est pas retenu** (décision utilisateur du 2026-09-12).
>
> **Flux E — Bulk V2 : ✅ COMPLET (M1→M5)**, livré le soir du 2026-09-12
> (`081bc6a`→`5ae0b8b`, 4 contrats gelés) — référence :
> `docs/ROADMAP_BULK_V2.md`. (La fiche `docs/ROADMAP_BULK_V2_STATS.md` du
> matin, qui parquait M2–M5, est caduque — voir son en-tête ; **archivée** le
> 2026-09-14 → `docs/archive/ROADMAP_BULK_V2_STATS.md`.)
>
> Les trois arbitrages (`NEW-3`, `4E-4`, UX 3B/4B/6B) ont été **rendus le
> 2026-09-14** — voir le bandeau en tête de ce §2 et `STATUS.md` §2am.
> **Jalons désormais débloqués : `4E-4` et `NEW-3`** (le volet flux A reste
> terminé). Voir l'ordre d'actionnabilité en tête de ce §2 (décision
> utilisateur 2026-09-13).

> 📌 **Règle ajoutée le 2026-09-12 — re-mesurer avant de planifier.**
> **Trois** fiches §3 de `ROADMAP_presentation_stats.md` se sont révélées
> périmées d'affilée : `base_size` (PLOT-S1), `pageLength` (PLOT-S3), `plotly`
> (PLOT-S6). Les fiches ont été rédigées lors de l'audit initial et l'arbre a
> évolué depuis. **Toute fiche §3 doit être re-mesurée contre le dépôt avant
> d'être planifiée** — c'est le §3.2 du protocole, appliqué plus tôt.

Chaque flux a sa roadmap et son avancement dans `STATUS.md`. Ils sont
**indépendants** sauf mention contraire : on peut en mener un seul à la fois,
mais rien n'oblige à les séquencer entre eux.

### 2.0 Ordre d'actionnabilité (arbitrage utilisateur du 2026-09-13)

> 📌 **Règle ajoutée le 2026-09-14 — politique de tests.**
> Tests **ciblés par défaut** : à chaque jalon, on ne lance que les fichiers
> de tests concernés par le changement (+ les gates conventions/duplication).
> La suite complète (~14 min) ne tourne qu'**en fin de version** — après
> accumulation de plusieurs fonctionnalités, avant un tag release/RC.
> Consignée dans `AGENTS.md` §1 (Test policy).

Séquence **recommandée** — elle suit l'arbitrage de l'utilisateur, pas
l'ancienneté des fiches. Aucune date calendaire n'est imposée : le dépôt
avance jalon par jalon, un jalon = un commit.

| Rang | Jalon | Effort | Dépend de | État |
|---|---|---|---|---|
| — | ~~**CCC 7–8** — interop OmniPath / LIANA~~ | — | — | ✅ **LIVRÉ le 2026-09-13** (`a88577f`, route (b) sans dépendance) — `STATUS.md` §2aa |
| — | ~~**CCC 9** — rare-cell annotator = **question 1 : rareté par population**~~ | M | — | ✅ **LIVRÉ le 2026-09-13** (`8c1a969`) — `STATUS.md` §2ac ; questions 2 et 3 non retenues |
| — | ~~**STAT-S2** — réseau d'enrichissement (`emapplot`/`cnetplot`)~~ | M | — | ✅ **LIVRÉ le 2026-09-13** — `STATUS.md` §2ad |
| — | ~~**STAT-S3** — clustering de profils (kmeans MVP)~~ | L | — | ✅ **LIVRÉ le 2026-09-13** — `STATUS.md` §2ae ; V2 floue (Mfuzz) non retenue |
| — | ~~**NEW-1** — dose-réponse / time-course~~ | M | — | ✅ **LIVRÉ le 2026-09-13** (`drc`, justification renv.lock au contrat §2) — `STATUS.md` §2af |
| — | ~~**NEW-2** — fusion de jeux~~ | M | ~~STAT-S1~~ ✅ prérequis levé | ✅ **LIVRÉ le 2026-09-13** (fiche re-mesurée : fusion depuis le conteneur `bulk_datasets`, produit chargé comme jeu actif — `STATUS.md` §2ag) |
| 6 | **NEW-3** — réseau PCSF | L | ~~interactome local vs contrainte offline~~ ✅ **tranché le 2026-09-14** : **interactome embarqué**, téléchargement en ligne en complément **si local-first** | 🟢 **DÉBLOQUÉ** — backlog conditionnel **levé** (besoin exprimé) — `STATUS.md` §2am.3 |
| 7 | **UX 3B / 4B / 6B** | M | ~~décision 2~~ ✅ **tranchée le 2026-09-14** | 🧊 **GELÉ** — 3A/4A/6A livrées, 3B/4B/6B gelées. La refonte UX **ne reprend pas** sans décision nouvelle — `STATUS.md` §2am.2 |
| 8 | **4E-4** — exécution async de la DA | M | ~~décision 1 (pool)~~ ✅ **tranchée le 2026-09-14** : **(b) pool applicatif partagé** + réserve « pool dédié » multi-échantillons | 🟢 **DÉBLOQUÉ** — `STATUS.md` §2am.1 |
| 9 | ~~Boutons d'export DT sur les ~43 tables restantes + `pageLength` normalisé~~ | M | ~~décision ouverte~~ | ✅ **LIVRÉ le 2026-09-14 (DT-EXPORT)** — contrat `PLOT_DATATABLE_CONTRACT.md` §6 option B, 73 sites via `ts_datatable()`, `pageLength = 15` + boutons nommés sur les tables de résultats, aperçus exclus — `STATUS.md` §2ai |

#### 2.0bis Chantier courant — moteur CellChat natif (décision §2al, `STATUS.md` §2ao)

Découpé en jalons **CC-1 → CC-5**, un jalon = un commit. Source de conception :
`docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md` §7.

| Rang | Jalon | Effort | Dépend de | État |
|---|---|---|---|---|
| 10 | ~~**CC-1** — épinglage `CellChat` par SHA + insertion **chirurgicale** dans `renv.lock`~~ | S | décision §2al | ✅ **LIVRÉ** (2026-09-15) — `CellChat` 2.2.0.9001 épinglé par SHA `75253cd0…358f` ; lockfile 425 → **438** entrées (**+535 / −0**, 0 changement de version). Cause racine du précédent échec trouvée : **segfault de teardown** (tout R chargeant `dplyr`/`ggplot2`/`igraph` sort en 139) tuant le sous-processus de lazy-load. Contournement `--no-clean-on-error --no-test-load` + `Meta/nsInfo.rds`. ⚠️ **Réserve** : `Meta/` incomplet, et `ggpubr` résolu côté bibliothèque système. Détail : `STATUS.md` §2ao.5 |
| 11 | ~~**CC-2** — contrat gelé `CELLCHAT_ENGINE_CONTRACT.md`~~ | S | ~~CC-1~~ | ✅ **LIVRÉ** — 29ᵉ contrat, `STATUS.md` §2ao.4 |
| 12 | ~~**CC-3** — moteur `R/sc/sc_communication_engine.R` (12 champs, moteur éphémère)~~ | M | CC-2 | ✅ **LIVRÉ** |
| 13 | ~~**CC-4** — test + gardes (conventions, duplication)~~ | S | CC-3 | ✅ **LIVRÉ** — **74 assertions, 0 échec, 0 skip** (le run réel n'est plus skippé) ; gardes à la baseline (0/324, 0/3) |
| 14 | **CC-5** — module UI « calculer dans l'app » (Path B) + export conservé | M | **CC-1** | ⬜ **PROCHAIN JALON — débloqué par CC-1.** Le moteur s'exécute réellement ; exposer l'action a maintenant du sens. |

⚠️ **Deux mesures préalables qui corrigent la proposition** (détail `STATUS.md`
§2ao) :

1. **Empreinte réelle ≠ estimation §2.2.** La proposition annonçait « +8 à +12 »
   en ne listant que `NMF` + transitifs + `collapse` + `ggalluvial`. Mesuré
   contre le `DESCRIPTION` amont réel (CellChat 2.2.0.9001) : **39 dépendances
   directes**, dont **32 déjà dans le lock** (la chaîne lourde —
   `ComplexHeatmap`, `circlize`, `igraph`, `RcppEigen`, `RSpectra`, `FNN`,
   `BiocNeighbors`… — est bien déjà payée) et **7 manquantes** → fermeture
   transitive = **17 paquets hors lock**, dont **6 déjà installés mais non
   enregistrés** (`corrplot`, `ggpubr`, `ggsci`, `ggsignif`, `polynom`,
   `rstatix`). **11 à installer** + `CellChat`. L'estimation initiale était donc
   juste… pour les mauvaises raisons (4ᵉ occurrence de la règle « re-mesurer
   avant de planifier »).
2. **Ne jamais lancer `renv::snapshot()` globalement.** Le projet est
   **désynchronisé** : 20 paquets en dérive de version (`igraph` 2.2.1 vs 2.3.3,
   `future` 1.75.0 vs 1.70.0…) et **48 paquets « utilisés mais non enregistrés »**
   (`WGCNA`, `GSVA`, `sva`, `survminer`…). Un `snapshot()` aval capturerait
   toute cette dette étrangère au jalon. ⇒ **insertion chirurgicale**, comme
   NEW-1 (`drc`).

Hors séquence, **gelé** : CCC 5–6 (sans suite). Hors séquence, **non demandé** :
élargissement du cache (règle 8).

### Flux A — Présentation & statistiques · `ROADMAP_presentation_stats.md`
Quick wins **tous livrés** : PLOT-Q1..Q5 (`4dee553`) et STAT-Q1..Q4
(`fda5a81`..`5f3c9f0`). Puis **PLOT-S1..S5 livrés** (S1..S3 commités, **S4 et
S5 non commités** — décision utilisateur) ; **PLOT-S6 déjà satisfait par
l'arbre** (mesuré 2026-09-12) et **PLOT-S6′ non retenu** → volet présentation
**clos**. Côté statistiques : **STAT-S1 ✅ livré le 2026-09-12** (ComBat-seq).
**Le volet flux A est TERMINÉ** (NEW-1 et NEW-2 livrés le 2026-09-13,
`STATUS.md` §2af/§2ag) — restent **NEW-3** (backlog conditionnel) et les
arbitrages.

Ordre conseillé (fondations d'abord) :
`PLOT-S1 → PLOT-S2 → PLOT-S3 → PLOT-S4 → PLOT-S5` ✅ **fait** ; `PLOT-S6` ✅
**déjà fait par l'arbre** ; `STAT-S1` ✅ **fait** ; `STAT-S2` ✅ **livré le
2026-09-13** (réseau d'enrichissement, `STATUS.md` §2ad) ; `STAT-S3` ✅
**livré le 2026-09-13** (clustering de profils, `STATUS.md` §2ae) ; `NEW-1` ✅
**livré le 2026-09-13** (dose-réponse, `STATUS.md` §2af) ; `NEW-2` ✅ **livré
le 2026-09-13** (fusion de jeux, `STATUS.md` §2ag). **Le volet flux A est
TERMINÉ** — restent `NEW-3` (backlog conditionnel) et les arbitrages
(`4E-4`, UX 3B/4B/6B — §5).

### Flux B — CCC avancée · `ROADMAP_CCC_ADVANCED.md`
Phases 1–4 livrées ; **5–6 GELÉES SANS SUITE** (2026-09-13) ; **7–8 LIVRÉES**
(route (b) sans dépendance, `a88577f`) ; **9 = question 1** (rareté par
population) — **proposition écrite**, 5 décisions à valider (`STATUS.md` §2ab) ;
**10** toujours parkée — conditions et routes dans `ROADMAP_CCC_ADVANCED.md`
§4 ; décisions consignées dans `STATUS.md` §2y et §2ab.

### Flux C — Propositions V1.x · `docs/release/UPGRADE_AND_COMPATIBILITY.md` §3
`4D-3` (bloqué upstream) · `4E-4` (décision de pool à prendre) · `4F-ext`
(prêt) · cache (explicitement **non** à étendre).

### Flux D — UX/UI · `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md`
Lots 0/1/2/3A/4A/6A/5 livrés. Restent les options **3B/4B/6B** (jamais
tranchées) et les libellés techniques des modules enfants Spatial.

### Flux E — Bulk V2 · ✅ **COMPLET (M1→M5) — roadmap dédiée créée**
La recommandation « créer une roadmap dédiée » est appliquée :
**`docs/ROADMAP_BULK_V2.md`** (jalons/contrats/commits, découvertes
d'environnement, restes ouverts). État détaillé : `STATUS.md` §2h + §2r.
Rapport de stage : `docs/ROADMAP_HANDOFF_STAGE_BULK_V2_M2_M5.md`.
Restes ouverts du chantier (NON planifiés) : intégration rapport (4F-ext),
async (arbitrage 4E-4), caching (règle 8).

---

## 3. Protocole de session

1. **Lire** `AGENTS.md` puis `STATUS.md`.
2. **Vérifier les ancres dans le dépôt** avant toute édition — chemins réels,
   fonctions existantes, paquets déjà présents. Si la roadmap et l'arbre se
   contredisent, **l'arbre fait foi**.
3. **Un jalon = un commit.** C'est une règle d'arrêt, pas seulement de commit.
4. **Ne pas enchaîner** sur le jalon suivant sans instruction explicite
   (« continue to <ID> »).
5. **Tests** : ne jamais citer un compte de tests partiel comme référence de
   non-régression — lancer la suite complète pour une référence (voir
   `STATUS.md` §6.4).
6. **Rapport de stage** en 6 sections (modèle :
   `ROADMAP_HANDOFF_STAGE_PLOT_Q.md`) : fait / fichiers touchés / non fait /
   risques / comment continuer / commandes de vérification.
7. **Mettre à jour** `STATUS.md` (état) et le kanban si nécessaire, dans le
   même commit que le travail.

---

## 4. Principes de conception transverses

- **Contract-first** : tout nouveau domaine = code + freeze test + doc
  `docs/contracts/` simultanément. Les **28** contrats gelés ne changent que
  selon cette règle. Le gel est porté par un freeze test **éponyme**
  (`test-<sujet>-contract-freeze.R`) pour 24 d'entre eux ; les 5 autres
  (`BULK_DOSE_RESPONSE`, `BULK_PATTERN`, `PLOT_DATATABLE`, `PLOT_EXPORT`,
  `PLOT_HEATMAP`) portent leurs assertions de gel **dans le test principal** du
  domaine — écart de **nommage** uniquement, aucun contrat n'est sans test
  (garde **C8** = 0).
- **Conventions vérifiées** : `docs/CONVENTIONS.md` (règles C1..C12) +
  `tools/check_conventions.R`. Les règles de niveau ERREUR sont à **zéro** et
  doivent y rester ; les AVERT. sont des **plafonds de dette** qui ne doivent
  pas augmenter (relevé §12 du doc).
- **`R/` = logique pure, `modules/` = UI.** Aucun fichier sous `R/modules/`.
- **Import-only tant que 4D-3 n'est pas levé** pour la CCC : les scores
  importés ne sont jamais recalculés ni mélangés entre sources.
- **Un seul pool de workers** : mirai. Jamais `BiocParallel::MulticoreParam`
  sous Windows, jamais `WGCNA::enableWGCNAThreads()`.
- **i18n** : toute chaîne visible passe par `tr` ; clé = texte FR.
- **Local-first / offline** : ne pas introduire de dépendance qui exige un
  accès réseau à l'exécution.

### Principe « double jeu de données » (Single-Cell) — intention utilisateur

Direction produit pour le module SC : deux fichiers peuvent être chargés et
traités selon **trois relations déclarées**, et non un seul comportement figé :

1. **Analyses séparées, paramètres partagés** — deux jeux comparables traités
   avec les *mêmes* réglages, pour que les différences observées soient
   biologiques et non méthodologiques.
2. **Analyses séparées, paramètres distincts** — quand les jeux diffèrent
   légitimement (profondeur de séquençage, tissu, contrôle vs traitement) et
   exigent des seuils propres.
3. **Fusion** — quand les fichiers sont des **réplicats** (rep1 + rep2) et
   doivent être intégrés en un seul objet pour une analyse conjointe.

Le choix de relation est une **décision déclarée** qui détermine le partage des
paramètres (partagés / indépendants) ou la fusion (avec intégration type
Harmony). Conséquence de provenance : la relation et le périmètre des
paramètres doivent être enregistrés dans la traçabilité.

*État actuel* : l'app **fusionne toujours** les imports multiples
(`merge()` + `add.cell.ids`, chaque import devenant un `orig.ident`, Harmony
auto dès ≥ 2 échantillons). Les modes 1 et 2 n'existent pas encore — c'est une
évolution, pas un correctif.

---

## 5. Décisions en attente (à trancher par l'utilisateur)

| # | Décision | Impact |
|---|---|---|
| 1 | ~~**4E-4** : pool mirai dédié SC / pool applicatif unique / rester synchrone~~ — ✅ **TRANCHÉ le 2026-09-14** : **(b) pool applicatif partagé**, *avec réserve* — un pool mirai **dédié** reste pertinent pour plusieurs **échantillons** en parallèle (3 réplicats × 2 conditions) ⇒ ne pas fermer la porte au parallélisme par échantillon (`STATUS.md` §2am.1) | Architecture async de la DA — **jalon DÉBLOQUÉ** |
| 2 | **UX** : options 3B, 4B, 6B — ✅ **TRANCHÉ le 2026-09-14 : TOUT LE VOLET UX EST GELÉ.** 3A/4A/6A livrées ; **3B, 4B, 6B gelées** (« aucune idée pour 4B → gèle » ; « 4A pourquoi pas mais gèle aussi, non prioritaire »). L'écart 4A/4B est **sans objet** : aucune des deux ne sera faite. Rappel des définitions (A = sans risque, B = structurelle) en `STATUS.md` §2am.2 | Refonte UX **ne reprend pas** — ne pas re-proposer 3B/4B/6B sans décision nouvelle |
| 3 | ~~**Bulk V2** : lui créer une roadmap dédiée ?~~ — ✅ **TRANCHÉ le 2026-09-12** : `docs/ROADMAP_BULK_V2.md` créée (Flux E complet M1→M5) | ~~Traçabilité~~ résolue |
| 4 | **`docs/` gitignoré** : garder local, ou versionner `docs/*.md` + `docs/contracts/*.md` ? | Les **28** contrats ne sont pas versionnés — voir la remarque ci-dessous (⚠️ un fichier **sourcé** ne doit jamais être ignoré : cas corrigé le 2026-09-13, garde C3) |
| 5 | ~~**Modes 1/2 du double jeu SC** : priorité et périmètre~~ — ✅ **TRANCHÉ par réalisation le 2026-09-13** : `MD-4` livre le conteneur `sc_datasets` + les relations déclarées `standalone` / `shared_params` (mode 1) / `distinct_params` (mode 2), contrat `SC_MULTI_CONTRACT.md` (`STATUS.md` §2w) | ~~Direction produit du module SC~~ résolue — reste l'**usage** des modes dans les panneaux d'analyse (évolution, pas un correctif) |
| 6 | ~~**PLOT-S6′**~~ — ✅ **TRANCHÉE le 2026-09-12 : PLOT-S6 clos, repli PLOT-S6′ non retenu** | Volet « présentation » **terminé** — voir `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` |
| 7 | **Outillage MCP** : ~~snapshoter `mcptools`/`btw`/`ellmer` dans `renv.lock`, ou assumer qu'ils restent locaux ?~~ → **recommandation posée le 2026-09-12** (`STATUS.md` §2s) : profil renv `dev` séparé, pas de snapshot dans le lockfile principal — **à valider**. Diagnostic ABI §2p fait le soir : hypothèse d'un décalage massif **réfutée** (seul `jsonlite` est Built 4.4.3) ; template de connexion ZCode posé (`mcp.examples/`) | Un clone neuf ne peut pas démarrer `scripts/mcp_server.R` tant que ce n'est pas tranché |
| 8 | ~~**Multi-échantillons + pseudobulk « comme Spatial »**~~ — ✅ **LIVRÉ le 2026-09-13** : `MD-1` (conteneur `bulk_datasets`), `MD-2` (comparaison), `MD-3` (pont pseudobulk), `MD-4` (conteneur `sc_datasets`) + `4F-EXT` (rapport consolidé) | ~~Chantier neuf~~ **clos** — voir `docs/ROADMAP_MULTI_DATASET.md` |
| 9 | ~~**CCC phases 7–8** : route (a) `OmnipathR`/`liana` avec `renv.lock` justifié, ou (b) **import de résultats LIANA externes sans dépendance** ?~~ — ✅ **TRANCHÉ et LIVRÉ le 2026-09-13** : **route (b)** retenue, implémentée (`a88577f`), contrat étendu. La route (a) reste une décision **séparée**, non demandée | Interop inter-méthodes ; (b) n'ajoute aucune dépendance et respecte le périmètre « import-only » |
| 10 | **Dette de conventions** (relevé 2026-09-13) : C6 = 16 `library()` au top-level de `R/`, C9 = 37 fichiers sans test éponyme, C10 = 270 `stop()` non classés — chantier de réduction, ou statu quo avec plafond ? | Qualité long terme ; les compteurs ne doivent **pas augmenter** (garde `tools/check_conventions.R`) |
| 11 | ~~**CCC phase 9** : laquelle des trois questions (rareté par population / par voisinage / × communication) ?~~ — ✅ **TRANCHÉE et LIVRÉE le 2026-09-13** : **question 1 — rareté par population annotée** (`8c1a969` ; les 5 décisions de conception §12 de la proposition ont été **appliquées telles quelles**) | Périmètre du jalon CCC 9 — voir `STATUS.md` §2ac |
| 12 | **Travail non commité d'un autre auteur** : `.ensure_10x_features()` (`modules/import/mod_import_sc.R`, **indexé** ; `tests/manual/test_ensure_10x_features.R`, **non suivi**) — compat CellRanger `genes.tsv` à colonne unique | L'arbre n'est pas propre : à réviser puis committer, ou à écarter — **ne pas l'écraser, ne pas le committer aveuglément** (`STATUS.md` §2aj) |
| 13 | ~~**NEW-3** : interactome embarqué / téléchargé en ligne / les deux ?~~ — ✅ **TRANCHÉ le 2026-09-14** : **embarqué** ; l'en ligne est accepté **en complément**, « du moment qu'une solution **local first** existe ». ⚠️ **Mesurer le poids** avant d'embarquer ; le choix de la source (STRING / BioGRID / OmniPath…) se fera **sur chiffres** (licence + volume) — échec réseau ⇒ repli silencieux sur l'embarqué, jamais d'erreur bloquante (`STATUS.md` §2am.3) | ~~Backlog conditionnel NEW-3~~ **levé** — jalon **débloqué** |

> **Remarque sur la décision 4** — garder roadmaps et instructions d'agents
> locales est un choix défendable. En revanche `docs/contracts/*.md` est un cas
> différent : `AGENTS.md` impose qu'un changement de contrat se fasse
> **code + freeze test + doc simultanément**. Si le contrat n'est pas versionné,
> cette règle est mécaniquement inapplicable (le doc ne peut pas être dans le
> même commit). C'est le seul point où le `.gitignore` contredit une règle
> écrite du dépôt.
>
> **Précédent concret (2026-09-13)** : le `.gitignore` excluait aussi
> `R/plotting/complex_heatmap.R`, que `app.R` source — un clone neuf ne
> pouvait pas démarrer l'application. Corrigé (fichier désormais suivi) et
> **verrouillé** par la règle **C3** de `tools/check_conventions.R` : toute
> cible de `source()` doit exister **et** être versionnée. Le risque résiduel
> se limite donc désormais aux fichiers que l'app ne source pas
> (`docs/`, `AGENTS.md`, `mcp.examples/`, `scripts/mcp_server.R`).
