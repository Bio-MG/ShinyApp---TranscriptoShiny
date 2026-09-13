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
| `docs/ROADMAP_HANDOFF_NEXT.md` | **Prochaine session** : périmètre MD-1, ancres vérifiées, prompt à coller | **Au démarrage de la prochaine session** |
| `docs/ROADMAP_MULTI_DATASET.md` | Design MD-1..MD-4 (multi-pipeline Bulk + pseudobulk + SC double jeu, fusion décisions 8+5) | Avant tout travail multi-dataset |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_Q.md` | Rapport de stage PLOT-Q (6 sections) | Exemple du format de rapport attendu |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_Q.md` | Rapport de stage STAT-Q (6 sections) | Historique du volet statistique |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` | Mesure PLOT-S6 (6 sections) — **prémisse périmée, jalon déjà satisfait par l'arbre** | Avant de planifier PLOT-S6 / PLOT-S6′ |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_S1.md` | Rapport de stage STAT-S1 (6 sections) — **ComBat-seq** | **Dernier rapport livré (2026-09-12)** |
| `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md` | Conception des lots UX (⚠️ état corrigé en tête) | Avant toute reprise UX |
| `docs/proposals/CCC_DATA_PATH_ASSESSMENT.md` | 10X → CellChat : faisabilité et chemins | Avant 4D-3 / phases 5–6 |
| `docs/proposals/CCC_7_8_LIANA_IMPORT_PROPOSAL.md` | **Proposition CCC 7–8 route (b)** : import de rangs LIANA, 9 éléments V1.x, 4 décisions — ✅ **tranchées et livrées** (`a88577f`) | Référence de conception (état réel : `STATUS.md` §2aa) |
| `docs/proposals/CCC_9_RARE_CELLS_MILO_AUDIT.md` | **Audit Milo (règle 3) de la phase 9** : aucun moteur à dupliquer, mais Milo ne sert pas la question mono-condition ; périmètre non spécifié dans le dépôt | Avant toute reprise de la phase 9 |
| `docs/proposals/CCC_9_POPULATION_RARITY_PROPOSAL.md` | **Proposition CCC 9 — question 1** (rareté par population annotée) : jalon descriptif, 9 éléments V1.x, 5 décisions — 🟡 **en attente de validation** | Avant d'implémenter la phase 9 (`STATUS.md` §2ab) |
| `docs/release/UPGRADE_AND_COMPATIBILITY.md` | §3 = parking officiel V1.x (4D-3, 4E-4, 4F) | Pour les propositions V1.x |
| `docs/contracts/*.md` | **24** contrats gelés (contract-first) — ⚠️ non versionnés (`docs/` gitignoré) | Avant de toucher un domaine gelé |
| `docs/kanban_roadmap.html` | Tableau visuel (lecture seule, miroir de `STATUS.md`) | Démonstration / vue d'ensemble |
| `CHANGELOG.md` (racine, suivi par git) | Historique des livraisons | Après un commit notable |

---

## 2. Les quatre flux de travail

> **▶ Étape courante (arbitrage utilisateur du 2026-09-13 — cf.
> `STATUS.md` §2y)** : le flux multi-dataset est **terminé**
> (`MD-1` → `MD-4` ✅ + `4F-EXT` ✅). Les prochains jalons, dans l'ordre
> d'actionnabilité décidé par l'utilisateur :
>
> 1. **CCC phase 7–8** (interop OmniPath / LIANA) — **DÉBLOQUÉE** : choisir la
>    route (a) `OmnipathR`/`liana` = `renv.lock` justifié, ou (b) import de
>    résultats LIANA externes **sans dépendance** (extension du contrat Stage
>    11 ; comparaison en **rangs/recouvrement uniquement**, jamais de score
>    consensus).
> 2. **CCC phase 9** (rare cells) — **PÉRIMÈTRE TRANCHÉ (2026-09-13)** : la
>    phase 9 est la **question 1** (« quelles populations annotées sont
>    rares ? ») — jalon **descriptif, mono-condition**, donc **sans graphe
>    kNN** : l'audit Milo (fait, `docs/proposals/CCC_9_RARE_CELLS_MILO_AUDIT.md`)
>    devient **sans objet** pour ce jalon. **Proposition écrite** :
>    `docs/proposals/CCC_9_POPULATION_RARITY_PROPOSAL.md` — **5 décisions à
>    valider** avant implémentation (`STATUS.md` §2ab).
> 3. **STAT-S2** (réseau d'enrichissement, effort M) — `enrichplot` déjà
>    présent ; puis **STAT-S3** (clustering de profils, effort L).
> 4. **NEW-1..3** (dose-réponse, fusion de jeux, PCSF) — le prérequis de
>    `NEW-2` est levé par STAT-S1.
> 5. **UX 3B / 4B / 6B** — options jamais tranchées.
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
> matin, qui parquait M2–M5, est caduque — voir son en-tête.)
>
> Restent `STAT-S2`/`STAT-S3`, `NEW-1..3` (⚠️ le prérequis de `NEW-2` est
> **levé** par STAT-S1), ou un arbitrage (`4E-4`, UX 3B/4B/6B — §5). Voir
> l'ordre d'actionnabilité en tête de ce §2 (décision utilisateur 2026-09-13).

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

Séquence **recommandée** — elle suit l'arbitrage de l'utilisateur, pas
l'ancienneté des fiches. Aucune date calendaire n'est imposée : le dépôt
avance jalon par jalon, un jalon = un commit.

| Rang | Jalon | Effort | Dépend de | État |
|---|---|---|---|---|
| — | ~~**CCC 7–8** — interop OmniPath / LIANA~~ | — | — | ✅ **LIVRÉ le 2026-09-13** (`a88577f`, route (b) sans dépendance) — `STATUS.md` §2aa |
| — | ~~**CCC 9** — rare-cell annotator = **question 1 : rareté par population**~~ | M | — | ✅ **LIVRÉ le 2026-09-13** (`8c1a969`) — `STATUS.md` §2ac ; questions 2 et 3 non retenues |
| — | ~~**STAT-S2** — réseau d'enrichissement (`emapplot`/`cnetplot`)~~ | M | — | ✅ **LIVRÉ le 2026-09-13** — `STATUS.md` §2ad |
| — | ~~**STAT-S3** — clustering de profils (kmeans MVP)~~ | L | — | ✅ **LIVRÉ le 2026-09-13** — `STATUS.md` §2ae ; V2 floue (Mfuzz) non retenue |
| 4 | **NEW-1** — dose-réponse / time-course | M | — | 🟢 **PROCHAIN** (re-mesurer la fiche §6 avant planification ; dépendance `drc` = justification renv.lock requise) |
| 5 | **NEW-2** — fusion de jeux | M | ~~STAT-S1~~ ✅ prérequis levé | 🟢 prêt |
| 6 | **NEW-3** — réseau PCSF | L | interactome local vs contrainte offline | 🔵 backlog conditionnel |
| 7 | **UX 3B / 4B / 6B** | M | décision 2 | ⏸ en attente d'arbitrage |
| 8 | **4E-4** — exécution async de la DA | M | décision 1 (pool) | ⏸ en attente d'arbitrage |
| 9 | Boutons d'export DT sur les ~43 tables restantes + `pageLength` normalisé | M | décision ouverte (changement **visible**) | ⏸ jalon dédié à créer |

Hors séquence, **gelé** : CCC 5–6 (sans suite). Hors séquence, **non demandé** :
élargissement du cache (règle 8).

### Flux A — Présentation & statistiques · `ROADMAP_presentation_stats.md`
Quick wins **tous livrés** : PLOT-Q1..Q5 (`4dee553`) et STAT-Q1..Q4
(`fda5a81`..`5f3c9f0`). Puis **PLOT-S1..S5 livrés** (S1..S3 commités, **S4 et
S5 non commités** — décision utilisateur) ; **PLOT-S6 déjà satisfait par
l'arbre** (mesuré 2026-09-12) et **PLOT-S6′ non retenu** → volet présentation
**clos**. Côté statistiques : **STAT-S1 ✅ livré le 2026-09-12** (ComBat-seq).
Restent **STAT-S2..S3**, **NEW-1..3**.

Ordre conseillé (fondations d'abord) :
`PLOT-S1 → PLOT-S2 → PLOT-S3 → PLOT-S4 → PLOT-S5` ✅ **fait** ; `PLOT-S6` ✅
**déjà fait par l'arbre** ; `STAT-S1` ✅ **fait** ; `STAT-S2` ✅ **livré le
2026-09-13** (réseau d'enrichissement, `STATUS.md` §2ad) ; `STAT-S3` ✅
**livré le 2026-09-13** (clustering de profils, `STATUS.md` §2ae). Restent
`NEW-1..3` (**NEW-1 prochain** ; prérequis de `NEW-2` levé par STAT-S1).

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
  `docs/contracts/` simultanément. Les **24** contrats gelés ne changent que
  selon cette règle.
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
| 1 | **4E-4** : pool mirai dédié SC / pool applicatif unique / rester synchrone | Architecture async de la DA |
| 2 | **UX** : options 3B, 4B, 6B | Reprise de la refonte UX |
| 3 | ~~**Bulk V2** : lui créer une roadmap dédiée ?~~ — ✅ **TRANCHÉ le 2026-09-12** : `docs/ROADMAP_BULK_V2.md` créée (Flux E complet M1→M5) | ~~Traçabilité~~ résolue |
| 4 | **`docs/` gitignoré** : garder local, ou versionner `docs/*.md` + `docs/contracts/*.md` ? | Les **24** contrats ne sont pas versionnés — voir la remarque ci-dessous (⚠️ un fichier **sourcé** ne doit jamais être ignoré : cas corrigé le 2026-09-13, garde C3) |
| 5 | ~~**Modes 1/2 du double jeu SC** : priorité et périmètre~~ — ✅ **TRANCHÉ par réalisation le 2026-09-13** : `MD-4` livre le conteneur `sc_datasets` + les relations déclarées `standalone` / `shared_params` (mode 1) / `distinct_params` (mode 2), contrat `SC_MULTI_CONTRACT.md` (`STATUS.md` §2w) | ~~Direction produit du module SC~~ résolue — reste l'**usage** des modes dans les panneaux d'analyse (évolution, pas un correctif) |
| 6 | ~~**PLOT-S6′**~~ — ✅ **TRANCHÉE le 2026-09-12 : PLOT-S6 clos, repli PLOT-S6′ non retenu** | Volet « présentation » **terminé** — voir `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` |
| 7 | **Outillage MCP** : ~~snapshoter `mcptools`/`btw`/`ellmer` dans `renv.lock`, ou assumer qu'ils restent locaux ?~~ → **recommandation posée le 2026-09-12** (`STATUS.md` §2s) : profil renv `dev` séparé, pas de snapshot dans le lockfile principal — **à valider**. Diagnostic ABI §2p fait le soir : hypothèse d'un décalage massif **réfutée** (seul `jsonlite` est Built 4.4.3) ; template de connexion ZCode posé (`mcp.examples/`) | Un clone neuf ne peut pas démarrer `scripts/mcp_server.R` tant que ce n'est pas tranché |
| 8 | ~~**Multi-échantillons + pseudobulk « comme Spatial »**~~ — ✅ **LIVRÉ le 2026-09-13** : `MD-1` (conteneur `bulk_datasets`), `MD-2` (comparaison), `MD-3` (pont pseudobulk), `MD-4` (conteneur `sc_datasets`) + `4F-EXT` (rapport consolidé) | ~~Chantier neuf~~ **clos** — voir `docs/ROADMAP_MULTI_DATASET.md` |
| 9 | ~~**CCC phases 7–8** : route (a) `OmnipathR`/`liana` avec `renv.lock` justifié, ou (b) **import de résultats LIANA externes sans dépendance** ?~~ — ✅ **TRANCHÉ et LIVRÉ le 2026-09-13** : **route (b)** retenue, implémentée (`a88577f`), contrat étendu. La route (a) reste une décision **séparée**, non demandée | Interop inter-méthodes ; (b) n'ajoute aucune dépendance et respecte le périmètre « import-only » |
| 10 | **Dette de conventions** (relevé 2026-09-13) : C6 = 16 `library()` au top-level de `R/`, C9 = 37 fichiers sans test éponyme, C10 = 270 `stop()` non classés — chantier de réduction, ou statu quo avec plafond ? | Qualité long terme ; les compteurs ne doivent **pas augmenter** (garde `tools/check_conventions.R`) |
| 11 | ~~**CCC phase 9** : laquelle des trois questions (rareté par population / par voisinage / × communication) ?~~ — ✅ **TRANCHÉE le 2026-09-13 : question 1 — rareté par population annotée** → proposition `docs/proposals/CCC_9_POPULATION_RARITY_PROPOSAL.md`. Restent **5 décisions de conception** à valider (§12 de la proposition) | Périmètre du jalon CCC 9 — voir `STATUS.md` §2ab |

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
