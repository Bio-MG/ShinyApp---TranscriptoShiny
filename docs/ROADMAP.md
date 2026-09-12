# ROADMAP.md — Orchestrateur des chantiers

> **Point d'entrée unique.** Ce fichier ne décrit ni l'état, ni les règles, ni
> le détail des tâches : il dit **quoi lire, dans quel ordre, et où aller**.
>
> | Question | Fichier |
> |---|---|
> | Où en est-on ? (état réel, commits) | **`docs/STATUS.md`** — source de vérité |
> | Quelles sont les règles non négociables ? | **`AGENTS.md`** (racine, local) |
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
| `docs/STATUS.md` | État réel : livré / ouvert / bloqué + refs de commit | Au début et à la fin de chaque session |
| `docs/ROADMAP_presentation_stats.md` | Plots & stats : PLOT-Q/S, STAT-Q/S, NEW-1..3 | Pour tout travail présentation/statistique |
| `docs/ROADMAP_CCC_ADVANCED.md` | CCC avancée, phases 1–10 + conditions de déblocage | Pour tout travail communication |
| `docs/ROADMAP_HANDOFF_STAGE_11_20.md` | Stages 8–20 (V1.0, livré) + **HANDOFF V1.x** | Historique V1.0 ; entrées V1.x |
| `docs/ROADMAP_HANDOFF_NEXT.md` | **Prochaine session** : périmètre PLOT-S1, ancres vérifiées, décisions à prendre, prompt à coller | **Au démarrage de la prochaine session** |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_Q.md` | Rapport de stage PLOT-Q (6 sections) | Exemple du format de rapport attendu |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_Q.md` | Rapport de stage STAT-Q (6 sections) | Historique du volet statistique |
| `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` | Mesure PLOT-S6 (6 sections) — **prémisse périmée, jalon déjà satisfait par l'arbre** | Avant de planifier PLOT-S6 / PLOT-S6′ |
| `docs/ROADMAP_HANDOFF_STAGE_STAT_S1.md` | Rapport de stage STAT-S1 (6 sections) — **ComBat-seq** | **Dernier rapport livré (2026-09-12)** |
| `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md` | Conception des lots UX (⚠️ état corrigé en tête) | Avant toute reprise UX |
| `docs/proposals/CCC_DATA_PATH_ASSESSMENT.md` | 10X → CellChat : faisabilité et chemins | Avant 4D-3 / phases 5–6 |
| `docs/release/UPGRADE_AND_COMPATIBILITY.md` | §3 = parking officiel V1.x (4D-3, 4E-4, 4F) | Pour les propositions V1.x |
| `docs/contracts/*.md` | **18** contrats gelés (contract-first) — ⚠️ non versionnés (`docs/` gitignoré) | Avant de toucher un domaine gelé |
| `docs/kanban_roadmap.html` | Tableau visuel (lecture seule, miroir de `STATUS.md`) | Démonstration / vue d'ensemble |
| `CHANGELOG.md` (racine, suivi par git) | Historique des livraisons | Après un commit notable |

---

## 2. Les quatre flux de travail

> **▶ Prochaine étape : `STAT-S2` — réseau d'enrichissement** (`emapplot` /
> `cnetplot`, effort **M** ; `enrichplot` est **déjà présent**). **`STAT-S1`
> (ComBat-seq) a été livré le 2026-09-12** — rapport
> `docs/ROADMAP_HANDOFF_STAGE_STAT_S1.md`, état `STATUS.md` §2q.
>
> Le volet « présentation » (`plot-shared-helpers`) est **terminé et clos** :
> PLOT-S1..S5 livrés ; **PLOT-S6 était déjà satisfait par l'arbre** (mesure du
> 2026-09-12, `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md`) et le repli **PLOT-S6′
> n'est pas retenu** (décision utilisateur du 2026-09-12).
>
> **Direction utilisateur du 2026-09-12, à cadrer** : **analyse
> multi-échantillons avec son propre pipeline**, puis **pseudobulk**, « chacun
> comme le module Spatial ». C'est un chantier **neuf — aucune fiche n'existe** :
> **à transformer en roadmap dédiée avant toute planification** (même situation
> que Bulk V2, §2h).
>
> Restent ensuite `STAT-S3`, `NEW-1..3` (⚠️ le prérequis de `NEW-2` est **levé**
> par STAT-S1), ou un arbitrage (`4E-4`, UX 3B/4B/6B — §5).

> 📌 **Règle ajoutée le 2026-09-12 — re-mesurer avant de planifier.**
> **Trois** fiches §3 de `ROADMAP_presentation_stats.md` se sont révélées
> périmées d'affilée : `base_size` (PLOT-S1), `pageLength` (PLOT-S3), `plotly`
> (PLOT-S6). Les fiches ont été rédigées lors de l'audit initial et l'arbre a
> évolué depuis. **Toute fiche §3 doit être re-mesurée contre le dépôt avant
> d'être planifiée** — c'est le §3.2 du protocole, appliqué plus tôt.

Chaque flux a sa roadmap et son avancement dans `STATUS.md`. Ils sont
**indépendants** sauf mention contraire : on peut en mener un seul à la fois,
mais rien n'oblige à les séquencer entre eux.

### Flux A — Présentation & statistiques · `ROADMAP_presentation_stats.md`
Quick wins **tous livrés** : PLOT-Q1..Q5 (`4dee553`) et STAT-Q1..Q4
(`fda5a81`..`5f3c9f0`). Puis **PLOT-S1..S5 livrés** (S1..S3 commités, **S4 et
S5 non commités** — décision utilisateur) ; **PLOT-S6 déjà satisfait par
l'arbre** (mesuré 2026-09-12) et **PLOT-S6′ non retenu** → volet présentation
**clos**. Côté statistiques : **STAT-S1 ✅ livré le 2026-09-12** (ComBat-seq).
Restent **STAT-S2..S3**, **NEW-1..3**.

Ordre conseillé (fondations d'abord) :
`PLOT-S1 → PLOT-S2 → PLOT-S3 → PLOT-S4 → PLOT-S5` ✅ **fait** ; `PLOT-S6` ✅
**déjà fait par l'arbre** ; `STAT-S1` ✅ **fait**. `STAT-S2` et `STAT-S3` sont
indépendants et parallélisables. `NEW-2` dépendait de `STAT-S1` → **prérequis
levé**.

### Flux B — CCC avancée · `ROADMAP_CCC_ADVANCED.md`
Phases 1–4 livrées. **Phases 5–10 parkées**, chacune avec sa condition de
déblocage (§4 de la roadmap). Les phases 5–6 sont bloquées par le contrat
d'entrée de l'app upstream — voir `STATUS.md` §3 et
`docs/proposals/CCC_DATA_PATH_ASSESSMENT.md`.

### Flux C — Propositions V1.x · `docs/release/UPGRADE_AND_COMPATIBILITY.md` §3
`4D-3` (bloqué upstream) · `4E-4` (décision de pool à prendre) · `4F-ext`
(prêt) · cache (explicitement **non** à étendre).

### Flux D — UX/UI · `docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md`
Lots 0/1/2/3A/4A/6A/5 livrés. Restent les options **3B/4B/6B** (jamais
tranchées) et les libellés techniques des modules enfants Spatial.

### Flux E — Bulk V2 / batch-QC · ⚠️ **sans roadmap**
Travail non commité dans l'arbre. N'appartient à aucun des flux ci-dessus.
**Recommandation** : lui créer sa propre roadmap avant de reprendre, sinon il
restera le seul chantier sans état traçable. Voir `STATUS.md` §2h.

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
  `docs/contracts/` simultanément. Les 13 contrats gelés ne changent que selon
  cette règle.
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
| 3 | **Bulk V2** : lui créer une roadmap dédiée ? | Traçabilité du seul chantier sans pilotage |
| 4 | **`docs/` gitignoré** : garder local, ou versionner `docs/*.md` + `docs/contracts/*.md` ? | Les 13 contrats ne sont pas versionnés — voir la remarque ci-dessous |
| 5 | **Modes 1/2 du double jeu SC** : priorité et périmètre | Direction produit du module SC |
| 6 | ~~**PLOT-S6′**~~ — ✅ **TRANCHÉE le 2026-09-12 : PLOT-S6 clos, repli PLOT-S6′ non retenu** | Volet « présentation » **terminé** — voir `docs/ROADMAP_HANDOFF_STAGE_PLOT_S6.md` |
| 7 | **Outillage MCP** : snapshoter `mcptools`/`btw`/`ellmer` dans `renv.lock` (avec justification), ou assumer qu'ils restent locaux à cette machine ? | Un clone neuf ne peut pas démarrer `scripts/mcp_server.R` tant que ce n'est pas tranché — `STATUS.md` §2p |
| 8 | **Multi-échantillons + pseudobulk « comme Spatial »** (demandé le 2026-09-12) : créer une roadmap dédiée ? Quel périmètre (Bulk, SC, les deux) ? | Chantier **neuf, sans aucune fiche** — à cadrer avant planification |

> **Remarque sur la décision 4** — garder roadmaps et instructions d'agents
> locales est un choix défendable. En revanche `docs/contracts/*.md` est un cas
> différent : `AGENTS.md` impose qu'un changement de contrat se fasse
> **code + freeze test + doc simultanément**. Si le contrat n'est pas versionné,
> cette règle est mécaniquement inapplicable (le doc ne peut pas être dans le
> même commit). C'est le seul point où le `.gitignore` contredit une règle
> écrite du dépôt.
