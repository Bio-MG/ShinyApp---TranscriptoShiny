# CONVENTIONS.md — conventions de code TranscriptoShiny ("Cerberus")

> **Rôle de ce fichier** : une seule source de vérité pour les conventions de
> CODE. Il ne décrit ni l'état (`docs/STATUS.md`), ni la navigation
> (`docs/ROADMAP.md`), ni les règles dures d'environnement et de périmètre
> (`AGENTS.md`). Il dit **comment écrire** le code pour rester cohérent avec
> l'arbre.
>
> Créé le 2026-09-13 (passe de maintenance « conventions »). Chaque règle porte
> un identifiant **C1..C12** : ceux-ci sont les mêmes que dans
> `tools/check_conventions.R`, qui les **vérifie mécaniquement**. Une
> convention non vérifiable n'est qu'un vœu — toute nouvelle règle doit donc
> entrer dans les deux à la fois.
>
> **Langue** : ce document et les commentaires sont en français ; les messages
> visibles par l'utilisateur sont en français et passent par `tr()`.

---

## 1. Vérification automatique (à lancer avant tout merge)

```bash
cd "D:/Data_science/SHINYAPP test (git work)/SHINYAPP test"

# Garde des conventions (0 erreur attendu ; les avertissements = dette mesurée)
"D:/Data_science/R-4.4.2/bin/Rscript.exe" tools/check_conventions.R

# Idem, mais la dette fait échouer la garde (usage CI stricte)
"D:/Data_science/R-4.4.2/bin/Rscript.exe" tools/check_conventions.R --strict

# Garde anti-duplication (baseline : 0 erreur / 3 avertissements)
"D:/Data_science/R-4.4.2/bin/Rscript.exe" tools/check_duplication.R modules R . --ext=R
```

Les deux gardes sont en **base R uniquement** : exécutables sans installer
quoi que ce soit, ce qui est indispensable pour un contrôle pré-merge.

**Durée** : `check_conventions.R` tourne en ~30 s, dont ~13 s d'activation renv
incompressibles. C'est délibérément sous le timeout par défaut des shells non
interactifs (120 s) : au-delà, le garde est tué **avant** de rendre son verdict,
ce qui donne l'illusion d'un outil instable. Mesurer avec une sonde
`system.time()` par règle avant d'optimiser — le poste le plus coûteux n'est
presque jamais celui qu'on imagine.

> **Après toute modification d'une règle, jouer un CAS NÉGATIF.** Créer un
> fichier qui viole la règle, vérifier qu'elle le signale **et** que le code de
> sortie vaut 1, puis retirer la sonde. Un garde qui affiche « 0 erreur » peut
> simplement ne plus rien détecter : le run nominal ne prouve rien.
>
> Incident réel du 2026-09-13 : `git check-ignore --stdin` ne reçoit aucun
> `stdin` via `system2()` sous Windows (stdin vide → aucun chemin signalé),
> alors que la même commande fonctionne en shell. La règle C3 — la plus
> importante du garde — était devenue muette tout en restant verte. Les chemins
> sont désormais passés en **arguments**, par lots de 200.

---

## 2. Arborescence et responsabilités

| Dossier | Contient | Ne contient **jamais** |
|---|---|---|
| `R/` | logique pure, testable hors Shiny | `input$`, `output$`, `render*()`, `observeEvent()`, `reactive()` |
| `modules/` | UI + orchestration réactive | du calcul métier (extraire dans `R/`) |
| `config/` | paramètres **déclarés** (`TS_*`) | des valeurs codées en dur ailleurs |
| `reports/` | templates de rapport | de la logique de calcul |
| `tests/testthat/` | tests + helpers de fixtures | des tests qui dépendent du réseau |
| `tools/` | gardes statiques, outils de maintenance | du code chargé par l'app |
| `i18n/` | `translation.json` (paires `{fr, en}`) | des clés dupliquées (fait planter le boot) |
| `docs/` | pilotage : `STATUS.md`, roadmaps, contrats | — (non versionné, voir §12) |

Règle transverse : `app.R` source **tout** dans un seul environnement global.
Deux conséquences directes, déjà arrivées dans ce dépôt :

1. une fonction top-level définie deux fois ⇒ la dernière gagne silencieusement
   (d'où `tools/check_duplication.R`) ;
2. un `library()` exécuté au `source()` **attache** le paquet pour toute
   l'application et peut masquer une fonction maison (d'où la règle C6).

---

## 3. `R/` — logique pure

### 3.1 Interdits (C2)

Aucun symbole de réactivité : `input$`, `output$`, `renderPlot/UI/Table()`,
`downloadHandler()`, `observeEvent()`, `observe()`, `reactive()`,
`reactiveVal()`, `reactiveValues()`.

### 3.2 Exception assumée — la couche d'état (C2)

`R/core/state.R` (et son shim de rétro-compatibilité `R/sc/sc_state.R`) **a
pour rôle** de fabriquer les schémas `reactiveValues` partagés. C'est la seule
partie de `R/` autorisée à le faire, et elle est explicitement documentée
comme telle en en-tête de fichier. Partout ailleurs, c'est une erreur.

### 3.3 Injection explicite de `input` / `session` (C2)

Une fonction de `R/` peut **recevoir** `input` ou `session` en paramètre —
c'est de l'injection de dépendance, pas de la réactivité :

```r
run_sc_auto_pipeline <- function(input, global_data, shared_rv, session, sc_log_rv)
.safe_plot_render   <- function(session, output_id, plot_fn, min_px = 30)
```

En revanche, y accéder sans que l'identifiant soit un paramètre de la fonction
englobante est une erreur : dans ce cas, déplacer l'appel dans `modules/`.

### 3.4 Dépendances (C6)

Pas de `library()` / `require()` au top-level de `R/` : utiliser
`requireNamespace("pkg", quietly = TRUE)` puis `pkg::fonction()`. Les 16
occurrences actuelles sont de la dette héritée (listées §12) ; **aucune
nouvelle** n'est acceptée.

---

## 4. `modules/` — UI et orchestration

- Un module = `mod_<domaine>_<sous-partie>.R` exposant `*_ui()` / `*_server()`.
- Les namespaces se créent avec `NS(id)` ; aucun `input$` global.
- Toute chaîne visible passe par `tr()` / `i18n$t()` (C7).
- Le calcul lourd se délègue à `R/` ; le module ne fait qu'orchestrer et
  afficher.

---

## 5. Nommage

| Objet | Convention | Exemple |
|---|---|---|
| Fichier `R/` | `snake_case`, préfixé du domaine | `R/bulk/bulk_multi_compare.R` |
| Fichier `modules/` | `mod_<domaine>_<role>.R` | `modules/bulk/mod_bulk_multi.R` |
| Fonction | `snake_case`, verbe d'abord | `build_cellchat_input()` |
| Helper partagé transverse | préfixe `ts_` | `ts_theme()`, `ts_export_plot()`, `ts_datatable()`, `ts_complex_heatmap()` |
| Fonction interne | préfixe `.` | `.parse_formals()` |
| Erreur classée | classe `<domaine>_error` | `communication_import_error` |
| Paramètre déclaré | `TS_<DOMAINE>_<NOM>` dans `config/` | `TS_VELOCITY_OVERLAP_MIN` |
| Test | `tests/testthat/test-<domaine>-<fichier>.R` | `R/bulk/bulk_gsva.R` → `test-bulk-gsva.R` |
| Freeze test de contrat | `test-<sujet>-contract-freeze.R` | `test-bulk-multi-contract-freeze.R` |

---

## 6. Contract-first (règle transversale)

Tout nouveau domaine = **code + freeze test + `docs/contracts/*.md`
simultanément**. Un contrat ne change que si les trois bougent dans le même
commit. État au **2026-09-14** : **28 contrats gelés**, tous référencés par au
moins un test (**C8 = 0**). Le **nommage** du freeze test n'est pas uniforme :
**24** contrats ont un fichier éponyme `test-<sujet>-contract-freeze.R` ; **5**
(`BULK_DOSE_RESPONSE`, `BULK_PATTERN`, `PLOT_DATATABLE`, `PLOT_EXPORT`,
`PLOT_HEATMAP`) portent leurs assertions de gel **dans le test principal** du
domaine. C'est un écart de nommage, **pas** une exception à la règle : aucun
contrat n'est sans test.

Conséquence pratique : avant de modifier un objet « gelé », ouvrir son
contrat, puis le freeze test correspondant — jamais l'inverse.

---

## 7. Erreurs et messages

```r
# Forme attendue (domaine nouveau ou code neuf)
stop(errorCondition(
  sprintf("Échec %s : une matrice numérique de counts bruts est requise (reçu : %s).", ctx, cls),
  class = "batch_correction_error"
))

# Forme acceptable (héritage)
stop("Message en français.", call. = FALSE)
```

- Message **en français**, explicite, avec la valeur reçue quand c'est utile.
- Erreur **classée** (`class = "<domaine>_error"`) pour qu'un appelant puisse
  la distinguer.
- Succès renvoyé en `invisible(...)`.
- Dette mesurée : 270 `stop()` non classés (C10) — **interdit dans le code
  neuf**.

---

## 8. i18n

- Toute chaîne visible passe par `tr()` / `i18n$t()` ; **la clé est le texte
  français**.
- Ajout de clés : `tools/add_i18n_keys.R` (idempotent, ajoute `{fr, en}`,
  refuse les doublons — une clé `fr` dupliquée fait planter le démarrage,
  gardé par `test-i18n-integrity.R`).
- La traduction anglaise est **obligatoire** : jamais d'entrée `en` vide.
- C7 vérifie que les ~2000 clés utilisées existent dans
  `i18n/translation.json` : **0 manquante** au 2026-09-13 (101 clés ajoutées
  lors de la passe de maintenance).

---

## 9. Paramètres déclarés

Aucune valeur de seuil / limite / format codée en dur dans `R/` ou
`modules/` : elle se déclare dans `config/` (`thresholds.R`, `defaults.R`)
sous la forme `TS_*` et se consomme par son nom.

---

## 10. Async, cache, provenance

- **Un seul pool de workers : mirai.** Jamais `BiocParallel::MulticoreParam`
  sous Windows (pas de fork fiable) → `SerialParam` ; jamais
  `WGCNA::enableWGCNAThreads()` ; pas de second framework (`future` est
  toléré uniquement pour forcer `plan("sequential")` autour d'appels
  Seurat/Spatial).
- **Cache** limité au périmètre de la règle 8 (trajectory, velocity, markers,
  pathways, déconvolution spatiale). Ni communication, ni DA — demander avant
  d'étendre.
- **Provenance** PRODUITE à chaque étape d'analyse, CONSOLIDÉE au rapport —
  jamais reconstruite après coup.
- **Nouvelle dépendance** : justification documentée obligatoire ; la
  convention « nouvelle dépendance = `renv.lock` justifié » **n'est pas
  immuable** (décision utilisateur 2026-09-13) : une dépendance qui apporte
  une efficience sans régression reste acceptable documentée.

---

## 11. Tests et non-régression

- `testthat` ; un jalon = un commit ; ne jamais citer un compte partiel comme
  référence.
- Référence de non-régression = **suite complète** (`tools/run_full_suite.R`),
  pas un filtre.
- Baseline au 2026-09-13 : **0 FAIL / 0 ERROR — 4511 PASS / 1 SKIP**
  (skip = ping LIVE conditionnel de `test-mod-geo.R`) ; duplication gate
  **0 erreur / 3 avertissements**.
- Fixtures : `helper-*.R` dans `tests/testthat/` (communication, velocity, DA,
  Milo, scCODA).

---

## 12. Ce que la garde mesure (relevé du 2026-09-13)

| Règle | Niveau | État |
|---|---|---|
| C1 `R/modules/` interdit | ERREUR | 0 |
| C2 réactivité Shiny hors `modules/` | ERREUR | 0 |
| C3 cible de `source()` existante **et versionnée** | ERREUR | 0 |
| C4 aucun `setwd()` | ERREUR | 0 |
| C5 aucun `browser()` | ERREUR | 0 |
| C6 `library()`/`require()` au top-level de `R/` | AVERT. | **16** |
| C7 clés i18n complètes | ERREUR | 0 (2367 clés) |
| C8 contrat référencé par un test | AVERT. | 0 (24/24) |
| C9 fichier de `R/` avec test éponyme | AVERT. | **37** fichiers sans test éponyme |
| C10 `stop()` classé ou `call. = FALSE` | AVERT. | **270** |
| C11 primitive parallèle à vérifier | AVERT. | **1** (`MulticoreParam` sous garde Unix) |
| C12 en-tête commenté | AVERT. | 0 (62/62) |

Les compteurs d'avertissements sont des **plafonds** : ils ne doivent pas
augmenter. Les faire baisser est un chantier, pas une correction implicite.

---

## 13. Anti-pièges appris sur ce dépôt

1. **Re-mesurer avant de planifier.** Trois fiches de roadmap se sont révélées
   périmées d'affilée (`base_size`, `pageLength`, `plotly`) parce que l'arbre
   avait évolué après leur rédaction.
2. **Un fichier sourcé ne doit jamais être dans `.gitignore`.** C'est arrivé :
   `R/plotting/complex_heatmap.R` (PLOT-S4) n'avait **jamais été commité** —
   exclu par le `.gitignore`, alors que `app.R:64` le source ; l'app ne
   démarrait que sur ce poste. Corrigé le 2026-09-13 (premier commit du
   fichier) ; la règle **C3** empêche la récidive.
   ⚠️ Le `.gitignore` lui-même n'est **pas** versionné (dé-tracqué en
   `7e67581`) : c'est un fichier local, à ne pas confondre avec une règle
   partagée.
3. **Attention aux faux positifs statiques** : `report_input$type` n'est pas
   `input$`, et un `library(Seurat)` peut n'exister que dans le texte d'un
   script généré. La garde retire chaînes et commentaires avant d'analyser.
4. **Le chemin du projet contient des parenthèses** (`… (git work) …`) : ne
   jamais l'injecter tel quel dans une expression régulière.
