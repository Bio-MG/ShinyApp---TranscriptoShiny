# Changelog

Tous les changements notables de TranscriptoShiny (« Cerberus ») sont documentés ici.
Format inspiré de [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/) ;
versionnement [SemVer](https://semver.org/lang/fr/). Une étape = un commit sur `main`.

## [V1.1.0-rc] — 2026-09-05 (vague UX/UI, mandat utilisateur — candidat pré-release)

Refonte UX/UI fondée sur l'audit utilisateur (6 frictions, see
`docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md`) : **zéro changement de
comportement scientifique**, contrats figés intouchés, IDs de modules
inchangés, migration par lots reversibles (un lot = un commit).

### Ajouté
- **Paradigme pipeline mutualisé** : le pipeline auto (1 clic) est le
  **premier panneau de l'accordéon (0.)** dans les trois domaines (SC, Bulk,
  Spatial), avec badge de paradigme en tête de chaque sidebar (« auto dispo » /
  « mode guidé » / « auto async »).
- **Bulk** : étape « 1. Pipeline Bulk — Contrôle qualité & filtrage » ;
  vue « Résumé Pipeline Bulk » (champs `shared_rv` existants uniquement,
  aucun nouveau calcul) ; dernier panneau = « 4. Livrables — Rapport & Script R ».
- **Spatial** : conteneur standard `layout_sidebar` — étapes numérotées en
  accordéon à gauche (une étape ouverte à la fois), résultats à droite ;
  dataset + statut des daemons toujours visibles dans la sidebar ; le module
  pipeline est scindé (contrôles dans l'accordéon, résumé dans le navset
  droit, même namespace → serveur inchangé).
- **Import** : rappel « mapping des IDs » + bouton natif « Aller au mapping
  des IDs » dans Import Single-Cell/Bulk/GEO (saut via `page_navbar(id="main_nav")`
  + `accordion_panel_open` ; aucun panneau déplacé, aucune UI dupliquée).
- **GEO** : renommé « Source publique (GEO) » (source de données, pas une
  4e modalité), phrase d'orientation ; placé en DERNIER du menu Import
  (préférence utilisateur du 2026-09-05).
- **i18n** : 36 clés FR/EN (glossaire double libellé : Pseudobulk, Vélocité
  ARN, Niches spatiales… + tooltips Moran/Milo/scCODA). Clés figées 8b→9b
  conservées (gate d'intégrité vert).
- **Test fonctionnel** `test-sc-auto-pipeline.R` : run complet de
  `run_sc_auto_pipeline` sur fixture minima (QC → PCA → clustering → UMAP →
  t-SNE → marqueurs → trajectoire → commit) + chemin d'échec gracieux.

### Modifié
- **SC** : accordéon plat (17 panneaux) regroupé en 5 sections parent
  (Préparation / Analyse / Dynamique / Abondance cellulaire / Livrables) ;
  les 4 panneaux DA 8c–8f sont nidifiés en un panneau « Abondance
  différentielle » à onglets internes (A. design — B. Milo/scCODA —
  C. vues croisées ; gating inchangé). Valeurs de panneaux préservées.

### Corrigé
- **Crash Spatial** : le binding accordéon bslib retourne un **vecteur de tous
  les panneaux ouverts** (`multiple=TRUE` par défaut) — l'observer de sync
  crashait par indexation récursive dès l'ouverture d'un 2e panneau.
  Correctif : `multiple = FALSE` (une étape à la fois, comme l'ancien navset
  horizontal) + défense `tail(1)`.
- **Gate G4** (`scripts/verify_release_gates.R`) : faux positif permanent
  depuis le Stage 19 (le script se matchait lui-même sur ses propres regex de
  chemins locaux) ; exclusion de soi via pathspec git.

### Vérification (pré-release)
- Suite complète : **1873 assertions PASS / 0 FAIL / 0 ERROR / 0 SKIP**
  (référence V1.0 : 1858 + 15 du nouveau test fonctionnel).
- Duplication : 0 erreur / 3 warnings pré-existants. Boot headless HTTP 200.
- Gates packaging : 6 PASS / 1 WARN / 0 FAIL. e2e shinytest2 : 13 PASS.

## [Post-V1.0] — 2026-09-04 (polish mandat utilisateur — revue des manquants)

Revue des fonctionnalités manquantes demandée après la V1.0 : les rapports ne
restitution pas les domaines ajoutés depuis leur création.

### Rapport Rmd Single-Cell (panneau 9)
- **Nouvelles sections** (tables pures, restituées du résultat canonique tel
  quel — aucune re-exécution, aucun recalcul de figure) : **Vitesse ARN**
  (statut, dimensions alignées, analysis_id), **Communication cellulaire**
  (méthode source, compteurs d'import, table canonique en extrait), **DA**
  (design expérimental + éligibilités, Milo — voisinages avec disclaimer
  « niveau voisinage », scCODA — effets crédibles avec disclaimer « pas des
  p-values »). Opt-in via les cases « Sections » du panneau 9 (aucun changement
  des rapports par défaut) ; paramètres canoniques exposés depuis
  `shared_rv` (expositions Stage 17 + pseudobulk).

### Rapport consolidé 4F (panneau 9b)
- **Domaines ajoutés au contrat (9 → 11)** : `pseudobulk` (exposition additive
  `shared_rv$pseudobulk_result` du panneau 4b — moteur, contraste, table DE)
  et `correlation` (gène cible + table de corrélations, déjà dans l'état
  partagé). Bundle : `tables/correlation.csv`, `tables/pseudobulk_de.csv`.
  Contrat + test de freeze + tests fonctionnels mis à jour simultanément.
- i18n : 10 clés FR/EN ajoutées (libellés de sections + disclaimers
  scientifiques).

### Restants (parking V1.x, décision requise)
- Rapports Rmd Bulk/Spatial : périmètre déjà complet par domaine (contrôlé).
- Compilation 4F des états Bulk/Spatial : proposition (extension contrat).
- 4D-3 / 4E-4 : inchangés (voir `UPGRADE_AND_COMPATIBILITY.md` §3).

## [V1.0.0] — 2026-09-04 (Stage 20)

Déclaration V1.0 : toutes les exigences de la roadmap vérifiées et prouvées
(`docs/release/V1_0_DECLARATION.md`) — contrats figés, provenance active,
rapports compilés, exports traçables, limitations documentées, gates RC passées.

### Vérification finale
- Suite complète : **1837 PASS / 0 FAIL / 0 ERROR / 0 SKIP / 16 warnings bénins**.
- Gates packaging : 6 PASS / 1 WARN documenté / 0 FAIL ; duplication 0 erreur ;
  lancement headless HTTP 200.
- Tags : `v1.0.0-rc.1` → `v1.0.0` (locaux ; push après confirmation).

## [V1.0.0-rc.1] — 2026-09-04 (Release Candidate, Stage 19)

Première version formelle candidate à la V1.0. Macro-fonctionnalités livres :
plateforme multi-omique 3 domaines (Single-Cell, Bulk RNA, Spatial), vitesse ARN
(3B), communication cellulaire (4D), abondance différentielle (4E), rapport
consolidé + reproductibilité (4F), durcissement release.

### Ajouts — Analyse Single-Cell
- **Vitesse ARN (Stages 8–10)** : import mtx/rds validé, 9 états de validité,
  empreinte d'objet v2, contrat résultat figé (`VELOCITY_RESULT_CONTRACT.md`),
  visualisations consommatrices pures + exports PNG/PDF/CSV.
- **Communication cellulaire (Stages 11–12)** : import CellChat/CellPhoneDB
  (schémas stricts, jamais de devinette), table canonique 12 champs,
  harmonisation d'identités par correspondance EXACTE, QC d'import, vues
  exploratoires (dotplot, heatmap pathways, réseau circulaire ggplot2 pur),
  centralité descriptive, filtres avec provenance. Génération depuis données
  brutes = proposition 4D-3 (parking).
- **Abondance différentielle (Stages 13–16)** : validation de design
  expérimental bloquant la pseudoreplication (les cellules ne sont pas des
  réplicas biologiques) ; Milo 4E-1 (voisinages, seed appliquée) ; scCODA 4E-2
  (composition par échantillon, environnement Python explicite, diagnostic de
  convergence en pur R) ; vues croisées 4E-3 descriptives (7 catégories de
  concordance figées, aucune p-value de consensus).
- **Rapport consolidé 4F (Stage 17)** : compilateur d'état canonique + de
  provenance (aucune ré-exécution) — collecteur, validateur à 7 états (les
  sections sans provenance sont refusées), rendu HTML autonome sans pandoc,
  bundle d'export projet (manifeste, tables fidèles, script R reproductible,
  session info).

### Ajouts — Socle & release
- **Durcissement (Stage 18)** : matrice 14 catégories × domaines
  (`docs/release/HARDENING_MATRIX.md`), tests de stress (rapport à 1200 entrées
  de provenance), baseline performance synthétique, limitations connues
  documentées.
- **Packaging RC (Stage 19)** : `scripts/verify_release_gates.R` (lock valide,
  aucune dépendance obligatoire non verrouillée, i18n sans doublon, aucun
  chemin local/credential dans les fichiers suivis) ; renv.lock complété des
  dépendances obligatoires manquantes **shiny.i18n 0.3.0, shinyWidgets 0.9.1,
  shinycssloaders 1.1.0** (419 packages au total) ; test de non-régression
  i18n (un doublon de clé FR fait crasher le démarrage).

### Corrections
- Onglet « 3. Visualisation » : le Bloc 3 « Dynamique & Écosystème » passe de
  « à venir » à « disponible » (panneaux 8 → 9b livrés) — la carte annonçait
  des fonctionnalités existantes comme futures.
- Clé i18n dupliquée « Verdict » (crash au démarrage via shiny.i18n) —
  détectée par le gate de lancement headless, corrigée avant commit.

### Sécurité / données
- Aucune donnée brute embarquée dans les rapports par défaut ; exports =
  résumés et tables de résultats uniquement ; noms de fichiers sources
  originaux seulement (jamais de chemins locaux) dans la provenance.

## [Versions antérieures — non étiquetées]
- Chrysalis 2A–2F : socle `R/core/` (state, validation, provenance, jobs,
  caching, io/pathway helpers) avec tests ; gate de duplication.
- Phases 1–7 historiques : import SC/Bulk/Spatial, pipeline SC (QC, HVG,
  PCA/UMAP/t-SNE, clustering, sketch, BPCells), marqueurs, corrélations,
  pathways, trajectoire, annotation SingleR, pseudobulk, rapports Rmd par
  domaine, spatial (QC, clustering, déconvolution RCTD/STdeconvolve, niches,
  Moran, multi-échantillons).
