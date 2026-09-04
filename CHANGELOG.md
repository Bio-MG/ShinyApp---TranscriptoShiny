# Changelog

Tous les changements notables de TranscriptoShiny (« Cerberus ») sont documentés ici.
Format inspiré de [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/) ;
versionnement [SemVer](https://semver.org/lang/fr/). Une étape = un commit sur `main`.

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
