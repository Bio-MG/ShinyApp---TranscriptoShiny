# TranscriptoShiny v1.1

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/language-R-blue.svg)](https://www.r-project.org/)

**FR** — Une application R Shiny modulaire pour l'analyse transcriptomique et multi-omique.  
**EN** — A modular R Shiny application for transcriptomic and multi-omics analysis.

TranscriptoShiny prend en charge les workflows **single-cell RNA-seq**, **bulk RNA-seq** et **transcriptomique spatiale** (Visium/Xenium), avec un accent sur **l'interactivité**, **la robustesse**, **la scalabilité** et **la reproductibilité**.

TranscriptoShiny supports **single-cell RNA-seq**, **bulk RNA-seq**, and **spatial transcriptomics** (Visium/Xenium) workflows, with a strong focus on **interactivity**, **robustness**, **scalability**, and **reproducibility**.

---

## Langues / Languages

- [Français](#français)
- [English](#english)

---

# Français

## Présentation

TranscriptoShiny est conçu pour une exploration itérative de données transcriptomiques par des biologistes et bioinformaticiens, avec une architecture modulaire orientée réutilisabilité.

L'application s'appuie sur :

- une séparation claire entre **import**, **prétraitement**, **analyse** et **visualisation** ;
- un état partagé via `reactiveValues` pour stocker les objets `sc_obj`, `bulk_obj` et `spatial_obj` ;
- une approche **local-first**, adaptée à une utilisation sur station de travail R/RStudio ;
- une interface pensée pour guider l'utilisateur avec des contrôles progressifs et des sorties interactives.

## Nouveautés de la v1.1 (Step-2)

- **Refactoring `global.R`** : le fichier monolithique (>3000 lignes) est désormais découpé en 5 fichiers distincts (`global.R` + 4 `helpers_*.R`) pour une meilleure maintenabilité.
- **Import GEO hors-ligne** : `parse_geo_series_matrix()` — parsing direct d'un fichier `*_series_matrix.txt` GEO sans `GEOquery`, sans accès réseau.
- **Onglet Venn / UpSet multi-contrastes** : comparaison visuelle des ensembles de gènes DEG entre contrastes, avec table des intersections et export CSV/PNG.
- **Rapport multi-contrastes enrichi** : tableau récapitulatif de tous les contrastes + diagramme UpSet intégré dans le rapport HTML/PDF exporté.
- **Garde-fous design expérimental** : hard-block sur n=1 après NA d'une covariable et sur covariable à une seule modalité observée — couvre les modes single-pair ET pairwise-auto.
- **Infrastructure de test** : scripts `shiny::testServer()` dans `tests/manual/` pour validation sans navigateur.

## Nouveautés de la v1.0

- **Refonte architecturale** : le dossier `modules/` est désormais organisé par domaines fonctionnels (`import/`, `sc/`, `bulk/`, `spatial/`) pour simplifier la maintenance et l'évolution du code.
- **Bulk RNA-seq renforcé** : import plus robuste des matrices de counts bruts, meilleur alignement counts/métadonnées, et intégration de workflows différentiels plus propres.
- **UI Single-Cell améliorée** : meilleure organisation des panneaux, navigation plus lisible et espace graphique optimisé.
- **Base v1.0 plus modulaire** : meilleure séparation des responsabilités entre modules d'import, pipeline, visualisation, annotation et analyses avancées.

## Fonctionnalités principales

### Single-Cell RNA-seq

- **Import** : dossiers 10X, fichiers `.rds`, `.h5`, `.h5ad`, `.loom`, y compris des scénarios multi-échantillons avec conservation de `orig.ident`.
- **Pipeline standard** : contrôle qualité, normalisation, PCA, réduction de dimension, clustering, UMAP/t-SNE.
- **Correction de batch** : intégration de méthodes comme **Harmony** selon le contexte expérimental.
- **Annotation cellulaire** : annotation automatique via **SingleR**.
- **Analyse de marqueurs** : détection via `FindAllMarkers` et exploration interactive des résultats.
- **Visualisation** : plots dédiés pour embedding, expression, distributions, corrélations et analyses exploratoires avancées.
- **Fonctions avancées** : enrichissement de voies, réseaux de corrélation, trajectoires et modules spécialisés réutilisables.

### Bulk RNA-seq

- **Import intelligent** : lecture de matrices de counts, gestion des doublons de gènes (fusion par somme), appariement des métadonnées.
- **Import GEO hors-ligne** : parsing direct d'un fichier `*_series_matrix.txt` via `parse_geo_series_matrix()` — aucun accès réseau requis, aucune dépendance à `GEOquery`.
- **Analyse différentielle** : workflows basés sur **DESeq2**, **edgeR** et **limma** avec garde-fous sur le design expérimental (n=1, covariable single-level).
- **Filtrage et transformation** : pré-filtrage des gènes faiblement exprimés et transformation VST pour l'exploration.
- **Visualisations** : PCA, QC échantillons, volcano plot, MA-plot, heatmap et tableaux de résultats.
- **Venn / UpSet multi-contrastes** : comparaison des gènes DEG entre contrastes, table des intersections exportable (CSV), export PNG.
- **Enrichissement fonctionnel** : ORA/GSEA selon le jeu de résultats disponible.
- **Rapport HTML/PDF** : export complet avec tous les contrastes, tableau récapitulatif n_sig/n_up/n_down et diagramme UpSet si ≥ 2 contrastes.

### Transcriptomique spatiale

- **Import** : données de type 10X Visium.
- **Exploration spatiale** : visualisation et structuration des objets pour analyses spatiales.
- **Développement en cours** : certaines briques restent en évolution selon la feuille de route.

## Architecture du projet

L'application repose sur un cœur Shiny modulaire. Le point d'entrée initialise les ressources globales, puis délègue l'interface et la logique aux modules spécialisés.

```text
ShinyApp---TranscriptoShiny/
├── app.R                   # Point d'entrée de l'application
├── global.R                # Packages et options globales uniquement (~100 lignes)
├── helpers_io.R            # I/O multi-format, mapping IDs, parse_geo_series_matrix()
├── helpers_bulk.R          # DESeq2/edgeR/limma, plots bulk, Venn/UpSet, validate_bulk_design()
├── helpers_sc.R            # Seurat : scatter/violin/corrélation/trajectoire
├── helpers_pathway.R       # ORA/GSEA partagé sc/bulk
├── README.md
├── tests/
│   └── manual/             # Scripts shiny::testServer() — validation sans navigateur
│       ├── test_mod_bulk_de.R
│       └── test_mod_import_bulk.R
├── modules/
│   ├── import/             # Import single-cell, bulk, spatial, GEO, helpers d'entrée
│   ├── sc/                 # Pipeline scRNA-seq, annotation, visualisation, marqueurs
│   ├── bulk/               # Import, DE, Venn/UpSet, pathway, reporting bulk
│   └── spatial/            # Modules transcriptomique spatiale
└── www/                    # CSS, JS et ressources statiques
```

### Principes d'organisation

- **`app.R`** assemble l'interface globale et les modules, source les 4 `helpers_*.R`.
- **`global.R`** centralise uniquement les dépendances et options (~100 lignes).
- **`helpers_*.R`** contiennent toutes les fonctions utilitaires, séparées par domaine.
- **`modules/`** contient les modules Shiny organisés par domaine fonctionnel.
- **`tests/manual/`** regroupe les scripts de validation via `shiny::testServer()`.

## Prérequis

### Environnement recommandé

- **R** ≥ 4.2
- **RStudio** recommandé pour le développement et le débogage
- **Mémoire RAM** :
  - **16 Go minimum recommandés** pour un usage confortable ;
  - **32 Go** conseillés pour des jeux single-cell plus volumineux ;
  - Bulk RNA-seq reste globalement plus léger mais certains enrichissements et visualisations peuvent devenir coûteux.

### Remarques pratiques

- Sous Windows, une session R propre peut être nécessaire après certaines mises à jour de packages Bioconductor.
- Pour les gros jeux de données single-cell, une stratégie mémoire adaptée est recommandée.
- L'application est conçue pour une exécution locale, avec un contrôle maximal sur les dépendances et les fichiers d'entrée.

## Installation

### 1. Cloner le dépôt

```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Installer les dépendances principales

Exécutez dans R ou RStudio :

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "plotly", "DT", "future",
  "ggplot2", "dplyr", "patchwork", "viridis", "igraph",
  "data.table", "harmony"
))

if (!require("BiocManager")) install.packages("BiocManager")

BiocManager::install(c(
  "Seurat",
  "SingleR",
  "celldex",
  "SingleCellExperiment",
  "DESeq2",
  "edgeR",
  "limma",
  "ComplexHeatmap",
  "ReactomePA",
  "KEGGREST",
  "clusterProfiler",
  "enrichplot",
  "org.Hs.eg.db",
  "org.Mm.eg.db",
  "GEOquery"
))
```

### 3. Dépendances optionnelles

```r
# Onglet Venn bulk (2–4 contrastes) — requis uniquement si VennDiagram souhaité
# L'UpSet (ComplexHeatmap) fonctionne sans cette dépendance
install.packages(c("VennDiagram", "futile.logger"))
```

> **Note** : si `VennDiagram` est installé mais que le diagramme Venn ne s'affiche pas, vérifier que `futile.logger` est bien installé — c'est une dépendance directe de `VennDiagram` qui peut manquer silencieusement.

### 4. Lancer l'application

```r
source("global.R")
shiny::runApp()
```

Ou ouvrez `app.R` dans RStudio puis cliquez sur **Run App**.

## Workflow recommandé

### Single-Cell

1. Importer un objet ou des matrices compatibles.
2. Lancer le pipeline de base : QC, normalisation, réduction de dimension, clustering.
3. Annoter les types cellulaires.
4. Explorer les marqueurs et visualisations.
5. Ajouter, si nécessaire, les modules avancés : enrichissement, corrélation, trajectoire.

### Bulk RNA-seq

1. Importer une matrice de **counts bruts** et les métadonnées.
2. Vérifier l'appariement échantillons / metadata.
3. Appliquer le filtrage et la transformation VST.
4. Définir le design expérimental et lancer l'analyse différentielle.
5. Explorer les résultats via PCA, volcano, heatmap, tables et enrichissement.
6. Comparer les contrastes via l'onglet **Venn / UpSet** si plusieurs contrastes ont été calculés.
7. Exporter le rapport HTML/PDF multi-contrastes.

### GEO Bulk — import hors-ligne

1. Télécharger le fichier `GSExxxxxx_series_matrix.txt` depuis la page GEO du jeu de données.
2. Dans l'onglet Import Bulk, charger ce fichier dans le slot "Fichier de Métadonnées".
3. `parse_geo_series_matrix()` détecte automatiquement le format et extrait les caractéristiques échantillons.
4. Charger ensuite la matrice de counts bruts normalement.
5. Utiliser le module Bulk comme pour un import local standard.

## Jeu de test recommandé

Pour tester rapidement le module Bulk RNA-seq, un jeu GEO simple et pédagogique comme **GSE52778** (Himes et al.) est une bonne option : 23 532 gènes × 16 échantillons, matrice de counts bruts disponible sur NCBI GEO, métadonnées extractibles via `parse_geo_series_matrix()`.

En pratique, il est recommandé de partir d'une **matrice de counts bruts** et d'un **fichier de métadonnées**, plutôt que de matrices déjà normalisées de type FPKM/TPM.

## Feuille de route

- [x] Garde-fous design expérimental (n=1 post-NA, covariable single-level) — *v1.1*
- [x] Comparaison multi-contrastes : onglet Venn / UpSet avec table des intersections et export CSV/PNG — *v1.1*
- [x] Rapport multi-contrastes avec tableau récapitulatif et UpSet intégré — *v1.1*
- [x] Import GEO hors-ligne via `parse_geo_series_matrix()` — *v1.1*
- [x] Refactoring `global.R` → `helpers_io/bulk/sc/pathway.R` — *v1.1*
- [ ] Import multi-fichiers per-sample (un fichier par échantillon, merge interne).
- [ ] Export reproductible d'un script R dérivé des actions réalisées dans l'interface.
- [ ] Internationalisation FR / EN.
- [ ] Renforcement du module spatial.
- [ ] Module GEO en ligne (`mod_geo.R` — import direct via accession GEO).

## Contribution

Les contributions sont les bienvenues.

1. Forker le dépôt.
2. Créer une branche dédiée :
   ```bash
   git checkout -b feature/ma-fonctionnalite
   ```
3. Commiter les modifications :
   ```bash
   git commit -m "Ajout d'une nouvelle fonctionnalité"
   ```
4. Pousser la branche :
   ```bash
   git push origin feature/ma-fonctionnalite
   ```
5. Ouvrir une **Pull Request**.

## Licence

Ce projet est distribué sous licence **MIT**.

Voir le fichier [`LICENSE`](LICENSE) pour le texte complet.

---

# English

## Overview

TranscriptoShiny is designed for iterative transcriptomic data exploration by biologists and bioinformaticians, with a modular architecture focused on reuse and maintainability.

The application relies on:

- a clear separation between **import**, **preprocessing**, **analysis**, and **visualization**;
- a shared application state through `reactiveValues` storing `sc_obj`, `bulk_obj`, and `spatial_obj`;
- a **local-first** approach suitable for workstation-based use in R/RStudio;
- an interface designed to guide users through progressive controls and interactive outputs.

## What's new in v1.1 (Step-2)

- **`global.R` refactoring**: the monolithic file (>3000 lines) is now split into 5 files (`global.R` + 4 `helpers_*.R`) for better maintainability.
- **Offline GEO import**: `parse_geo_series_matrix()` — direct parsing of a `*_series_matrix.txt` GEO file without `GEOquery`, no network access required.
- **Venn / UpSet multi-contrast tab**: visual comparison of DEG gene sets across contrasts, with intersection table and CSV/PNG export.
- **Enriched multi-contrast report**: summary table for all contrasts + integrated UpSet diagram in the exported HTML/PDF report.
- **Experimental design safeguards**: hard-block on n=1 after covariate NA and on single-level covariates — covers both single-pair AND pairwise-auto modes.
- **Test infrastructure**: `shiny::testServer()` scripts in `tests/manual/` for browser-free validation.

## What's new in v1.0

- **Architectural refactor**: the `modules/` directory is now organized by functional domains (`import/`, `sc/`, `bulk/`, `spatial/`) for easier maintenance and future extension.
- **Stronger Bulk RNA-seq support**: more robust raw count import, better count/metadata alignment, and cleaner differential expression workflows.
- **Improved Single-Cell UI**: clearer panel organization, better navigation, and more space dedicated to graphics.
- **More modular v1.0 base**: better separation of responsibilities across import, pipeline, visualization, annotation, and advanced analysis modules.

## Core features

### Single-Cell RNA-seq

- **Import**: 10X directories, `.rds`, `.h5`, `.h5ad`, `.loom`, including multi-sample scenarios with `orig.ident` preservation.
- **Standard pipeline**: quality control, normalization, PCA, dimensionality reduction, clustering, UMAP/t-SNE.
- **Batch correction**: support for methods such as **Harmony** depending on the experimental setting.
- **Cell annotation**: automated annotation through **SingleR**.
- **Marker analysis**: detection with `FindAllMarkers` and interactive result exploration.
- **Visualization**: dedicated plots for embeddings, expression, distributions, correlations, and advanced exploratory analysis.
- **Advanced modules**: pathway enrichment, correlation networks, trajectories, and reusable specialized tools.

### Bulk RNA-seq

- **Smart import**: count matrix loading with duplicate gene handling (sum merge), metadata matching.
- **Offline GEO import**: direct parsing of a `*_series_matrix.txt` file via `parse_geo_series_matrix()` — no network access, no `GEOquery` dependency.
- **Differential analysis**: workflows based on **DESeq2**, **edgeR**, and **limma**, with experimental design safeguards (n=1, single-level covariate).
- **Filtering and transformation**: pre-filtering of lowly expressed genes and VST transformation for exploratory analysis.
- **Visualizations**: PCA, sample QC, volcano plot, MA-plot, heatmap, and result tables.
- **Venn / UpSet multi-contrast**: comparison of DEG gene sets across contrasts, exportable intersection table (CSV), PNG export.
- **Functional enrichment**: ORA/GSEA depending on the available result set.
- **HTML/PDF report**: full export with all contrasts, n_sig/n_up/n_down summary table, and UpSet diagram if ≥ 2 contrasts.

### Spatial transcriptomics

- **Import**: 10X Visium-type data.
- **Spatial exploration**: object structuring and visualization for downstream spatial analysis.
- **Ongoing development**: several components are still evolving according to the roadmap.

## Project architecture

The application is built around a modular Shiny core. The entry point initializes global resources and then delegates interface and server logic to specialized modules.

```text
ShinyApp---TranscriptoShiny/
├── app.R                   # Application entry point
├── global.R                # Packages and global options only (~100 lines)
├── helpers_io.R            # Multi-format I/O, ID mapping, parse_geo_series_matrix()
├── helpers_bulk.R          # DESeq2/edgeR/limma, bulk plots, Venn/UpSet, validate_bulk_design()
├── helpers_sc.R            # Seurat: scatter/violin/correlation/trajectory
├── helpers_pathway.R       # ORA/GSEA shared across sc/bulk
├── README.md
├── tests/
│   └── manual/             # shiny::testServer() scripts — browser-free validation
│       ├── test_mod_bulk_de.R
│       └── test_mod_import_bulk.R
├── modules/
│   ├── import/             # Single-cell, bulk, spatial, GEO import helpers
│   ├── sc/                 # scRNA-seq pipeline, annotation, visualization, markers
│   ├── bulk/               # Bulk import, DE, Venn/UpSet, pathways, reporting
│   └── spatial/            # Spatial transcriptomics modules
└── www/                    # CSS, JS, and static assets
```

### Organization principles

- **`app.R`** assembles the global interface, loads modules, and sources the 4 `helpers_*.R` files.
- **`global.R`** centralizes dependencies and options only (~100 lines).
- **`helpers_*.R`** contain all utility functions, separated by domain.
- **`modules/`** stores domain-specific Shiny modules.
- **`tests/manual/`** contains validation scripts using `shiny::testServer()`.

## Requirements

### Recommended environment

- **R** ≥ 4.2
- **RStudio** recommended for development and debugging
- **RAM**:
  - **16 GB minimum recommended** for comfortable use;
  - **32 GB** advised for larger single-cell datasets;
  - Bulk RNA-seq is usually lighter, but some enrichment analyses and visualizations may still become memory intensive.

### Practical notes

- On Windows, restarting R may be necessary after some Bioconductor package updates.
- For large single-cell datasets, an explicit memory strategy is recommended.
- The application is designed for local execution, with full control over dependencies and input files.

## Installation

### 1. Clone the repository

```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Install the main dependencies

Run the following in R or RStudio:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "plotly", "DT", "future",
  "ggplot2", "dplyr", "patchwork", "viridis", "igraph",
  "data.table", "harmony"
))

if (!require("BiocManager")) install.packages("BiocManager")

BiocManager::install(c(
  "Seurat",
  "SingleR",
  "celldex",
  "SingleCellExperiment",
  "DESeq2",
  "edgeR",
  "limma",
  "ComplexHeatmap",
  "ReactomePA",
  "KEGGREST",
  "clusterProfiler",
  "enrichplot",
  "org.Hs.eg.db",
  "org.Mm.eg.db",
  "GEOquery"
))
```

### 3. Optional dependencies

```r
# Venn diagram in bulk tab (2–4 contrasts) — optional
# UpSet (ComplexHeatmap) works without this dependency
install.packages(c("VennDiagram", "futile.logger"))
```

> **Note**: if `VennDiagram` is installed but the Venn diagram does not render, check that `futile.logger` is installed — it is a direct dependency that may be silently missing.

### 4. Launch the application

```r
source("global.R")
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

## Recommended workflow

### Single-Cell

1. Import a compatible object or matrix set.
2. Run the standard pipeline: QC, normalization, dimensionality reduction, clustering.
3. Annotate cell types.
4. Explore markers and visualizations.
5. Add advanced modules when needed: enrichment, correlation, trajectory.

### Bulk RNA-seq

1. Import a **raw count matrix** and metadata.
2. Check sample / metadata alignment.
3. Apply filtering and VST transformation.
4. Define the experimental design and run differential expression.
5. Explore PCA, volcano, heatmap, tables, and enrichment results.
6. Compare contrasts via the **Venn / UpSet** tab if multiple contrasts were computed.
7. Export the multi-contrast HTML/PDF report.

### GEO Bulk — offline import

1. Download the `GSExxxxxx_series_matrix.txt` file from the GEO dataset page.
2. In the Bulk Import tab, load this file in the "Metadata File" slot.
3. `parse_geo_series_matrix()` automatically detects the format and extracts sample characteristics.
4. Load the raw count matrix normally.
5. Use the Bulk module exactly as with a local dataset.

## Recommended test dataset

To quickly test the Bulk RNA-seq module, a simple and pedagogical GEO dataset such as **GSE52778** (Himes et al.) is a good starting point: 23,532 genes × 16 samples, raw count matrix available on NCBI GEO, metadata extractable via `parse_geo_series_matrix()`.

In practice, it is recommended to start from a **raw count matrix** and a **metadata file**, rather than already normalized matrices such as FPKM or TPM.

## Roadmap

- [x] Experimental design safeguards (n=1 post-NA, single-level covariate) — *v1.1*
- [x] Multi-contrast comparison: Venn / UpSet tab with intersection table and CSV/PNG export — *v1.1*
- [x] Multi-contrast report with summary table and integrated UpSet diagram — *v1.1*
- [x] Offline GEO import via `parse_geo_series_matrix()` — *v1.1*
- [x] `global.R` refactoring → `helpers_io/bulk/sc/pathway.R` — *v1.1*
- [ ] Multi-file per-sample import (one file per sample, internal merge).
- [ ] Reproducible R script export derived from UI actions.
- [ ] Native FR / EN internationalization.
- [ ] Further reinforcement of the spatial module.
- [ ] Online GEO module (`mod_geo.R` — direct import via GEO accession).

## Contributing

Contributions are welcome.

1. Fork the repository.
2. Create a dedicated branch:
   ```bash
   git checkout -b feature/my-feature
   ```
3. Commit your changes:
   ```bash
   git commit -m "Add a new feature"
   ```
4. Push the branch:
   ```bash
   git push origin feature/my-feature
   ```
5. Open a **Pull Request**.

## License

This project is distributed under the **MIT** license.

See [`LICENSE`](LICENSE) for the full text.
