# TranscriptoShiny v1.0

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

## Nouveautés de la v1.0

- **Refonte architecturale** : le dossier `modules/` est désormais organisé par domaines fonctionnels (`import/`, `sc/`, `bulk/`, `spatial/`) pour simplifier la maintenance et l'évolution du code.
- **Bulk RNA-seq renforcé** : import plus robuste des matrices de counts bruts, meilleur alignement counts/métadonnées, et intégration de workflows différentiels plus propres.
- **Intégration GEO** : possibilité de récupérer plus facilement des jeux de données Bulk et leurs métadonnées à partir d'identifiants GEO.
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

- **Import intelligent** : lecture de matrices de counts et appariement des métadonnées.
- **Prise en charge GEO** : préparation facilitée de jeux de données Bulk provenant de GEO.
- **Analyse différentielle** : workflows basés sur **DESeq2** et **edgeR**.
- **Filtrage et transformation** : pré-filtrage des gènes faiblement exprimés et transformation VST pour l'exploration.
- **Visualisations** : PCA, QC échantillons, volcano plot, MA-plot, heatmap et tableaux de résultats.
- **Enrichissement fonctionnel** : ORA/GSEA selon le jeu de résultats disponible.

### Transcriptomique spatiale

- **Import** : données de type 10X Visium.
- **Exploration spatiale** : visualisation et structuration des objets pour analyses spatiales.
- **Développement en cours** : certaines briques restent en évolution selon la feuille de route.

## Architecture du projet

L'application repose sur un cœur Shiny modulaire. Le point d'entrée initialise les ressources globales, puis délègue l'interface et la logique aux modules spécialisés.

```text
ShinyApp---TranscriptoShiny/
├── app.R                   # Point d'entrée de l'application
├── global.R                # Packages, options globales, helpers utilitaires
├── README.md
├── modules/
│   ├── import/             # Import single-cell, bulk, spatial, GEO, helpers d'entrée
│   ├── sc/                 # Pipeline scRNA-seq, annotation, visualisation, marqueurs
│   ├── bulk/               # Import, DE, visualisation, pathway, reporting bulk
│   └── spatial/            # Modules transcriptomique spatiale
└── www/                    # CSS, JS et ressources statiques
```

### Principes d'organisation

- **`app.R`** assemble l'interface globale et les modules.
- **`global.R`** centralise les dépendances, options, fonctions utilitaires et helpers partagés.
- **`modules/`** contient les modules Shiny organisés par domaine fonctionnel.
- **`Tests/`** regroupe les tests de non-régression et validations ciblées.

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

### 3. Lancer l'application

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

### GEO Bulk

1. Identifier un accession GEO de type `GSEXXXXX`.
2. Télécharger ou reconstruire les métadonnées associées.
3. Charger la matrice de counts bruts dans l'application.
4. Utiliser le module Bulk comme pour un import local standard.

## Jeu de test recommandé

Pour tester rapidement le module Bulk RNA-seq, un jeu GEO simple et pédagogique comme **GSE52778** est une bonne option : il permet de récupérer une matrice de counts bruts et des métadonnées exploitables pour valider l'import, le design, la PCA et l'analyse différentielle.

En pratique, il est recommandé de partir d'une **matrice de counts bruts** et d'un **fichier de métadonnées**, plutôt que de matrices déjà normalisées de type FPKM/TPM.

## Feuille de route

- [ ] Garde-fous supplémentaires sur le design expérimental avant DESeq2/edgeR.
- [ ] Comparaison multi-contrastes avec visualisations dédiées (Venn / UpSet).
- [ ] Export reproductible d'un script R dérivé des actions réalisées dans l'interface.
- [ ] Internationalisation FR / EN.
- [ ] Renforcement du module spatial.
- [ ] Amélioration continue des modules Bulk : mapping d'identifiants, pairwise automatique, exports enrichis et reporting.

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

## What's new in v1.0

- **Architectural refactor**: the `modules/` directory is now organized by functional domains (`import/`, `sc/`, `bulk/`, `spatial/`) for easier maintenance and future extension.
- **Stronger Bulk RNA-seq support**: more robust raw count import, better count/metadata alignment, and cleaner differential expression workflows.
- **GEO integration**: easier retrieval and preparation of Bulk datasets and related metadata from GEO accessions.
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

- **Smart import**: count matrix loading and metadata matching.
- **GEO support**: easier preparation of Bulk RNA-seq datasets from GEO.
- **Differential analysis**: workflows based on **DESeq2** and **edgeR**.
- **Filtering and transformation**: pre-filtering of lowly expressed genes and VST transformation for exploratory analysis.
- **Visualizations**: PCA, sample QC, volcano plot, MA-plot, heatmap, and result tables.
- **Functional enrichment**: ORA/GSEA depending on the available result set.

### Spatial transcriptomics

- **Import**: 10X Visium-type data.
- **Spatial exploration**: object structuring and visualization for downstream spatial analysis.
- **Ongoing development**: several components are still evolving according to the roadmap.

## Project architecture

The application is built around a modular Shiny core. The entry point initializes global resources and then delegates interface and server logic to specialized modules.

```text
ShinyApp---TranscriptoShiny/
├── app.R                   # Application entry point
├── global.R                # Packages, global options, shared helpers
├── README.md
├── Tests/                  # Validation scripts and regression tests
├── modules/
│   ├── import/             # Single-cell, bulk, spatial, GEO import helpers
│   ├── sc/                 # scRNA-seq pipeline, annotation, visualization, markers
│   ├── bulk/               # Bulk import, DE, visualization, pathways, reporting
│   └── spatial/            # Spatial transcriptomics modules
└── www/                    # CSS, JS, and static assets
```

### Organization principles

- **`app.R`** assembles the global interface and loads the modules.
- **`global.R`** centralizes dependencies, options, utility functions, and shared helpers.
- **`modules/`** stores domain-specific Shiny modules.
- **`Tests/`** contains non-regression checks and targeted validation scripts.

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

### 3. Launch the application

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

### GEO Bulk

1. Identify a GEO accession such as `GSEXXXXX`.
2. Download or reconstruct the corresponding metadata.
3. Load the raw count matrix into the application.
4. Use the Bulk module exactly as with a local dataset.

## Recommended test dataset

To quickly test the Bulk RNA-seq module, a simple and pedagogical GEO dataset such as **GSE52778** is a good starting point: it provides a raw count matrix and usable metadata to validate import, design handling, PCA, and differential expression.

In practice, it is recommended to start from a **raw count matrix** and a **metadata file**, rather than already normalized matrices such as FPKM or TPM.

## Roadmap

- [ ] Additional safeguards for experimental design before DESeq2/edgeR execution.
- [ ] Multi-contrast comparison with dedicated visualization layers (Venn / UpSet).
- [ ] Reproducible R script export derived from UI actions.
- [ ] Native FR / EN internationalization.
- [ ] Further reinforcement of the spatial module.
- [ ] Continuous Bulk module improvements: ID mapping, automatic pairwise contrasts, richer exports, and reporting.

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
