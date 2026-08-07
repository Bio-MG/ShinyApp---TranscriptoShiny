# TranscriptoShiny

Une plateforme R/Shiny modulaire, **local-first**, dédiée à l’exploration et à l’analyse guidées de données de bulk RNA-seq, de single-cell RNA-seq et de transcriptomique spatiale.

[![R](https://img.shields.io/badge/language-R-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Langues :** [Français](#français) · [English](#english)

---

# Français

## Pourquoi TranscriptoShiny ?

TranscriptoShiny est conçu pour l’exploration itérative de données transcriptomiques par des biologistes et des bioinformaticiens. L’application combine des interfaces guidées, des messages de validation proactifs, des visualisations interactives et des rapports reproductibles, tout en maintenant un contrôle strict des données locales par l’utilisateur.

L’application privilégie :

- **Une exécution local-first** : les données restent sur votre station de travail, afin de préserver confidentialité et contrôle.
- **Un accompagnement progressif** : des workflows intuitifs pour les non-experts, sans masquer les paramètres analytiques critiques aux utilisateurs avancés.
- **La scalabilité** : des stratégies conscientes de la mémoire, par exemple le sketching de Seurat v5 et les matrices sur disque BPCells, pour traiter de grands jeux single-cell et spatiaux sur du matériel standard.
- **La rigueur scientifique** : des garde-fous intégrés pour le design expérimental en expression différentielle bulk, ainsi que des étapes analytiques transparentes et exportables.

*Note sur la langue : l’interface principale est actuellement en français pour le moment. Une localisation anglaise est prévue dans une prochaine version.*

## Workflows pris en charge

TranscriptoShiny fournit des environnements modulaires dédiés à trois domaines transcriptomiques principaux :

1. **Single-cell RNA-seq** : de l’import de données 10x ou de matrices jusqu’au clustering, à l’annotation, à la recherche de marqueurs et à l’analyse de voies.
2. **Bulk RNA-seq** : des matrices de comptages bruts et métadonnées jusqu’à l’expression différentielle, à la comparaison multi-contrastes et à l’enrichissement fonctionnel.
3. **Transcriptomique spatiale** : des imports Visium/Xenium/CosMx/Slide-seq jusqu’au clustering spatial, à la déconvolution, à l’intégration multi-échantillons et à l’analyse de niches.

## Capacités principales

### Single-cell RNA-seq

- **Import flexible** : prise en charge des dossiers 10x, ainsi que des formats `.rds`, `.h5`, `.h5ad` et `.loom`, avec conservation de `orig.ident` dans les workflows multi-échantillons.
- **Mapping des identifiants géniques** : conversion optionnelle et robuste des identifiants Ensembl/Entrez en symboles géniques avant l’analyse.
- **Pipeline standard** : contrôle qualité, normalisation, sélection de gènes hautement variables, PCA, construction du graphe de voisins, clustering, UMAP et t-SNE optionnel.
- **Correction de batch** : intégration avec Harmony lorsque plusieurs échantillons ou batchs sont présents.
- **Scalabilité consciente de la mémoire** : workflows de sketch Seurat v5 (`SketchData` avec LeverageScore, `ProjectData`), gestion compatible BPCells, mise à l’échelle ciblée des variables et sous-échantillonnage stratifié pour les explorations coûteuses.
- **Annotation et exploration** : annotation automatique des types cellulaires via SingleR (références celldex), recherche de marqueurs (`FindAllMarkers`), corrélation génique, analyse de voies et visualisations variées (embeddings, FeaturePlots, violons, DotPlots, heatmaps, vues ridge/empilées).
- **Reproductibilité** : rapports HTML/PDF paramétrés et export de scripts R réutilisables.
- *Note scientifique* : le module de trajectoire inclus fournit un **pseudotemps exploratoire basé sur un graphe**. Il ne remplace pas les outils dédiés d’inférence de lignage (par exemple Slingshot) ; la détection native de doublets et la régression du cycle cellulaire ne sont pas encore intégrées.

### Bulk RNA-seq

- **Import intelligent** : prise en charge de matrices de comptages bruts fusionnées ou de fichiers de comptages par échantillon, avec alignement automatique des métadonnées et résolution des doublons de gènes.
- **Prise en charge de GEO** : parsing hors-ligne de fichiers GEO `series_matrix.txt`, sans accès réseau ni dépendance à `GEOquery`.
- **QC exploratoire** : filtrage, transformation stabilisant la variance (VST), PCA, scree plots et heatmaps de corrélation entre échantillons.
- **Expression différentielle** : workflows propulsés par DESeq2, edgeR et limma-voom.
- **Garde-fous de design** : contrôles proactifs des covariables confondues, des valeurs manquantes invalidant le modèle et des covariables ne présentant qu’un seul niveau observé.
- **Gestion des contrastes** : contrastes standards, définis par l’utilisateur et pairwise, avec comparaison multi-méthodes et exploration par consensus de rangs.
- **Visualisation et enrichissement** : volcano plots, MA plots, heatmaps, comparaisons Venn/UpSet multi-contrastes et analyse de voies ORA/GSEA.
- *Note scientifique* : les résultats d’analyse de voies doivent être interprétés au regard de l’univers de fond choisi et du mapping des identifiants. Le consensus de rangs multi-méthodes est une aide exploratoire, pas une méta-analyse formelle.

### Transcriptomique spatiale

- **Import étendu** : Visium, Visium HD (layouts pris en charge), Xenium, CosMx et Slide-seq, sous réserve de compatibilité avec les layouts de fichiers standards.
- **Architecture sur disque** : les grands jeux spatiaux utilisent BPCells pour conserver les matrices de comptages sur disque, tandis que des représentations légères de type sketch/métadonnées permettent une analyse interactive en RAM.
- **Exécution asynchrone** : les opérations lourdes passent par un pool de démons `mirai`, avec vérifications d’état, journaux de tâches, délais d’expiration et cache par jeu de données afin d’éviter le blocage de l’interface.
- **Analyse spatiale** : QC spatial, analyse de gènes spatialement variables de type Moran et approche légère de clustering spatial tenant compte du voisinage (inspirée de BANKSY).
- **Déconvolution** : RCTD, transfert de labels depuis une référence et approches de type LDA, avec préparation de référence et garde-fous de validation.
- **Multi-échantillons et niches** : intégration par sketch respectueuse de la mémoire, avec correction de batch Harmony optionnelle, et analyse de niches fondée sur la composition des voisinages locaux.
- **Visualisations avancées** : superpositions histologiques, vues spatiales/embeddings liées, sélection ROI au lasso, exploration de marqueurs de ROI et export de sous-ensembles.
- *Note scientifique* : la précision de la déconvolution dépend fortement de la qualité de la référence, de la compatibilité entre plateformes et du contexte tissulaire. Ces méthodes sont destinées à l’exploration scientifique et ne sont pas validées pour la décision clinique.

## Architecture

TranscriptoShiny repose sur une architecture Shiny modulaire, séparant les responsabilités afin d’améliorer la maintenabilité et la testabilité.

- `app.R` : point d’entrée principal, assemblant l’interface et la logique serveur.
- `global.R` : initialise les packages et les options d’exécution globales.
- `helpers_*.R` : fonctions utilitaires par domaine (I/O, single-cell, bulk, voies), sans réactivité Shiny, donc plus faciles à tester et à réutiliser.
- `modules/` : modules Shiny organisés par domaine (`import/`, `sc/`, `bulk/`, `spatial/`).
- `R/` : utilitaires spécifiques au spatial, implémentant les I/O sur disque, l’exécution asynchrone, la préparation des références, l’intégration multi-échantillons et les niches.
- **Gestion d’état** : les modules de domaine communiquent via des objets `reactiveValues` partagés. Les rapports sont générés à partir de modèles R Markdown paramétrés.

```text
TranscriptoShiny/
├── app.R                   # Point d’entrée de l’application
├── global.R                # Chargement des packages et options globales
├── helpers_bulk.R          # Moteurs DE bulk, graphiques et validation
├── helpers_io.R            # I/O multi-format et mapping d’identifiants
├── helpers_pathway.R       # Utilitaires ORA/GSEA
├── helpers_sc.R            # Helpers Seurat de visualisation et d’analyse
├── helpers_sc_bpcells.R    # Support de pipeline sur disque (BPCells)
├── R/                      # Utilitaires spatiaux (async, I/O, multi-échantillons, niches)
├── modules/
│   ├── import/             # Modules d’import (sc, bulk, spatial, GEO)
│   ├── sc/                 # Modules d’analyse single-cell
│   ├── bulk/               # Modules d’analyse bulk RNA-seq
│   └── spatial/            # Modules de transcriptomique spatiale
└── www/                    # Ressources statiques (CSS, JS)
```

## Installation

### 1. Cloner le dépôt

```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Installer les dépendances

Ouvrez R ou RStudio dans le répertoire du projet cloné. Installez les packages CRAN et Bioconductor nécessaires. L’ensemble exact des dépendances varie selon les workflows utilisés, en particulier pour la déconvolution spatiale et le rendu des rapports.

```r
# Installer BiocManager s’il n’est pas déjà présent
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Installer les dépendances principales (ajoutez celles requises par vos workflows)
install.packages(c("shiny", "bslib", "bsicons", "plotly", "DT", "future",
                   "ggplot2", "dplyr", "patchwork", "viridis", "igraph"))

# Installer les dépendances Bioconductor
BiocManager::install(c("Seurat", "SingleR", "celldex", "SingleCellExperiment",
                       "DESeq2", "edgeR", "limma", "ComplexHeatmap",
                       "clusterProfiler", "org.Hs.eg.db", "org.Mm.eg.db"))
```

*Note : les workflows spatiaux peuvent exiger des packages supplémentaires tels que `BPCells`, `mirai`, `RANN` et `spacexr`. Consultez les messages de démarrage de l’application ou les en-têtes des modules pour connaître les dépendances précises.*

### 3. Lancer l’application

```r
shiny::runApp()
```

Vous pouvez aussi ouvrir `app.R` dans RStudio puis cliquer sur **Run App**.

### Environnement recommandé

| Composant | Recommandation |
| :--- | :--- |
| **Version de R** | >= 4.2 |
| **IDE** | RStudio (recommandé pour le développement et le débogage) |
| **RAM (usage courant)** | 16 Go minimum |
| **RAM (grands jeux de données)** | 32 Go recommandés pour les grands jeux single-cell ou spatiaux |
| **Calcul** | L’exécution CPU-only est entièrement prise en charge ; aucun GPU n’est requis |
| **Stockage** | Un espace disque local rapide et suffisant est essentiel pour les fichiers temporaires, les rapports et les données spatiales BPCells |

## Workflows typiques

### Single-cell RNA-seq

1. Importez des données compatibles (dossiers 10x, `.rds`, `.h5`, `.h5ad` ou `.loom`).
2. Mappez facultativement les identifiants géniques vers des symboles standards.
3. Lancez le pipeline guidé : QC, normalisation, PCA, clustering et UMAP/t-SNE.
4. Annotez les types cellulaires avec SingleR.
5. Explorez les marqueurs, les corrélations géniques et les enrichissements de voies.
6. Exportez un rapport paramétré ou un script R reproductible.

### Bulk RNA-seq

1. Importez des matrices de comptages bruts de type entier et les métadonnées correspondantes. *(N’utilisez pas de valeurs TPM/FPKM pré-normalisées pour l’expression différentielle.)*
2. Mappez facultativement les identifiants géniques.
3. Appliquez filtrage et VST, puis examinez la PCA et le QC de corrélation entre échantillons.
4. Définissez le design expérimental, examinez les alertes relatives aux covariables et spécifiez les contrastes.
5. Lancez l’expression différentielle (DESeq2, edgeR ou limma-voom).
6. Explorez les résultats avec volcano/MA plots, heatmaps et comparaisons Venn/UpSet multi-contrastes.
7. Réalisez l’analyse de voies et exportez le rapport HTML/PDF multi-contrastes ou le script R.

### Transcriptomique spatiale

1. Importez des données spatiales (Visium, Visium HD, Xenium, CosMx ou Slide-seq).
2. Examinez les métriques de QC spatial et appliquez les filtres de spots/cellules.
3. Lancez le clustering spatial tenant compte du voisinage et/ou l’analyse de gènes spatialement variables de type Moran.
4. Préparez et validez facultativement un jeu de données single-cell de référence.
5. Lancez la déconvolution (RCTD, transfert de labels ou LDA).
6. Visualisez les résultats avec superpositions histologiques, vues liées et sélection de ROI au lasso.
7. Réalisez facultativement une intégration multi-échantillons par sketch ou une analyse de composition des niches.

## Travailler avec de grands jeux de données

TranscriptoShiny intègre des garde-fous spécifiques pour gérer les limites de mémoire et de calcul sur des stations de travail :

- **Utilisez les workflows de sketch** : pour les grands jeux single-cell ou spatiaux, activez les options de sketch Seurat v5 ou l’intégration spatiale par sketch afin d’éviter le chargement des matrices complètes en RAM.
- **Respectez les limites du stockage sur disque** : ne forcez pas les matrices de comptages spatiales complètes en mémoire ; utilisez les tâches asynchrones fournies et adossées à BPCells.
- **Exploitez les aperçus** : utilisez les visualisations d’aperçu/sous-échantillonnées proposées par l’interface et réservez les exports pleine fidélité au rapport final.
- **Gérez le stockage temporaire** : assurez-vous que le `tempdir()` de R ou l’emplacement de cache configuré dispose d’un espace libre suffisant, en particulier pour les imports volumineux `.h5` ou `.h5ad` et les artefacts BPCells.
- **Échelonnez les opérations lourdes** : sur une station avec 32 Go de RAM, exécutez une seule analyse lourde à la fois, par exemple une déconvolution spatiale ou une recherche de marqueurs à grande échelle.
- **Utilisez un stockage local rapide** : conservez les données brutes, les rapports générés et les répertoires BPCells sur des disques locaux rapides, par exemple NVMe SSD, plutôt que sur des volumes réseau.

## Reproductibilité et utilisation scientifique

TranscriptoShiny favorise la recherche reproductible grâce à l’export de rapports HTML/PDF paramétrés et de scripts R spécifiques à chaque domaine, qui récapitulent les étapes analytiques effectuées dans l’interface.

La reproductibilité et la validité scientifique dépendent également :

- Des fichiers d’entrée exacts et de leur formatage.
- Des versions précises de R, Bioconductor et des packages dépendants.
- De la version des bases d’annotation, par exemple `org.Hs.eg.db`, utilisée lors de l’analyse.
- Des paramètres explicitement choisis par l’utilisateur.

**Bonnes pratiques :**

- Enregistrez votre `sessionInfo()` lors du partage ou de la publication de résultats.
- Envisagez un fichier de verrouillage des dépendances au niveau du projet, par exemple `renv`, lors du déploiement ou du partage d’un environnement d’analyse.
- Considérez l’application comme une aide analytique. Tous les résultats nécessitent une revue biologique et statistique indépendante ; TranscriptoShiny ne remplace ni un design d’étude rigoureux, ni le jugement en contrôle qualité, ni l’expertise du domaine.

## Feuille de route

Les développements à venir visent à étendre l’accessibilité, la robustesse et la profondeur analytique :

- [ ] Localisation de l’interface en anglais et support de l’internationalisation.
- [ ] Choix élargis d’intégration single-cell au-delà de Harmony.
- [ ] Détection dédiée des doublets au sein du pipeline de QC.
- [ ] Intégration d’outils dédiés à l’inférence de lignage et de trajectoire.
- [ ] Tests automatisés et outils de reproductibilité plus étendus.
- [ ] Extension continue des workflows spatiaux et des analyses fondées sur des références.

## Contribuer

Les contributions sont les bienvenues. Si vous souhaitez contribuer :

1. Ouvrez d’abord une issue afin de discuter de la motivation biologique ou technique du changement.
2. Pour les rapports de bug, fournissez un exemple minimal reproductible, votre système d’exploitation, votre version de R et le message d’erreur complet.
3. Évitez d’ajouter de grands fichiers de données à Git ; utilisez des données synthétiques ou fortement sous-échantillonnées pour les tests.
4. Soumettez une pull request avec des commits clairs et ciblés.

## Obtenir de l’aide

En cas de problème, ouvrez une GitHub Issue en incluant :

- Votre système d’exploitation et votre version de R.
- Les versions des packages clés, par exemple Seurat, DESeq2 et BPCells.
- Le format des données d’entrée et un exemple minimal reproductible, si le partage des données le permet.
- Le message d’erreur complet de la console ou une capture d’écran de l’erreur dans l’interface.


## Licence

Ce projet est distribué sous licence MIT. Consultez le fichier `LICENSE` du dépôt pour le texte complet.

---

# English

## Why TranscriptoShiny?

TranscriptoShiny is designed for iterative transcriptomic data exploration by biologists and bioinformaticians. It combines guided user interfaces, proactive validation messages, interactive visualizations, and reproducible reporting, all while maintaining strict user control over local data.

The application prioritizes:
- **Local-first execution**: Data remains on your workstation, ensuring privacy and control.
- **Progressive guidance**: Intuitive workflows for non-experts, without hiding critical analytical parameters from advanced users.
- **Scalability**: Memory-aware strategies (e.g., Seurat v5 sketching, BPCells disk-backed matrices) to handle large single-cell and spatial datasets on standard hardware.
- **Scientific rigor**: Built-in experimental-design safeguards for bulk differential expression and transparent, exportable analytical steps.

*Note on language*: The primary interface language is currently French for now,  English translation is actively planned for a future release.

## Supported workflows

TranscriptoShiny provides dedicated, modular environments for three primary transcriptomic domains:
1. **Single-cell RNA-seq**: From raw 10x or matrix imports to clustering, annotation, marker discovery, and pathway analysis.
2. **Bulk RNA-seq**: From raw count matrices and metadata to differential expression, multi-contrast comparison, and functional enrichment.
3. **Spatial transcriptomics**: From Visium/Xenium/CosMx/Slide-seq imports to spatial clustering, deconvolution, multi-sample integration, and niche analysis.

## Key capabilities

### Single-cell RNA-seq
- **Flexible import**: Supports 10x directories, `.rds`, `.h5`, `.h5ad`, and `.loom` formats, preserving `orig.ident` for multi-sample workflows.
- **Gene identifier mapping**: Optional, robust conversion of Ensembl/Entrez IDs to gene symbols prior to analysis.
- **Standard pipeline**: QC, normalization, highly variable feature selection, PCA, neighbor graph construction, clustering, UMAP, and optional t-SNE.
- **Batch correction**: Harmony-based integration when multiple samples or batches are present.
- **Memory-aware scaling**: Seurat v5 sketch workflows (`SketchData` with LeverageScore, `ProjectData`), BPCells-aware handling, targeted feature scaling, and stratified subsampling for costly exploratory tasks.
- **Annotation & exploration**: Automatic cell-type annotation via SingleR (celldex references), marker discovery (`FindAllMarkers`), gene correlation, pathway analysis, and diverse visualizations (embeddings, feature plots, violins, dot plots, heatmaps, ridge/stacked views).
- **Reproducibility**: Parameterized HTML/PDF reports and reusable R-script export.
- *Scientific note*: The included trajectory module provides **exploratory graph-based pseudotime**. It is not a replacement for dedicated lineage-inference tools (e.g., Slingshot), and built-in doublet detection or cell-cycle regression is not currently provided.

### Bulk RNA-seq
- **Smart import**: Handles merged raw count matrices or per-sample count files, with automated metadata alignment and gene duplicate resolution.
- **GEO support**: Offline parsing of GEO `series_matrix.txt` files without requiring network access or `GEOquery`.
- **Exploratory QC**: Filtering, variance-stabilizing transformation (VST), PCA, scree plots, and sample-correlation heatmaps.
- **Differential expression**: Workflows powered by DESeq2, edgeR, and limma-voom.
- **Design safeguards**: Proactive checks for confounding covariates, missing covariate values that invalidate the model, and covariates with only one observed level.
- **Contrast management**: Standard, user-defined, and pairwise contrasts, alongside multi-method comparison and rank-consensus exploration.
- **Visualization & enrichment**: Volcano plots, MA plots, heatmaps, multi-contrast Venn/UpSet comparisons, and ORA/GSEA pathway analysis.
- *Scientific note*: Pathway analysis results should be interpreted in the context of the chosen background universe and identifier mapping. Multi-method rank consensus is an exploratory aid, not a formal meta-analysis.

### Spatial transcriptomics
- **Broad import support**: Visium, Visium HD (supported layouts), Xenium, CosMx, and Slide-seq, subject to standard file-layout compatibility.
- **Disk-backed architecture**: Large spatial assays utilize BPCells to store count matrices on disk, while lightweight sketch/metadata representations support interactive analysis in RAM.
- **Asynchronous execution**: Heavy operations run via a `mirai` daemon pool with health checks, task logs, timeouts, and per-dataset caching to prevent UI blocking.
- **Spatial analysis**: Spatial QC, Moran-style spatially variable feature analysis, and a lightweight, neighborhood-aware spatial clustering approach (BANKSY-inspired).
- **Deconvolution**: RCTD, reference label transfer, and LDA-style approaches, featuring a prepared-reference workflow with validation safeguards.
- **Multi-sample & niches**: Memory-aware sketch integration (with optional Harmony batch correction) and niche analysis based on local neighborhood composition.
- **Advanced visualization**: Histology overlays, linked spatial/embedding views, lasso ROI selection, ROI marker exploration, and subset export.
- *Scientific note*: Deconvolution accuracy depends heavily on reference quality, platform compatibility, and tissue context. These methods are for research exploration and are not validated for clinical decision-making.

## Architecture

TranscriptoShiny is built on a modular Shiny architecture, separating concerns to improve maintainability and testability.

- `app.R`: The main application entry point, assembling the UI and server logic.
- `global.R`: Initializes packages and global runtime options.
- `helpers_*.R`: Domain-specific utility functions (I/O, single-cell, bulk, pathway) that contain no Shiny reactivity, making them easier to test and reuse.
- `modules/`: Shiny modules organized by domain (`import/`, `sc/`, `bulk/`, `spatial/`).
- `R/`: Spatial-specific utilities implementing disk-backed I/O, asynchronous execution, reference preparation, multi-sample integration, and niche analysis.
- **State management**: Domain modules communicate via shared `reactiveValues` objects. Reports are generated from parameterized R Markdown templates.

```text
TranscriptoShiny/
├── app.R                   # Application entry point
├── global.R                # Package loading and global options
├── helpers_bulk.R          # Bulk DE engines, plots, and validation
├── helpers_io.R            # Multi-format I/O and ID mapping
├── helpers_pathway.R       # ORA/GSEA utilities
├── helpers_sc.R            # Seurat plotting and analysis helpers
├── helpers_sc_bpcells.R    # Disk-backed (BPCells) pipeline support
├── R/                      # Spatial utilities (async, I/O, multi-sample, niches)
├── modules/
│   ├── import/             # Data import modules (sc, bulk, spatial, GEO)
│   ├── sc/                 # Single-cell analysis modules
│   ├── bulk/               # Bulk RNA-seq analysis modules
│   └── spatial/            # Spatial transcriptomics modules
└── www/                    # Static assets (CSS, JS)
```

## Installation

### 1. Clone the repository
```bash
git clone https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git
cd ShinyApp---TranscriptoShiny
```

### 2. Install dependencies
Open R or RStudio in the cloned project directory. Install the required CRAN and Bioconductor packages. The exact dependency footprint varies depending on the workflows you plan to use (especially spatial deconvolution and report rendering).

```r
# Install BiocManager if not already present
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Install core dependencies (add others as needed for your specific workflows)
install.packages(c("shiny", "bslib", "bsicons", "plotly", "DT", "future", 
                   "ggplot2", "dplyr", "patchwork", "viridis", "igraph"))

# Install Bioconductor dependencies
BiocManager::install(c("Seurat", "SingleR", "celldex", "SingleCellExperiment",
                       "DESeq2", "edgeR", "limma", "ComplexHeatmap", 
                       "clusterProfiler", "org.Hs.eg.db", "org.Mm.eg.db"))
```
*Note: Spatial workflows may require additional packages such as `BPCells`, `mirai`, `RANN`, and `spacexr`. Consult the application startup messages or module headers for specific requirements.*

### 3. Launch the application
```r
shiny::runApp()
```
Alternatively, open `app.R` in RStudio and click **Run App**.

### Recommended environment
| Component | Recommendation |
| :--- | :--- |
| **R Version** | >= 4.2 |
| **IDE** | RStudio (recommended for development and debugging) |
| **RAM (Routine)** | 16 GB minimum |
| **RAM (Large Data)** | 32 GB recommended for large single-cell or spatial datasets |
| **Compute** | CPU-only execution is fully supported; GPU is not required |
| **Storage** | Sufficient fast local disk space is critical for temporary files, reports, and BPCells-backed spatial data |

## Typical workflows

### Single-cell RNA-seq
1. Import compatible data (10x directories, `.rds`, `.h5`, `.h5ad`, or `.loom`).
2. (Optional) Map gene identifiers to standard symbols.
3. Run the guided pipeline: QC, normalization, PCA, clustering, and UMAP/t-SNE.
4. Annotate cell types using SingleR.
5. Explore markers, gene correlations, and pathway enrichments.
6. Export a parameterized report or a reproducible R script.

### Bulk RNA-seq
1. Import raw integer-like count matrices and corresponding metadata. *(Note: Do not use pre-normalized TPM/FPKM values for differential expression).*
2. (Optional) Map gene identifiers.
3. Apply filtering and variance-stabilizing transformation (VST); review PCA and sample-correlation QC.
4. Define the experimental design, checking for covariate warnings, and specify contrasts.
5. Run differential expression (DESeq2, edgeR, or limma-voom).
6. Explore results via volcano/MA plots, heatmaps, and multi-contrast Venn/UpSet comparisons.
7. Perform pathway analysis and export the multi-contrast HTML/PDF report or R script.

### Spatial transcriptomics
1. Import spatial data (Visium, Visium HD, Xenium, CosMx, or Slide-seq).
2. Review spatial QC metrics and apply spot/cell filters.
3. Run spatial clustering (neighborhood-aware) and/or Moran-style spatially variable feature analysis.
4. (Optional) Prepare and validate a single-cell reference dataset.
5. Run deconvolution (RCTD, label transfer, or LDA).
6. Visualize results with histology overlays, linked views, and lasso ROI selection.
7. (Optional) Perform multi-sample sketch integration or niche composition analysis.

## Working with large datasets

TranscriptoShiny includes specific safeguards to manage memory and compute limits on workstation-scale hardware:
- **Use sketch workflows**: For large single-cell or spatial datasets, enable the Seurat v5 sketching options or spatial sketch integration to avoid loading full-resolution matrices into RAM.
- **Respect disk-backed boundaries**: Do not attempt to force full-resolution spatial count matrices into memory; rely on the provided BPCells-backed asynchronous tasks.
- **Leverage previews**: Utilize preview/subsampled visualizations where offered in the UI, reserving full-fidelity exports for final reporting.
- **Manage temporary storage**: Ensure that your R `tempdir()` or configured cache location has adequate free disk space, especially for large `.h5` or `.h5ad` uploads and BPCells artifacts.
- **Pace heavy operations**: On a 32 GB workstation, run one heavy asynchronous analysis (e.g., spatial deconvolution or large-scale marker discovery) at a time.
- **Use fast local storage**: Keep raw input data, generated reports, and BPCells directories on fast local drives (e.g., NVMe SSD) rather than network-mounted volumes.

## Reproducibility and scientific use

TranscriptoShiny supports reproducible research by exporting parameterized HTML/PDF reports and domain-specific R scripts that recapitulate the analytical steps performed in the UI.

However, true reproducibility and scientific validity also depend on:
- The exact input files and their formatting.
- The specific versions of R, Bioconductor, and dependent packages used.
- The version of annotation databases (e.g., `org.Hs.eg.db`) at the time of analysis.
- The parameters explicitly chosen by the user.

**Best practices**:
- Record your `sessionInfo()` when sharing or publishing results.
- Consider using a project-level dependency lockfile (e.g., `renv`) when deploying or sharing an analysis environment.
- Treat the application as an analytical aid. All results require independent biological and statistical review; TranscriptoShiny does not replace rigorous study design, quality control judgment, or domain expertise.

## Roadmap

Future development is focused on expanding accessibility, robustness, and analytical depth:
- [ ] English UI localization and internationalization support.
- [ ] Expanded single-cell integration choices beyond Harmony.
- [ ] Dedicated doublet-detection support within the QC pipeline.
- [ ] Integration of dedicated lineage and trajectory inference tools.
- [ ] Broader automated testing and reproducibility tooling.
- [ ] Continued expansion of spatial workflows and reference-based analyses.

## Contributing

Contributions are welcome. If you wish to contribute:
1. Please open an issue first to discuss the biological or technical motivation for the change.
2. For bug reports, provide a minimal reproducible example, your OS, R version, and the complete error message.
3. Avoid committing large data files to Git; use synthetic or heavily subsampled data for testing.
4. Submit a pull request with clear, focused commits.

## Getting help

If you encounter issues, please open a GitHub Issue and include:
- Your operating system and R version.
- The versions of key packages (e.g., Seurat, DESeq2, BPCells).
- The input data format and a minimal reproducible example (if data sharing permits).
- The complete console error message or a screenshot of the UI error.


## License

This project is distributed under the MIT License. See the `LICENSE` file in the repository for full details.