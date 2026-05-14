# TranscriptoShiny

[License: MIT](https://opensource.org/licenses/MIT)  
[R](https://www.r-project.org/)

**A modular Shiny application for transcriptomics and multi-omics analysis.**  
Supports **single-cell RNA-seq**, **bulk RNA-seq**, and **spatial transcriptomics** (Visium/Xenium) workflows with a focus on **interactivity**, **scalability**, and **reproducibility**.
Language : French 

---

## 📌 **Overview**

TranscriptoShiny is designed for **iterative exploratory analysis** of transcriptomic data, with:

- **Modular architecture**: Separates import, preprocessing, and analysis logic into reusable modules.
- **Shared state**: Uses `reactiveValues` to store `sc_obj`, `bulk_obj`, and `spatial_obj` across modules.
- **Memory optimization**: Configures `future::plan(multisession)` and high `future.globals.maxSize` for heavy workloads.
- **User guidance**: Built-in help modal, memory monitoring, and object status reporting.

---

## ✨ **Key Features**

### 🧬 **Single-Cell Analysis**

- **Import**: Supports 10X directories, `.rds`, `.h5`, `.h5ad`, `.loom` (multi-sample with preserved `orig.ident`).
- **Pipeline**: QC, normalization (LogNorm), PCA, Harmony (batch correction), clustering, UMAP/t-SNE/diffusion maps.
- **Annotation**: SingleR-based cell type annotation.
- **Markers**: `FindAllMarkers` with interactive DataTables.
- **Visualization**: 11 plot types (DimPlot, FeaturePlot, violin, ridge, dot plot, scatter, correlation matrix, volcano, etc.).
- **Advanced**: Correlation networks (igraph), pathway enrichment (GO/KEGG/Reactome), trajectory analysis (Slingshot).

### 📊 **Bulk RNA-seq**  [IN BUILDING]

- **Import**: Count matrices and metadata.
- **Analysis**: DESeq2/edgeR differential expression.

### 🗺️ **Spatial Transcriptomics** [IN BUILDING]

- **Import**: 10X Visium-like data.
- **Analysis**: Spatial clustering and visualization (WIP).

---

## 🗂️ **Project Structure**

```
TranscriptoShiny/
├── app.R                 # Entry point: UI/server, initializes global_data
├── global.R              # Global env: packages, options, helpers
├── R/
│   ├── mod_import_sc.R    # Single-cell import (BPCells/SeuratDisk)
│   ├── mod_bulk.R         # Bulk RNA-seq (DESeq2/edgeR)
│   ├── mod_spatial.R      # Spatial transcriptomics (Visium/Xenium)
│   ├── mod_sc.R           # Parent router: UI layout & module coordinator
│   ├── mod_sc_pipeline.R  # QC, normalization, PCA, Harmony, clustering
│   ├── mod_sc_annotation.R # SingleR annotation
│   ├── mod_sc_viz.R       # 11 visualization types
│   ├── mod_sc_markers.R   # FindAllMarkers + DataTables
│   ├── mod_sc_corr.R      # Correlation networks
│   ├── mod_sc_pathways.R  # GO/KEGG/Reactome enrichment
│   └── mod_sc_trajectory.R # Slingshot trajectory analysis
└── www/                  # CSS, JS, static assets
```

---

## 🛠️ **Requirements**

### **Core Dependencies** (loaded in `global.R`):

- **CRAN**: `shiny`, `plotly`, `DT`, `future`, `igraph`, `harmony`, `Seurat`, `SingleR`, `celldex`, `SingleCellExperiment`.
- **Bioconductor**: `DESeq2`, `edgeR`, `ComplexHeatmap`, `ReactomePA`, `KEGGREST`, `org.Hs.eg.db`, `org.Mm.eg.db`.

### **System Requirements**:

- R ≥ 4.2.
- **Memory**: ≥16GB recommended for large datasets (e.g., >100k cells).
- **Windows users**: Clean R session may be required for Bioconductor DLLs.

---

## 🚀 **Installation & Usage**

### 1. Clone the Repository

```bash
git clone https://github.com/LesNewBro/TranscriptoShiny.git
cd TranscriptoShiny/my_shiny_app
```

### 2. Install Dependencies

Open R/RStudio and run:

```r
# Install CRAN packages
install.packages(c("shiny", "plotly", "DT", "future", "igraph", "harmony"))

# Install Bioconductor packages
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install(c("Seurat", "SingleR", "celldex", "SingleCellExperiment", "DESeq2", "edgeR", "ComplexHeatmap", "ReactomePA", "KEGGREST", "org.Hs.eg.db", "org.Mm.eg.db"))
```

### 3. Launch the App

```r
source("global.R")  # Load packages and settings
source("app.R")     # Run the app
```

Or click **Run App** in RStudio with `app.R` open.

---

## 🔄 **Recommended Workflow**

1. **Import Data**: Use the dedicated tabs for single-cell, bulk, or spatial data.
2. **Preprocess**: Run the single-cell pipeline (QC → normalization → clustering → UMAP).
3. **Analyze**:
  - Annotate cells with SingleR.
  - Detect markers (`FindAllMarkers`).
  - Explore pathways (GO/KEGG/Reactome).
  - Visualize with DimPlot, FeaturePlot, etc.
4. **Export**: Save results from tables/plots (e.g., markers, pathway enrichment).

> **Tip**: For multi-sample single-cell projects, import with preserved `orig.ident` to enable batch-aware processing (e.g., Harmony).

---

## 🤝 **Contributing**

Contributions are welcome! Please:

1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/your-feature`).
3. Commit your changes (`git commit -m "Add your feature"`).
4. Push to the branch (`git push origin feature/your-feature`).
5. Open a Pull Request.

---

## 📜 **License**

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

```text
MIT License

Copyright (c) 2026 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```


