# GEO Smoke-Test Dataset Manifest — CellChat / scVelo candidates

Probed live 2026-09-13 against the hardened GEO-import module
(`modules/import/mod_geo.R`, commit `c004303`). No large file was downloaded:
structure was verified with HTTP Range requests into the supplementary TARs
(first tar headers, octal sizes) plus partial gunzip of small byte ranges.

Selection this round (2 of the 9 requested): **GSE147424** (primary CellChat
protocol dataset) and **GSE288459** (larger heterogeneous test). The remaining
datasets (GSM3453535–38 sample-level, GSE275552, scVelo pancreas) are deferred
to the next round.

## Manifest

| Column | GSE147424 | GSE288459 |
|---|---|---|
| accession | GSE147424 | GSE288459 |
| accession_type | GEO series | GEO series |
| title | Single-cell transcriptome analysis of human skin identifies novel fibroblast subpopulation and enrichment of immune subsets in atopic dermatitis | Single Cell Transcriptome Signatures of Sarcoidosis in Lung Immune Cell Populations |
| organism | Homo sapiens | Homo sapiens |
| tissue | Skin (Lesional / Non-lesional / Healthy) | Bronchoalveolar lavage |
| condition | disease: Atopic dermatitis vs Healthy; tissue type; frozen vs fresh | diagnosis: SarcP / SarcNP / Control (+ age, Sex) |
| technology | scRNA-seq, GPL16791 (Illumina HiSeq 2500) | scRNA-seq, GPL34281 |
| sample_count | 17 (GSM4430459 … GSM4430475) | 16 (GSM8768252 … GSM8768267) |
| matrix_available | YES — `GSE147424_RAW.tar` (319.8 MB): 17 per-GSM `GSM*_MS.sample*.clean.data.txt.gz` (14.7–41.3 MB each) | YES — `GSE288459_RAW.tar` (886.2 MB): 10x triplets per GSM (`*_barcodes.tsv.gz` / `*_features.tsv.gz` / `*_matrix.mtx.gz`, ≈55 MB mtx per sample) |
| raw_counts_available | **NO** — values are normalized floats ("clean data", e.g. 0.52) | **YES** — 10x MatrixMarket raw counts |
| cell_metadata_available | NO (GEO carries sample-level disease/tissue only; no cell-level metadata) | NO (cell metadata only inside the 14.3 GB Seurat object) |
| cell_annotations_available | NO | Only inside `GSE288459_geo_allSeuratObject.Rdata.gz` (14.3 GB) |
| spliced_layer_available | NO | NO |
| unspliced_layer_available | NO | NO |
| cellchat_ready | NO — expression matrix suitable, but requires Seurat preprocessing + clustering + cell-type labels first (matches CellChat v2 protocol usage) | NO — same; annotations exist but locked in the 14.3 GB object |
| cellphonedb_ready | NO — same prerequisites (expression + cell-group labels export) | NO — same |
| scvelo_ready | NO — no spliced/unspliced layers | NO — no spliced/unspliced layers |
| recommended_test | Partial import: extract 1–2 GSM `.txt.gz` from the tar and run the offline import path (CSV, gene symbols × `S#_<barcode>` columns) + Seurat pipeline | Listing/metadata smoke only; optional single-sample 10x-MTX import (GSM8768252 triplet ≈56 MB) when SC-GEO import exists |
| notes | CSV quoted, first column = gene **symbols** (A1BG, A1BG-AS1…), columns = cells prefixed `S1_`+16 bp barcode. Dense text: one 30 MB sample ⇒ ~2 GB in RAM once parsed — load one sample at a time. Full tar ≈2.5–3 GB extracted. | ⚠ **`GSE288459_geo_allSeuratObject.Rdata.gz` = 14.3 GB — exceeds the 32 GB RAM budget when loaded; never auto-download.** features.tsv.gz ≈0.3 MB ⇒ ~36 k features; gene-ID format assumed Ensembl+symbol (10x standard), to confirm at first import. |

## Validation checklist results (import-module smoke)

1. GEO series lookup — PASS (both series, 1 platform each).
2. GSM sample lookup — PASS (17 / 16 GSMs listed in pData).
3. Metadata parsing — PASS (informative pData columns: 6/42 and 6/47; disease, tissue type, frozen vs fresh; diagnosis, age, Sex).
4. Supplementary-file listing — PASS (`GSE147424_RAW.tar` 319.8 MB; `GSE288459_RAW.tar` 886.2 MB + Rdata 14.3 GB).
5. Matrix-file detection — PARTIAL: `.tar` archives are NOT auto-detected as tabular candidates by `.geo_fetch` (by design); inner formats verified manually (see manifest).
6. Matrix download — NOT exercised for these two (files >300 MB; size shown first per policy). The download path itself is validated by GSE147507/GSE145919/GSE52778 (see test-mod-geo.R, TS_GEO_LIVE_SMOKE=1).
7. Sparse-matrix import — NOT APPLICABLE via mod_geo (dense tabular path only). GSE288459's 10x MTX triplets need the single-cell import workflow (future SC-GEO work), not the bulk-oriented mod_geo.
8. Barcode/gene-ID validation — GSE147424: gene symbols + `S#_`-prefixed 16 bp barcodes (verified by partial gunzip). GSE288459: 10x barcodes/features (structure verified; ID format to confirm).
9. Sample metadata creation — available from pData characteristics (disease / tissue type / frozen-fresh; diagnosis / age / Sex).
10. Seurat object creation — NOT run (depends on SC import path).
11. H5AD import — NOT AVAILABLE (no .h5ad in either series).
12–13. spliced/unspliced detection & scVelo validation — N/A (absent in both; explicitly not scvelo_ready).
14. CellPhoneDB export — not attempted (no cell labels available).
15–16. Graceful handling — `.geo_fetch` refuses non-tabular supplements with an actionable French message; unsupported formats surface the "mode hors-ligne" hint.
17. Memory warnings — GSE288459 Rdata flagged (14.3 GB); GSE147424 dense per-sample parsing flagged (~2 GB RAM per sample).

## Open items for next rounds

- GSM3453535–GSM3453538 (sample-level, same CellChat protocol) — multi-sample import + metadata harmonization.
- GSE275552 — condition-aware communication comparison (WNT/ACTIVIN/IFN-I).
- scVelo pancreas (`.h5ad`, spliced/unspliced) — not a GEO accession; requires the AnnData/H5AD import path.
- When SC-GEO import (10x MTX triplets → Seurat) is implemented, re-run items 7, 10 and flip `cellchat_ready`/`cellphonedb_ready` per dataset after clustering + labels exist.
