# CONTRAT — TABLES DE RÉSULTATS (`ts_datatable()`)

PLOT-S3 (V1.x) · créé le 2026-09-11 · toute modification doit passer
**simultanément** par : le code (`R/plotting/datatable.R`), le test
(`tests/testthat/test-plot-datatable.R`) et ce document.

## 1. Champ d'application

Un seul endroit pour construire les tables de résultats, au lieu de 49 appels
`DT::datatable()` épars (30 fichiers).

## 2. Mesure préalable — **il y a un piège** (contrairement à PLOT-S2)

| Paramètre | Mesure réelle (2026-09-11) | Conséquence |
|---|---|---|
| `pageLength` | **10 (×22), 15 (×15), 8 (×5), 20 (×3), 6 (×1)** | **Aucune valeur par défaut neutre** — même situation que `base_size` en PLOT-S1 → `page_length` est **obligatoire** |
| boutons d'export | **1 seul site sur 49** déclare `extensions = "Buttons"` + `dom = "Bfrtip"` (la table pathways) | `buttons = FALSE` par défaut : activer les boutons est un **opt-in** |
| `rownames` | `FALSE` (×48), `TRUE` (×7), dynamique (`colnames`, `barcodes`, `sample`…) | paramètre exposé, jamais figé |
| `filter` | `"top"` (×12), absent ailleurs | défaut `"top"` (majorité) |
| `scrollX` | `TRUE` (×38) | défaut `TRUE` |

> ⚠️ **La fiche `ROADMAP_presentation_stats.md` §3 est factuellement inversée** :
> elle annonce que `build_de_results_dt` a les boutons et que `pathway_table`
> ne les a pas. C'est l'inverse aujourd'hui (seule la table pathways les a).

## 3. Décision — zéro changement de comportement

Le critère d'acceptation de la fiche (« toutes les tables ont les mêmes 4
boutons ») est **explicitement un changement de comportement** sur ~48 tables,
et une normalisation de `pageLength` en changerait 22 supplémentaires. La règle
n°1 du dépôt l'emporte (même arbitrage que pour `base_size` en PLOT-S1) :

- **`buttons = FALSE` par défaut** → le helper ne pose ni `extensions`, ni
  `dom`, ni `buttons`. Strictement identique à l'appel direct d'origine.
- **`page_length` obligatoire** → le helper refuse de choisir à la place de
  l'appelant.
- Chaque site migré garde **ses valeurs actuelles écrites explicitement**.

**Périmètre livré** : les **6 sites** dont la forme est canonique et donc
mécaniquement vérifiable — dont les 4 nommés par la fiche
(`bulk_helpers.R` 15, `pathway_helpers.R` 10 + boutons, `sc_helpers.R` 15,
`mod_bulk_de_multimethod.R` 15) + `mod_bulk_de_venn.R` 15 et
`mod_spatial_cluster.R` 20.

**Non livré (décision explicite requise)** : les ~43 tables restantes, de formes
hétérogènes, et le déploiement global des boutons. Voir §6.

## 4. API

| Fonction | Rôle |
|---|---|
| `ts_datatable(df, page_length, filename_base, filter, rownames, scroll_x, buttons, dom, extensions, extra_options, ...)` | Construction de la table |
| `ts_datatable_buttons(filename_base)` | Jeu `copy / print / csv / excel` |
| `ts_datatable_page_lengths()` | `6 / 8 / 10 / 15 / 20` |
| `ts_datatable_public_api()` | Surface figée |

Constantes dans `config/defaults.R` : `TS_DT_PAGE_LENGTHS`,
`TS_DT_PAGE_LENGTH_DEFAULT` (`15L`), `TS_DT_BUTTONS_DEFAULT` (`TRUE`).

**`extra_options` (jalon DT-EXPORT)** : liste nommée fusionnée dans `options`
avec priorité maximale — canal unique pour les options DT non exposées
(`language`, `lengthMenu`, `order`, `autoWidth`…). Ne **jamais** passer
`options =` via `...` (collision avec l'option interne ⇒ erreur
« formal argument matched by multiple actual arguments » au rendu).

## 5. États d'erreur

Classe `plot_datatable_error` (FR, `call. = FALSE`) :

| État | Déclencheur |
|---|---|
| `invalid_page_length` | `page_length` manquant, `NULL`, `NA`, non numérique ou < 1 |
| `invalid_data` | donnée non coercible en data.frame |
| `invalid_buttons` | `buttons` non logique |
| `invalid_extra_options` | `extra_options` non NULL et pas une liste nommée |

## 6. Jalon DT-EXPORT (2026-09-14) — déploiement global des boutons

**Décision livrée** (option **B** de la fiche, arbitrage utilisateur du
2026-09-14 — le jalon dédié demandé en §3) :

- **Boutons d'export** : le défaut de `ts_datatable()` devient
  `TS_DT_BUTTONS_DEFAULT = TRUE` — toute table de résultat gagne les 4 boutons
  `copy / print / csv / excel` + `dom = "Bfrtip"` + l'extension `Buttons`.
  `filename_base` reste recommandé (export nommé) ; sans lui, DT applique ses
  boutons par défaut.
- **`pageLength` normalisé à 15** (`TS_DT_PAGE_LENGTH_DEFAULT = 15L`) sur les
  tables de résultats — chaque site écrit `page_length = 15L` explicitement,
  la signature reste **obligatoire** (pas de défaut caché).
- **Exception documentée — tables d'APERÇU** : les previews `dom = "t"/"tip"`
  et les petites tables QC (ex. `pageLength = 5/6/8` sur des tables de
  contrôle) **ne sont ni normalisées ni boutonnées** : elles passent
  `buttons = FALSE` et conservent leur `page_length` d'origine. C'est
  l'écart entre les « ~43 tables » de la fiche et les ~49 sites totaux.
- **Migration complète** : plus aucun appel direct `DT::datatable()` /
  `datatable()` dans `modules/` ni `R/` (hors le wrapper lui-même) — garde
  ajoutée au test de gel.
- Rétrocompatibilité : un site peut revenir au comportement PLOT-S3 en
  passant `buttons = FALSE` explicitement ; la constante
  `TS_DT_BUTTONS_DEFAULT` reste l'interrupteur central.

## 7. Vérification

```bash
"/d/Data_science/R-4.4.2/bin/Rscript.exe" -e \
  'source("tests/testthat/helper-source.R"); testthat::test_file("tests/testthat/test-plot-datatable.R")'
# attendu : FAIL 0 (gèle DT-EXPORT inclus : défaut boutonné + zéro appel direct)
```

## 8. État

- **PLOT-S3 (2026-09-11)** — `R/plotting/datatable.R` créé, 6 sites migrés,
  71 assertions vertes, duplication gate inchangée (0 erreur / 3
  avertissements).
- **DT-EXPORT (2026-09-14)** — défaut `buttons = TRUE` (config), migration
  des ~49 sites restants, `pageLength = 15` normalisé sur les tables de
  résultats, garde « zéro appel direct » ajoutée.
