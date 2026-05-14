TranscriptoShinyUne application R Shiny modulaire pour l'analyse transcriptomique locale (Single-Cell & Bulk), optimisée pour Seurat v5.InstallationAssurez-vous d'avoir R et RStudio installés.Installez les dépendances :install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "DT", "patchwork", "viridis", "bsicons"))
# Pour Seurat (si non installé)
install.packages("Seurat")
Placez les fichiers dans la structure suivante :app.R (racine)global.R (racine)modules/ (dossier)mod_import.Rmod_sc.Rmod_bulk.RUtilisationOuvrez app.R dans RStudio.Cliquez sur Run App.Allez dans l'onglet Données pour charger un fichier .rds (Objet Seurat v5).Note : Pour tester, vous pouvez sauvegarder l'objet pbmc_small de Seurat : saveRDS(SeuratObject::pbmc_small, "test.rds").Basculez vers l'onglet Single-Cell pour visualiser.ArchitectureModularité : Chaque onglet est un module Shiny indépendant (moduleServer).Données : Les données sont partagées via un objet reactiveValues appelé global_data.Performance : Les calculs lourds utilisent des validateurs pour éviter les plantages si les données sont absentes.

a coller dans la console.

install.packages("bsicons")
install.packages("bslib")

