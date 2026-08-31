# =============================================================================
# R/sc/sc_state.R — SC shared reactive state factory (Block 8 refactor)
# =============================================================================
# Defines the reactiveValues contract shared across all SC sub-modules.
# Called once by mod_sc_server(). Future: restructure into domain-scoped
# sub-lists (sc_state$dataset, sc_state$clustering, etc.) without changing
# the external contract.
# =============================================================================

#' Create the SC shared reactive state
#' @return A reactiveValues object with all SC module fields initialized.
create_sc_shared_state <- function() {
  reactiveValues(
    markers_data     = NULL,
    correlated_genes = NULL,
    corr_target_gene = NULL,
    pathway_results  = NULL,
    pathway_db       = NULL,
    selected_genes   = character(0),
    active_tab       = NULL,
    report_viz_list  = list(),
    traj_reduction   = NULL,
    traj_method      = NULL,
    traj_genes       = character(0),
    max_cells_heavy  = Inf,
    sc_palette              = "default",
    sc_manual_colors        = NULL,
    sc_manual_gradient      = NULL,
    sc_manual_volcano_colors = NULL
  )
}
