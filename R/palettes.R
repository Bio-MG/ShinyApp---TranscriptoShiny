# R/palettes.R
# Shared palette helpers for TranscriptoShiny
#
# Provides consistent discrete/continuous/diverging palette functions and
# the manual color-picker UI used by mod_sc, helpers_bulk and spatial modules.
#
# Backwards-compatible function names are exported so existing modules can
# call these functions without editing (later we can refactor callers to use
# a single namespace if desired).

`%||%` <- function(a, b) if (is.null(a)) b else a

# Okabe-Ito colorblind-safe palette
.sc_okabe <- c("#E69F00","#56B4E9","#009E73","#F0E442",
               "#0072B2","#D55E00","#CC79A7","#999999")

# Default manual color sequence (Okabe-Ito recycled)
.default_manual_colors <- function(n) {
  base <- c("#E69F00","#56B4E9","#009E73","#F0E442",
            "#0072B2","#D55E00","#CC79A7","#999999")
  rep_len(base, n)
}

# Native HTML5 color-picker row (reusable by modules). Namespacing `ns`
# is the session$ns function supplied by module servers.
manual_color_picker_ui <- function(ns, ids, labels, defaults) {
  tagList(
    div(
      style = "display:flex;flex-wrap:wrap;gap:14px;align-items:center;padding:8px 0;",
      lapply(seq_along(ids), function(i) {
        full_id <- ns(ids[i])
        div(
          style = "display:flex;align-items:center;gap:6px;",
          tags$input(
            type     = "color",
            id       = full_id,
            value    = defaults[i],
            style    = "width:34px;height:28px;border:1px solid #ccc;border-radius:4px;padding:0;cursor:pointer;",
            onchange = sprintf("Shiny.setInputValue('%s', this.value)", full_id)
          ),
          tags$span(labels[i], style = "font-size:0.85em;")
        )
      })
    )
  )
}

# ---------------------------
# Single-cell palette helpers
# ---------------------------

# sc_discrete_scale: returns a ggplot discrete scale layer (color or fill) or NULL
sc_discrete_scale <- function(palette = "default", manual_colors = NULL, aesthetic = "color") {
  fn_m <- if (aesthetic == "color") ggplot2::scale_color_manual else ggplot2::scale_fill_manual
  fn_v <- if (aesthetic == "color") viridis::scale_color_viridis_d else viridis::scale_fill_viridis_d
  fn_b <- if (aesthetic == "color") ggplot2::scale_color_brewer   else ggplot2::scale_fill_brewer

  if (identical(palette, "manual") && length(manual_colors) > 0)
    return(fn_m(values = manual_colors))

  switch(palette %||% "default",
    okabeito = fn_m(values = .sc_okabe),
    viridis  = fn_v(option = "turbo"),
    set2     = fn_b(palette = "Set2"),
    NULL)   # default -> let Seurat/ggplot choose
}

# sc_continuous_scale: returns sequential color scale for numeric features
sc_continuous_scale <- function(palette = "default", aesthetic = "color", gradient = NULL) {
  fn_g <- if (aesthetic == "color") ggplot2::scale_color_gradient else ggplot2::scale_fill_gradient
  fn_v <- if (aesthetic == "color") viridis::scale_color_viridis_c else viridis::scale_fill_viridis_c

  if (identical(palette, "manual") && !is.null(gradient))
    return(fn_g(low = gradient$low %||% "#2166AC", high = gradient$high %||% "#B2182B"))

  opt <- switch(palette %||% "default",
    okabeito = "plasma", viridis = "plasma", set2 = "viridis", "viridis")
  fn_v(option = opt)
}

# sc_diverging_scale: diverging scale for heatmap / correlation matrix
sc_diverging_scale <- function(palette = "default", aesthetic = "fill", gradient = NULL) {
  fn_g2 <- if (aesthetic == "color") ggplot2::scale_color_gradient2 else ggplot2::scale_fill_gradient2
  if (identical(palette, "manual") && !is.null(gradient))
    return(fn_g2(low = gradient$low %||% "#2166AC", mid = gradient$mid %||% "white",
                 high = gradient$high %||% "#B2182B", midpoint = 0))
  NULL
}

# sc_discrete_colors: concrete hex vector for a set of levels (order preserved)
sc_discrete_colors <- function(levels, palette = "default", manual_colors = NULL) {
  n <- length(levels)
  if (n == 0) return(NULL)
  if (identical(palette, "manual") && length(manual_colors) > 0) {
    cols <- unname(manual_colors[levels])
    cols[is.na(cols)] <- "#999999"
    return(cols)
  }
  switch(palette %||% "default",
    okabeito = rep(.sc_okabe, length.out = n),
    viridis  = {
      if (requireNamespace("viridis", quietly = TRUE)) {
        viridis::viridis(n, option = "turbo")
      } else if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::viridis(n)
      } else rep(.sc_okabe, length.out = n)
    },
    set2     = {
      if (requireNamespace("scales", quietly = TRUE) && requireNamespace("RColorBrewer", quietly = TRUE)) {
        scales::brewer_pal(palette = "Set2")(n)
      } else rep(.sc_okabe, length.out = n)
    },
    NULL)   # default -> let caller use built-in defaults
}

# ---------------------------
# Bulk palette helpers
# ---------------------------

# bulk_color_scale: returns a ggplot discrete color scale layer or NULL
bulk_color_scale <- function(palette = "default", manual_colors = NULL) {
  if (identical(palette, "manual") && !is.null(manual_colors) && length(manual_colors) > 0) {
    return(ggplot2::scale_color_manual(values = manual_colors))
  }
  switch(palette %||% "default",
    okabeito = ggplot2::scale_color_manual(values = .default_manual_colors(8)),
    viridis  = {
      if (requireNamespace("viridis", quietly = TRUE)) viridis::scale_color_viridis_d() else ggplot2::scale_color_viridis_d()
    },
    set2     = ggplot2::scale_color_brewer(palette = "Set2"),
    NULL
  )
}

# bulk_annotation_colors: named color vector for ComplexHeatmap annotation (or NULL for default)
bulk_annotation_colors <- function(levels, palette = "default", manual_colors = NULL) {
  palette <- palette %||% "default"
  if (identical(palette, "default")) return(NULL)
  levels <- unique(as.character(levels))
  n <- length(levels)
  if (n == 0) return(NULL)

  if (identical(palette, "manual")) {
    defaults <- .default_manual_colors(n)
    vals <- vapply(seq_along(levels), function(i) {
      v <- if (levels[i] %in% names(manual_colors)) manual_colors[[levels[i]]] else NA_character_
      if (is.null(v) || is.na(v) || !nzchar(v)) defaults[i] else v
    }, character(1))
    return(setNames(vals, levels))
  }

  cols <- switch(palette,
    okabeito = .default_manual_colors(n),
    viridis  = {
      if (requireNamespace("viridisLite", quietly = TRUE)) viridisLite::viridis(n) else .default_manual_colors(n)
    },
    set2     = {
      if (requireNamespace("RColorBrewer", quietly = TRUE)) RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)] else .default_manual_colors(n)
    },
    .default_manual_colors(n)
  )
  setNames(cols, levels)
}

# bulk_role_colors: Up/Down/NS semantic colors (for volcano/MA)
bulk_role_colors <- function(palette = "default", manual_colors = NULL) {
  presets <- list(
    default  = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7"),
    okabeito = c(Up = "#D55E00", Down = "#0072B2", NS = "#999999"),
    viridis  = {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        v <- viridisLite::viridis(3)
        c(Up = v[3], Down = v[1], NS = "#BDC3C7")
      } else c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
    },
    set2     = {
      if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        s <- RColorBrewer::brewer.pal(3, "Set2"); c(Up = s[1], Down = s[2], NS = "#BDC3C7")
      } else c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
    }
  )
  base <- presets[[palette %||% "default"]]
  if (is.null(base)) base <- presets$default
  if (identical(palette, "manual") && !is.null(manual_colors)) {
    for (role in intersect(names(manual_colors), names(base))) {
      v <- manual_colors[[role]]
      if (!is.null(v) && nzchar(v)) base[[role]] <- v
    }
  }
  base
}
