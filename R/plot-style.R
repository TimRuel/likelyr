# ======================================================================
# plot-style.R
#
# Semantic accessors for plot colors and styles defined in
# inst/styles/plots.yml
# ======================================================================

# ----------------------------------------------------------------------
# Internal: load style spec
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
.load_plot_style <- function() {
  path <- system.file("styles", "plots.yml", package = "likelyr")
  if (path == "") {
    stop("plots.yml not found in installed package.", call. = FALSE)
  }
  yaml::read_yaml(path)
}

# ----------------------------------------------------------------------
# Internal helpers
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
.get_style_node <- function(path) {
  style <- .load_plot_style()
  node <- style

  for (p in path) {
    if (!p %in% names(node)) {
      return(NULL)
    }
    node <- node[[p]]
  }

  node
}

#' @keywords internal
#' @noRd
.get_style_value <- function(path, key, label) {
  node <- .get_style_node(path)

  if (is.null(node) || !key %in% names(node)) {
    stop("Unknown plot ", label, ": ", key, call. = FALSE)
  }

  node[[key]]
}

# ----------------------------------------------------------------------
# Curve styling
# ----------------------------------------------------------------------

#' Resolve curve color
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param comparison Logical; use comparison styling?
#' @return Hex color string
#' @keywords internal
#' @noRd
plot_curve_color <- function(pseudolikelihood, comparison = FALSE) {
  pseudolikelihood <- tolower(pseudolikelihood)

  if (isTRUE(comparison)) {
    node <- .get_style_node(c("curve", "comparison"))
  } else {
    node <- .get_style_node(c("curve", "by_pseudolikelihood"))
  }

  if (is.null(node) || !pseudolikelihood %in% names(node)) {
    stop(
      "Unknown curve styling for pseudolikelihood '",
      pseudolikelihood,
      "'.",
      call. = FALSE
    )
  }

  node[[pseudolikelihood]]$color
}

#' Resolve curve linetype
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param comparison Logical; use comparison styling?
#' @return Character linetype
#' @keywords internal
#' @noRd
plot_curve_linetype <- function(pseudolikelihood, comparison = FALSE) {
  pseudolikelihood <- tolower(pseudolikelihood)

  if (isTRUE(comparison)) {
    node <- .get_style_node(c("curve", "comparison"))
  } else {
    node <- .get_style_node(c("curve", "by_method"))
  }

  if (is.null(node) || !pseudolikelihood %in% names(node)) {
    stop(
      "Unknown curve styling for pseudolikelihood '",
      pseudolikelihood,
      "'.",
      call. = FALSE
    )
  }

  node[[pseudolikelihood]]$linetype
}

#' Resolve curve linewidth
#'
#' @param pseudolikelihood Optional pseudolikelihood type
#' @param comparison Logical; use comparison styling?
#' @return Numeric linewidth
#' @keywords internal
#' @noRd
plot_curve_linewidth <- function(pseudolikelihood = NULL, comparison = FALSE) {
  base <- .get_style_node(c("curve", "linewidth"))
  pseudolikelihood <- tolower(pseudolikelihood %||% "")

  if (isTRUE(comparison) && nzchar(pseudolikelihood)) {
    node <- .get_style_node(c("curve", "comparison"))
    if (!is.null(node[[pseudolikelihood]]$linewidth)) {
      return(node[[pseudolikelihood]]$linewidth)
    }
  }

  if (!isTRUE(comparison) && nzchar(pseudolikelihood)) {
    node <- .get_style_node(c("curve", "by_method"))
    if (!is.null(node[[pseudolikelihood]]$linewidth)) {
      return(node[[pseudolikelihood]]$linewidth)
    }
  }

  base
}

# ----------------------------------------------------------------------
# Point cloud styling
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
plot_point_cloud_size <- function() {
  .get_style_node(c("points", "cloud", "size"))
}

#' @keywords internal
#' @noRd
plot_point_cloud_alpha <- function() {
  .get_style_node(c("points", "cloud", "alpha"))
}

# ----------------------------------------------------------------------
# Point estimate styling
# ----------------------------------------------------------------------

#' Resolve point estimate color
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param comparison Logical
#' @return Hex color string
#' @keywords internal
#' @noRd
plot_point_estimate_color <- function(pseudolikelihood, comparison = FALSE) {
  pseudolikelihood <- tolower(pseudolikelihood)

  if (isTRUE(comparison)) {
    node <- .get_style_node(c("point_estimate", "comparison"))
    node[[pseudolikelihood]]$color
  } else {
    .get_style_node(c("point_estimate", "color"))
  }
}

#' Resolve point estimate size
#'
#' @return Numeric size
#' @keywords internal
#' @noRd
plot_point_estimate_size <- function() {
  .get_style_node(c("point_estimate", "linewidth"))
}

#' Resolve point estimate linetype
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param comparison Logical
#' @return Character linetype
#' @keywords internal
#' @noRd
plot_point_estimate_linetype <- function(pseudolikelihood, comparison = FALSE) {
  pseudolikelihood <- tolower(pseudolikelihood)

  if (isTRUE(comparison)) {
    node <- .get_style_node(c("point_estimate", "comparison"))
    node[[pseudolikelihood]]$linetype
  } else {
    .get_style_node(c("point_estimate", "linetype"))
  }
}

#' Resolve point estimate linewidth
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param comparison Logical
#' @return Numeric linewidth
#' @keywords internal
#' @noRd
plot_point_estimate_linewidth <- function(
  pseudolikelihood,
  comparison = FALSE
) {
  pseudolikelihood <- tolower(pseudolikelihood)

  if (isTRUE(comparison)) {
    node <- .get_style_node(c("point_estimate", "comparison"))
    node[[pseudolikelihood]]$linewidth
  } else {
    .get_style_node(c("point_estimate", "linewidth"))
  }
}

# ----------------------------------------------------------------------
# Truth marker styling (simple accessors)
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
plot_truth_color <- function() .get_style_node(c("truth", "color"))
#' @keywords internal
#' @noRd
plot_truth_linetype <- function() .get_style_node(c("truth", "linetype"))
#' @keywords internal
#' @noRd
plot_truth_linewidth <- function() .get_style_node(c("truth", "linewidth"))
#' @keywords internal
#' @noRd
plot_truth_shape <- function() .get_style_node(c("truth", "shape"))
#' @keywords internal
#' @noRd
plot_truth_size <- function() .get_style_node(c("truth", "size"))
#' @keywords internal
#' @noRd
plot_truth_stroke <- function() .get_style_node(c("truth", "stroke"))

# ----------------------------------------------------------------------
# Confidence interval styling
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
plot_ci_linetype <- function() {
  .get_style_node(c("interval_estimate", "line", "linetype"))
}

#' @keywords internal
#' @noRd
plot_ci_linewidth <- function() {
  .get_style_node(c("interval_estimate", "line", "linewidth"))
}

#' Generate confidence interval color palette
#'
#' @param interval_estimate_df Data frame of interval estimates
#' @return Named character vector of colors
#' @keywords internal
#' @noRd
plot_ci_palette <- function(interval_estimate_df) {
  style <- .load_plot_style()
  cfg <- style$interval_estimate

  name_from <- cfg$name_from %||% "Level"
  n <- nrow(interval_estimate_df)
  p <- cfg$params

  cols <- colorspace::sequential_hcl(
    n,
    h = p$h,
    c = p$c,
    l = p$l,
    power = p$power
  )

  names(cols) <- interval_estimate_df[[name_from]]
  cols
}

# ----------------------------------------------------------------------
# Reference lines
# ----------------------------------------------------------------------

#' Resolve reference line style
#'
#' @param name Style key
#' @return Named list of style values
#' @keywords internal
#' @noRd
plot_reference_line_style <- function(name) {
  .get_style_value(c("reference_lines"), name, "reference line")
}

# ----------------------------------------------------------------------
# Theme accessors
# ----------------------------------------------------------------------

#' Theme configuration
#' @keywords internal
#' @noRd
plot_theme_cfg <- function() {
  .get_style_node(c("theme"))
}

#' Panel background fill
#' @keywords internal
#' @noRd
plot_bg_panel_fill <- function() {
  .get_style_node(c("theme", "background", "panel", "fill"))
}

#' Panel background border
#' @keywords internal
#' @noRd
plot_bg_panel_color <- function() {
  .get_style_node(c("theme", "background", "panel", "color"))
}

#' Plot background fill
#' @keywords internal
#' @noRd
plot_bg_plot_fill <- function() {
  .get_style_node(c("theme", "background", "plot", "fill"))
}

#' Plot background border
#' @keywords internal
#' @noRd
plot_bg_plot_color <- function() {
  .get_style_node(c("theme", "background", "plot", "color"))
}

#' Legend background fill
#' @keywords internal
#' @noRd
plot_bg_legend_fill <- function() {
  .get_style_node(c("theme", "background", "legend", "fill"))
}

#' Major grid color
#' @keywords internal
#' @noRd
plot_grid_major_color <- function() {
  .get_style_node(c("theme", "grid", "major", "color"))
}

#' Minor grid color
#' @keywords internal
#' @noRd
plot_grid_minor_color <- function() {
  .get_style_node(c("theme", "grid", "minor", "color"))
}

#' Axis tick color
#' @keywords internal
#' @noRd
plot_axis_tick_color <- function() {
  .get_style_node(c("theme", "axis", "ticks", "color"))
}

#' Axis text color
#' @keywords internal
#' @noRd
plot_axis_text_color <- function() {
  .get_style_node(c("theme", "axis", "text", "color"))
}

#' Axis title color
#' @keywords internal
#' @noRd
plot_axis_title_color <- function() {
  .get_style_node(c("theme", "axis", "title", "color"))
}

#' Strip label color
#' @keywords internal
#' @noRd
plot_strip_text_color <- function() {
  .get_style_node(c("theme", "strip", "text", "color"))
}

#' Plot title color
#' @keywords internal
#' @noRd
plot_title_color <- function() {
  .get_style_node(c("theme", "plot", "title", "color"))
}

#' Plot title face
#' @keywords internal
#' @noRd
plot_title_face <- function() {
  .get_style_node(c("theme", "plot", "title", "face"))
}

#' Legend text color
#' @keywords internal
#' @noRd
plot_legend_text_color <- function() {
  .get_style_node(c("theme", "legend", "text", "color"))
}

#' Legend title color
#' @keywords internal
#' @noRd
plot_legend_title_color <- function() {
  .get_style_node(c("theme", "legend", "title", "color"))
}

# ----------------------------------------------------------------------
# Diagnostics styling
# ----------------------------------------------------------------------

#' Diagnostics default styles
#' @return Named list
#' @keywords internal
#' @noRd
plot_diagnostics_defaults <- function() {
  .get_style_node(c("diagnostics", "defaults"))
}

#' Diagnostics pseudolikelihood overrides
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @return Named list
#' @keywords internal
#' @noRd
plot_diagnostics_pseudolikelihood_cfg <- function(pseudolikelihood) {
  .get_style_node(c("diagnostics", "by_pseudolikelihood"))[[pseudolikelihood]]
}

#' Diagnostics plot overrides
#'
#' @param plot_name Plot key
#' @return Named list
#' @keywords internal
#' @noRd
plot_diagnostics_plot_cfg <- function(plot_name) {
  .get_style_node(c("diagnostics", "plots"))[[plot_name]]
}

#' Resolve diagnostics style
#'
#' @param pseudolikelihood "integrated" or "profile"
#' @param plot Plot key
#' @return Named list of merged styles
#' @keywords internal
#' @noRd
plot_diagnostics_style <- function(pseudolikelihood, plot) {
  defaults <- plot_diagnostics_defaults()
  by_pseudolikelihood <- plot_diagnostics_pseudolikelihood_cfg(
    pseudolikelihood
  ) %||%
    list()
  by_plot <- plot_diagnostics_plot_cfg(plot) %||% list()

  style <- modifyList(defaults, by_pseudolikelihood)
  style <- modifyList(style, by_plot)

  style
}

# ----------------------------------------------------------------------
# Text sizing API
# ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
.get_base_text_size <- function(path, default = 11) {
  cfg <- plot_theme_cfg()

  val <- tryCatch(
    Reduce(function(x, y) x[[y]], path, init = cfg),
    error = function(e) NULL
  )

  val %||% default
}

#' @keywords internal
#' @noRd
.get_plot_text_override <- function(plot, path) {
  if (is.null(plot)) {
    return(NULL)
  }

  style <- .load_plot_style()
  cfg <- style$theme$plots %||% list()

  if (!plot %in% names(cfg)) {
    return(NULL)
  }

  tryCatch(
    Reduce(function(x, y) x[[y]], path, init = cfg[[plot]]),
    error = function(e) NULL
  )
}

#' @keywords internal
#' @noRd
.resolve_text_size <- function(plot, base_path, default = 11) {
  override <- .get_plot_text_override(plot, base_path)

  if (!is.null(override)) {
    return(override)
  }

  .get_base_text_size(base_path, default)
}

#' Axis text size
#' @keywords internal
#' @noRd
plot_axis_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("axis", "text", "size"), 11)
}

#' Axis title size
#' @keywords internal
#' @noRd
plot_axis_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("axis", "title", "size"), 12)
}

#' Plot title size
#' @keywords internal
#' @noRd
plot_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "title", "size"), 14)
}

#' Plot subtitle size
#' @keywords internal
#' @noRd
plot_subtitle_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "subtitle", "size"), 12)
}

#' Plot caption size
#' @keywords internal
#' @noRd
plot_caption_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "caption", "size"), 10)
}

#' Label text size
#' @keywords internal
#' @noRd
plot_label_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("labels", "text", "size"), 11)
}

#' Legend text size
#' @keywords internal
#' @noRd
plot_legend_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("legend", "text", "size"), 10)
}

#' Legend title size
#' @keywords internal
#' @noRd
plot_legend_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("legend", "title", "size"), 11)
}

#' Strip label size
#' @keywords internal
#' @noRd
plot_strip_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("strip", "text", "size"), 11)
}
