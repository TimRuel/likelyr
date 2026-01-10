# ======================================================================
# plot-style.R
#
# Semantic accessors for plot colors and styles defined in
# inst/styles/plots.yml
#
# Plots should NEVER hard-code hex values or style constants.
# They should ask for colors and styles by meaning.
# ======================================================================

# ----------------------------------------------------------------------
# Internal: load style spec
# ----------------------------------------------------------------------

#' Load plot style YAML
#'
#' @description
#' Reads inst/styles/plots.yml from the installed package.
#'
#' @return Named list of style configuration
#' @keywords internal
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

#' Navigate nested style node
#'
#' @param path Character vector of node names
#' @return Node value or NULL
#' @keywords internal
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

#' Lookup style value with validation
#'
#' @param path Base path
#' @param key Value name
#' @param label Human-readable label for errors
#' @return Requested style value
#' @keywords internal
.get_style_value <- function(path, key, label) {
  node <- .get_style_node(path)

  if (is.null(node) || !key %in% names(node)) {
    stop("Unknown plot ", label, ": ", key, call. = FALSE)
  }

  node[[key]]
}

#' Comparison-aware style lookup
#'
#' @param base_path Base node
#' @param key Field name
#' @param comparison Logical
#' @return Style value
#' @keywords internal
.get_comparison_value <- function(base_path, key, comparison) {
  node <- .get_style_node(base_path)

  if (isTRUE(comparison) && !is.null(node$comparison)) {
    comp <- node$comparison[[key]]
    if (!is.null(comp)) return(comp)
  }

  node[[key]]
}

#' Resolve scalar vs named list values
#'
#' @keywords internal
.resolve_pseudolikelihood_value <- function(val, pseudolikelihood) {
  if (is.list(val)) {
    return(val[[pseudolikelihood]])
  }

  val
}


# ----------------------------------------------------------------------
# Curve styling
# ----------------------------------------------------------------------

#' Curve color
#'
#' @param pseudolikelihood "integrate" or "profile"
#' @param comparison Logical
#' @return Hex color
#' @export
plot_curve_color <- function(pseudolikelihood, comparison = FALSE) {
  val <- .get_comparison_value(c("curve"), "colors", comparison)
  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Curve linetype
#'
#' @param pseudolikelihood "integrate" or "profile"
#' @param comparison Logical
#' @return Character linetype
#' @export
plot_curve_linetype <- function(pseudolikelihood, comparison = FALSE) {
  val <- .get_comparison_value(c("curve"), "linetype", comparison)
  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Curve linewidth
#'
#' @return Numeric linewidth
#' @export
plot_curve_linewidth <- function() {
  .get_style_node(c("curve", "linewidth"))
}


# ----------------------------------------------------------------------
# Point cloud styling
# ----------------------------------------------------------------------

#' Point cloud size
#'
#' @return Numeric size
#' @export
plot_point_cloud_size <- function() {
  .get_style_node(c("points", "cloud", "size"))
}

#' Point cloud alpha
#'
#' @return Numeric alpha
#' @export
plot_point_cloud_alpha <- function() {
  .get_style_node(c("points", "cloud", "alpha"))
}


# ----------------------------------------------------------------------
# Point estimate styling
# ----------------------------------------------------------------------

#' Point estimate color
#'
#' @param pseudolikelihood "integrate" or "profile"
#' @param comparison Logical
#' @return Hex color
#' @export
plot_point_estimate_color <- function(pseudolikelihood, comparison = FALSE) {
  val <- .get_comparison_value(c("point_estimate"), "colors", comparison)
  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Point estimate size
#'
#' @return Numeric size
#' @export
plot_point_estimate_size <- function() {
  .get_style_node(c("point_estimate", "size"))
}

#' Point estimate linetype
#'
#' @param pseudolikelihood "integrate" or "profile"
#' @param comparison Logical
#' @return Character linetype
#' @export
plot_point_estimate_linetype <- function(pseudolikelihood, comparison = FALSE) {
  val <- .get_comparison_value(c("point_estimate"), "linetype", comparison)
  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}


# ----------------------------------------------------------------------
# Truth marker styling
# ----------------------------------------------------------------------

#' Truth marker color
#' @export
plot_truth_color <- function() .get_style_node(c("truth", "color"))

#' Truth marker shape
#' @export
plot_truth_shape <- function() .get_style_node(c("truth", "shape"))

#' Truth marker size
#' @export
plot_truth_size <- function() .get_style_node(c("truth", "size"))

#' Truth marker stroke
#' @export
plot_truth_stroke <- function() .get_style_node(c("truth", "stroke"))


# ----------------------------------------------------------------------
# Confidence interval styling
# ----------------------------------------------------------------------

#' CI linetype
#' @export
plot_ci_linetype <- function() {
  .get_style_node(c("interval_estimate", "line", "linetype"))
}

#' CI linewidth
#' @export
plot_ci_linewidth <- function() {
  .get_style_node(c("interval_estimate", "line", "linewidth"))
}

#' CI color palette
#'
#' @param interval_estimate_df Data frame
#' @return Named character vector
#' @export
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

#' Reference line style
#'
#' @param name Style key
#' @return Named list
#' @export
plot_reference_line_style <- function(name) {
  .get_style_value(c("reference_lines"), name, "reference line")
}


# ----------------------------------------------------------------------
# Theme accessors
# ----------------------------------------------------------------------

#' Full theme configuration
#'
#' @return Named list
#' @export
plot_theme_cfg <- function() {
  .get_style_node(c("theme"))
}


# ----------------------------------------------------------------------
# Public accessors required by plot-utils.R
# ----------------------------------------------------------------------

#' Panel background fill
#' @export
plot_bg_panel_fill <- function() {
  .get_style_node(c("theme", "background", "panel", "fill"))
}

#' Panel background border color
#' @export
plot_bg_panel_color <- function() {
  .get_style_node(c("theme", "background", "panel", "color"))
}

#' Plot background fill
#' @export
plot_bg_plot_fill <- function() {
  .get_style_node(c("theme", "background", "plot", "fill"))
}

#' Plot background border color
#' @export
plot_bg_plot_color <- function() {
  .get_style_node(c("theme", "background", "plot", "color"))
}

#' Legend background fill
#' @export
plot_bg_legend_fill <- function() {
  .get_style_node(c("theme", "background", "legend", "fill"))
}


#' Major grid color
#' @export
plot_grid_major_color <- function() {
  .get_style_node(c("theme", "grid", "major", "color"))
}

#' Minor grid color
#' @export
plot_grid_minor_color <- function() {
  .get_style_node(c("theme", "grid", "minor", "color"))
}


#' Axis tick color
#' @export
plot_axis_tick_color <- function() {
  .get_style_node(c("theme", "axis", "ticks", "color"))
}

#' Axis text color
#' @export
plot_axis_text_color <- function() {
  .get_style_node(c("theme", "axis", "text", "color"))
}

#' Axis title color
#' @export
plot_axis_title_color <- function() {
  .get_style_node(c("theme", "axis", "title", "color"))
}


#' Strip label color
#' @export
plot_strip_text_color <- function() {
  .get_style_node(c("theme", "strip", "text", "color"))
}


#' Plot title color
#' @export
plot_title_color <- function() {
  .get_style_node(c("theme", "plot", "title", "color"))
}

#' Plot title face
#' @export
plot_title_face <- function() {
  .get_style_node(c("theme", "plot", "title", "face"))
}


#' Legend text color
#' @export
plot_legend_text_color <- function() {
  .get_style_node(c("theme", "legend", "text", "color"))
}

#' Legend title color
#' @export
plot_legend_title_color <- function() {
  .get_style_node(c("theme", "legend", "title", "color"))
}


# ----------------------------------------------------------------------
# Diagnostics styling
# ----------------------------------------------------------------------

#' Diagnostics defaults
#' @export
plot_diagnostics_defaults <- function() {
  .get_style_node(c("diagnostics", "defaults"))
}

#' Diagnostics method override
#'
#' @param method "integrate" or "profile"
#' @export
plot_diagnostics_method_cfg <- function(method) {
  .get_style_node(c("diagnostics", "by_method"))[[method]]
}

#' Diagnostics plot override
#'
#' @param plot_name Plot key
#' @export
plot_diagnostics_plot_cfg <- function(plot_name) {
  .get_style_node(c("diagnostics", "plots"))[[plot_name]]
}

#' Resolve diagnostics style
#'
#' @param method Method
#' @param plot Plot name
#' @return Named list
#' @export
plot_diagnostics_style <- function(method, plot) {
  defaults <- plot_diagnostics_defaults()
  by_meth <- plot_diagnostics_method_cfg(method) %||% list()
  by_plot <- plot_diagnostics_plot_cfg(plot) %||% list()

  style <- modifyList(defaults, by_meth)
  style <- modifyList(style, by_plot)

  style
}


# ----------------------------------------------------------------------
# Text sizing
# ----------------------------------------------------------------------

#' Base text size
#' @keywords internal
.get_base_text_size <- function(path, default = 11) {
  cfg <- plot_theme_cfg()

  val <- tryCatch(
    Reduce(function(x, y) x[[y]], path, init = cfg),
    error = function(e) NULL
  )

  val %||% default
}

#' Plot-specific override
#' @keywords internal
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

#' Resolve text size
#' @keywords internal
.resolve_text_size <- function(plot, base_path, default = 11) {
  override <- .get_plot_text_override(plot, base_path)
  if (!is.null(override)) {
    return(override)
  }
  .get_base_text_size(base_path, default)
}


# ----------------------------------------------------------------------
# Public text accessors
# ----------------------------------------------------------------------

#' Axis text size
#' @export
plot_axis_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("axis", "text", "size"), 11)
}

#' Axis title size
#' @export
plot_axis_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("axis", "title", "size"), 12)
}

#' Plot title size
#' @export
plot_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "title", "size"), 14)
}

#' Plot subtitle size
#' @export
plot_subtitle_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "subtitle", "size"), 12)
}

#' Plot caption size
#' @export
plot_caption_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("plot", "caption", "size"), 10)
}

#' Label text size
#' @export
plot_label_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("labels", "text", "size"), 11)
}

#' Legend text size
#' @export
plot_legend_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("legend", "text", "size"), 10)
}

#' Legend title size
#' @export
plot_legend_title_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("legend", "title", "size"), 11)
}

#' Strip label size
#' @export
plot_strip_text_size <- function(plot = NULL) {
  .resolve_text_size(plot, c("strip", "text", "size"), 11)
}
