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

.get_style_node <- function(path) {
  style <- .load_plot_style()
  node  <- style
  for (p in path) node <- node[[p]]
  node
}

.get_style_value <- function(path, key, label) {
  node <- .get_style_node(path)

  if (!key %in% names(node)) {
    stop("Unknown plot ", label, ": ", key, call. = FALSE)
  }
  node[[key]]
}

# Comparison-aware lookup with graceful fallback
.get_comparison_value <- function(base_path, key, comparison) {

  style <- .load_plot_style()
  node  <- style
  for (p in base_path) node <- node[[p]]

  if (isTRUE(comparison) && !is.null(node$comparison)) {
    comp <- node$comparison[[key]]
    if (!is.null(comp)) {
      return(comp)
    }
  }

  node[[key]]
}

# Resolve scalar vs named-list values
.resolve_pseudolikelihood_value <- function(val, pseudolikelihood) {
  if (is.list(val)) {
    return(val[[pseudolikelihood]])
  }
  val
}


# ----------------------------------------------------------------------
# Curve styling
# ----------------------------------------------------------------------

#' Get curve color by pseudolikelihood
#'
#' @param pseudolikelihood One of "integrated", "profile"
#' @param comparison Logical; use comparison styling?
#' @return Hex color string
#' @export
plot_curve_color <- function(pseudolikelihood, comparison = FALSE) {

  val <- .get_comparison_value(
    base_path  = c("curve"),
    key        = "colors",
    comparison = comparison
  )

  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Get curve linetype
#'
#' @param pseudolikelihood One of "integrated", "profile"
#' @param comparison Logical; use comparison styling?
#' @return Character linetype
#' @export
plot_curve_linetype <- function(pseudolikelihood, comparison = FALSE) {

  val <- .get_comparison_value(
    base_path  = c("curve"),
    key        = "linetype",
    comparison = comparison
  )

  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Get curve linewidth
#'
#' @return Numeric linewidth
#' @export
plot_curve_linewidth <- function() {
  .get_style_node(c("curve", "linewidth"))
}


# ----------------------------------------------------------------------
# Point cloud styling (evaluated likelihood points)
# ----------------------------------------------------------------------

#' Get point cloud size
#'
#' @return Numeric size
#' @export
plot_point_cloud_size <- function() {
  .get_style_node(c("points", "cloud", "size"))
}

#' Get point cloud alpha
#'
#' @return Numeric alpha
#' @export
plot_point_cloud_alpha <- function() {
  .get_style_node(c("points", "cloud", "alpha"))
}


# ----------------------------------------------------------------------
# Point estimate styling
# ----------------------------------------------------------------------

#' Get point estimate color by pseudolikelihood
#'
#' @param pseudolikelihood One of "integrated", "profile"
#' @param comparison Logical; use comparison styling?
#' @return Hex color string
#' @export
plot_point_estimate_color <- function(pseudolikelihood, comparison = FALSE) {

  val <- .get_comparison_value(
    base_path  = c("point_estimate"),
    key        = "colors",
    comparison = comparison
  )

  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}

#' Get point estimate marker size
#'
#' @return Numeric size
#' @export
plot_point_estimate_size <- function() {
  .get_style_node(c("point_estimate", "size"))
}

#' Get point estimate linetype
#'
#' @param pseudolikelihood One of "integrated", "profile"
#' @param comparison Logical; use comparison styling?
#' @return Character linetype
#' @export
plot_point_estimate_linetype <- function(pseudolikelihood, comparison = FALSE) {

  val <- .get_comparison_value(
    base_path  = c("point_estimate"),
    key        = "linetype",
    comparison = comparison
  )

  .resolve_pseudolikelihood_value(val, pseudolikelihood)
}


# ----------------------------------------------------------------------
# Truth marker styling
# ----------------------------------------------------------------------

#' Get truth marker color
#'
#' @return Hex color string
#' @export
plot_truth_color <- function() {
  .get_style_node(c("truth", "color"))
}

#' Get truth marker shape
#'
#' @return Integer shape code
#' @export
plot_truth_shape <- function() {
  .get_style_node(c("truth", "shape"))
}

#' Get truth marker size
#'
#' @return Numeric size
#' @export
plot_truth_size <- function() {
  .get_style_node(c("truth", "size"))
}

#' Get truth marker stroke width
#'
#' @return Numeric stroke width
#' @export
plot_truth_stroke <- function() {
  .get_style_node(c("truth", "stroke"))
}


# ----------------------------------------------------------------------
# Confidence interval styling
# ----------------------------------------------------------------------

#' Get CI line linetype
#'
#' @return Character linetype
#' @export
plot_ci_linetype <- function() {
  .get_style_node(c("interval_estimate", "line", "linetype"))
}

#' Get CI line linewidth
#'
#' @return Numeric linewidth
#' @export
plot_ci_linewidth <- function() {
  .get_style_node(c("interval_estimate", "line", "linewidth"))
}

#' Generate CI color palette
#'
#' @param interval_estimate_df Data frame containing interval metadata.
#' @return Named character vector of colors.
#' @export
plot_ci_palette <- function(interval_estimate_df) {

  style <- .load_plot_style()
  cfg   <- style$interval_estimate

  stopifnot(
    !is.null(cfg$generator),
    cfg$generator == "sequential_hcl",
    !is.null(cfg$params)
  )

  name_from <- cfg$name_from %||% "Level"
  if (!name_from %in% names(interval_estimate_df)) {
    stop(
      "interval_estimate_df must contain column '",
      name_from, "'.",
      call. = FALSE
    )
  }

  n      <- nrow(interval_estimate_df)
  params <- cfg$params

  cols <- colorspace::sequential_hcl(
    n,
    h     = params$h,
    c     = params$c,
    l     = params$l,
    power = params$power
  )

  names(cols) <- interval_estimate_df[[name_from]]
  cols
}


# ----------------------------------------------------------------------
# Reference line styling
# ----------------------------------------------------------------------

#' Get reference line style by name
#'
#' @param name Reference line key (e.g. "loglik_zero")
#' @return Named list with linetype, linewidth, and color
#' @export
plot_reference_line_style <- function(name) {
  .get_style_value(
    path  = c("reference_lines"),
    key   = name,
    label = "reference line"
  )
}


# ----------------------------------------------------------------------
# Theme accessors
# ----------------------------------------------------------------------

#' Get full ggplot theme configuration
#'
#' @return Named list corresponding to styles/plots.yml$theme
#' @export
plot_theme_cfg <- function() {
  .get_style_node(c("theme"))
}


# ----------------------------------------------------------------------
# Convenience: flatten style for legacy usage
# ----------------------------------------------------------------------

#' Flatten plot style to a named list
#'
#' @export
flatten_plot_style <- function() {

  style <- .load_plot_style()

  c(
    list(
      curve_integrated = style$curve$colors$integrated,
      curve_profile    = style$curve$colors$profile,
      curve_linewidth  = style$curve$linewidth,

      cloud_size       = style$points$cloud$size,
      cloud_alpha      = style$points$cloud$alpha,

      point_integrated = style$point_estimate$colors$integrated,
      point_profile    = style$point_estimate$colors$profile,
      point_size       = style$point_estimate$size,

      truth_color      = style$truth$color,
      truth_shape      = style$truth$shape,
      truth_size       = style$truth$size,
      truth_stroke     = style$truth$stroke
    ),
    style$interval_estimate$params
  )
}
