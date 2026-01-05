# =====================================================================
# plot-utils.R
# Low-level Plot Building Blocks
#
# Consumes semantic accessors from plot-style.R.
# Never reads YAML directly and never hard-codes values.
# =====================================================================


# ---------------------------------------------------------------------
# Base plot surface
# ---------------------------------------------------------------------

#' Base Dark Theme for Log-Likelihood Plots
#'
#' @return A ggplot2 object representing the base plotting surface.
#' @keywords internal
plot_base <- function() {

  theme_cfg <- plot_theme_cfg()

  base_theme <- switch(
    theme_cfg$base,
    minimal = ggplot2::theme_minimal(),
    classic = ggplot2::theme_classic(),
    ggplot2::theme_minimal()
  )

  ggplot2::ggplot() +
    base_theme +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(
        fill  = theme_cfg$background$panel$fill,
        color = theme_cfg$background$panel$color
      ),
      plot.background   = ggplot2::element_rect(
        fill  = theme_cfg$background$plot$fill,
        color = theme_cfg$background$plot$color
      ),
      panel.grid.major  = ggplot2::element_line(
        color = theme_cfg$grid$major$color
      ),
      panel.grid.minor  = ggplot2::element_line(
        color = theme_cfg$grid$minor$color
      ),
      axis.ticks        = ggplot2::element_line(
        color = theme_cfg$axis$ticks$color
      ),
      axis.text         = ggplot2::element_text(
        color = theme_cfg$axis$text$color
      ),
      axis.title        = ggplot2::element_text(
        color = theme_cfg$axis$title$color
      ),
      strip.text        = ggplot2::element_text(
        color = theme_cfg$strip$text$color
      ),
      plot.title        = ggplot2::element_text(
        color = theme_cfg$plot$title$color,
        face  = theme_cfg$plot$title$face
      ),
      legend.background = ggplot2::element_rect(
        fill = theme_cfg$background$legend$fill
      ),
      legend.text       = ggplot2::element_text(
        color = theme_cfg$legend$text$color
      ),
      legend.title      = ggplot2::element_text(
        color = theme_cfg$legend$title$color
      )
    )
}


# ---------------------------------------------------------------------
# Likelihood curve layers
# ---------------------------------------------------------------------

#' Build stat_function layer for pseudo-log-likelihood curves
#'
#' @param psi_endpoints Numeric range of psi.
#' @param zero_max_psi_ll_fn Zero-shifted log-likelihood function.
#' @param pseudolikelihood One of "integrated", "profile".
#' @param comparison Logical; use comparison styling?
#'
#' @keywords internal
make_stat_fn <- function(
    psi_endpoints,
    zero_max_psi_ll_fn,
    pseudolikelihood,
    comparison = FALSE
) {

  ggplot2::stat_function(
    fun       = zero_max_psi_ll_fn,
    geom      = "line",
    color     = plot_curve_color(pseudolikelihood, comparison = comparison),
    linetype  = plot_curve_linetype(pseudolikelihood, comparison = comparison),
    linewidth = plot_curve_linewidth(),
    xlim      = psi_endpoints
  )
}


# ---------------------------------------------------------------------
# Axes & titles
# ---------------------------------------------------------------------

#' Likelihood plot title
#'
#' @keywords internal
likelihood_title <- function(type) {
  paste(type, "Log-Likelihood")
}

#' Likelihood plot axes
#'
#' @keywords internal
likelihood_axes <- function() {
  ggplot2::labs(
    x = expression(psi),
    y = expression("log L("*psi*")")
  )
}


# ---------------------------------------------------------------------
# Reference lines
# ---------------------------------------------------------------------

#' Zero log-likelihood reference line
#'
#' @keywords internal
loglik_reference_line <- function() {

  style <- plot_reference_line_style("loglik_zero")

  ggplot2::geom_hline(
    yintercept = 0,
    linetype   = style$linetype,
    linewidth  = style$linewidth,
    color      = style$color,
    inherit.aes = FALSE
  )
}


# ---------------------------------------------------------------------
# Confidence interval utilities
# ---------------------------------------------------------------------

#' Pivot CI endpoints to long format
#'
#' @keywords internal
extract_ci_long <- function(interval_estimate_df) {

  raw <- attr(interval_estimate_df, "interval_estimate_raw")
  raw$level <- interval_estimate_df$Level

  tidyr::pivot_longer(
    raw,
    cols      = c("lower", "upper"),
    names_to  = "position",
    values_to = "endpoint"
  )
}

#' Compute y-axis limits from alpha levels
#'
#' @keywords internal
compute_y_limits <- function(alpha) {

  crit_max <- 0.5 * stats::qchisq(1 - min(alpha), df = 1)
  c(-crit_max - 0.5, 0.1)
}

#' Vertical CI lines
#'
#' @keywords internal
make_ci_vline_layer <- function(ci_long) {

  ggplot2::geom_vline(
    data = ci_long,
    ggplot2::aes(xintercept = endpoint, color = level),
    linetype  = plot_ci_linetype(),
    linewidth = plot_ci_linewidth(),
    inherit.aes = FALSE
  )
}

#' Horizontal CI cutoff lines (multi-curve)
#'
#' @keywords internal
make_ci_hline_layer <- function(crit_df) {

  ggplot2::geom_hline(
    data = crit_df,
    ggplot2::aes(yintercept = -crit, color = label),
    linetype  = plot_ci_linetype(),
    linewidth = plot_ci_linewidth()
  )
}


# ---------------------------------------------------------------------
# Label helpers
# ---------------------------------------------------------------------

#' Vertical reference lines for labeled points
#'
#' @param label_data Data frame with columns source, value
#' @param comparison Logical; use comparison styling?
#'
#' @keywords internal
make_label_vlines <- function(label_data, comparison = FALSE) {

  layers <- list()

  for (src in unique(label_data$source)) {

    if (src == "Truth") {
      layers[[src]] <- ggplot2::geom_vline(
        data = subset(label_data, source == src),
        ggplot2::aes(xintercept = value),
        color      = plot_truth_color(),
        linetype   = "solid",
        show.legend = FALSE
      )
    } else {
      layers[[src]] <- ggplot2::geom_vline(
        data = subset(label_data, source == src),
        ggplot2::aes(xintercept = value),
        color      = plot_point_estimate_color(src, comparison = comparison),
        linetype   = plot_point_estimate_linetype(src, comparison = comparison),
        show.legend = FALSE
      )
    }
  }

  layers
}


#' Repelled labels for point annotations
#'
#' @keywords internal
make_label_repel <- function(label_data, y) {

  ggrepel::geom_label_repel(
    data = label_data,
    ggplot2::aes(
      x     = value,
      y     = y,
      label = label,
      color = source
    ),
    direction   = "y",
    force       = TRUE,
    hjust       = 0.5,
    parse       = TRUE,
    seed        = 7835,
    show.legend = FALSE
  )
}


# ---------------------------------------------------------------------
# Point layers
# ---------------------------------------------------------------------

#' Point estimate marker (single-likelihood plots)
#'
#' @keywords internal
make_point_estimate_layer <- function(x, y, pseudolikelihood) {

  ggplot2::geom_point(
    ggplot2::aes(x = x, y = y),
    color = plot_point_estimate_color(pseudolikelihood),
    size  = plot_point_estimate_size()
  )
}

#' Truth marker
#'
#' @keywords internal
make_truth_layer <- function(x, y) {

  ggplot2::geom_point(
    ggplot2::aes(x = x, y = y),
    color  = plot_truth_color(),
    shape  = plot_truth_shape(),
    size   = plot_truth_size(),
    stroke = plot_truth_stroke()
  )
}
