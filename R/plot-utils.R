# =====================================================================
# plot-utils.R
# Low-level Plot Building Blocks
# =====================================================================

# ---------------------------------------------------------------------
# Base theme selector
# ---------------------------------------------------------------------

#' Resolve base ggplot theme
#'
#' @description
#' Returns the base ggplot2 theme specified in the plot style
#' configuration file. Supported values currently include
#' \code{"minimal"} and \code{"classic"}.
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
#' @noRd
plot_theme_base <- function() {
  base <- plot_theme_cfg()$base

  switch(
    base,
    minimal = ggplot2::theme_minimal(),
    classic = ggplot2::theme_classic(),
    ggplot2::theme_minimal()
  )
}

# ---------------------------------------------------------------------
# Base plot surface
# ---------------------------------------------------------------------

#' Construct base plot surface with dark theme
#'
#' @description
#' Builds an empty ggplot object with the global dark theme
#' applied. Optionally accepts a plot name to enable
#' plot-specific text size overrides.
#'
#' @param plot Optional character name of plot type for
#'   applying theme overrides.
#'
#' @return A ggplot object with theme applied.
#'
#' @keywords internal
#' @noRd
plot_base <- function(plot = NULL) {
  ggplot2::ggplot() +
    plot_theme_base() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = plot_bg_panel_fill(),
        color = plot_bg_panel_color()
      ),

      plot.background = ggplot2::element_rect(
        fill = plot_bg_plot_fill(),
        color = plot_bg_plot_color()
      ),

      legend.background = ggplot2::element_rect(
        fill = plot_bg_legend_fill()
      ),

      panel.grid.major = ggplot2::element_line(
        color = plot_grid_major_color()
      ),

      panel.grid.minor = ggplot2::element_line(
        color = plot_grid_minor_color()
      ),

      axis.ticks = ggplot2::element_line(
        color = plot_axis_tick_color()
      ),

      axis.text = ggplot2::element_text(
        color = plot_axis_text_color(),
        size = plot_axis_text_size(plot)
      ),

      axis.title = ggplot2::element_text(
        color = plot_axis_title_color(),
        size = plot_axis_title_size(plot)
      ),

      strip.text = ggplot2::element_text(
        color = plot_strip_text_color(),
        size = plot_strip_text_size(plot)
      ),

      plot.title = ggplot2::element_text(
        color = plot_title_color(),
        face = plot_title_face(),
        size = plot_title_size(plot)
      ),

      plot.subtitle = ggplot2::element_text(
        size = plot_subtitle_size(plot)
      ),

      plot.caption = ggplot2::element_text(
        size = plot_caption_size(plot)
      ),

      legend.text = ggplot2::element_text(
        color = plot_legend_text_color(),
        size = plot_legend_text_size(plot)
      ),

      legend.title = ggplot2::element_text(
        color = plot_legend_title_color(),
        size = plot_legend_title_size(plot)
      )
    )
}

# ---------------------------------------------------------------------
# Likelihood curve layers
# ---------------------------------------------------------------------

#' Build likelihood curve layer
#'
#' @description
#' Constructs a \code{stat_function} layer for a zero-shifted
#' log-likelihood function using semantic styling from
#' \code{plot-style.R}.
#'
#' @param psi_endpoints Numeric vector of length 2 giving
#'   the x-axis limits.
#' @param zero_max_psi_ll_fn Function computing zero-shifted
#'   log-likelihood values.
#' @param pseudolikelihood Character, \code{"integrate"} or
#'   \code{"profile"}.
#' @param comparison Logical; apply comparison styling?
#'
#' @return A ggplot layer.
#'
#' @keywords internal
#' @noRd
make_stat_fn <- function(
  psi_endpoints,
  zero_max_psi_ll_fn,
  pseudolikelihood,
  comparison = FALSE
) {
  ggplot2::stat_function(
    fun = zero_max_psi_ll_fn,
    geom = "line",
    color = plot_curve_color(pseudolikelihood, comparison),
    linetype = plot_curve_linetype(pseudolikelihood, comparison),
    linewidth = plot_curve_linewidth(pseudolikelihood, comparison),
    xlim = psi_endpoints
  )
}

# ---------------------------------------------------------------------
# Titles & axes
# ---------------------------------------------------------------------

#' Generate likelihood plot title
#'
#' @param type Character likelihood type.
#'
#' @return Character plot title.
#'
#' @keywords internal
#' @noRd
likelihood_title <- function(type) {
  type <- tolower(type)

  if (type == "integrate") {
    return("Integrated Log-Likelihood")
  }
  if (type == "profile") {
    return("Profile Log-Likelihood")
  }

  stop(
    "likelihood_title(): unknown type '",
    type,
    "'.",
    call. = FALSE
  )
}

#' Generate likelihood plot axis labels
#'
#' @return ggplot labels object.
#'
#' @keywords internal
#' @noRd
likelihood_axes <- function() {
  ggplot2::labs(
    x = expression(psi),
    y = expression("log L(" * psi * ")")
  )
}

# ---------------------------------------------------------------------
# Reference lines
# ---------------------------------------------------------------------

#' Zero log-likelihood reference line
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
loglik_reference_line <- function() {
  style <- plot_reference_line_style("loglik_zero")

  ggplot2::geom_hline(
    yintercept = 0,
    linetype = style$linetype,
    linewidth = style$linewidth,
    color = style$color,
    inherit.aes = FALSE
  )
}

# ---------------------------------------------------------------------
# Confidence intervals
# ---------------------------------------------------------------------

#' Pivot confidence interval endpoints to long format
#'
#' @param interval_estimate_df Interval estimate data frame.
#'
#' @return Long-format data frame.
#'
#' @keywords internal
#' @noRd
extract_ci_long <- function(interval_estimate_df) {
  raw <- attr(interval_estimate_df, "interval_estimate_raw")
  raw$level <- interval_estimate_df$level

  tidyr::pivot_longer(
    raw,
    cols = c("lower", "upper"),
    names_to = "position",
    values_to = "endpoint"
  )
}

#' Compute y-axis limits for likelihood plot
#'
#' @param psi_ll_df Likelihood evaluation data frame.
#'
#' @return Numeric vector of length 2.
#'
#' @keywords internal
#' @noRd
compute_y_limits <- function(psi_ll_df) {
  y <- psi_ll_df$loglik - max(psi_ll_df$loglik)
  pad <- 0.1
  range(y) + c(-pad, pad)
}


#' Vertical CI endpoint lines
#'
#' @param ci_long Long-format CI data.
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
make_ci_vline_layer <- function(ci_long) {
  ci_long <- ci_long |>
    dplyr::filter(!is.na(endpoint))

  ggplot2::geom_vline(
    data = ci_long,
    ggplot2::aes(xintercept = endpoint, color = level),
    linetype = plot_ci_linetype(),
    linewidth = plot_ci_linewidth(),
    inherit.aes = FALSE
  )
}

#' Horizontal CI cutoff lines
#'
#' @param crit_df Data frame of cutoff values.
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
make_ci_hline_layer <- function(crit_df) {
  ggplot2::geom_hline(
    data = crit_df,
    ggplot2::aes(yintercept = -crit, color = label),
    linetype = plot_ci_linetype(),
    linewidth = plot_ci_linewidth()
  )
}

# ---------------------------------------------------------------------
# Labels
# ---------------------------------------------------------------------

#' Build vertical label reference lines
#'
#' @param label_data Data frame with columns \code{source} and \code{value}.
#' @param comparison Logical; apply comparison styling?
#'
#' @return Named list of ggplot layers.
#'
#' @keywords internal
#' @noRd
make_label_vlines <- function(label_data, comparison = FALSE) {
  layers <- list()

  for (src in unique(label_data$source)) {
    if (src == "Truth") {
      layers[[src]] <- ggplot2::geom_vline(
        data = subset(label_data, source == src),
        ggplot2::aes(xintercept = value),
        color = plot_truth_color(),
        linetype = plot_truth_linetype(),
        linewidth = plot_truth_linewidth(),
        show.legend = FALSE
      )
    } else {
      layers[[src]] <- ggplot2::geom_vline(
        data = subset(label_data, source == src),
        ggplot2::aes(xintercept = value),
        color = plot_point_estimate_color(src, comparison),
        linetype = plot_point_estimate_linetype(src, comparison),
        linewidth = plot_point_estimate_linewidth(src, comparison),
        show.legend = FALSE
      )
    }
  }

  layers
}

#' Repelled text labels for points
#'
#' @param label_data Data frame of labels.
#' @param y Numeric y-position for labels.
#' @param plot Optional plot name for sizing.
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
make_label_repel <- function(label_data, y, plot = NULL) {
  ggrepel::geom_label_repel(
    data = label_data,
    ggplot2::aes(
      x = value,
      y = y,
      label = label,
      color = source
    ),
    size = plot_label_text_size(plot),
    direction = "y",
    force = TRUE,
    hjust = 0.5,
    parse = TRUE,
    seed = 7835,
    show.legend = FALSE
  )
}

# ---------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------

#' Diagnostics line layer
#'
#' @param mapping ggplot aesthetic mapping.
#' @param style Named list of style values.
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
make_diagnostics_line <- function(mapping, style) {
  ggplot2::geom_line(
    mapping = mapping,
    color = style$line$color,
    linewidth = style$line$linewidth,
    linetype = style$line$linetype,
    alpha = style$line$alpha
  )
}

#' Diagnostics point layer
#'
#' @param mapping ggplot aesthetic mapping.
#' @param style Named list of style values.
#'
#' @return ggplot layer.
#'
#' @keywords internal
#' @noRd
make_diagnostics_point <- function(mapping, style) {
  ggplot2::geom_point(
    mapping = mapping,
    color = style$point$color,
    size = style$point$size,
    alpha = style$point$alpha,
    shape = style$point$shape
  )
}
