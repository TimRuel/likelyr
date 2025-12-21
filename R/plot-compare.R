# =====================================================================
# plot-compare.R
# Likelihood Comparison Visualization (Profile vs Integrated)
# =====================================================================

#' Plot Likelihood Comparison
#'
#' @description
#' Visualizes a comparison between profile likelihood (PL) and
#' integrated likelihood (IL) results. This plot is intended to
#' *contrast methods*, not to display within-method uncertainty.
#'
#' The input must be a `"likelyr_comparison"` object produced by
#' [compare()].
#'
#' @param x A `likelyr_comparison` object.
#' @param show_intervals Logical; whether to display confidence
#'   interval annotations.
#' @param show_points Logical; whether to display point estimates
#'   (e.g., MLEs, truth).
#' @param ... Additional arguments reserved for future extensions.
#'
#' @return A ggplot object.
#'
#' @export
plot_likelihood_comparison <- function(
    x,
    show_intervals = TRUE,
    show_points    = TRUE,
    ...
) {

  validate_comparison_plot_input(x)

  plot_data <- extract_comparison_plot_data(x)

  p <- build_comparison_plot(
    plot_data     = plot_data,
    show_intervals = show_intervals,
    show_points    = show_points
  )

  invisible(p)
}

# =====================================================================
# Validation
# =====================================================================

#' Validate input for likelihood comparison plotting
#'
#' @keywords internal
#' @noRd
validate_comparison_plot_input <- function(x) {

  if (!inherits(x, "likelyr_comparison")) {
    stop(
      "plot_likelihood_comparison() requires a 'likelyr_comparison' object.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# =====================================================================
# Data Extraction
# =====================================================================

#' Extract plotting data from a comparison object
#'
#' @description
#' Pulls likelihood curve data, point estimates, and interval
#' summaries from a `likelyr_comparison` object and returns them in
#' a standardized structure suitable for plotting.
#'
#' @keywords internal
#' @noRd
extract_comparison_plot_data <- function(x) {

  # Placeholder:
  # - likelihood curves for PL and IL
  # - point estimates (psi_hat, psi_0)
  # - interval summaries (already computed in infer())
  list()
}

# =====================================================================
# Plot Construction
# =====================================================================

#' Build likelihood comparison plot
#'
#' @description
#' Constructs a ggplot object overlaying likelihood curves for
#' profile and integrated likelihoods, with optional annotations.
#'
#' @keywords internal
#' @noRd
build_comparison_plot <- function(
    plot_data,
    show_intervals,
    show_points
) {

  p <- plot_base()

  # -- Likelihood curves (PL vs IL)
  # -- Interval annotations (optional)
  # -- Point estimate annotations (optional)
  # -- Legends and labels

  p
}
