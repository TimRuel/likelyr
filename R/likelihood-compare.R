# ======================================================================
# likelihood-compare.R
#
# Defines compare(), which constructs and attaches a likelihood
# comparison object to a calibrated model specification.
#
# compare() is an orchestrator only. All substantive comparison
# construction is delegated to helpers.
# ======================================================================

#' Compare Integrated and Profile Likelihood Inference
#'
#' @description
#' Orchestrates comparison between profile likelihood (PL) and
#' integrated likelihood (IL) inference results for a calibrated
#' model specification.
#'
#' The input must already have:
#'   • been calibrated via [calibrate()]
#'   • had both [profile()] and [integrate()] applied
#'   • had [infer()] run on *both* likelihood components
#'
#' The result is the same calibrated model spec, augmented with a
#' comparison object of class `"likelyr_comparison"`.
#'
#' @param x A calibrated model specification.
#'
#' @return The input object with a comparison result attached.
#'
#' @export
compare <- function(x) {

  validate_compare_input(x)

  point_estimate_comparison <- render_point_estimate_comparison(x)

  interval_estimate_comparison <- render_interval_estimate_comparison(x)

  estimate_comparison <- render_estimate_comparison(x)

  log_likelihood_comparison_plot <- plot_log_likelihood_comparison(x)

  tables <- list(
    point_estimate_comparison    = point_estimate_comparison,
    interval_estimate_comparison = interval_estimate_comparison,
    estimate_comparison          = estimate_comparison
  )

  comparison <- list(
    tables = tables,
    plot   = log_likelihood_comparison_plot,
  )

  x$comparison <- new_comparison_result(comparison)

  return(x)
}

# ======================================================================
# Validation
# ======================================================================

#' Validate input for compare()
#'
#' @keywords internal
#' @noRd
validate_compare_input <- function(x) {

  if (!inherits(x, "likelyr_calibrated")) {
    stop(
      "compare() requires a calibrated model specification.",
      call. = FALSE
    )
  }

  if (is.null(x$profile) || is.null(x$integrated)) {
    stop(
      "compare() requires both profile() and integrate() to have been run.",
      call. = FALSE
    )
  }

  if (is.null(x$profile$inference)) {
    stop(
      "compare() requires infer() to be run on the profile likelihood.",
      call. = FALSE
    )
  }

  if (is.null(x$integrated$inference)) {
    stop(
      "compare() requires infer() to be run on the integrated likelihood.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}




