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
#' @param cal A calibrated model specification.
#'
#' @return The input object with a comparison result attached.
#'
#' @export
compare <- function(cal) {

  validate_compare_input(cal)

  pseudolikelihood_plot <- plot_pseudolikelihoods(cal$results)

  pseudolikelihood_tables <- synthesize_comparison(cal$results)

  comparison <- list(
    tables = pseudolikelihood_tables,
    plot   = pseudolikelihood_plot
  )

  cal$results$comparison <- new_comparison_result(comparison)

  return(cal)
}

# ======================================================================
# Validation
# ======================================================================

#' Validate input for compare()
#'
#' @keywords internal
#' @noRd
validate_compare_input <- function(cal) {

  if (!inherits(cal, "calibrated_model")) {
    stop(
      "compare() requires a calibrated model specification.",
      call. = FALSE
    )
  }

  if (is.null(cal$results$PL) || is.null(cal$results$IL)) {
    stop(
      "compare() requires both profile() and integrate() to have been run.",
      call. = FALSE
    )
  }

  if (is.null(cal$results$PL$inference)) {
    stop(
      "compare() requires infer() to be run on the profile likelihood.",
      call. = FALSE
    )
  }

  if (is.null(cal$results$IL$inference)) {
    stop(
      "compare() requires infer() to be run on the integrated likelihood.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}




