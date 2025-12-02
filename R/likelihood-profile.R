# ======================================================================
# likelihood-profile.R
#
# Profile likelihood engine (placeholder)
# ======================================================================

#' Profile Likelihood
#'
#' @description
#' Placeholder function. The profile likelihood engine is not yet
#' implemented. Calling this function will throw an informative error.
#'
#' @param cal A calibrated_model.
#' @export
profile <- function(cal, ...) {
  UseMethod("profile")
}

# ----------------------------------------------------------------------

#' @export
profile.default <- function(cal, ...) {
  stop("profile() requires a calibrated_model.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
profile.calibrated_model <- function(cal, ...) {
  stop("Profile likelihood is not yet implemented.", call. = FALSE)
}
