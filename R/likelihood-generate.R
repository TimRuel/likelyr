# ======================================================================
# likelihood-generate.R
#
# Unified user-facing likelihood generation:
#   generate(cal) → integrate(cal) or profile(cal)
# ======================================================================

#' Generate Likelihood Output (Profile or Integrated)
#'
#' @description
#' After running:
#'   cal <- calibrate(model, data)
#'
#' the user calls:
#'   out <- generate(cal)
#'
#' which dispatches based on `cal$type`:
#'   • "IL" → integrated likelihood
#'   • "PL" → profile likelihood
#'
#' @param cal A calibrated_model.
#' @param ... Additional arguments passed to lower-level engines.
#'
#' @return An integrated_likelihood or profile_likelihood object.
#' @export
generate <- function(cal, ...) {
  UseMethod("generate")
}

# ----------------------------------------------------------------------

#' @export
generate.default <- function(cal, ...) {
  stop("`generate()` requires a calibrated_model created via calibrate().",
       call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
generate.calibrated_model <- function(cal, ...) {

  if (is.null(cal$type)) {
    stop("cal$type is NULL. Did you forget to specify IL or PL?",
         call. = FALSE)
  }

  if (identical(cal$type, "IL")) {
    return(integrate(cal, ...))
  }

  if (identical(cal$type, "PL")) {
    return(profile(cal, ...))
  }

  stop("Unknown calibration type: cal$type = ", deparse(cal$type),
       call. = FALSE)
}
