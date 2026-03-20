# ======================================================================
# branch-mode-dispatch.R — Branch Mode Locator Dispatcher
#
# Resolves a method name to a concrete branch mode locator factory.
#
# To add a new method:
#   1. Create branch-mode-<name>.R implementing branch_mode_locator_<name>()
#   2. Add a case to the switch() below
#   3. Register the method string in pipeline_spec() validation
# ======================================================================

#' Resolve Branch Mode Locator Method
#'
#' @description
#' Maps a validated method name to its branch mode locator factory.
#' Downstream code treats all locators as a black box with the contract:
#'
#' \preformatted{
#'   function(omega_hat) -> list(
#'     psi_hat,
#'     param_hat,
#'     loglik_at_mode,
#'     status
#'   )
#' }
#'
#' @param method Character scalar. Currently supported: \code{"bracket_gss"}.
#'
#' @return A branch mode locator factory function.
#'
#' @keywords internal
#' @noRd
get_branch_mode_locator <- function(method) {
  locator_factory <- switch(
    method,
    bracket_gss = branch_mode_locator_bracket_gss,
    stop("Unknown branch mode locator method: '", method, "'.", call. = FALSE)
  )

  if (!is.function(locator_factory)) {
    stop(
      "Branch mode locator '",
      method,
      "' did not resolve to a function.",
      call. = FALSE
    )
  }

  locator_factory
}
