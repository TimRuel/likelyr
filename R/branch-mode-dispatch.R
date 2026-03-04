# ======================================================================
# branch-mode-dispatch.R — Branch Mode Locator Dispatcher
# ======================================================================
#
# Provides:
#   • get_branch_mode_locator() — internal dispatcher mapping method names
#     to concrete branch mode locator implementations.
#
# Design notes:
#   • Method names are validated upstream in optimizer_spec() via match.arg()
#   • This dispatcher performs NO validation or calibration
#   • All returned locators are treated as black boxes downstream
# ======================================================================

#' Resolve Branch Mode Locator Method
#'
#' @description
#' Internal dispatcher that resolves a validated branch mode locator
#' method name to a concrete branch mode locator implementation.
#'
#' The returned object is a function with signature:
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
#' The internal algorithm used to locate the branch mode depends on the
#' selected method but is treated as a black box by downstream code.
#'
#' @param method
#'   Character scalar specifying the branch mode locator method.
#'   Must already be validated and normalized by \code{optimizer_spec()}.
#'
#' @return
#' A branch mode locator function.
#'
#' @keywords internal
#' @noRd
get_branch_mode_locator <- function(method) {
  locator_factory <- switch(
    method,
    hybrid = branch_mode_locator_hybrid,
    grid_scan = branch_mode_locator_grid_scan,
    brent = branch_mode_locator_brent,
    multiplier_root = branch_mode_locator_multiplier_root,
    stop(
      "Unknown branch mode locator method: ",
      method,
      call. = FALSE
    )
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
