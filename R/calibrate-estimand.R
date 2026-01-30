# ======================================================================
# Estimand Calibration
# ======================================================================

#' Calibrate Estimand Component
#'
#' @description
#' Binds data into psi_fn(), computes psi_MLE and (optionally) psi_0,
#' and evaluates the search interval.
#'
#' If psi_0 is already present on the estimand_spec (e.g. supplied
#' directly by the user or injected earlier), it is respected and
#' not recomputed.
#'
#' @param estimand An estimand_spec object.
#' @param data User data.
#' @param param_mle Numeric vector; MLE of param.
#' @param param_0 Optional numeric vector; true parameter value.
#'
#' @return The same estimand_spec object, enriched with:
#'   • psi_fn (data-bound)
#'   • psi_jac (data-bound if present)
#'   • psi_mle
#'   • psi_0 (if available)
#'   • search_interval
#'
#' @keywords internal
calibrate_estimand <- function(estimand, data, param_mle, param_0 = NULL) {
  stopifnot(inherits(estimand, "estimand_spec"))

  # -------------------------------------------------------------
  # 1. Bind ψ(θ, data)
  # -------------------------------------------------------------
  orig_psi_fn <- estimand$psi_fn
  estimand$psi_fn <- function(param) {
    orig_psi_fn(param, data)
  }

  # -------------------------------------------------------------
  # 2. Bind Jacobian if present
  # -------------------------------------------------------------
  if (!is.null(estimand$psi_jac)) {
    orig_psi_jac <- estimand$psi_jac
    estimand$psi_jac <- function(param) {
      orig_psi_jac(param, data)
    }
  }

  # -------------------------------------------------------------
  # 3. Compute ψ̂_MLE
  # -------------------------------------------------------------
  psi_mle <- estimand$psi_fn(param_mle)

  if (!is.numeric(psi_mle) || length(psi_mle) != 1L || !is.finite(psi_mle)) {
    stop("psi_fn(param_mle) must return a finite scalar.", call. = FALSE)
  }

  estimand$psi_mle <- psi_mle

  # -------------------------------------------------------------
  # 4. Set ψ₀ (respect existing value)
  # -------------------------------------------------------------
  if (is.null(estimand$psi_0) && !is.null(param_0)) {
    psi_0 <- estimand$psi_fn(param_0)

    if (!is.numeric(psi_0) || length(psi_0) != 1L || !is.finite(psi_0)) {
      stop("psi_fn(param_0) must return a finite scalar.", call. = FALSE)
    }

    estimand$psi_0 <- psi_0
  }

  # -------------------------------------------------------------
  # 5. Compute search interval
  # -------------------------------------------------------------
  si <- estimand$search_interval_fn(param_mle, data)

  if (
    !is.numeric(si) ||
      length(si) != 2L ||
      any(!is.finite(si)) ||
      si[1] >= si[2]
  ) {
    stop(
      "search_interval_fn(param_mle, data) must return c(lower, upper) with finite lower < upper.",
      call. = FALSE
    )
  }

  estimand$search_interval <- si

  estimand
}
