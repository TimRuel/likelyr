# ======================================================================
# Estimand Calibration (v1.2) — bounded ψ support
# ======================================================================

#' Calibrate Estimand Component
#'
#' @description
#' Binds data into psi_fn(), computes psi_MLE and (optionally) psi_0,
#' evaluates the search interval, and enforces geometric bounds on ψ.
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
#'   • search_interval (clipped to ψ bounds)
#'
#' @keywords internal
calibrate_estimand <- function(estimand, data, param_mle, param_0 = NULL) {
  stopifnot(inherits(estimand, "estimand_spec"))

  psi_lower <- estimand$psi_lower
  psi_upper <- estimand$psi_upper

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

  # Enforce ψ bounds at the MLE
  if (!is.null(psi_lower) && psi_mle < psi_lower) {
    stop("Computed psi_mle lies below psi_lower.", call. = FALSE)
  }
  if (!is.null(psi_upper) && psi_mle > psi_upper) {
    stop("Computed psi_mle lies above psi_upper.", call. = FALSE)
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

    # Enforce ψ bounds for ψ₀ as well
    if (!is.null(psi_lower) && psi_0 < psi_lower) {
      stop("Computed psi_0 lies below psi_lower.", call. = FALSE)
    }
    if (!is.null(psi_upper) && psi_0 > psi_upper) {
      stop("Computed psi_0 lies above psi_upper.", call. = FALSE)
    }

    estimand$psi_0 <- psi_0
  }

  # -------------------------------------------------------------
  # 5. Compute and clip search interval
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

  # Clip to ψ bounds if present
  if (!is.null(psi_lower)) {
    si[1] <- max(si[1], psi_lower)
  }
  if (!is.null(psi_upper)) {
    si[2] <- min(si[2], psi_upper)
  }

  if (si[1] >= si[2]) {
    stop(
      "Search interval collapses after applying ψ bounds.",
      call. = FALSE
    )
  }

  estimand$search_interval <- si

  estimand
}
