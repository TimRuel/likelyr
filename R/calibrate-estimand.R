# ======================================================================
# Estimand Calibration (v2.0)
#
# Changes from v1.2:
#   • search_interval_fn and search_interval computation removed —
#     these now live in calibrate_pipeline().
#   • Reduced to three responsibilities: bind data, compute psi_mle,
#     set psi_0 if available.
# ======================================================================

#' Calibrate Estimand Component
#'
#' @description
#' Binds data into \code{psi_fn()} and \code{psi_jac()}, computes
#' \code{psi_mle}, and optionally sets \code{psi_0} from \code{param_0}.
#'
#' If \code{psi_0} is already present on the \code{estimand_spec}
#' (supplied directly by the user), it is respected and not recomputed.
#'
#' @param estimand  An \code{estimand_spec} object.
#' @param data      User data.
#' @param param_mle Numeric vector; MLE of param.
#' @param param_0   Optional numeric vector; true parameter value.
#'
#' @return The same \code{estimand_spec} object, enriched with:
#'   \itemize{
#'     \item \code{$psi_fn}  — data-bound closure
#'     \item \code{$psi_jac} — data-bound closure (if present)
#'     \item \code{$psi_mle}
#'     \item \code{$psi_0}   — if available
#'   }
#'
#' @keywords internal
calibrate_estimand <- function(estimand, data, param_mle, param_0 = NULL) {
  stopifnot(inherits(estimand, "estimand_spec"))

  psi_lower <- estimand$psi_lower
  psi_upper <- estimand$psi_upper

  # -------------------------------------------------------------------
  # 1. Bind data to psi_fn
  # -------------------------------------------------------------------
  orig_psi_fn <- estimand$psi_fn
  estimand$psi_fn <- function(param) orig_psi_fn(param, data)

  # -------------------------------------------------------------------
  # 2. Bind data to psi_jac (if present)
  # -------------------------------------------------------------------
  if (!is.null(estimand$psi_jac)) {
    orig_psi_jac <- estimand$psi_jac
    estimand$psi_jac <- function(param) orig_psi_jac(param, data)
  }

  # -------------------------------------------------------------------
  # 3. Compute psi_mle
  # -------------------------------------------------------------------
  psi_mle <- estimand$psi_fn(param_mle)

  if (!is.numeric(psi_mle) || length(psi_mle) != 1L || !is.finite(psi_mle)) {
    stop("psi_fn(param_mle) must return a finite scalar.", call. = FALSE)
  }

  if (!is.null(psi_lower) && psi_mle < psi_lower) {
    stop("Computed psi_mle lies below psi_lower.", call. = FALSE)
  }

  if (!is.null(psi_upper) && psi_mle > psi_upper) {
    stop("Computed psi_mle lies above psi_upper.", call. = FALSE)
  }

  estimand$psi_mle <- psi_mle

  # -------------------------------------------------------------------
  # 4. Set psi_0 (respect existing value if already supplied by user)
  # -------------------------------------------------------------------
  if (is.null(estimand$psi_0) && !is.null(param_0)) {
    psi_0 <- estimand$psi_fn(param_0)

    if (!is.numeric(psi_0) || length(psi_0) != 1L || !is.finite(psi_0)) {
      stop("psi_fn(param_0) must return a finite scalar.", call. = FALSE)
    }

    if (!is.null(psi_lower) && psi_0 < psi_lower) {
      stop("Computed psi_0 lies below psi_lower.", call. = FALSE)
    }

    if (!is.null(psi_upper) && psi_0 > psi_upper) {
      stop("Computed psi_0 lies above psi_upper.", call. = FALSE)
    }

    estimand$psi_0 <- psi_0
  }

  estimand
}
