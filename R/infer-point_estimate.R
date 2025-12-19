# =====================================================================
# infer-point_estimate.R — Point estimation from ψ log-likelihood
# =====================================================================

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid and returns
#' a function for evaluating the smoothed log-likelihood at arbitrary ψ.
#'
#' This helper assumes that `psi_ll_df` represents a *unimodal* log-likelihood
#' curve on a sufficiently dense grid.
#'
#' @param psi_ll_df A data frame containing columns:
#'   \describe{
#'     \item{psi}{Numeric ψ grid values.}
#'     \item{loglik}{Corresponding log-likelihood values.}
#'   }
#'
#' @return
#' A function `f(psi)` returning the smoothed log-likelihood at `psi`.
#'
#' @keywords internal
fit_psi_ll_fn <- function(psi_ll_df) {

  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_ll_df))) {
    stop(
      "fit_psi_ll_fn(): psi_ll_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  psi_ll_spline <- stats::smooth.spline(
    x = psi_ll_df$psi,
    y = psi_ll_df$loglik
  )

  function(psi) {
    stats::predict(psi_ll_spline, psi)$y
  }
}

#' Locate the maximizer of a smoothed ψ log-likelihood
#'
#' @description
#' Finds the maximizer and maximum value of a smoothed log-likelihood
#' function over the ψ grid range.
#'
#' @param psi_ll_fn A function of the form returned by `fit_psi_ll_fn()`.
#' @param psi_ll_df A data frame containing a numeric `psi` column defining
#'   the optimization bounds.
#'
#' @return
#' A tibble with columns:
#'   \describe{
#'     \item{argmax}{The ψ value maximizing the log-likelihood.}
#'     \item{maximum}{The corresponding maximum log-likelihood value.}
#'   }
#'
#' @keywords internal
get_psi_ll_max_point <- function(psi_ll_fn, psi_ll_df) {

  opt <- stats::optimize(
    f       = psi_ll_fn,
    lower   = min(psi_ll_df$psi),
    upper   = max(psi_ll_df$psi),
    maximum = TRUE
  )

  tibble::tibble(
    argmax  = opt$maximum,
    maximum = opt$objective
  )
}

#' Compute a curvature-based standard error at the ψ MLE
#'
#' @description
#' Approximates the standard error of a ψ point estimate using the observed
#' curvature of the log-likelihood evaluated on a discrete grid.
#'
#' This uses a central finite-difference approximation to the second
#' derivative and is therefore sensitive to grid resolution and smoothness.
#'
#' @param point_estimate Numeric ψ value (typically the MLE).
#' @param psi_ll_df A data frame containing ordered `psi` and `loglik` values.
#'
#' @return
#' A numeric scalar giving the approximate standard error.
#'
#' @keywords internal
get_se_point_estimate <- function(point_estimate, psi_ll_df) {

  psi_vals <- psi_ll_df$psi
  ll_vals  <- psi_ll_df$loglik

  # Index of grid point closest to the estimate
  k <- which.min(abs(psi_vals - point_estimate))

  if (k <= 1L || k >= length(psi_vals)) {
    stop(
      "get_se_point_estimate(): point_estimate must lie strictly ",
      "inside the ψ grid to compute curvature-based SE.",
      call. = FALSE
    )
  }

  # Grid spacing (assumed locally regular)
  h <- psi_vals[k + 1L] - psi_vals[k]

  # Second derivative via central difference
  second_deriv <- (
    ll_vals[k + 1L] -
      2 * ll_vals[k] +
      ll_vals[k - 1L]
  ) / h^2

  obs_info <- -second_deriv

  if (!is.finite(obs_info) || obs_info <= 0) {
    stop(
      "get_se_point_estimate(): non-positive observed information ",
      "at ψ = ", format(point_estimate),
      ". Log-Likelihood may be flat or poorly resolved.",
      call. = FALSE
    )
  }

  1 / sqrt(obs_info)
}
