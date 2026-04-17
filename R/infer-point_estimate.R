# =====================================================================
# infer-psi_hat.R — Point estimation from ψ log-likelihood
# =====================================================================

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid and returns
#' a function for evaluating the smoothed log-likelihood at arbitrary ψ.
#'
#' This helper assumes that `psi_loglik_df` represents a *unimodal* log-likelihood
#' curve on a sufficiently dense grid of psi values.
#'
#' @param psi_loglik_df A data frame containing columns:
#'   \describe{
#'     \item{psi}{Numeric ψ grid values.}
#'     \item{loglik}{Corresponding log-likelihood values.}
#'   }
#'
#' @return
#' A function `f(psi)` returning the smoothed log-likelihood at `psi`.
#'
#' @keywords internal
fit_psi_loglik <- function(psi_loglik_df) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "fit_psi_loglik(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  psi_loglik_spline <- stats::smooth.spline(
    x = psi_loglik_df$psi,
    y = psi_loglik_df$loglik
  )

  psi_loglik <- function(psi) {
    stats::predict(psi_loglik_spline, psi)$y
  }

  attr(psi_loglik, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )
  attr(psi_loglik, "psi range") <- range(psi_loglik_spline$x)

  psi_loglik
}

#' Locate the maximizer of a smoothed ψ log-likelihood
#'
#' @description
#' Finds the maximizer and maximum value of a smoothed log-likelihood
#' function over the ψ grid range.
#'
#' @param psi_loglik A function of the form returned by `fit_psi_loglik()`.
#'
#' @return
#' A tibble with columns:
#'   \describe{
#'     \item{argmax}{The ψ value maximizing the log-likelihood.}
#'     \item{maximum}{The corresponding maximum log-likelihood value.}
#'   }
#'
#' @keywords internal
get_psi_loglik_max_point <- function(psi_loglik) {
  psi_range <- attr(psi_loglik, "psi range")

  opt <- stats::optimize(
    f = psi_loglik,
    lower = min(psi_range),
    upper = max(psi_range),
    maximum = TRUE
  )

  c(argmax = opt$maximum, maximum = opt$objective)
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
#' @param psi_hat Numeric ψ value (typically the MLE).
#' @param psi_loglik_df A data frame containing ordered `psi` and `loglik` values.
#'
#' @return
#' A numeric scalar giving the approximate standard error.
#'
#' @keywords internal
get_se_psi_hat <- function(psi_hat, psi_loglik_df) {
  psi_vals <- psi_loglik_df$psi
  loglik_vals <- psi_loglik_df$loglik

  # Index of grid point closest to the estimate
  k <- which.min(abs(psi_vals - psi_hat))

  if (k <= 1L || k >= length(psi_vals)) {
    stop(
      "get_se_psi_hat(): psi_hat must lie strictly ",
      "inside the ψ grid to compute curvature-based SE.",
      call. = FALSE
    )
  }

  # Grid spacing (assumed locally regular)
  h <- psi_vals[k + 1L] - psi_vals[k]

  # Second derivative via central difference
  second_deriv <- (loglik_vals[k + 1L] -
    2 * loglik_vals[k] +
    loglik_vals[k - 1L]) /
    h^2

  obs_info <- -second_deriv

  if (!is.finite(obs_info) || obs_info <= 0) {
    stop(
      "get_se_psi_hat(): non-positive observed information ",
      "at ψ = ",
      format(psi_hat),
      ". Log-Likelihood may be flat or poorly resolved.",
      call. = FALSE
    )
  }

  1 / sqrt(obs_info)
}

get_point_estimate_df <- function(psi_loglik_df, psi_0) {
  psi_loglik <- fit_psi_loglik(psi_loglik_df)
  psi_loglik_max_point <- get_psi_loglik_max_point(psi_loglik)
  psi_hat <- psi_loglik_max_point[["argmax"]]

  se_psi_hat <- get_se_psi_hat(psi_hat, psi_loglik_df)

  point_estimate_df <- tibble::tibble(
    psi_0 = psi_0,
    psi_hat = psi_hat,
    error = psi_hat - psi_0,
    se_psi_hat = se_psi_hat
  )

  attr(point_estimate_df, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )

  point_estimate_df
}
