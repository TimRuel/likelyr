# =====================================================================
# infer-psi_hat.R — Point estimation from ψ log-likelihood
# =====================================================================

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid and returns
#' a function for evaluating the smoothed log-likelihood at arbitrary ψ.
#'
#' This helper assumes that `psi_loglik_df` represents a *unimodal*
#' log-likelihood curve on a sufficiently dense grid of ψ values.
#'
#' When \code{spar} is \code{NULL} (default), the smoothing parameter is
#' selected automatically. When the discrete maximum lies in the interior
#' of the grid, \code{spar} is chosen to minimise the distance between the
#' spline argmax and the discrete argmax, encouraging the smooth curve to
#' peak in the same location as the data. When the discrete maximum is at
#' or near a boundary (within one grid increment of either edge), this
#' criterion is degenerate, so GCV is used instead.
#'
#' @param psi_loglik_df A data frame containing columns:
#'   \describe{
#'     \item{psi}{Numeric ψ grid values.}
#'     \item{loglik}{Corresponding log-likelihood values.}
#'   }
#' @param spar Optional numeric smoothing parameter passed to
#'   \code{smooth.spline}. When \code{NULL} (default), selected
#'   automatically as described above.
#'
#' @return
#' A function \code{f(psi)} returning the smoothed log-likelihood at
#' \code{psi}, with attributes \code{"pseudolikelihood"},
#' \code{"psi range"}, and \code{"spar"}.
#'
#' @keywords internal
fit_psi_loglik <- function(psi_loglik_df, spar = NULL) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "fit_psi_loglik(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  i_max <- which.max(psi_loglik_df$loglik)
  psi_max_obs <- psi_loglik_df$psi[i_max]

  .fit_spline <- function(s) {
    stats::smooth.spline(
      x = psi_loglik_df$psi,
      y = psi_loglik_df$loglik,
      spar = s
    )
  }

  .spline_argmax <- function(sp) {
    psi_range <- range(sp$x)
    psi_loglik_fn <- function(psi) stats::predict(sp, psi)$y
    stats::optimize(
      f = psi_loglik_fn,
      lower = psi_range[1],
      upper = psi_range[2],
      maximum = TRUE
    )$maximum
  }

  if (is.null(spar)) {
    psi_range <- range(psi_loglik_df$psi)
    grid_increment <- psi_loglik_df$psi[2] - psi_loglik_df$psi[1]
    at_boundary <- (psi_max_obs - psi_range[1]) < grid_increment ||
      (psi_range[2] - psi_max_obs) < grid_increment

    if (at_boundary) {
      # Argmax-alignment criterion is degenerate when the mode is at or
      # near a boundary — use GCV instead.
      psi_loglik_spline <- stats::smooth.spline(
        x = psi_loglik_df$psi,
        y = psi_loglik_df$loglik
      )
      spar <- psi_loglik_spline$spar
    } else {
      opt <- stats::optimize(
        f = function(s) abs(.spline_argmax(.fit_spline(s)) - psi_max_obs),
        lower = 0.4,
        upper = 0.7,
        maximum = FALSE
      )
      spar <- opt$minimum
      psi_loglik_spline <- .fit_spline(spar)
    }
  } else {
    psi_loglik_spline <- .fit_spline(spar)
  }

  psi_loglik <- function(psi) {
    stats::predict(psi_loglik_spline, psi)$y
  }

  attr(psi_loglik, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )
  attr(psi_loglik, "psi range") <- range(psi_loglik_spline$x)
  attr(psi_loglik, "spar") <- spar

  psi_loglik
}

#' Locate the maximizer of a smoothed ψ log-likelihood
#'
#' @description
#' Finds the maximizer and maximum value of a smoothed log-likelihood
#' function over the ψ grid range.
#'
#' @param psi_loglik A function of the form returned by \code{fit_psi_loglik()}.
#'
#' @return
#' A named numeric vector with elements \code{argmax} and \code{maximum}.
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
#' @param psi_loglik_df A data frame containing ordered \code{psi} and
#'   \code{loglik} values.
#'
#' @return
#' A numeric scalar giving the approximate standard error.
#'
#' @keywords internal
get_se_psi_hat <- function(psi_hat, psi_loglik_df) {
  psi_vals <- psi_loglik_df$psi
  loglik_vals <- psi_loglik_df$loglik

  k <- which.min(abs(psi_vals - psi_hat))

  if (k <= 1L || k >= length(psi_vals)) {
    stop(
      "get_se_psi_hat(): psi_hat must lie strictly ",
      "inside the ψ grid to compute curvature-based SE.",
      call. = FALSE
    )
  }

  h <- psi_vals[k + 1L] - psi_vals[k]

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
      ". Log-likelihood may be flat or poorly resolved.",
      call. = FALSE
    )
  }

  1 / sqrt(obs_info)
}

#' Compute point estimate and standard error from a ψ log-likelihood grid
#'
#' @param psi_loglik_df A data frame with columns \code{psi} and \code{loglik}.
#' @param psi_0 Numeric true value of the parameter of interest.
#'
#' @return
#' A tibble with columns \code{psi_0}, \code{psi_hat}, \code{error},
#' and \code{se_psi_hat} (which is \code{NA} when the SE cannot be computed).
#'
#' @keywords internal
get_point_estimate_df <- function(psi_loglik_df, psi_0) {
  psi_loglik <- fit_psi_loglik(psi_loglik_df)
  psi_loglik_max_point <- get_psi_loglik_max_point(psi_loglik)
  psi_hat <- psi_loglik_max_point[["argmax"]]

  se_psi_hat <- tryCatch(
    get_se_psi_hat(psi_hat, psi_loglik_df),
    error = function(e) {
      warning(
        "get_point_estimate_df(): se_psi_hat could not be computed — ",
        conditionMessage(e),
        call. = FALSE
      )
      NA_real_
    }
  )

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