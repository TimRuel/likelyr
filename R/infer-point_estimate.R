# =====================================================================
# infer-point_estimate.R — Point estimation from ψ log-likelihood
# =====================================================================

#' Compute the upper convex hull indices of a planar point sequence
#'
#' @description
#' Given a sequence of points (x, y) sorted by x, returns the indices
#' of the points forming the upper convex hull — the piecewise linear
#' concave envelope lying at or above all other points. This is the
#' discrete Least Concave Majorant (LCM).
#'
#' Uses the standard cross-product stack algorithm: a point is removed
#' from the hull when adding the next point would create a left turn
#' (convex kink), which would violate the concavity of the upper envelope.
#'
#' @param x Numeric vector of x-coordinates (must be sorted ascending).
#' @param y Numeric vector of y-coordinates, same length as x.
#'
#' @return Integer vector of indices into x/y forming the upper hull,
#'   always including the first and last points.
#'
#' @keywords internal
.upper_convex_hull <- function(x, y) {
  n <- length(x)
  if (n < 2L) {
    return(seq_len(n))
  }

  hull <- integer(n)
  size <- 0L

  for (i in seq_len(n)) {
    while (size >= 2L) {
      o <- hull[size - 1L]
      a <- hull[size]
      cp <- (x[a] - x[o]) * (y[i] - y[o]) - (y[a] - y[o]) * (x[i] - x[o])
      if (cp >= 0) {
        size <- size - 1L
      } else {
        break
      }
    }
    size <- size + 1L
    hull[size] <- i
  }

  hull[seq_len(size)]
}

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid.
#' Optionally projects the result onto its Least Concave Majorant (LCM)
#' to enforce global concavity.
#'
#' When \code{enforce_concavity = TRUE}, the procedure is:
#' \enumerate{
#'   \item Fit \code{smooth.spline} to the raw grid.
#'   \item Evaluate the spline on a fine internal grid (500 points).
#'   \item Compute the upper convex hull of the fine-grid evaluations —
#'     the tightest concave piecewise-linear function lying at or above
#'     the spline. This is the LCM.
#'   \item Return a linear interpolant through the LCM knots.
#' }
#'
#' When \code{enforce_concavity = FALSE} (default), only the smoothing
#' spline is fitted and returned directly, without LCM projection.
#'
#' @param psi_loglik_df A data frame containing columns:
#'   \describe{
#'     \item{psi}{Numeric ψ grid values.}
#'     \item{loglik}{Corresponding log-likelihood values.}
#'   }
#' @param enforce_concavity Logical. Whether to project the spline onto
#'   its LCM to enforce global concavity. Default: \code{FALSE}.
#'
#' @return
#' A function \code{f(psi)} returning the smoothed log-likelihood at
#' \code{psi}, optionally concavity-corrected.
#'
#' @keywords internal
fit_psi_loglik <- function(psi_loglik_df, enforce_concavity = FALSE) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "fit_psi_loglik(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Step 0: drop non-finite rows.
  #
  # smooth.spline() errors on NA/NaN/Inf in x or y. Jagged / pathological
  # integrated branches routinely produce non-finite loglik at some grid
  # points (failed solves, -Inf branch values), which would otherwise
  # abort the entire fit. Drop them, preserving the pseudolikelihood
  # attribute (which plain data-frame subsetting would strip).
  # ------------------------------------------------------------------
  pseudolik_attr <- attr(psi_loglik_df, "pseudolikelihood")

  finite_rows <- is.finite(psi_loglik_df$psi) &
    is.finite(psi_loglik_df$loglik)

  if (!all(finite_rows)) {
    n_dropped <- sum(!finite_rows)
    warning(
      "fit_psi_loglik(): dropping ",
      n_dropped,
      " non-finite (psi, loglik) row",
      if (n_dropped > 1L) "s" else "",
      " before spline fit.",
      call. = FALSE
    )
    psi_loglik_df <- psi_loglik_df[finite_rows, , drop = FALSE]
    attr(psi_loglik_df, "pseudolikelihood") <- pseudolik_attr
  }

  if (nrow(psi_loglik_df) < 4L) {
    stop(
      "fit_psi_loglik(): fewer than 4 finite (psi, loglik) points remain ",
      "after filtering; cannot fit a smoothing spline.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Step 1: fit smooth spline to raw grid
  # ------------------------------------------------------------------
  psi_loglik_spline <- stats::smooth.spline(
    x = psi_loglik_df$psi,
    y = psi_loglik_df$loglik
  )

  psi_range <- range(psi_loglik_spline$x)

  if (!enforce_concavity) {
    psi_loglik <- function(psi) {
      stats::predict(psi_loglik_spline, psi)$y
    }
    attr(psi_loglik, "pseudolikelihood") <- attr(
      psi_loglik_df,
      "pseudolikelihood"
    )
    attr(psi_loglik, "psi range") <- psi_range
    return(psi_loglik)
  }

  # ------------------------------------------------------------------
  # Step 2: evaluate on fine grid
  # ------------------------------------------------------------------
  psi_fine <- seq(psi_range[1], psi_range[2], length.out = 500L)
  y_fine <- stats::predict(psi_loglik_spline, psi_fine)$y

  # ------------------------------------------------------------------
  # Step 3: project onto LCM via upper convex hull
  # ------------------------------------------------------------------
  hull_idx <- .upper_convex_hull(psi_fine, y_fine)
  psi_lcm <- psi_fine[hull_idx]
  y_lcm <- y_fine[hull_idx]

  # ------------------------------------------------------------------
  # Step 4: linear interpolant through LCM knots
  # ------------------------------------------------------------------
  lcm_interp <- stats::approxfun(
    psi_lcm,
    y_lcm,
    method = "linear",
    rule = 2L
  )

  psi_loglik <- function(psi) lcm_interp(psi)

  attr(psi_loglik, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )
  attr(psi_loglik, "psi range") <- psi_range

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
#' @param enforce_concavity Logical. Passed to \code{fit_psi_loglik()}.
#'   Default: \code{FALSE}.
#'
#' @return
#' A tibble with columns \code{psi_0}, \code{psi_hat}, \code{error},
#' and \code{se_psi_hat}.
#'
#' @keywords internal
get_point_estimate_df <- function(
  psi_loglik_df,
  psi_0,
  enforce_concavity = FALSE
) {
  psi_loglik <- fit_psi_loglik(psi_loglik_df, enforce_concavity = enforce_concavity)
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