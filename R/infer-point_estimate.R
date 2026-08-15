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

#' Down-weight isolated interior outlier points (Hampel filter)
#'
#' @description
#' For each interior point (excluding \code{edge_exclude} points at
#' either end), compares it to the median of a small window of its
#' immediate neighbors. A point that deviates from that LOCAL median by
#' more than \code{n_sigma} local MADs gets weight 0; everything else
#' gets weight 1.
#'
#' This is local by construction: it can never confuse "the curve bends
#' hard here" with "this one point is wrong", because the reference
#' (median/MAD of a small window) is recomputed fresh at every point
#' rather than compared to one global smooth. That locality matters: an
#' earlier global-residual version (comparing every point to a single
#' whole-curve smoothing spline's residuals) flagged large swaths of
#' genuinely curved-but-clean regions as outliers, because a global
#' residual scale can't tell steep real curvature apart from a solver
#' hitch. See \code{fit_psi_loglik()}'s documentation for the full
#' history.
#'
#' Edge points are deliberately excluded (via \code{edge_exclude}, kept
#' small — just enough to skip the one or two points immediately at a
#' domain boundary): a data point sitting hard against a domain boundary
#' can legitimately look like a "local outlier" purely from steep
#' boundary geometry (e.g. Simpson's index near its degenerate lower
#' bound), which isn't the kind of solver hitch this filter is meant to
#' catch — any remaining edge irregularity that this filter correctly
#' leaves alone is instead handled by concavity enforcement in
#' \code{fit_psi_loglik()} (see its documentation for why that turned
#' out to be a far more robust fix than trying to detect and patch edge
#' artifacts directly).
#'
#' @param y Numeric vector of loglik values, in \code{psi} order.
#' @param half_window Integer half-width (in grid points) of the
#'   comparison window around each point. Default 3.
#' @param n_sigma Numeric threshold multiplier on the local MAD.
#'   Default 3.
#' @param edge_exclude Integer number of points at each end left
#'   unchecked (weight forced to 1).
#'
#' @return Numeric vector of weights (0 or 1), same length as \code{y}.
#'
#' @keywords internal
.hampel_outlier_weights <- function(y, half_window = 3L, n_sigma = 3,
                                     edge_exclude = 3L) {
  n <- length(y)
  w <- rep(1, n)

  if ((edge_exclude * 2L) >= n) {
    return(w) # too short to have a meaningful interior
  }

  for (i in (edge_exclude + 1L):(n - edge_exclude)) {
    lo <- max(1L, i - half_window)
    hi <- min(n, i + half_window)
    window <- y[lo:hi]
    med <- stats::median(window)
    mad_val <- stats::mad(window, center = med)
    if (mad_val < .Machine$double.eps) {
      next # locally flat, nothing to flag
    }
    if (abs(y[i] - med) > n_sigma * mad_val) {
      w[i] <- 0
    }
  }

  w
}

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid, weighted
#' to down-weight isolated interior outliers (see
#' \code{.hampel_outlier_weights()}). By default, projects the result
#' onto its Least Concave Majorant (LCM) to enforce global concavity.
#'
#' \strong{History (2026-08-14/15):} an isolated interior outlier or a
#' domain edge sitting against a closed boundary (e.g. Simpson's index
#' near its degenerate lower bound \code{1/J}) can make
#' \code{stats::smooth.spline()}'s GCV-selected fit dip, flatten, or
#' overshoot locally — visually a "hitch", a "shelf", or a boundary
#' wiggle depending on exactly where and how it happens. SIX fix
#' attempts tried to detect and patch these artifacts directly (in
#' order: replacing \code{smooth.spline()} with \code{mgcv::gam(...,
#' REML)}; global Tukey-biweight IRLS re-weighting; feeding
#' \code{smooth.spline()} synthetic boundary points; blending a
#' monotone interpolant across the edge unconditionally over a
#' fixed-width margin; detecting the artifact via outright sign
#' reversal; blending the interpolant against the spline via a single
#' smoothstep spanning the whole corrected span). Each fixed the
#' specific cases that motivated it and then missed, or was fooled by,
#' the next case: a global correction can't distinguish genuine local
#' curvature from a local defect (attempts 1-3); correcting
#' unconditionally, over a fixed region, or based on a sign check
#' instead of a magnitude check let real problems through or created
#' new ones (attempts 4-6).
#'
#' All of that machinery turned out to be unnecessary. Every one of
#' those artifacts — an outlier dip, a shelf, a boundary wiggle — is
#' geometrically the same thing: a point (or short run of points) lying
#' locally BELOW the chord connecting its neighbors, i.e. a local
#' violation of concavity. Profile and integrated log-likelihoods in a
#' scalar parameter of interest are theoretically single-peaked
#' (concave), so enforcing that isn't an assumption being imposed on
#' the data — it's the shape the object being estimated is already
#' supposed to have. The LCM (\code{enforce_concavity = TRUE}, the
#' default) removes any such violation by construction, with a
#' mathematical guarantee (no detector can be fooled, because there is
#' no detector — the hull just never touches a point below its
#' neighbors' chord), while still touching every point where the data
#' is already concave, getting as close to a real feature (like a
#' domain boundary) as the data supports. Re-validated against all 40
#' real profile/integrated curves from the Simpson's-index batch that
#' drove the six earlier attempts (\code{exp_v6}): every curve came out
#' globally concave, \code{psi_hat} matched the elaborate six-attempt
#' pipeline to within 0.0003 everywhere, and several curves that
#' pipeline never fully resolved (a residual wobble the last attempt
#' left behind) came out completely clean under plain LCM projection —
#' confirmed by direct visual comparison, not just an aggregate score.
#'
#' When \code{enforce_concavity = FALSE}, only the Hampel-weighted
#' smoothing spline is fitted and returned directly, without LCM
#' projection — no protection against edge artifacts in this mode; use
#' it only when a non-concave-enforced view is specifically wanted.
#'
#' @param psi_loglik_df A data frame containing columns:
#'   \describe{
#'     \item{psi}{Numeric ψ grid values.}
#'     \item{loglik}{Corresponding log-likelihood values.}
#'   }
#' @param enforce_concavity Logical. Whether to project the spline onto
#'   its LCM to enforce global concavity. Default: \code{TRUE}.
#'
#' @return
#' A function \code{f(psi)} returning the smoothed log-likelihood at
#' \code{psi}, optionally concavity-corrected.
#'
#' @keywords internal
fit_psi_loglik <- function(psi_loglik_df, enforce_concavity = TRUE) {
  required <- c("psi", "loglik")
  if (!all(required %in% names(psi_loglik_df))) {
    stop(
      "fit_psi_loglik(): psi_loglik_df must contain columns ",
      paste(shQuote(required), collapse = ", "),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Step 0: drop non-finite rows, then sort by psi.
  #
  # smooth.spline() errors on NA/NaN/Inf in x or y. Jagged / pathological
  # integrated branches routinely produce non-finite loglik at some grid
  # points (failed solves, -Inf branch values), which would otherwise
  # abort the entire fit. Drop them, preserving the pseudolikelihood
  # attribute (which plain data-frame subsetting would strip). Sorting
  # is required by the outlier filter, which assumes points are in psi
  # order.
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

  psi_order <- order(psi_loglik_df$psi)
  psi_loglik_df <- psi_loglik_df[psi_order, , drop = FALSE]
  attr(psi_loglik_df, "pseudolikelihood") <- pseudolik_attr

  x <- psi_loglik_df$psi
  y <- psi_loglik_df$loglik
  psi_range <- range(x)

  # ------------------------------------------------------------------
  # Step 1: down-weight isolated interior outliers (Hampel filter),
  # then fit smooth.spline to the weighted grid.
  # ------------------------------------------------------------------
  weights <- .hampel_outlier_weights(y)
  psi_loglik_spline <- stats::smooth.spline(x = x, y = y, w = weights)

  if (!enforce_concavity) {
    psi_loglik <- function(psi) stats::predict(psi_loglik_spline, psi)$y
    attr(psi_loglik, "pseudolikelihood") <- attr(
      psi_loglik_df,
      "pseudolikelihood"
    )
    attr(psi_loglik, "psi range") <- psi_range
    return(psi_loglik)
  }

  # ------------------------------------------------------------------
  # Step 2: evaluate on a fine grid over the TRUE psi range
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
#'   Default: \code{TRUE}.
#'
#' @return
#' A tibble with columns \code{psi_0}, \code{psi_hat}, \code{error},
#' and \code{se_psi_hat}.
#'
#' @keywords internal
get_point_estimate_df <- function(
  psi_loglik_df,
  psi_0,
  enforce_concavity = TRUE
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