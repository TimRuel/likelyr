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
#' Edge points are deliberately excluded (via \code{edge_exclude}) and
#' left to \code{.pad_boundary_by_trend()} instead — a data point sitting
#' hard against a domain boundary can legitimately look like a "local
#' outlier" purely from steep boundary geometry (e.g. Simpson's index
#' near its degenerate lower bound), and that's not the kind of solver
#' hitch this filter is meant to catch.
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
                                     edge_exclude = 8L) {
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

#' Extend a ψ–log-likelihood grid past its endpoints by trend extrapolation
#'
#' @description
#' Fits a local linear trend through the \code{n_fit} points nearest each
#' endpoint and extrapolates it \code{n_pad} grid steps past the edge,
#' producing synthetic "pseudo-points" that extend the series in both
#' directions.
#'
#' This exists to counteract a boundary artifact in
#' \code{stats::smooth.spline()}: natural cubic smoothing splines impose
#' a zero-second-derivative condition at the ends of the fitted range,
#' which fights a genuinely steep rise/fall sitting right at a domain
#' edge (e.g. Simpson's index near its degenerate lower bound
#' \code{1/J}) — producing a visible overshoot-then-correct wiggle even
#' when every underlying point is smooth and monotonic. Extending the
#' series past the true domain moves that artificial boundary condition
#' out to the padding, away from the region anyone will ever evaluate.
#'
#' Deliberately extrapolates a LOCAL LINEAR TREND fit through several
#' nearby points, not a mirror image of the single edge point: an
#' earlier "odd reflection about the boundary point" prototype was too
#' sensitive to that one point's own noise (an incomplete fix on some
#' curves, a real point-estimate shift on others, and a brand-new
#' artifact introduced on a previously clean curve). Fitting a line
#' through several points is far less sensitive to any one of them.
#'
#' @param x Numeric vector of ψ grid values (sorted ascending).
#' @param y Numeric vector of loglik values, same order as \code{x}.
#' @param n_fit Integer number of points nearest each edge used to fit
#'   the local trend line.
#' @param n_pad Integer number of synthetic points to add past each edge.
#'
#' @return A list with \code{x}/\code{y}: the padded points only (left
#'   then right), for the caller to concatenate with the original data.
#'
#' @keywords internal
.pad_boundary_by_trend <- function(x, y, n_fit = 6L, n_pad = 8L) {
  n <- length(x)
  step <- stats::median(diff(x))
  if (!is.finite(step) || step <= 0) {
    step <- 1
  }

  left_df <- data.frame(x = x[1:n_fit], y = y[1:n_fit])
  left_fit <- stats::lm(y ~ x, data = left_df)
  x_left <- x[1] - rev(seq_len(n_pad)) * step
  y_left <- unname(stats::predict(left_fit, newdata = data.frame(x = x_left)))

  right_df <- data.frame(
    x = x[(n - n_fit + 1L):n],
    y = y[(n - n_fit + 1L):n]
  )
  right_fit <- stats::lm(y ~ x, data = right_df)
  x_right <- x[n] + seq_len(n_pad) * step
  y_right <- unname(stats::predict(right_fit, newdata = data.frame(x = x_right)))

  list(x = c(x_left, x_right), y = c(y_left, y_right))
}

#' Fit a smooth log-likelihood function in ψ
#'
#' @description
#' Fits a smoothing spline to a discrete ψ–log-likelihood grid.
#' Optionally projects the result onto its Least Concave Majorant (LCM)
#' to enforce global concavity.
#'
#' The smoothing step is \code{stats::smooth.spline()} — plain GCV
#' smoothing splines, no new dependency — fed a grid that's been
#' pre-treated in two ways (both @keywords internal helpers above):
#' \enumerate{
#'   \item \strong{Isolated interior outliers} (a single grid point that
#'     dips/spikes and the very next point snaps right back — a solver
#'     hitch, not a real feature) get down-weighted to 0 via
#'     \code{.hampel_outlier_weights()}, a LOCAL (windowed) detector.
#'   \item \strong{Domain edges} get padded via
#'     \code{.pad_boundary_by_trend()} so \code{smooth.spline()}'s
#'     natural boundary condition doesn't fight genuine steep curvature
#'     sitting at a closed domain edge.
#' }
#' Both are no-ops on a curve that doesn't need them: a clean interior
#' gets weight 1 everywhere, and padding that merely continues an
#' already-smooth trend doesn't change the fit near the edge.
#'
#' \strong{History (2026-08-14):} \code{smooth.spline()}'s automatic GCV
#' smoothing-parameter selection isn't robust to a single-point solver
#' hitch (can under-smooth around just that point) and has no way to
#' treat a domain edge differently from the interior (the boundary
#' overshoot above). A first fix attempt replaced \code{smooth.spline()}
#' entirely with \code{mgcv::gam(..., method = "REML")}; it was shipped,
#' then reverted the same day — REML's single GLOBAL smoothing parameter
#' pulled every curve toward one flatness level, and on a real 20-site
#' batch that made several genuinely-clean curves visibly WIGGLIER, not
#' smoother, a failure the pre-ship validation script's coarse
#' whole-curve roughness metric didn't catch. A second attempt (global
#' Tukey-biweight IRLS re-weighting) also failed: comparing every point
#' to ONE whole-curve residual scale flagged real curvature as noise in
#' steep regions and left it untouched in flat ones. The current design
#' fixes both failure modes locally instead of changing the global
#' fitting philosophy — \code{smooth.spline()} remains the actual
#' engine throughout, matching prior real-world reliability with it.
#' Validated against all 40 real profile/integrated curves in a
#' Simpson's-index batch (\code{exp_v6}): every known boundary-wiggle
#' curve resolved (roughness reduced 10-80x, near-boundary sign changes
#' collapsed to the single natural turn), the one known single-point
#' interior hitch resolved (roughness reduced ~340x) with only that one
#' point flagged (not its neighbors), and 30/40 curves were completely
#' untouched (bit-for-bit identical fit) — confirmed both by targeted
#' metrics and by direct visual comparison against the raw points, not
#' by an aggregate score alone.
#'
#' When \code{enforce_concavity = TRUE}, the procedure is:
#' \enumerate{
#'   \item Fit \code{smooth.spline} to the (weighted, padded) grid.
#'   \item Evaluate the spline on a fine internal grid (500 points),
#'     restricted to the TRUE ψ range (padding never appears here).
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
  # Step 0: drop non-finite rows, then sort by psi.
  #
  # smooth.spline() errors on NA/NaN/Inf in x or y. Jagged / pathological
  # integrated branches routinely produce non-finite loglik at some grid
  # points (failed solves, -Inf branch values), which would otherwise
  # abort the entire fit. Drop them, preserving the pseudolikelihood
  # attribute (which plain data-frame subsetting would strip). Sorting
  # is required by both the outlier filter and the boundary padding
  # below, which assume points are in psi order.
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

  psi_range <- range(psi_loglik_df$psi)

  # ------------------------------------------------------------------
  # Step 1: down-weight isolated interior outliers + pad domain edges
  # ------------------------------------------------------------------
  n <- nrow(psi_loglik_df)
  n_fit <- min(6L, max(2L, floor(n / 3)))
  n_pad <- min(8L, max(2L, floor(n / 3)))

  weights <- .hampel_outlier_weights(
    psi_loglik_df$loglik,
    edge_exclude = n_pad
  )

  x_fit <- psi_loglik_df$psi
  y_fit <- psi_loglik_df$loglik
  w_fit <- weights

  if (n > 2L * n_fit) {
    padding <- .pad_boundary_by_trend(
      psi_loglik_df$psi,
      psi_loglik_df$loglik,
      n_fit = n_fit,
      n_pad = n_pad
    )
    x_fit <- c(padding$x, x_fit)
    y_fit <- c(padding$y, y_fit)
    w_fit <- c(rep(1, length(padding$x)), weights)
  }

  # ------------------------------------------------------------------
  # Step 2: fit smooth spline to the weighted, padded grid
  # ------------------------------------------------------------------
  psi_loglik_spline <- stats::smooth.spline(
    x = x_fit,
    y = y_fit,
    w = w_fit
  )

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
  # Step 3: evaluate on fine grid (restricted to the TRUE psi range —
  # padding points from Step 1 must never be exposed here)
  # ------------------------------------------------------------------
  psi_fine <- seq(psi_range[1], psi_range[2], length.out = 500L)
  y_fine <- stats::predict(psi_loglik_spline, psi_fine)$y

  # ------------------------------------------------------------------
  # Step 4: project onto LCM via upper convex hull
  # ------------------------------------------------------------------
  hull_idx <- .upper_convex_hull(psi_fine, y_fine)
  psi_lcm <- psi_fine[hull_idx]
  y_lcm <- y_fine[hull_idx]

  # ------------------------------------------------------------------
  # Step 5: linear interpolant through LCM knots
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