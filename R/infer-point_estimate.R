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
#' domain boundary) and left to the edge-blend logic in
#' \code{fit_psi_loglik()} instead: a data point sitting hard against a
#' domain boundary can legitimately look like a "local outlier" purely
#' from steep boundary geometry (e.g. Simpson's index near its
#' degenerate lower bound), which isn't the kind of solver hitch this
#' filter is meant to catch. \code{edge_exclude} is deliberately much
#' smaller than the edge-blend margin below — shrinking it from an
#' earlier, wider default was necessary after it was found to also
#' exclude a genuine single-point interior hitch sitting close enough to
#' an edge to fall inside the wider exclusion zone.
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

#' Smoothstep interpolation weight
#'
#' @description
#' The standard cubic smoothstep \code{3t^2 - 2t^3}, clamped to
#' \code{[0, 1]}: 0 and 1 at the endpoints with zero slope there, so
#' blending two functions with this weight introduces no kink at either
#' end of the transition.
#'
#' @param t Numeric vector.
#'
#' @return Numeric vector of the same length as \code{t}, in \code{[0, 1]}.
#'
#' @keywords internal
.smoothstep <- function(t) {
  t <- pmin(pmax(t, 0), 1)
  3 * t^2 - 2 * t^3
}

#' Detect how far a fitted spline's own slope re-accelerates near an edge
#'
#' @description
#' Evaluates the fitted spline on a moderate sub-grid running out from
#' one end of the domain and looks for a genuine RE-ACCELERATION in its
#' own slope sequence (a later slope at least \code{tol} times larger in
#' magnitude than the one before it, same direction) — the signature of
#' a "shelf": the curve briefly flattens, then resumes climbing, without
#' ever reversing direction outright. Returns the x-coordinate just past
#' the last such re-acceleration, i.e. exactly how far the artifact
#' extends, rather than assuming a fixed margin.
#'
#' This exists because \code{stats::smooth.spline()} imposes a
#' zero-second-derivative condition at the ends of the fitted range,
#' which can fight genuinely steep curvature sitting right at a closed
#' domain edge (e.g. Simpson's index near its degenerate lower bound
#' \code{1/J}). The resulting artifact doesn't always cross zero (a
#' plain sign-reversal check misses it) — it can just visibly flatten
#' before continuing to rise, which is what this detector targets.
#'
#' Deliberately checks the FITTED spline's own slopes, not the raw
#' data's: the raw grid frequently has enough ordinary point-to-point
#' sampling noise to trip a slope-ratio check on its own (tried and
#' rejected — flagged curves, mostly profile curves, that the actual fit
#' already handled fine). Tying the check to the fitted curve itself
#' means it only fires when there is a visible symptom to fix.
#'
#' @param x Numeric vector of ψ grid values (sorted ascending), from the
#'   edge being checked outward (i.e. already reversed for a right edge).
#' @param spline_at Function evaluating the fitted spline at arbitrary ψ.
#' @param max_check_x Numeric ψ value bounding how far out to look.
#' @param tol Numeric re-acceleration ratio threshold. Default 1.15.
#' @param n_grid Integer number of sub-grid points to evaluate.
#'
#' @return Numeric ψ value marking the end of the artifact, or \code{NA}
#'   if no re-acceleration was found (no correction needed).
#'
#' @keywords internal
.find_fit_instability_extent <- function(x, spline_at, max_check_x,
                                          tol = 1.15, n_grid = 40L) {
  xf <- seq(x[1], max_check_x, length.out = n_grid)
  yf <- spline_at(xf)
  slopes <- diff(yf) / diff(xf)
  ratios <- slopes[-1] / slopes[-length(slopes)]
  anomalous <- abs(ratios) >= tol & sign(slopes[-1]) == sign(slopes[-length(slopes)])

  if (!any(anomalous)) {
    return(NA_real_)
  }
  last_bad <- max(which(anomalous))
  idx <- min(last_bad + 3L, n_grid) # small buffer past the last re-acceleration
  xf[idx]
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
#'   \item \strong{Domain edges} get a local correction ONLY where the
#'     fitted spline's own slope sequence shows a genuine
#'     RE-ACCELERATION near the edge (\code{.find_fit_instability_extent()})
#'     — the signature of a "shelf": the curve briefly flattens, then
#'     resumes climbing, without ever reversing direction outright (so a
#'     plain sign-reversal check misses it). Over exactly that
#'     (data-driven, not fixed-width) extent, the raw points are trusted
#'     directly via a monotone Hermite interpolant
#'     (\code{stats::splinefun(..., method = "monoH.FC")}),
#'     smoothstep-blended into the spline's own prediction so there's no
#'     seam.
#' }
#' Both are no-ops on a curve that doesn't need them: a clean interior
#' gets weight 1 everywhere, and an edge with no detected re-acceleration
#' is left as pure \code{smooth.spline()} output, untouched.
#'
#' \strong{History (2026-08-14):} \code{smooth.spline()}'s automatic GCV
#' smoothing-parameter selection isn't robust to a single-point solver
#' hitch (can under-smooth around just that point) and has no way to
#' treat a domain edge differently from the interior (the boundary
#' overshoot above). Five fix attempts were tried and rejected before
#' reaching the current design:
#' \enumerate{
#'   \item Replacing \code{smooth.spline()} entirely with
#'     \code{mgcv::gam(..., method = "REML")}. Shipped, then reverted
#'     the same day — REML's single GLOBAL smoothing parameter pulled
#'     every curve toward one flatness level, and on a real 20-site
#'     batch that made several genuinely-clean curves visibly WIGGLIER,
#'     not smoother, a failure the pre-ship validation script's coarse
#'     whole-curve roughness metric didn't catch.
#'   \item Global Tukey-biweight IRLS re-weighting (comparing every
#'     point to ONE whole-curve residual scale). Flagged real curvature
#'     as noise in steep regions and left it untouched in flat ones.
#'   \item Feeding \code{smooth.spline()} synthetic boundary points
#'     extrapolated from a local linear trend (padding). This fixed the
#'     edge artifact in isolation, but \code{smooth.spline()} has ONE
#'     global smoothing parameter for the whole curve — adding data
#'     anywhere changes the fit everywhere, which surfaced as a real
#'     curve's fitted peak exceeding every observed data point near it
#'     (the correction "spent its effort" pulling the tail in, at the
#'     peak's expense).
#'   \item Blending a local monotone interpolant across the edge
#'     UNCONDITIONALLY, over a FIXED-width margin. Applying it even
#'     where nothing was wrong made several already-perfect curves
#'     (mostly ones whose edges never touch a degenerate boundary)
#'     measurably worse, since a raw-data interpolant doesn't
#'     necessarily agree with an already-fine spline fit there. A fixed
#'     margin also sometimes ended before the spline had genuinely
#'     recovered, blending against a still-wrong function.
#'   \item Detecting the artifact via outright SIGN REVERSAL (does the
#'     spline's fitted direction ever contradict the raw data's dominant
#'     direction). Fixed the cases it caught cleanly, but missed a whole
#'     class of real artifacts: a "shelf" where the curve flattens
#'     dramatically without ever actually reversing, invisible to a
#'     check that only looks for a sign flip. A follow-up attempt
#'     applying the same re-acceleration idea to the RAW data (rather
#'     than the fit) was also rejected — ordinary point-to-point
#'     sampling noise in the raw grid trips a slope-ratio check on its
#'     own, flagging curves (mostly profile curves) the fit already
#'     handled fine.
#' }
#' The current design fixes all of the above: the correction only
#' engages where the FITTED CURVE ITSELF shows a re-acceleration (not
#' unconditionally, and not from raw-data noise alone), and it never
#' touches \code{smooth.spline()}'s own fit computation (no padding —
#' the correction is a display-time override, not new data), so nothing
#' more than a few points near a genuinely misbehaving edge is ever
#' affected. Validated against all 40 real profile/integrated curves in
#' a Simpson's-index batch (\code{exp_v6}), including 8 curves found to
#' have a subtle shelf artifact the prior (sign-reversal) design missed
#' entirely: every one resolved, the one known single-point interior
#' hitch and the original boundary-wiggle cases all remained fixed, and
#' only one curve showed a sign-change count change elsewhere (traced to
#' a genuine small dip/rise already present in that curve's raw data,
#' negligible visually) — confirmed by targeted metrics AND direct
#' visual comparison against the raw points on every case, not an
#' aggregate roughness score alone (which was in fact actively
#' misleading earlier in this investigation: a fit that respects a
#' genuinely sharp-but-monotonic rise in the raw data can show a LARGER
#' raw second-difference than one that smooths over it, without being
#' any less correct).
#'
#' When \code{enforce_concavity = TRUE}, the procedure is:
#' \enumerate{
#'   \item Fit \code{smooth.spline} to the (Hampel-weighted) grid, then
#'     apply the edge correction described above.
#'   \item Evaluate the corrected fit on a fine internal grid (500
#'     points) over the TRUE ψ range.
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
  # is required by both the outlier filter and the edge correction
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

  x <- psi_loglik_df$psi
  y <- psi_loglik_df$loglik
  n <- length(x)
  psi_range <- range(x)

  # ------------------------------------------------------------------
  # Step 1: down-weight isolated interior outliers (Hampel filter),
  # then fit smooth.spline to the untouched, weighted grid — no
  # synthetic data added anywhere, so this fit is exactly what
  # smooth.spline() would produce on the real data everywhere except
  # the edge margins corrected in Step 2.
  # ------------------------------------------------------------------
  weights <- .hampel_outlier_weights(y)

  psi_loglik_spline <- stats::smooth.spline(x = x, y = y, w = weights)
  .spline_at <- function(psi) stats::predict(psi_loglik_spline, psi)$y

  # ------------------------------------------------------------------
  # Step 2: correct domain edges ONLY where the spline's own fitted
  # slope sequence re-accelerates near the edge (a "shelf" — flattens
  # without reversing) — trusting the raw points directly (via a
  # monotone Hermite interpolant) over exactly that data-driven extent,
  # smoothstep-blended into the spline so there's no seam at the
  # hand-off.
  # ------------------------------------------------------------------
  max_edge_check <- min(20L, max(5L, floor(n / 4)))

  x_rev <- rev(x)
  x_left_stop <- .find_fit_instability_extent(x, .spline_at, x[max_edge_check + 1L])
  x_right_stop <- .find_fit_instability_extent(x_rev, .spline_at, x_rev[max_edge_check + 1L])

  left_needed <- !is.na(x_left_stop)
  right_needed <- !is.na(x_right_stop)

  n_left <- if (left_needed) max(4L, which.min(abs(x - x_left_stop))) else 0L
  n_right <- if (right_needed) max(4L, which.min(abs(x_rev - x_right_stop))) else 0L

  left_interp <- if (left_needed) {
    stats::splinefun(x[1:n_left], y[1:n_left], method = "monoH.FC")
  } else {
    NULL
  }
  right_interp <- if (right_needed) {
    idx <- (n - n_right + 1L):n
    stats::splinefun(x[idx], y[idx], method = "monoH.FC")
  } else {
    NULL
  }

  x_left_bound <- if (left_needed) x[n_left] else NA_real_
  x_right_bound <- if (right_needed) x[n - n_right + 1L] else NA_real_
  x0 <- x[1]
  xn <- x[n]

  .corrected_fit <- function(psi) {
    y_out <- .spline_at(psi)

    if (left_needed) {
      in_left <- psi <= x_left_bound
      if (any(in_left)) {
        t <- (psi[in_left] - x0) / (x_left_bound - x0)
        w_local <- 1 - .smoothstep(t) # 1 at true edge, 0 at artifact's inner edge
        y_out[in_left] <- w_local * left_interp(psi[in_left]) +
          (1 - w_local) * y_out[in_left]
      }
    }

    if (right_needed) {
      in_right <- psi >= x_right_bound
      if (any(in_right)) {
        t <- (xn - psi[in_right]) / (xn - x_right_bound)
        w_local <- 1 - .smoothstep(t)
        y_out[in_right] <- w_local * right_interp(psi[in_right]) +
          (1 - w_local) * y_out[in_right]
      }
    }

    y_out
  }

  if (!enforce_concavity) {
    psi_loglik <- function(psi) .corrected_fit(psi)
    attr(psi_loglik, "pseudolikelihood") <- attr(
      psi_loglik_df,
      "pseudolikelihood"
    )
    attr(psi_loglik, "psi range") <- psi_range
    return(psi_loglik)
  }

  # ------------------------------------------------------------------
  # Step 3: evaluate the corrected fit on a fine grid over the TRUE
  # psi range
  # ------------------------------------------------------------------
  psi_fine <- seq(psi_range[1], psi_range[2], length.out = 500L)
  y_fine <- .corrected_fit(psi_fine)

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