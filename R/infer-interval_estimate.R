# ========================================================================
# infer-interval_estimate.R — Likelihood-based confidence intervals for ψ
# ========================================================================

# ----------------------------------------------------------------------
# Internal: Shift log-likelihood by a constant
# ----------------------------------------------------------------------

#' Shift a Psi Log-Likelihood Function
#'
#' @description
#' Returns a new function corresponding to \eqn{\ell(\psi) + shift_val}.
#'
#' @param psi_loglik Function returning \eqn{\ell(\psi)}.
#' @param shift_val  Numeric scalar shift value.
#'
#' @return Function of \eqn{\psi} returning the shifted log-likelihood.
#'
#' @keywords internal
shift_psi_loglik <- function(psi_loglik, shift_val) {
  psi_loglik_shifted <- function(psi) psi_loglik(psi) + shift_val
  # NB: fit_psi_loglik() stores the range under "psi range" (with a
  # space); match that key so the shifted function actually carries it.
  attr(psi_loglik_shifted, "psi range") <- attr(psi_loglik, "psi range")
  psi_loglik_shifted
}

# ----------------------------------------------------------------------
# Internal: Find confidence interval endpoints
# ----------------------------------------------------------------------

#' Find Confidence Interval Endpoints by Log-Likelihood Inversion
#'
#' @description
#' Returns endpoints for a \eqn{(1-\alpha) 100\%} likelihood ratio
#' confidence interval for \eqn{\psi} by solving for the roots of
#' \deqn{\ell(\psi) - \ell(\hat\psi) + \tfrac{1}{2}\chi^2_{1-\alpha,1} = 0.}
#'
#' If \code{uniroot} fails on a side because the grid reaches a declared
#' parameter space boundary before descending to the critical threshold,
#' the boundary value is substituted for that endpoint. A side is
#' considered to have reached its boundary when the grid edge is within
#' \code{1.5 * grid_increment} of the boundary, on either side — whether
#' the grid stopped just short of the boundary (the common case; e.g. an
#' open boundary that's a limit point the grid can never actually reach,
#' or a closed boundary the traversal ran out of solvable room before
#' reaching — the latter occurs when the underlying constraint geometry
#' degenerates near the boundary, e.g. Simpson's index level set shrinks
#' to a point as psi -> 1/J) or landed slightly past it (floating-point
#' slack in how the traversal grid is anchored). This avoids relying on
#' reconstructing a theoretical grid index from \code{psi_mle} via
#' floor/ceiling arithmetic, which is unreliable exactly in the
#' near-integer cases that matter most (a tiny floating-point
#' discrepancy in \code{psi_mle} can flip the computed index by a full
#' step). If the grid stopped well short of the boundary for an
#' unrelated reason, \code{NA_real_} is returned for that endpoint.
#'
#' @param psi_loglik     Function returning \eqn{\ell(\psi)}.
#' @param alpha          Numeric scalar in (0, 1).
#' @param psi_interval   Optional \code{sets::interval} object defining
#'   the parameter space domain. When provided, boundary substitution is
#'   applied on each side independently.
#' @param grid_increment Positive numeric scalar. The ψ grid spacing,
#'   used to determine whether the grid edge is close enough to the
#'   domain boundary to trigger boundary substitution.
#' @param psi_mle        Retained for API compatibility; not used by the
#'   boundary check, which relies only on \code{grid_increment}.
#'
#' @return A one-row tibble with columns \code{alpha}, \code{lower},
#'   \code{upper}, and attribute \code{psi_hat}.
#'
#' @keywords internal
find_interval_endpoints <- function(
  psi_loglik,
  alpha,
  psi_interval = NULL,
  grid_increment = NULL,
  psi_mle = NULL
) {
  crit <- 0.5 * stats::qchisq(1 - alpha, df = 1)
  psi_loglik_max_point <- get_psi_loglik_max_point(psi_loglik)
  psi_hat <- psi_loglik_max_point[["argmax"]]
  psi_loglik_max <- psi_loglik_max_point[["maximum"]]
  shift_val <- crit - psi_loglik_max
  psi_loglik_shifted <- shift_psi_loglik(psi_loglik, shift_val)

  psi_range <- attr(psi_loglik, "psi range")

  increment <- grid_increment %||% sqrt(.Machine$double.eps)

  # ------------------------------------------------------------------
  # A grid edge is treated as "at the boundary" if it's within 1.5 grid
  # increments of the boundary, on EITHER side. Symmetric on purpose
  # (2026-08-04 fix — was asymmetric: -0.01 * increment vs +1.5 *
  # increment): the original tolerance assumed a closed boundary is
  # always reached essentially exactly (the traversal's own
  # eval-at-boundary step usually arranges this), but that assumption
  # breaks when the constraint geometry degenerates near the boundary
  # (branches/profile can't solve arbitrarily close to it — observed on
  # the Simpson's-index application, where the level set's radius
  # shrinks to zero as psi -> 1/J) and the grid genuinely stops short.
  # A grid that stops well short of the boundary for an unrelated reason
  # still correctly falls outside this tolerance and returns NA_real_
  # below, preserving the original safety behavior. Symmetric tolerance
  # also naturally covers the open-boundary case (grid edge sits inside
  # a limit point it can never reach) without needing to reconstruct a
  # theoretical grid index from psi_mle.
  # ------------------------------------------------------------------
  .at_theoretical_boundary <- function(grid_edge, boundary) {
    if (is.null(boundary) || !is.finite(boundary)) {
      return(FALSE)
    }
    gap <- boundary - grid_edge
    abs(gap) <= increment * 1.5
  }

  # ------------------------------------------------------------------
  # Fine scan grid for locating threshold crossings.
  #
  # On jagged log-likelihoods the shifted curve can cross zero several
  # times per side. A single uniroot() bracket over the whole side
  # returns SOME crossing (whichever bisection happens to land on), which
  # tends to be an INNER crossing and yields a too-narrow interval. We
  # instead scan a fine grid, find every sign change, and bracket the
  # OUTERMOST crossing on each side — the widest LR interval consistent
  # with the fitted curve (the conservative choice under the default
  # enforce_concavity = FALSE). psi_loglik_shifted is vectorized.
  # ------------------------------------------------------------------
  scan_step <- if (is.finite(increment) && increment > 0) {
    increment / 2
  } else {
    (psi_range[2] - psi_range[1]) / 1000
  }
  n_scan <- as.integer((psi_range[2] - psi_range[1]) / scan_step) + 1L
  n_scan <- min(20001L, max(1001L, n_scan))
  psi_scan <- seq(psi_range[1], psi_range[2], length.out = n_scan)
  f_scan <- psi_loglik_shifted(psi_scan)

  # Indices i where the shifted curve changes sign on [psi_scan[i], psi_scan[i+1]]
  fa <- f_scan[-length(f_scan)]
  fb <- f_scan[-1]
  sign_change <- which(is.finite(fa) & is.finite(fb) & (fa * fb < 0))

  .outermost_root <- function(side) {
    if (length(sign_change) == 0L) {
      return(NULL)
    }
    cand <- if (side == "lower") {
      sign_change[psi_scan[sign_change + 1L] <= psi_hat]
    } else {
      sign_change[psi_scan[sign_change] >= psi_hat]
    }
    if (length(cand) == 0L) {
      return(NULL)
    }
    i <- if (side == "lower") min(cand) else max(cand)
    tryCatch(
      stats::uniroot(
        f = psi_loglik_shifted,
        interval = c(psi_scan[i], psi_scan[i + 1L])
      )$root,
      error = function(e) NULL
    )
  }

  # Boundary substitution: if a side never crosses the threshold within
  # the grid but the grid edge sits at a declared domain boundary while
  # still above threshold, report the boundary value. Guarded so a NULL
  # psi_interval or a non-finite comparison never triggers if(NA).
  .boundary_sub <- function(side) {
    if (is.null(psi_interval)) {
      return(NA_real_)
    }
    if (side == "lower") {
      edge <- psi_range[1]
      bnd <- suppressWarnings(min(psi_interval))
    } else {
      edge <- psi_range[2]
      bnd <- suppressWarnings(max(psi_interval))
    }
    if (!is.finite(bnd)) {
      return(NA_real_)
    }
    at_bound <- isTRUE(.at_theoretical_boundary(edge, bnd)) &&
      isTRUE(psi_loglik_shifted(edge) > 0)
    if (isTRUE(at_bound)) bnd else NA_real_
  }

  # ------------------------------------------------------------------
  # Lower endpoint
  # ------------------------------------------------------------------
  lower <- .outermost_root("lower") %||% .boundary_sub("lower")

  # ------------------------------------------------------------------
  # Upper endpoint
  # ------------------------------------------------------------------
  upper <- .outermost_root("upper") %||% .boundary_sub("upper")

  ci_endpoints <- tibble::tibble(alpha = alpha, lower = lower, upper = upper)
  attr(ci_endpoints, "psi_hat") <- psi_hat
  ci_endpoints
}

# ----------------------------------------------------------------------
# Internal: Add CI diagnostics and truth coverage
# ----------------------------------------------------------------------

#' Add Diagnostic Quantities to Psi Confidence Intervals
#'
#' @param interval_estimate_df Data frame with columns \code{alpha},
#'   \code{lower}, \code{upper}.
#' @param psi_0 Optional numeric scalar. True value of \eqn{\psi}.
#'
#' @return Data frame with additional diagnostic columns.
#'
#' @keywords internal
add_interval_diagnostics <- function(interval_estimate_df, psi_0 = NA_real_) {
  psi_hat <- attr(interval_estimate_df, "psi_hat")

  interval_estimate_df <- interval_estimate_df |>
    dplyr::mutate(
      length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),
      lower_dev = dplyr::if_else(is.na(lower), NA_real_, psi_hat - lower),
      upper_dev = dplyr::if_else(is.na(upper), NA_real_, upper - psi_hat),
      contains_truth = if (is.na(psi_0)) {
        NA
      } else {
        !is.na(lower) & !is.na(upper) & lower <= psi_0 & upper >= psi_0
      }
    )

  attr(interval_estimate_df, "psi_0") <- psi_0
  interval_estimate_df
}

# ----------------------------------------------------------------------
# Internal: Format confidence intervals for presentation
# ----------------------------------------------------------------------

#' Format Psi Confidence Interval Table for Display
#'
#' @param interval_estimate_df Data frame of CI bounds and diagnostics.
#'
#' @return Formatted data frame suitable for display.
#'
#' @keywords internal
format_interval_estimate_df <- function(interval_estimate_df) {
  formatted_df <- interval_estimate_df |>
    dplyr::mutate(
      level = scales::percent(1 - alpha),
      interval = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_character_,
        sprintf("[%.2f, %.2f]", lower, upper)
      )
    ) |>
    dplyr::select(
      interval,
      length,
      lower_dev,
      upper_dev,
      contains_truth,
      level
    )

  attr(formatted_df, "interval_estimate_raw") <- interval_estimate_df |>
    dplyr::select(alpha, lower, upper)

  formatted_df
}

# ----------------------------------------------------------------------
# Internal: Compute confidence intervals across all α levels
# ----------------------------------------------------------------------

#' Compute Confidence Intervals for a Scalar Parameter Psi
#'
#' @description
#' Computes likelihood-based confidence intervals by inverting a
#' zero-maximized log-likelihood function over multiple confidence levels.
#'
#' @param psi_loglik_df A data frame with columns \code{psi} and
#'   \code{loglik}.
#' @param alpha_levels  Numeric vector of significance levels.
#' @param psi_0         Optional numeric scalar. True value of \eqn{\psi}.
#'   Default: \code{NA_real_}.
#' @param psi_interval  Optional \code{sets::interval} object defining
#'   the parameter space domain. Passed to \code{find_interval_endpoints()}
#'   for boundary substitution.
#' @param enforce_concavity Logical. Whether to project the fitted spline
#'   onto its LCM before inverting for interval endpoints. Default:
#'   \code{FALSE}.
#' @param psi_mle       Retained for API compatibility; passed through
#'   but not used by \code{find_interval_endpoints()}'s boundary check.
#'
#' @return Formatted data frame of confidence interval summaries.
#'
#' @keywords internal
get_interval_estimate_df <- function(
  psi_loglik_df,
  alpha_levels,
  psi_0 = NA_real_,
  psi_interval = NULL,
  enforce_concavity = FALSE,
  psi_mle = NULL
) {
  psi_loglik <- fit_psi_loglik(
    psi_loglik_df,
    enforce_concavity = enforce_concavity
  )

  grid_increment <- psi_loglik_df$psi |>
    diff() |>
    max()

  interval_estimate_df <- alpha_levels |>
    purrr::map_dfr(
      \(alpha) {
        find_interval_endpoints(
          psi_loglik = psi_loglik,
          alpha = alpha,
          psi_interval = psi_interval,
          grid_increment = grid_increment,
          psi_mle = psi_mle
        )
      }
    ) |>
    add_interval_diagnostics(psi_0) |>
    format_interval_estimate_df()

  attr(interval_estimate_df, "pseudolikelihood") <- attr(
    psi_loglik_df,
    "pseudolikelihood"
  )
  attr(interval_estimate_df, "psi_interval") <- psi_interval

  interval_estimate_df
}

# ========================================================================
# END infer-interval_estimate.R
# ========================================================================