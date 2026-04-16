# ======================================================================
# branch-utils.R — Shared Utilities for Branch Computation
# ======================================================================

# ----------------------------------------------------------------------
# ψ-grid construction
# ----------------------------------------------------------------------

#' Construct a ψ-Grid Anchored at the MLE
#'
#' @description
#' Builds a grid object representing the integer lattice
#' \code{psi_mle + k * increment} clipped to \code{[psi_lower, psi_upper]}.
#' The grid object is passed to traversal functions to ensure all
#' evaluations are aligned to the same anchor.
#'
#' @param psi_mle   Numeric scalar. MLE of ψ — the anchor point.
#' @param increment Positive numeric scalar. Grid spacing.
#' @param psi_lower Optional numeric scalar. Lower bound.
#' @param psi_upper Optional numeric scalar. Upper bound.
#'
#' @return A named list with slots:
#'   \code{psi_mle}, \code{increment}, \code{psi_lower}, \code{psi_upper}.
#'
#' @keywords internal
psi_grid_anchor <- function(
  psi_mle,
  increment,
  psi_lower = NULL,
  psi_upper = NULL
) {
  if (!is.numeric(psi_mle) || length(psi_mle) != 1L) {
    stop("psi_mle must be a numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(increment) || length(increment) != 1L || increment <= 0) {
    stop("increment must be a positive numeric scalar.", call. = FALSE)
  }

  list(
    psi_mle = psi_mle,
    increment = increment,
    psi_lower = psi_lower,
    psi_upper = psi_upper
  )
}

# ----------------------------------------------------------------------
# Safe branch evaluation
# ----------------------------------------------------------------------

#' Safely Evaluate Branch Log-Likelihood
#'
#' @description
#' Evaluates the branch log-likelihood at a given ψ value, returning
#' \code{-Inf} on failure. Intended for coarse scans and bracketing.
#'
#' @param psi              Numeric scalar ψ value.
#' @param param_init       Numeric vector initial θ guess.
#' @param branch_evaluator Function(psi, param_init) → list(param_hat, branch_val).
#'
#' @return Numeric scalar (possibly \code{-Inf}).
#'
#' @keywords internal
safe_eval_branch <- function(psi, param_init, branch_evaluator) {
  val <- tryCatch(
    branch_evaluator(psi, param_init)$branch_val,
    error = function(e) -Inf
  )
  if (!is.numeric(val) || length(val) != 1L || !is.finite(val)) -Inf else val
}

# ----------------------------------------------------------------------
# Coarse ψ grid
# ----------------------------------------------------------------------

#' Generate Coarse ψ Grid
#'
#' @param interval Numeric length-2 vector \code{z(lower, upper)}.
#' @param n        Integer number of grid points.
#'
#' @return Numeric vector of ψ values.
#'
#' @keywords internal
make_coarse_psi_grid <- function(interval, n = 25L) {
  if (!is.numeric(interval) || length(interval) != 2L) {
    stop("interval must be a numeric vector of length 2.", call. = FALSE)
  }
  if (interval[1] >= interval[2]) {
    stop("interval[1] must be < interval[2].", call. = FALSE)
  }
  if (!is.numeric(n) || n < 3) {
    stop("n must be an integer >= 3.", call. = FALSE)
  }
  seq(interval[1], interval[2], length.out = n)
}

# ----------------------------------------------------------------------
# Safe which.max
# ----------------------------------------------------------------------

#' Locate Maximum Index Safely
#'
#' @param x Numeric vector.
#'
#' @return Integer index, or \code{NA_integer_} if no finite values exist.
#'
#' @keywords internal
safe_which_max <- function(x) {
  if (!is.numeric(x)) {
    return(NA_integer_)
  }
  ok <- is.finite(x)
  if (!any(ok)) {
    return(NA_integer_)
  }
  which.max(x[ok])[1L]
}

# ----------------------------------------------------------------------
# Adjacent ψ grid points
# ----------------------------------------------------------------------

#' Get Adjacent ψ Grid Points Around a Mode
#'
#' @description
#' Returns the nearest grid points immediately left and right of a
#' located branch mode, using floating-point guards to handle cases
#' where the mode falls exactly on a grid point.
#'
#' @param psi_hat_branch Numeric scalar. Located branch mode.
#' @param grid           A grid object from \code{psi_grid_anchor()}.
#'
#' @return Named list with \code{left}, \code{right}, \code{k_left},
#'   \code{k_right}.
#'
#' @keywords internal
get_adjacent_psi_points <- function(psi_hat_branch, grid) {
  k_float <- (psi_hat_branch - grid$psi_mle) / grid$increment
  k_left <- floor(k_float + 1e-12)
  k_right <- ceiling(k_float - 1e-12)
  list(
    left = grid$psi_mle + k_left * grid$increment,
    right = grid$psi_mle + k_right * grid$increment,
    k_left = k_left,
    k_right = k_right
  )
}

# ----------------------------------------------------------------------
# Standardize branch mode output
# ----------------------------------------------------------------------

#' Standardize Branch Mode Output
#'
#' @param psi_hat        Numeric scalar ψ̂.
#' @param param_hat      Numeric vector θ̂.
#' @param loglik_at_mode Numeric scalar.
#' @param status         Character scalar status label.
#'
#' @return Named list with standardized fields.
#'
#' @keywords internal
make_branch_mode_result <- function(
  psi_hat,
  param_hat,
  loglik_at_mode,
  status = "success"
) {
  list(
    psi_hat = psi_hat,
    param_hat = param_hat,
    loglik_at_mode = loglik_at_mode,
    status = status
  )
}

# ----------------------------------------------------------------------
# Drop magnitude checks
# ----------------------------------------------------------------------

#' Drop Check
#'
#' @description
#' Used during \code{probe()} to decide whether to reject an omega-hat.
#' Applies two checks:
#' \enumerate{
#'   \item \strong{Absolute cap}: rejects if the drop exceeds
#'     \code{max_drop_cap}, which is calibrated from the profile
#'     likelihood during \code{preprocess()} as
#'     \code{cap_multiplier * median(profile_drops)}. This catches
#'     genuine discontinuities while scaling automatically with the
#'     curvature of the surface.
#'   \item \strong{Relative check}: once \code{k_recent} drops have
#'     accumulated, rejects if the drop exceeds \code{drop_multiplier}
#'     times the recent median. Rarely fires given typical
#'     \code{n_adjacent} and \code{k_recent} settings.
#' }
#'
#' @param drop             Numeric scalar. The new drop to check.
#' @param recent_drops     Numeric vector. Recent drop history.
#' @param drop_multiplier  Positive numeric scalar.
#' @param max_drop_cap     Positive numeric scalar. Absolute cap derived
#'   from the profile likelihood by \code{preprocess()}.
#' @param k_recent         Non-negative integer. Minimum history required
#'   before the relative check applies.
#'
#' @return Logical scalar. \code{TRUE} if the drop is acceptable.
#'
#' @keywords internal
check_drop <- function(
  drop,
  recent_drops,
  drop_multiplier,
  max_drop_cap,
  k_recent
) {
  # Absolute cap — calibrated from profile curvature
  if (drop > max_drop_cap) {
    return(FALSE)
  }

  # Relative check — only once enough history exists
  if (length(recent_drops) < k_recent) {
    return(TRUE)
  }
  ref <- median(recent_drops)
  if (ref <= 0) {
    return(TRUE)
  }
  drop <= drop_multiplier * ref
}

# ----------------------------------------------------------------------
# Common psi interval
# ----------------------------------------------------------------------

#' Compute Common ψ Interval for Branch Generation
#'
#' @description
#' Derives the common ψ support interval to be used across all Monte
#' Carlo branches, ensuring full overlap and valid CI estimation.
#'
#' The interval starts from the profile likelihood extent, optionally
#' expanded by \code{interval_buffer}, then intersected with
#' \code{psi_interval}.
#'
#' Additionally, if the profile reached a finite parameter space boundary
#' without dropping to its cutoff — detected by checking whether
#' \code{profile_lower} is within one grid step of \code{domain_lower}
#' (and analogously for the upper side) — the corresponding common
#' interval bound is automatically snapped to that boundary. This
#' handles the case where the likelihood is flat near the boundary and
#' the IL tails may not drop to the CI cutoff before reaching it.
#'
#' @param psi_loglik_df   Data frame with \code{psi} and \code{loglik}
#'   columns (i.e. \code{model$workspace$profile$psi_loglik_df}).
#' @param psi_interval    A \code{sets::interval} object or \code{NULL}.
#' @param increment       Positive numeric scalar. Grid spacing, used as
#'   the tolerance for boundary proximity detection.
#' @param interval_buffer Positive numeric scalar. Multiplicative
#'   expansion factor applied to the profile half-width before
#'   intersecting with \code{psi_interval}. A value of \code{1.0}
#'   uses the profile extent as-is. Default: \code{1.0}.
#'
#' @return A named list with:
#'   \itemize{
#'     \item \code{$psi_lower}         — numeric scalar lower bound
#'     \item \code{$psi_upper}         — numeric scalar upper bound
#'     \item \code{$snapped_to_lower}  — logical; TRUE if lower bound
#'       was snapped to the domain boundary
#'     \item \code{$snapped_to_upper}  — logical; TRUE if upper bound
#'       was snapped to the domain boundary
#'   }
#'
#' @keywords internal
compute_common_interval <- function(
  psi_loglik_df,
  psi_interval = NULL,
  increment,
  interval_buffer = 1.0
) {
  if (is.null(psi_loglik_df) || nrow(psi_loglik_df) == 0L) {
    stop(
      "compute_common_interval(): profile curve is empty or NULL.\n",
      "Run profile() before computing the common interval.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Profile extent
  # -------------------------------------------------------------------
  profile_lower <- min(psi_loglik_df$psi)
  profile_upper <- max(psi_loglik_df$psi)

  # -------------------------------------------------------------------
  # Apply multiplicative buffer around profile half-width
  # -------------------------------------------------------------------
  center <- (profile_lower + profile_upper) / 2
  half_width <- (profile_upper - profile_lower) / 2

  psi_lower <- center - half_width * interval_buffer
  psi_upper <- center + half_width * interval_buffer

  # -------------------------------------------------------------------
  # Domain bounds
  # -------------------------------------------------------------------
  domain_lower <- if (!is.null(psi_interval)) min(psi_interval) else NULL
  domain_upper <- if (!is.null(psi_interval)) max(psi_interval) else NULL

  # -------------------------------------------------------------------
  # Boundary proximity detection:
  # If the profile reached a finite boundary without dropping to its
  # cutoff (detected as profile endpoint within one grid step of the
  # domain boundary), snap the common interval to that boundary so
  # branches are evaluated all the way to it.
  # -------------------------------------------------------------------
  snapped_to_lower <- FALSE
  snapped_to_upper <- FALSE

  if (!is.null(domain_lower) && is.finite(domain_lower)) {
    if (profile_lower <= domain_lower + increment) {
      psi_lower <- domain_lower
      snapped_to_lower <- TRUE
    } else {
      psi_lower <- max(psi_lower, domain_lower)
    }
  }

  if (!is.null(domain_upper) && is.finite(domain_upper)) {
    if (profile_upper >= domain_upper - increment) {
      psi_upper <- domain_upper
      snapped_to_upper <- TRUE
    } else {
      psi_upper <- min(psi_upper, domain_upper)
    }
  }

  if (psi_lower >= psi_upper) {
    stop(
      "compute_common_interval(): lower bound >= upper bound.\n",
      "Check profile extent and psi_interval.",
      call. = FALSE
    )
  }

  list(
    psi_lower = psi_lower,
    psi_upper = psi_upper,
    snapped_to_lower = snapped_to_lower,
    snapped_to_upper = snapped_to_upper
  )
}
