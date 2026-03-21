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
#' @param psi            Numeric scalar ψ value.
#' @param param_init     Numeric vector initial θ guess.
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
#' @param interval Numeric length-2 vector \code{c(lower, upper)}.
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
# Drop magnitude check
# ----------------------------------------------------------------------

#' Check Whether a Drop is Within an Acceptable Range
#'
#' @description
#' Returns \code{TRUE} if \code{drop} is within \code{drop_multiplier}
#' times the median of \code{recent_drops}. Always returns \code{TRUE}
#' when \code{recent_drops} is empty or has a non-positive median.
#'
#' @param drop         Numeric scalar. The new drop to check.
#' @param recent_drops Numeric vector. Recent drop history.
#' @param drop_multiplier Positive numeric scalar.
#'
#' @return Logical scalar.

#' @importFrom stats median
#' @keywords internal
check_drop <- function(drop, recent_drops, drop_multiplier) {
  if (length(recent_drops) < 1L) {
    return(TRUE)
  }
  ref <- median(recent_drops)
  if (ref <= 0) {
    return(TRUE)
  }
  drop <= drop_multiplier * ref
}
