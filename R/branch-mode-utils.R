# ======================================================================
# branch-mode-utils.R — Shared Utilities for Branch Mode Location
# ======================================================================
#
# Internal helpers used by branch mode locator implementations.
#
# Design principles:
#   • No dependence on specific algorithms (Brent, grid, etc.)
#   • No calibration logic
#   • Defensive checks and explicit failure modes
#   • Small, composable helpers
# ======================================================================

# ----------------------------------------------------------------------
# Evaluate branch log-likelihood safely
# ----------------------------------------------------------------------

#' Safely Evaluate Branch Log-Likelihood
#'
#' @description
#' Evaluates the branch log-likelihood at a given ψ value using
#' \code{branch_fn}, returning \code{-Inf} on failure.
#'
#' This is intended for coarse scans and bracketing, where robustness
#' is more important than propagating solver errors.
#'
#' @param psi Numeric scalar ψ value.
#' @param param_init Numeric vector initial θ guess.
#' @param branch_fn Function(psi, param_init) → list(param_hat, branch_val).
#'
#' @return Numeric scalar log-likelihood value (possibly \code{-Inf}).
#'
#' @keywords internal
safe_eval_branch <- function(psi, param_init, branch_fn) {
  val <- tryCatch(
    {
      out <- branch_fn(psi, param_init)
      out$branch_val
    },
    error = function(e) -Inf
  )

  if (!is.numeric(val) || length(val) != 1 || !is.finite(val)) {
    -Inf
  } else {
    val
  }
}


# ----------------------------------------------------------------------
# Generate a coarse ψ grid
# ----------------------------------------------------------------------

#' Generate Coarse ψ Grid
#'
#' @description
#' Generates a coarse grid of ψ values over a supplied interval.
#'
#' @param interval Numeric length-2 vector \code{c(lower, upper)}.
#' @param n Integer number of grid points.
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
# Locate index of maximum value (defensive)
# ----------------------------------------------------------------------

#' Locate Maximum Index Safely
#'
#' @description
#' Returns the index of the maximum finite value in a numeric vector.
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

  which.max(x[ok])[1]
}


# ----------------------------------------------------------------------
# Build a local bracket around a mode
# ----------------------------------------------------------------------

#' Build Local ψ Bracket
#'
#' @description
#' Constructs a local bracketing interval around a candidate mode index
#' in a ψ grid.
#'
#' @param psi_grid Numeric vector of ψ values.
#' @param idx Integer index of candidate mode.
#'
#' @return Numeric length-2 vector \code{c(lower, upper)}.
#'
#' @keywords internal
build_local_bracket <- function(psi_grid, idx) {
  n <- length(psi_grid)

  if (idx <= 1 || idx >= n) {
    return(range(psi_grid))
  }

  c(psi_grid[idx - 1], psi_grid[idx + 1])
}


# ----------------------------------------------------------------------
# Enforce ordering and finiteness of a bracket
# ----------------------------------------------------------------------

#' Validate ψ Bracket
#'
#' @description
#' Ensures a bracketing interval is finite and ordered.
#'
#' @param bracket Numeric length-2 vector.
#'
#' @return Numeric length-2 vector.
#'
#' @keywords internal
validate_psi_bracket <- function(bracket) {
  if (!is.numeric(bracket) || length(bracket) != 2L) {
    stop("bracket must be a numeric vector of length 2.", call. = FALSE)
  }

  if (any(!is.finite(bracket))) {
    stop("bracket must contain finite values.", call. = FALSE)
  }

  if (bracket[1] >= bracket[2]) {
    stop("bracket lower bound must be < upper bound.", call. = FALSE)
  }

  bracket
}


# ----------------------------------------------------------------------
# Standardize branch mode output
# ----------------------------------------------------------------------

#' Standardize Branch Mode Output
#'
#' @description
#' Ensures that a branch mode locator returns a standardized result
#' containing required fields.
#'
#' @param psi_hat Numeric scalar ψ̂.
#' @param param_hat Numeric vector θ̂.
#' @param loglik_at_mode Numeric scalar.
#' @param status Character scalar status label.
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
