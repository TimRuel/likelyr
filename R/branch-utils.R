# ======================================================================
# Branch Utilities: R Count, Alpha Allocation, and ψ-Grid Helpers
# File: R/branch-utils.R
# ======================================================================


# ======================================================================
# 1. Compute Number of Monte Carlo Branches
# ======================================================================

#' Compute Number of Monte Carlo Branches
#'
#' @description
#' Determines the number of Monte Carlo branches `R` implied by an
#' execution specification created with [serial_spec()] or [parallel_spec()].
#'
#' * **Serial execution:**
#'   `R = execution$R`
#'
#' * **Parallel execution:**
#'   `R = execution$num_workers * execution$chunk_size`
#'
#' @param execution Execution specification object.
#'
#' @return Integer number of branches `R`.
#' @keywords internal
compute_num_branches <- function(execution) {

  if (inherits(execution, "serial_spec")) {

    R <- execution$R

  } else if (inherits(execution, "parallel_spec")) {

    R <- execution$num_workers * execution$chunk_size

  } else {

    stop(
      "`execution` must be created via serial_spec() or parallel_spec().",
      call. = FALSE
    )
  }

  if (!is.numeric(R) || R < 1)
    stop("Computed number of branches R must be a positive integer.",
         call. = FALSE)

  as.integer(R)
}


# ======================================================================
# 2. Compute Required Per-Branch Alpha (Guarantee Global Alpha Cutoff)
# ======================================================================

#' Compute Required Per-Branch Alpha
#'
#' @description
#' Computes the per-branch tail probability `alpha_branch` such that
#' each branch is extended far enough to guarantee the **global**
#' integrated-likelihood cutoff for tail probability `alpha`.
#'
#' The requirement is:
#' \deqn{
#'   \text{branch\_cutoff} \ge c_{\text{global}} + \log R
#' }
#'
#' where:
#'
#' \deqn{c_{\text{global}} = \tfrac12 \chi^2_1(1 - \alpha)}
#'
#' @param R Positive integer number of branches.
#' @param alpha Global tail probability in `(0, 1)`.
#'
#' @return Numeric scalar `alpha_branch`.
#' @keywords internal
compute_required_branch_alpha <- function(R, alpha) {

  if (!is.numeric(R) || R < 1)
    stop("`R` must be a positive integer.", call. = FALSE)

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("`alpha` must be strictly between 0 and 1.", call. = FALSE)

  # global cutoff at desired CI depth
  c_global <- 0.5 * stats::qchisq(1 - alpha, df = 1)

  # required branch cutoff to hit global shape after averaging
  c_branch <- c_global + log(R)

  # convert back to tail probability
  alpha_branch <- 1 - stats::pchisq(2 * c_branch, df = 1)

  alpha_branch
}



# ======================================================================
# 3. ψ-Grid Anchor (ψ_k = ψ_MLE + k * increment)
# ======================================================================

#' Create a ψ-Grid Anchor
#'
#' @description
#' Constructs a lightweight, regular ψ-grid representation:
#' \deqn{
#'   \psi_k = \psi_{\text{MLE}} + k \cdot \text{increment}.
#' }
#'
#' Used for branch sweeps where only relative grid indices matter.
#'
#' @param psi_mle Numeric: ψ_MLE.
#' @param increment Positive numeric: grid spacing.
#'
#' @return An object of class `"psi_grid"`.
#' @keywords internal
psi_grid_anchor <- function(psi_mle, increment) {

  if (!is.numeric(increment) || increment <= 0)
    stop("`increment` must be a strictly positive scalar.", call. = FALSE)

  structure(
    list(
      psi_mle   = psi_mle,
      increment = increment
    ),
    class = "psi_grid"
  )
}



# ======================================================================
# 4. Snap ψ to Nearest Grid Point (rarely used)
# ======================================================================

#' Snap ψ to Nearest Grid Point
#'
#' @description
#' Given a ψ value and a ψ-grid created by [psi_grid_anchor()],
#' return the nearest ψ-grid point.
#'
#' @param psi Numeric scalar.
#' @param grid A `"psi_grid"` object.
#'
#' @return Numeric: nearest ψ-grid point.
#' @keywords internal
snap_to_grid <- function(psi, grid) {

  k_float <- (psi - grid$psi_mle) / grid$increment

  # guard against floating point drift
  k <- round(k_float)

  grid$psi_mle + k * grid$increment
}
