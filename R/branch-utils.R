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

  if (!is.numeric(R) || R < 1) {
    stop(
      "Computed number of branches R must be a positive integer.",
      call. = FALSE
    )
  }

  as.integer(R)
}


# ======================================================================
# 2. Compute Required Per-Branch Alpha (Guarantee Global Alpha Cutoff)
# ======================================================================

#' Compute Branch-Level Tail Probability for Integrated Likelihood Cutoff
#'
#' @description
#' Computes a branch-level tail probability \eqn{\alpha_\text{branch}} such
#' that, when \eqn{R} Monte Carlo integrated-likelihood branches are averaged
#' via a log-sum-exp operation, the resulting integrated log-likelihood is
#' very likely to reach the desired global confidence level
#' \eqn{1 - \alpha}.
#'
#' The calculation is based on a likelihood-ratio cutoff for a scalar
#' parameter of interest, with a tempered correction for branch averaging.
#' The tempering parameter \code{gamma} reduces the conservativeness of the
#' worst-case \eqn{\log(R)} penalty, reflecting the fact that only a subset
#' of branches typically contribute meaningfully to the integrated likelihood.
#'
#' @details
#' Let \eqn{c_\text{global} = \tfrac{1}{2}\chi^2_{1,\,1-\alpha}} denote the
#' standard likelihood-ratio cutoff defining a \eqn{1-\alpha} confidence
#' interval. When averaging \eqn{R} branches, a sufficient condition for the
#' integrated log-likelihood to fall below this cutoff is that each branch
#' falls below
#' \deqn{
#'   c_\text{branch} = c_\text{global} + \gamma \log(R),
#' }
#' where \eqn{\gamma \in (0,1]} controls the degree of conservativeness.
#'
#' Setting \code{gamma = 1} recovers a worst-case bound in which all branches
#' contribute equally. Smaller values of \code{gamma} provide a less
#' conservative but typically adequate cutoff, reflecting unequal branch
#' weights in practice.
#'
#' @param R Positive integer giving the number of Monte Carlo branches.
#' @param alpha Numeric scalar in \eqn{(0,1)} giving the desired global
#'   significance level.
#' @param gamma Numeric scalar in \eqn{(0,1]} tempering the \eqn{\log(R)}
#'   correction; smaller values reduce conservativeness. Defaults to \code{0.5}.
#'
#' @return
#' A numeric scalar giving the branch-level tail probability
#' \eqn{\alpha_\text{branch}} corresponding to the required branch cutoff.
#'
#' @seealso
#' \code{\link{integrate}}, \code{\link{profile}}
#'
#' @keywords internal
compute_required_branch_alpha <- function(R, alpha, gamma = 0.5) {
  if (!is.numeric(R) || R < 1) {
    stop("`R` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(gamma) || gamma <= 0 || gamma > 1) {
    stop("`gamma` must be in (0, 1].", call. = FALSE)
  }

  c_global <- 0.5 * stats::qchisq(1 - alpha, df = 1)
  c_branch <- c_global + gamma * log(R)

  1 - stats::pchisq(2 * c_branch, df = 1)
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
  if (!is.numeric(increment) || increment <= 0) {
    stop("`increment` must be a strictly positive scalar.", call. = FALSE)
  }

  structure(
    list(
      psi_mle = psi_mle,
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
