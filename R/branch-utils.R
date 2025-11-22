# ======================================================================
# INTERNAL UTILS: Branch Count + Alpha Allocation
# File: R/utils-branch-params.R
# ======================================================================

#' Compute number of Monte Carlo branches (R)
#'
#' @description
#' Determines how many nuisance-based branches should be drawn for
#' Monte Carlo integrated likelihood, based on the execution specification.
#'
#' For:
#'  * serial:   R is explicitly provided via `serial_spec(R = ...)`
#'  * parallel: R = num_workers * chunk_size
#'
#' @param execution An execution spec object created by [serial_spec()] or [parallel_spec()]
#'
#' @return Integer number of branches (R)
#'
#' @keywords internal
compute_num_branches <- function(execution) {

  if (inherits(execution, "likelihood_execution_serial")) {
    R <- execution$R

  } else if (inherits(execution, "likelihood_execution_parallel")) {
    R <- execution$num_workers * execution$chunk_size

  } else {
    stop("Invalid execution object: must be serial_spec() or parallel_spec().",
         call. = FALSE)
  }

  if (!is.numeric(R) || R < 1)
    stop("Computed number of branches R must be a positive integer.", call. = FALSE)

  as.integer(R)
}

# ======================================================================

#' Compute required per-branch alpha to guarantee global alpha cutoff
#'
#' @description
#' Given:
#'   * a target global confidence level α (e.g., α = 0.01 for 99% CI)
#'   * number of branches R
#'
#' compute the per-branch alpha level required such that **each branch**
#' is extended far enough to guarantee capturing the **integrated** cutoff
#' after log-sum-exp averaging.
#'
#' The derivation follows the established bound:
#'
#'   per_branch_cutoff ≥ global_cutoff + log(R)
#'
#' @param R Integer number of branches
#' @param alpha Global target tail probability (e.g., 0.01 for 99% CI)
#'
#' @return Numeric per-branch alpha to be used when determining branch depth
#'
#' @keywords internal
compute_required_branch_alpha <- function(R, alpha) {

  if (!is.numeric(R) || R < 1)
    stop("R must be a positive integer.", call. = FALSE)

  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1)
    stop("alpha must be a number strictly between 0 and 1.", call. = FALSE)

  # Global cutoff
  c_global <- 0.5 * stats::qchisq(1 - alpha, df = 1)

  # Required branch cutoff
  c_branch <- c_global + log(R)

  # Convert back from cutoff to tail prob
  alpha_branch <- 1 - stats::pchisq(2 * c_branch, df = 1)

  alpha_branch
}

