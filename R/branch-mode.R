#' Solve for the Continuous Branch Mode ψ̂ (Internal)
#'
#' @description
#' Finds the **branch-specific mode** \eqn{\hat{\psi}} by maximizing the
#' branch log-likelihood over a *continuous* ψ domain.
#'
#' The evaluation function `eval_psi_fun(psi, theta_init)` returns:
#' \itemize{
#'   \item `theta_hat` — the nuisance-optimized θ̂(ψ)
#'   \item `branch_val` — log-likelihood at θ̂(ψ)
#' }
#'
#' This routine then solves:
#' \deqn{
#'   \hat{\psi} = \arg\max_{\psi \in \mathcal{I}} \; \ell(\hat{\theta}(\psi))
#' }
#'
#' using a Brent optimization over the user-defined interval.
#' The resulting mode **is not aligned to the ψ-grid**.
#' Grid-snapping happens later inside \code{get_adjacent_psi_points()}.
#'
#' @param psi_mle Numeric scalar. Global ψ MLE (used as the initial Brent point).
#' @param eval_psi_fun Function with signature \code{f(psi, theta_init)} returning
#'   a list \code{(theta_hat, branch_val)} for that ψ.
#' @param theta_init Initial nuisance parameter vector for inner optimizations.
#' @param search_interval Numeric vector of length 2 giving lower/upper bounds for ψ.
#' @param retries Integer. Number of boundary-expansion attempts if Brent mode
#'   lands on a boundary. Default: 3.
#' @param expand_factor Numeric expansion multiplier when increasing interval
#'   width around \code{psi_mle}. Default: 1.5.
#'
#' @return A list with components:
#' \describe{
#'   \item{psi_hat}{Continuous maximizer (branch-specific mode).}
#'   \item{theta_hat}{Nuisance optimizer θ̂ evaluated at \code{psi_hat}.}
#'   \item{loglik_at_mode}{Log-likelihood at the mode.}
#' }
#'
#' @details
#' If the Brent solution lands exactly on the boundary of the allowed search
#' interval, the function expands the interval *around ψ_MLE* and retries up to
#' \code{retries} times. This prevents misleading solutions caused by an overly
#' narrow initial range.
#'
#' @keywords internal
#' @noRd
branch_mode_solve <- function(
    psi_mle,
    eval_psi_fun,
    theta_init,
    search_interval,
    retries       = 3,
    expand_factor = 1.5
) {
  lower <- search_interval[1]
  upper <- search_interval[2]

  # 1. Objective for Brent optimization
  obj <- function(psi_val) {
    res <- eval_psi_fun(psi_val, theta_init)
    -res$branch_val
  }

  # 2. Initial Brent solve
  opt <- optim(
    par    = psi_mle,
    fn     = obj,
    method = "Brent",
    lower  = lower,
    upper  = upper
  )
  psi_hat <- opt$par

  # 3. Boundary-sticking logic
  k <- 0L
  while ((psi_hat <= lower || psi_hat >= upper) && k < retries) {

    width  <- (upper - lower) * (expand_factor^k)
    center <- psi_mle

    lower2 <- max(lower, center - width)
    upper2 <- min(upper, center + width)

    opt <- optim(
      par    = psi_mle,
      fn     = obj,
      method = "Brent",
      lower  = lower2,
      upper  = upper2
    )

    psi_hat <- opt$par
    k <- k + 1L
  }

  # 4. Evaluate nuisance optimizer and loglik at ψ̂
  mode_eval <- eval_psi_fun(psi_hat, theta_init)

  list(
    psi_hat        = psi_hat,
    theta_hat      = mode_eval$theta_hat,
    loglik_at_mode = mode_eval$branch_val
  )
}
