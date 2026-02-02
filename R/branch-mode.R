#' Solve for the Continuous Branch Mode ψ̂ (Internal)
#'
#' @description
#' Finds the **branch-specific mode** \eqn{\hat{\psi}} by maximizing the
#' branch log-likelihood over a *continuous* ψ domain for a **fixed branch**
#' uniquely identified by \code{omega_hat}.
#'
#' The branch evaluator \code{eval_psi_fun(psi, param_start)} is assumed to:
#' \itemize{
#'   \item solve the nuisance optimization at fixed ψ, starting from
#'         \code{param_start};
#'   \item return a list containing:
#'     \itemize{
#'       \item \code{param_hat} — the nuisance optimizer \eqn{\hat{\theta}(\psi)};
#'       \item \code{branch_val} — the log-likelihood evaluated at
#'         \eqn{\hat{\theta}(\psi)}.
#'     }
#' }
#'
#' This function then solves the one-dimensional optimization problem
#' \deqn{
#'   \hat{\psi}
#'   =
#'   \arg\max_{\psi \in \mathcal{I}}
#'   \; \ell\bigl(\hat{\theta}(\psi)\bigr),
#' }
#' using a Brent optimizer over the user-supplied interval.
#'
#' Crucially, nuisance parameters are **not frozen** during the ψ search.
#' Instead, nuisance optimization is warm-started using a continuation
#' strategy that tracks \eqn{\hat{\theta}(\psi)} across nearby ψ values.
#'
#' @param eval_psi_fun Function with signature
#'   \code{f(psi, param_start)} returning a list with components
#'   \code{param_hat} and \code{branch_val}.
#'
#' @param psi_init Numeric scalar giving the initial ψ value for the Brent
#'   optimizer. This value is used **only as a numerical starting point** and
#'   does not otherwise constrain or bias the ψ search. In typical usage,
#'   \code{psi_init} equals \eqn{g(\hat{\omega}_\theta)} for the branch-defining
#'   \code{omega_hat}.
#'
#' @param omega_hat Nuisance parameter vector (or structure containing it)
#'   defining the branch. This value is used as the **required initial
#'   nuisance start** for the first ψ evaluation and must be valid input to
#'   the inner optimizer (e.g., \code{auglag()}).
#'
#' @param search_interval Numeric vector of length 2 giving the lower and upper
#'   bounds of the admissible ψ domain.
#'
#' @param max_iter Integer giving the maximum number of Brent iterations.
#'   Default is 20.
#'
#' @param tol Numeric convergence tolerance passed to the Brent optimizer.
#'   Default is \code{1e-6}.
#'
#' @return A list with components:
#' \describe{
#'   \item{psi_hat}{Continuous branch mode \eqn{\hat{\psi}}.}
#'   \item{param_hat}{Nuisance optimizer \eqn{\hat{\theta}} evaluated at
#'     \code{psi_hat}.}
#'   \item{loglik_at_mode}{Branch log-likelihood at the mode.}
#' }
#'
#' @details
#' This routine is designed to be a **cheap, branch-consistent anchor**
#' for subsequent branch construction via \code{generate_branches()}.
#' It deliberately avoids ψ-grid construction, boundary expansion heuristics,
#' or proto-branch sweeps.
#'
#' For fixed \code{omega_hat}, the returned \code{psi_hat} is intended to be
#' invariant to:
#' \itemize{
#'   \item nuisance initialization beyond \code{omega_hat};
#'   \item small perturbations of \code{psi_init};
#'   \item optimizer path details.
#' }
#'
#' The resulting ψ̂ need not equal the global ψ MLE and is not snapped to a
#' ψ grid. Grid alignment and left/right continuation are handled later during
#' branch generation.
#'
#' @keywords internal
branch_mode_solve <- function(
  eval_psi_fun,
  psi_init,
  omega_hat,
  search_interval,
  max_iter = 20L,
  tol = 1e-6
) {
  lower <- search_interval[1]
  upper <- search_interval[2]

  # Internal continuation state
  last_psi <- psi_init
  last_param <- omega_hat # REQUIRED: auglag-safe initial start

  eval_branch <- function(psi) {
    # Always provide a valid start to auglag()
    # Use continuation when ψ is nearby
    param_start <- if (!is.null(last_psi) && abs(psi - last_psi) < 0.1) {
      last_param
    } else {
      omega_hat
    }

    res <- eval_psi_fun(psi, param_start)

    # Update continuation state
    last_psi <<- psi
    last_param <<- res$param_hat

    -res$branch_val
  }

  opt <- optim(
    par = psi_init,
    fn = eval_branch,
    method = "Brent",
    lower = lower,
    upper = upper,
    control = list(maxit = max_iter, reltol = tol)
  )

  # Final evaluation at ψ̂ (consistent nuisance)
  final <- eval_psi_fun(opt$par, last_param)

  list(
    psi_hat = opt$par,
    param_hat = final$param_hat,
    loglik_at_mode = final$branch_val
  )
}
