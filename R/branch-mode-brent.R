# ======================================================================
# branch-mode-brent.R — Brent Branch Mode Locator
# ======================================================================
#
# Direct continuous maximization of the branch log-likelihood in ψ
# using Brent's method. Assumes unimodality over the search interval.
#
# This method is fast but less robust than the hybrid locator.
# ======================================================================

#' Brent Branch Mode Locator
#'
#' @description
#' Locates the branch mode by directly maximizing the ω̂-conditioned
#' branch log-likelihood over ψ using Brent's method.
#'
#' This method assumes the branch log-likelihood is unimodal over the
#' supplied search interval. No coarse bracketing is performed.
#'
#' @return
#' A function with signature:
#' \preformatted{
#'   function(omega_hat) -> list(
#'     psi_hat,
#'     param_hat,
#'     loglik_at_mode,
#'     status
#'   )
#' }
#'
#' @keywords internal
branch_mode_locator_brent <- function() {
  # -------------------------------------------------------------------
  # Return locator: function(omega_hat) → mode object
  # -------------------------------------------------------------------
  function(omega_hat) {
    # ---------------------------------------------------------------
    # Retrieve calibrated components from calling environment
    # ---------------------------------------------------------------
    branch_fn <- get("branch_fn", inherits = TRUE)
    psi_init <- get("psi_init", inherits = TRUE)
    search_interval <- get("search_interval", inherits = TRUE)
    param_init <- get("param_init", inherits = TRUE)

    # ---------------------------------------------------------------
    # Brent maximization
    # ---------------------------------------------------------------
    opt <- tryCatch(
      {
        stats::optimize(
          f = function(psi) {
            safe_eval_branch(
              psi = psi,
              param_init = param_init,
              branch_fn = branch_fn
            )
          },
          interval = search_interval,
          maximum = TRUE
        )
      },
      error = function(e) NULL
    )

    if (is.null(opt) || !is.finite(opt$objective)) {
      return(
        make_branch_mode_result(
          psi_hat = NA_real_,
          param_hat = param_init,
          loglik_at_mode = -Inf,
          status = "brent_failed"
        )
      )
    }

    # ---------------------------------------------------------------
    # Final evaluation at ψ̂
    # ---------------------------------------------------------------
    psi_hat <- opt$maximum
    out <- branch_fn(psi_hat, param_init)

    make_branch_mode_result(
      psi_hat = psi_hat,
      param_hat = out$param_hat,
      loglik_at_mode = out$branch_val,
      status = "success"
    )
  }
}
