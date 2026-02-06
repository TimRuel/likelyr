# ======================================================================
# branch-mode-grid_scan.R — Grid-Scan Branch Mode Locator
# ======================================================================
#
# Pure grid-based branch mode location.
#
# This method evaluates the branch log-likelihood on a coarse ψ grid
# and selects the maximizer directly. No refinement is performed.
#
# Intended for robustness, debugging, and validation.
# ======================================================================

#' Grid-Scan Branch Mode Locator
#'
#' @description
#' Locates the branch mode by evaluating the ω̂-conditioned branch
#' log-likelihood on a coarse ψ grid and selecting the maximizer.
#'
#' No refinement or smoothness assumptions are made.
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
branch_mode_locator_grid_scan <- function() {
  # -------------------------------------------------------------------
  # Return locator: function(omega_hat) → mode object
  # -------------------------------------------------------------------
  function(omega_hat) {
    # ---------------------------------------------------------------
    # Retrieve calibrated components from calling environment
    # ---------------------------------------------------------------
    branch_fn <- get("branch_fn", inherits = TRUE)
    search_interval <- get("search_interval", inherits = TRUE)
    param_init <- get("param_init", inherits = TRUE)

    # ---------------------------------------------------------------
    # Build coarse ψ grid
    # ---------------------------------------------------------------
    psi_grid <- make_coarse_psi_grid(
      interval = search_interval,
      n = 50L
    )

    # ---------------------------------------------------------------
    # Evaluate branch log-likelihood on grid
    # ---------------------------------------------------------------
    vals <- vapply(
      psi_grid,
      safe_eval_branch,
      numeric(1),
      param_init = param_init,
      branch_fn = branch_fn
    )

    idx <- safe_which_max(vals)

    if (is.na(idx)) {
      return(
        make_branch_mode_result(
          psi_hat = NA_real_,
          param_hat = param_init,
          loglik_at_mode = -Inf,
          status = "grid_scan_failed"
        )
      )
    }

    # ---------------------------------------------------------------
    # Final evaluation at ψ̂
    # ---------------------------------------------------------------
    psi_hat <- psi_grid[idx]
    out <- branch_fn(psi_hat, param_init)

    make_branch_mode_result(
      psi_hat = psi_hat,
      param_hat = out$param_hat,
      loglik_at_mode = out$branch_val,
      status = "success"
    )
  }
}
