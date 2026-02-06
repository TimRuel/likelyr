# ======================================================================
# branch-mode-hybrid.R — Hybrid Branch Mode Locator
# ======================================================================
#
# Default branch mode locator:
#   • coarse ψ grid scan to locate dominant region
#   • local bracketing around grid maximum
#   • Brent refinement for continuous maximization
#
# This method prioritizes robustness and correctness while achieving
# near-Brent performance in smooth cases.
# ======================================================================

#' Hybrid Branch Mode Locator
#'
#' @description
#' Locates the branch mode using a hybrid strategy:
#'
#' \enumerate{
#'   \item Perform a coarse grid scan over ψ to identify the dominant region.
#'   \item Construct a local bracketing interval around the grid maximum.
#'   \item Refine the solution using Brent maximization.
#' }
#'
#' The returned locator is a function of \code{omega_hat} that produces
#' a standardized branch mode object.
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
branch_mode_locator_hybrid <- function() {
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
    # 1. Coarse grid scan
    # ---------------------------------------------------------------
    psi_grid <- make_coarse_psi_grid(
      interval = search_interval,
      n = 25
    )

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
    # 2. Build and validate local bracket
    # ---------------------------------------------------------------
    bracket <- validate_psi_bracket(
      build_local_bracket(psi_grid, idx)
    )

    # ---------------------------------------------------------------
    # 3. Brent refinement
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
          interval = bracket,
          maximum = TRUE
        )
      },
      error = function(e) NULL
    )

    if (is.null(opt) || !is.finite(opt$objective)) {
      # Fallback: use best grid point
      psi_hat <- psi_grid[idx]
      out <- branch_fn(psi_hat, param_init)

      return(
        make_branch_mode_result(
          psi_hat = psi_hat,
          param_hat = out$param_hat,
          loglik_at_mode = out$branch_val,
          status = "brent_failed_fallback"
        )
      )
    }

    # ---------------------------------------------------------------
    # 4. Final evaluation at ψ̂
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
