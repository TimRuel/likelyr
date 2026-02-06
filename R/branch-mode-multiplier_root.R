# ======================================================================
# branch-mode-multiplier_root.R — FD Multiplier-Root Branch Mode Locator
# ======================================================================
#
# Branch mode location via root-finding on the *numerical derivative*
# of the ψ-profiled branch log-likelihood.
#
# This implementation is compatible with augmented Lagrangian solvers
# (e.g. nloptr::auglag), which do not expose true KKT multipliers.
#
# The “multiplier” is defined operationally as:
#   d/dψ ℓ_branch(ψ)
#
# Key stabilizers vs naïve FD:
#   • continuation (warm-start) across a ψ grid to stay on one branch
#   • anchored FD: solve at ψ first, then use θ̂(ψ) to evaluate ψ±ε
#   • local bracketing of the sign change before calling uniroot()
#
# ======================================================================

#' Multiplier-Root Branch Mode Locator (Finite-Difference)
#'
#' @description
#' Locates the branch mode by solving for the root of the numerical
#' derivative of the ψ-profiled branch log-likelihood.
#'
#' The derivative is approximated using a finite difference scheme
#' and does not rely on solver-exposed Lagrange multipliers.
#'
#' This method is fast near smooth interior modes but requires that the
#' constrained solver follow a *single* branch (handled here via warm
#' starts and an anchored FD derivative).
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
branch_mode_locator_multiplier_root <- function() {
  function(omega_hat) {
    # ---------------------------------------------------------------
    # Retrieve calibrated components from calling environment
    # ---------------------------------------------------------------
    branch_fn <- get("branch_fn", inherits = TRUE)
    search_interval <- get("search_interval", inherits = TRUE)
    param_init <- get("param_init", inherits = TRUE)

    lo <- search_interval[1L]
    hi <- search_interval[2L]

    # ---------------------------------------------------------------
    # FD step (scale-aware default)
    # ---------------------------------------------------------------
    fd_eps_opt <- getOption("likelyr.fd_eps", NULL)
    if (is.null(fd_eps_opt) || !is.finite(fd_eps_opt) || fd_eps_opt <= 0) {
      fd_eps <- max(1e-4, 1e-3 * (hi - lo))
    } else {
      fd_eps <- fd_eps_opt
    }

    # ---------------------------------------------------------------
    # Anchored numerical derivative:
    #   1) solve at ψ (warm-started) -> θ̂(ψ)
    #   2) evaluate at ψ±ε using θ̂(ψ) as init
    # Returns list(lambda, param_hat) so we can continue warm-starting.
    # ---------------------------------------------------------------
    eval_lambda_fd <- function(psi, param_curr) {
      # Solve at psi to anchor both sides
      out0 <- tryCatch(branch_fn(psi, param_curr), error = function(e) NULL)
      if (is.null(out0) || !is.finite(out0$branch_val)) {
        return(list(lambda = NA_real_, param_hat = param_curr))
      }

      theta0 <- out0$param_hat

      # --- Central difference (interior) ---
      if (psi - fd_eps > lo && psi + fd_eps < hi) {
        outL <- tryCatch(branch_fn(psi - fd_eps, theta0), error = function(e) {
          NULL
        })
        outR <- tryCatch(branch_fn(psi + fd_eps, theta0), error = function(e) {
          NULL
        })

        if (
          is.null(outL) ||
            is.null(outR) ||
            !is.finite(outL$branch_val) ||
            !is.finite(outR$branch_val)
        ) {
          return(list(lambda = NA_real_, param_hat = theta0))
        }

        return(list(
          lambda = (outR$branch_val - outL$branch_val) / (2 * fd_eps),
          param_hat = theta0
        ))
      }

      # --- Forward difference (left boundary) ---
      if (psi + fd_eps < hi) {
        outR <- tryCatch(branch_fn(psi + fd_eps, theta0), error = function(e) {
          NULL
        })

        if (is.null(outR) || !is.finite(outR$branch_val)) {
          return(list(lambda = NA_real_, param_hat = theta0))
        }

        return(list(
          lambda = (outR$branch_val - out0$branch_val) / fd_eps,
          param_hat = theta0
        ))
      }

      # --- Backward difference (right boundary) ---
      if (psi - fd_eps > lo) {
        outL <- tryCatch(branch_fn(psi - fd_eps, theta0), error = function(e) {
          NULL
        })

        if (is.null(outL) || !is.finite(outL$branch_val)) {
          return(list(lambda = NA_real_, param_hat = theta0))
        }

        return(list(
          lambda = (out0$branch_val - outL$branch_val) / fd_eps,
          param_hat = theta0
        ))
      }

      list(lambda = NA_real_, param_hat = theta0)
    }

    # ---------------------------------------------------------------
    # Build a local bracket for λ(ψ)=0 using a warm-started grid sweep
    # ---------------------------------------------------------------
    psi_grid <- make_coarse_psi_grid(interval = search_interval, n = 25)

    lambda_vals <- rep(NA_real_, length(psi_grid))
    param_curr <- param_init

    for (k in seq_along(psi_grid)) {
      tmp <- eval_lambda_fd(psi_grid[k], param_curr)
      lambda_vals[k] <- tmp$lambda

      # Continue from the anchored θ̂(ψ_k) even if lambda is NA
      # (theta0 is still the best local point we have)
      param_curr <- tmp$param_hat
    }

    # Determine first sign change on finite, nonzero values
    s <- sign(lambda_vals)
    ok <- is.finite(s) & s != 0
    s_ok <- s[ok]
    g_ok <- psi_grid[ok]

    if (length(s_ok) < 2L) {
      return(
        make_branch_mode_result(
          psi_hat = NA_real_,
          param_hat = param_init,
          loglik_at_mode = -Inf,
          status = "lambda_grid_failed"
        )
      )
    }

    idx <- which(diff(s_ok) != 0L)[1L]

    # ---------------------------------------------------------------
    # No sign change: boundary mode (derivative does not cross 0)
    # ---------------------------------------------------------------
    if (is.na(idx)) {
      if (all(lambda_vals <= 0, na.rm = TRUE)) {
        psi_hat <- lo
        out <- branch_fn(psi_hat, param_init)

        return(
          make_branch_mode_result(
            psi_hat = psi_hat,
            param_hat = out$param_hat,
            loglik_at_mode = out$branch_val,
            status = "boundary_left"
          )
        )
      }

      if (all(lambda_vals >= 0, na.rm = TRUE)) {
        psi_hat <- hi
        out <- branch_fn(psi_hat, param_init)

        return(
          make_branch_mode_result(
            psi_hat = psi_hat,
            param_hat = out$param_hat,
            loglik_at_mode = out$branch_val,
            status = "boundary_right"
          )
        )
      }

      # Mixed signs but no clean crossing after filtering -> too noisy
      return(
        make_branch_mode_result(
          psi_hat = NA_real_,
          param_hat = param_init,
          loglik_at_mode = -Inf,
          status = "lambda_no_bracket"
        )
      )
    }

    bracket <- c(g_ok[idx], g_ok[idx + 1L])

    # ---------------------------------------------------------------
    # Root finding on the *bracketed* interval
    # Use a stateful wrapper that warm-starts within uniroot evaluations.
    # ---------------------------------------------------------------
    param_state <- param_init
    f_root <- function(psi) {
      tmp <- eval_lambda_fd(psi, param_state)
      param_state <<- tmp$param_hat
      tmp$lambda
    }

    root <- tryCatch(
      {
        stats::uniroot(
          f = f_root,
          interval = bracket
        )
      },
      error = function(e) NULL
    )

    if (is.null(root) || !is.finite(root$root)) {
      return(
        make_branch_mode_result(
          psi_hat = NA_real_,
          param_hat = param_init,
          loglik_at_mode = -Inf,
          status = "multiplier_root_failed"
        )
      )
    }

    # ---------------------------------------------------------------
    # Final evaluation at ψ̂
    # ---------------------------------------------------------------
    psi_hat <- root$root
    out <- branch_fn(psi_hat, param_state)

    make_branch_mode_result(
      psi_hat = psi_hat,
      param_hat = out$param_hat,
      loglik_at_mode = out$branch_val,
      status = "success"
    )
  }
}
