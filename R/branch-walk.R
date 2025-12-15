#' One-Sided Branch Sweep Along the ψ-Grid (Internal)
#'
#' @description
#' Performs a one-sided sweep of the branch log-likelihood by stepping
#' through integer grid indices k, computing ψ_k from the supplied ψ-grid,
#' and calling:
#'
#'   eval_psi_fun(psi_k, theta_init)
#'
#' @param grid A psi_grid object created by psi_grid_anchor().
#' @param k_direction +1 (right sweep) or -1 (left sweep)
#' @param k_start Integer index where sweeping begins
#' @param branch_cutoff Numeric threshold for terminating the sweep
#' @param init_guess Initial θ̂ used at ψ_MLE
#' @param eval_psi_fun Function(psi_value, theta_init) → list(theta_hat, branch_val)
#' @param max_retries Integer number of jitter retries for monotonicity
#'
#' @return A tibble with columns k and loglik, sorted by k.
#'
#' @keywords internal
walk_branch_side <- function(
    grid,
    k_direction,
    k_start,
    branch_cutoff,
    init_guess,
    eval_psi_fun,
    max_retries
) {

  k_curr      <- k_start
  current_par <- init_guess
  current_val <- Inf

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {

    retry <- 0L

    # Convert k → psi
    psi_k <- grid$psi_mle + k_curr * grid$increment

    # --------------------------------------------------------------
    # Evaluate, retrying with jitter if monotonicity violated
    # --------------------------------------------------------------
    repeat {
      eval <- eval_psi_fun(psi_k, current_par)

      if (eval$branch_val <= current_val || retry >= max_retries)
        break

      # Monotonicity violated: jitter initial guess
      retry <- retry + 1L
      scale <- 0.1 * retry

      current_par <- current_par + stats::rnorm(
        n  = length(current_par),
        sd = scale
      )

    }

    # Final fallback: if monotonicity still violated
    if (eval$branch_val > current_val && max_retries > 0L) {
      warning(
        sprintf(
          "Monotonicity violation at grid index k=%d after %d retries; using fallback.",
          k_curr, retry
        ),
        call. = FALSE
      )
      eval <- eval_psi_fun(psi_k, current_par)
    }

    # --------------------------------------------------------------
    # Stop if branch value too small or non-finite
    # --------------------------------------------------------------
    current_val <- eval$branch_val

    if (!is.finite(current_val) || current_val < branch_cutoff)
      break

    # Update θ̂
    current_par <- eval$theta_hat

    # Record this step
    df <- dplyr::add_row(df, k = k_curr, loglik = current_val)

    # Next grid index
    k_curr <- k_curr + k_direction
  }

  dplyr::distinct(df) |> dplyr::arrange(.data$k)
}
