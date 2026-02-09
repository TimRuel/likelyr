#' One-Sided Branch Sweep Along the ψ-Grid (Internal)
#'
#' @keywords internal
walk_branch_side <- function(
  grid,
  k_direction,
  k_start,
  branch_cutoff,
  init_guess,
  branch_fn,
  max_retries,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE
) {
  k_curr <- k_start
  current_par <- init_guess
  current_val <- Inf

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {
    retry <- 0L

    # --------------------------------------------------------------
    # Convert k → ψ
    # --------------------------------------------------------------
    psi_k <- grid$psi_mle + k_curr * grid$increment

    # --------------------------------------------------------------
    # Geometry: ψ bounds
    # --------------------------------------------------------------
    hit_lower <- !is.null(psi_lower) && psi_k < psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k > psi_upper

    if (hit_lower || hit_upper) {
      if (!stop_at_bounds) {
        # ignore geometry, continue
      } else {
        if (eval_at_bounds) {
          psi_k <- if (hit_lower) psi_lower else psi_upper
        } else {
          break
        }
      }
    }

    # --------------------------------------------------------------
    # Evaluate, retrying with jitter if monotonicity violated
    # --------------------------------------------------------------
    repeat {
      eval <- branch_fn(psi_k, current_par)

      if (eval$branch_val <= current_val || retry >= max_retries) {
        break
      }

      retry <- retry + 1L
      scale <- 0.1 * retry

      current_par <- current_par +
        stats::rnorm(
          n = length(current_par),
          sd = scale
        )
    }

    # Final fallback if monotonicity still violated
    if (eval$branch_val > current_val && max_retries > 0L) {
      warning(
        sprintf(
          "Monotonicity violation at grid index k=%d after %d retries; using fallback.",
          k_curr,
          retry
        ),
        call. = FALSE
      )
      eval <- branch_fn(psi_k, current_par)
    }

    # --------------------------------------------------------------
    # Update
    # --------------------------------------------------------------
    current_val <- eval$branch_val

    if (!is.finite(current_val)) {
      stop(
        "walk_branch_side(): Non-finite log-likelihood at k = ",
        k_curr,
        " (value = ",
        current_val,
        ").",
        call. = FALSE
      )
    }

    df <- dplyr::add_row(df, k = k_curr, loglik = current_val)

    # --------------------------------------------------------------
    # Likelihood cutoff
    # --------------------------------------------------------------
    if (!is.null(branch_cutoff) && current_val < branch_cutoff) {
      break
    }

    # --------------------------------------------------------------
    # If we evaluated exactly at a bound, stop after recording
    # --------------------------------------------------------------
    if (stop_at_bounds && (hit_lower || hit_upper)) {
      break
    }

    # --------------------------------------------------------------
    # Prepare next step
    # --------------------------------------------------------------
    current_par <- eval$param_hat
    k_curr <- k_curr + k_direction
  }

  dplyr::distinct(df) |>
    dplyr::arrange(.data$k)
}
