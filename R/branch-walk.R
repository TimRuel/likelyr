# ======================================================================
# INTERNAL: One-sided ψ sweep down to the cutoff
# ======================================================================

walk_branch_side <- function(
    increment,
    start,
    branch_cutoff,
    init_guess,
    eval_psi_fun,
    max_retries
) {

  current_psi <- start
  current_val <- Inf
  current_par <- init_guess

  df <- tibble::tibble(psi = numeric(), value = numeric())

  while (is.finite(current_val) && current_val >= branch_cutoff) {

    retry <- 0L

    repeat {
      eval <- eval_psi_fun(current_psi, current_par)

      # If the branch is dropping as expected or max retries hit → break
      if (eval$branch_val <= current_val || retry >= max_retries) break

      # Otherwise jitter the initial guess and re-evaluate
      retry <- retry + 1L
      scale <- 0.1 * retry

      current_par <- current_par + stats::rnorm(
        n  = length(current_par),
        sd = scale
      )
    }

    # If retries failed to restore monotonicity
    if (eval$branch_val > current_val) {
      warning(
        sprintf(
          "Monotonicity violation at psi=%.4f after %d retries; using fallback.",
          current_psi, retry
        ),
        call. = FALSE
      )
      eval <- eval_psi_fun(current_psi, current_par)
    }

    # Update branch progression
    current_val <- eval$branch_val
    current_par <- eval$theta_hat

    df <- dplyr::add_row(df, psi = current_psi, value = current_val)

    current_psi <- current_psi + increment
  }

  df |> dplyr::distinct() |> dplyr::arrange(.data$psi)
}
