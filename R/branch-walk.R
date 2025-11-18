# Internal: walk one side of a branch
# Notes:
#  - eval_psi_fun must return list(branch_val, theta_hat)
#  - branch_cutoff is raw-likelihood cutoff (not centered)
#  - returns *raw* values only; centering done later
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

  branch_df <- tibble::tibble(
    psi   = numeric(0),
    value = numeric(0)
  )

  while (is.finite(current_val) && current_val >= branch_cutoff) {

    retry <- 0

    repeat {
      eval <- eval_psi_fun(current_psi, current_par)

      if (eval$branch_val <= current_val || retry >= max_retries)
        break

      retry <- retry + 1
      jitter_scale <- 0.1 * retry
      current_par  <- current_par + stats::rnorm(
        n  = length(current_par),
        sd = jitter_scale
      )
    }

    # If still monotonicity-violating, accept but warn and re-evaluate
    if (eval$branch_val > current_val) {
      warning(
        sprintf(
          "Monotonicity violation at psi = %.4f after %d retries; using non-monotone value.",
          current_psi, retry
        ),
        call. = FALSE
      )
      eval <- eval_psi_fun(current_psi, current_par)
    }

    current_val <- eval$branch_val
    current_par <- eval$theta_hat

    branch_df <- dplyr::bind_rows(
      branch_df,
      tibble::tibble(
        psi   = current_psi,
        value = current_val
      )
    )

    current_psi <- current_psi + increment
  }

  branch_df |>
    dplyr::distinct() |>
    dplyr::arrange(.data$psi)
}
