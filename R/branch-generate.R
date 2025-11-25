# ======================================================================
# INTERNAL: Build a full branch (left + right sweep) on the ψ-grid
# ======================================================================

#' Build a Full Branch on the Psi Grid (Internal)
#'
#' @description
#' Constructs a full likelihood branch by sweeping both left and right from
#' integer grid indices adjacent to the branch mode. Sweeps are performed on
#' the *k*-grid (integer index), and ψ-values are recovered as:
#'   ψ = ψ_MLE + k * increment.
#'
#' @param psi_MLE Numeric scalar. Global ψ MLE.
#' @param increment Grid spacing between ψ values.
#' @param k_left_start Integer index for starting the left sweep.
#' @param k_right_start Integer index for starting the right sweep.
#' @param branch_cutoff Numeric. Mode loglik minus cutoff.
#' @param init_guess Initial θ vector for optimization.
#' @param max_retries Maximum retries during monotonicity enforcement.
#' @param eval_psi_fun Function of the form f(psi, theta_init) → list(theta_hat, branch_val).
#'
#' @return A data.frame with columns:
#'   \itemize{
#'     \item `k` Integer grid indices.
#'     \item `value` Loglikelihood at each grid point.
#'     \item `psi` Grid-aligned ψ values.
#'     \item `value_centered` Loglik minus max(loglik).
#'   }
#'
#' @keywords internal
#' @noRd
build_one_branch <- function(
    psi_hat_branch,
    theta_hat_branch,
    branch_cutoff,
    grid,
    eval_psi_fun,
    max_retries
) {

  adj <- get_adjacent_psi_points(
    psi_hat_branch,
    grid
  )

  # Walk left side (k decreasing)
  left <- walk_branch_side(
    grid          = grid,
    k_direction   = -1L,
    k_start       = adj$k_left,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Walk right side (k increasing)
  right <- walk_branch_side(
    grid          = grid,
    k_direction   = +1L,
    k_start       = adj$k_right,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Merge + assign ψ-values
  branch <- dplyr::bind_rows(left, right) |>
    dplyr::mutate(
      psi = psi_mle + k * increment
    ) |>
    dplyr::arrange(.data$psi) |>
    dplyr::mutate(
      value_centered = value - max(value)
    )

  # Metadata
  attr(branch, "mode_index") <- which.max(branch$value)
  attr(branch, "n_points")   <- nrow(branch)
  attr(branch, "psi_MLE")    <- grid$psi_mle

  branch
}



# ======================================================================
# INTERNAL: Generate Monte Carlo Branches (IL only)
# ======================================================================

#' Generate Monte Carlo Likelihood Branches (IL only)
#'
#' @description
#' Internal function used by `generate()` for integrated likelihood
#' computation. For each Monte Carlo draw, this:
#'
#' \enumerate{
#'   \item draws a nuisance parameter \eqn{\omega} satisfying ψ(ω) = ψ_MLE,
#'   \item constructs an evaluation function for ψ-constrained θ-optimization,
#'   \item solves for the branch mode,
#'   \item descends left and right on the ψ-grid until cutoff.
#' }
#'
#' @param cal An `IL_calibration` object produced by \code{\link{calibrate}}.
#' @param verbose Logical; print progress output.
#'
#' @return A list with components:
#' \describe{
#'   \item{branches}{List of branch data.frames.}
#'   \item{omega_draws}{List of nuisance parameter draws.}
#' }
#'
#' @details
#' This function is **not** user-facing. It is called internally by
#' \code{\link{generate}} when `type = "IL"`.
#'
#' @keywords internal
#' @noRd
#'
## If you want this available to users, add @export.
## But generally it should stay internal.
generate_branches <- function(cal, verbose = TRUE) {

  # Sanity checks
  if (!inherits(cal, "IL_calibration"))
    stop("generate_branches() requires an IL_calibration object.", call. = FALSE)

  wf        <- cal$workflow
  optimizer <- wf$optimizer
  estimand  <- wf$estimand
  execution <- wf$execution

  # IL-specific components installed by calibrate_IL()
  il <- cal$il
  if (is.null(il) ||
      is.null(il$generate_init) ||
      is.null(il$sample_omega_hat)) {
    stop("IL calibration must supply $il$generate_init and $il$sample_omega_hat.",
         call. = FALSE)
  }

  # Parallel mode detection
  is_parallel <- inherits(execution, "likelihood_parallel")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() was provided but no parallel backend is active.\n",
      "Call: future::plan(multisession, workers = execution$num_workers)\n",
      "BEFORE calling generate().",
      call. = FALSE
    )
  }

  `%op%` <- if (is_parallel) `%dofuture%` else `%do%`

  # Effective number of MC branches
  if (is_parallel) {
    R <- execution$num_workers * execution$chunk_size
  } else {
    R <- execution$R
  }

  if (verbose) {
    cat("\n[generate_branches] Mode:",
        if (is_parallel) "PARALLEL" else "SERIAL",
        "| Branches:", R, "\n")
  }

  # Critical cutoff height
  alpha_target <- estimand$alpha_target
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  increment <- estimand$increment
  psi_mle   <- cal$psi_mle
  theta_mle <- cal$theta_mle

  # Build ψ-grid anchor structure
  grid <- psi_grid_anchor(psi_mle, increment)

  # Parallel foreach options
  seed_opt   <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list   <- execution$packages %||% character()

  # Zero-closure ψ-evaluator builder
  eval_psi_builder <- build_eval_psi_fun(cal)

  # MAIN LOOP
  result <- foreach::foreach(
    r = seq_len(R),
    .packages = pkg_list,
    .options.future = list(
      seed       = seed_opt,
      chunk.size = chunk_size
    )
  ) %op% {

    # (1) Draw ω̂ on ψ(ω̂)=ψ_MLE manifold
    init      <- il$generate_init()
    omega_hat <- il$sample_omega_hat(init)

    # (2) Build ψ-evaluator for this ω̂
    eval_psi_fun <- eval_psi_builder(omega_hat)

    # (3) Solve branch mode
    mode_obj <- branch_mode_solve(
      psi_mle         = psi_mle,
      eval_psi_fun    = eval_psi_fun,
      theta_init      = theta_mle,
      search_interval = estimand$search_interval
    )

    psi_hat_branch   <- mode_obj$psi_hat
    loglik_at_mode   <- mode_obj$loglik_at_mode
    theta_hat_branch <- mode_obj$theta_hat

    # (4) Branch cutoff
    branch_cutoff <- loglik_at_mode - crit

    # (5) Build full branch
    branch <- build_one_branch(
      psi_hat_branch   = psi_hat_branch,
      theta_hat_branch = theta_hat_branch,
      branch_cutoff    = branch_cutoff,
      grid             = grid,
      eval_psi_fun     = eval_psi_fun,
      max_retries      = optimizer$max_retries
    )

    list(branch = branch, omega_hat = omega_hat)
  }

  # Combine results
  branches   <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  if (verbose) cat("[generate_branches] Finished.\n")

  list(
    branches    = branches,
    omega_draws = omega_hats
  )
}
