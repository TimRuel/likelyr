# ======================================================================
# INTERNAL: Build a full branch (left + right sweep) on the ψ-grid
# ======================================================================

#' Build a Full Branch on the Psi Grid (Internal)
#'
#' @description
#' Constructs a full likelihood branch by sweeping both left and right from
#' integer grid indices adjacent to the branch mode. Sweeps are performed on
#' integer *k*-indices of the ψ-grid. ψ-values are recovered as:
#'
#'   ψ = ψ_MLE + k * increment
#'
#' @param psi_hat_branch Numeric scalar ψ̂ at branch mode.
#' @param theta_hat_branch θ̂ at the branch mode.
#' @param branch_cutoff Numeric. Mode value minus cutoff.
#' @param grid A ψ-grid anchor produced by `psi_grid_anchor()`.
#'   Must contain fields `psi_mle` and `increment`.
#' @param eval_psi_fun A function(psi_target, theta_init) → list(theta_hat, branch_val).
#' @param max_retries Maximum retry attempts for monotonicity enforcement.
#'
#' @return A data.frame with:
#'   k, psi, value, value_centered
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

  # Extract grid metadata
  psi_mle   <- grid$psi_mle
  increment <- grid$increment

  # Determine adjacent left/right k-indices
  adj <- get_adjacent_psi_points(
    psi_hat_branch,
    grid
  )

  # -------------------------------------------------------------
  # Left sweep (k decreasing)
  # -------------------------------------------------------------
  left <- walk_branch_side(
    grid          = grid,
    k_direction   = -1L,
    k_start       = adj$k_left,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # -------------------------------------------------------------
  # Right sweep (k increasing)
  # -------------------------------------------------------------
  right <- walk_branch_side(
    grid          = grid,
    k_direction   = +1L,
    k_start       = adj$k_right,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # -------------------------------------------------------------
  # Merge & assign ψ values
  # -------------------------------------------------------------
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
  attr(branch, "psi_MLE")    <- psi_mle

  branch
}


# ======================================================================
# INTERNAL: Generate Monte Carlo Branches (IL only)
# ======================================================================

#' Generate Monte Carlo Likelihood Branches (IL only)
#'
#' @description
#' Internal function used by `generate()` for integrated likelihood.
#'
#' @param cal A `calibrated_model` from calibrate().
#' @param verbose Logical.
#'
#' @return list(branches = list-of-branches, omega_draws = list-of-ω̂)
#' @keywords internal
#' @noRd
generate_branches <- function(cal, verbose = TRUE) {

  # -------------------------------------------------------------------
  # 0. Sanity checks
  # -------------------------------------------------------------------
  if (!inherits(cal, "calibrated_model"))
    stop("generate_branches() requires a calibrated_model.", call. = FALSE)

  lik       <- cal$likelihood
  estimand  <- cal$estimand
  optimizer <- cal$optimizer
  execution <- cal$execution

  # IL-specific components: calibrate() must have added them
  il <- cal$il
  if (is.null(il) ||
      is.null(il$generate_init) ||
      is.null(il$sample_omega_hat))
    stop("IL calibration must provide cal$il$generate_init and cal$il$sample_omega_hat.",
         call. = FALSE)

  # -------------------------------------------------------------------
  # 1. Parallel mode detection
  # -------------------------------------------------------------------
  is_parallel <- inherits(execution, "parallel_spec")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() requires an active future backend.\n",
      "Call: future::plan(multisession, workers = execution$num_workers)",
      call. = FALSE
    )
  }

  `%op%` <- if (is_parallel) `%dofuture%` else `%do%`

  # -------------------------------------------------------------------
  # 2. Determine number of Monte Carlo branches
  # -------------------------------------------------------------------
  if (is_parallel) {
    R <- execution$num_workers * execution$chunk_size
  } else {
    R <- execution$R
  }

  # -------------------------------------------------------------------
  # 3. Compute cutoff
  # -------------------------------------------------------------------
  alpha_target <- min(1 - estimand$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  psi_mle   <- cal$psi_mle
  theta_mle <- cal$theta_mle
  increment <- cal$increment

  # Build ψ-grid anchor structure
  grid <- psi_grid_anchor(psi_mle, increment)

  # Foreach configuration
  seed_opt   <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list   <- execution$packages %||% character()

  # -------------------------------------------------------------------
  # 4. Zero-allocation ψ-conditional optimizer factory
  # -------------------------------------------------------------------
  eval_psi_builder <- build_eval_psi_fun(cal)

  # -------------------------------------------------------------------
  # 5. MAIN LOOP
  # -------------------------------------------------------------------
  result <- foreach::foreach(
    r = seq_len(R),
    .packages = pkg_list,
    .options.future = list(
      seed       = seed_opt,
      chunk.size = chunk_size
    )
  ) %op% {

    # (1) Draw ω̂
    init      <- il$generate_init()
    omega_hat <- il$sample_omega_hat(init)

    # (2) Build ψ-evaluator for this draw
    eval_psi_fun <- eval_psi_builder(omega_hat)

    # (3) Solve branch mode
    mode_obj <- branch_mode_solve(
      psi_mle         = psi_mle,
      eval_psi_fun    = eval_psi_fun,
      theta_init      = theta_mle,
      search_interval = cal$search_interval
    )

    psi_hat_branch   <- mode_obj$psi_hat
    loglik_at_mode   <- mode_obj$loglik_at_mode
    theta_hat_branch <- mode_obj$theta_hat

    # (4) Critical cutoff
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

    list(
      branch    = branch,
      omega_hat = omega_hat
    )
  }

  # -------------------------------------------------------------------
  # 6. Collect output
  # -------------------------------------------------------------------
  branches   <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  list(
    branches    = branches,
    omega_draws = omega_hats
  )
}
