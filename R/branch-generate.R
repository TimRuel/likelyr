# ======================================================================
# INTERNAL: Build a full branch (left + right sweep) on the ψ-grid
# ======================================================================

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

  # Left sweep
  left <- walk_branch_side(
    grid          = grid,
    k_direction   = -1L,
    k_start       = adj$k_left,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Right sweep
  right <- walk_branch_side(
    grid          = grid,
    k_direction   = +1L,
    k_start       = adj$k_right,
    branch_cutoff = branch_cutoff,
    init_guess    = theta_hat_branch,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Merge & assign ψ values
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

generate_branches <- function(cal, verbose = TRUE) {

  if (!inherits(cal, "calibrated_model"))
    stop("generate_branches() requires a calibrated_model.", call. = FALSE)

  lik       <- cal$likelihood
  estimand  <- cal$estimand
  optimizer <- cal$optimizer
  execution <- cal$execution

  # -------------------------------------------------------------------
  # IL components now live in cal$results$IL (temporary storage)
  # -------------------------------------------------------------------
  il <- cal$results$IL
  if (is.null(il) ||
      is.null(il$generate_init) ||
      is.null(il$sample_omega_hat))
    stop("integrate() must set cal$results$IL$generate_init and $sample_omega_hat",
         call. = FALSE)

  # -------------------------------------------------------------------
  # Parallel mode detection
  # -------------------------------------------------------------------
  is_parallel <- inherits(execution, "parallel_spec")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop("parallel_spec() requires a future backend. Call: future::plan(multisession)",
         call. = FALSE)
  }

  `%op%` <- if (is_parallel) `%dofuture%` else `%do%`

  # -------------------------------------------------------------------
  # Number of Monte Carlo branches
  # -------------------------------------------------------------------
  R <- execution$total_branches

  # -------------------------------------------------------------------
  # Cutoff value
  # -------------------------------------------------------------------
  alpha_target <- min(1 - estimand$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  # Updated locations of calibrated quantities
  psi_mle     <- estimand$psi_mle
  theta_mle   <- lik$theta_mle
  increment   <- estimand$increment
  interval    <- estimand$search_interval

  # ψ-grid anchor
  grid <- psi_grid_anchor(
    psi_mle   = psi_mle,
    increment = increment
  )

  # foreach configuration
  seed_opt   <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list   <- execution$packages %||% character()

  # -------------------------------------------------------------------
  # Zero-allocation ψ-conditional optimizer factory
  # -------------------------------------------------------------------
  eval_psi_builder <- build_eval_psi_fun(cal)

  # -------------------------------------------------------------------
  # MAIN LOOP: R Monte Carlo branches
  # -------------------------------------------------------------------
  result <- foreach::foreach(
    r = seq_len(R),
    .packages       = pkg_list,
    .options.future = list(
      seed       = seed_opt,
      chunk.size = chunk_size
    )
  ) %op% {

    # (1) Draw ω̂
    init      <- il$generate_init()
    omega_hat <- il$sample_omega_hat(init)

    # (2) Build ψ evaluator for ω̂
    eval_psi_fun <- eval_psi_builder(omega_hat)

    # (3) Solve branch mode
    mode_obj <- branch_mode_solve(
      psi_mle         = psi_mle,
      eval_psi_fun    = eval_psi_fun,
      theta_init      = theta_mle,
      search_interval = interval
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
  # 6. Collect results
  # -------------------------------------------------------------------
  branches   <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  list(
    branches    = branches,
    omega_draws = omega_hats
  )
}
