# ======================================================================
# INTERNAL: Build a full branch (left + right sweep)
# ======================================================================

build_one_branch <- function(
    psi_MLE,
    increment,
    left_start,
    right_start,
    branch_cutoff,
    init_guess,
    max_retries,
    eval_psi_fun
) {

  # ---- Walk left side ----
  left <- walk_branch_side(
    increment     = -increment,
    start         = left_start,
    branch_cutoff = branch_cutoff,
    init_guess    = init_guess,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # ---- Walk right side ----
  right <- walk_branch_side(
    increment     = increment,
    start         = right_start,
    branch_cutoff = branch_cutoff,
    init_guess    = init_guess,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # ---- Merge + post-process ----
  branch <- dplyr::bind_rows(left, right) |>
    dplyr::arrange(.data$psi) |>
    dplyr::mutate(
      k              = round((psi - psi_MLE) / increment),
      value_centered = value - max(value)
    )

  # Metadata
  attr(branch, "mode_index") <- which.max(branch$value)
  attr(branch, "n_points")   <- nrow(branch)
  attr(branch, "psi_MLE")    <- psi_MLE

  branch
}


# ======================================================================
# Internal: Generate Monte Carlo Branches (Serial or Parallel)
# Called only from fit_integrated()
# ======================================================================

generate_branches <- function(cal, verbose = TRUE) {

  wf        <- cal$workflow
  model     <- wf$model
  nuisance  <- wf$nuisance
  optimizer <- wf$optimizer
  estimand  <- wf$estimand
  execution <- wf$execution

  # ------------------------------------------------------------------
  # Parallel mode detection
  # ------------------------------------------------------------------
  is_parallel <- inherits(execution, "likelihood_parallel")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() was provided but no parallel backend is active.\n",
      "Call: future::plan(multisession, workers = execution$num_workers)\n",
      "BEFORE calling fit_integrated().",
      call. = FALSE
    )
  }

  # Select foreach operator
  `%op%` <- if (is_parallel) `%dofuture%` else `%do%`

  # Branch count R
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

  # ------------------------------------------------------------------
  # Compute branch cutoff critical value
  # ------------------------------------------------------------------
  alpha_target <- estimand$alpha_target
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  increment <- estimand$increment
  psi_mle <- cal$psi_mle
  theta_mle <- cal$theta_mle

  # ------------------------------------------------------------------
  # Option lists for foreach
  # ------------------------------------------------------------------
  seed_opt   <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list   <- execution$packages %||% character()

  # ------------------------------------------------------------------
  # Prebuild the ψ–optimizer factory (depends on model + estimand + optimizer)
  # ------------------------------------------------------------------
  eval_psi_builder <- build_eval_psi_fun(cal)

  # ------------------------------------------------------------------
  # Foreach loop for branch computation
  # ------------------------------------------------------------------
  result <- foreach::foreach(
    r = seq_len(R),
    .packages = pkg_list,
    .options.future = list(
      seed       = seed_opt,
      chunk.size = chunk_size
    )
  ) %op% {

    # --------------------------------------------------------------
    # 1. Draw nuisance element ω̂
    # --------------------------------------------------------------
    omega_hat <- nuisance$sampler(model$param_dim)
    print(omega_hat)
    # --------------------------------------------------------------
    # 2. Build ψ-evaluator for this ω̂
    # --------------------------------------------------------------
    eval_psi_fun <- eval_psi_builder(omega_hat)
    # --------------------------------------------------------------
    # 3. Branch mode solve: find (θ̂, ψ̂, loglik at mode)
    # --------------------------------------------------------------
    mode_obj <- branch_mode_solve(
      psi_mle         = psi_mle,
      eval_psi_fun    = eval_psi_fun,
      theta_init      = theta_mle,
      search_interval = estimand$search_interval
    )

    psi_hat_branch   <- mode_obj$psi_hat
    loglik_at_mode   <- mode_obj$loglik_at_mode
    theta_hat_branch <- mode_obj$theta_hat

    # --------------------------------------------------------------
    # 4. Branch cutoff for descending each side
    # --------------------------------------------------------------
    branch_cutoff <- loglik_at_mode - crit

    # --------------------------------------------------------------
    # 5. Determine nearest grid points for left/right start
    # --------------------------------------------------------------
    adj <- get_adjacent_psi_points(
      psi_hat_branch = psi_hat_branch,
      psi_mle        = psi_mle,
      increment      = increment
    )

    # --------------------------------------------------------------
    # 6. Build full branch (left + right)
    # --------------------------------------------------------------
    branch <- build_one_branch(
      psi_MLE       = psi_mle,
      increment     = increment,
      left_start    = adj$left,
      right_start   = adj$right,
      branch_cutoff = branch_cutoff,
      init_guess    = theta_hat_branch,
      max_retries   = optimizer$max_retries,
      eval_psi_fun  = eval_psi_fun
    )

    list(branch = branch, omega_hat = omega_hat)
  }

  # ------------------------------------------------------------------
  # Reformat result
  # ------------------------------------------------------------------
  branches   <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  if (verbose) cat("[generate_branches] Finished.\n")

  list(
    branches   = branches,
    omega_hats = omega_hats
  )
}
