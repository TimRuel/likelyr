# ======================================================================
# INTERNAL: Build a full branch (left + right)
# ======================================================================

#' Build a full branch (left + right) around a branch-specific mode
#'
#' @keywords internal
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

  # Left side (decreasing psi)
  left <- walk_branch_side(
    increment     = -increment,
    start         = left_start,
    branch_cutoff = branch_cutoff,
    init_guess    = init_guess,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Right side (increasing psi)
  right <- walk_branch_side(
    increment     = increment,
    start         = right_start,
    branch_cutoff = branch_cutoff,
    init_guess    = init_guess,
    eval_psi_fun  = eval_psi_fun,
    max_retries   = max_retries
  )

  # Merge, order, compute integer grid index, add centered LL
  branch <- dplyr::bind_rows(left, right) |>
    dplyr::arrange(.data$psi) |>
    dplyr::mutate(
      k              = round((psi - psi_MLE) / increment),
      value_centered = value - max(value)
    )

  # Store metadata
  attr(branch, "mode_index") <- which.max(branch$value)
  attr(branch, "n_points")   <- nrow(branch)
  attr(branch, "psi_MLE")    <- psi_MLE

  branch
}

# ======================================================================
# INTERNAL: Generate Monte Carlo branches (serial or parallel)
# ======================================================================

#' Generate Monte Carlo branches for integrated likelihood
#'
#' @description
#' Internal engine used by [fit_integrated()]. It:
#' \itemize{
#'   \item determines the required per-branch alpha level based on
#'         the widest requested confidence interval and the execution spec,
#'   \item draws nuisance parameters,
#'   \item locates branch modes,
#'   \item extends each branch left/right until its cutoff, and
#'   \item returns all branches and nuisance draws.
#' }
#'
#' @param cal A `likelihood_calibration` object
#' @param verbose Logical; print progress messages
#'
#' @return A list with:
#'   \item{branches}{list of per-branch data.frames}
#'   \item{omega_hats}{list of nuisance draws (one per branch)}
#'
#' @keywords internal
generate_branches <- function(cal, verbose = TRUE) {

  if (!inherits(cal, "likelihood_calibration")) {
    stop("`generate_branches()` requires a likelihood_calibration object.",
         call. = FALSE)
  }

  wf        <- cal$workflow
  model     <- wf$model
  nuisance  <- wf$nuisance
  optimizer <- wf$optimizer
  estimand  <- wf$estimand
  execution <- wf$execution

  # --------------------------------------------------------------------
  # Determine Monte Carlo branch count R from execution spec
  # --------------------------------------------------------------------
  R <- compute_num_branches(execution)

  # --------------------------------------------------------------------
  # Determine target alpha from widest requested confidence level
  # alpha_target = 1 - max(confidence_levels)
  # then compute per-branch alpha to guarantee that global CI level.
  # --------------------------------------------------------------------
  conf_levels <- estimand$confidence_levels %||% 0.95
  alpha_target <- 1 - max(conf_levels)

  alpha_branch <- compute_required_branch_alpha(
    R     = R,
    alpha = alpha_target
  )

  # chi-square cutoff for each branch
  crit <- 0.5 * stats::qchisq(1 - alpha_branch, df = 1)

  # --------------------------------------------------------------------
  # Parallel safety and execution mode
  # --------------------------------------------------------------------
  is_parallel <- inherits(execution, "likelihood_execution_parallel")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() was provided, but no parallel backend is active.\n",
      "Call e.g. `future::plan(multisession, workers = execution$num_workers)`\n",
      "BEFORE calling fit_integrated().",
      call. = FALSE
    )
  }

  if (verbose) {
    mode_label <- if (is_parallel) "PARALLEL" else "SERIAL"
    msg <- sprintf(
      "[generate_branches] Mode: %s | Branches (R): %d | alpha_target = %.4f | alpha_branch = %.4f",
      mode_label, R, alpha_target, alpha_branch
    )
    message(msg)
  }

  # Common quantities
  psi_MLE   <- cal$psi_mle
  theta_mle <- cal$theta_mle

  # Step size for psi grid
  psi_step <- estimand$grid_increment

  if (is.null(psi_step) || !is.numeric(psi_step) || psi_step <= 0) {
    stop("`estimand$grid_increment` must be a positive numeric.", call. = FALSE)
  }

  # ====================================================================
  # SERIAL EXECUTION (foreach + %do%)
  # ====================================================================
  if (!is_parallel) {

    branches   <- vector("list", R)
    omega_hats <- vector("list", R)

    for (r in seq_len(R)) {

      # 1. Draw nuisance (omega_hat)
      omega_hat <- nuisance$sampler(model$param_dim)

      # 2. Build eval_psi_fun(theta, psi) closure for this omega_hat
      eval_psi_fun <- build_eval_psi_fun(
        E_loglik  = cal$E_loglik,
        psi       = cal$psi,
        omega     = omega_hat,
        optimizer = optimizer
      )

      # 3. Locate branch mode for this omega_hat
      mode_obj <- branch_mode_solve(
        psi_mle      = psi_MLE,
        eval_psi_fun = eval_psi_fun,
        theta_init   = optimizer$theta_init %||% theta_mle,
        psi_step     = psi_step,
        optimizer    = optimizer
      )

      # 4. Compute branch cutoff at this mode
      branch_cutoff <- mode_obj$loglik_at_mode - crit

      # 5. Find left and right starting psi values on the grid
      adj <- get_adjacent_psi_points(
        psi_hat_branch = mode_obj$psi_hat,
        psi_mle        = psi_MLE,
        increment      = psi_step
      )

      # 6. Build full branch
      branch <- build_one_branch(
        psi_MLE        = psi_MLE,
        increment      = psi_step,
        left_start     = adj$left,
        right_start    = adj$right,
        branch_cutoff  = branch_cutoff,
        init_guess     = mode_obj$theta_hat,
        max_retries    = optimizer$max_retries %||% 5L,
        eval_psi_fun   = eval_psi_fun
      )

      branches[[r]]   <- branch
      omega_hats[[r]] <- omega_hat
    }

    if (verbose) message("[generate_branches] Finished (serial).")

    return(list(
      branches   = branches,
      omega_hats = omega_hats
    ))
  }

  # ====================================================================
  # PARALLEL EXECUTION (foreach + %dofuture%)
  # ====================================================================

  # We'll use doFuture's %dofuture% operator
  `%op%` <- `%dofuture%`

  # Packages to load on workers
  packages <- execution$packages %||% character()

  result <- foreach::foreach(
    r = seq_len(R),
    .combine        = "list",
    .multicombine   = TRUE,
    .errorhandling  = "remove",
    .packages       = packages,
    .options.future = list(
      seed       = execution$seed %||% TRUE,
      chunk.size = execution$chunk_size
    )
  ) %op% {

    # 1. Draw nuisance (omega_hat)
    omega_hat <- nuisance$sampler(model$param_dim)

    # 2. Build eval_psi_fun(theta, psi) closure
    eval_psi_fun <- build_eval_psi_fun(
      E_loglik  = cal$E_loglik,
      psi       = cal$psi,
      omega     = omega_hat,
      optimizer = optimizer
    )

    # 3. Locate branch mode
    mode_obj <- branch_mode_solve(
      psi_mle      = psi_MLE,
      eval_psi_fun = eval_psi_fun,
      theta_init   = optimizer$theta_init %||% theta_mle,
      psi_step     = psi_step,
      optimizer    = optimizer
    )

    # 4. Branch cutoff
    branch_cutoff <- mode_obj$loglik_at_mode - crit

    # 5. Left/right starting psi
    adj <- get_adjacent_psi_points(
      psi_hat_branch = mode_obj$psi_hat,
      psi_mle        = psi_MLE,
      increment      = psi_step
    )

    # 6. Build full branch
    branch <- build_one_branch(
      psi_MLE        = psi_MLE,
      increment      = psi_step,
      left_start     = adj$left,
      right_start    = adj$right,
      branch_cutoff  = branch_cutoff,
      init_guess     = mode_obj$theta_hat,
      max_retries    = optimizer$max_retries %||% 5L,
      eval_psi_fun   = eval_psi_fun
    )

    list(
      branch   = branch,
      omega_hat = omega_hat
    )
  }

  branches   <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  if (verbose) message("[generate_branches] Finished (parallel).")

  list(
    branches   = branches,
    omega_hats = omega_hats
  )
}
