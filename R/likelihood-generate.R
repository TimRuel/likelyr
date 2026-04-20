# ======================================================================
# likelihood-generate.R — Branch Generation
#
# Provides:
#   generate() — dispatches to task-specific branch generation
#
# Relies on:
#   traverse_branch()         — branch-traverse.R
#   traverse_profile_side()   — profile-traverse.R
#   psi_grid_anchor()         — branch-utils.R
#   assemble_branch_df()      — branch-traverse.R
#   compute_common_interval() — branch-utils.R
# ======================================================================

#' Generate Branches
#'
#' @description
#' Dispatches to the appropriate branch generation strategy based on
#' \code{task}:
#' \itemize{
#'   \item \code{"integrate"} — Monte Carlo branches from pre-sieved
#'     seeds stored in \code{model$workspace$integrated$cache$branch_seeds}.
#'     The ψ grid is set from the common interval derived by
#'     \code{preprocess()}, ensuring full overlap across all branches.
#'   \item \code{"profile"} — single deterministic branch at
#'     \code{param_mle}, stored in \code{model$workspace$profile}.
#' }
#'
#' @param model     A calibrated \code{model} object.
#' @param task    Character scalar. One of \code{"integrate"} or
#'   \code{"profile"}. Default: \code{"integrate"}.
#' @param verbose Logical. Print progress. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to the task-specific
#'   implementation.
#'
#' @return The SAME calibrated \code{model} object with results stored
#'   on the workspace.
#'
#' @export
generate <- function(model, ...) {
  UseMethod("generate")
}

#' @export
generate.default <- function(model, ...) {
  stop("generate() requires a calibrated 'model' object.", call. = FALSE)
}

#' @export
generate.model <- function(
  model,
  task = "integrate",
  verbose = FALSE,
  ...
) {
  task <- match.arg(task, c("integrate", "profile"))

  switch(
    task,
    integrate = .generate_integrate(model, verbose = verbose, ...),
    profile = .generate_profile(model, verbose = verbose, ...)
  )
}

# ======================================================================
# TASK: integrate
# ======================================================================

#' @keywords internal
.generate_integrate <- function(model, verbose = FALSE, ...) {
  traversal <- model$traversal
  estimand <- model$estimand
  execution <- model$execution

  branch_seeds <- model$workspace$integrated$cache$branch_seeds

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "generate(task = 'integrate') requires pre-sieved branch seeds.\n",
      "Run sieve(model) before generate().",
      call. = FALSE
    )
  }

  n_seeds <- length(branch_seeds)

  branch_binder <- traversal$branch_binder
  common_interval <- model$workspace$integrated$cache$common_interval

  if (is.null(common_interval)) {
    stop(
      "generate(task = 'integrate') requires a common interval.\n",
      "Run preprocess(model) before generate().",
      call. = FALSE
    )
  }

  grid <- psi_grid_anchor(
    psi_mle = estimand$psi_mle,
    increment = traversal$increment,
    psi_lower = common_interval$psi_lower,
    psi_upper = common_interval$psi_upper
  )

  is_parallel <- inherits(execution, "parallel_spec")

  if (is_parallel && future::nbrOfWorkers() <= 1L) {
    stop("parallel_spec() requires a future backend.", call. = FALSE)
  }

  `%op%` <- if (is_parallel) doFuture::`%dofuture%` else foreach::`%do%`
  seed_opt <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list <- execution$packages %||% character()

  if (verbose) {
    cat(
      "[generate] task = integrate",
      " | traversal method: ",
      traversal$traversal_method %||% "topdown",
      "\n",
      "[generate] Seeds = ",
      n_seeds,
      " | ",
      if (is_parallel) "PARALLEL" else "SERIAL",
      "\n",
      "[generate] Common interval: [",
      common_interval$psi_lower,
      ", ",
      common_interval$psi_upper,
      "]\n",
      sep = ""
    )
  }

  results <- foreach::foreach(
    r = seq_len(n_seeds),
    .options.future = list(
      packages = pkg_list,
      seed = seed_opt,
      chunk.size = chunk_size
    )
  ) %op%
    {
      branch_seed <- branch_seeds[[r]]
      branch_evaluator <- branch_binder(branch_seed$omega_hat)

      result <- traverse_branch(
        branch_seed = branch_seed,
        traversal = traversal,
        grid = grid,
        branch_evaluator = branch_evaluator
      )

      if (verbose && !is_parallel) {
        cat("[generate] Branch ", r, "/", n_seeds, " complete.\n", sep = "")
      }

      result$branch_df
    }

  if (verbose) {
    cat("[generate] Complete. Branches: ", n_seeds, ".\n", sep = "")
  }

  model$workspace$integrated$cache$branches <- results

  model
}

# ======================================================================
# TASK: profile
# ======================================================================

#' @keywords internal
.generate_profile <- function(model, verbose = FALSE, ...) {
  solver <- model$solver
  traversal <- model$traversal
  estimand <- model$estimand
  param_mle <- model$parameter$param_mle
  psi_mle <- estimand$psi_mle

  profile_evaluator <- traversal$branch_binder(param_mle)

  grid <- psi_grid_anchor(
    psi_mle = psi_mle,
    increment = traversal$increment,
    psi_lower = min(estimand$psi_interval),
    psi_upper = max(estimand$psi_interval)
  )

  alpha_target <- min(1 - traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * traversal$cutoff_buffer
  loglik_at_mle <- profile_evaluator(psi_mle, param_mle)$branch_val
  cutoff <- loglik_at_mle - effective_crit
  max_retries <- solver$max_retries %||% 4L

  if (verbose) {
    cat(
      "[generate] task = profile",
      " | increment = ",
      traversal$increment,
      " | cutoff = ",
      round(cutoff, 3),
      "\n",
      sep = ""
    )
  }

  left <- traverse_profile_side(
    grid = grid,
    k_start = -1L,
    cutoff = cutoff,
    init_guess = param_mle,
    profile_evaluator = profile_evaluator,
    max_retries = max_retries,
    stop_at_bounds = TRUE,
    eval_at_bounds = TRUE
  )

  right <- traverse_profile_side(
    grid = grid,
    k_start = +1L,
    cutoff = cutoff,
    init_guess = param_mle,
    profile_evaluator = profile_evaluator,
    max_retries = max_retries,
    stop_at_bounds = TRUE,
    eval_at_bounds = TRUE
  )

  psi_loglik_df <- left |>
    dplyr::bind_rows(
      tibble::tibble(k = 0L, loglik = loglik_at_mle),
      right
    ) |>
    assemble_branch_df(grid) |>
    magrittr::set_attr("pseudolikelihood", "profile")

  if (verbose) {
    cat(
      "[generate] Complete. Profile points: ",
      nrow(psi_loglik_df),
      ".\n",
      sep = ""
    )
  }

  model$workspace$profile <- list(
    psi_loglik_df = psi_loglik_df,
    psi_hat = psi_loglik_df |>
      dplyr::slice(which.max(loglik)) |>
      dplyr::pull(psi)
  )

  model
}
