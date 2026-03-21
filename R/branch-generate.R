# ======================================================================
# branch-generate.R — Monte Carlo Branch Generation
#
# Provides:
#   generate() — orchestrates branch traversals for all
#                pre-sieved seeds from sieve()
#
# Relies on:
#   traverse_branch()  — branch-traverse.R
#   score_branch()     — branch-score.R
#   psi_grid_anchor()  — branch-utils.R
# ======================================================================

#' Generate Monte Carlo Branches
#'
#' @description
#' Orchestrates branch traversal for all pre-sieved branch seeds
#' stored in \code{cal$workspace$integrate$branch_seeds} by
#' \code{sieve()}. All seeds are evaluated — no early stopping.
#'
#' Each completed branch is scored via \code{score_branch()}. Branches,
#' scores, and omega-hats are stored on the model for use by
#' \code{aggregate()}.
#'
#' No omega-hat sampling or mode location occurs here — those
#' responsibilities belong to \code{sieve()}.
#'
#' @param cal     A \code{calibrated} model object with
#'   \code{cal$workspace$integrate$branch_seeds} populated by
#'   \code{sieve()}.
#' @param task    Character scalar. One of \code{"integrate"} or
#'   \code{"profile"}. Controls which branch generation strategy is
#'   used. Default: \code{"integrate"}.
#' @param verbose Logical. Print progress. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to the task-specific
#'   implementation.
#'
#' @return The SAME \code{calibrated} model object, with branch
#'   results stored in \code{cal$workspace$integrate}.
#'
#' @export
generate <- function(cal, ...) {
  UseMethod("generate")
}

#' @export
generate.default <- function(cal, ...) {
  stop("generate() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
generate.calibrated <- function(cal, task = "integrate", verbose = FALSE, ...) {
  task <- match.arg(task, c("integrate", "profile"))

  switch(
    task,
    integrate = .generate_integrate(cal, verbose = verbose, ...),
    profile = .generate_profile(cal, verbose = verbose, ...)
  )
}

# ======================================================================
# TASK: integrate
# ======================================================================

#' @keywords internal
.generate_integrate <- function(cal, verbose = FALSE, ...) {
  traversal <- cal$traversal
  estimand <- cal$estimand
  execution <- cal$execution

  # -------------------------------------------------------------------
  # Unpack pre-sieved seeds
  # -------------------------------------------------------------------
  branch_seeds <- cal$workspace$integrate$branch_seeds

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "generate(task = 'integrate') requires pre-sieved branch seeds.\n",
      "Run sieve(cal) before generate().",
      call. = FALSE
    )
  }

  n_seeds <- length(branch_seeds)

  # -------------------------------------------------------------------
  # Branch binder — already built and stored during calibration
  # -------------------------------------------------------------------
  branch_binder <- traversal$branch_binder

  # -------------------------------------------------------------------
  # ψ-grid
  # -------------------------------------------------------------------
  grid <- psi_grid_anchor(
    psi_mle = estimand$psi_mle,
    increment = traversal$increment,
    psi_lower = min(estimand$psi_interval),
    psi_upper = max(estimand$psi_interval)
  )

  # -------------------------------------------------------------------
  # Branch cutoff
  # -------------------------------------------------------------------
  alpha_target <- min(1 - traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * traversal$cutoff_buffer

  # -------------------------------------------------------------------
  # Execution setup
  # -------------------------------------------------------------------
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
      sep = ""
    )
  }

  # -------------------------------------------------------------------
  # Main loop — evaluate all seeds
  # -------------------------------------------------------------------
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
        branch_evaluator = branch_evaluator,
        effective_crit = effective_crit
      )

      k_mode <- round(
        (branch_seed$psi_mode - estimand$psi_mle) / traversal$increment
      )

      score <- score_branch(
        branch_df = result$branch_df,
        k_mode = k_mode,
        ll_mode = branch_seed$ll_mode,
        crit = crit
      )

      list(
        branch_df = result$branch_df,
        score = score,
        omega_hat = branch_seed$omega_hat
      )
    }

  branches <- lapply(results, `[[`, "branch_df")
  scores <- vapply(results, `[[`, "score", FUN.VALUE = numeric(1))
  omega_hats <- lapply(results, `[[`, "omega_hat")

  if (verbose) {
    n_perfect <- sum(scores >= 1.0)
    cat(
      "[generate] Complete.",
      " Perfect branches: ",
      n_perfect,
      "/",
      n_seeds,
      ".\n",
      sep = ""
    )
  }

  cal$workspace$integrate$branches <- branches
  cal$workspace$integrate$scores <- scores
  cal$workspace$integrate$omega_hats <- omega_hats

  cal
}

# ======================================================================
# TASK: profile (placeholder)
# ======================================================================

#' @keywords internal
.generate_profile <- function(cal, verbose = FALSE, ...) {
  stop(
    "generate(task = 'profile') is not yet implemented.",
    call. = FALSE
  )
}
