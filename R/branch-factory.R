# ======================================================================
# branch-factory.R — Monte Carlo Branch Computation
#
# Provides:
#   compute_branches() — orchestrates branch traversals for all
#                        pre-sieved seeds from sieve()
#
# Relies on:
#   traverse_branch()  — branch-traverse.R
#   score_branch()     — branch-score.R
#   psi_grid_anchor()  — branch-utils.R
# ======================================================================

#' Compute Monte Carlo Branches for Integrated Log-Likelihood
#'
#' @description
#' Orchestrates branch traversal for all pre-sieved branch seeds
#' stored in \code{cal$workspace$integrate$branch_seeds} by
#' \code{sieve()}. All seeds are evaluated — no early stopping.
#'
#' Each completed branch is scored via \code{score_branch()}. Scores
#' and diagnostics are stored alongside the branches for use by
#' the aggregation step.
#'
#' No omega-hat sampling or mode location occurs here — those
#' responsibilities belong to \code{sieve()}.
#'
#' @param cal     A \code{calibrated} model object with
#'   \code{cal$workspace$integrate$branch_seeds} populated by
#'   \code{sieve()}.
#' @param verbose Logical. Print progress. Default: \code{FALSE}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{$branches}   — list of branch tibbles (one per seed)
#'     \item \code{$scores}     — numeric vector of branch scores
#'     \item \code{$omega_hats} — list of omega-hat vectors (one per seed)
#'   }
#'
#' @keywords internal
compute_branches <- function(cal, verbose = FALSE) {
  stopifnot(inherits(cal, "calibrated"))

  traversal <- cal$traversal
  estimand <- cal$estimand
  execution <- cal$execution

  # -------------------------------------------------------------------
  # Unpack pre-sieved seeds
  # -------------------------------------------------------------------
  branch_seeds <- cal$workspace$integrate$branch_seeds

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "compute_branches() requires pre-sieved branch seeds.\n",
      "Run sieve(cal) before integrate().",
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
      "[compute_branches] Traversal method: ",
      traversal$traversal_method %||% "topdown",
      "\n",
      "[compute_branches] Seeds = ",
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
      "[compute_branches] Complete.",
      " Perfect branches: ",
      n_perfect,
      "/",
      n_seeds,
      ".\n",
      sep = ""
    )
  }

  list(
    branches = branches,
    scores = scores,
    omega_hats = omega_hats
  )
}
