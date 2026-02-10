# ================================================================================
# branch-generate.R — Integrated Log-Likelihood Branch Construction (Monte Carlo)
# ================================================================================
#
# Provides:
#   * build_one_branch()   — full left+right sweep on the ψ-grid
#   * generate_branches()  — Monte Carlo branch generation for integrated log-likelihood
#
# cutoff_buffer is applied in generate_branches() when computing the
# branch cutoff; build_one_branch() simply respects the cutoff it is
# given and does not know about the buffer.
# ================================================================================

# ======================================================================
# INTERNAL: Build a full branch (left + right sweep) on the ψ-grid
# ======================================================================

#' Build a Full Branch on the ψ-Grid (Integrated Log-Likelihood)
#' @param psi_hat_branch Numeric scalar. Branch mode ψ̂ for this ω̂ draw.
#' @param param_hat_branch Numeric vector. θ̂ at ψ̂_branch.
#' @param branch_cutoff Numeric scalar. Log-likelihood cutoff for stopping.
#' @param grid A ψ-grid object created by [psi_grid_anchor()].
#'   Must contain components:
#'   \itemize{
#'     \item psi_mle   — ψ̂_MLE (global ψ mode)
#'     \item increment — step size between ψ-grid points
#'   }
#' @param branch_fn Function of the form
#'   \code{branch_fn(psi_value, param_init) -> list(param_hat, branch_val)}
#'   used to locally maximize the log-likelihood at fixed ψ.
#' @param max_retries Integer. Maximum number of jitter retries for
#'   enforcing monotonic branch behavior.
#'
#' @return A tibble with columns:
#'   \item{k}{Integer ψ-grid index relative to ψ̂_MLE}
#'   \item{loglik}{Log-likelihood at each ψ_k}
#'   \item{psi}{ψ-grid value}
#'   \item{loglik_centered}{Centered loglik: loglik - max(loglik)}
#'
#' with attributes:
#'   \item{mode_index}{Index of the maximum branch loglik value}
#'   \item{n_points}{Number of grid points in the branch}
#'   \item{psi_MLE}{Global ψ̂_MLE}
#'
#' @keywords internal
build_one_branch <- function(
  psi_hat_branch,
  param_hat_branch,
  branch_cutoff,
  grid,
  branch_fn,
  max_retries,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE
) {
  psi_mle <- grid$psi_mle
  increment <- grid$increment

  adj <- get_adjacent_psi_points(
    psi_hat_branch,
    grid
  )

  left <- walk_branch_side(
    grid = grid,
    k_direction = -1L,
    k_start = adj$k_left,
    branch_cutoff = branch_cutoff,
    init_guess = param_hat_branch,
    branch_fn = branch_fn,
    max_retries = max_retries,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds
  )

  right <- walk_branch_side(
    grid = grid,
    k_direction = +1L,
    k_start = adj$k_right,
    branch_cutoff = branch_cutoff,
    init_guess = param_hat_branch,
    branch_fn = branch_fn,
    max_retries = max_retries,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds
  )

  branch <- dplyr::bind_rows(left, right) |>
    dplyr::mutate(
      psi = psi_mle + k * increment
    ) |>
    dplyr::arrange(.data$psi) |>
    dplyr::mutate(
      loglik_centered = .data$loglik - max(.data$loglik, na.rm = TRUE)
    )

  attr(branch, "mode_index") <- which.max(branch$loglik)
  attr(branch, "n_points") <- nrow(branch)
  attr(branch, "psi_MLE") <- psi_mle

  branch
}

# =========================================================================
# INTERNAL: Generate Monte Carlo Branches (integrated log-likelihood only)
# =========================================================================

#' Generate Monte Carlo Branches (Integrated Log-Likelihood Only)
#'
#' @keywords internal
generate_branches <- function(cal, verbose = TRUE) {
  if (!inherits(cal, "calibrated")) {
    stop(
      "generate_branches() requires a 'calibrated' model object.",
      call. = FALSE
    )
  }

  parameter <- cal$parameter
  likelihood <- cal$likelihood
  estimand <- cal$estimand
  nuisance <- cal$nuisance
  optimizer <- cal$optimizer
  execution <- cal$execution
  data <- cal$data

  integrate_result <- cal$workspace$integrate
  if (
    is.null(integrate_result) ||
      is.null(integrate_result$generate_init) ||
      is.null(integrate_result$sample_omega_hat)
  ) {
    stop(
      "integrate() must set cal$workspace$integrate$generate_init and $sample_omega_hat.",
      call. = FALSE
    )
  }

  if (
    is.null(optimizer$branch_mode_locator) ||
      !is.function(optimizer$branch_mode_locator)
  ) {
    stop(
      "cal$optimizer$branch_mode_locator must be a function.",
      call. = FALSE
    )
  }

  is_parallel <- inherits(execution, "parallel_spec")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() requires a future backend.",
      call. = FALSE
    )
  }

  `%op%` <- if (is_parallel) doFuture::`%dofuture%` else foreach::`%do%`

  psi_mle <- estimand$psi_mle
  increment <- estimand$increment

  max_retries <- optimizer$max_retries

  psi_lower <- estimand$psi_lower
  psi_upper <- estimand$psi_upper

  grid <- psi_grid_anchor(
    psi_mle = psi_mle,
    increment = increment,
    psi_lower = psi_lower,
    psi_upper = psi_upper
  )

  R <- execution$total_branches
  seed_opt <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list <- execution$packages %||% character()

  alpha_target <- min(1 - estimand$confidence_levels)
  gamma <- estimand$gamma %||% 0.5
  alpha_branch <- compute_required_branch_alpha(R, alpha_target, gamma)
  crit <- 0.5 * stats::qchisq(1 - alpha_branch, df = 1)

  cutoff_buffer <- estimand$cutoff_buffer %||% 0
  effective_crit <- crit * (1 + cutoff_buffer)

  stop_at_bounds <- optimizer$stop_at_bounds
  eval_at_bounds <- optimizer$eval_at_bounds

  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer,
    data = data
  )

  if (verbose) {
    cat("[integrate] Generating Monte Carlo branches\n")
    cat("[integrate]  R =", R, "\n")
  }

  result <- foreach::foreach(
    r = seq_len(R),
    .options.future = list(
      packages = pkg_list,
      seed = seed_opt,
      chunk.size = chunk_size
    )
  ) %op%
    {
      init <- integrate_result$generate_init()
      omega_hat <- integrate_result$sample_omega_hat(init)

      mode_obj <- optimizer$branch_mode_locator(omega_hat)

      if (
        is.null(mode_obj) ||
          is.null(mode_obj$psi_hat) ||
          is.null(mode_obj$param_hat) ||
          is.null(mode_obj$loglik_at_mode)
      ) {
        stop(
          "branch_mode_locator() must return psi_hat, param_hat, loglik_at_mode.",
          call. = FALSE
        )
      }

      psi_hat_branch <- mode_obj$psi_hat
      loglik_at_mode <- mode_obj$loglik_at_mode
      param_hat_branch <- mode_obj$param_hat

      if (!is.null(psi_lower) && psi_hat_branch < psi_lower) {
        stop("Branch mode ψ̂ lies below ψ lower bound.", call. = FALSE)
      }

      if (!is.null(psi_upper) && psi_hat_branch > psi_upper) {
        stop("Branch mode ψ̂ lies above ψ upper bound.", call. = FALSE)
      }

      branch_cutoff <- loglik_at_mode - effective_crit
      branch_fn <- branch_fn_factory(omega_hat)

      branch <- build_one_branch(
        psi_hat_branch = psi_hat_branch,
        param_hat_branch = param_hat_branch,
        branch_cutoff = branch_cutoff,
        grid = grid,
        branch_fn = branch_fn,
        max_retries = max_retries,
        stop_at_bounds = stop_at_bounds,
        eval_at_bounds = eval_at_bounds
      )

      list(
        branch = branch,
        omega_hat = omega_hat
      )
    }

  branches <- lapply(result, `[[`, "branch")
  omega_hats <- lapply(result, `[[`, "omega_hat")

  if (verbose) {
    cat("[integrate] Branch generation complete.\n")
  }

  list(
    branches = branches,
    omega_draws = omega_hats
  )
}

# ======================================================================
# END branch-generate.R
# ======================================================================
