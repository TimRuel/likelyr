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
# ======================================================================

# ======================================================================
# INTERNAL: Build a full branch (left + right sweep) on the ψ-grid
# ======================================================================

#' Build a Full Branch on the ψ-Grid (Integrated Log-Likelihood)
#'
#' @description
#' Given the branch mode (ψ̂_branch, θ̂_branch), this function sweeps
#' outward along the ψ-grid in both directions (left and right) until
#' the log-likelihood falls below a supplied cutoff.
#'
#' This is used for **integrated log-likelihood branches only**; the cutoff
#' threshold (including any user-specified buffer) is computed upstream
#' in [generate_branches()].
#'
#' @param psi_hat_branch Numeric scalar. Branch mode ψ̂ for this ω̂ draw.
#' @param param_hat_branch Numeric vector. θ̂ at ψ̂_branch.
#' @param branch_cutoff Numeric scalar. Log-likelihood cutoff for stopping.
#' @param grid A ψ-grid object created by [psi_grid_anchor()].
#'   Must contain components:
#'   \itemize{
#'     \item psi_mle   — ψ̂_MLE (global ψ mode)
#'     \item increment — step size between ψ-grid points
#'   }
#' @param eval_psi_fun Function of the form
#'   \code{eval_psi_fun(psi_value, param_init) -> list(param_hat, branch_val)}
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
  eval_psi_fun,
  max_retries
) {
  # Extract grid metadata
  psi_mle <- grid$psi_mle
  increment <- grid$increment

  # Determine adjacent left/right k-indices around psi_hat_branch
  adj <- get_adjacent_psi_points(
    psi_hat_branch,
    grid
  )

  # Left sweep
  left <- walk_branch_side(
    grid = grid,
    k_direction = -1L,
    k_start = adj$k_left,
    branch_cutoff = branch_cutoff,
    init_guess = param_hat_branch,
    eval_psi_fun = eval_psi_fun,
    max_retries = max_retries
  )

  # Right sweep
  right <- walk_branch_side(
    grid = grid,
    k_direction = +1L,
    k_start = adj$k_right,
    branch_cutoff = branch_cutoff,
    init_guess = param_hat_branch,
    eval_psi_fun = eval_psi_fun,
    max_retries = max_retries
  )

  # Merge & assign ψ values
  branch <- dplyr::bind_rows(left, right) |>
    dplyr::mutate(
      psi = psi_mle + k * increment
    ) |>
    dplyr::arrange(.data$psi)

  # Center loglik values AFTER arranging
  branch <- branch |>
    dplyr::mutate(
      loglik_centered = .data$loglik - max(.data$loglik, na.rm = TRUE)
    )

  # Metadata
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
#' @description
#' Generates `R` Monte Carlo branches for the integrated log-likelihood,
#' each branch corresponding to a draw of ω̂ satisfying ψ(ω̂) = ψ_MLE.
#'
#' For each branch:
#' \enumerate{
#'   \item Draw ω̂ via \code{cal$workspace$integratd$generate_init()} and
#'         \code{cal$workspace$integrate$sample_omega_hat()}.
#'   \item Construct a ψ-conditioned evaluator via [build_eval_psi_fun()].
#'   \item Solve the branch mode: (ψ̂_branch, θ̂_branch).
#'   \item Compute a loglik cutoff using the LR criterion with an
#'         optional \code{cutoff_buffer} from the estimand spec:
#'         \deqn{crit_eff = crit * (1 + cutoff_buffer)}
#'   \item Sweep left and right along the ψ-grid via [build_one_branch()].
#' }
#'
#' @param cal A `calibrated` model object used by [integrate()].
#' @param verbose Logical; if TRUE, prints progress messages.
#'
#' @return A list with components:
#'   \item{branches}{List of branch data frames}
#'   \item{omega_draws}{List of ω̂ vectors used per branch}
#'
#' @keywords internal
generate_branches <- function(cal, verbose = TRUE) {
  if (!inherits(cal, "calibrated")) {
    stop(
      "generate_branches() requires a 'calibrated' model object.",
      call. = FALSE
    )
  }

  param <- cal$parameter
  lik <- cal$likelihood
  estimand <- cal$estimand
  nuisance <- cal$nuisance
  optimizer <- cal$optimizer
  execution <- cal$execution

  # -------------------------------------------------------------------------
  # Integrated log-likelihood components now live in cal$workspace$integrate
  # -------------------------------------------------------------------------
  integrate_result <- cal$workspace$integrate
  if (
    is.null(integrate_result) ||
      is.null(integrate_result$generate_init) ||
      is.null(integrate_result$sample_omega_hat)
  ) {
    stop(
      "integrate() must set cal$workspace$integrate_result$generate_init and $sample_omega_hat.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Parallel mode detection
  # -------------------------------------------------------------------
  is_parallel <- inherits(execution, "parallel_spec")

  if (is_parallel && future::nbrOfWorkers() <= 1) {
    stop(
      "parallel_spec() requires a future backend. Call future::plan(multisession).",
      call. = FALSE
    )
  }

  `%op%` <- if (is_parallel) doFuture::`%dofuture%` else foreach::`%do%`

  # -------------------------------------------------------------------
  # Core calibrated quantities
  # -------------------------------------------------------------------
  psi_mle <- estimand$psi_mle
  param_mle <- param$param_mle
  increment <- estimand$increment
  interval <- estimand$search_interval

  max_retries <- optimizer$max_retries

  # ψ-grid anchor
  grid <- psi_grid_anchor(
    psi_mle = psi_mle,
    increment = increment
  )

  # -------------------------------------------------------------------
  # Monte Carlo configuration
  # -------------------------------------------------------------------
  R <- execution$total_branches
  seed_opt <- if (is_parallel) execution$seed else NULL
  chunk_size <- if (is_parallel) execution$chunk_size else 1L
  pkg_list <- execution$packages %||% character()

  # -------------------------------------------------------------------
  # LR cutoff and buffer
  # -------------------------------------------------------------------
  alpha_target <- min(1 - estimand$confidence_levels)
  alpha_branch <- compute_required_branch_alpha(R, alpha_target)
  crit <- 0.5 * stats::qchisq(1 - alpha_branch, df = 1)

  cutoff_buffer <- estimand$cutoff_buffer %||% 0
  effective_crit <- crit * (1 + cutoff_buffer)

  # ψ-conditional optimizer factory
  eval_psi_builder <- build_eval_psi_fun(cal)

  if (verbose) {
    cat("[integrate] Generating Monte Carlo branches\n")
    cat("[integrate]  R =", R, "\n")
  }

  # -------------------------------------------------------------------
  # MAIN LOOP: R Monte Carlo branches
  # -------------------------------------------------------------------
  result <- foreach::foreach(
    r = seq_len(R),
    .options.future = list(
      packages = pkg_list,
      seed = seed_opt,
      chunk.size = chunk_size
    )
  ) %op%
    {
      # 1. Draw ω̂
      init <- integrate_result$generate_init()
      omega_hat <- integrate_result$sample_omega_hat(init)

      # 2. Build ψ evaluator for this ω̂
      eval_psi_fun <- eval_psi_builder(omega_hat)

      # 3. Solve branch mode
      mode_obj <- branch_mode_solve(
        psi_mle = psi_mle,
        eval_psi_fun = eval_psi_fun,
        param_init = param_mle,
        search_interval = interval
      )

      psi_hat_branch <- mode_obj$psi_hat
      loglik_at_mode <- mode_obj$loglik_at_mode
      param_hat_branch <- mode_obj$param_hat

      # 4. Critical cutoff with buffer
      branch_cutoff <- loglik_at_mode - effective_crit

      # 5. Build full branch
      branch <- build_one_branch(
        psi_hat_branch = psi_hat_branch,
        param_hat_branch = param_hat_branch,
        branch_cutoff = branch_cutoff,
        grid = grid,
        eval_psi_fun = eval_psi_fun,
        max_retries = max_retries
      )

      list(
        branch = branch,
        omega_hat = omega_hat
      )
    }

  # -------------------------------------------------------------------
  # Collect results
  # -------------------------------------------------------------------
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
