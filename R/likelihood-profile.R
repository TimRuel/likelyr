# ======================================================================
# likelihood-profile.R — Profile Log-Likelihood API (HPC-safe)
# ======================================================================

#' Profile Log-Likelihood
#'
#' @description
#' Computes the *profile log-likelihood* curve for the calibrated model.
#'
#' Unlike the integrated log-likelihood, the profile log-likelihood
#' does **not** involve ω̂–sampling or Monte Carlo methods. Instead,
#' nuisance parameters are fixed at their MLE:
#'
#' \preformatted{
#'    ω̂ = θ̂_MLE
#' }
#'
#' The log-likelihood is then evaluated along a ψ–grid, forming a *single
#' deterministic branch*. The result is attached to:
#'
#' \preformatted{
#'   cal$workspace$profile
#' }
#'
#' This function performs **no plotting or table rendering**. Visualization
#' is deferred to `plot()` / `view()` methods, which materialize plots/tables
#' locally from stored data frames.
#'
#' @param cal A `calibrated` model object produced by [calibrate()].
#' @param verbose Logical; print diagnostic messages (default FALSE).
#' @param ... Additional arguments forwarded to [generate_profile()].
#'
#' @return
#' The SAME `calibrated` model object, augmented with:
#'   * class `profiled`
#'   * `workspace$profile` — a `profile` object
#'
#' @export
profile <- function(cal, ...) {
  UseMethod("profile")
}

# ----------------------------------------------------------------------

#' @export
profile.default <- function(cal, ...) {
  stop("profile() requires a 'calibrated' model object", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
profile.calibrated <- function(cal, verbose = FALSE, ...) {
  # ------------------------------------------------------------------
  # 0A. Ensure calibration has occurred
  # ------------------------------------------------------------------
  if (!is_calibrated(cal)) {
    stop("profile() requires calibrate() first.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 0B. Validate required components (optimizer/execution not required)
  # ------------------------------------------------------------------
  validate_profile_input(cal)

  # ------------------------------------------------------------------
  # 1. Extract calibrated quantities
  # ------------------------------------------------------------------
  parameter <- cal$parameter
  likelihood <- cal$likelihood
  estimand <- cal$estimand
  nuisance <- cal$nuisance
  optimizer <- cal$optimizer
  execution <- cal$execution
  data <- cal$data

  psi_mle <- estimand$psi_mle
  psi_fn <- estimand$psi_fn
  param_mle <- parameter$param_mle
  loglik_fn <- likelihood$loglik

  # ------------------------------------------------------------------
  # 2. Execution summary
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[profile] Profile Log-Likelihood\n")
  }

  # ------------------------------------------------------------------
  # 3. Compute branch cutoff from confidence levels
  # ------------------------------------------------------------------
  loglik_at_mle <- loglik_fn(param_mle)

  alpha_target <- min(1 - estimand$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  cutoff_buffer <- estimand$cutoff_buffer %||% 0
  effective_crit <- crit * (1 + cutoff_buffer)

  cutoff <- loglik_at_mle - effective_crit

  # Build ψ → loglik evaluator at θ̂
  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer,
    data = data
  )
  branch_fn <- branch_fn_factory(param_mle)

  # ------------------------------------------------------------------
  # 4. Construct branch grid and walk it
  # ------------------------------------------------------------------
  increment <- estimand$increment %||% 0.05
  max_retries <- optimizer$max_retries %||% 4
  psi_lower <- estimand$psi_lower
  psi_upper <- estimand$psi_upper
  stop_at_bounds <- optimizer$stop_at_bounds %||% TRUE
  eval_at_bounds <- optimizer$eval_at_bounds %||% TRUE

  psi_ll_df <- tryCatch(
    generate_profile(
      psi_mle = psi_mle,
      param_mle = param_mle,
      loglik_at_mle = loglik_at_mle,
      increment = increment,
      cutoff = cutoff,
      branch_fn = branch_fn,
      max_retries = max_retries,
      stop_at_bounds = stop_at_bounds,
      eval_at_bounds = eval_at_bounds,
      psi_lower = psi_lower,
      psi_upper = psi_upper
    ),
    error = function(e) {
      if (verbose) {
        cat("[profile] ERROR in generate_profile():\n")
        message(e)
      }
      NULL
    }
  )

  if (!is.null(psi_ll_df)) {
    attr(psi_ll_df, "type") <- "profile"
  }

  # ------------------------------------------------------------------
  # 5. Wrap into profile_result (data only)
  # ------------------------------------------------------------------
  profile_result <- new_profile_result(list(
    psi_ll_df = psi_ll_df,
    psi_mle = psi_mle,
    param_mle = param_mle,
    status = if (!is.null(psi_ll_df)) "success" else "failed"
  ))

  # ------------------------------------------------------------------
  # 6. Store and return
  # ------------------------------------------------------------------
  if (is.null(cal$workspace)) {
    cal$workspace <- list()
  }

  cal$workspace$profile <- profile_result
  cal <- mark_profiled(cal)

  if (verbose) {
    cat("[profile] Finished.\n")
  }

  cal
}

# ======================================================================
# VALIDATION
# ======================================================================

#' Validate inputs prior to profile likelihood computation
#'
#' @description
#' Checks that a model object contains all required structural
#' components needed to compute a profile log-likelihood.
#'
#' Profile likelihood requires:
#' \itemize{
#'   \item \code{parameter_spec()}
#'   \item \code{likelihood_spec()}
#'   \item \code{estimand_spec()}
#'   \item \code{nuisance_spec()}
#' }
#'
#' @param cal A model object intended for profile likelihood computation.
#'
#' @return Invisibly returns \code{cal} if validation succeeds.
#'
#' @keywords internal
#' @noRd
validate_profile_input <- function(cal) {
  if (!inherits(cal$parameter, "parameter_spec")) {
    stop("model$parameter must be a 'parameter_spec' object.")
  }
  if (!inherits(cal$likelihood, "likelihood_spec")) {
    stop("model$likelihood must be a 'likelihood_spec' object.")
  }
  if (!inherits(cal$estimand, "estimand_spec")) {
    stop("model$estimand must be an 'estimand_spec' object.")
  }
  if (!inherits(cal$nuisance, "nuisance_spec")) {
    stop("model$nuisance must be a 'nuisance_spec' object.")
  }
  invisible(cal)
}

# ======================================================================
# RESULT CLASS SUPPORT
# ======================================================================

#' @method print profile
#' @export
print.profile <- function(x, ...) {
  cat("<Profile Log-Likelihood Result>\n")

  if (!is.null(x$status)) {
    cat("Status: ", x$status, "\n", sep = "")
  }

  # -----------------------------------------------------------
  # Lifecycle flags (slot presence)
  # -----------------------------------------------------------
  has_inference <- !is.null(x$inference)
  has_diagnostics <- !is.null(x$diagnostics)

  cat("Lifecycle:\n")
  cat("  inferred:   ", if (has_inference) "✓" else "×", "\n", sep = "")
  cat("  diagnosed:  ", if (has_diagnostics) "✓" else "×", "\n", sep = "")

  # -----------------------------------------------------------
  # Estimates
  # -----------------------------------------------------------
  if (!is.null(x$psi_mle)) {
    cat("psi_MLE: ", format(x$psi_mle), "\n", sep = "")
  }
  if (!is.null(x$param_mle)) {
    cat(
      "param_MLE: (",
      paste(format(x$param_mle), collapse = ", "),
      ")\n",
      sep = ""
    )
  }

  # -----------------------------------------------------------
  # Grid information
  # -----------------------------------------------------------
  if (!is.null(x$psi_ll_df)) {
    cat("Grid points: ", nrow(x$psi_ll_df), "\n", sep = "")
  }

  invisible(x)
}

# =====================================================================
# S3 Plot Method (local-only materialization)
# =====================================================================

#' @method plot profile
#' @export
plot.profile <- function(x, ...) {
  .assert_local_plotting()

  psi_ll_df <- x$psi_ll_df
  if (is.null(psi_ll_df) && !is.null(x$inference)) {
    psi_ll_df <- x$inference$psi_ll_df
  }

  if (is.null(psi_ll_df)) {
    stop("No pseudolikelihood data available to plot.", call. = FALSE)
  }

  plot_pseudolikelihood_points(psi_ll_df)
}
