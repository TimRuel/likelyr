# ======================================================================
# likelihood-integrate.R
#
# Full Integrated Likelihood engine:
#   integrate(cal)
#       → attach IL components
#       → generate_branches()
#       → log-sum-exp average
#
# Requires:
#   - eval-omega-hat.R
#   - build_eval_psi_fun.R
#   - generate_branches.R
#   - average_branches()
# ======================================================================


# ======================================================================
# Generic S3 front-end
# ======================================================================

#' Integrated Likelihood
#'
#' @description
#' Computes the integrated likelihood (IL) after calibration with
#' `calibrate()`. The method attaches IL-specific components to `cal`
#' and then runs the Monte Carlo branch engine.
#'
#' @param cal A calibrated_model produced by `calibrate()`.
#' @param verbose Logical.
#' @param ... Additional arguments.
#'
#' @return An `integrated_likelihood` object.
#' @export
integrate <- function(cal, ...) {
  UseMethod("integrate")
}

# ----------------------------------------------------------------------

#' @export
integrate.default <- function(cal, ...) {
  stop("integrate() requires a calibrated_model.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
integrate.calibrated_model <- function(cal, verbose = TRUE, ...) {

  # ------------------------------------------------------------------
  # 0. Validate nuisance specification
  # ------------------------------------------------------------------
  if (is.null(cal$nuisance)) {
    stop("Integrated likelihood requires a nuisance_spec() in the model_spec.",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 1. Attach IL-specific components (formerly calibrate_IL)
  # ------------------------------------------------------------------
  constraint_fn <- function(theta) cal$psi_fn(theta) - cal$psi_mle

  il <- list(
    constraint_fn    = constraint_fn,
    generate_init    = make_omega_hat_initgen(cal),
    sample_omega_hat = make_omega_hat_sampler(cal)
  )

  cal$il               <- il
  cal$omega_hat_con_fn <- constraint_fn
  cal$type             <- "IL"      # enforce consistency
  class(cal)           <- c("IL_calibration", class(cal))

  # ------------------------------------------------------------------
  # 2. Display execution mode
  # ------------------------------------------------------------------
  exec        <- cal$execution
  is_parallel <- inherits(exec, "parallel_spec")

  R_eff <- if (is_parallel)
    exec$num_workers * exec$chunk_size
  else
    exec$R

  if (verbose) {
    cat("[integrate] Monte Carlo Integrated Likelihood\n")
    cat("[integrate] Mode:",
        if (is_parallel) "PARALLEL" else "SERIAL",
        "| Branches:", R_eff, "\n")
  }

  # ------------------------------------------------------------------
  # 3. Branch computation
  # ------------------------------------------------------------------
  branch_result <- generate_branches(
    cal     = cal,
    verbose = verbose,
    ...
  )

  # ------------------------------------------------------------------
  # 4. Final aggregation (log-sum-exp)
  # ------------------------------------------------------------------
  out <- tryCatch(
    {
      branches    <- branch_result$branches
      omega_draws <- branch_result$omega_draws

      log_L_bar <- average_branches(branches)

      out <- list(
        log_L_bar_df = log_L_bar$df,
        branch_mat   = log_L_bar$branch_mat,
        branches     = branches,
        omega_draws  = omega_draws,
        theta_mle    = cal$theta_mle,
        psi_mle      = cal$psi_mle,
        calibration  = cal,
        status       = "success"
      )

      class(out) <- "integrated_likelihood"
      out
    },

    error = function(e) {
      fallback <- branch_result
      fallback$status    <- "failed"
      fallback$error_msg <- conditionMessage(e)

      if (verbose) {
        cat("[integrate] WARNING: Final averaging failed.\n")
      }
      fallback
    }
  )

  if (verbose) cat("[integrate] Finished.\n")

  return(out)
}
