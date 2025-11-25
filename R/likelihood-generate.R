# ======================================================================
# Integrated Likelihood & Profile Likelihood: GENERATE API
# ======================================================================
# User-facing:
#
#     out <- generate(cal)
#
# Dispatches to the appropriate method based on calibration type:
#     * generate.IL_calibration()
#     * generate.PL_calibration()
#
# Returned objects have class:
#     * "likelihood_integrated"
#     * "likelihood_profile"
#
# ======================================================================


#' Generate Integrated or Profile Likelihood
#'
#' @description
#' Generic user-facing entry point for likelihood generation.
#'
#' After a workflow has been calibrated with [calibrate()], calling:
#'
#' \preformatted{
#'   out <- generate(cal)
#' }
#'
#' will dispatch to:
#'
#' * [generate.IL_calibration()] for integrated likelihood
#' * [generate.PL_calibration()] for profile likelihood
#'
#' depending on the class of `cal`.
#'
#' @param cal A calibration object produced by [calibrate()], with class
#'   `"IL_calibration"` or `"PL_calibration"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return
#' A likelihood object of class:
#'
#' * `"likelihood_integrated"` for IL
#' * `"likelihood_profile"` for PL
#'
#' containing:
#' \describe{
#'   \item{il_curve / pl_curve}{A data frame of ψ vs. log-likelihood}
#'   \item{branches}{List of branch data frames}
#'   \item{omega_draws}{For IL only: nuisance draws ω̂}
#'   \item{theta_mle}{Global MLE of the model parameters}
#'   \item{psi_mle}{MLE of the estimand ψ}
#'   \item{workflow}{Original workflow}
#'   \item{calibration}{Calibration object `cal`}
#' }
#'
#' @export
generate <- function(cal, ...) {
  UseMethod("generate")
}


#' @rdname generate
#' @export
generate.default <- function(cal, ...) {
  stop("`generate()` requires a calibrated workflow object. ",
       "Did you call calibrate(wf, data) first?",
       call. = FALSE)
}


# ======================================================================
# IL: Integrated Likelihood Generation
# ======================================================================

#' Generate Monte Carlo Integrated Likelihood
#'
#' @description
#' Computes the **Monte Carlo integrated likelihood** for a calibrated IL
#' workflow.
#'
#' This function:
#'
#' 1. Draws nuisance-manifold samples ω̂ using
#'    `cal$il$sample_omega_hat(init)`
#' 2. Builds branch-specific ψ-evaluators using
#'    [build_eval_psi_fun()]
#' 3. For each ω̂, constructs a full left+right branch using
#'    [generate_branches()]
#' 4. Averages the branches on the log scale via [average_branches()]
#'
#' The resulting integrated log-likelihood curve is returned as an object
#' of class `"likelihood_integrated"`.
#'
#' @param cal An `"IL_calibration"` object from [calibrate(type = "IL")].
#' @param verbose Logical; print progress messages.
#' @param ... Additional arguments forwarded to lower-level branch
#'   generation utilities.
#'
#' @return
#' An object of class `"likelihood_integrated"` with elements:
#' \describe{
#'   \item{il_curve}{ψ-grid and integrated log-likelihood}
#'   \item{branches}{List of Monte Carlo branch data frames}
#'   \item{omega_draws}{List or matrix of nuisance draws ω̂}
#'   \item{theta_mle}{Model MLE θ̂}
#'   \item{psi_mle}{Estimand MLE ψ̂}
#'   \item{workflow}{Original workflow}
#'   \item{calibration}{Calibration object `cal`}
#' }
#'
#' @export
generate.IL_calibration <- function(cal, verbose = TRUE, ...) {

  if (!inherits(cal, "IL_calibration")) {
    stop("`generate()` for IL requires an `IL_calibration` object.",
         call. = FALSE)
  }

  wf <- cal$workflow

  if (verbose) {
    cat("[generate] Computing Monte Carlo Integrated Likelihood...\n")
  }

  # ---- 1. Branch computation -------------------------------------------
  branch_result <- generate_branches(
    cal     = cal,
    verbose = verbose,
    ...
  )

  branches    <- branch_result$branches
  omega_draws <- branch_result$omega_draws

  # ---- 2. Log-sum-exp averaging ----------------------------------------
  log_L_bar <- average_branches(branches)

  # ---- 3. Assemble output ----------------------------------------------
  out <- list(
    log_L_bar_df = log_L_bar$df,
    branch_mat   = log_L_bar$branch_mat,
    branches     = branches,
    omega_draws  = omega_draws,
    theta_mle    = cal$theta_mle,
    psi_mle      = cal$psi_mle,
    calibration  = cal
  )

  class(out) <- "likelihood_integrated"
  out
}


# ======================================================================
# PL: Profile Likelihood Generation (placeholder)
# ======================================================================

#' Generate Profile Likelihood (not yet implemented)
#'
#' @description
#' Placeholder method for profile likelihood generation.
#' Will eventually mirror the IL branch-generation framework.
#'
#' @param cal A `"PL_calibration"` object.
#' @param verbose Logical; print progress messages.
#' @param ... Ignored.
#'
#' @return
#' Currently throws an error.
#'
#' @export
generate.PL_calibration <- function(cal, verbose = TRUE, ...) {
  stop("Profile likelihood generation not yet implemented.")
}
