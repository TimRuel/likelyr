# ======================================================================
# Integrated Likelihood Fit (Monte Carlo)
# ======================================================================

#' Integrated Likelihood Fit
#'
#' @description
#' Executes Monte Carlo integrated likelihood using the workflow's model,
#' estimand, nuisance, optimizer, and execution settings. This triggers:
#'   (1) nuisance sampling,
#'   (2) branch mode solving,
#'   (3) adaptive branch extension, and
#'   (4) log-sum-exp aggregation.
#'
#' @param cal     A `likelihood_calibration` object from [calibrate()]
#' @param verbose Logical; print progress information (default = TRUE)
#'
#' @return An S3 object of class `likelihood_integrated` containing:
#'   \item{il_curve}{data.frame with columns psi and loglik}
#'   \item{branches}{list of data.frames (one per branch)}
#'   \item{omega_draws}{list of nuisance parameter draws}
#'   \item{theta_mle}{full-model MLE for theta}
#'   \item{psi_mle}{estimand evaluated at theta_mle}
#'   \item{workflow}{the originating workflow}
#'   \item{calibration}{input calibration object}
#'
#' @export
fit_integrated <- function(cal, verbose = TRUE) {

  # --- Validation -------------------------------------------------------
  if (!inherits(cal, "likelihood_calibration")) {
    stop("`fit_integrated()` requires a `likelihood_calibration` object.",
         call. = FALSE)
  }

  wf <- cal$workflow

  # --- Monte Carlo branch computation ----------------------------------
  branch_result <- generate_branches(
    cal     = cal,
    verbose = verbose
  )

  branches     <- branch_result$branches
  omega_draws  <- branch_result$omega_draws

  # --- Average via log-sum-exp -----------------------------------------
  il_curve <- average_branches(branches)

  # --- Construct output -------------------------------------------------
  out <- list(
    il_curve    = il_curve,
    branches    = branches,
    omega_draws = omega_draws,
    theta_mle   = cal$theta_mle,
    psi_mle     = cal$psi_mle,
    workflow    = wf,
    calibration = cal
  )

  class(out) <- "likelihood_integrated"
  out
}
