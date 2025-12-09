# ======================================================================
# likelihood-profile.R — Profile Likelihood API
# ======================================================================

#' Profile Likelihood
#'
#' @description
#' Computes the *profile likelihood* curve for the calibrated model.
#'
#' Unlike the integrated likelihood (IL), the profile likelihood (PL)
#' does **not** involve ω̂–sampling or Monte Carlo methods. Instead,
#' nuisance parameters are fixed at their MLE:
#'
#' \preformatted{
#'    ω̂ = θ̂_MLE
#' }
#'
#' The likelihood is then evaluated along a ψ–grid, forming a *single
#' deterministic branch*. The result is attached to:
#'
#' \preformatted{
#'   cal$results$PL
#' }
#'
#' @param cal A `calibrated_model` produced by [calibrate()].
#' @param verbose Logical; print diagnostic messages (default FALSE).
#' @param ... Additional arguments forwarded to [generate_profile()].
#'
#' @return
#' The SAME calibrated_model object, augmented with:
#'   * `results$PL` — a `likelyr_profile_result` object
#'   * class `"likelyr_profiled"`
#'
#' @examples
#' \dontrun{
#' cal <- calibrate(model, data)
#' cal <- profile(cal)
#' plot(cal$results$PL)
#' }
#'
#' @export
profile <- function(cal, ...) {
  UseMethod("profile")
}

# ----------------------------------------------------------------------

#' @export
profile.default <- function(cal, ...) {
  stop("profile() requires a calibrated_model.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
profile.calibrated_model <- function(cal, verbose = FALSE, ...) {

  # ------------------------------------------------------------------
  # 0A. Ensure calibration has occurred
  # ------------------------------------------------------------------
  if (!isTRUE(cal$.__calibrated__))
    stop("profile() requires calibrate() first.", call. = FALSE)

  # ------------------------------------------------------------------
  # 0B. Profile likelihood does NOT require optimizer/execution specs
  # ------------------------------------------------------------------
  .validate_model_for_profile(cal)

  # ------------------------------------------------------------------
  # 1. Extract calibrated quantities
  # ------------------------------------------------------------------
  estimand   <- cal$estimand
  psi_mle    <- estimand$psi_mle
  psi_fn     <- estimand$psi_fn
  theta_mle  <- cal$parameter$theta_mle
  loglik_fn  <- cal$likelihood$loglik

  # ------------------------------------------------------------------
  # 2. Execution summary
  # ------------------------------------------------------------------
  if (verbose) cat("[profile] Profile Likelihood\n")

  # ------------------------------------------------------------------
  # 3. Compute branch cutoff from confidence levels
  # ------------------------------------------------------------------
  loglik_at_mle <- loglik_fn(theta_mle)

  alpha_target <- min(1 - estimand$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  cutoff_buffer  <- estimand$cutoff_buffer %||% 0
  effective_crit <- crit * (1 + cutoff_buffer)

  cutoff <- loglik_at_mle - effective_crit

  # Build ψ→loglik evaluator at θ̂
  eval_psi_fun <- build_eval_psi_fun(cal)(theta_mle)

  # ------------------------------------------------------------------
  # 4. Construct branch grid and walk it
  # ------------------------------------------------------------------

  # increment & max_retries must come from execution or default settings
  increment   <- cal$execution$increment   %||% 0.05
  max_retries <- cal$execution$max_retries %||% 4

  profile_df <- tryCatch(

    generate_profile(
      psi_mle       = psi_mle,
      theta_mle     = theta_mle,
      loglik_at_mle = loglik_at_mle,
      increment     = increment,
      cutoff        = cutoff,
      eval_psi_fun  = eval_psi_fun,
      max_retries   = max_retries,
      ...
    ),

    error = function(e) {
      if (verbose) {
        cat("[profile] ERROR in generate_profile():\n")
        message(e)
      }
      return(NULL)
    }
  )

  # ------------------------------------------------------------------
  # 5. Wrap into likelyr_profile_result
  # ------------------------------------------------------------------
  pl_result <- new_pl_result(list(
    profile_df = profile_df,
    psi_mle    = psi_mle,
    theta_mle  = theta_mle,
    status     = if (!is.null(profile_df)) "success" else "failed"
  ))

  # ------------------------------------------------------------------
  # 6. Store and return
  # ------------------------------------------------------------------
  if (is.null(cal$results)) cal$results <- list()
  cal$results$PL <- pl_result

  cal <- mark_profiled(cal)

  if (verbose) cat("[profile] Finished.\n")

  cal
}

# ======================================================================
# VALIDATION
# ======================================================================

# Profile likelihood requires:
#   • likelihood spec
#   • estimand spec
#   • nuisance spec
# but NOT optimizer or execution specs.
.validate_model_for_profile <- function(cal) {

  if (!inherits(cal$parameter, "parameter_spec"))
    stop("model$parameter must be a 'parameter_spec' object.")

  if (!inherits(cal$likelihood, "likelihood_spec"))
    stop("model$likelihood must be a 'likelihood_spec' object.")

  if (!inherits(cal$estimand, "estimand_spec"))
    stop("model$estimand must be an 'estimand_spec' object.")

  if (!inherits(cal$nuisance, "nuisance_spec"))
    stop("model$nuisance must be a 'nuisance_spec' object.")

  invisible(cal)
}

# ======================================================================
# RESULT CLASS SUPPORT
# ======================================================================

#' @export
print.likelyr_pl_result <- function(x, ...) {
  cat("<Profile Likelihood Result>\n")
  if (!is.null(x$status))    cat("Status:     ", x$status, "\n", sep = "")
  if (!is.null(x$psi_mle))   cat("psi_MLE:    ", format(x$psi_mle), "\n", sep = "")
  if (!is.null(x$theta_mle)) cat("theta_MLE: (",
                                 paste(format(x$theta_mle), collapse = ", "),
                                 ")\n", sep = "")
  if (!is.null(x$profile_df))
    cat("Grid points:", nrow(x$profile_df), "\n")
  invisible(x)
}

# ----------------------------------------------------------------------

#' @export
summary.likelyr_pl_result <- function(object, ...) {
  out <- list(
    status     = object$status,
    psi_mle    = object$psi_mle,
    theta_mle  = object$theta_mle,
    n_grid     = if (!is.null(object$profile_df))
      nrow(object$profile_df) else NA_integer_
  )
  class(out) <- "summary_likelyr_profile_result"
  out
}

#' @export
print.summary_likelyr_pl_result <- function(x, ...) {
  cat("<Summary: Profile Likelihood>\n")
  cat("Status:        ", x$status, "\n", sep = "")
  cat("psi_MLE:       ", format(x$psi_mle), "\n", sep = "")
  cat("theta_MLE:     ",
      paste(format(x$theta_mle), collapse = ", "), "\n", sep = "")
  cat("# Grid points: ", x$n_grid, "\n", sep = "")
  invisible(x)
}
