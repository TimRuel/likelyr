# ======================================================================
# likelihood-profile.R — Profile likelihood API
# ======================================================================

#' Profile Likelihood
#'
#' @description
#' Computes the profile likelihood (PL) and attaches it to `$results$PL`
#' inside the calibrated_model. The PL is a *single branch* of the IL with
#' nuisance parameters fixed at their MLE (i.e., omega_hat = theta_MLE).
#'
#' @param cal A calibrated_model object
#' @param verbose Logical; print diagnostics
#' @param ... Additional arguments passed to build_one_branch()
#'
#' @return The SAME calibrated_model, augmented with:
#'   * class "likelyr_profile"
#'   * results$PL — a likelyr_profile_result object
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
  if (!isTRUE(cal$.__calibrated__)) {
    stop("profile() requires calibrate() first.", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 0B. Ensure model spec ready for likelihood generation
  # ------------------------------------------------------------------
  .validate_model_for_generation(cal)

  # Pull calibrated quantities
  psi_mle    <- cal$estimand$psi_mle
  psi_fn     <- cal$estimand$psi_fn
  theta_mle  <- cal$likelihood$theta_mle

  # ------------------------------------------------------------------
  # 2. Execution summary
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[profile] Profile Likelihood\n")
  }

  # ------------------------------------------------------------------
  # 3. Build ONE branch
  # ------------------------------------------------------------------
  # Required pieces for build_one_branch():
  #   psi_hat_branch   = psi_mle
  #   theta_hat_branch = theta_mle
  #   branch_cutoff    = exec$branch_cutoff
  #   grid             = exec$grid
  #   eval_psi_fun     = eval_psi   (via calibrate)
  #   max_retries      = exec$max_retries

  grid <- psi_grid_anchor(
    psi_mle   = psi_mle,
    increment = increment
  )

  loglik_at_mle <- cal$likelihood$loglik(theta_mle)

  alpha_target <- min(1 - estimand$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)

  branch_cutoff <- loglik_at_mle - crit

  eval_psi_fun <- eval_psi_builder(theta_mle)

  profile_df <- tryCatch({

    build_one_branch(
      psi_hat_branch   = psi_mle,
      theta_hat_branch = theta_mle,
      branch_cutoff    = branch_cutoff,
      grid             = grid,
      eval_psi_fun     = eval_psi_fun,
      max_retries      = optimizer$max_retries,
      ...
    )

  }, error = function(e) {
    if (verbose) {
      cat("[profile] ERROR in build_one_branch():\n")
      message(e)
    }
    return(NULL)
  })

  # ------------------------------------------------------------------
  # 4. Wrap into a likelyr_profile_result object
  # ------------------------------------------------------------------
  pl_result <- tryCatch({

    new_profile_result(list(
      psi_mle    = psi_mle,
      theta_mle  = theta_mle,
      profile_df  = profile_df,
      status     = if (!is.null(profile_df)) "success" else "failed"
    ))

  }, error = function(e) {

    new_profile_result(list(
      psi_mle    = psi_mle,
      theta_mle  = theta_mle,
      profile_df  = profile_df,
      status     = "failed",
      error_msg  = conditionMessage(e)
    ))

  })

  # ------------------------------------------------------------------
  # 5. Store final result and finish
  # ------------------------------------------------------------------

  # Ensure results list exists
  if (is.null(cal$results)) cal$results <- list()

  cal$results$PL <- pl_result

  # Mark this model as profile likelihood
  cal <- mark_profile(cal)

  if (verbose) cat("[profile] Finished.\n")

  cal
}

# ======================================================================
# RESULT CLASS SUPPORT
# ======================================================================

new_profile_result <- function(x) {
  class(x) <- c("likelyr_profile_result")
  x
}

#' @export
print.likelyr_profile_result <- function(x, ...) {
  cat("<Profile Likelihood Result>\n")
  cat("Status: ", x$status, "\n", sep = "")
  cat("psi_MLE: ", format(x$psi_mle), "\n", sep = "")
  cat("theta_MLE: (", paste(format(x$theta_mle), collapse = ", "), ")\n", sep = "")

  if (!is.null(x$profile_df))
    cat("Grid points: ", nrow(x$profile_df), "\n", sep = "")

  invisible(x)
}

# ----------------------------------------------------------------------

#' @export
summary.likelyr_profile_result <- function(object, ...) {

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
print.summary_likelyr_profile_result <- function(x, ...) {
  cat("<Summary: Profile Likelihood>\n")
  cat("Status:        ", x$status, "\n", sep = "")
  cat("psi_MLE:       ", format(x$psi_mle), "\n", sep = "")
  cat("theta_MLE:     ",
      paste(format(x$theta_mle), collapse = ", "), "\n", sep = "")
  cat("# Grid points: ", x$n_grid, "\n", sep = "")
  invisible(x)
}
