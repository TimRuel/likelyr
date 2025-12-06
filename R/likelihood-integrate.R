# ======================================================================
# likelihood-integrate.R  — Unified likelyr API version
# ======================================================================

#' Integrated Likelihood
#'
#' @description
#' Computes the integrated likelihood (IL) and attaches it to the input
#' `calibrated_model` under `$results$IL`. The updated object is returned.
#'
#' This function is **silent by default** for pipe-friendly workflows.
#' Set `verbose = TRUE` to display diagnostic messages.
#'
#' @param cal A calibrated_model.
#' @param verbose Logical; print diagnostics. Default: FALSE.
#' @param ... Additional arguments passed to `generate_branches()`.
#'
#' @return The SAME calibrated_model object, augmented with:
#'         • class `"likelyr_integrated"`
#'         • `$results$IL` — a `likelyr_il_result` object
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
integrate.calibrated_model <- function(cal, verbose = FALSE, ...) {

  # ------------------------------------------------------------------
  # 0A. Ensure object has been calibrated properly
  # ------------------------------------------------------------------
  if (!isTRUE(cal$.__calibrated__)) {
    stop("integrate() requires a model that has been calibrated via calibrate().",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 0B. Ensure model_spec is complete for IL generation
  # ------------------------------------------------------------------
  .validate_model_for_generation(cal)

  # Pull calibrated quantities
  psi_fn    <- cal$estimand$psi_fn
  psi_mle   <- cal$estimand$psi_mle
  theta_mle <- cal$likelihood$theta_mle

  # Ensure results list is present
  if (is.null(cal$results)) cal$results <- list()

  # ------------------------------------------------------------------
  # 1. Create IL working area inside results$IL
  # ------------------------------------------------------------------
  constraint_fn    <- function(theta) psi_fn(theta) - psi_mle
  generate_init    <- make_omega_hat_initgen(cal)
  sample_omega_hat <- make_omega_hat_sampler(cal)

  cal$results$IL <- list(
    constraint_fn    = constraint_fn,
    generate_init    = generate_init,
    sample_omega_hat = sample_omega_hat
  )

  # Mark object
  cal <- mark_integrated(cal)

  # ------------------------------------------------------------------
  # 2. Display execution summary (only if verbose)
  # ------------------------------------------------------------------
  exec <- cal$execution

  if (verbose) {
    cat("[integrate] Monte Carlo Integrated Likelihood\n")
    cat("[integrate] Mode:",
        if (inherits(exec, "parallel_spec")) "PARALLEL" else "SERIAL",
        "| Branches:", exec$total_branches, "\n")
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
  il_result <- tryCatch({

    branches    <- branch_result$branches
    omega_draws <- branch_result$omega_draws

    log_L_bar <- average_branches(branches)

    new_il_result(list(
      log_L_bar_df = log_L_bar$df,
      branch_mat   = log_L_bar$branch_mat,
      branches     = branches,
      omega_draws  = omega_draws,
      theta_mle    = theta_mle,
      psi_mle      = psi_mle,
      status       = "success"
    ))

  }, error = function(e) {

    if (verbose)
      cat("[integrate] WARNING: Final averaging failed.\n")

    new_il_result(list(
      status      = "failed",
      error_msg   = conditionMessage(e),
      branches    = branch_result$branches,
      omega_draws = branch_result$omega_draws
    ))
  })

  # ------------------------------------------------------------------
  # 5. Replace IL working area with final result
  # ------------------------------------------------------------------
  cal$results$IL <- il_result

  if (verbose) cat("[integrate] Finished.\n")

  cal
}

# ======================================================================
# INTERNAL VALIDATION FOR IL GENERATION
# ======================================================================

.validate_model_for_generation <- function(cal) {

  model <- cal  # a calibrated_model inherits model_spec fields

  if (!.is_model_spec_generation_complete(model)) {

    missing <- c()
    if (is.null(model$likelihood)) missing <- c(missing, "likelihood_spec()")
    if (is.null(model$estimand))  missing <- c(missing, "estimand_spec()")
    if (is.null(model$nuisance))  missing <- c(missing, "nuisance_spec()")
    if (is.null(model$optimizer)) missing <- c(missing, "optimizer_spec()")
    if (is.null(model$execution)) missing <- c(missing, "execution_spec()")

    stop(
      "[integrate] Model is not ready for integrated likelihood.\n",
      "Missing required specifications:\n  - ",
      paste(missing, collapse = "\n  - "),
      "\nAdd missing specs using add(model, spec) before calling integrate().",
      call. = FALSE
    )
  }

  invisible(cal)
}

# ======================================================================
# PRINT AND SUMMARY METHODS
# ======================================================================

#' @export
print.likelyr_il_result <- function(x, ...) {
  cat("<Integrated Likelihood Result>\n")
  cat("Status: ", x$status, "\n", sep = "")

  if (!is.null(x$psi_mle))
    cat("psi_MLE: ", format(x$psi_mle), "\n", sep = "")

  if (!is.null(x$theta_mle))
    cat("theta_MLE: (",
        paste(format(x$theta_mle), collapse = ", "),
        ")\n", sep = "")

  if (!is.null(x$log_L_bar_df))
    cat("Grid points: ", nrow(x$log_L_bar_df), "\n", sep = "")

  invisible(x)
}

#' @export
summary.likelyr_il_result <- function(object, ...) {
  out <- list(
    status     = object$status,
    psi_mle    = object$psi_mle,
    theta_mle  = object$theta_mle,
    n_grid     = if (!is.null(object$log_L_bar_df))
      nrow(object$log_L_bar_df) else NA_integer_,
    n_branches = if (!is.null(object$branches))
      length(object$branches) else NA_integer_
  )

  class(out) <- "summary_likelyr_il_result"
  out
}

#' @export
print.summary_likelyr_il_result <- function(x, ...) {
  cat("<Summary: Integrated Likelihood>\n")
  cat("Status:        ", x$status, "\n", sep = "")
  cat("psi_MLE:       ", format(x$psi_mle), "\n", sep = "")
  cat("theta_MLE:     ", paste(format(x$theta_mle), collapse = ", "), "\n", sep = "")
  cat("# Grid points: ", x$n_grid, "\n", sep = "")
  cat("# Branches:    ", x$n_branches, "\n", sep = "")
  invisible(x)
}
