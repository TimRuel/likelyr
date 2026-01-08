# ======================================================================
# likelihood-integrate.R  — Unified likelyr API version
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood and attaches it to the input
#' `calibrated` model object under `$workspace$integrate`. The updated object is returned.
#'
#' This function is **silent by default** for pipe-friendly workflows.
#' Set `verbose = TRUE` to display diagnostic messages.
#'
#' @param cal A `calibrated` model object.
#' @param verbose Logical; print diagnostics. Default: FALSE.
#' @param ... Additional arguments passed to `generate_branches()`.
#'
#' @return The SAME `calibrated` model object, augmented with:
#'         • class `integrated`
#'         • `$workspace$integrate` — an `integrate` object
#' @export
integrate <- function(cal, ...) {
  UseMethod("integrate")
}

# ----------------------------------------------------------------------

#' @export
integrate.default <- function(cal, ...) {
  stop("integrate() requires a 'calibrated' model object.", call. = FALSE)
}

# ----------------------------------------------------------------------

#' @export
integrate.calibrated <- function(cal, verbose = FALSE, ...) {

  # ------------------------------------------------------------------
  # 0A. Ensure object has been calibrated properly
  # ------------------------------------------------------------------
  if (!is_calibrated(cal)) {
    stop("integrate() requires a model that has been calibrated via calibrate().",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 0B. Ensure model_spec is complete for likelihood integration
  # ------------------------------------------------------------------
  validate_integrate_input(cal)

  # Pull calibrated quantities
  psi_fn    <- cal$estimand$psi_fn
  psi_mle   <- cal$estimand$psi_mle
  theta_mle <- cal$parameter$theta_mle

  # ------------------------------------------------------------------
  # 1. Create `integrate` working area inside workspace$integrated
  # ------------------------------------------------------------------
  constraint_fn    <- function(theta) psi_fn(theta) - psi_mle
  generate_init    <- make_omega_hat_initgen(cal)
  sample_omega_hat <- make_omega_hat_sampler(cal)

  cal$workspace$integrate <- list(
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
    cat("[integrate] Monte Carlo Integrated Log-Likelihood\n")
    cat("[integrate] Execution:",
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
  integrate_result <- tryCatch({

    branches    <- branch_result$branches
    omega_draws <- branch_result$omega_draws

    branch_avg <- average_branches(branches)

    pseudolikelihood_points <- plot_pseudolikelihood_points(branch_avg$psi_ll_df)

    new_integrate_result(list(
      psi_ll_df               = branch_avg$psi_ll_df,
      branch_mat              = branch_avg$branch_mat,
      branches                = branches,
      omega_draws             = omega_draws,
      theta_mle               = theta_mle,
      psi_mle                 = psi_mle,
      status                  = "success",
      pseudolikelihood_points = pseudolikelihood_points
    ))

  }, error = function(e) {

    if (verbose)
      cat("[integrate] WARNING: Final averaging failed.\n")

    new_integrate_result(list(
      status      = "failed",
      error_msg   = conditionMessage(e),
      branches    = branch_result$branches,
      omega_draws = branch_result$omega_draws
    ))
  })

  # ------------------------------------------------------------------
  # 5. Replace `integrate` working area with final result
  # ------------------------------------------------------------------
  if (is.null(cal$workspace)) cal$workspace <- list()
  cal$workspace$integrate <- integrate_result

  if (verbose) cat("[integrate] Finished.\n")

  cal
}

# ======================================================================
# INTERNAL VALIDATION FOR LIKELIHOOD INTEGRATION WRT NUISANCE PARAMETER
# ======================================================================

validate_integrate_input <- function(cal) {

  model <- cal

  if (!.is_model_spec_complete(model)) {

    missing <- c()
    if (is.null(model$parameter)) missing <- c(missing, "parameter_spec()")
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
print.integrate <- function(x, ...) {
  cat("<Integrated Log-Likelihood Result>\n")
  cat("Status: ", x$status, "\n", sep = "")

  if (!is.null(x$psi_mle))
    cat("psi_MLE: ", format(x$psi_mle), "\n", sep = "")

  if (!is.null(x$theta_mle))
    cat("theta_MLE: (",
        paste(format(x$theta_mle), collapse = ", "),
        ")\n", sep = "")

  if (!is.null(x$psi_ll_df))
    cat("Grid points: ", nrow(x$psi_ll_df), "\n", sep = "")

  invisible(x)
}

#' @export
summary.integrate <- function(object, ...) {
  out <- list(
    status     = object$status,
    psi_mle    = object$psi_mle,
    theta_mle  = object$theta_mle,
    n_grid     = if (!is.null(object$psi_ll_df))
      nrow(object$psi_ll_df) else NA_integer_,
    n_branches = if (!is.null(object$branches))
      length(object$branches) else NA_integer_
  )

  class(out) <- "summary_integrate"
  out
}

#' @export
print.summary_integrate <- function(x, ...) {
  cat("<Summary: Integrated Log-Likelihood>\n")
  cat("Status:        ", x$status, "\n", sep = "")
  cat("psi_MLE:       ", format(x$psi_mle), "\n", sep = "")
  cat("theta_MLE:     ", paste(format(x$theta_mle), collapse = ", "), "\n", sep = "")
  cat("# Grid points: ", x$n_grid, "\n", sep = "")
  cat("# Branches:    ", x$n_branches, "\n", sep = "")
  invisible(x)
}

# =====================================================================
# S3 Plot Method
# =====================================================================

#' @export
plot.integrate <- function(x, ...) {

  if (is.null(x$pseudolikelihood_points)) {
    stop("No plot available in integrated log-likelihood result.", call. = FALSE)
  }

  x$pseudolikelihood_points
}
