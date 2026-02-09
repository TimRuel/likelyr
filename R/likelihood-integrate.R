# ======================================================================
# likelihood-integrate.R  — Unified likelyr API version (HPC-safe)
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood and attaches it to the input
#' `calibrated` model object under `$workspace$integrate`. The updated object
#' is returned.
#'
#' This function performs **no plotting or table rendering**. Visualization
#' is deferred to `plot()` / `view()` methods, which materialize plots/tables
#' locally from stored data frames.
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
    stop(
      "integrate() requires a model that has been calibrated via calibrate().",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 0B. Ensure model_spec is complete for likelihood integration
  # ------------------------------------------------------------------
  validate_integrate_input(cal)

  # Pull calibrated quantities
  psi_fn <- cal$estimand$psi_fn
  psi_mle <- cal$estimand$psi_mle
  param_mle <- cal$parameter$param_mle

  # ------------------------------------------------------------------
  # 1. Create `integrate` working area inside workspace$integrate
  # ------------------------------------------------------------------
  generate_init <- make_omega_hat_initgen(cal)
  sample_omega_hat <- make_omega_hat_sampler(cal)

  if (is.null(cal$workspace)) {
    cal$workspace <- list()
  }

  cal$workspace$integrate <- list(
    generate_init = generate_init,
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
    cat(
      "[integrate] Execution:",
      if (inherits(exec, "parallel_spec")) "PARALLEL" else "SERIAL",
      "| Branches:",
      exec$total_branches,
      "\n"
    )
  }

  # ------------------------------------------------------------------
  # 3. Branch computation (HPC-safe; may rely on nloptr, parallel, etc.)
  # ------------------------------------------------------------------
  branch_result <- generate_branches(
    cal = cal,
    verbose = verbose,
    ...
  )

  # ------------------------------------------------------------------
  # 4. Final aggregation (log-sum-exp)
  #    NOTE: No plotting or rendering here; only store data frames.
  # ------------------------------------------------------------------
  integrate_result <- tryCatch(
    {
      branches <- branch_result$branches
      omega_draws <- branch_result$omega_draws

      branch_avg <- average_branches(branches)

      new_integrate_result(list(
        psi_ll_df = branch_avg$psi_ll_df,
        branch_mat = branch_avg$branch_mat,
        branches = branches,
        omega_draws = omega_draws,
        param_mle = param_mle,
        psi_mle = psi_mle,
        status = "success"
      ))
    },
    error = function(e) {
      if (verbose) {
        cat("[integrate] WARNING: Final averaging failed.\n")
      }

      new_integrate_result(list(
        status = "failed",
        error_msg = conditionMessage(e),
        branches = branch_result$branches,
        omega_draws = branch_result$omega_draws
      ))
    }
  )

  # ------------------------------------------------------------------
  # 5. Replace `integrate` working area with final result
  # ------------------------------------------------------------------
  cal$workspace$integrate <- integrate_result

  if (verbose) {
    cat("[integrate] Finished.\n")
  }

  cal
}

# ======================================================================
# INTERNAL VALIDATION FOR LIKELIHOOD INTEGRATION WRT NUISANCE PARAMETER
# ======================================================================

#' Validate inputs prior to integrated likelihood computation
#'
#' @description
#' Checks that a model object is fully specified and ready for
#' integrated likelihood estimation. This validator ensures that
#' *all required structural components* are present before
#' \code{integrate()} is allowed to proceed.
#'
#' Specifically, this function verifies the presence of:
#' \itemize{
#'   \item \code{parameter_spec()}
#'   \item \code{likelihood_spec()}
#'   \item \code{estimand_spec()}
#'   \item \code{nuisance_spec()}
#'   \item \code{optimizer_spec()}
#'   \item \code{execution_spec()}
#' }
#'
#' If any components are missing, a detailed error message is
#' generated listing the missing specifications and explaining
#' how to fix the issue.
#'
#' @param cal A model object intended for integrated likelihood
#'   computation.
#'
#' @return Invisibly returns \code{cal} if validation succeeds.
#'
#' @keywords internal
#' @noRd
validate_integrate_input <- function(cal) {
  model <- cal

  if (!.is_model_spec_complete(model)) {
    missing <- c()

    if (is.null(model$parameter)) {
      missing <- c(missing, "parameter_spec()")
    }
    if (is.null(model$likelihood)) {
      missing <- c(missing, "likelihood_spec()")
    }
    if (is.null(model$estimand)) {
      missing <- c(missing, "estimand_spec()")
    }
    if (is.null(model$nuisance)) {
      missing <- c(missing, "nuisance_spec()")
    }
    if (is.null(model$optimizer)) {
      missing <- c(missing, "optimizer_spec()")
    }
    if (is.null(model$execution)) {
      missing <- c(missing, "execution_spec()")
    }

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

#' @method print integrate
#' @export
print.integrate <- function(x, ...) {
  cat("<Integrated Log-Likelihood Result>\n")
  cat("Status: ", x$status, "\n", sep = "")

  # -----------------------------------------------------------
  # Lifecycle flags (slot presence, not helpers)
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

  if (!is.null(x$branches)) {
    cat("# Branches:    ", length(x$branches), "\n", sep = "")
  }

  invisible(x)
}

# =====================================================================
# S3 Plot Method (local-only materialization)
# =====================================================================

#' @method plot integrate
#' @export
plot.integrate <- function(x, ...) {
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
