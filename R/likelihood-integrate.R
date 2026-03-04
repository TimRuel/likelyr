# ======================================================================
# likelihood-integrate.R — Integrated Likelihood (post-screening)
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood using *pre-screened* omega-hat
#' values stored in the calibrated model object.
#'
#' This function assumes [screen()] has already been run and will error
#' otherwise. No omega-hat sampling occurs inside integrate().
#'
#' @param cal A `calibrated` model object.
#' @param verbose Logical; print diagnostics. Default: FALSE.
#' @param ... Additional arguments passed to `generate_branches()`.
#'
#' @return The SAME `calibrated` model object, augmented with:
#'   • class `integrated`
#'   • `$workspace$integrate` — integrated likelihood result
#'
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
  # 0A. Ensure object has been calibrated
  # ------------------------------------------------------------------
  if (!is_calibrated(cal)) {
    stop(
      "integrate() requires a model that has been calibrated via calibrate().",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 0B. Ensure structural completeness
  # ------------------------------------------------------------------
  validate_integrate_input(cal)

  # ------------------------------------------------------------------
  # 0C. Require pre-screened omega-hats
  # ------------------------------------------------------------------
  ws <- cal$workspace
  omega_hats <- ws$integrate$omega_hats %||% NULL

  if (is.null(omega_hats) || length(omega_hats) == 0L) {
    stop(
      "integrate() requires pre-screened omega-hats.\n",
      "Run screen(cal) before integrate().",
      call. = FALSE
    )
  }

  R <- length(omega_hats)

  # Pull calibrated quantities
  param_mle <- cal$parameter$param_mle
  psi_mle <- cal$estimand$psi_mle
  exec <- cal$execution

  # ------------------------------------------------------------------
  # 1. Initialize integrate workspace (omega-hats already present)
  # ------------------------------------------------------------------
  if (is.null(cal$workspace)) {
    cal$workspace <- list()
  }

  cal$workspace$integrate <- list(
    omega_hats = omega_hats
  )

  # Mark lifecycle state
  cal <- mark_integrated(cal)

  # ------------------------------------------------------------------
  # 2. Execution summary
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[integrate] Integrated Log-Likelihood\n")
    cat(
      "[integrate] Execution: ",
      if (inherits(exec, "parallel_spec")) "PARALLEL" else "SERIAL",
      " | Branches: ",
      R,
      "\n",
      sep = ""
    )
  }

  # ------------------------------------------------------------------
  # 3. Branch computation (uses fixed omega-hats)
  # ------------------------------------------------------------------
  branch_result <- generate_branches(
    cal = cal,
    verbose = verbose,
    ...
  )

  # ------------------------------------------------------------------
  # 4. Final aggregation (log-sum-exp)
  # ------------------------------------------------------------------
  integrate_result <- tryCatch(
    {
      branches <- branch_result$branches
      branch_agg_args <- cal$optimizer$branch_agg_args

      branch_agg <- aggregate_branches(
        branches,
        min_points = branch_agg_args$min_points,
        q_delta = branch_agg_args$q_delta,
        delta_min = branch_agg_args$delta_min,
        delta_max = branch_agg_args$delta_max,
        min_support = branch_agg_args$min_support
      )

      new_integrate_result(list(
        psi_ll_df = branch_agg$psi_ll_df,
        branch_mat = branch_agg$branch_mat,
        R_eff = branch_agg$R_eff,
        branches = branches,
        omega_draws = omega_hats,
        param_mle = param_mle,
        psi_mle = psi_mle,
        status = "success"
      ))
    },
    error = function(e) {
      if (verbose) {
        cat("[integrate] WARNING: Final aggregation failed.\n")
      }

      new_integrate_result(list(
        status = "failed",
        error_msg = conditionMessage(e),
        branches = branch_result$branches,
        omega_draws = omega_hats
      ))
    }
  )

  # ------------------------------------------------------------------
  # 5. Store final integrate result
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
