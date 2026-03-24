# ======================================================================
# likelihood-integrate.R — Integrated Likelihood (post-sieving)
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood using pre-screened branch seeds
#' stored in the calibrated model object by \code{sieve()}.
#'
#' This function assumes \code{sieve()} has already been run and will
#' error otherwise. No omega-hat sampling or mode location occurs here.
#'
#' Requires \code{E_loglik} to be supplied in \code{likelihood_spec()}.
#' A clear error is raised if it is absent.
#'
#' After branch computation, \code{aggregate()} is called with
#' default parameters. Call \code{aggregate()} directly on the
#' returned model to re-aggregate with different settings without
#' repeating branch computation.
#'
#' @param cal     A \code{calibrated} model object.
#' @param verbose Logical; print diagnostics. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to \code{generate()}.
#'
#' @return The SAME \code{calibrated} model object, augmented with
#'   \code{$workspace$integrate} containing branches and the
#'   initial aggregation result.
#'
#' @export
integrate <- function(cal, ...) {
  UseMethod("integrate")
}

#' @export
integrate.default <- function(cal, ...) {
  stop("integrate() requires a 'calibrated' model object.", call. = FALSE)
}

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
  # 0B. Validate structural completeness
  # ------------------------------------------------------------------
  validate_integrate_input(cal)

  # ------------------------------------------------------------------
  # 0C. Require pre-sieved branch seeds
  # ------------------------------------------------------------------
  branch_seeds <- cal$workspace$integrate$branch_seeds %||% NULL

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "integrate() requires pre-sieved branch seeds.\n",
      "Pass the model through sieve() before running integrate().",
      call. = FALSE
    )
  }

  exec <- cal$execution

  # ------------------------------------------------------------------
  # 1. Re-initialise integrate workspace (preserve seeds and sieve diag)
  # ------------------------------------------------------------------
  sieve_diag <- cal$workspace$integrate$sieve
  common_interval <- cal$workspace$integrate$common_interval
  cal$workspace$integrate <- list(
    branch_seeds = branch_seeds,
    sieve = sieve_diag,
    common_interval = common_interval
  )

  # ------------------------------------------------------------------
  # 2. Execution summary
  # ------------------------------------------------------------------
  if (verbose) {
    cat("[integrate] Integrated Log-Likelihood\n")
    cat(
      "[integrate] Execution: ",
      if (inherits(exec, "parallel_spec")) "PARALLEL" else "SERIAL",
      " | Seeds: ",
      length(branch_seeds),
      "\n",
      sep = ""
    )
  }

  # ------------------------------------------------------------------
  # 3. Branch generation + aggregation with failure handling
  # ------------------------------------------------------------------
  cal <- tryCatch(
    {
      cal <- generate(cal, task = "integrate", verbose = verbose, ...)
      cal <- aggregate(cal, verbose = verbose)
      cal
    },
    error = function(e) {
      if (verbose) {
        cat("[integrate] ERROR during integration.\n")
      }
      cal$workspace$integrate$result <- list(
        status = "failed",
        error_msg = conditionMessage(e)
      )
      cal
    }
  )

  # ------------------------------------------------------------------
  # 4. Wrap result and mark
  # ------------------------------------------------------------------
  raw <- cal$workspace$integrate$result

  cal$workspace$integrate$result <- new_integrate_result(c(
    raw,
    list(status = raw$status %||% "success")
  ))

  cal <- mark_integrated(cal)

  if (verbose) {
    cat("[integrate] Finished.\n")
  }

  cal
}

# ======================================================================
# INTERNAL VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
validate_integrate_input <- function(cal) {
  missing <- character(0)

  if (!inherits(cal$parameter, "parameter_spec")) {
    missing <- c(missing, "parameter_spec()")
  }
  if (!inherits(cal$likelihood, "likelihood_spec")) {
    missing <- c(missing, "likelihood_spec()")
  }
  if (!inherits(cal$estimand, "estimand_spec")) {
    missing <- c(missing, "estimand_spec()")
  }
  if (!inherits(cal$sampler, "sampler_spec")) {
    missing <- c(missing, "sampler_spec()")
  }
  if (!inherits(cal$traversal, "traversal_spec")) {
    missing <- c(missing, "traversal_spec()")
  }
  if (!inherits(cal$solver, "solver_spec")) {
    missing <- c(missing, "solver_spec()")
  }
  if (!inherits(cal$execution, "execution_spec")) {
    missing <- c(missing, "execution_spec()")
  }

  if (length(missing) > 0) {
    stop(
      "[integrate] Model is not ready for integrated likelihood.\n",
      "Missing required specifications:\n  - ",
      paste(missing, collapse = "\n  - "),
      "\nAdd missing specs using add(model, spec) before calling integrate().",
      call. = FALSE
    )
  }

  if (is.null(cal$likelihood$E_loglik)) {
    stop(
      "[integrate] E_loglik is required for integrated likelihood.\n",
      "Supply it via likelihood_spec(E_loglik = ...).",
      call. = FALSE
    )
  }

  invisible(cal)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @method print integrate
#' @export
print.integrate <- function(x, ...) {
  cat("<Integrated Log-Likelihood Result>\n")
  cat("Status: ", x$status, "\n", sep = "")

  cat("Lifecycle:\n")
  cat("  inferred:   ", if (!is.null(x$inference)) "✓" else "×", "\n", sep = "")
  cat(
    "  diagnosed:  ",
    if (!is.null(x$diagnostics)) "✓" else "×",
    "\n",
    sep = ""
  )

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
  if (!is.null(x$psi_ll_df)) {
    cat("Grid points: ", nrow(x$psi_ll_df), "\n", sep = "")
  }
  if (!is.null(x$branches)) {
    cat("# Branches: ", length(x$branches), "\n", sep = "")
  }

  invisible(x)
}

# ======================================================================
# PLOT METHOD
# ======================================================================

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
