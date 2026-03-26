# ======================================================================
# likelihood-integrate.R — Integrated Likelihood
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood using pre-screened branch seeds
#' stored in \code{cal$workspace$integrated$cache} by \code{preprocess()}.
#'
#' After branch computation, \code{aggregate()} is called with default
#' parameters. Call \code{aggregate()} directly on the returned model
#' to re-aggregate without repeating branch computation.
#'
#' @param cal     A \code{calibrated} model object.
#' @param verbose Logical. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to \code{generate()}.
#'
#' @return The SAME \code{calibrated} model object, with
#'   \code{$workspace$integrated} populated.
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
  if (!is_calibrated(cal)) {
    stop(
      "integrate() requires a model calibrated via calibrate().",
      call. = FALSE
    )
  }

  validate_integrate_input(cal)

  branch_seeds <- cal$workspace$integrated$cache$branch_seeds %||% NULL

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "integrate() requires pre-sieved branch seeds.\n",
      "Run preprocess() before integrate().",
      call. = FALSE
    )
  }

  exec <- cal$execution

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
  # Generate + aggregate with failure handling
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
      cal$workspace$integrated <- list(
        psi_loglik_df = NULL,
        psi_hat = NA_real_,
        status = "failed",
        error_msg = conditionMessage(e)
      )
      cal
    }
  )

  # ------------------------------------------------------------------
  # Wrap and mark
  # ------------------------------------------------------------------
  raw <- cal$workspace$integrated

  cal$workspace$integrated <- new_integrated_result(
    c(raw, list(status = raw$status %||% "success"))
  )

  cal <- mark_integrated(cal)

  if (verbose) {
    cat("[integrate] Finished.\n")
  }

  cal
}

# ======================================================================
# VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
validate_integrated_input <- function(cal) {
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

#' @method print integrated
#' @export
print.integrated <- function(integrated_result, ...) {
  cat("<Integrated Log-Likelihood Result>\n")
  cat("Status: ", integrated_result$status, "\n", sep = "")

  cat("Lifecycle:\n")
  cat(
    "  inferred:   ",
    if (is_inferred(integrated_result)) "✓" else "×",
    "\n",
    sep = ""
  )
  cat(
    "  diagnosed:  ",
    if (is_diagnosed(integrated_result)) "✓" else "×",
    "\n",
    sep = ""
  )

  if (!is.null(integrated_result$psi_hat)) {
    cat("psi_hat: ", format(integrated_result$psi_hat), "\n", sep = "")
  }
  if (!is.null(integrated_result$psi_loglik_df)) {
    cat("Grid points: ", nrow(integrated_result$psi_loglik_df), "\n", sep = "")
  }
  if (!is.null(integrated_result$R)) {
    cat("Branches: ", integrated_result$R, "\n", sep = "")
  }

  invisible(integrated_result)
}

# ======================================================================
# PLOT METHOD
# ======================================================================

#' @method plot integrated
#' @export
plot.integrated <- function(integrated_result, ...) {
  .assert_local_plotting()

  if (is.null(integrated_result$psi_loglik_df)) {
    stop("No pseudolikelihood data available to plot.", call. = FALSE)
  }

  plot_pseudolikelihood_points(integrated_result)
}
