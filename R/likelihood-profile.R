# ======================================================================
# likelihood-profile.R — Profile Log-Likelihood API
# ======================================================================

#' Profile Log-Likelihood
#'
#' @description
#' Computes the profile log-likelihood curve for the calibrated model.
#' Nuisance parameters are fixed at the MLE, forming a single
#' deterministic branch evaluated along the ψ-grid. The result is
#' stored in \code{cal$workspace$profile}.
#'
#' @param cal     A \code{calibrated} model object.
#' @param verbose Logical. Print diagnostics. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to
#'   \code{generate(task = "profile")}.
#'
#' @return The SAME \code{calibrated} model object, augmented with
#'   class \code{profiled} and \code{cal$workspace$profile}.
#'
#' @export
profile <- function(cal, ...) {
  UseMethod("profile")
}

#' @export
profile.default <- function(cal, ...) {
  stop("profile() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
profile.calibrated <- function(cal, verbose = FALSE, ...) {
  if (!is_calibrated(cal)) {
    stop("profile() requires calibrate() first.", call. = FALSE)
  }

  validate_profile_input(cal)

  if (verbose) {
    cat("[profile] Profile Log-Likelihood\n")
  }

  cal <- tryCatch(
    generate(cal, task = "profile", verbose = verbose, ...),
    error = function(e) {
      if (verbose) {
        cat("[profile] ERROR during generation.\n")
      }
      cal$workspace$profile <- new_profile_result(list(
        psi_ll_df = NULL,
        psi_mle = cal$estimand$psi_mle,
        param_mle = cal$parameter$param_mle,
        status = "failed",
        error_msg = conditionMessage(e)
      ))
      cal
    }
  )

  # Wrap raw list from .generate_profile() into classed result
  if (!inherits(cal$workspace$profile, "profile")) {
    raw <- cal$workspace$profile
    cal$workspace$profile <- new_profile_result(list(
      psi_ll_df = raw$psi_ll_df,
      psi_mle = raw$psi_mle,
      param_mle = raw$param_mle,
      status = "success"
    ))
  }

  cal <- mark_profiled(cal)

  if (verbose) {
    cat("[profile] Finished.\n")
  }

  cal
}

# ======================================================================
# VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
validate_profile_input <- function(cal) {
  if (!inherits(cal$parameter, "parameter_spec")) {
    stop("model$parameter must be a 'parameter_spec'.")
  }
  if (!inherits(cal$likelihood, "likelihood_spec")) {
    stop("model$likelihood must be a 'likelihood_spec'.")
  }
  if (!inherits(cal$estimand, "estimand_spec")) {
    stop("model$estimand must be an 'estimand_spec'.")
  }
  if (!inherits(cal$traversal, "traversal_spec")) {
    stop("model$traversal must be a 'traversal_spec'.")
  }
  invisible(cal)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @method print profile
#' @export
print.profile <- function(x, ...) {
  cat("<Profile Log-Likelihood Result>\n")
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

  invisible(x)
}

# ======================================================================
# PLOT METHOD
# ======================================================================

#' @method plot profile
#' @export
plot.profile <- function(x, ...) {
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
