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
        pl_df = NULL,
        psi_hat = NA_real_,
        param_mle = cal$parameter$param_mle,
        status = "failed",
        error_msg = conditionMessage(e)
      ))
      cal
    }
  )

  if (!inherits(cal$workspace$profile, "profile")) {
    raw <- cal$workspace$profile
    cal$workspace$profile <- new_profile_result(list(
      pl_df = raw$pl_df,
      psi_hat = raw$psi_hat,
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
  cat(
    "  inferred:   ",
    if (!is.null(x$point_estimate_df)) "✓" else "×",
    "\n",
    sep = ""
  )
  cat(
    "  diagnosed:  ",
    if (!is.null(x$diagnostics)) "✓" else "×",
    "\n",
    sep = ""
  )

  if (!is.null(x$psi_hat)) {
    cat("psi_hat: ", format(x$psi_hat), "\n", sep = "")
  }
  if (!is.null(x$param_mle)) {
    cat(
      "param_mle: (",
      paste(format(x$param_mle), collapse = ", "),
      ")\n",
      sep = ""
    )
  }
  if (!is.null(x$pl_df)) {
    cat("Grid points: ", nrow(x$pl_df), "\n", sep = "")
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

  if (!is.null(x$point_estimate_df)) {
    plot_pseudolikelihood_curve(
      psi_ll_df = x$pl_df,
      zero_max_psi_ll_fn = x$zero_max_psi_ll_fn,
      point_estimate_df = x$point_estimate_df,
      interval_estimate_df = x$interval_estimate_df
    )
  } else {
    if (is.null(x$pl_df)) {
      stop("No pseudolikelihood data available to plot.", call. = FALSE)
    }
    plot_pseudolikelihood_points(x$pl_df)
  }
}
