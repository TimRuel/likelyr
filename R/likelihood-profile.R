# ======================================================================
# likelihood-profile.R — Profile Log-Likelihood API
# ======================================================================

#' Profile Log-Likelihood
#'
#' @description
#' Computes the profile log-likelihood curve for the calibrated model.
#' Nuisance parameters are fixed at the MLE, forming a single
#' deterministic branch evaluated along the ψ-grid. The result is
#' stored in \code{model$workspace$profile}.
#'
#' @param model     A calibrated \code{model} object.
#' @param verbose Logical. Print diagnostics. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to
#'   \code{generate(task = "profile")}.
#'
#' @return The SAME calibrated \code{model} object, augmented with
#'   class \code{profiled} and \code{model$workspace$profile}.
#'
#' @export
profile <- function(model, ...) {
  UseMethod("profile")
}

#' @export
profile.default <- function(model, ...) {
  stop("profile() requires a calibrated 'model' object.", call. = FALSE)
}

#' @export
profile.model <- function(model, verbose = FALSE, ...) {
  if (!is_calibrated(model)) {
    stop("profile() requires calibrate() first.", call. = FALSE)
  }

  validate_profile_input(model)

  if (verbose) {
    cat("[profile] Profile Log-Likelihood\n")
  }

  t0 <- proc.time()[["elapsed"]]

  model <- tryCatch(
    generate(model, task = "profile", verbose = verbose, ...),
    error = function(e) {
      if (verbose) {
        cat("[profile] ERROR during generation.\n")
      }
      model$workspace$profile <- new_profile_result(
        list(
          status = "failed",
          error_msg = conditionMessage(e)
        )
      )
      model
    }
  )

  elapsed <- proc.time()[["elapsed"]] - t0

  if (!inherits(model$workspace$profile, "profile")) {
    raw <- model$workspace$profile
    model$workspace$profile <- new_profile_result(
      list(
        psi_loglik_df = raw$psi_loglik_df,
        psi_hat = raw$psi_hat,
        status = "success"
      )
    )
  }

  model$workspace$profile$runtime <- list(elapsed = elapsed)

  model <- mark_profiled(model)

  if (verbose) {
    cat("[profile] Finished in ", round(elapsed, 2), "s.\n", sep = "")
  }

  model
}

# ======================================================================
# VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
validate_profile_input <- function(model) {
  if (!inherits(model$parameter, "parameter_spec")) {
    stop("model$parameter must be a 'parameter_spec'.")
  }
  if (!inherits(model$likelihood, "likelihood_spec")) {
    stop("model$likelihood must be a 'likelihood_spec'.")
  }
  if (!inherits(model$estimand, "estimand_spec")) {
    stop("model$estimand must be an 'estimand_spec'.")
  }
  if (!inherits(model$traversal, "traversal_spec")) {
    stop("model$traversal must be a 'traversal_spec'.")
  }
  invisible(model)
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
    "  diagnosed:  ",
    if (is_diagnosed(x)) "\u2713" else "\u00d7",
    "\n",
    sep = ""
  )
  cat(
    "  inferred:   ",
    if (is_inferred(x)) "\u2713" else "\u00d7",
    "\n",
    sep = ""
  )

  if (!is.null(x$psi_hat)) {
    cat("\u03c8\u0302: ", format(x$psi_hat), "\n", sep = "")
  }
  if (!is.null(x$psi_loglik_df)) {
    cat("Grid points: ", nrow(x$psi_loglik_df), "\n", sep = "")
  }
  if (!is.null(x$runtime)) {
    cat("Runtime:     ", round(x$runtime$elapsed, 2), "s\n", sep = "")
  }

  invisible(x)
}

# ======================================================================
# PLOT METHOD
# ======================================================================

#' @method plot profile
#' @export
plot.profile <- function(x, points = FALSE, ...) {
  .assert_local_plotting()

  if (is.null(x$psi_loglik_df)) {
    stop("No pseudolikelihood data available to plot.", call. = FALSE)
  }

  if (is_inferred(x) && !points) {
    plot_pseudolikelihood_curve(x$inference, psi_loglik_df = x$psi_loglik_df)
  } else {
    plot_pseudolikelihood_points(x)
  }
}

# ======================================================================
# END likelihood-profile.R
# ======================================================================
