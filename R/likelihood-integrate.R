# ======================================================================
# likelihood-integrate.R — Integrated Likelihood
# ======================================================================

#' Integrated Log-Likelihood
#'
#' @description
#' Computes the integrated log-likelihood using pre-screened branch seeds
#' stored in \code{model$workspace$integrated$cache} by \code{preprocess()}.
#'
#' After branch computation, \code{aggregate()} is called with default
#' parameters. Call \code{aggregate()} directly on the returned model
#' to re-aggregate without repeating branch computation.
#'
#' @param model     A calibrated \code{model} object.
#' @param verbose Logical. Default: \code{FALSE}.
#' @param ...     Additional arguments passed to \code{generate()}.
#'
#' @return The SAME calibrated \code{model} object, with
#'   \code{$workspace$integrated} populated.
#'
#' @export
integrate <- function(model, ...) {
  UseMethod("integrate")
}

#' @export
integrate.default <- function(model, ...) {
  stop("integrate() requires a calibrated 'model' object.", call. = FALSE)
}

#' @export
integrate.model <- function(model, verbose = FALSE, ...) {
  if (!is_calibrated(model)) {
    stop("integrate() requires calibrate() first.", call. = FALSE)
  }

  validate_integrate_input(model)

  branch_seeds <- model$workspace$integrated$cache$branch_seeds %||% NULL

  if (is.null(branch_seeds) || length(branch_seeds) == 0L) {
    stop(
      "integrate() requires pre-sieved branch seeds.\n",
      "Run preprocess() before integrate().",
      call. = FALSE
    )
  }

  # Carry sieve runtime forward before generate() overwrites the cache slot
  sieve_elapsed <- model$workspace$integrated$cache$runtime$sieve_elapsed %||%
    NA_real_

  exec <- model$execution

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
  t0_generate <- proc.time()[["elapsed"]]

  model <- tryCatch(
    {
      model <- generate(model, task = "integrate", verbose = verbose, ...)
      model <- aggregate(model, verbose = verbose)
      model
    },
    error = function(e) {
      if (verbose) {
        cat("[integrate] ERROR during integration.\n")
      }
      model$workspace$integrated <- list(
        psi_loglik_df = NULL,
        psi_hat = NA_real_,
        cache = model$workspace$integrated$cache,
        status = "failed",
        error_msg = conditionMessage(e)
      )
      model
    }
  )

  generate_elapsed <- proc.time()[["elapsed"]] - t0_generate

  # ------------------------------------------------------------------
  # Wrap and mark
  # ------------------------------------------------------------------
  raw <- model$workspace$integrated

  runtime <- list(
    sieve_elapsed = sieve_elapsed,
    generate_elapsed = generate_elapsed,
    total_elapsed = sieve_elapsed + generate_elapsed
  )

  model$workspace$integrated <- new_integrated_result(
    c(
      raw,
      list(
        status = raw$status %||% "success",
        runtime = runtime
      )
    )
  )

  model <- mark_integrated(model)

  if (verbose) {
    cat(
      "[integrate] Finished.",
      " Sieve: ",
      round(sieve_elapsed, 2),
      "s",
      " | Generate: ",
      round(generate_elapsed, 2),
      "s",
      " | Total: ",
      round(runtime$total_elapsed, 2),
      "s\n",
      sep = ""
    )
  }

  model
}

# ======================================================================
# VALIDATION
# ======================================================================

#' @keywords internal
#' @noRd
validate_integrate_input <- function(model) {
  missing <- character(0)

  if (!inherits(model$parameter, "parameter_spec")) {
    missing <- c(missing, "parameter_spec()")
  }
  if (!inherits(model$likelihood, "likelihood_spec")) {
    missing <- c(missing, "likelihood_spec()")
  }
  if (!inherits(model$estimand, "estimand_spec")) {
    missing <- c(missing, "estimand_spec()")
  }
  if (!inherits(model$sampler, "sampler_spec")) {
    missing <- c(missing, "sampler_spec()")
  }
  if (!inherits(model$traversal, "traversal_spec")) {
    missing <- c(missing, "traversal_spec()")
  }
  if (!inherits(model$solver, "solver_spec")) {
    missing <- c(missing, "solver_spec()")
  }
  if (!inherits(model$execution, "execution_spec")) {
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

  if (is.null(model$likelihood$E_loglik)) {
    stop(
      "[integrate] E_loglik is required for integrated likelihood.\n",
      "Supply it via likelihood_spec(E_loglik = ...).",
      call. = FALSE
    )
  }

  invisible(model)
}

# ======================================================================
# PRINT METHODS
# ======================================================================

#' @export
print.integrated_cache <- function(x, ...) {
  cache <- x$cache
  ci <- cache$common_interval

  cat("<Integrated Likelihood Cache>\n\n")

  cat("Seeds:\n")
  cat("  accepted:  ", cache$total_seeds_accepted, "\n", sep = "")
  cat("  requested: ", cache$total_seeds_requested, "\n", sep = "")

  if (!is.null(ci)) {
    cat("\nCommon interval:\n")
    cat(
      "  [",
      round(ci$psi_lower, 4),
      ", ",
      round(ci$psi_upper, 4),
      "]\n",
      sep = ""
    )
    cat("  mode_mean: ", round(ci$mode_mean, 4), "\n", sep = "")
    cat("  mode_sd:   ", round(ci$mode_sd, 4), "\n", sep = "")
    cat("  z:         ", round(ci$z, 3), "\n", sep = "")
  }

  if (!is.null(cache$runtime)) {
    cat("\nRuntime:\n")
    cat("  sieve: ", round(cache$runtime$sieve_elapsed, 2), "s\n", sep = "")
  }

  invisible(x)
}

#' @method print integrated
#' @export
print.integrated <- function(x, ...) {
  if (!is_result(x)) {
    return(NextMethod())
  }

  cat("<Integrated Log-Likelihood Result>\n")
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
  if (!is.null(x$R)) {
    cat("Branches: ", x$R, "\n", sep = "")
  }
  if (!is.null(x$runtime)) {
    cat("Runtime:\n")
    cat("  sieve:    ", round(x$runtime$sieve_elapsed, 2), "s\n", sep = "")
    cat("  generate: ", round(x$runtime$generate_elapsed, 2), "s\n", sep = "")
    cat("  total:    ", round(x$runtime$total_elapsed, 2), "s\n", sep = "")
  }

  invisible(x)
}

# ======================================================================
# PLOT METHOD
# ======================================================================

#' @method plot integrated
#' @export
plot.integrated <- function(x, ...) {
  .assert_local_plotting()

  if (!is_result(x)) {
    stop(
      "plot.integrated() requires an integrated result object.",
      call. = FALSE
    )
  }

  if (is.null(x$psi_loglik_df)) {
    stop("No pseudolikelihood data available to plot.", call. = FALSE)
  }

  plot_pseudolikelihood_points(x)
}

# ======================================================================
# END likelihood-integrate.R
# ======================================================================
