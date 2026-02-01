# ================================================================================
# likelihood-diagnose.R
# Unified diagnostics for integrated + profile likelihood results (HPC-safe)
# ================================================================================

# ================================================================================
# Public API
# ================================================================================

#' Diagnostics for Likelyr Results
#'
#' @description
#' Attaches diagnostics *data* to each likelihood result (integrated or profile)
#' stored in a calibrated model.
#'
#' This function performs **no plotting**. All diagnostics plots are
#' materialized later via `plot()` (local-only).
#'
#' Diagnostics are attached at:
#' \preformatted{
#'   cal$workspace[[name]]$diagnostics
#' }
#'
#' @param cal A `calibrated` model object with pseudolikelihood results.
#' @param verbose Logical; print diagnostic summaries.
#'
#' @return The same `calibrated` model object, with each likelihood result
#'   marked as diagnosed.
#'
#' @export
diagnose <- function(cal, verbose = TRUE) {
  UseMethod("diagnose")
}

#' @export
diagnose.default <- function(cal, ...) {
  stop("diagnose() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
diagnose.calibrated <- function(cal, verbose = FALSE) {
  which <- validate_diagnose_input(cal)

  for (name in which) {
    res <- cal$workspace[[name]]

    # --------------------------------------------------
    # Run diagnostics engine (compute-only)
    # --------------------------------------------------
    if (is_integrate(res)) {
      diag_raw <- diagnose_integrate(res)
      res$diagnostics <- new_diagnostics_result(
        diag_raw,
        likelihood = "integrate"
      )
    } else if (is_profile(res)) {
      diag_raw <- diagnose_profile(res)
      res$diagnostics <- new_diagnostics_result(
        diag_raw,
        likelihood = "profile"
      )
    } else {
      stop(
        "diagnose(): Unsupported result type for '",
        name,
        "'.",
        call. = FALSE
      )
    }

    # --------------------------------------------------
    # Attach diagnostics data only
    # --------------------------------------------------
    cal$workspace[[name]] <- mark_diagnosed(res)

    if (verbose) {
      cat("\n[diagnose] Diagnostics for result:", name, "\n")
      print(res$diagnostics)
    }
  }

  cal
}

# ================================================================================
# Validation
# ================================================================================

#' Validate inputs prior to running diagnostics
#'
#' @keywords internal
#' @noRd
validate_diagnose_input <- function(cal) {
  if (!is_calibrated(cal)) {
    stop("diagnose() requires a calibrated model.", call. = FALSE)
  }

  if (is.null(cal$workspace) || length(cal$workspace) == 0) {
    stop(
      "diagnose(): No pseudolikelihood results found. ",
      "Run integrate() or profile() first.",
      call. = FALSE
    )
  }

  available <- names(cal$workspace)

  # --------------------------------------------------
  # Drop comparison results silently
  # --------------------------------------------------
  which <- available[
    !vapply(cal$workspace, is_comparison, logical(1))
  ]

  invisible(which)
}

# ================================================================================
# Plot Materialization (local-only)
# ================================================================================

#' Build diagnostics plots (dispatcher)
#'
#' @description
#' Materializes diagnostics plots **on demand** from stored diagnostics data.
#' Dispatches to likelihood-specific plot builders.
#'
#' @param diag A diagnostics result object.
#'
#' @return A named list of ggplot objects.
#'
#' @keywords internal
#' @noRd
build_diagnostics_plots <- function(diag) {
  if (!inherits(diag, "diagnostics")) {
    stop("Expected a diagnostics result object.", call. = FALSE)
  }

  if (!isTRUE(diag$supported)) {
    stop("Diagnostics plots not supported for this likelihood.", call. = FALSE)
  }

  switch(
    diag$likelihood,
    integrate = build_diagnostics_plots_integrate(diag),
    profile = build_diagnostics_plots_profile(diag),
    stop(
      "build_diagnostics_plots(): Unknown diagnostics likelihood '",
      diag$likelihood,
      "'.",
      call. = FALSE
    )
  )
}

# ================================================================================
# S3 Methods
# ================================================================================

# ----------------------------------------------------------------------
# Print
# ----------------------------------------------------------------------

#' @export
print.diagnostics <- function(x, ...) {
  cat("<diagnostics>\n")

  if (!isTRUE(x$supported)) {
    cat("  Diagnostics not supported.\n")
    cat("  Message: ", x$message, "\n", sep = "")
    return(invisible(x))
  }

  cat("  Likelihood:  ", x$likelihood, "\n", sep = "")
  cat("  R (branches): ", x$R, "\n", sep = "")
  cat("  ESS (min):    ", sprintf("%.1f", x$summary$ess_min), "\n", sep = "")
  cat("  ESS (median): ", sprintf("%.1f", x$summary$ess_median), "\n", sep = "")
  cat("  Rel SE max:   ", sprintf("%.3f", x$summary$rel_se_max), "\n", sep = "")
  cat(
    "  Outlier max:  ",
    sprintf("%.3f", x$summary$outlier_max),
    "\n",
    sep = ""
  )

  if (!is.null(x$omega_dispersion)) {
    cat("  Omega-hat manifold:\n")
    cat(
      "    Effective rank: ",
      sprintf("%.2f", x$omega_dispersion$effective_rank),
      "\n",
      sep = ""
    )
    cat(
      "    Collapsed:      ",
      if (x$omega_dispersion$collapsed) "YES" else "no",
      "\n",
      sep = ""
    )
  }

  if (length(x$warnings) > 0) {
    cat("\n  Warnings:\n")
    for (w in x$warnings) {
      cat("   • ", w, "\n", sep = "")
    }
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------

#' @export
summary.diagnostics <- function(diag, ...) {
  out <- list(
    likelihood = diag$likelihood,
    supported = diag$supported,
    summary = diag$summary %||% NULL,
    warnings = diag$warnings
  )

  class(out) <- "summary_diagnostics"
  out
}

#' @export
print.summary_diagnostics <- function(x, ...) {
  cat("<summary of diagnostics>\n\n")

  if (!isTRUE(x$supported)) {
    cat("Diagnostics not supported for this likelihood.\n")
    return(invisible(x))
  }

  cat("Likelihood: ", x$likelihood, "\n\n", sep = "")

  if (!is.null(x$summary)) {
    for (nm in names(x$summary)) {
      cat("• ", nm, ": ", format(x$summary[[nm]]), "\n", sep = "")
    }
  }

  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in x$warnings) {
      cat(" • ", w, "\n", sep = "")
    }
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Plot (local-only materialization)
# ----------------------------------------------------------------------

#' @export
plot.diagnostics <- function(x, ...) {
  .assert_local_plotting()

  plots <- build_diagnostics_plots(x)

  for (p in plots) {
    print(p)
  }

  invisible(x)
}

# ================================================================================
# END likelihood-diagnose.R
# ================================================================================
