# ================================================================================
# likelihood-diagnose.R
# Unified diagnostics generic, model dispatcher, and shared S3 methods
# ================================================================================

# ================================================================================
# Generic
# ================================================================================

#' Diagnostics for Likelyr Results
#'
#' @description
#' Attaches diagnostics data to each pseudolikelihood result (integrated or
#' profile) stored in a calibrated model. Dispatches to result-type-specific
#' methods via S3.
#'
#' This function performs **no plotting**. All diagnostics plots are
#' materialized later via `plot()` (local-only).
#'
#' Diagnostics are attached at:
#' \preformatted{
#'   model$workspace$<pseudolikelihood>$diagnostics
#' }
#'
#' @param model A calibrated `model` object with pseudolikelihood results,
#'   or a pseudolikelihood result object.
#' @param verbose Logical; print diagnostic summaries.
#'
#' @return The input object with diagnostics attached and marked as diagnosed.
#'
#' @export
diagnose <- function(model, verbose = FALSE) {
  UseMethod("diagnose")
}

#' @export
diagnose.default <- function(model, ...) {
  stop("diagnose() requires a calibrated 'model' object.", call. = FALSE)
}

# ================================================================================
# Model dispatcher
# ================================================================================

#' @export
diagnose.model <- function(model, verbose = FALSE) {
  validate_diagnose_input(model)

  for (slot in c("profile", "integrated")) {
    res <- model$workspace[[slot]]
    if (is.null(res)) {
      next
    }

    model$workspace[[slot]] <- diagnose(res, verbose = verbose)
  }

  model
}

# ================================================================================
# Result-type methods
# ================================================================================

#' @export
diagnose.integrated <- function(x, verbose = FALSE, ...) {
  if (!is_result(x)) {
    return(NextMethod())
  }
  if (!is_integrated_result(x)) {
    stop(
      "diagnose.integrated() requires an integrated result object.",
      call. = FALSE
    )
  }

  diag_raw <- diagnose_integrated(x)
  x$diagnostics <- new_diagnostic_result(diag_raw)
  x <- mark_diagnosed(x)

  if (verbose) {
    print(x$diagnostics)
  }

  x
}

#' @export
diagnose.profile <- function(x, verbose = FALSE, ...) {
  if (!is_result(x)) {
    return(NextMethod())
  }
  if (!is_profile_result(x)) {
    stop("diagnose.profile() requires a profile result object.", call. = FALSE)
  }

  diag_raw <- diagnose_profile(x)
  x$diagnostics <- new_diagnostic_result(diag_raw)
  x <- mark_diagnosed(x)

  if (verbose) {
    print(x$diagnostics)
  }

  x
}

# ================================================================================
# Plot materialization dispatcher (local-only)
# ================================================================================

#' Build diagnostics plots (dispatcher)
#'
#' @description
#' Materializes diagnostics plots on demand from stored diagnostics data.
#' Dispatches to pseudolikelihood-specific plot builders.
#'
#' @param object A `diagnostic` result object.
#'
#' @return A named list of ggplot objects.
#'
#' @keywords internal
#' @noRd
build_diagnostic_plots <- function(object) {
  if (!is_diagnostic_result(object)) {
    stop("Expected a 'diagnostic' result object.", call. = FALSE)
  }

  if (!isTRUE(object$supported)) {
    stop(
      "Diagnostics plots not supported for this pseudolikelihood.",
      call. = FALSE
    )
  }

  switch(
    object$pseudolikelihood,
    integrated = build_diagnostics_plots_integrated(object),
    profile = build_diagnostics_plots_profile(object),
    stop(
      "build_diagnostic_plots(): Unknown pseudolikelihood '",
      object$pseudolikelihood,
      "'.",
      call. = FALSE
    )
  )
}

# ================================================================================
# Validation
# ================================================================================

#' @keywords internal
#' @noRd
validate_diagnose_input <- function(model) {
  if (!is_calibrated(model)) {
    stop("diagnose() requires a calibrated 'model' object.", call. = FALSE)
  }

  has_any <- has_profile_result(model$workspace) ||
    has_integrated_result(model$workspace)

  if (!has_any) {
    stop(
      "diagnose(): No pseudolikelihood results found. ",
      "Run profile() or integrate() first.",
      call. = FALSE
    )
  }

  invisible(model)
}

# ================================================================================
# S3 Methods for diagnostic results
# ================================================================================

# ----------------------------------------------------------------------
# Print
# ----------------------------------------------------------------------

#' @export
print.diagnostic <- function(x, ...) {
  cat("<diagnostics>\n")

  if (!isTRUE(x$supported)) {
    cat("  Diagnostics not supported.\n")
    cat("  Message: ", x$message, "\n", sep = "")
    return(invisible(x))
  }

  cat("  R (branches):      ", x$R, "\n", sep = "")
  cat(
    "  ESS (min):         ",
    sprintf("%.1f", x$summary$ess_min),
    "\n",
    sep = ""
  )
  cat(
    "  ESS (median):      ",
    sprintf("%.1f", x$summary$ess_median),
    "\n",
    sep = ""
  )
  cat(
    "  Rel SE max:        ",
    sprintf("%.3f", x$summary$rel_se_max),
    "\n",
    sep = ""
  )
  cat(
    "  Outlier max:       ",
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
      cat("   \u2022 ", w, "\n", sep = "")
    }
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------

#' @export
summary.diagnostic <- function(object, ...) {
  out <- list(
    pseudolikelihood = object$pseudolikelihood,
    supported = object$supported,
    summary = object$summary %||% NULL,
    warnings = object$warnings
  )

  class(out) <- "summary_diagnostic"
  out
}

#' @export
print.summary_diagnostic <- function(x, ...) {
  cat("<Summary of diagnostics>\n\n")

  if (!isTRUE(x$supported)) {
    cat("Diagnostics not supported for this pseudolikelihood.\n")
    return(invisible(x))
  }

  cat("Pseudolikelihood: ", x$pseudolikelihood, "\n\n", sep = "")

  if (!is.null(x$summary)) {
    for (nm in names(x$summary)) {
      cat("\u2022 ", nm, ": ", format(x$summary[[nm]]), "\n", sep = "")
    }
  }

  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in x$warnings) {
      cat(" \u2022 ", w, "\n", sep = "")
    }
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Plot (local-only)
# ----------------------------------------------------------------------

#' @export
plot.diagnostic <- function(x, ...) {
  .assert_local_plotting()

  plots <- build_diagnostic_plots(x)

  for (p in plots) {
    print(p)
  }

  invisible(x)
}

# ================================================================================
# END likelihood-diagnose.R
# ================================================================================
