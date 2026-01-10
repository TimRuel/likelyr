# ================================================================================
# likelihood-diagnose.R
# Unified diagnostics for integrated + profile likelihood results
# ================================================================================

# ================================================================================
# Public API
# ================================================================================

#' Diagnostics for Likelyr Results
#'
#' @description
#' Attaches diagnostics to each likelihood result (integrated or profile)
#' stored in a calibrated model.
#'
#' Integrated likelihood receives full Monte Carlo diagnostics; profile
#' likelihood receives its own diagnostics implementation.
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
  validate_diagnose_input(cal)

  for (name in names(cal$workspace)) {
    res <- cal$workspace[[name]]

    # ------------------------------------------------------------------
    # Run diagnostics engine (type-dispatched)
    # ------------------------------------------------------------------

    if (is_integrate(res)) {
      diag_raw <- diagnose_integrate(res)
    } else if (is_profile(res)) {
      diag_raw <- diagnose_profile(res)
    } else {
      stop(
        "diagnose(): Unsupported result type for '",
        name,
        "'.",
        call. = FALSE
      )
    }

    # ------------------------------------------------------------------
    # Attach and mark result
    # ------------------------------------------------------------------

    res$diagnostics <- new_diagnostics_result(diag_raw)
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

  invisible(TRUE)
}

# ================================================================================
# S3 Methods
# ================================================================================
# NOTE:
#   • Diagnostics computation lives in:
#       - likelihood-diagnose-integrate.R
#       - likelihood-diagnose-profile.R
#   • Plot construction is handled *inside* those files.
#   • This file contains only dispatch, orchestration, and presentation.
# ================================================================================

# ----------------------------------------------------------------------
# Print
# ----------------------------------------------------------------------

#' @export
print.diagnostics <- function(x, ...) {
  cat("<diagnostics>\n")

  if (!isTRUE(x$supported)) {
    cat("  Type: Profile Log-Likelihood (placeholder)\n")
    cat("  Message: ", x$message, "\n", sep = "")
    return(invisible(x))
  }

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
# Plot
# ----------------------------------------------------------------------

#' @export
plot.diagnostics <- function(x, ...) {
  x$plots %||% list()
}

# ================================================================================
# END likelihood-diagnose.R
# ================================================================================
