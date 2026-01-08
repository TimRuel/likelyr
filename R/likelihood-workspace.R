# ======================================================================
# likelihood-workspace.R — Workspace S3 Methods
# ======================================================================

# ----------------------------------------------------------------------
# Print method
# ----------------------------------------------------------------------

#' @export
print.workspace <- function(x, ...) {

  cat("<workspace>\n\n")

  # --------------------------------------------------
  # Expected result slots
  # --------------------------------------------------
  has_profile    <- any(vapply(x, is_profile, logical(1)))
  has_integrate  <- any(vapply(x, is_integrate, logical(1)))
  has_comparison <- is_compared(x)

  cat("Available results:\n")
  cat("  profile:     ", if (has_profile)    "✓" else "×", "\n", sep = "")
  cat("  integrate:   ", if (has_integrate)  "✓" else "×", "\n", sep = "")
  cat("  comparison:  ", if (has_comparison) "✓" else "×", "\n", sep = "")

  # --------------------------------------------------
  # List actual contents
  # --------------------------------------------------
  n <- length(x)

  if (n == 0) {
    cat("\n  (no results stored)\n")
    return(invisible(x))
  }

  cat("\nStored objects:\n")

  for (nm in names(x)) {
    res <- x[[nm]]
    cls <- class(res)[1]
    cat("  • ", nm, " <", cls, ">\n", sep = "")
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Summary method
# ----------------------------------------------------------------------

#' @export
summary.workspace <- function(object, ...) {

  results <- lapply(object, function(res) {
    list(
      classes    = class(res),
      profile    = is_profile(res),
      integrate  = is_integrate(res),
      inferred   = is_inferred(res),
      diagnosed  = is_diagnosed(res)
    )
  })

  out <- list(
    slots = list(
      profile    = any(vapply(object, is_profile, logical(1))),
      integrate  = any(vapply(object, is_integrate, logical(1))),
      comparison = is_compared(object)
    ),
    n_results = length(object),
    names     = names(object),
    results   = results
  )

  class(out) <- "summary_workspace"
  out
}

# ----------------------------------------------------------------------
# Print summary method
# ----------------------------------------------------------------------

#' @export
print.summary_workspace <- function(x, ...) {

  cat("<summary of workspace>\n\n")

  cat("Result slots:\n")
  cat("  profile:     ", if (x$slots$profile)    "✓" else "×", "\n", sep = "")
  cat("  integrate:   ", if (x$slots$integrate)  "✓" else "×", "\n", sep = "")
  cat("  comparison:  ", if (x$slots$comparison) "✓" else "×", "\n", sep = "")

  if (x$n_results == 0) {
    cat("\n(no stored results)\n")
    return(invisible(x))
  }

  cat("\nStored results:\n")

  for (nm in x$names) {

    res <- x$results[[nm]]

    type <- if (res$profile) {
      "profile"
    } else if (res$integrate) {
      "integrate"
    } else if (res$comparision) {
      "comparison"
      } else {
      "unknown"
    }

    flags <- c()
    if (res$inferred)  flags <- c(flags, "inferred")
    if (res$diagnosed) flags <- c(flags, "diagnosed")

    flag_str <- if (length(flags)) {
      paste0(" [", paste(flags, collapse = ", "), "]")
    } else {
      ""
    }

    cat("  • ", nm, ": ", type, flag_str, "\n", sep = "")
  }

  invisible(x)
}

# ======================================================================
# END likelihood-workspace.R
# ======================================================================
