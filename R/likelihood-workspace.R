# ======================================================================
# likelihood-workspace.R — Workspace S3 Methods
# ======================================================================

# ----------------------------------------------------------------------
# Print method
# ----------------------------------------------------------------------

#' @export
print.workspace <- function(x, ...) {
  cat("<workspace>\n\n")

  has_profile <- "profile" %in% names(x)
  has_integrated <- "integrated" %in% names(x)
  has_comparison <- "comparison" %in% names(x)

  cat("Results:\n")
  cat(
    "  profile:     ",
    if (has_profile) "✓ present" else "× absent",
    "\n",
    sep = ""
  )
  cat(
    "  integrated:   ",
    if (has_integrated) "✓ present" else "× absent",
    "\n",
    sep = ""
  )
  cat(
    "  comparison:  ",
    if (has_comparison) "✓ present" else "× absent",
    "\n",
    sep = ""
  )

  if (length(x) == 0) {
    cat("\n  (no results stored)\n")
    return(invisible(x))
  }

  invisible(x)
}

# ======================================================================
# END likelihood-workspace.R
# ======================================================================
