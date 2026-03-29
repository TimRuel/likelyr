# ======================================================================
#  workspace.R — Workspace S3 Methods
# ======================================================================

# ----------------------------------------------------------------------
# Print method
# ----------------------------------------------------------------------

#' @export
print.workspace <- function(x, ...) {
  cat("<workspace>\n\n")

  # The integrated slot has three states:
  #   NULL                → not yet preprocessed
  #   non-null, no result → preprocessing cache present, not yet integrated
  #   integrated_result   → integration complete
  integrated_status <- if (has_integrated_result(x)) {
    "✓ present"
  } else if (!is.null(x$integrated)) {
    "~ preprocessed"
  } else {
    "× absent"
  }

  cat("Results:\n")
  cat(
    "  profile:    ",
    if (has_profile_result(x)) "✓ present" else "× absent",
    "\n",
    sep = ""
  )
  cat("  integrated: ", integrated_status, "\n", sep = "")
  cat(
    "  comparison: ",
    if (has_comparison_result(x)) "✓ present" else "× absent",
    "\n",
    sep = ""
  )

  invisible(x)
}

# ======================================================================
# END workspace.R
# ======================================================================
