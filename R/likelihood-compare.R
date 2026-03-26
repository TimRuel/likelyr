# ======================================================================
# likelihood-compare.R — Likelihood comparison for likelyr (HPC-safe)
# ======================================================================

#' Compare Integrated and Profile Likelihood Inference
#'
#' @description
#' Computes **comparison data only** between integrated and profile
#' likelihood inference results. No tables or plots are rendered here.
#'
#' Tables and plots are materialized later via `view()` and `plot()`
#' (local-only), using the stored data frames.
#'
#' @export
compare <- function(cal) {
  validate_compare_input(cal)

  # ------------------------------------------------------------------
  # Compute-only comparison synthesis (HPC-safe)
  # ------------------------------------------------------------------
  comparison <- synthesize_comparison(cal$workspace)
  attr(comparison, "workspace") <- cal$workspace

  cal$workspace$comparison <- new_comparison_result(comparison)
  cal$workspace <- mark_compared(cal$workspace)

  cal
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

#' Validate inputs prior to likelihood comparison
#'
#' @keywords internal
#' @noRd
validate_compare_input <- function(cal) {
  if (!is_calibrated(cal)) {
    stop("compare() requires a model that has been calibrated.", call. = FALSE)
  }

  if (is.null(cal$workspace$profile) || is.null(cal$workspace$integrated)) {
    stop("compare() requires profile() and integrate().", call. = FALSE)
  }

  if (
    is.null(cal$workspace$profile$inference) ||
      is.null(cal$workspace$integrated$inference)
  ) {
    stop("compare() requires infer() on both likelihoods.", call. = FALSE)
  }

  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Print
# ---------------------------------------------------------------------

#' @export
print.comparison <- function(x, ...) {
  cat("<comparison result>\n\n")

  dfs <- Filter(is.data.frame, x)
  if (length(dfs)) {
    cat("Data frames:\n")
    for (nm in names(dfs)) {
      df <- dfs[[nm]]
      cat("•", nm, "(", nrow(df), "x", ncol(df), ")\n")
    }
  }

  cat("\nUse view() to render tables and plot() to visualize curves.\n")

  invisible(x)
}

# ---------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------

#' @export
summary.comparison <- function(object, ...) {
  out <- list(
    data_frames = Filter(is.data.frame, object)
  )

  class(out) <- "summary_comparison"
  out
}

#' @export
print.summary_comparison <- function(x, ...) {
  cat("<summary of comparison>\n\n")

  if (length(x$data_frames)) {
    cat("Data frames:\n")
    for (nm in names(x$data_frames)) {
      df <- x$data_frames[[nm]]
      cat("•", nm, "(", nrow(df), "x", ncol(df), ")\n")
    }
  }

  cat("\nUse view() / plot() locally for presentation.\n")

  invisible(x)
}

# ---------------------------------------------------------------------
# View (local-only)
# ---------------------------------------------------------------------

#' @export
view.comparison <- function(x, ...) {
  required <- c(
    "point_estimates_df",
    "interval_estimates_df"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "comparison object missing required data frame(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    point_estimates = render_point_estimates_comparison_table(
      x$point_estimates_df
    ),

    interval_estimates = render_interval_estimates_comparison_table(
      x$interval_estimates_df
    ),

    combined = render_estimates_comparison_table(
      x$point_estimates_df,
      x$interval_estimates_df
    )
  )
}

# ---------------------------------------------------------------------
# Plot (local-only)
# ---------------------------------------------------------------------

#' @export
plot.comparison <- function(x, ...) {
  .assert_local_plotting()

  ws <- attr(x, "workspace")
  if (is.null(ws)) {
    stop("Comparison object has no workspace reference.", call. = FALSE)
  }

  plot_pseudolikelihood_curves(
    list(
      profile = ws$profile,
      integrate = ws$integrated
    )
  )
}
