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
compare <- function(model) {
  validate_compare_input(model)

  # ------------------------------------------------------------------
  # Compute-only comparison synthesis (HPC-safe)
  # ------------------------------------------------------------------
  res_list <- model$workspace[c("profile", "integrated")]
  comparison <- synthesize_comparison(res_list)
  attr(comparison, "workspace") <- model$workspace

  model$workspace$comparison <- new_comparison_result(comparison)
  model <- mark_compared(model)

  model
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_compare_input <- function(model) {
  if (!is_calibrated(model)) {
    stop("compare() requires a model that has been calibrated.", call. = FALSE)
  }

  if (is.null(model$workspace$profile) || is.null(model$workspace$integrated)) {
    stop("compare() requires profile() and integrate().", call. = FALSE)
  }

  if (
    is.null(model$workspace$profile$inference) ||
      is.null(model$workspace$integrated$inference)
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
      cat("\u2022", nm, "(", nrow(df), "x", ncol(df), ")\n")
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
    data_frames = list(
      estimate = object$point_estimates_df,
      interval = object$interval_estimates_df
    )
  )

  class(out) <- "summary_comparison"
  out
}

#' @export
print.summary_comparison <- function(x, ...) {
  cat("<Summary of Likelihood Comparison>\n\n")

  if (!is.null(x$data_frames$estimate)) {
    cat("Point estimates:\n")
    print(x$data_frames$estimate)
  }

  if (!is.null(x$data_frames$interval)) {
    cat("\nInterval estimates:\n")
    print(x$data_frames$interval)
  }

  if (is.null(x$data_frames$estimate) && is.null(x$data_frames$interval)) {
    cat("Estimates: <none>\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# View (local-only)
# ---------------------------------------------------------------------

#' @export
view.comparison <- function(
  x,
  which = c("combined", "point", "interval"),
  ...
) {
  which <- match.arg(which)

  required <- c("point_estimates_df", "interval_estimates_df")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "comparison object missing required data frame(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  switch(
    which,
    point = render_point_estimates_comparison_table(x$point_estimates_df),
    interval = render_interval_estimates_comparison_table(
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
      integrated = ws$integrated
    )
  )
}

# ======================================================================
# END likelihood-compare.R
# ======================================================================
