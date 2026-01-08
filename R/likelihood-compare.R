# ======================================================================
# likelihood-compare.R — Likelihood comparison for likelyr
# ======================================================================

#' Compare Integrated and Profile Likelihood Inference
#' @export
compare <- function(cal) {

  validate_compare_input(cal)

  cal$workspace$comparison <- NULL

  comparison <- list(
    tables = synthesize_comparison(cal$workspace),
    pseudolikelihood_curves = plot_pseudolikelihood_curves(cal$workspace)
  )

  cal$workspace$comparison <- new_comparison_result(comparison)

  cal$workspace <- mark_compared(cal$workspace)

  cal
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

validate_compare_input <- function(cal) {

  if (!is_calibrated(cal))
    stop("compare() requires a model that has been calibrated.", call. = FALSE)

  if (is.null(cal$workspace$profile) || is.null(cal$workspace$integrate))
    stop("compare() requires profile() and integrate().", call. = FALSE)

  if (is.null(cal$workspace$profile$inference) ||
      is.null(cal$workspace$integrate$inference))
    stop("compare() requires infer() on both likelihoods.", call. = FALSE)

  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Print
# ---------------------------------------------------------------------

#' @export
print.comparison <- function(x, ...) {

  cat("<comparison result>\n\n")

  dfs <- Filter(is.data.frame, x$tables)
  if (length(dfs)) {
    cat("Data frames:\n")
    for (nm in names(dfs)) {
      df <- dfs[[nm]]
      cat("•", nm, "(", nrow(df), "x", ncol(df), ")\n")
    }
  }

  html <- Filter(Negate(is.data.frame), x$tables)
  if (length(html)) {
    cat("\nHTML tables available: use view()\n")
  }

  if (!is.null(x$plot)) {
    cat("Plot available: use plot()\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------

#' @export
summary.comparison <- function(object, ...) {

  out <- list(
    data_frames = Filter(is.data.frame, object$tables),
    tables      = Filter(Negate(is.data.frame), object$tables),
    plot        = object$pseudolikelihood_curves
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

  if (length(x$tables)) {
    cat("\nHTML tables:\n")
    for (nm in names(x$tables)) {
      cat("•", nm, "\n")
    }
  }

  if (!is.null(x$plot)) {
    cat("\nPlot available: use plot()\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# View
# ---------------------------------------------------------------------

#' @export
view.comparison <- function(x, ...) {

  tables <- Filter(Negate(is.data.frame), x$tables)

  if (!length(tables)) {
    stop("No HTML tables to render.", call. = FALSE)
  }

  for (tbl in tables) {
    print(tbl)
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------

#' @export
plot.comparison <- function(x, ...) {

  if (is.null(x$pseudolikelihood_curves)) {
    stop("No plot available in pseudolikelihood comparison result", call. = FALSE)
  }

  x$pseudolikelihood_curves
}
