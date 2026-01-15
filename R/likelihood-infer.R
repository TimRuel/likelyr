# =====================================================================
# likelihood-infer.R — Likelihood-based inference for likelyr
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#' @export
infer <- function(cal, which = NULL, alpha_levels = NULL) {
  which <- validate_infer_input(cal, which)

  if (is.null(alpha_levels)) {
    alpha_levels <- 1 - cal$estimand$confidence_levels
  }

  expand_factor <- cal$estimand$uniroot_expand_factor
  psi_0 <- cal$estimand$psi_0

  for (name in which) {
    res <- cal$workspace[[name]]

    synthesis <- synthesize_inference(
      psi_ll_df = res$psi_ll_df,
      alpha_levels = alpha_levels,
      psi_0 = psi_0,
      expand_factor = expand_factor
    )

    res$inference <- new_inference_result(synthesis)

    cal$workspace[[name]] <- mark_inferred(res)
  }

  cal
}


# ---------------------------------------------------------------------
# Validation (preconditions for infer())
# ---------------------------------------------------------------------

#' Validate inputs prior to likelihood-based inference
#'
#' @description
#' Checks that a calibrated model object is in a valid state for
#' inference and determines which likelihood results should be
#' processed.
#'
#' Specifically, this function:
#' \itemize{
#'   \item Verifies that the model has been calibrated
#'   \item Ensures pseudolikelihood results are present
#'   \item Resolves the target result set (\code{which})
#'   \item Silently drops any comparison results
#'   \item Confirms required fields (\code{psi_ll_df}) exist
#' }
#'
#' Comparison objects are excluded automatically, since inference
#' is only defined for individual likelihood results (profile or
#' integrated).
#'
#' @param cal A calibrated model object.
#' @param which Optional character vector of result names to infer on.
#'   If \code{NULL}, all available results are used.
#'
#' @return A character vector of workspace result names eligible for
#'   inference (comparison results removed). Returned invisibly.
#'
#' @keywords internal
#' @noRd
validate_infer_input <- function(cal, which) {
  if (!is_calibrated(cal)) {
    stop("infer() requires a model that has been calibrated.", call. = FALSE)
  }

  if (is.null(cal$workspace)) {
    stop(
      "infer(): No pseudolikelihood results present. ",
      "Run integrate() or profile() first.",
      call. = FALSE
    )
  }

  available <- names(cal$workspace)

  # --------------------------------------------------
  # Resolve target set
  # --------------------------------------------------

  if (is.null(which)) {
    which <- available
  } else {
    missing <- setdiff(which, available)
    if (length(missing)) {
      stop(
        "infer(): Unknown result(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------
  # Drop comparison results silently
  # --------------------------------------------------

  which <- which[
    !vapply(
      cal$workspace[which],
      is_comparison,
      logical(1)
    )
  ]

  # --------------------------------------------------
  # Validate required fields
  # --------------------------------------------------

  for (nm in which) {
    if (is.null(cal$workspace[[nm]]$psi_ll_df)) {
      stop(
        "infer(): Missing psi_ll_df in result '",
        nm,
        "'.",
        call. = FALSE
      )
    }
  }

  invisible(which)
}

# ---------------------------------------------------------------------
# Print
# ---------------------------------------------------------------------

#' @export
print.inference <- function(x, ...) {
  cat("<inference result>\n")

  if (!is.null(x$psi_ll_df)) {
    cat("Type: ", attr(x$psi_ll_df, "type"), "\n", sep = "")
  }

  if (!is.null(x$estimate_df)) {
    cat("Estimates available: use summary()\n")
  }

  if (!is.null(x$estimate_table)) {
    cat("HTML table available: use view()\n")
  }

  if (!is.null(x$pseudolikelihood_curve)) {
    cat("Plot available: use plot()\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------

#' @export
summary.inference <- function(object, ...) {
  out <- list(
    type = attr(object$psi_ll_df, "type"),
    data_frames = list(
      estimate = object$estimate_df
    ),
    tables = list(
      estimate = object$estimate_table
    ),
    plot = object$pseudolikelihood_curve
  )

  class(out) <- "summary_inference"
  out
}

#' @export
print.summary_inference <- function(x, ...) {
  cat("<summary of inference>\n\n")
  cat("Type: ", x$type, "\n\n", sep = "")

  if (!is.null(x$data_frames$estimate)) {
    cat("Estimates:\n")
    print(x$data_frames$estimate)
  } else {
    cat("Estimates: <none>\n")
  }

  if (!is.null(x$tables$estimate)) {
    cat("\nHTML table available: use view()\n")
  }

  if (!is.null(x$plot)) {
    cat("Plot available: use plot()\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# View
# ---------------------------------------------------------------------

#' @export
view.inference <- function(x, ...) {
  tbl <- x$estimate_table

  if (is.null(tbl)) {
    stop("No HTML estimate table to render.", call. = FALSE)
  }

  print(tbl)
  invisible(x)
}

# ---------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------

#' @export
plot.inference <- function(x, ...) {
  if (is.null(x$pseudolikelihood_curve)) {
    stop(
      "No plot available in pseudolikelihood inference result",
      call. = FALSE
    )
  }

  x$pseudolikelihood_curve
}
