# =====================================================================
# likelihood-infer.R — Likelihood-based inference for likelyr (HPC-safe)
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Computes likelihood-based inference **data only** (point estimates and
#' confidence interval diagnostics) for each pseudolikelihood result.
#'
#' This function performs **no table rendering and no plotting**.
#' Presentation is deferred to `view()` and `plot()` (local-only).
#'
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

    # Attach inference data only
    res$inference <- new_inference_result(synthesis)
    res$psi_ll_df <- NULL
    cal$workspace[[name]] <- mark_inferred(res)
  }

  cal
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

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

  # Drop comparison results
  which <- which[
    !vapply(cal$workspace[which], is_comparison, logical(1))
  ]

  # Require psi_ll_df
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

  cat("Use view() to render tables and plot() to visualize curves.\n")

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
      estimate = object$estimate_df,
      interval = object$interval_estimate_df
    )
  )

  class(out) <- "summary_inference"
  out
}

#' @export
print.summary_inference <- function(x, ...) {
  cat("<summary of inference>\n\n")
  cat("Type: ", x$type, "\n\n", sep = "")

  if (!is.null(x$data_frames$estimate)) {
    cat("Point + interval estimates:\n")
    print(x$data_frames$estimate)
  } else {
    cat("Estimates: <none>\n")
  }

  cat("\nUse view() / plot() locally for presentation.\n")

  invisible(x)
}

# ---------------------------------------------------------------------
# View (local-only)
# ---------------------------------------------------------------------

#' @export
view.inference <- function(x, ...) {
  if (is.null(x$estimate_df)) {
    stop("No inference data available to render.", call. = FALSE)
  }

  list(
    point = render_point_estimate_table(x$point_estimate_df),
    interval = render_interval_estimate_table(x$interval_estimate_df),
    combined = render_estimate_table(x$estimate_df)
  )
}

# ---------------------------------------------------------------------
# Plot (local-only)
# ---------------------------------------------------------------------

#' @export
plot.inference <- function(x, ...) {
  .assert_local_plotting()

  if (is.null(x$psi_ll_df)) {
    stop(
      "No pseudolikelihood data available to plot.",
      call. = FALSE
    )
  }

  plot_pseudolikelihood_curve(
    psi_ll_df = x$psi_ll_df,
    zero_max_psi_ll_fn = x$zero_max_psi_ll_fn,
    point_estimate_df = x$point_estimate_df,
    interval_estimate_df = x$interval_estimate_df
  )
}
