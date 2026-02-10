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

    res <- infer_result(
      res,
      cal = cal,
      alpha_levels = alpha_levels,
      psi_0 = psi_0,
      expand_factor = expand_factor
    )

    cal$workspace[[name]] <- mark_inferred(res)
  }

  cal
}

# ---------------------------------------------------------------------
# Result dispatch
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result <- function(res, ...) {
  if (is.null(res$psi_ll_df)) {
    stop(
      "infer(): Result is missing psi_ll_df.",
      call. = FALSE
    )
  }

  type <- attr(res$psi_ll_df, "type")

  if (identical(type, "integrate")) {
    infer_result.integrated(res, ...)
  } else if (identical(type, "profile")) {
    infer_result.profile(res, ...)
  } else {
    infer_result.default(res, ...)
  }
}

# ---------------------------------------------------------------------
# Integrated likelihood inference (uses aggregate_branches)
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.integrated <- function(
  res,
  cal,
  alpha_levels,
  psi_0,
  expand_factor,
  ...
) {
  # NOTE:
  # Integrated likelihood curves are re-aggregated at inference time
  # using the current optimizer specification.
  branch_agg_args <- cal$optimizer$branch_agg_args

  if (is.null(branch_agg_args)) {
    stop(
      "infer(): Missing aggregation arguments in optimizer_spec().",
      call. = FALSE
    )
  }

  branch_agg <- aggregate_branches(
    branches = res$branches,
    min_points = branch_agg_args$min_points,
    q_delta = branch_agg_args$q_delta,
    delta_min = branch_agg_args$delta_min,
    delta_max = branch_agg_args$delta_max,
    min_support = branch_agg_args$min_support
  )

  synthesis <- synthesize_inference(
    psi_ll_df = branch_agg$psi_ll_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0,
    expand_factor = expand_factor
  )

  res$inference <- new_inference_result(synthesis)

  res$R_eff <- branch_agg$R_eff
  res$branch_mat <- branch_agg$branch_mat
  res$psi_ll_df <- branch_agg$psi_ll_df

  res
}

# ---------------------------------------------------------------------
# Profile likelihood inference (no aggregation)
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.profile <- function(
  res,
  cal,
  alpha_levels,
  psi_0,
  expand_factor,
  ...
) {
  synthesis <- synthesize_inference(
    psi_ll_df = res$psi_ll_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0,
    expand_factor = expand_factor
  )

  res$inference <- new_inference_result(synthesis)

  res
}

# ---------------------------------------------------------------------
# Default (unsupported result type)
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.default <- function(res, ...) {
  stop(
    "infer(): Unsupported result type '",
    attr(res$psi_ll_df, "type"),
    "'.",
    call. = FALSE
  )
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
