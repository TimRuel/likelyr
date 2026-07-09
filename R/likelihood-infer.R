# =====================================================================
# likelihood-infer.R — Likelihood-based inference
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Computes point estimates and confidence intervals from the profile
#' and/or integrated log-likelihood curves stored on the model workspace.
#' Inference results are folded directly onto each result object —
#' \code{point_estimate_df}, \code{interval_estimate_df}, and
#' \code{zero_max_psi_ll_fn} become top-level slots alongside
#' \code{pl_df}/\code{il_df}.
#'
#' The filtered \code{psi_loglik_df} (after applying the \code{above_crit}
#' mask) is stored on the inference result so that plotting uses the same
#' data as inference. The unfiltered \code{psi_loglik_df} remains on the
#' workspace result for use by \code{diagnose()}.
#'
#' @param model A calibrated \code{model} object.
#' @param which Character vector. Subset of
#'   \code{c("profile", "integrated")}. Default: all available.
#' @param alpha_levels Numeric vector of significance levels. Default:
#'   derived from \code{traversal$confidence_levels}.
#'
#' @return The SAME calibrated \code{model} object with inference
#'   results attached.
#'
#' @export
infer <- function(model, which = NULL, alpha_levels = NULL) {
  which <- validate_infer_input(model, which)

  if (is.null(alpha_levels)) {
    alpha_levels <- 1 - model$traversal$confidence_levels
  }

  psi_0 <- model$estimand$psi_0
  psi_interval <- model$estimand$psi_interval %||% NULL

  for (pseudolikelihood in which) {
    res <- .get_workspace_result(model, pseudolikelihood)
    infer_result <- .get_infer_result(
      res,
      alpha_levels = alpha_levels,
      psi_0 = psi_0,
      psi_interval = psi_interval
    )
    model <- .set_infer_result(model, pseudolikelihood, infer_result)
  }

  model
}

# ---------------------------------------------------------------------
# Workspace accessors
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.get_workspace_result <- function(model, name) {
  model$workspace[[name]]
}

#' @keywords internal
#' @noRd
.set_infer_result <- function(model, name, res) {
  # res is list(result = <synthesize_inference output>,
  #             psi_loglik_df = <filtered df>)
  inference <- new_inference_result(res$result)
  inference$psi_loglik_df <- res$psi_loglik_df
  model$workspace[[name]]$inference <- inference
  model$workspace[[name]] <- mark_inferred(model$workspace[[name]])
  model
}

# ---------------------------------------------------------------------
# Inference computation
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.get_infer_result <- function(
  workspace_result,
  alpha_levels,
  psi_0,
  psi_interval = NULL,
  ...
) {
  if (
    !(is_profile_result(workspace_result) ||
      is_integrated_result(workspace_result))
  ) {
    stop(
      "infer(): unsupported result type '",
      paste(class(workspace_result), collapse = "/"),
      "'.",
      call. = FALSE
    )
  }

  psi_loglik_df <- workspace_result$psi_loglik_df

  if (is.null(psi_loglik_df)) {
    stop("infer(): workspace_result is missing psi_loglik_df", call. = FALSE)
  }

  # Apply above_crit filter when present. The filtered df is used for
  # all inference computations and stored on the inference result so that
  # plotting uses the same data. The unfiltered df remains on the
  # workspace result for diagnose().
  psi_loglik_df_filtered <- if ("above_crit" %in% names(psi_loglik_df)) {
    psi_loglik_df |> dplyr::filter(above_crit)
  } else {
    psi_loglik_df
  }

  result <- synthesize_inference(
    psi_loglik_df = psi_loglik_df_filtered,
    alpha_levels = alpha_levels,
    psi_0 = psi_0,
    psi_interval = psi_interval
  )

  list(
    result = result,
    psi_loglik_df = psi_loglik_df_filtered
  )
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_infer_input <- function(model, which) {
  if (!is_calibrated(model)) {
    stop("infer() requires a calibrated model.", call. = FALSE)
  }

  if (is.null(model$workspace)) {
    stop(
      "infer(): no pseudolikelihood results present.\n",
      "Run integrate() or profile() first.",
      call. = FALSE
    )
  }

  available <- character(0)
  if (!is.null(model$workspace$profile$psi_loglik_df)) {
    available <- c(available, "profile")
  }
  if (!is.null(model$workspace$integrated$psi_loglik_df)) {
    available <- c(available, "integrated")
  }

  if (length(available) == 0L) {
    stop(
      "infer(): no psi_loglik_df found in workspace.\n",
      "Run integrate() or profile() first.",
      call. = FALSE
    )
  }

  if (is.null(which)) {
    which <- available
  } else {
    which <- match.arg(which, c("profile", "integrated"), several.ok = TRUE)
    missing <- setdiff(which, available)
    if (length(missing)) {
      stop(
        "infer(): requested result(s) not available: ",
        paste(missing, collapse = ", "),
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
  cat("Pseudolikelihood: ", paste(class(x), collapse = "/"), "\n", sep = "")

  if (!is.null(x$point_estimate_df)) {
    cat("Inference: available\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------

#' @export
summary.inference <- function(x, ...) {
  out <- list(
    pseudolikelihood = attr(x$point_estimate_df, "pseudolikelihood"),
    data_frames = list(
      estimate = x$point_estimate_df,
      interval = x$interval_estimate_df
    )
  )

  class(out) <- "summary_inference"
  out
}

#' @export
print.summary_inference <- function(x, ...) {
  cat(
    "<Summary of ",
    tools::toTitleCase(x$pseudolikelihood),
    " Log-Likelihood Inference>\n\n",
    sep = ""
  )

  if (!is.null(x$data_frames$estimate)) {
    cat("Point estimate:\n")
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
view.inference <- function(
  x,
  which = c("combined", "point", "interval"),
  ...
) {
  which <- match.arg(which)

  if (is.null(x$point_estimate_df)) {
    stop(
      "No inference data available to render.\n",
      "Run infer() first.",
      call. = FALSE
    )
  }

  switch(
    which,
    point = render_point_estimate_table(x$point_estimate_df),
    interval = render_interval_estimate_table(x$interval_estimate_df),
    combined = render_estimate_table(
      x$point_estimate_df,
      x$interval_estimate_df
    )
  )
}

# ---------------------------------------------------------------------
# Plot (local-only) — inference
# ---------------------------------------------------------------------

#' @method plot inference
#' @export
plot.inference <- function(x, points = FALSE, display_truth = TRUE, ...) {
  .assert_local_plotting()

  if (is.null(x$psi_loglik_df)) {
    stop(
      "plot.inference() requires a psi_loglik_df on the inference result.\n",
      "Run infer() first.",
      call. = FALSE
    )
  }

  plot_pseudolikelihood_curve(
    inference_result = x,
    psi_loglik_df = x$psi_loglik_df,
    points = points,
    display_truth = display_truth
  )
}

# =====================================================================
# END likelihood-infer.R
# =====================================================================
