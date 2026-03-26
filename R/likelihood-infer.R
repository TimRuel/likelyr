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
#' @param cal          A \code{calibrated} model object.
#' @param which        Character vector. Subset of
#'   \code{c("profile", "integrate")}. Default: all available.
#' @param alpha_levels Numeric vector of significance levels. Default:
#'   derived from \code{traversal$confidence_levels}.
#'
#' @return The SAME \code{calibrated} model object with inference
#'   results attached.
#'
#' @export
infer <- function(cal, which = NULL, alpha_levels = NULL) {
  which <- validate_infer_input(cal, which)

  if (is.null(alpha_levels)) {
    alpha_levels <- 1 - cal$traversal$confidence_levels
  }

  psi_0 <- cal$estimand$psi_0

  for (name in which) {
    res <- .get_infer_result(cal, name)
    res <- infer_result(res, alpha_levels = alpha_levels, psi_0 = psi_0)
    cal <- .set_infer_result(cal, name, mark_inferred(res))
  }

  cal
}

# ---------------------------------------------------------------------
# Workspace accessors
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.get_infer_result <- function(cal, name) {
  cal$workspace[[name]]
}

#' @keywords internal
#' @noRd
.set_infer_result <- function(cal, name, res) {
  cal$workspace[[name]] <- res
  cal
}

# ---------------------------------------------------------------------
# Result dispatch
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result <- function(res, ...) {
  if (inherits(res, "integrate")) {
    infer_result.integrate(res, ...)
  } else if (inherits(res, "profile")) {
    infer_result.profile(res, ...)
  } else {
    stop(
      "infer(): unsupported result type '",
      paste(class(res), collapse = "/"),
      "'.",
      call. = FALSE
    )
  }
}

# ---------------------------------------------------------------------
# Integrated likelihood inference
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.integrate <- function(
  integrated_result,
  alpha_levels,
  psi_0,
  ...
) {
  psi_loglik_df <- integrated_result$psi_loglik_df

  if (is.null(psi_loglik_df)) {
    stop("infer(): integrated_result is missing psi_loglik_df", call. = FALSE)
  }

  if (!is.null(psi_loglik_df$above_crit)) {
    psi_loglik_df <- psi_loglik_df |>
      dplyr::filter(above_crit)
  }

  synthesis <- synthesize_inference(
    psi_loglik_df = psi_loglik_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0
  )

  res$point_estimate_df <- synthesis$point_estimate_df
  res$interval_estimate_df <- synthesis$interval_estimate_df
  res
}

# ---------------------------------------------------------------------
# Profile likelihood inference
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.profile <- function(profile_result, alpha_levels, psi_0, ...) {
  if (is.null(profile_result$psi_loglik_df)) {
    stop("infer(): profile result is missing psi_loglik_df", call. = FALSE)
  }

  synthesis <- synthesize_inference(
    psi_loglik_df = profile_result$psi_loglik_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0
  )

  res$point_estimate_df <- synthesis$point_estimate_df
  res$interval_estimate_df <- synthesis$interval_estimate_df
  res$zero_max_psi_ll_fn <- synthesis$zero_max_psi_ll_fn
  res
}

# ---------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_infer_input <- function(cal, which) {
  if (!is_calibrated(cal)) {
    stop("infer() requires a calibrated model.", call. = FALSE)
  }

  if (is.null(cal$workspace)) {
    stop(
      "infer(): no pseudolikelihood results present.\n",
      "Run integrate() or profile() first.",
      call. = FALSE
    )
  }

  available <- character(0)
  if (!is.null(cal$workspace$profile$psi_loglik_df)) {
    available <- c(available, "profile")
  }
  if (!is.null(cal$workspace$integrated$psi_loglik_df)) {
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
    which <- match.arg(which, c("profile", "integrate"), several.ok = TRUE)
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
summary.inference <- function(object, ...) {
  out <- list(
    pseudolikelihood = paste(class(object), collapse = "/"),
    data_frames = list(
      estimate = object$point_estimate_df,
      interval = object$interval_estimate_df
    )
  )

  class(out) <- "summary_inference"
  out
}

#' @export
print.summary_inference <- function(x, ...) {
  cat("<summary of inference>\n\n")
  cat("Pseudolikelihood: ", x$pseudolikelihood, "\n\n", sep = "")

  if (!is.null(x$data_frames$estimate)) {
    cat("Point + interval estimates:\n")
    print(x$data_frames$estimate)
  } else {
    cat("Estimates: <none>\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# View (local-only)
# ---------------------------------------------------------------------

#' @export
view.inference <- function(x, ...) {
  if (is.null(x$point_estimate_df)) {
    stop(
      "No inference data available to render.\n",
      "Run infer() first.",
      call. = FALSE
    )
  }

  list(
    point_estimate = render_point_estimate_table(x$point_estimate_df),
    interval_estimate = render_interval_estimate_table(x$interval_estimate_df),
    combined = render_estimate_table(
      x$point_estimate_df,
      x$interval_estimate_df
    )
  )
}

# ---------------------------------------------------------------------
# Plot (local-only)
# ---------------------------------------------------------------------

#' @export
plot.inference <- function(inference_result, ...) {
  .assert_local_plotting()
  plot_pseudolikelihood_curve(inference_result)
}
