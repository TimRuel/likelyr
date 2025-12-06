# =====================================================================
# likelihood-inference.R — Likelihood-based inference for likelyr
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Computes likelihood-based inferential quantities — MLEs, smoothed
#' likelihood curves, relative log-likelihood functions, and LR-based
#' confidence intervals — for one or more likelihood results stored
#' inside a `calibrated_model`.
#'
#' This function **modifies the calibrated model in place**, attaching
#' an inference object to each likelihood component under:
#'
#' \preformatted{
#'   cal$results[[name]]$inference
#' }
#'
#' The updated calibrated model is returned, with class
#' `"likelyr_inferred"` added via [mark_inferred()].
#'
#' @details
#' A calibrated model may contain:
#'
#' * Integrated likelihood results (`"IL"`)
#' * Profile likelihood results (`"PL"`)
#' * Other likelihood-based outputs
#'
#' For each selected result, `infer()` generates:
#'
#' * Spline-smoothed likelihood curve
#' * MLE and maximum log-likelihood
#' * Relative log-likelihood function rLL(ψ)
#' * Likelihood-ratio confidence intervals
#'
#' @param cal A `calibrated_model` returned by [calibrate()] and
#'   subsequently by [integrate()] or [profile()].
#' @param which Optional character vector specifying which entries of
#'   `cal$results` to infer on. Default: all likelihood results.
#' @param alpha Numeric vector of confidence levels (e.g. `0.05`).
#'
#' @return The **same calibrated_model**, but with each selected result
#' updated to contain an `"inference"` object. The returned model gains
#' class `"likelyr_inferred"`.
#'
#' @export
infer <- function(cal, which = NULL, alpha = 0.05) {

  if (!inherits(cal, "calibrated_model"))
    stop("infer() requires a calibrated_model.", call. = FALSE)

  if (!isTRUE(cal$.__calibrated__))
    stop("infer() requires a model that has been calibrated.", call. = FALSE)

  if (is.null(cal$results))
    stop("infer(): No likelihood results present. Run integrate() or profile().",
         call. = FALSE)

  available <- names(cal$results)

  if (is.null(which)) {
    which <- available
  } else {
    missing <- setdiff(which, available)
    if (length(missing) > 0)
      stop("infer(): Unknown result(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
  }

  for (name in which) {
    res <- cal$results[[name]]
    cal$results[[name]]$inference <- infer_one_result(res, alpha)
  }

  cal <- mark_inferred(cal)
  cal
}

# =====================================================================
# Internal: Inference for a Single IL or PL Result
# =====================================================================

#' @title Inference for a Single Likelihood Grid
#' @description
#' Internal helper used by [infer()] to process **one** likelihood grid.
#'
#' @param res A likelihood result object containing either:
#'   * `log_L_bar_df` (integrated likelihood)
#'   * `profile_df`   (profile likelihood)
#' @param alpha Numeric vector of confidence levels.
#'
#' @return A `"likelyr_inference"` object.
#' @keywords internal
infer_one_result <- function(res, alpha) {

  if (!is.null(res$log_L_bar_df)) {
    df   <- res$log_L_bar_df
    mode <- "Integrated"
  } else if (!is.null(res$profile_df)) {
    df   <- res$profile_df
    mode <- "Profile"
  } else {
    stop("infer(): Result object does not contain a likelihood grid.", call. = FALSE)
  }

  if (!all(c("psi", "value") %in% names(df)))
    stop("Likelihood grid must contain columns 'psi' and 'value'.", call. = FALSE)

  df <- dplyr::rename(df, loglik = value)

  spline_model <- fit_ll_spline(df)
  MLE_data     <- compute_MLE(spline_model, df)
  rel_ll_fn    <- make_relative_loglik_fn(spline_model, MLE_data$Maximum)
  conf_ints    <- compute_ci(rel_ll_fn, df, MLE_data, alpha)

  new_inference_result(list(
    psi             = df$psi,
    loglik          = df$loglik,
    spline_model    = spline_model,
    MLE             = MLE_data$MLE,
    max_loglik      = MLE_data$Maximum,
    relative_loglik = rel_ll_fn,
    conf_ints       = conf_ints,
    mode            = mode
  ))
}

# =====================================================================
# Inference Object Class Documentation
# =====================================================================

#' Likelyr Inference Object
#'
#' @name likelyr_inference
#' @rdname likelyr_inference
#'
#' @description
#' Objects of class `"likelyr_inference"` store inference results for a
#' single likelihood curve.
#'
#' @details
#' Each object contains:
#'
#' * `psi` — grid of ψ values
#' * `loglik` — log-likelihood values
#' * `spline_model` — smoothing spline fit
#' * `MLE` — maximum-likelihood estimate
#' * `max_loglik` — maximum log-likelihood
#' * `relative_loglik` — function returning rLL(ψ)
#' * `conf_ints` — LR confidence intervals
#' * `mode` — `"Integrated"` or `"Profile"`
#'
#' @return A `"likelyr_inference"` object.
NULL

# =====================================================================
# S3 Print Method
# =====================================================================

#' @export
print.likelyr_inference <- function(x, ...) {

  cat("<likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n", sep = "")
  cat("  MLE:      ", format(x$MLE), "\n", sep = "")
  cat("  Max LL:   ", format(x$max_loglik), "\n", sep = "")

  if (!is.null(x$conf_ints)) {
    cat("  CIs:\n")
    print(x$conf_ints)
  } else {
    cat("  CIs:      <none>\n")
  }

  invisible(x)
}

# =====================================================================
# S3 Summary Method
# =====================================================================

#' @export
summary.likelyr_inference <- function(object, ...) {

  out <- list(
    mode       = object$mode,
    MLE        = object$MLE,
    max_loglik = object$max_loglik,
    conf_ints  = object$conf_ints
  )

  class(out) <- "summary_likelyr_inference"
  out
}

#' @export
print.summary_likelyr_inference <- function(x, ...) {

  cat("<Summary: likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n", sep = "")
  cat("  MLE:      ", format(x$MLE), "\n", sep = "")
  cat("  Max LL:   ", format(x$max_loglik), "\n", sep = "")

  cat("\n  Confidence Intervals:\n")
  if (!is.null(x$conf_ints)) print(x$conf_ints) else cat("    <none>\n")

  invisible(x)
}

# =====================================================================
# S3 Plot Method
# =====================================================================

#' Plot a Likelyr Inference Object
#'
#' @description
#' Plots the relative log-likelihood curve rLL(ψ), marks the MLE, and
#' optionally displays confidence interval boundaries.
#'
#' @param x A `"likelyr_inference"` object.
#' @param show_ci Logical; display CI bounds if TRUE.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly).
#'
#' @export
plot.likelyr_inference <- function(x, show_ci = TRUE, ...) {

  psi <- x$psi
  rLL <- x$relative_loglik(psi)

  df <- tibble::tibble(psi = psi, rLL = rLL)

  p <- plot_base() +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(x = psi, y = rLL),
      color = "#00A2FF", linewidth = 1.2
    ) +
    ggplot2::geom_point(
      data = tibble::tibble(psi = x$MLE, rLL = 0),
      ggplot2::aes(x = psi, y = rLL),
      color = "#FF5D00", size = 3
    ) +
    ggplot2::labs(
      title = paste0("Relative Log-Likelihood (", x$mode, ")"),
      x = expression(psi),
      y = "Relative log-likelihood"
    )

  if (show_ci && !is.null(x$conf_ints)) {
    for (i in seq_len(nrow(x$conf_ints))) {
      ci <- x$conf_ints[i, ]

      if (!is.na(ci$lower))
        p <- p + ggplot2::geom_vline(
          xintercept = ci$lower,
          linetype   = "dashed",
          color      = "gray80"
        )

      if (!is.na(ci$upper))
        p <- p + ggplot2::geom_vline(
          xintercept = ci$upper,
          linetype   = "dashed",
          color      = "gray80"
        )
    }
  }

  print(p)
  invisible(p)
}
