# =====================================================================
# likelihood-inference.R — Likelihood-based inference for likelyr
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Computes likelihood-based inferential quantities — MLEs, smoothed
#' likelihood curves, relative log-likelihood functions, and
#' likelihood-ratio (LR) confidence intervals.
#'
#' This function **modifies** the calibrated model by attaching, for
#' each likelihood result in `cal$results`, a `"likelyr_inference"`
#' object nested at:
#'
#' \preformatted{
#'   cal$results[[name]]$inference
#' }
#'
#' @details
#' If `alpha_levels` is **NULL**, it is automatically derived from the estimand:
#'
#' \preformatted{
#'   alpha_levels = 1 - estimand$confidence_levels
#' }
#'
#' This ensures that inference defaults to the confidence levels the user
#' supplied when defining the estimand specification.
#'
#' @param cal A `calibrated_model` produced by calibrate() and then
#'   integrate() or profile().
#' @param which Optional vector of names in `cal$results` to infer on.
#'   Default = all.
#' @param alpha_levels Optional numeric vector of α-levels (e.g. c(0.05, 0.1)).
#'   If NULL, uses values derived from the estimand spec.
#'
#' @return The same `calibrated_model`, now with class
#'   `"likelyr_inferred"` and inference objects added.
#'
#' @export
infer <- function(cal, which = NULL, alpha_levels = NULL) {

  if (!inherits(cal, "calibrated_model"))
    stop("infer() requires a calibrated_model.", call. = FALSE)

  if (!isTRUE(cal$.__calibrated__))
    stop("infer() requires a model that has been calibrated.", call. = FALSE)

  if (is.null(cal$results))
    stop("infer(): No likelihood results present. Run integrate() or profile().",
         call. = FALSE)

  # If alpha_levels not supplied, derive from estimand spec
  if (is.null(alpha_levels)) {
    alpha_levels <- 1 - cal$estimand$confidence_levels
  }

  available <- names(cal$results)

  if (is.null(which)) {
    which <- available
  } else {
    missing <- setdiff(which, available)
    if (length(missing) > 0)
      stop(
        "infer(): Unknown result(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
  }

  psi_0 <- cal$estimand$psi_0

  # Perform inference for each requested result
  for (name in which) {
    res <- cal$results[[name]]

    # Make the estimand spec available for CI expansion
    res$estimand <- cal$estimand

    cal$results[[name]]$inference <- infer_one_result(res, alpha_levels, psi_0)
  }

  mark_inferred(cal)
}

# =====================================================================
# Internal: Inference for a Single Likelihood Result
# =====================================================================

#' @title Inference for a Single Likelihood Grid
#' @description
#' Internal helper used by infer() to compute inference for one
#' likelihood grid (Integrated or Profiled).
#'
#' @param res A likelihood result object containing one of:
#'   * `log_L_bar_df` — Integrated Likelihood grid
#'   * `profile_df`   — Profile Likelihood grid
#' @param alpha Numeric vector of α levels.
#'
#' @return A `"likelyr_inference"` object.
#' @keywords internal
infer_one_result <- function(res, alpha_levels, psi_0) {

  # Determine grid type
  if (!is.null(res$log_L_bar_df)) {
    df   <- res$log_L_bar_df
    mode <- "Integrated"
  } else if (!is.null(res$profile_df)) {
    df   <- res$profile_df
    mode <- "Profile"
  } else {
    stop("infer_one_result(): Result object does not contain a likelihood grid.",
         call. = FALSE)
  }

  if (!all(c("psi", "value") %in% names(df)))
    stop("Likelihood grid must contain 'psi' and 'value'.", call. = FALSE)

  # Retrieve estimand (for uniroot expansion)
  estimand <- res$estimand %||%
    stop("infer_one_result(): result must contain $estimand.", call. = FALSE)

  expand_factor <- estimand$uniroot_expand_factor %||% 0.05

  # Validate the factor
  if (!is.numeric(expand_factor) || length(expand_factor) != 1 || expand_factor < 0)
    stop("estimand$uniroot_expand_factor must be a non-negative scalar.", call. = FALSE)

  # Prepare loglik column
  df <- dplyr::rename(df, loglik = value)

  # Fit spline, obtain MLE, construct rLL
  spline_model <- fit_ll_spline(df)
  MLE_data     <- compute_MLE(spline_model, df)
  rel_ll_fn    <- make_relative_loglik_fn(spline_model, MLE_data$Maximum)

  # Compute LR-based CIs
  conf_ints <- compute_ci(
    relative_loglik_fn    = rel_ll_fn,
    df                    = df,
    MLE_data              = MLE_data,
    alpha_levels          = alpha_levels,
    uniroot_expand_factor = expand_factor
  )

  new_inference_result(list(
    psi             = df$psi,
    loglik          = df$loglik,
    spline_model    = spline_model,
    psi_0           = psi_0,
    psi_mle         = MLE_data$MLE,
    max_loglik      = MLE_data$Maximum,
    relative_loglik = rel_ll_fn,
    conf_ints       = conf_ints,
    mode            = mode,
    alpha_levels    = alpha_levels
  ))
}

# =====================================================================
# S3 Print Method
# =====================================================================

#' @export
print.likelyr_inference <- function(x, ...) {

  cat("<likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n", sep = "")
  cat("  MLE:      ", format(x$MLE), "\n", sep = "")
  cat("  Max LL:   ", format(x$max_loglik), "\n", sep = "")
  cat("  Alpha:    ", paste(format(x$alpha_levels), collapse = ", "), "\n")

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
    mode         = object$mode,
    MLE          = object$MLE,
    max_loglik   = object$max_loglik,
    alpha_levels = object$alpha_levels,
    conf_ints    = object$conf_ints
  )

  class(out) <- "summary_likelyr_inference"
  out
}

#' @export
print.summary_likelyr_inference <- function(x, ...) {

  cat("<Summary: likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n")
  cat("  MLE:      ", format(x$MLE), "\n")
  cat("  Max LL:   ", format(x$max_loglik), "\n")
  cat("  Alpha:    ", paste(format(x$alpha_levels), collapse = ", "), "\n")

  cat("\n  Confidence Intervals:\n")
  if (!is.null(x$conf_ints)) print(x$conf_ints) else cat("    <none>\n")

  invisible(x)
}

# =====================================================================
# S3 Plot Method
# =====================================================================

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
          color      = "gray70"
        )

      if (!is.na(ci$upper))
        p <- p + ggplot2::geom_vline(
          xintercept = ci$upper,
          linetype   = "dashed",
          color      = "gray70"
        )
    }
  }

  print(p)
  invisible(p)
}
