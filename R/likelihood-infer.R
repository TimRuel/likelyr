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
#' @param res A likelihood result object containing:
#'   * `df` — Dataframe of pseudolikelihood (psi, log L(psi)) values
#' @param alpha Numeric vector of α levels.
#'
#' @return A `"likelyr_inference"` object.
#' @keywords internal
infer_one_result <- function(res, alpha_levels, psi_0) {

  psi_ll_df <- res$psi_ll_df
  mode <- res$mode

  psi_ll_fn <- fit_psi_ll_fn(psi_ll_df)
  psi_ll_max_point <- get_psi_ll_max_point(psi_ll_fn, psi_ll_df)
  psi_ll_argmax <- psi_ll_max_point$argmax

  # MLE
  se_psi_hat <- get_se_psi_hat(psi_ll_argmax, psi_ll_df)
  psi_mle_df <- data.frame(psi_0 = psi_0, psi_hat = psi_ll_argmax, se_psi_hat = se_psi_hat)
  psi_mle_kable <- render_psi_mle_kable(psi_mle_df, mode, show_caption = TRUE)

  # CI
  psi_ll_max <- psi_ll_max_point$max
  zero_max_psi_ll_fn <- shift_psi_ll_fn(psi_ll_fn, psi_ll_max)

  expand_factor <- res$estimand$uniroot_expand_factor

  psi_conf_ints_df <- get_psi_conf_ints_df(
    zero_max_psi_ll_fn    = zero_max_psi_ll_fn,
    psi_ll_argmax         = psi_ll_argmax,
    psi_ll_df             = psi_ll_df,
    alpha_levels          = alpha_levels,
    uniroot_expand_factor = expand_factor,
    psi_0                 = psi_0
  )

  psi_conf_ints_table <- get_psi_conf_ints_table(psi_conf_ints_df, psi_ll_argmax, psi_0)
  ci_kable <- render_ci_kable(ci_table, mode, show_caption = TRUE)

  inference_table <- render_inference_table(
    mle_kable = mle_kable,
    ci_kable  = ci_kable,
    mode      = mode
  )

  new_inference_result(list(
    spline_model    = spline_model,
    relative_loglik = rel_ll_fn,
    loglik_df       = df,
    psi_0           = psi_0,
    psi_mle_df        = psi_mle_df,
    mle_kable       = mle_kable,
    conf_ints       = conf_ints,
    ci_kable        = ci_kable,
    table           = inference_table,
    mode            = mode
  ))
}

# =====================================================================
# S3 Print Method
# =====================================================================

#' @export
print.likelyr_inference <- function(x, ...) {

  cat("<likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n", sep = "")
  cat("  MLE:      ", format(x$mle_data$MLE), "\n", sep = "")
  cat("  Max LL:   ", format(x$mle_data$Maximum), "\n", sep = "")
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
    MLE          = object$mle_data$MLE,
    max_loglik   = object$mle_data$maximum,
    conf_ints    = object$conf_ints
  )

  class(out) <- "summary_likelyr_inference"
  out
}

#' @export
print.summary_likelyr_inference <- function(x, ...) {

  cat("<Summary: likelyr_inference>\n")
  cat("  Type:     ", x$mode, "\n")
  cat("  MLE:      ", format(x$mle_data$MLE), "\n")
  cat("  Max LL:   ", format(x$mle_data$max_loglik), "\n")

  cat("\n  Confidence Intervals:\n")
  if (!is.null(x$conf_ints)) print(x$conf_ints) else cat("    <none>\n")

  invisible(x)
}

# =====================================================================
# S3 Plot Method
# =====================================================================

#' @export
plot.likelyr_inference <- function(x) {

  p <- plot_pseudolikelihood_curve(x$psi_ll_df)

  print(p)
  invisible(p)
}
