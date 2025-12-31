# =====================================================================
# likelihood-infer.R — Likelihood-based inference for likelyr
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Attaches likelihood-based inferential summaries—point estimates and
#' likelihood-ratio confidence intervals—to previously computed likelihood
#' results.
#'
#' This function does **not** recompute likelihoods. It operates on likelihood
#' objects (e.g. from `integrate()` or `profile()`) already stored in a
#' calibrated model.
#'
#' For each selected likelihood result in `cal$results`, a
#' `"likelyr_inference"` object is attached at:
#'
#' \preformatted{
#'   cal$results[[name]]$inference
#' }
#'
#' @details
#' If `alpha_levels` is **NULL**, values are derived from the estimand
#' specification:
#'
#' \preformatted{
#'   alpha_levels = 1 - estimand$confidence_levels
#' }
#'
#' ensuring consistency between estimation and inference defaults.
#'
#' @param cal A `calibrated_model` produced by `calibrate()` and then
#'   `integrate()` or `profile()`.
#' @param which Optional character vector naming entries in `cal$results`
#'   to perform inference on. Defaults to all available likelihood results.
#' @param alpha_levels Optional numeric vector of α-levels
#'   (e.g. `c(0.05, 0.1)`). If `NULL`, derived from the estimand specification.
#'
#' @return
#' The same `calibrated_model`, augmented with inference results and
#' assigned the additional class `"likelyr_inferred"`.
#'
#' @export
infer <- function(cal, which = NULL, alpha_levels = NULL) {

  if (!inherits(cal, "calibrated_model"))
    stop("infer() requires a calibrated_model.", call. = FALSE)

  if (!isTRUE(cal$.__calibrated__))
    stop("infer() requires a model that has been calibrated.", call. = FALSE)

  if (is.null(cal$results))
    stop(
      "infer(): No likelihood results present. ",
      "Run integrate() or profile() first.",
      call. = FALSE
    )

  # --------------------------------------------------
  # Alpha levels
  # --------------------------------------------------
  if (is.null(alpha_levels)) {
    alpha_levels <- 1 - cal$estimand$confidence_levels
  }

  # --------------------------------------------------
  # Interval-expansion control (from estimand spec)
  # --------------------------------------------------
  expand_factor <- cal$estimand$uniroot_expand_factor

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

  # --------------------------------------------------
  # Perform inference for each requested result
  # --------------------------------------------------
  for (name in which) {

    res <- cal$results[[name]]

    if (is.null(res$psi_ll_df)) {
      stop(
        "infer(): Missing psi_ll_df in result '", name, "'.",
        call. = FALSE
      )
    }

    inference_synthesis <- synthesize_inference(
      psi_ll_df     = res$psi_ll_df,
      alpha_levels  = alpha_levels,
      psi_0         = psi_0,
      expand_factor = expand_factor,
      render        = TRUE
    )

    cal$results[[name]]$inference <- new_inference_result(inference_synthesis)
  }

  mark_inferred(cal)
}

# =====================================================================
# S3 Print Method
# =====================================================================

#' @export
print.likelyr_inference <- function(x, ...) {

  cat("<likelyr_inference>\n")

  if (!is.null(x$psi_ll_df)) {
    cat("  Type:     ", attr(x$psi_ll_df, "type"), "\n")
  }

  if (!is.null(x$point_estimate_df)) {

    pe <- x$point_estimate_df

    cat(
      "  MLE:      ",
      format(pe$psi_hat),
      "\n",
      sep = ""
    )

    if ("se_psi_hat" %in% names(pe)) {
      cat(
        "  SE(MLE):  ",
        format(pe$se_psi_hat),
        "\n",
        sep = ""
      )
    }
  }

  if (!is.null(x$interval_estimate_df) &&
      "Level" %in% names(x$interval_estimate_df)) {

    alpha_levels <- 1 - scales::parse_percent(x$interval_estimate_df$Level)

    cat(
      "  Alpha:    ",
      paste(format(alpha_levels), collapse = ", "),
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$interval_estimate_df)) {
    cat("  CIs:\n")
    print(x$interval_estimate_df)
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

  pe <- object$point_estimate_df
  ci <- object$interval_estimate_df

  out <- list(
    type       = attr(object$psi_ll_df, "type"),
    psi_hat    = if (!is.null(pe)) pe$psi_hat else NA_real_,
    se_psi_hat = if (!is.null(pe) && "se_psi_hat" %in% names(pe)) pe$se_psi_hat else NA_real_,
    conf_ints  = ci
  )

  class(out) <- "summary_likelyr_inference"
  out
}

#' @export
print.summary_likelyr_inference <- function(x, ...) {

  cat("<Summary: likelyr_inference>\n")

  if (!is.null(x$psi_ll_df)) {
    cat("  Type:     ", attr(x$psi_ll_df, "type"), "\n")
  }

  cat("  MLE:      ", format(x$psi_hat), "\n")

  if (!is.na(x$se_psi_hat)) {
    cat("  SE(MLE):  ", format(x$se_psi_hat), "\n")
  }

  cat("\n  Confidence Intervals:\n")
  if (!is.null(x$conf_ints)) {
    print(x$conf_ints)
  } else {
    cat("    <none>\n")
  }

  invisible(x)
}

# =====================================================================
# S3 Plot Method
# =====================================================================

#' @export
plot.likelyr_inference <- function(x, ...) {

  p <- plot_pseudolikelihood_curve(
    psi_ll_df             = x$psi_ll_df,
    zero_max_psi_ll_fn    = x$zero_max_psi_ll_fn,
    point_estimate_df     = x$point_estimate_df,
    interval_estimate_df = x$interval_estimate_df
  )

  print(p)
  invisible(p)
}
