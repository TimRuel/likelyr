# =====================================================================
# likelihood-infer.R — Likelihood-based inference
# =====================================================================

#' Likelihood-Based Inference for a Calibrated Model
#'
#' @description
#' Computes point estimates and confidence intervals from the profile
#' and/or integrated log-likelihood curves stored on the model workspace.
#'
#' Pass \code{which} to restrict inference to a single pseudolikelihood
#' type. By default both are inferred if present.
#'
#' @param cal          A \code{calibrated} model object with at least
#'   one of \code{profile()} or \code{integrate()} having been run.
#' @param which        Character vector. Subset of
#'   \code{c("profile", "integrate")}. Default: all available.
#' @param alpha_levels Numeric vector of significance levels. Default:
#'   derived from \code{traversal$confidence_levels}.
#'
#' @return The SAME \code{calibrated} model object with inference
#'   results attached to each relevant workspace entry.
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

    res <- infer_result(
      res,
      alpha_levels = alpha_levels,
      psi_0 = psi_0
    )

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
  if (name == "integrate") {
    cal$workspace$integrate$result
  } else {
    cal$workspace[[name]]
  }
}

#' @keywords internal
#' @noRd
.set_infer_result <- function(cal, name, res) {
  if (name == "integrate") {
    cal$workspace$integrate$result <- res
  } else {
    cal$workspace[[name]] <- res
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
    stop("infer(): result is missing psi_ll_df.", call. = FALSE)
  }

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
  res,
  alpha_levels,
  psi_0,
  ...
) {
  psi_ll_df <- res$psi_ll_df

  if (!is.null(psi_ll_df$above_crit)) {
    psi_ll_df <- psi_ll_df |>
      dplyr::filter(above_crit)
  }

  synthesis <- synthesize_inference(
    psi_ll_df = psi_ll_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0
  )

  res$inference <- new_inference_result(synthesis)
  res
}

# ---------------------------------------------------------------------
# Profile likelihood inference
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
infer_result.profile <- function(
  res,
  alpha_levels,
  psi_0,
  ...
) {
  synthesis <- synthesize_inference(
    psi_ll_df = res$psi_ll_df,
    alpha_levels = alpha_levels,
    psi_0 = psi_0
  )

  res$inference <- new_inference_result(synthesis)
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
  if (!is.null(cal$workspace$profile$psi_ll_df)) {
    available <- c(available, "profile")
  }
  if (!is.null(cal$workspace$integrate$result$psi_ll_df)) {
    available <- c(available, "integrate")
  }

  if (length(available) == 0L) {
    stop(
      "infer(): no psi_ll_df found in workspace.\n",
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

  if (!is.null(x$psi_ll_df)) {
    cat("Type: ", paste(class(x), collapse = "/"), "\n", sep = "")
  }

  if (!is.null(x$inference)) {
    cat("Inference: available\n")
  }

  invisible(x)
}

# ---------------------------------------------------------------------
# Plot (local-only)
# ---------------------------------------------------------------------

#' @export
plot.inference <- function(x, ...) {
  .assert_local_plotting()

  if (is.null(x$psi_ll_df)) {
    stop("No pseudolikelihood data available to plot.", call. = FALSE)
  }

  plot_pseudolikelihood_curve(
    psi_ll_df = x$psi_ll_df,
    zero_max_psi_ll_fn = x$zero_max_psi_ll_fn,
    point_estimate_df = x$point_estimate_df,
    interval_estimate_df = x$interval_estimate_df
  )
}

# ---------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------

#' @export
summary.inference <- function(object, ...) {
  out <- list(
    type = paste(class(object), collapse = "/"),
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
  cat("Type: ", x$type, "\n\n", sep = "")

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
  if (is.null(x$inference$point_estimate_df)) {
    stop(
      "No inference data available to render.\n",
      "Run infer() first.",
      call. = FALSE
    )
  }

  list(
    point_estimate = render_point_estimate_table(x$inference$point_estimate_df),
    interval_estimate = render_interval_estimate_table(
      x$inference$interval_estimate_df
    ),
    combined = render_estimate_table(
      x$inference$point_estimate_df,
      x$inference$interval_estimate_df
    )
  )
}
