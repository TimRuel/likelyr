# =====================================================================
# utils-ll-processing.R
# Single-curve log-likelihood processing utilities for likelyr
# =====================================================================

# NOTE:
# All functions below operate on *one* likelihood curve at a time.
# They assume a data frame of the form:
#       psi   loglik


# =====================================================================
# Fit Spline to Log-Likelihood Curve
# =====================================================================

#' Fit a spline model to a single log-likelihood curve
#'
#' @param df A data frame with columns psi and loglik.
#'
#' @return A smooth.spline object.
#' @export
fit_ll_spline <- function(df) {

  if (!all(c("psi", "loglik") %in% names(df)))
    stop("Spline model requires columns 'psi' and 'loglik'.", call. = FALSE)

  stats::smooth.spline(df$psi, df$loglik)
}


# =====================================================================
# Compute MLE
# =====================================================================

#' Compute the MLE and maximum log-likelihood from a spline model
#'
#' @param spline_model A smooth.spline object from fit_ll_spline().
#' @param df A data frame with psi and loglik.
#'
#' @return A tibble with MLE and Maximum.
#' @export
compute_MLE <- function(spline_model, df) {

  opt <- stats::optimize(
    f       = \(psi) stats::predict(spline_model, psi)$y,
    lower   = min(df$psi),
    upper   = max(df$psi),
    maximum = TRUE
  )

  tibble::tibble(
    MLE     = opt$maximum,
    Maximum = opt$objective
  )
}

# =====================================================================
# Likelihood–Ratio Confidence Interval for One Curve
# =====================================================================

#' Compute Likelihood-Ratio Confidence Intervals (single curve)
#'
#' @description
#' Given a relative log-likelihood function:
#'     rLL(psi) = loglik(psi) - max_loglik,
#' compute LR-based confidence intervals satisfying:
#'     rLL(psi) >= -crit,
#' where crit = 0.5 * χ²₁(1 - α).
#'
#' @param relative_loglik_fn A function rLL(psi).
#' @param df A data frame with columns psi and loglik (grid).
#' @param MLE_data Output from compute_MLE().
#' @param alpha_levels Numeric vector of α levels.
#' @param uniroot_expand_factor Multiplicative factor used to expand
#'        the uniroot search interval outward (default 0.05 = 5%).
#'
#' @return A tibble with (confidence, alpha, lower, upper).
#' @export
compute_ci <- function(relative_loglik_fn,
                       df,
                       MLE_data,
                       alpha_levels,
                       uniroot_expand_factor) {

  psi_grid <- df$psi
  MLE      <- as.numeric(MLE_data$MLE)

  psi_min0 <- min(psi_grid)
  psi_max0 <- max(psi_grid)

  purrr::map_dfr(alpha_levels, function(alpha) {

    crit  <- 0.5 * stats::qchisq(1 - alpha, df = 1)
    label <- paste0(100 * (1 - alpha), "%")

    # ----------------------------------------------------------
    # Expand intervals multiplicatively to improve robustness
    # ----------------------------------------------------------
    left_width   <- MLE - psi_min0
    right_width  <- psi_max0 - MLE

    psi_min <- psi_min0 - left_width  * uniroot_expand_factor
    psi_max <- psi_max0 + right_width * uniroot_expand_factor

    # --- Lower bound ---
    lower <- tryCatch(
      stats::uniroot(
        f        = function(psi) relative_loglik_fn(psi) + crit,
        interval = c(psi_min, MLE)
      )$root,
      error = function(e) NA_real_
    )

    # --- Upper bound ---
    upper <- tryCatch(
      stats::uniroot(
        f        = function(psi) relative_loglik_fn(psi) + crit,
        interval = c(MLE, psi_max)
      )$root,
      error = function(e) NA_real_
    )

    tibble::tibble(
      confidence = label,
      alpha      = alpha,
      lower      = round(lower, 6),
      upper      = round(upper, 6)
    )
  })
}


# =====================================================================
# Normalized Relative Log-Likelihood Function
# =====================================================================

#' Construct a relative log-likelihood function
#'
#' @description
#' Produces a function:
#'   rLL(psi) = loglik(psi) - max_loglik
#'
#' This centers the log-likelihood at zero, preserving curvature and
#' enabling LR-based inference.
#'
#' @param spline_model A smooth.spline fit from fit_ll_spline().
#' @param max_loglik The maximum log-likelihood value from compute_MLE().
#'
#' @return A function rLL(psi).
#' @export
make_relative_loglik_fn <- function(spline_model, max_loglik) {

  \(psi) {
    stats::predict(spline_model, psi)$y - max_loglik
  }
}


# =====================================================================
# Evaluate a Relative Log-Likelihood Function on a Grid
# =====================================================================

#' Evaluate a spline-based log-likelihood function on a psi grid
#'
#' @param fn A function of psi (e.g., from make_relative_loglik_fn()).
#' @param psi_grid Numeric vector of psi values.
#'
#' @return A numeric vector of evaluated relative log-likelihood values.
#' @export
evaluate_ll_fn <- function(fn, psi_grid) {
  fn(psi_grid)
}

# =====================================================================
# END
# =====================================================================
