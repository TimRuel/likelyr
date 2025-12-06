# =====================================================================
# utils-ci.R
# Confidence intervals and inferential summaries (single-curve version)
# =====================================================================

# =====================================================================
# Likelihood–Ratio Confidence Interval for One Curve
# =====================================================================

#' Compute Likelihood-Ratio Confidence Intervals (single curve)
#'
#' @description
#' Given a relative log-likelihood function:
#'     rLL(psi) = loglik(psi) - max_loglik
#'
#' compute LR-based confidence intervals satisfying:
#'     rLL(psi) >= -crit
#' where crit = 0.5 * χ²₁(1 - α).
#'
#' @param relative_loglik_fn A function rLL(psi).
#' @param df A data frame with columns psi and loglik (grid).
#' @param MLE_data Output from compute_MLE().
#' @param alpha_levels Numeric vector of α levels.
#'
#' @return A tibble with (confidence, alpha, lower, upper).
#' @export
compute_ci <- function(relative_loglik_fn,
                       df,
                       MLE_data,
                       alpha_levels) {
  psi_grid <- df$psi
  MLE <- as.numeric(MLE_data$MLE)

  psi_min <- min(psi_grid)
  psi_max <- max(psi_grid)

  purrr::map_dfr(alpha_levels, function(alpha) {

    crit <- stats::qchisq(1 - alpha, df = 1) / 2
    label <- paste0(100 * (1 - alpha), "%")

    # --- Lower bound ---
    lower <- tryCatch(
      stats::uniroot(
        f = function(psi) relative_loglik_fn(psi) + crit,
        interval = c(psi_min, MLE)
      )$root,
      error = function(e) NA_real_
    )

    # --- Upper bound ---
    upper <- tryCatch(
      stats::uniroot(
        f = function(psi) relative_loglik_fn(psi) + crit,
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
# Summaries Across Simulation Replicates
# =====================================================================

#' Summarize Confidence Interval Performance Across Replicates
#'
#' @param ci_list A list of CI tables (each from get_confidence_intervals_single()).
#' @param true_value The true ψ used for coverage evaluation.
#'
#' @return A tibble summarizing width, coverage, and validity.
#' @export
summarize_confidence_intervals <- function(ci_list, true_value) {

  combined <- dplyr::bind_rows(ci_list, .id = "replicate_id")

  combined |>
    dplyr::mutate(
      valid       = !is.na(lower) & !is.na(upper),
      covers_true = lower <= true_value & upper >= true_value,
      width       = upper - lower
    ) |>
    dplyr::group_by(confidence) |>
    dplyr::summarise(
      mean_width    = mean(width, na.rm = TRUE),
      median_width  = stats::median(width, na.rm = TRUE),
      coverage_rate = mean(covers_true, na.rm = TRUE),
      valid_rate    = mean(valid),
      .groups = "drop"
    )
}


# =====================================================================
# Summaries for MLE Across Simulation Replicates
# =====================================================================

#' Summarize MLE Performance Across Simulations
#'
#' @param mle_list A list where each element contains a tibble with an MLE column.
#' @param true_value The true ψ used for bias/SD/RMSE.
#'
#' @return A tibble summarizing bias, SD, and RMSE.
#' @export
summarize_mle_performance <- function(mle_list, true_value) {

  combined <- dplyr::bind_rows(mle_list, .id = "replicate_id")

  combined |>
    dplyr::summarise(
      bias = mean(MLE - true_value, na.rm = TRUE),
      sd   = stats::sd(MLE, na.rm = TRUE),
      rmse = sqrt(mean((MLE - true_value)^2, na.rm = TRUE))
    )
}

# =====================================================================
# END
# =====================================================================
