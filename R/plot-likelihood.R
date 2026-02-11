# =====================================================================
# plot-likelihood.R
# Pseudolikelihood Visualization for Result and Inference Objects
# (local-only materialization)
# =====================================================================

# NOTE:
#   All functions in this file are **local-only**.
#   They must never be called on HPC.
#   Enforcement is via .assert_local_plotting() at entry.

# ---------------------------------------------------------------------
# Point cloud visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood evaluation points
#'
#' @keywords internal
#' @noRd
plot_pseudolikelihood_curve <- function(
  psi_ll_df,
  zero_max_psi_ll_fn,
  point_estimate_df,
  interval_estimate_df
) {
  .assert_local_plotting()

  type <- attr(psi_ll_df, "type")
  pseudolikelihood <- tolower(type)

  # --------------------------------------------------
  # Extract MLE
  # --------------------------------------------------
  psi_mle <- point_estimate_df$psi_hat

  # --------------------------------------------------
  # Confidence intervals (long format)
  # --------------------------------------------------
  ci_long <- extract_ci_long(interval_estimate_df)

  ci_raw <- ci_long |>
    dplyr::filter(!is.na(endpoint))

  # --------------------------------------------------
  # PSI limits — symmetric about MLE
  # --------------------------------------------------
  if (nrow(ci_raw) > 0) {
    alpha_widest <- min(ci_raw$alpha)

    ci_widest <- ci_raw |>
      dplyr::filter(alpha == alpha_widest)

    lower_val <- ci_widest |>
      dplyr::filter(position == "lower") |>
      dplyr::pull(endpoint)

    upper_val <- ci_widest |>
      dplyr::filter(position == "upper") |>
      dplyr::pull(endpoint)

    dist_left <- if (length(lower_val) == 1) psi_mle - lower_val else NA_real_
    dist_right <- if (length(upper_val) == 1) upper_val - psi_mle else NA_real_

    # Mirror missing side
    if (is.na(dist_left) && !is.na(dist_right)) {
      dist_left <- dist_right
    }
    if (is.na(dist_right) && !is.na(dist_left)) {
      dist_right <- dist_left
    }

    if (is.na(dist_left) && is.na(dist_right)) {
      half_width <- diff(range(psi_ll_df$psi)) / 2
    } else {
      half_width <- max(dist_left, dist_right)
    }
  } else {
    half_width <- diff(range(psi_ll_df$psi)) / 2
  }

  padding_factor <- 1.05
  half_width <- half_width * padding_factor

  psi_limits <- psi_mle + c(-1, 1) * half_width

  # --------------------------------------------------
  # Y limits — based on widest alpha
  # --------------------------------------------------
  if (nrow(ci_raw) > 0) {
    alpha_min <- min(ci_raw$alpha)
  } else {
    alpha_min <- 0.05
  }

  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  # --------------------------------------------------
  # Curve layer
  # --------------------------------------------------
  curve_layer <- make_stat_fn(
    psi_endpoints = psi_limits,
    zero_max_psi_ll_fn = zero_max_psi_ll_fn,
    pseudolikelihood = pseudolikelihood,
    comparison = FALSE
  )

  # --------------------------------------------------
  # Labels
  # --------------------------------------------------
  label_data <- data.frame(
    source = c(pseudolikelihood, "Truth"),
    value = c(
      point_estimate_df$psi_hat,
      point_estimate_df$psi_0
    ),
    label = c(
      paste0(
        "hat(psi)[",
        ifelse(pseudolikelihood == "profile", "PL", "IL"),
        "]"
      ),
      "psi[0]"
    )
  )

  # --------------------------------------------------
  # Assemble plot
  # --------------------------------------------------
  plot_base(plot = "single_curve") +
    curve_layer +
    loglik_reference_line() +

    # ---- CI endpoints ----
    make_ci_vline_layer(ci_long) +
    ggplot2::scale_color_manual(
      name = "Confidence",
      values = plot_ci_palette(interval_estimate_df),
      breaks = interval_estimate_df$level,
      guide = ggplot2::guide_legend(
        override.aes = list(
          linetype = "solid",
          linewidth = plot_ci_linewidth() * 3
        )
      )
    ) +
    ggnewscale::new_scale_color() +

    # ---- Labels ----
    make_label_vlines(label_data, comparison = FALSE) +
    make_label_repel(label_data, y = y_limits[1] / 2) +
    ggplot2::scale_color_manual(
      values = c(
        plot_point_estimate_color(pseudolikelihood),
        plot_truth_color()
      ),
      guide = "none"
    ) +

    # ---- Decorations ----
    ggplot2::labs(title = likelihood_title(type)) +
    likelihood_axes() +

    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = psi_limits, ylim = y_limits) +

    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )
}

# ---------------------------------------------------------------------
# Multi-curve comparison visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood comparison curves
#'
#' @keywords internal
#' @noRd
plot_pseudolikelihood_curves <- function(res_list) {
  .assert_local_plotting()

  # --------------------------------------------------
  # Extract all MLEs
  # --------------------------------------------------
  psi_mles <- purrr::map_dbl(
    res_list,
    \(x) x$inference$point_estimate_df$psi_hat
  )

  psi_center <- mean(range(psi_mles))

  # --------------------------------------------------
  # Extract ALL CI endpoints from both results
  # --------------------------------------------------
  ci_all <- purrr::map_dfr(
    res_list,
    \(x) {
      extract_ci_long(x$inference$interval_estimate_df)
    }
  ) |>
    dplyr::filter(!is.na(endpoint))

  # --------------------------------------------------
  # Determine symmetric psi limits
  # --------------------------------------------------
  if (nrow(ci_all) > 0) {
    alpha_widest <- min(ci_all$alpha)

    ci_widest <- ci_all |>
      dplyr::filter(alpha == alpha_widest)

    lower_vals <- ci_widest |>
      dplyr::filter(position == "lower") |>
      dplyr::pull(endpoint)

    upper_vals <- ci_widest |>
      dplyr::filter(position == "upper") |>
      dplyr::pull(endpoint)

    dist_left <- if (length(lower_vals) > 0) {
      psi_center - min(lower_vals)
    } else {
      NA_real_
    }

    dist_right <- if (length(upper_vals) > 0) {
      max(upper_vals) - psi_center
    } else {
      NA_real_
    }

    if (is.na(dist_left) && !is.na(dist_right)) {
      dist_left <- dist_right
    }
    if (is.na(dist_right) && !is.na(dist_left)) {
      dist_right <- dist_left
    }

    if (is.na(dist_left) && is.na(dist_right)) {
      half_width <- max(abs(psi_mles - psi_center))
    } else {
      half_width <- max(dist_left, dist_right)
    }
  } else {
    half_width <- max(abs(psi_mles - psi_center))
  }

  padding_factor <- 1.05
  half_width <- half_width * padding_factor

  psi_limits <- psi_center + c(-1, 1) * half_width

  # --------------------------------------------------
  # Y limits — based on smallest alpha across both
  # --------------------------------------------------
  alpha_min <- if (nrow(ci_all) > 0) min(ci_all$alpha) else 0.05

  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  # --------------------------------------------------
  # Curve layers (NOW using shared psi_limits)
  # --------------------------------------------------
  curve_layers <- purrr::map(
    res_list,
    \(x) {
      make_stat_fn(
        psi_endpoints = psi_limits,
        zero_max_psi_ll_fn = x$inference$zero_max_psi_ll_fn,
        pseudolikelihood = tolower(attr(x$psi_ll_df, "type")),
        comparison = TRUE
      )
    }
  )

  # --------------------------------------------------
  # Labels
  # --------------------------------------------------
  label_data <- purrr::imap_dfr(
    res_list,
    \(x, key) {
      suffix <- ifelse(key == "profile", "PL", "IL")

      data.frame(
        source = c(key, "Truth"),
        value = c(
          x$inference$point_estimate_df$psi_hat,
          x$inference$point_estimate_df$psi_0
        ),
        label = c(
          paste0("hat(psi)[", suffix, "]"),
          "psi[0]"
        ),
        color = c(
          plot_point_estimate_color(key, comparison = TRUE),
          plot_truth_color()
        )
      )
    }
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(label)

  # --------------------------------------------------
  # Confidence cutoffs (horizontal lines)
  # --------------------------------------------------
  crit_df <- purrr::map_dfr(
    res_list,
    \(x) {
      attr(
        x$inference$interval_estimate_df,
        "interval_estimate_raw"
      )[, "alpha", drop = FALSE]
    }
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      crit = 0.5 * stats::qchisq(1 - alpha, df = 1),
      label = paste0(100 * (1 - alpha), "%")
    )

  crit_df$color <- plot_ci_palette(
    dplyr::distinct(data.frame(Level = crit_df$label))
  )

  # --------------------------------------------------
  # Assemble plot
  # --------------------------------------------------
  plot_base(plot = "comparison") +
    curve_layers +
    loglik_reference_line() +

    make_ci_hline_layer(crit_df) +
    ggplot2::scale_color_manual(
      name = "Confidence",
      values = crit_df$color,
      breaks = crit_df$label
    ) +
    ggnewscale::new_scale_color() +

    make_label_vlines(label_data, comparison = TRUE) +
    make_label_repel(label_data, y = y_limits[1] / 2) +
    ggplot2::scale_color_manual(
      values = label_data$color,
      guide = "none"
    ) +

    ggplot2::labs(
      title = "Pseudo Log-Likelihood Comparison Plot"
    ) +
    likelihood_axes() +

    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = psi_limits, ylim = y_limits) +

    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )
}
