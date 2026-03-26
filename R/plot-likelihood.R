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
plot_pseudolikelihood_points <- function(inference_result) {
  psi_loglik_df <- inference_result$psi_loglik_df
  pseudolikelihood <- attr(psi_loglik_df, "pseudolikelihood")

  plot_base(plot = "points") +
    ggplot2::geom_point(
      data = psi_loglik_df,
      ggplot2::aes(x = psi, y = loglik),
      color = plot_curve_color(pseudolikelihood),
      size = plot_point_cloud_size(),
      alpha = plot_point_cloud_alpha()
    ) +
    ggplot2::labs(title = likelihood_title(pseudolikelihood)) +
    likelihood_axes()
}

# ---------------------------------------------------------------------
# Single-curve likelihood + inference visualization
# ---------------------------------------------------------------------

#' Plot single pseudolikelihood curve with inference overlays
#'
#' @importFrom stats qchisq
#' @keywords internal
#' @noRd
plot_pseudolikelihood_curve <- function(inference_result) {
  psi_loglik_df <- inference_result$psi_loglik_df
  point_estimate_df <- inference_result$point_estimate_df
  interval_estimate_df <- inference_result$interval_estimate_df
  pseudolikelihood <- attr(psi_loglik_df, "pseudolikelihood")
  psi_loglik <- fit_psi_loglik(psi_loglik_df)
  psi_hat <- point_estimate_df$psi_hat

  ci_long <- extract_ci_long(interval_estimate_df)
  ci_raw <- ci_long |>
    dplyr::filter(!is.na(endpoint))

  # --------------------------------------------------
  # PSI limits — contain all relevant psi values
  # --------------------------------------------------
  psi_anchors <- c(
    ci_raw$endpoint,
    psi_hat,
    point_estimate_df$psi_0,
    range(psi_loglik_df$psi)
  )
  psi_anchors <- psi_anchors[is.finite(psi_anchors)]

  if (length(psi_anchors) == 0L) {
    psi_anchors <- range(psi_loglik_df$psi)
  }

  padding <- diff(range(psi_anchors)) * 0.05
  if (padding == 0) {
    padding <- 0.01
  }

  psi_limits <- c(min(psi_anchors) - padding, max(psi_anchors) + padding)

  # --------------------------------------------------
  # Y limits
  # --------------------------------------------------
  alpha_min <- if (nrow(ci_raw) > 0) min(ci_raw$alpha) else 0.05
  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  # --------------------------------------------------
  # Curve layer
  # --------------------------------------------------
  curve_layer <- make_curve_layer(
    psi_loglik = psi_loglik,
    comparison = FALSE
  )

  # --------------------------------------------------
  # Labels
  # --------------------------------------------------
  label_data <- data.frame(
    source = c(pseudolikelihood, "Truth"),
    value = c(point_estimate_df$psi_hat, point_estimate_df$psi_0),
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

    make_label_vlines(label_data, comparison = FALSE) +
    make_label_repel(label_data, y = y_limits[1] / 2) +
    ggplot2::scale_color_manual(
      values = c(
        plot_point_estimate_color(pseudolikelihood),
        plot_truth_color()
      ),
      guide = "none"
    ) +

    ggplot2::labs(title = likelihood_title(pseudolikelihood)) +
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

  psi_mles <- purrr::map_dbl(res_list, \(x) x$point_estimate_df$psi_hat)
  psi_center <- mean(range(psi_mles))

  ci_all <- purrr::map_dfr(
    res_list,
    \(x) extract_ci_long(x$interval_estimate_df)
  ) |>
    dplyr::filter(!is.na(endpoint))

  # --------------------------------------------------
  # PSI limits — contain all relevant psi values
  # --------------------------------------------------
  psi_anchors <- c(
    ci_all$endpoint,
    psi_mles,
    purrr::map_dbl(res_list, \(x) x$point_estimate_df$psi_0),
    unlist(purrr::map(res_list, \(x) range(x$psi_loglik_df$psi)))
  )
  psi_anchors <- psi_anchors[is.finite(psi_anchors)]

  if (length(psi_anchors) == 0L) {
    psi_anchors <- psi_mles
  }

  padding <- diff(range(psi_anchors)) * 0.05
  if (padding == 0) {
    padding <- 0.01
  }

  psi_limits <- c(min(psi_anchors) - padding, max(psi_anchors) + padding)

  # --------------------------------------------------
  # Y limits
  # --------------------------------------------------
  alpha_min <- if (nrow(ci_all) > 0) min(ci_all$alpha) else 0.05
  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  # --------------------------------------------------
  # Curve layers
  # --------------------------------------------------
  curve_layers <- res_list |>
    purrr::map(
      \(x) {
        psi_loglik <- fit_psi_loglik(x$psi_loglik_df)
        make_curve_layer(
          psi_loglik = psi_loglik,
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
        value = c(x$point_estimate_df$psi_hat, x$point_estimate_df$psi_0),
        label = c(paste0("hat(psi)[", suffix, "]"), "psi[0]"),
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
  # Confidence cutoffs
  # --------------------------------------------------
  crit_df <- purrr::map_dfr(
    res_list,
    \(x) {
      attr(x$interval_estimate_df, "interval_estimate_raw")[,
        "alpha",
        drop = FALSE
      ]
    }
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      crit = 0.5 * qchisq(1 - alpha, df = 1),
      label = paste0(100 * (1 - alpha), "%")
    )

  crit_df$color <- plot_ci_palette(dplyr::distinct(data.frame(
    Level = crit_df$label
  )))

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
    ggplot2::scale_color_manual(values = label_data$color, guide = "none") +

    ggplot2::labs(title = "Pseudo Log-Likelihood Comparison Plot") +
    likelihood_axes() +

    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = psi_limits, ylim = y_limits) +

    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )
}
