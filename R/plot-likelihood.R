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
plot_pseudolikelihood_points <- function(psi_ll_df) {
  .assert_local_plotting()

  type <- attr(psi_ll_df, "type")
  pseudolikelihood <- tolower(type)

  plot_base(plot = "points") +
    ggplot2::geom_point(
      data = psi_ll_df,
      ggplot2::aes(x = psi, y = loglik),
      color = plot_curve_color(pseudolikelihood),
      size = plot_point_cloud_size(),
      alpha = plot_point_cloud_alpha()
    ) +
    ggplot2::labs(title = likelihood_title(type)) +
    likelihood_axes()
}

# ---------------------------------------------------------------------
# Single-curve likelihood + inference visualization
# ---------------------------------------------------------------------

#' Plot single pseudolikelihood curve with inference overlays
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
  # Axis limits
  # --------------------------------------------------
  psi_limits <- range(psi_ll_df$psi) +
    c(-1, 1) * 3 * head(diff(psi_ll_df$psi), 1)

  y_limits <- compute_y_limits(psi_ll_df)

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
  # Confidence intervals
  # --------------------------------------------------
  ci_long <- extract_ci_long(interval_estimate_df)

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
      breaks = interval_estimate_df$Level,
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

    # ---- X domain restriction ----
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = psi_limits
    ) +

    # ---- Y zoom ONLY ----
    ggplot2::coord_cartesian(ylim = y_limits) +

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
  # Curve layers
  # --------------------------------------------------
  curve_layers <- purrr::map(
    res_list,
    \(x) {
      psi_ll_df <- x$psi_ll_df

      make_stat_fn(
        psi_endpoints = range(psi_ll_df$psi),
        zero_max_psi_ll_fn = x$inference$zero_max_psi_ll_fn,
        pseudolikelihood = tolower(attr(psi_ll_df, "type")),
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
  # Confidence cutoffs
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
  # Axis limits
  # --------------------------------------------------
  psi_limits <- range(
    unlist(purrr::map(res_list, \(x) x$psi_ll_df$psi))
  )

  y_limits <- range(
    unlist(
      purrr::map(
        res_list,
        \(x) compute_y_limits(x$psi_ll_df)
      )
    )
  )

  # --------------------------------------------------
  # Assemble plot
  # --------------------------------------------------
  plot_base(plot = "comparison") +
    curve_layers +
    loglik_reference_line() +

    # ---- CI cutoffs ----
    make_ci_hline_layer(crit_df) +
    ggplot2::scale_color_manual(
      name = "Confidence",
      values = crit_df$color,
      breaks = crit_df$label
    ) +
    ggnewscale::new_scale_color() +

    # ---- Labels ----
    make_label_vlines(label_data, comparison = TRUE) +
    make_label_repel(label_data, y = y_limits[1] / 2) +
    ggplot2::scale_color_manual(
      values = label_data$color,
      guide = "none"
    ) +

    # ---- Decorations ----
    ggplot2::labs(
      title = "Pseudo Log-Likelihood Comparison Plot"
    ) +
    likelihood_axes() +

    # ---- X domain restriction ----
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = psi_limits
    ) +

    # ---- Y zoom ONLY ----
    ggplot2::coord_cartesian(ylim = y_limits) +

    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )
}
