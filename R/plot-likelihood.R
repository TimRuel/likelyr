# =====================================================================
# plot-likelihood.R
# Pseudolikelihood Visualization for Result and Inference Objects
#
# Orchestrates complete likelihood plots using:
#   - plot-style.R  (semantic accessors)
#   - plot-utils.R  (grammar + geometry)
#
# This file must not hard-code visual values or perform low-level
# reshaping. It decides *what* to plot and *when* comparison styling
# is enabled.
# =====================================================================


# ---------------------------------------------------------------------
# Point cloud visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood evaluation points
#'
#' @param psi_ll_df Data frame with columns psi, loglik and attribute "type"
#'
#' @return ggplot object (invisibly)
#' @keywords internal
plot_pseudolikelihood_points <- function(psi_ll_df) {

  type   <- attr(psi_ll_df, "type")
  pseudolikelihood <- tolower(type)

  p <- plot_base() +
    ggplot2::geom_point(
      data  = psi_ll_df,
      ggplot2::aes(x = psi, y = loglik),
      color = plot_curve_color(pseudolikelihood),
      size  = plot_point_cloud_size(),
      alpha = plot_point_cloud_alpha()
    ) +
    ggplot2::labs(title = likelihood_title(type)) +
    likelihood_axes()

  invisible(p)
}


# ---------------------------------------------------------------------
# Single-curve likelihood + inference visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood curve with inference annotations
#'
#' @param psi_ll_df Data frame of evaluated log-likelihood values
#' @param zero_max_psi_ll_fn Zero-shifted log-likelihood function
#' @param point_estimate_df Data frame with psi_hat and psi_0
#' @param interval_estimate_df Data frame of interval estimates
#'
#' @return ggplot object (invisibly)
#' @keywords internal
plot_pseudolikelihood_curve <- function(
    psi_ll_df,
    zero_max_psi_ll_fn,
    point_estimate_df,
    interval_estimate_df
) {

  type   <- attr(psi_ll_df, "type")
  pseudolikelihood <- tolower(type)

  psi_limits <- range(psi_ll_df$psi)

  # --------------------------------------------------
  # Curve layer (single-likelihood styling)
  # --------------------------------------------------

  curve_layer <- make_stat_fn(
    psi_endpoints      = psi_limits,
    zero_max_psi_ll_fn = zero_max_psi_ll_fn,
    pseudolikelihood   = pseudolikelihood,
    comparison         = FALSE
  )

  # --------------------------------------------------
  # Confidence intervals
  # --------------------------------------------------

  ci_long  <- extract_ci_long(interval_estimate_df)
  y_limits <- compute_y_limits(
    attr(interval_estimate_df, "interval_estimate_raw")$alpha
  )

  # --------------------------------------------------
  # Labels: MLE + truth
  # --------------------------------------------------

  label_data <- data.frame(
    source = c(pseudolikelihood, "Truth"),
    value  = c(point_estimate_df$psi_hat, point_estimate_df$psi_0),
    label  = c(
      paste0(
        "hat(psi)[",
        ifelse(pseudolikelihood == "profile", "PL", "IL"),
        "]"
        ),
      "psi[0]")
  )

  # --------------------------------------------------
  # Assemble plot
  # --------------------------------------------------

  p <- plot_base() +
    curve_layer +
    loglik_reference_line() +

    make_ci_vline_layer(ci_long) +
    ggplot2::scale_color_manual(
      name   = "Confidence",
      values = plot_ci_palette(interval_estimate_df),
      breaks = interval_estimate_df$Level,
      guide  = ggplot2::guide_legend(
        override.aes = list(
          linetype  = "solid",
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

    ggplot2::labs(title = likelihood_title(type)) +
    likelihood_axes() +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = psi_limits) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification   = c(1, 1)
    )

  invisible(p)
}


# ---------------------------------------------------------------------
# Multi-curve comparison visualization
# ---------------------------------------------------------------------

#' Compare multiple pseudolikelihood curves
#'
#' @param res_list Named list of result objects
#'
#' @return ggplot object (invisibly)
#' @keywords internal
plot_pseudolikelihood_curves <- function(res_list) {

  # --------------------------------------------------
  # Curve layers (comparison styling enabled)
  # --------------------------------------------------

  curve_layers <- purrr::map(
    res_list,
    \(x) {
      psi_ll_df <- x$psi_ll_df
      make_stat_fn(
        psi_endpoints      = range(psi_ll_df$psi),
        zero_max_psi_ll_fn = x$inference$zero_max_psi_ll_fn,
        pseudolikelihood   = tolower(attr(psi_ll_df, "type")),
        comparison         = TRUE
      )
    }
  )

  # --------------------------------------------------
  # Labels (MLEs + truth)
  # --------------------------------------------------

  label_data <- purrr::imap_dfr(
    res_list,
    \(x, key) {

      suffix <- ifelse(key == "profile", "PL", "IL")

      data.frame(
        source = c(key, "Truth"),
        value  = c(
          x$inference$point_estimate_df$psi_hat,
          x$inference$point_estimate_df$psi_0
          ),
        label  = c(
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
      crit  = 0.5 * stats::qchisq(1 - alpha, df = 1),
      label = paste0(100 * (1 - alpha), "%")
    )

  crit_df$color <- plot_ci_palette(
    dplyr::distinct(data.frame(Level = crit_df$label))
  )

  # --------------------------------------------------
  # Axes limits
  # --------------------------------------------------

  psi_limits <- range(unlist(purrr::map(res_list, \(x) x$psi_ll_df$psi)))
  y_limits   <- c(-max(crit_df$crit) - 0.5, 0.1)

  # --------------------------------------------------
  # Assemble plot
  # --------------------------------------------------

  p <- plot_base() +
    curve_layers +
    loglik_reference_line() +

    make_ci_hline_layer(crit_df) +
    ggplot2::scale_color_manual(
      name   = "Confidence",
      values = crit_df$color,
      breaks = crit_df$label
    ) +

    ggnewscale::new_scale_color() +

    make_label_vlines(label_data, comparison = TRUE) +
    make_label_repel(label_data, y = y_limits[1] / 2) +
    ggplot2::scale_color_manual(
      values = label_data$color,
      guide  = "none"
    ) +

    ggplot2::labs(
      title = "Pseudo Log-Likelihood Comparison Plot"
    ) +
    likelihood_axes() +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = psi_limits) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification   = c(1, 1)
    )

  invisible(p)
}
