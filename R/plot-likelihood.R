# =====================================================================
# plot-likelihood.R
# Pseudolikelihood Visualization for Result and Inference Objects
# =====================================================================

plot_pseudolikelihood_points <- function(psi_ll_df) {

  type <- attr(psi_ll_df, "type")

  p <- plot_base() +
    ggplot2::geom_point(
      data = psi_ll_df,
      ggplot2::aes(x = psi, y = loglik),
      color = "cyan",
      size = 3,
      alpha = 0.7
      ) +
    ggplot2::labs(
      title = paste(type, "Log-Likelihood"),
      x     = expression(psi),
      y     = expression("log L("*psi*")")
    )

  invisible(p)
}

plot_pseudolikelihood_curve <- function(
    psi_ll_df,
    zero_max_psi_ll_fn,
    point_estimate_df,
    interval_estimates_df
) {

  type <- attr(psi_ll_df, "type")

  psi_endpoints <- range(psi_ll_df$psi)
  stat_fn <- make_stat_fn(psi_endpoints, zero_max_psi_ll_fn)

  psi_hat <- point_estimate_df$psi_hat
  psi_0 <- point_estimate_df$psi_0

  label_data <- data.frame(
    source = c("MLE", "Truth"),
    value  = c(psi_hat, psi_0),
    label  = c("hat(psi)", "psi[0]")
  )

  conf_ints <- attr(interval_estimates_df, "interval_estimates_raw")
  conf_ints$level <- interval_estimates_df$Level

  conf_ints_long <- tidyr::pivot_longer(
    conf_ints,
    cols = c("lower", "upper"),
    names_to = "position",
    values_to = "endpoint"
  )

  ci_palette <- get_ci_palette(interval_estimates_df)

  crit_max <- 0.5 * qchisq(1 - min(conf_ints$alpha), 1)
  y_min    <- -crit_max - 0.5

  p <- plot_base() +
    stat_fn +
    ggplot2::geom_hline(yintercept = 0, linetype = 5) +

    # CI lines
    ggplot2::geom_vline(
      data = conf_ints_long,
      ggplot2::aes(xintercept = endpoint, color = level),
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      name   = "Confidence",
      values = ci_palette,
      breaks = interval_estimates_df$Level,
      guide  = ggplot2::guide_legend(
        override.aes = list(
          linetype = "solid",
          linewidth = 3
        )
      )
    ) +

    ggnewscale::new_scale_color() +

    # MLE + Truth
    ggplot2::geom_vline(
      data = label_data,
      ggplot2::aes(xintercept = value, color = source),
      show.legend = FALSE
    ) +
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = value, y = 0.5 * y_min, label = label, color = source),
      direction = "y",
      force = TRUE,
      hjust = 0.5,
      parse = TRUE,
      seed = 7835,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(MLE = MLE_COLOR, Truth = TRUTH_COLOR),
      guide  = "none"
    ) +

    ggplot2::labs(
      title = paste(type, "Log-Likelihood"),
      x     = expression(psi),
      y     = expression("log L("*psi*")")
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = psi_endpoints) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(y_min, 0.1)) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )

  invisible(p)
}

plot_pseudolikelihoods <- function(res) NULL

