# =====================================================================
# plot-likelihood.R
# Pseudolikelihood Visualization for Inference Objects
# =====================================================================

plot_pseudolikelihood <- function(x) {

  psi_endpoints <- range(x$psi)
  stat_fn <- make_stat_fn(psi_endpoints, x$relative_loglik)

  MLE_label <- if (x$mode == "Profile") "hat(psi)" else "bar(psi)"

  label_data <- data.frame(
    source = c("MLE", "Truth"),
    value  = c(x$psi_mle, x$psi_0),
    label  = c(MLE_label, "psi[0]")
  )

  conf_ints_long <- tidyr::pivot_longer(
    x$conf_ints,
    cols = c("lower", "upper"),
    names_to = "endpoint_position",
    values_to = "value"
  )

  ci_palette <- get_ci_colors(x$conf_ints)

  crit_max <- 0.5 * qchisq(1 - min(x$alpha_levels), 1)
  y_min    <- -crit_max - 0.5

  p <- plot_base() +
    stat_fn +
    ggplot2::geom_hline(yintercept = 0, linetype = 5) +

    # CI lines
    ggplot2::geom_vline(
      data = conf_ints_long,
      ggplot2::aes(xintercept = value, color = confidence),
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(
      name   = "Confidence",
      values = ci_palette,
      breaks = x$conf_ints$confidence,
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
      parse = TRUE,
      seed = 7835,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(MLE = MLE_COLOR, Truth = TRUTH_COLOR),
      guide  = "none"
    ) +

    ggplot2::labs(
      title = paste0(x$mode, " Log-Likelihood"),
      x     = expression(psi),
      y     = "Relative log-likelihood"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = psi_endpoints) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(y_min, 0.1)) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification = c(1, 1)
    )

  print(p)
  invisible(p)
}
