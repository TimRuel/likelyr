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
# Internal: boundary shading layers
# ---------------------------------------------------------------------

#' Build boundary shading and label layers for a psi plot
#'
#' @param psi_interval A sets::interval object or NULL.
#' @param psi_limits   Numeric length-2 vector of full plot x limits
#'   (may extend beyond the domain boundary).
#'
#' @return A list of ggplot2 layers (may be empty).
#'
#' @keywords internal
#' @noRd
make_boundary_layers <- function(psi_interval, psi_limits) {
  if (is.null(psi_interval)) {
    return(list())
  }

  domain_lower <- min(psi_interval)
  domain_upper <- max(psi_interval)

  boundary_fill <- "#888888"
  boundary_alpha <- 0.12

  layers <- list()

  if (is.finite(domain_lower) && domain_lower > psi_limits[1]) {
    layers <- c(
      layers,
      list(
        ggplot2::annotate(
          "rect",
          xmin = psi_limits[1],
          xmax = domain_lower,
          ymin = -Inf,
          ymax = Inf,
          fill = boundary_fill,
          alpha = boundary_alpha
        ),
        ggplot2::geom_vline(
          xintercept = domain_lower,
          linetype = "dashed",
          linewidth = 0.4,
          colour = "black"
        )
      )
    )
  }

  if (is.finite(domain_upper) && domain_upper < psi_limits[2]) {
    layers <- c(
      layers,
      list(
        ggplot2::annotate(
          "rect",
          xmin = domain_upper,
          xmax = psi_limits[2],
          ymin = -Inf,
          ymax = Inf,
          fill = boundary_fill,
          alpha = boundary_alpha
        ),
        ggplot2::geom_vline(
          xintercept = domain_upper,
          linetype = "dashed",
          linewidth = 0.4,
          colour = "black"
        )
      )
    )
  }

  layers
}

# ---------------------------------------------------------------------
# Internal: resolve psi_loglik_df for plotting
# ---------------------------------------------------------------------

#' Resolve the psi_loglik_df to use for spline fitting
#'
#' Prefers the filtered df stored on the inference result (used to
#' produce estimates) over the unfiltered df on the workspace result
#' (preserved for diagnose()). When inference has not been run, falls
#' back to the workspace df.
#'
#' @param inference_result An inference result object, or NULL.
#' @param workspace_psi_loglik_df The unfiltered df from the workspace.
#'
#' @return A psi_loglik_df suitable for spline fitting.
#'
#' @keywords internal
#' @noRd
.resolve_psi_loglik_df <- function(inference_result, workspace_psi_loglik_df) {
  inference_result$psi_loglik_df %||% workspace_psi_loglik_df
}

# ---------------------------------------------------------------------
# Internal: legend position
# ---------------------------------------------------------------------

#' Choose legend corner opposite to the point estimates
#'
#' Places the legend in the top-left when estimates cluster on the right
#' half of the plot, and top-right when they cluster on the left half.
#'
#' @param estimate_psis Numeric vector of estimate ψ values (psi_hat, psi_0).
#' @param psi_limits    Numeric length-2 vector of plot x limits.
#'
#' @return A list suitable for passing to \code{legend.position.inside}
#'   and \code{legend.justification}.
#'
#' @keywords internal
#' @noRd
.legend_corner <- function(estimate_psis, psi_limits) {
  anchor_mean <- mean(estimate_psis[is.finite(estimate_psis)], na.rm = TRUE)
  midpoint <- mean(psi_limits)
  x <- if (anchor_mean > midpoint) 0 else 1
  list(position = c(x, 1), justification = c(x, 1))
}

# ---------------------------------------------------------------------
# Point cloud visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood evaluation points
#'
#' @keywords internal
#' @noRd
plot_pseudolikelihood_points <- function(result) {
  psi_loglik_df <- result$psi_loglik_df
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
plot_pseudolikelihood_curve <- function(
  inference_result,
  psi_loglik_df,
  points = FALSE
) {
  point_estimate_df <- inference_result$point_estimate_df
  interval_estimate_df <- inference_result$interval_estimate_df
  pseudolikelihood <- attr(psi_loglik_df, "pseudolikelihood")

  # Use the filtered df from inference when available — it matches the
  # data used to compute psi_hat, keeping the curve peak aligned with
  # the estimate. Fall back to the workspace df before infer() is run.
  psi_loglik_df_fit <- .resolve_psi_loglik_df(inference_result, psi_loglik_df)
  psi_loglik <- fit_psi_loglik(psi_loglik_df_fit)
  psi_loglik_max <- get_psi_loglik_max_point(psi_loglik)[["maximum"]]

  psi_hat <- point_estimate_df$psi_hat
  ci_long <- extract_ci_long(interval_estimate_df)
  ci_raw <- ci_long |> dplyr::filter(!is.na(endpoint))

  psi_interval <- attr(interval_estimate_df, "psi_interval")
  domain_lower <- if (!is.null(psi_interval)) min(psi_interval) else -Inf
  domain_upper <- if (!is.null(psi_interval)) max(psi_interval) else Inf

  psi_anchors <- c(
    ci_raw$endpoint,
    psi_hat,
    point_estimate_df$psi_0
  )
  psi_anchors <- psi_anchors[is.finite(psi_anchors)]

  if (length(psi_anchors) == 0L) {
    psi_anchors <- range(psi_loglik_df_fit$psi)
  }

  padding <- diff(range(psi_anchors)) * 0.05
  if (padding == 0) {
    padding <- 0.01
  }

  psi_limits <- c(
    min(psi_anchors) - padding,
    max(psi_anchors) + padding
  )

  curve_limits <- c(
    max(psi_limits[1], domain_lower),
    min(psi_limits[2], domain_upper)
  )

  alpha_min <- if (nrow(ci_raw) > 0) min(ci_raw$alpha) else 0.05
  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  boundary_layers <- make_boundary_layers(psi_interval, psi_limits)

  curve_layer <- make_curve_layer(
    psi_loglik = psi_loglik,
    psi_limits = curve_limits,
    comparison = FALSE
  )

  points_layer <- if (points) {
    ggplot2::geom_point(
      data = psi_loglik_df_fit,
      ggplot2::aes(x = psi, y = loglik - psi_loglik_max),
      color = "black",
      size = plot_point_cloud_size(),
      alpha = plot_point_cloud_alpha()
    )
  } else {
    NULL
  }

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

  plot_base(plot = "single_curve") +
    boundary_layers +
    curve_layer +
    points_layer +
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
      legend.position.inside = .legend_corner(
        c(psi_hat, point_estimate_df$psi_0),
        psi_limits
      )$position,
      legend.justification = .legend_corner(
        c(psi_hat, point_estimate_df$psi_0),
        psi_limits
      )$justification
    )
}

# ---------------------------------------------------------------------
# Multi-curve comparison visualization
# ---------------------------------------------------------------------

#' Plot pseudolikelihood comparison curves
#'
#' @description
#' Expects a named list of result objects (e.g. \code{list(profile = ...,
#' integrated = ...)}), each carrying \code{$psi_loglik_df} and
#' \code{$inference}.
#'
#' @keywords internal
#' @noRd
plot_pseudolikelihood_curves <- function(res_list) {
  .assert_local_plotting()

  infer_list <- purrr::map(res_list, \(x) x$inference)

  # For each result, prefer the inference df (filtered, used for
  # estimation) over the workspace df (unfiltered, for diagnose()).
  psi_loglik_dfs <- purrr::map(res_list, \(x) {
    .resolve_psi_loglik_df(x$inference, x$psi_loglik_df)
  })

  psi_hats <- purrr::map_dbl(infer_list, \(x) x$point_estimate_df$psi_hat)

  ci_all <- purrr::map_dfr(
    infer_list,
    \(x) extract_ci_long(x$interval_estimate_df)
  ) |>
    dplyr::filter(!is.na(endpoint))

  psi_interval <- purrr::detect(
    purrr::map(
      infer_list,
      \(x) attr(x$interval_estimate_df, "psi_interval")
    ),
    \(x) !is.null(x)
  )
  domain_lower <- if (!is.null(psi_interval)) min(psi_interval) else -Inf
  domain_upper <- if (!is.null(psi_interval)) max(psi_interval) else Inf

  psi_anchors <- c(
    ci_all$endpoint,
    psi_hats,
    purrr::map_dbl(infer_list, \(x) x$point_estimate_df$psi_0)
  )
  psi_anchors <- psi_anchors[is.finite(psi_anchors)]

  if (length(psi_anchors) == 0L) {
    psi_anchors <- psi_hats
  }

  padding <- diff(range(psi_anchors)) * 0.05
  if (padding == 0) {
    padding <- 0.01
  }

  psi_limits <- c(
    min(psi_anchors) - padding,
    max(psi_anchors) + padding
  )

  curve_limits <- c(
    max(psi_limits[1], domain_lower),
    min(psi_limits[2], domain_upper)
  )

  alpha_min <- if (nrow(ci_all) > 0) min(ci_all$alpha) else 0.05
  y_lower <- -0.5 * qchisq(1 - alpha_min, df = 1)
  y_limits <- c(y_lower * 1.05, 0)

  boundary_layers <- make_boundary_layers(psi_interval, psi_limits)

  curve_layers <- psi_loglik_dfs |>
    purrr::map(
      \(df) {
        make_curve_layer(
          psi_loglik = fit_psi_loglik(df),
          psi_limits = curve_limits,
          comparison = TRUE
        )
      }
    )

  label_data <- purrr::imap_dfr(
    infer_list,
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

  crit_df <- purrr::map_dfr(
    infer_list,
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

  crit_df$color <- plot_ci_palette(
    dplyr::distinct(data.frame(Level = crit_df$label))
  )

  plot_base(plot = "comparison") +
    boundary_layers +
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
      legend.position.inside = .legend_corner(
        c(
          psi_hats,
          purrr::map_dbl(infer_list, function(x) x$point_estimate_df$psi_0)
        ),
        psi_limits
      )$position,
      legend.justification = .legend_corner(
        c(
          psi_hats,
          purrr::map_dbl(infer_list, function(x) x$point_estimate_df$psi_0)
        ),
        psi_limits
      )$justification
    )
}
