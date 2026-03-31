# ================================================================================
# likelihood-diagnose-integrated-plots.R
# Plot helpers for integrated likelihood diagnostics (local-only)
# ================================================================================

# NOTE:
#   All functions in this file are **local-only**.
#   They must never be called on HPC.
#   Enforcement is via .assert_local_plotting() at entry.

#' Plot effective sample size (ESS) across the ψ-grid
#'
#' @keywords internal
#' @noRd
build_integrated_ess_plot <- function(ess) {
  idx <- seq_along(ess)
  style <- plot_diagnostics_style("integrated", "ess")

  plot_base(plot = "ess") +
    make_diagnostics_line(ggplot2::aes(x = idx, y = ess), style) +
    ggplot2::labs(
      title = "Effective Sample Size",
      x = "Grid index",
      y = "ESS"
    )
}

#' Plot relative Monte Carlo standard error
#'
#' @keywords internal
#' @noRd
build_integrated_rel_se_plot <- function(rel_se) {
  idx <- seq_along(rel_se)
  style <- plot_diagnostics_style("integrated", "rel_se")

  plot_base(plot = "rel_se") +
    make_diagnostics_line(ggplot2::aes(x = idx, y = rel_se), style) +
    ggplot2::labs(
      title = "Relative Monte Carlo SE",
      x = "Grid index",
      y = "Relative SE"
    )
}

#' Plot outlier fraction across the ψ-grid
#'
#' @keywords internal
#' @noRd
build_integrated_outlier_plot <- function(outlier_fraction) {
  idx <- seq_along(outlier_fraction)
  style <- plot_diagnostics_style("integrated", "outliers")

  plot_base(plot = "outliers") +
    make_diagnostics_line(ggplot2::aes(x = idx, y = outlier_fraction), style) +
    ggplot2::labs(
      title = "Outlier Fraction",
      x = "Grid index",
      y = "Fraction"
    )
}

#' Plot omega-hat covariance eigenvalues
#'
#' @keywords internal
#' @noRd
build_integrated_omega_eigen_plot <- function(eigenvalues) {
  if (is.null(eigenvalues)) {
    return(NULL)
  }

  k <- seq_along(eigenvalues)
  style <- plot_diagnostics_style("integrated", "omega_eigen")

  plot_base(plot = "omega_eigen") +
    make_diagnostics_point(ggplot2::aes(x = k, y = eigenvalues), style) +
    make_diagnostics_line(ggplot2::aes(x = k, y = eigenvalues), style) +
    ggplot2::labs(
      title = "Omega-Hat Covariance Eigenvalues",
      x = "Index",
      y = "Eigenvalue"
    )
}

#' Plot omega-hat PCA scatter
#'
#' @keywords internal
#' @noRd
build_integrated_omega_pca_plot <- function(Omega) {
  if (is.null(Omega) || ncol(Omega) < 2) {
    return(NULL)
  }

  pca <- stats::prcomp(Omega, scale. = TRUE)
  style <- plot_diagnostics_style("integrated", "omega_pca")

  plot_base(plot = "omega_pca") +
    make_diagnostics_point(
      ggplot2::aes(x = pca$x[, 1], y = pca$x[, 2]),
      style
    ) +
    ggplot2::labs(
      title = "Omega-Hat PCA Scatter",
      x = "PC1",
      y = "PC2"
    )
}

#' Plot integrated log-likelihood omega-hat branches
#'
#' @keywords internal
#' @noRd
build_integrated_omega_branch_plot <- function(branch_mat, psi_vals) {
  style <- plot_diagnostics_style("integrated", "omega_branches")

  # Shift each branch relative to its max
  branch_mat_shifted <- t(apply(branch_mat, 2, function(col) {
    col - max(col, na.rm = TRUE)
  }))

  psi_grid <- seq(
    min(psi_vals, na.rm = TRUE),
    max(psi_vals, na.rm = TRUE),
    length.out = 10
  )

  df <- as.data.frame(branch_mat_shifted)
  colnames(df) <- psi_vals
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- df |>
    tidyr::pivot_longer(-CurveID, names_to = "psi", values_to = "value") |>
    dplyr::mutate(psi = as.numeric(psi))

  p <- plot_base(plot = "omega_branches")

  if (isTRUE(style$color_by_curve)) {
    p <- p +
      ggplot2::geom_line(
        data = df_long,
        ggplot2::aes(x = psi, y = value, group = CurveID, color = CurveID),
        linewidth = style$line$linewidth,
        alpha = style$line$alpha
      )
  } else {
    p <- p +
      ggplot2::geom_line(
        data = df_long,
        ggplot2::aes(x = psi, y = value, group = CurveID),
        color = style$line$color,
        linewidth = style$line$linewidth,
        alpha = style$line$alpha
      )
  }

  p +
    ggplot2::scale_x_continuous(
      breaks = psi_grid,
      labels = round(psi_grid, 2)
    ) +
    ggplot2::labs(
      title = "Integrated Log-Likelihood Branches",
      x = "\u03C8",
      y = expression("log L(" * psi * ")")
    ) +
    ggplot2::theme(legend.position = "none")
}

#' Plot branch value distribution at selected psi locations
#'
#' @description
#' Shows the marginal distribution of branch log-likelihood values at
#' evenly spaced ψ locations via a strip plot with horizontal jitter.
#' Values are shown on the raw log-likelihood scale.
#'
#' @param branch_mat Numeric matrix of log-likelihood evaluations.
#'   Rows correspond to ψ grid points, columns to Monte Carlo draws.
#' @param psi_vals   Numeric vector of ψ grid values.
#'
#' @return A ggplot object, or \code{NULL} if inputs are missing.
#'
#' @keywords internal
#' @noRd
build_integrated_branch_distribution_plot <- function(branch_mat, psi_vals) {
  if (is.null(psi_vals) || is.null(branch_mat)) {
    return(NULL)
  }

  n_psi <- nrow(branch_mat)
  n_show <- min(30L, n_psi)
  idx <- round(seq(1, n_psi, length.out = n_show))

  df <- purrr::map_dfr(idx, \(i) {
    tibble::tibble(psi = psi_vals[i], loglik = branch_mat[i, ])
  })

  jitter_width <- diff(range(psi_vals[idx])) / n_show * 0.35
  style <- plot_diagnostics_style("integrated", "branch_distribution")

  plot_base(plot = "branch_distribution") +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(x = psi, y = loglik),
      color = style$point$color,
      size = style$point$size,
      alpha = style$point$alpha,
      shape = style$point$shape,
      position = ggplot2::position_jitter(width = jitter_width, height = 0)
    ) +
    ggplot2::labs(
      title = "Branch Value Distribution",
      x = "\u03c8",
      y = expression("log L(" * psi * ")")
    )
}

#' Plot Mahalanobis distance distribution of omega-hat draws
#'
#' @description
#' Computes the Mahalanobis distance of each ω̂ from the sample centroid
#' and plots the distribution as a histogram with a theoretical χ²(d)
#' density overlay, where d is the dimension of ω̂. Well-spread draws
#' that fill the sampling space should roughly track the χ²(d) reference;
#' clustering or poor coverage appears as a distribution concentrated
#' near zero, while boundary-hugging appears as a heavy right tail.
#'
#' @param Omega Numeric matrix of stacked ω̂ draws (R × d).
#'
#' @return A ggplot object, or \code{NULL} if \code{Omega} is \code{NULL}
#'   or has fewer than 2 columns.
#'
#' @keywords internal
#' @noRd
build_integrated_omega_mahalanobis_plot <- function(Omega) {
  if (is.null(Omega) || ncol(Omega) < 2) {
    return(NULL)
  }

  R <- nrow(Omega)
  d <- ncol(Omega)

  mu <- colMeans(Omega)
  Z <- sweep(Omega, 2, mu)
  S <- crossprod(Z) / max(1, R - 1)
  S_reg <- S + diag(1e-8, d)

  mahal <- tryCatch(
    mahalanobis(Omega, center = mu, cov = S_reg),
    error = function(e) NULL
  )

  if (is.null(mahal)) {
    return(NULL)
  }

  style <- plot_diagnostics_style("integrated", "mahalanobis")
  x_max <- max(stats::qchisq(0.999, df = d), max(mahal, na.rm = TRUE))
  x_seq <- seq(0, x_max, length.out = 300)

  ref_df <- tibble::tibble(x = x_seq, density = stats::dchisq(x_seq, df = d))
  hist_df <- tibble::tibble(mahal = mahal)

  plot_base(plot = "mahalanobis") +
    ggplot2::geom_histogram(
      data = hist_df,
      ggplot2::aes(x = mahal, y = ggplot2::after_stat(density)),
      fill = style$hist$fill,
      color = style$hist$color,
      alpha = style$hist$alpha,
      bins = style$hist$bins
    ) +
    ggplot2::geom_line(
      data = ref_df,
      ggplot2::aes(x = x, y = density),
      color = style$ref$color,
      linewidth = style$ref$linewidth,
      linetype = style$ref$linetype
    ) +
    ggplot2::labs(
      title = "Omega-Hat Mahalanobis Distance",
      subtitle = paste0("\u03c7\u00b2(", d, ") reference"),
      x = "Mahalanobis distance",
      y = "Density"
    )
}

# ================================================================================
# END likelihood-diagnose-integrated-plots.R
# ================================================================================
