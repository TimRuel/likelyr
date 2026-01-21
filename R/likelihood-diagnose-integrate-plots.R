# ================================================================================
# likelihood-diagnose-integrate-plots.R
# Plot helpers for integrated likelihood diagnostics
# ================================================================================

#' Plot effective sample size (ESS) across the ψ-grid
#'
#' @description
#' Constructs a diagnostic line plot showing the effective sample size
#' (ESS) at each grid index used in the integrated likelihood
#' computation.
#'
#' @param ess Numeric vector of effective sample sizes.
#'
#' @return A ggplot object visualizing ESS over grid indices.
#'
#' @keywords internal
#' @noRd
build_integrate_ess_plot <- function(ess) {
  idx <- seq_along(ess)
  style <- plot_diagnostics_style("integrate", "ess")

  plot_base(plot = "ess") +
    make_diagnostics_line(
      ggplot2::aes(x = idx, y = ess),
      style
    ) +
    ggplot2::labs(
      title = "Effective Sample Size",
      x = "Grid index",
      y = "ESS"
    )
}

#' Plot relative Monte Carlo standard error
#'
#' @description
#' Builds a diagnostic plot showing the relative Monte Carlo
#' standard error at each grid index for integrated likelihood
#' estimation.
#'
#' @param rel_se Numeric vector of relative standard errors.
#'
#' @return A ggplot object visualizing relative SE across the grid.
#'
#' @keywords internal
#' @noRd
build_integrate_rel_se_plot <- function(rel_se) {
  idx <- seq_along(rel_se)
  style <- plot_diagnostics_style("integrate", "rel_se")

  plot_base(plot = "rel_se") +
    make_diagnostics_line(
      ggplot2::aes(x = idx, y = rel_se),
      style
    ) +
    ggplot2::labs(
      title = "Relative Monte Carlo SE",
      x = "Grid index",
      y = "Relative SE"
    )
}

#' Plot outlier fraction across the ψ-grid
#'
#' @description
#' Generates a diagnostic plot displaying the fraction of Monte Carlo
#' samples classified as outliers at each grid index.
#'
#' @param outlier_fraction Numeric vector of outlier fractions.
#'
#' @return A ggplot object visualizing outlier fractions.
#'
#' @keywords internal
#' @noRd
build_integrate_outlier_plot <- function(outlier_fraction) {
  idx <- seq_along(outlier_fraction)
  style <- plot_diagnostics_style("integrate", "outliers")

  plot_base(plot = "outliers") +
    make_diagnostics_line(
      ggplot2::aes(x = idx, y = outlier_fraction),
      style
    ) +
    ggplot2::labs(
      title = "Outlier Fraction",
      x = "Grid index",
      y = "Fraction"
    )
}

#' Plot omega-hat covariance eigenvalues
#'
#' @description
#' Visualizes the eigenvalues of the omega-hat covariance matrix,
#' providing insight into effective dimensionality and conditioning.
#'
#' @param eigenvalues Numeric vector of covariance eigenvalues.
#'
#' @return A ggplot object, or \code{NULL} if no eigenvalues supplied.
#'
#' @keywords internal
#' @noRd
build_integrate_omega_eigen_plot <- function(eigenvalues) {
  if (is.null(eigenvalues)) {
    return(NULL)
  }

  k <- seq_along(eigenvalues)
  style <- plot_diagnostics_style("integrate", "omega_eigen")

  plot_base(plot = "omega_eigen") +
    make_diagnostics_point(
      ggplot2::aes(x = k, y = eigenvalues),
      style
    ) +
    make_diagnostics_line(
      ggplot2::aes(x = k, y = eigenvalues),
      style
    ) +
    ggplot2::labs(
      title = "Omega-Hat Covariance Eigenvalues",
      x = "Index",
      y = "Eigenvalue"
    )
}

#' Plot omega-hat PCA scatter
#'
#' @description
#' Performs PCA on the omega-hat samples and visualizes the first two
#' principal components to assess structure and dispersion.
#'
#' @param Omega Numeric matrix of omega-hat samples.
#'
#' @return A ggplot object, or \code{NULL} if PCA is not applicable.
#'
#' @keywords internal
#' @noRd
build_integrate_omega_pca_plot <- function(Omega) {
  if (is.null(Omega) || ncol(Omega) < 2) {
    return(NULL)
  }

  pca <- stats::prcomp(Omega, scale. = TRUE)
  style <- plot_diagnostics_style("integrate", "omega_pca")

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
#' @description
#' Visualizes Monte Carlo log-likelihood branches across ψ values.
#' Each branch is shifted by its maximum to emphasize relative shape.
#'
#' @param branch_mat Numeric matrix of log-likelihood branches.
#' @param psi_vals Numeric vector of ψ grid values.
#'
#' @return A ggplot object showing branch trajectories.
#'
#' @keywords internal
#' @noRd
build_integrate_omega_branch_plot <- function(branch_mat, psi_vals) {
  style <- plot_diagnostics_style("integrate", "omega_branches")

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
    tidyr::pivot_longer(
      -CurveID,
      names_to = "psi",
      values_to = "value"
    ) |>
    dplyr::mutate(psi = as.numeric(psi))

  p <- plot_base(plot = "omega_branches")

  if (isTRUE(style$color_by_curve)) {
    p <- p +
      ggplot2::geom_line(
        data = df_long,
        ggplot2::aes(
          x = psi,
          y = value,
          group = CurveID,
          color = CurveID
        ),
        linewidth = style$line$linewidth,
        alpha = style$line$alpha
      )
  } else {
    p <- p +
      ggplot2::geom_line(
        data = df_long,
        ggplot2::aes(
          x = psi,
          y = value,
          group = CurveID
        ),
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
    )
}

# ================================================================================
# END
# ================================================================================
