# ================================================================================
# likelihood-diagnose-plots.R
# Diagnostics plot construction helpers
# ================================================================================

#' Build diagnostics plots
#'
#' @description
#' Constructs diagnostic plots from a diagnostics object.
#'
#' Plots are **created once at diagnose-time** and stored on the diagnostics
#' object. The `plot.diagnostics()` S3 method simply retrieves these plots.
#'
#' This function performs **no printing and no dispatch**.
#'
#' @param diag A diagnostics list produced by a likelihood diagnostics engine.
#'
#' @return A named list of ggplot objects (possibly empty).
#'
#' @keywords internal
build_diagnostics_plots <- function(diag) {

  if (!isTRUE(diag$supported))
    return(list())

  plots <- list()
  idx   <- seq_along(diag$ess)

  # ------------------------------------------------------------------
  # Effective Sample Size
  # ------------------------------------------------------------------

  plots$ess <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, ess = diag$ess),
      ggplot2::aes(x = idx, y = ess)
    ) +
    ggplot2::labs(
      title = "Effective Sample Size",
      x     = "Grid index",
      y     = "ESS"
    )

  # ------------------------------------------------------------------
  # Relative Monte Carlo Standard Error
  # ------------------------------------------------------------------

  plots$rel_se <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, rel_se = diag$rel_se),
      ggplot2::aes(x = idx, y = rel_se)
    ) +
    ggplot2::labs(
      title = "Relative Monte Carlo SE",
      x     = "Grid index",
      y     = "Relative SE"
    )

  # ------------------------------------------------------------------
  # Outlier Fraction
  # ------------------------------------------------------------------

  plots$outliers <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, outlier = diag$outlier_fraction),
      ggplot2::aes(x = idx, y = outlier)
    ) +
    ggplot2::labs(
      title = "Outlier Fraction",
      x     = "Grid index",
      y     = "Fraction"
    )

  # ------------------------------------------------------------------
  # Omega-hat covariance eigenvalues
  # ------------------------------------------------------------------

  omega_diag <- diag$omega_dispersion

  if (!is.null(omega_diag) &&
      !is.null(omega_diag$covariance_eigenvalues)) {

    eig <- omega_diag$covariance_eigenvalues

    plots$omega_eigenvalues <- plot_base() +
      ggplot2::geom_point(
        data = tibble::tibble(k = seq_along(eig), eig = eig),
        ggplot2::aes(x = k, y = eig)
      ) +
      ggplot2::geom_line(
        data = tibble::tibble(k = seq_along(eig), eig = eig),
        ggplot2::aes(x = k, y = eig)
      ) +
      ggplot2::labs(
        title = "Omega-Hat Covariance Eigenvalues",
        x     = "Index",
        y     = "Eigenvalue"
      )
  }

  # ------------------------------------------------------------------
  # Omega-hat PCA scatter
  # ------------------------------------------------------------------

  Omega <- attr(diag, "omega_matrix", exact = TRUE)

  if (!is.null(Omega) && ncol(Omega) >= 2) {

    pca <- stats::prcomp(Omega, scale. = TRUE)

    plots$omega_pca <- plot_base() +
      ggplot2::geom_point(
        data = tibble::tibble(
          PC1 = pca$x[, 1],
          PC2 = pca$x[, 2]
        ),
        ggplot2::aes(x = PC1, y = PC2)
      ) +
      ggplot2::labs(
        title = "Omega-Hat PCA Scatter",
        x     = "PC1",
        y     = "PC2"
      )
  }

  plots
}

# ================================================================================
# END likelihood-diagnose-plots.R
# ================================================================================
