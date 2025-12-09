# =====================================================================
# plot-branches.R
# Branch Visualization Utilities
# =====================================================================

#' Plot Integrated Log-Likelihood Branches
#'
#' @param mat Matrix of log-likelihood branches.
#' @return A ggplot object.
#' @export
plot_branches <- function(mat) {
  df <- as.data.frame(mat)
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- tidyr::pivot_longer(df, -CurveID, names_to="X", values_to="Y") |>
    dplyr::mutate(X = as.numeric(X))

  plot_base() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_line(
      data = df_long,
      ggplot2::aes(x = X, y = Y, group = CurveID, color = CurveID),
      linewidth = 1
    ) +
    ggplot2::labs(
      title = "Integrated Log-Likelihood Branches",
      x = "\u03C8",
      y = expression("log L("*psi*")")
    )
}

#' Plot Shifted Integrated Log-Likelihood Branches
#'
#' @param mat Branch matrix.
#' @param crit Critical value for horizontal cutoff line.
#'
#' @return A ggplot object.
#' @export
get_branches_plot_shifted <- function(mat, crit) {

  mat_shifted <- t(apply(mat, 1, function(row) row - max(row, na.rm = TRUE)))
  psi_vals <- as.numeric(colnames(mat_shifted))

  psi_grid <- seq(
    min(psi_vals, na.rm = TRUE),
    max(psi_vals, na.rm = TRUE),
    length.out = 10
  )

  df <- as.data.frame(mat_shifted)
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- df |>
    tidyr::pivot_longer(-CurveID, names_to = "X", values_to = "Y") |>
    dplyr::mutate(X = as.numeric(X))

  plot_base() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_line(
      data = df_long,
      ggplot2::aes(x = X, y = Y, group = CurveID, color = CurveID),
      linewidth = 1
    ) +
    ggplot2::geom_hline(yintercept = -crit, color = "red") +
    ggplot2::scale_x_continuous(
      breaks = psi_grid,
      labels = round(psi_grid, 2)
    ) +
    ggplot2::labs(
      title = "Integrated Log-Likelihood Branches (shifted)",
      x = "\u03C8",
      y = expression("log L("*psi*")")
    )
}
