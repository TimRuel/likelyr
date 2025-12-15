# =====================================================================
# plot-branches.R
# Branch Visualization Utilities
# =====================================================================

#' Plot Integrated Log-Likelihood Branches
#'
#' @param mat Branch matrix.
#' @param crit Critical value for horizontal cutoff line.
#'
#' @return A ggplot object.
#' @export
plot_branches <- function(x) {

  mat <- x$branch_mat
  df  <- x$df

  # Shift each BRANCH relative to its maximum (correct: apply over rows)
  mat_shifted <- t(apply(mat, 2, function(col) col - max(col, na.rm = TRUE)))

  psi_vals <- as.numeric(df$psi)

  psi_grid <- seq(
    min(psi_vals, na.rm = TRUE),
    max(psi_vals, na.rm = TRUE),
    length.out = 10
  )

  df <- as.data.frame(mat_shifted)
  colnames(df) <- psi_vals
  rownames(df) <- NULL
  df$CurveID <- paste0("Curve_", seq_len(nrow(df)))

  df_long <- df |>
    tidyr::pivot_longer(-CurveID, names_to = "psi", values_to = "value") |>
    dplyr::mutate(psi = as.numeric(psi))   # <-- CRITICAL FIX

  plot_base() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_line(
      data = df_long,
      ggplot2::aes(x = psi, y = value, group = CurveID, color = CurveID),
      linewidth = 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = psi_grid,
      labels = round(psi_grid, 2)
    ) +
    ggplot2::labs(
      title = "Integrated Log-Likelihood Branches",
      x = "\u03C8",
      y = expression("log L("*psi*")")
    )
}

