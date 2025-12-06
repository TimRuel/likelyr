# =====================================================================
# utils-plots.R
# Plotting utilities for log-likelihood and branch visualization
# =====================================================================

#' Base Dark Theme for Log-Likelihood Plots
#'
#' @return A ggplot object representing the base plotting surface.
#' @keywords internal
plot_base <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#444444", color = NA),
      plot.background  = ggplot2::element_rect(fill = "#2A2A2A", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#4C4C4C"),
      panel.grid.minor = ggplot2::element_line(color = "#333333"),
      axis.ticks       = ggplot2::element_line(color = "white"),
      axis.text        = ggplot2::element_text(color = "white"),
      axis.title       = ggplot2::element_text(color = "white"),
      strip.text       = ggplot2::element_text(color = "white"),
      plot.title       = ggplot2::element_text(color = "white", face = "bold"),
      legend.background = ggplot2::element_rect(fill = "#444444"),
      legend.text      = ggplot2::element_text(color = "white"),
      legend.title     = ggplot2::element_text(color = "white")
    )
}


#' Plot Log-Likelihood Curve
#'
#' @param df A data frame with columns `psi` and a likelihood column.
#'
#' @return A ggplot object.
#' @export
get_LL_plot <- function(df) {

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = psi, y = .data[[names(df)[2]]])
  ) +
    ggplot2::geom_point(color = "cyan", size = 3, alpha = 0.7) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#2E2E2E", color = NA),
      panel.background = ggplot2::element_rect(fill = "#3A3A3F", color = "#1A1A1A", linewidth = 2),
      axis.text        = ggplot2::element_text(color = "white"),
      axis.title       = ggplot2::element_text(color = "white"),
      plot.title       = ggplot2::element_text(color = "white", size = 18, face = "bold"),
      plot.caption     = ggplot2::element_text(color = "gray", size = 10),
      panel.grid       = ggplot2::element_line(color = "gray30", linetype = "dashed")
    ) +
    ggplot2::labs(
      title = paste(names(df)[[2]], "Log-Likelihood"),
      x     = "\u03C8",
      y     = expression("log L("*psi*")")
    )
}


#' Plot Integrated Log-Likelihood Branches
#'
#' @param mat A matrix of log-likelihood branches.
#'
#' @return A ggplot object.
#' @export
get_branches_plot <- function(mat) {

  df <- as.data.frame(mat)
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


#' Build ggplot stat_function layers for each pseudolikelihood
#'
#' @param pseudolikelihoods List of functions.
#' @param LL_df Data frame containing psi grid.
#'
#' @return A named list of ggplot2 stat layers.
#' @keywords internal
get_stat_fns <- function(pseudolikelihoods, LL_df) {

  psi_endpoints <- LL_df |>
    dplyr::pull(psi) |>
    (\(x) c(head(x, 1), tail(x, 1)))()

  pseudolikelihoods |>
    purrr::imap(
      \(pseudolikelihood, name) {
        ggplot2::stat_function(
          fun = pseudolikelihood,
          geom = "line",
          ggplot2::aes(color = name),
          linewidth = 1.5,
          show.legend = FALSE,
          xlim = psi_endpoints
        )
      }
    )
}


#' Colors for Likelihood-Ratio Confidence Levels
#'
#' @param crit_df Data frame containing CI labels.
#'
#' @return Named vector of colors.
#' @keywords internal
get_ci_colors <- function(crit_df) {
  labels <- crit_df$label
  n <- length(labels)

  colors <- RColorBrewer::brewer.pal(n, "Dark2")
  names(colors) <- labels

  colors
}
