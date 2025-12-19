# =====================================================================
# plot-theme.R
# Base Theme for Log-Likelihood Plots
# =====================================================================

#' Base Dark Theme for Log-Likelihood Plots
#'
#' @return A ggplot2 object representing the base plotting surface.
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
