# ============================================================================
# compare-render.R — Rendering functions for pseudo-log-likelihood comparison
# (local-only)
# ============================================================================

# ---------------------------------------------------------------------
# Point estimate comparison table
# ---------------------------------------------------------------------

#' Render point estimate comparison table
#'
#' @description
#' Renders a formatted comparison table of point estimates and associated
#' uncertainty measures (error and standard error) across pseudolikelihoods.
#'
#' This function is **local-only** and must not be called on HPC.
#'
#' @param point_estimates_df A data frame of point estimates and diagnostics.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @keywords internal
render_point_estimates_comparison_table <- function(point_estimates_df) {
  required <- c("psi_0", "psi_hat", "error", "se_psi_hat", "pseudolikelihood")
  stopifnot(all(required %in% names(point_estimates_df)))

  bg <- .pe_row_bg(point_estimates_df$pseudolikelihood)

  .render_point_estimate_base(
    df = point_estimates_df,
    col_names = c(
      "$\\psi_0$",
      "$\\hat{\\psi}$",
      "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
      "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
      "Pseudolikelihood"
    ),
    header_groups = c("Truth" = 1, "Estimate" = 4),
    caption = .table_caption(
      "Point Estimates and Uncertainty Measures"
    ),
    stripe_bg = bg,
    collapse_truth = TRUE
  )
}

# ---------------------------------------------------------------------
# Interval estimate comparison table
# ---------------------------------------------------------------------

#' Render interval estimate comparison table
#'
#' @description
#' Renders a comparison table of confidence intervals and associated
#' diagnostics across pseudolikelihoods and confidence levels.
#'
#' This function is **local-only** and must not be called on HPC.
#'
#' @param interval_estimates_df A formatted interval estimates data frame
#'   with required attributes attached.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @keywords internal
render_interval_estimates_comparison_table <- function(interval_estimates_df) {
  required <- c(
    "Level",
    "Pseudolikelihood",
    "Interval",
    "Length",
    "Lower Deviation",
    "Upper Deviation",
    "Status"
  )
  stopifnot(all(required %in% names(interval_estimates_df)))

  interval_estimates_raw <- attr(
    interval_estimates_df,
    "interval_estimates_raw"
  )
  point_estimates <- attr(interval_estimates_df, "point_estimates")
  psi_0 <- attr(interval_estimates_df, "psi_0")

  stopifnot(
    !is.null(interval_estimates_raw),
    !is.null(point_estimates),
    !is.null(psi_0)
  )

  df_render <- interval_estimates_df |>
    dplyr::mutate(
      psi_0 = round(psi_0, 2),
      .before = Interval
    ) |>
    dplyr::mutate(
      Diagram = "",
      .after = Pseudolikelihood
    )

  bg <- .interval_level_bg(df_render$Level)

  .render_interval_estimate_base(
    df = df_render,
    col_names = c(
      "$\\psi_0$",
      "Interval",
      "Pseudo- <br/>Likelihood",
      "Diagram",
      "Length",
      "Lower<br/>Deviation",
      "Upper<br/>Deviation",
      "Covers $\\psi_0$",
      "Confidence<br/>Level"
    ),
    header_groups = c("Truth" = 1, "Estimate" = 8),
    caption = .table_caption(
      "Interval Estimates and Uncertainty Measures"
    ),
    stripe_bg = bg,
    diagram_x = rep(point_estimates, nrow(interval_estimates_raw) / 2),
    diagram_lower = interval_estimates_raw$lower,
    diagram_upper = interval_estimates_raw$upper,
    vline = psi_0,
    include_pl = TRUE,
    collapse_cols = c(1, 9)
  )
}

# ---------------------------------------------------------------------
# Combined inference comparison table
# ---------------------------------------------------------------------

#' Render combined point and interval comparison table
#'
#' @description
#' Renders a unified comparison table containing both point estimates and
#' interval estimates for multiple pseudolikelihood methods.
#'
#' This function is **local-only** and must not be called on HPC.
#'
#' @param estimates_df Data frame combining point and interval estimates
#'   across pseudolikelihoods and confidence levels.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @keywords internal
render_estimates_comparison_table <- function(estimates_df) {
  df_render <- estimates_df |>
    dplyr::mutate(Diagram = "", .after = "Pseudolikelihood") |>
    dplyr::mutate(
      dplyr::across(
        c(psi_hat, error, se_psi_hat),
        ~ paste0(.x, "<span style='display:none'>", pseudolikelihood, "</span>")
      )
    )

  bg_interval <- .interval_level_bg(df_render$Level)
  bg_pe <- .pe_row_bg(df_render$pseudolikelihood)

  body_spec_fun <- function(tbl) {
    tbl |>
      kableExtra::column_spec(1, color = table_text_body("pseudolikelihood")) |>
      kableExtra::column_spec(2, color = table_text_body("se")) |>
      kableExtra::column_spec(3, color = table_text_body("error")) |>
      kableExtra::column_spec(4, color = table_text_body("psi_hat")) |>
      kableExtra::column_spec(5, color = table_text_body("psi_0")) |>
      kableExtra::column_spec(6, color = table_text_body("interval")) |>
      kableExtra::column_spec(7, color = table_text_body("pseudolikelihood")) |>
      kableExtra::column_spec(
        8,
        image = kableExtra::spec_pointrange(
          x = estimates_df$psi_hat,
          xmin = estimates_df$psi_hat - estimates_df$`Lower Deviation`,
          xmax = estimates_df$psi_hat + estimates_df$`Upper Deviation`,
          vline = estimates_df$psi_0,
          line_col = table_text_body("diagram"),
          width = 300,
          height = 150,
          cex = 0.6
        )
      ) |>
      kableExtra::column_spec(9, color = table_text_body("length")) |>
      kableExtra::column_spec(10, color = table_text_body("lower_dev")) |>
      kableExtra::column_spec(11, color = table_text_body("upper_dev")) |>
      kableExtra::column_spec(13, color = table_text_body("level"))
  }

  .render_estimates_base(
    df_render = df_render,
    caption = .table_caption(
      "Pseudolikelihood Estimates and Uncertainty Measures"
    ),
    header_groups = c(
      "Point Estimates" = 4,
      "Truth" = 1,
      "Interval Estimates" = 8
    ),
    header_bg = c(
      table_group_header_bg("point_estimate"),
      table_group_header_bg("truth"),
      table_group_header_bg("interval_estimate")
    ),
    header_cols = list(
      point = 1:4,
      truth = 5,
      interval = 6:13
    ),
    body_spec_fun = body_spec_fun,
    stripe_bg = bg_interval,
    collapse_cols = c(1:5, 13),
    include_pl = TRUE,
    pe_bg = bg_pe
  )
}

# ============================================================================
# END compare-render.R
# ============================================================================
