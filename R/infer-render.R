# =====================================================================
# infer-render.R — Rendering helpers for likelihood inference (local-only)
# =====================================================================

# ---------------------------------------------------------------------
# Point estimate table
# ---------------------------------------------------------------------

#' Render point estimate table
#'
#' @keywords internal
#' @noRd
render_point_estimate_table <- function(point_estimate_df) {
  required <- c("psi_0", "psi_hat", "error", "se_psi_hat")
  stopifnot(all(required %in% names(point_estimate_df)))

  pseudolikelihood <- attr(point_estimate_df, "pseudolikelihood", exact = TRUE)

  .render_point_estimate_base(
    df = point_estimate_df,
    col_names = c(
      "$\\psi_0$",
      "$\\hat{\\psi}$",
      "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
      "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$"
    ),
    header_groups = c("Truth" = 1, "Estimate" = 3),
    caption = .table_caption(
      "Point Estimate and Uncertainty Measures",
      pseudolikelihood
    )
  )
}

# ---------------------------------------------------------------------
# Interval estimate table
# ---------------------------------------------------------------------

#' Render confidence interval table
#'
#' @keywords internal
#' @noRd
render_interval_estimate_table <- function(interval_estimate_df) {
  required <- c(
    "interval",
    "length",
    "lower_dev",
    "upper_dev",
    "contains_truth",
    "level"
  )
  stopifnot(all(required %in% names(interval_estimate_df)))

  pseudolikelihood <- attr(
    interval_estimate_df,
    "pseudolikelihood",
    exact = TRUE
  )
  interval_estimate_raw <- attr(interval_estimate_df, "interval_estimate_raw")
  psi_hat <- attr(interval_estimate_df, "psi_hat")
  psi_0 <- attr(interval_estimate_df, "psi_0")

  stopifnot(
    !is.null(interval_estimate_raw),
    !is.null(psi_hat),
    !is.null(psi_0)
  )

  stripe_bg <- .interval_level_bg(interval_estimate_df$level)
  diagram_x <- rep(psi_hat, nrow(interval_estimate_raw))
  diagram_lower <- interval_estimate_raw$lower
  diagram_upper <- interval_estimate_raw$upper
  diagram_lim <- range(c(diagram_lower, diagram_upper, psi_0), na.rm = TRUE)

  df_render <- interval_estimate_df |>
    dplyr::mutate(psi_0 = round(psi_0, 2), .before = "interval") |>
    dplyr::mutate(diagram = "", .after = "interval") |>
    dplyr::mutate(
      contains_truth = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth ~ "\u2705",
        TRUE ~ "\u274c"
      )
    )

  .render_interval_estimate_base(
    df = df_render,
    col_names = c(
      "$\\psi_0$",
      "Interval",
      "Diagram",
      "Length",
      "Lower<br/>Deviation",
      "Upper<br/>Deviation",
      "Covers $\\psi_0$",
      "Confidence<br/>Level"
    ),
    header_groups = c("Truth" = 1, "Estimate" = 7),
    caption = .table_caption(
      "Interval Estimates and Uncertainty Measures",
      pseudolikelihood
    ),
    stripe_bg = stripe_bg,
    diagram_x = diagram_x,
    diagram_lower = diagram_lower,
    diagram_upper = diagram_upper,
    diagram_lim = diagram_lim,
    vline = psi_0,
    include_pl = FALSE,
    collapse_cols = 1
  )
}

# ---------------------------------------------------------------------
# Combined estimate table
# ---------------------------------------------------------------------

#' Render combined estimate table
#'
#' @keywords internal
#' @noRd
render_estimate_table <- function(point_estimate_df, interval_estimate_df) {
  required_point <- c("se_psi_hat", "error", "psi_hat", "psi_0")
  required_interval <- c(
    "interval",
    "length",
    "lower_dev",
    "upper_dev",
    "contains_truth",
    "level"
  )

  stopifnot(all(required_point %in% names(point_estimate_df)))
  stopifnot(all(required_interval %in% names(interval_estimate_df)))

  pseudolikelihood <- attr(point_estimate_df, "pseudolikelihood", exact = TRUE)
  interval_estimate_raw <- attr(interval_estimate_df, "interval_estimate_raw")
  stopifnot(!is.null(interval_estimate_raw))

  psi_0 <- unique(point_estimate_df$psi_0)
  stopifnot(length(psi_0) == 1)

  estimate_df <- point_estimate_df |>
    dplyr::bind_cols(interval_estimate_df) |>
    dplyr::select(
      se_psi_hat,
      error,
      psi_hat,
      psi_0,
      interval,
      length,
      lower_dev,
      upper_dev,
      contains_truth,
      level
    )

  df_render <- estimate_df |>
    dplyr::mutate(
      dplyr::across(
        c(se_psi_hat, error, psi_hat, psi_0, length, lower_dev, upper_dev),
        ~ round(.x, 2)
      )
    ) |>
    dplyr::mutate(diagram = "", .after = "interval") |>
    dplyr::mutate(
      contains_truth = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth ~ "\u2705",
        TRUE ~ "\u274c"
      )
    )

  bg_interval <- .interval_level_bg(df_render$level)
  bg_pe <- table_pe_row_bg(pseudolikelihood)
  diagram_x_raw <- estimate_df$psi_hat
  diagram_lower_raw <- interval_estimate_raw$lower
  diagram_upper_raw <- interval_estimate_raw$upper
  diagram_lim <- range(
    c(diagram_lower_raw, diagram_upper_raw, psi_0),
    na.rm = TRUE
  )

  body_spec_fun <- function(tbl) {
    tbl |>
      kableExtra::column_spec(1, color = table_text_body("se")) |>
      kableExtra::column_spec(2, color = table_text_body("error")) |>
      kableExtra::column_spec(3, color = table_text_body("psi_hat")) |>
      kableExtra::column_spec(4, color = table_text_body("psi_0")) |>
      kableExtra::column_spec(5, color = table_text_body("interval")) |>
      kableExtra::column_spec(
        6,
        image = kableExtra::spec_pointrange(
          x = diagram_x_raw,
          xmin = diagram_lower_raw,
          xmax = diagram_upper_raw,
          vline = psi_0,
          lim = diagram_lim,
          line_col = table_text_body("diagram"),
          width = 300,
          height = 150,
          cex = 0.6
        )
      ) |>
      kableExtra::column_spec(7, color = table_text_body("length")) |>
      kableExtra::column_spec(8, color = table_text_body("lower_dev")) |>
      kableExtra::column_spec(9, color = table_text_body("upper_dev")) |>
      kableExtra::column_spec(11, color = table_text_body("level"))
  }

  .render_estimates_base(
    df_render = df_render,
    caption = .table_caption(
      "Estimates and Uncertainty Measures",
      pseudolikelihood
    ),
    header_groups = c(
      "Point Estimates" = 3,
      "Truth" = 1,
      "Interval Estimates" = 7
    ),
    header_bg = c(
      table_group_header_bg("point_estimate"),
      table_group_header_bg("truth"),
      table_group_header_bg("interval_estimate")
    ),
    header_cols = list(point = 1:3, truth = 4, interval = 5:11),
    body_spec_fun = body_spec_fun,
    stripe_bg = bg_interval,
    collapse_cols = 1:4,
    include_pl = FALSE,
    pe_bg = bg_pe
  )
}

# =====================================================================
# END infer-render.R
# =====================================================================
