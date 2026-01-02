# =====================================================================
# infer-render.R — Rendering helpers for likelihood inference
# =====================================================================

# ---------------------------------------------------------------------
# Point estimate table
# ---------------------------------------------------------------------

#' Render point estimate table
#'
#' @param point_estimate_df Data frame with columns `psi_0`, `psi_hat`,
#'   and `se_psi_hat`.
#'
#' @return A kableExtra object.
#' Render point estimate table
#'
#' @description
#' Renders a formatted table of point estimates and associated uncertainty
#' measures using `kableExtra`.
#'
#' @details
#' If the input data frame has a character attribute \code{"type"}, it is used
#' to annotate the table caption (e.g., "Profile Log-Likelihood" or
#' "Integrated Log-Likelihood"). If no such attribute is present, the caption
#' is rendered without a subtitle.
#'
#' @param point_estimate_df Data frame with columns \code{psi_0},
#'   \code{psi_hat}, \code{error}, and \code{se_psi_hat}.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @export
render_point_estimate_table <- function(point_estimate_df) {

  required <- c("psi_0", "psi_hat", "error", "se_psi_hat")
  stopifnot(all(required %in% names(point_estimate_df)))

  type <- attr(point_estimate_df, "type", exact = TRUE)
  if (length(type) == 0) type <- NULL

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
      type
      )
  )
}

# ---------------------------------------------------------------------
# Interval estimate table
# ---------------------------------------------------------------------

#' Render confidence interval table
#'
#' @param interval_estimate_df Data frame produced by
#'   `get_interval_estimate_df()`.
#'
#' @return A kableExtra object.
render_interval_estimate_table <- function(interval_estimate_df) {

  required <- c(
    "Interval", "Length", "Lower Deviation",
    "Upper Deviation", "Status", "Level"
  )
  stopifnot(all(required %in% names(interval_estimate_df)))

  type <- attr(interval_estimate_df, "type", exact = TRUE)
  if (length(type) == 0) type <- NULL

  interval_estimate_raw <- attr(interval_estimate_df, "interval_estimate_raw")
  point_estimate        <- attr(interval_estimate_df, "point_estimate")
  psi_0                 <- attr(interval_estimate_df, "psi_0")

  stopifnot(
    !is.null(interval_estimate_raw),
    !is.null(point_estimate),
    !is.null(psi_0)
  )

  stripe_bg <- .interval_level_bg(interval_estimate_df$Level)

  df_render <- interval_estimate_df |>
    dplyr::mutate(
      psi_0 = round(psi_0, 2),
      .before = "Interval"
    ) |>
    dplyr::mutate(
      Diagram = "",
      .after = "Interval"
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
      type
    ),
    stripe_bg      = stripe_bg,
    diagram_x      = rep(point_estimate, nrow(interval_estimate_raw)),
    diagram_lower  = interval_estimate_raw$lower,
    diagram_upper  = interval_estimate_raw$upper,
    vline          = psi_0,
    include_pl     = FALSE,
    collapse_cols  = 1
  )
}

# ---------------------------------------------------------------------
# Combined estimate table
# ---------------------------------------------------------------------

#' Render combined estimate table
#'
#' @param estimate_df Data frame combining point and interval estimates.
#' @return A kableExtra object.
render_estimate_table <- function(estimate_df) {

  # --------------------------------------------------
  # Validate required columns
  # --------------------------------------------------
  required <- c(
    "se_psi_hat", "error", "psi_hat", "psi_0",
    "Interval", "Length", "Lower Deviation",
    "Upper Deviation", "Status", "Level"
  )
  stopifnot(all(required %in% names(estimate_df)))

  # --------------------------------------------------
  # Extract rendering metadata
  # --------------------------------------------------
  type <- attr(estimate_df, "type", exact = TRUE)
  if (length(type) == 0) type <- NULL

  interval_estimate_raw <- attr(estimate_df, "interval_estimate_raw")
  stopifnot(!is.null(interval_estimate_raw))

  psi_0 <- unique(estimate_df$psi_0)
  stopifnot(length(psi_0) == 1)

  # --------------------------------------------------
  # Prepare render data
  # --------------------------------------------------
  df_render <- estimate_df |>
    dplyr::mutate(
      dplyr::across(
        c(se_psi_hat, error, psi_hat, psi_0,
          Length, `Lower Deviation`, `Upper Deviation`),
        ~ round(.x, 2)
        )
      ) |>
    dplyr::mutate(
      Diagram = "",
      .after = "Interval"
    )

  bg_interval <- .interval_level_bg(df_render$Level)
  bg_pe       <- table_pe_row_bg(type)

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
          x     = estimate_df$psi_hat,
          xmin  = interval_estimate_raw$lower,
          xmax  = interval_estimate_raw$upper,
          vline = psi_0,
          line_col = table_text_body("diagram"),
          width  = 300,
          height = 150,
          cex    = 0.6
          )
        ) |>
      kableExtra::column_spec(7,  color = table_text_body("length")) |>
      kableExtra::column_spec(8,  color = table_text_body("lower_dev")) |>
      kableExtra::column_spec(9, color = table_text_body("upper_dev")) |>
      kableExtra::column_spec(11, color = table_text_body("level"))
  }

  # --------------------------------------------------
  # Render table via shared base
  # --------------------------------------------------
  .render_estimates_base(
    df_render = df_render,
    caption   = .table_caption(
      "Estimates and Uncertainty Measures",
      type
      ),
    header_groups = c(
      "Point Estimates"    = 3,
      "Truth"              = 1,
      "Interval Estimates" = 7
      ),
    header_bg = c(
      table_group_header_bg("point_estimate"),
      table_group_header_bg("truth"),
      table_group_header_bg("interval_estimate")
      ),
    header_cols = list(
      point    = 1:3,
      truth    = 4,
      interval = 5:11
      ),
    body_spec_fun = body_spec_fun,
    stripe_bg     = bg_interval,
    collapse_cols = 1:4,
    include_pl    = FALSE,
    pe_bg         = bg_pe
  )
}
