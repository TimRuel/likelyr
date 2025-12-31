# =========================================================================
# compare-render.R — Rendering helpers for pseudo-log-likelihood comparison
# =========================================================================

#' HTML caption wrapper for comparison tables
#'
#' @description
#' Internal helper that wraps a table caption string in styled HTML
#' suitable for dark-themed `kableExtra` tables.
#'
#' @param text Character scalar giving the caption text.
#'
#' @return A length-1 character string containing HTML.
#'
#' @keywords internal
#' @noRd
.table_caption <- function(text) {
  paste0(
    "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
    text, "<br>",
    "</span>"
  )
}

#' Apply standard header styling to a kable
#'
#' @description
#' Applies consistent background color, text color, and bold formatting
#' to the header row (row 0) of a `kableExtra` table.
#'
#' @param tbl A `kableExtra` table object.
#'
#' @return A modified `kableExtra` object.
#'
#' @keywords internal
#' @noRd
.apply_standard_headers <- function(tbl) {
  tbl |>
    kableExtra::row_spec(
      0,
      background = table_accent("header_row"),
      color      = table_text_header("column"),
      bold       = TRUE
    )
}

#' Apply row-wise striping using a background vector
#'
#' @description
#' Iteratively applies row background colors using `row_spec()`,
#' allowing non-alternating, semantically driven striping.
#'
#' @param tbl A `kableExtra` table object.
#' @param bg_vec Character vector of background colors, one per row.
#'
#' @return A modified `kableExtra` object.
#'
#' @keywords internal
#' @noRd
.apply_row_striping <- function(tbl, bg_vec) {
  Reduce(
    f = function(acc, i) {
      kableExtra::row_spec(acc, i, background = bg_vec[i])
    },
    x    = seq_along(bg_vec),
    init = tbl
  )
}

#' Compute background colors by confidence level blocks
#'
#' @description
#' Alternates background colors across blocks defined by distinct
#' confidence levels. Used to visually separate interval rows.
#'
#' @param levels Vector of confidence level labels.
#'
#' @return A character vector of background colors.
#'
#' @keywords internal
#' @noRd
.interval_level_bg <- function(levels) {
  idx <- match(levels, unique(levels))
  ifelse(
    idx %% 2L == 1L,
    table_row_block_bg(1, "level"),
    table_row_block_bg(2, "level")
  )
}

#' Compute background colors by pseudolikelihood
#'
#' @description
#' Maps pseudolikelihood identifiers to row background colors.
#'
#' @param pseudolikelihood Character vector identifying likelihood type.
#'
#' @return A character vector of background colors.
#'
#' @keywords internal
#' @noRd
.pe_row_bg <- function(pseudolikelihood) {
  table_pe_row_bg(pseudolikelihood)
}

#' Vertically center table cell contents
#'
#' @description
#' Applies CSS-based vertical centering to all table rows.
#'
#' @param tbl A `kableExtra` table object.
#' @param n_rows Number of body rows in the table.
#'
#' @return A modified `kableExtra` object.
#'
#' @keywords internal
#' @noRd
.apply_vertical_centering <- function(tbl, n_rows) {
  kableExtra::row_spec(
    tbl,
    row       = seq_len(n_rows),
    extra_css = "vertical-align: middle;"
  )
}

# ---------------------------------------------------------------------
# Point estimate comparison table
# ---------------------------------------------------------------------

#' Render point estimate comparison table
#'
#' @description
#' Renders a formatted comparison table of point estimates and associated
#' uncertainty measures (error and standard error) across pseudolikelihoods.
#'
#' @details
#' The input data frame is expected to contain one row per pseudolikelihood.
#' Missing values are rendered as em dashes for visual clarity.
#'
#' Required columns:
#' \itemize{
#'   \item \code{psi_0}
#'   \item \code{psi_hat}
#'   \item \code{error}
#'   \item \code{se_psi_hat}
#'   \item \code{pseudolikelihood}
#' }
#'
#' @param point_estimates_df A data frame of point estimates and diagnostics.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @export
render_point_estimates_comparison_table <- function(point_estimates_df) {

  df_render <- point_estimates_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ tidyr::replace_na(as.character(.), "—")
      )
    )

  tbl <- df_render |>
    kableExtra::kbl(
      col.names = c(
        "$\\psi_0$",
        "$\\hat{\\psi}$",
        "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
        "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
        "Pseudolikelihood"
      ),
      caption = .table_caption(
        "Point Estimates and Uncertainty Measures"
      ),
      escape = FALSE,
      align  = "c"
    ) |>
    kableExtra::add_header_above(
      c("Truth" = 1, "Estimate" = 4),
      bold       = TRUE,
      background = table_accent("group_row"),
      color      = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>

    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>

    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(2, color = table_text_body("psi_hat"), bold = TRUE) |>
    kableExtra::column_spec(3, color = table_text_body("error"), bold = TRUE)   |>
    kableExtra::column_spec(4, color = table_text_body("se"), bold = TRUE)      |>
    kableExtra::column_spec(5, color = table_text_body("pseudolikelihood"), bold = TRUE)

  bg <- .pe_row_bg(df_render$pseudolikelihood)

  tbl |>
    .apply_row_striping(bg) |>
    kableExtra::column_spec(1, background = table_column_bg("truth")) |>
    kableExtra::collapse_rows(1)
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
#' @details
#' The input data frame must include specific attributes used for rendering:
#'
#' \describe{
#'   \item{\code{interval_estimates_raw}}{Data frame containing raw lower and upper bounds.}
#'   \item{\code{point_estimates}}{Numeric vector of point estimates aligned with rows.}
#'   \item{\code{psi_0}}{Scalar true parameter value.}
#' }
#'
#' Required columns:
#' \itemize{
#'   \item \code{Level}
#'   \item \code{Pseudolikelihood}
#'   \item \code{Interval}
#'   \item \code{Length}
#'   \item \code{Lower Deviation}
#'   \item \code{Upper Deviation}
#'   \item \code{Status}
#' }
#'
#' Interval diagrams are rendered inline using `spec_pointrange()`.
#'
#' @param interval_estimates_df A formatted interval estimates data frame
#'   with required attributes attached.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @export
render_interval_estimates_comparison_table <- function(interval_estimates_df) {

  interval_estimates_raw <- attr(interval_estimates_df, "interval_estimates_raw")
  point_estimates        <- attr(interval_estimates_df, "point_estimates")
  psi_0                  <- attr(interval_estimates_df, "psi_0")

  required <- c(
    "Level", "Pseudolikelihood", "Interval", "Length",
    "Lower Deviation", "Upper Deviation", "Status"
  )
  stopifnot(all(required %in% names(interval_estimates_df)))

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

  tbl <- df_render |>
    kableExtra::kbl(
      col.names = c(
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
      caption = .table_caption(
        "Interval Estimates and Uncertainty Measures"
      ),
      escape = FALSE,
      align  = "c"
    ) |>

    kableExtra::add_header_above(
      c("Truth" = 1, "Estimate" = 8),
      bold       = TRUE,
      background = table_accent("group_row"),
      color      = table_text_header("group")
    ) |>

    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>

    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>

    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(2, color = table_text_body("interval"), bold = TRUE) |>
    kableExtra::column_spec(3, color = table_text_body("pseudolikelihood"), bold = TRUE) |>
    kableExtra::column_spec(
      4,
      image = kableExtra::spec_pointrange(
        x     = rep(point_estimates, nrow(interval_estimates_raw) / 2),
        xmin  = interval_estimates_raw$lower,
        xmax  = interval_estimates_raw$upper,
        vline = psi_0,
        line_col = table_text_body("diagram"),
        width  = 300,
        height = 150,
        cex    = 0.6
      )
    ) |>
    kableExtra::column_spec(5, color = table_text_body("length"), bold = TRUE) |>
    kableExtra::column_spec(6, color = table_text_body("lower_dev"), bold = TRUE) |>
    kableExtra::column_spec(7, color = table_text_body("upper_dev"), bold = TRUE) |>
    kableExtra::column_spec(9, color = table_text_body("level"), bold = TRUE)

  tbl |>
    .apply_row_striping(bg) |>
    kableExtra::column_spec(1, background = table_column_bg("truth")) |>
    kableExtra::collapse_rows(c(1, 9))
}

# ---------------------------------------------------------------------
# Combined inference table
# ---------------------------------------------------------------------

#' Render combined point and interval comparison table
#'
#' @description
#' Renders a unified comparison table containing both point estimates and
#' interval estimates for multiple pseudolikelihood methods.
#'
#' @details
#' This table aligns point estimates with their corresponding confidence
#' intervals and uses row collapsing and hidden HTML markers to preserve
#' grouping by pseudolikelihood.
#'
#' @param estimates_df Data frame combining point and interval estimates
#'   across pseudolikelihoods and confidence levels.
#'
#' @return A `kableExtra` HTML table object.
#'
#' @family inference-renderers
#' @export
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
  bg_pe       <- .pe_row_bg(df_render$pseudolikelihood)

  tbl <- df_render |>
    kableExtra::kbl(
      booktabs = TRUE,
      escape   = FALSE,
      align    = "c",
      col.names = c(
        "Pseudo- <br/>Likelihood",
        "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
        "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
        "$\\hat{\\psi}$",
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
      caption = .table_caption(
        "Pseudolikelihood Estimates and Uncertainty Measures"
      )
    ) |>

    kableExtra::add_header_above(
      c(
        "Point Estimates"    = 4,
        "Truth"              = 1,
        "Interval Estimates" = 8
      ),
      bold       = TRUE,
      background = c(
        table_group_header_bg("point_estimate"),
        table_group_header_bg("truth"),
        table_group_header_bg("interval_estimate")
      ),
      color = table_text_header("group")
    ) |>

    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>

    kableExtra::column_spec(
      1:4,
      background = table_column_header_bg("point_estimate"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      5,
      background = table_column_header_bg("truth"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      6:13,
      background = table_column_header_bg("interval_estimate"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>

    kableExtra::column_spec(1,  color = table_text_body("pseudolikelihood")) |>
    kableExtra::column_spec(2,  color = table_text_body("se")) |>
    kableExtra::column_spec(3,  color = table_text_body("error")) |>
    kableExtra::column_spec(4,  color = table_text_body("psi_hat")) |>
    kableExtra::column_spec(5,  color = table_text_body("psi_0")) |>
    kableExtra::column_spec(6,  color = table_text_body("interval")) |>
    kableExtra::column_spec(7,  color = table_text_body("pseudolikelihood")) |>
    kableExtra::column_spec(
      8,
      image = kableExtra::spec_pointrange(
        x     = estimates_df$psi_hat,
        xmin  = estimates_df$psi_hat - estimates_df$`Lower Deviation`,
        xmax  = estimates_df$psi_hat + estimates_df$`Upper Deviation`,
        vline = psi_0,
        line_col = table_text_body("diagram"),
        width  = 300,
        height = 150,
        cex    = 0.6
      )
    ) |>
    kableExtra::column_spec(9,  color = table_text_body("length")) |>
    kableExtra::column_spec(10, color = table_text_body("lower_dev")) |>
    kableExtra::column_spec(11, color = table_text_body("upper_dev")) |>
    kableExtra::column_spec(13, color = table_text_body("level")) |>

    kableExtra::kable_styling(full_width = FALSE, position = "center")

  tbl |>
    .apply_row_striping(bg_interval) |>
    .apply_vertical_centering(n_rows = nrow(df_render)) |>
    kableExtra::column_spec(1:4, background = bg_pe) |>
    kableExtra::column_spec(5, background = table_column_bg("truth")) |>
    kableExtra::collapse_rows(columns = c(1:5, 13))
}
