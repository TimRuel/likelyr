# ============================================================================
# render-utils.R — Shared rendering helpers and base table builders
# (local-only)
# ============================================================================

# ============================================================================
# Caption
# ============================================================================

#' HTML caption wrapper for kableExtra tables
#'
#' @description
#' Constructs a styled HTML caption. Supports an optional secondary subtitle,
#' typically used to indicate pseudolikelihood type.
#'
#' @param text             Character scalar giving the primary caption title.
#' @param pseudolikelihood Optional character scalar giving a pseudolikelihood
#'   label. Accepted special values:
#'   \itemize{
#'     \item \code{"integrated"} → "Integrated Log-Likelihood"
#'     \item \code{"profile"}   → "Profile Log-Likelihood"
#'   }
#'   Other values are used verbatim.
#'
#' @return Length-1 HTML character string for `kableExtra::kbl()`.
#'
#' @keywords internal
#' @noRd
.table_caption <- function(text, pseudolikelihood = NULL) {
  subtitle <- if (!is.null(pseudolikelihood)) {
    label <- switch(
      tolower(pseudolikelihood),
      integrated = "Integrated",
      profile = "Profile",
      pseudolikelihood
    )
    paste0("<em>(", label, " Log-Likelihood)</em>")
  } else {
    ""
  }

  paste0(
    "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
    text,
    if (nzchar(subtitle)) "<br>" else "",
    subtitle,
    "</span>"
  )
}


# ============================================================================
# Row/column styling helpers
# ============================================================================

#' Apply standard header styling to a kable
#'
#' @keywords internal
#' @noRd
.apply_standard_headers <- function(tbl) {
  tbl |>
    kableExtra::row_spec(
      0,
      background = table_accent("header_row"),
      color = table_text_header("column"),
      bold = TRUE
    )
}


#' Apply row-wise striping using a background vector
#'
#' @keywords internal
#' @noRd
.apply_row_striping <- function(tbl, bg_vec) {
  Reduce(
    f = function(acc, i) kableExtra::row_spec(acc, i, background = bg_vec[i]),
    x = seq_along(bg_vec),
    init = tbl
  )
}


#' Vertically center table cell contents
#'
#' @keywords internal
#' @noRd
.apply_vertical_centering <- function(tbl, n_rows) {
  kableExtra::row_spec(
    tbl,
    row = seq_len(n_rows),
    extra_css = "vertical-align: middle;"
  )
}


# ============================================================================
# Background color helpers
# ============================================================================

#' Compute background colors by confidence level blocks
#'
#' @description
#' Alternates background colors across blocks defined by distinct confidence
#' levels. Used to visually separate interval rows.
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
#' @keywords internal
#' @noRd
.pe_row_bg <- function(pseudolikelihood) {
  table_pe_row_bg(pseudolikelihood)
}


# ============================================================================
# Base table builders
# ============================================================================

#' Render base point-estimate table
#'
#' @keywords internal
#' @noRd
.render_point_estimate_base <- function(
  df,
  col_names,
  header_groups,
  caption,
  stripe_bg = NULL,
  collapse_truth = FALSE
) {
  tbl <- df |>
    kableExtra::kbl(
      col.names = col_names,
      caption = caption,
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::add_header_above(
      header_groups,
      bold = TRUE,
      background = table_accent("group_row"),
      color = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>
    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(
      2,
      color = table_text_body("psi_hat"),
      bold = TRUE
    ) |>
    kableExtra::column_spec(3, color = table_text_body("error"), bold = TRUE) |>
    kableExtra::column_spec(4, color = table_text_body("se"), bold = TRUE)

  if (ncol(df) >= 5) {
    tbl <- tbl |>
      kableExtra::column_spec(
        5,
        color = table_text_body("pseudolikelihood"),
        bold = TRUE
      )
  }

  if (!is.null(stripe_bg)) {
    tbl <- tbl |> .apply_row_striping(stripe_bg)
  }

  tbl <- tbl |>
    kableExtra::column_spec(1, background = table_column_bg("truth"))

  if (collapse_truth) {
    tbl <- tbl |> kableExtra::collapse_rows(1)
  }

  tbl
}


#' Render base interval-estimate table
#'
#' @keywords internal
#' @noRd
.render_interval_estimate_base <- function(
  df,
  col_names,
  header_groups,
  caption,
  stripe_bg,
  diagram_x,
  diagram_lower,
  diagram_upper,
  diagram_lim = NULL,
  vline,
  include_pl = FALSE,
  collapse_cols
) {
  diag_col <- if (include_pl) 4L else 3L

  tbl <- df |>
    kableExtra::kbl(
      col.names = col_names,
      caption = caption,
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::add_header_above(
      header_groups,
      bold = TRUE,
      background = table_accent("group_row"),
      color = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>
    .apply_row_striping(stripe_bg) |>
    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(2, color = table_text_body("interval"), bold = TRUE)

  if (include_pl) {
    tbl <- tbl |>
      kableExtra::column_spec(
        3,
        color = table_text_body("pseudolikelihood"),
        bold = TRUE
      )
  }

  tbl |>
    kableExtra::column_spec(
      diag_col,
      image = kableExtra::spec_pointrange(
        x = diagram_x,
        xmin = diagram_lower,
        xmax = diagram_upper,
        vline = vline,
        lim = diagram_lim,
        line_col = table_text_body("diagram"),
        width = 300,
        height = 150,
        cex = 0.6
      )
    ) |>
    kableExtra::column_spec(
      diag_col + 1L,
      color = table_text_body("length"),
      bold = TRUE
    ) |>
    kableExtra::column_spec(
      diag_col + 2L,
      color = table_text_body("lower_dev"),
      bold = TRUE
    ) |>
    kableExtra::column_spec(
      diag_col + 3L,
      color = table_text_body("upper_dev"),
      bold = TRUE
    ) |>
    kableExtra::column_spec(
      diag_col + 5L,
      color = table_text_body("level"),
      bold = TRUE
    ) |>
    kableExtra::column_spec(1, background = table_column_bg("truth")) |>
    kableExtra::collapse_rows(collapse_cols)
}


#' Render combined estimates base table
#'
#' @keywords internal
#' @noRd
.render_estimates_base <- function(
  df_render,
  caption,
  header_groups,
  header_bg,
  header_cols,
  body_spec_fun,
  stripe_bg,
  collapse_cols,
  include_pl = FALSE,
  pe_bg = NULL
) {
  col_names <- if (include_pl) {
    c(
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
    )
  } else {
    c(
      "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
      "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
      "$\\hat{\\psi}$",
      "$\\psi_0$",
      "Interval",
      "Diagram",
      "Length",
      "Lower<br/>Deviation",
      "Upper<br/>Deviation",
      "Covers $\\psi_0$",
      "Confidence<br/>Level"
    )
  }

  tbl <- df_render |>
    kableExtra::kbl(
      booktabs = TRUE,
      escape = FALSE,
      align = "c",
      col.names = col_names,
      caption = caption
    ) |>
    kableExtra::add_header_above(
      header_groups,
      bold = TRUE,
      background = header_bg,
      color = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>
    kableExtra::column_spec(
      header_cols$point,
      background = table_column_header_bg("point_estimate"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      header_cols$truth,
      background = table_column_header_bg("truth"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      header_cols$interval,
      background = table_column_header_bg("interval_estimate"),
      bold = TRUE,
      color = table_text_header("column"),
      include_thead = TRUE
    ) |>
    body_spec_fun() |>
    kableExtra::kable_styling(full_width = FALSE, position = "center") |>
    .apply_row_striping(stripe_bg) |>
    .apply_vertical_centering(n_rows = nrow(df_render))

  if (include_pl && !is.null(pe_bg)) {
    tbl <- kableExtra::column_spec(tbl, 1:4, background = pe_bg)
  }

  tbl |>
    kableExtra::column_spec(
      header_cols$truth,
      background = table_column_bg("truth")
    ) |>
    kableExtra::collapse_rows(columns = collapse_cols)
}

# ============================================================================
# END render-utils.R
# ============================================================================
