#' HTML caption wrapper for comparison tables
#'
#' @description
#' Internal helper that constructs a styled HTML caption for
#' `kableExtra` tables. Supports an optional secondary subtitle,
#' typically used to indicate likelihood type (e.g., Profile or Integrated).
#'
#' @details
#' The caption consists of a primary title rendered in bold text, with an
#' optional italicized subtitle displayed on a new line beneath the title.
#' This allows consistent caption styling across tables while avoiding
#' manual HTML construction in calling code.
#'
#' @param text Character scalar giving the primary caption title.
#' @param type Optional character scalar giving a likelihood type label
#'   (e.g., \code{"Profile"}, \code{"Integrated"}). If supplied, the subtitle
#'   is rendered as \code{"(<type> Log-Likelihood)"} on a separate line.
#'
#' @return A length-1 character string containing HTML suitable for the
#'   \code{caption} argument of \code{kableExtra::kbl()}.
#'
#' @keywords internal
#' @noRd
.table_caption <- function(text, type = NULL) {

  subtitle <- if (!is.null(type)) {
    paste0("<em>(", type, " Log-Likelihood)</em>")
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

# ------------------------------------------------------------------
# Internal: render base point-estimate table
# ------------------------------------------------------------------
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
      caption   = caption,
      escape    = FALSE,
      align     = "c"
    ) |>
    kableExtra::add_header_above(
      header_groups,
      bold       = TRUE,
      background = table_accent("group_row"),
      color      = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>

    # Truth column header
    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>

    # Text coloring
    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(2, color = table_text_body("psi_hat"), bold = TRUE) |>
    kableExtra::column_spec(3, color = table_text_body("error"), bold = TRUE)   |>
    kableExtra::column_spec(4, color = table_text_body("se"), bold = TRUE)

  # Optional pseudolikelihood column
  if (ncol(df) >= 5) {
    tbl <- tbl |>
      kableExtra::column_spec(
        5,
        color = table_text_body("pseudolikelihood"),
        bold  = TRUE
      )
  }

  # Optional row striping
  if (!is.null(stripe_bg)) {
    tbl <- tbl |>
      .apply_row_striping(stripe_bg)
  }

  # Truth column body background
  tbl <- tbl |>
    kableExtra::column_spec(1, background = table_column_bg("truth"))

  # Optional row collapsing
  if (collapse_truth) {
    tbl <- tbl |>
      kableExtra::collapse_rows(1)
  }

  tbl
}

# ------------------------------------------------------------------
# Internal: render base interval-estimate table
# ------------------------------------------------------------------
.render_interval_estimate_base <- function(
    df,
    col_names,
    header_groups,
    caption,
    stripe_bg,
    diagram_x,
    diagram_lower,
    diagram_upper,
    vline,
    include_pl = FALSE,
    collapse_cols
) {

  tbl <- df |>
    kableExtra::kbl(
      col.names = col_names,
      caption   = caption,
      escape    = FALSE,
      align     = "c"
    ) |>
    kableExtra::add_header_above(
      header_groups,
      bold       = TRUE,
      background = table_accent("group_row"),
      color      = table_text_header("group")
    ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>
    .apply_row_striping(stripe_bg) |>

    # Truth column header
    kableExtra::column_spec(
      1,
      background = table_column_header_bg("truth"),
      include_thead = TRUE
    ) |>

    # Text coloring
    kableExtra::column_spec(1, color = table_text_body("psi_0"), bold = TRUE) |>
    kableExtra::column_spec(2, color = table_text_body("interval"), bold = TRUE)

  if (include_pl) {
    tbl <- tbl |>
      kableExtra::column_spec(
        3,
        color = table_text_body("pseudolikelihood"),
        bold  = TRUE
      )
  }

  tbl <- tbl |>
    kableExtra::column_spec(
      if (include_pl) 4 else 3,
      image = kableExtra::spec_pointrange(
        x     = diagram_x,
        xmin  = diagram_lower,
        xmax  = diagram_upper,
        vline = vline,
        line_col = table_text_body("diagram"),
        width  = 300,
        height = 150,
        cex    = 0.6
      )
    ) |>
    kableExtra::column_spec(
      if (include_pl) 5 else 4,
      color = table_text_body("length"),
      bold  = TRUE
    ) |>
    kableExtra::column_spec(
      if (include_pl) 6 else 5,
      color = table_text_body("lower_dev"),
      bold  = TRUE
    ) |>
    kableExtra::column_spec(
      if (include_pl) 7 else 6,
      color = table_text_body("upper_dev"),
      bold  = TRUE
    ) |>
    kableExtra::column_spec(
      if (include_pl) 9 else 8,
      color = table_text_body("level"),
      bold  = TRUE
    ) |>
    kableExtra::column_spec(1, background = table_column_bg("truth")) |>
    kableExtra::collapse_rows(collapse_cols)

  tbl
}

# ------------------------------------------------------------------
# Internal: render combined estimates base table
# ------------------------------------------------------------------
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

  # --------------------------------------------------
  # Column names depend on whether pseudolikelihood
  # columns are present
  # --------------------------------------------------
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
      escape   = FALSE,
      align    = "c",
      col.names = col_names,
      caption   = caption
      ) |>

    kableExtra::add_header_above(
      header_groups,
      bold       = TRUE,
      background = header_bg,
      color      = table_text_header("group")
      ) |>

    kableExtra::kable_material_dark(font_size = 17) |>
    .apply_standard_headers() |>

    # --------------------------------------------------
    # Column headers
    # --------------------------------------------------
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

    # --------------------------------------------------
    # Body specs (delegated)
    # --------------------------------------------------
    body_spec_fun() |>
    kableExtra::kable_styling(full_width = FALSE, position = "center")

  tbl |>
    .apply_row_striping(stripe_bg) |>
    .apply_vertical_centering(n_rows = nrow(df_render)) |>
    (\(x) {
      if (include_pl && !is.null(pe_bg)) {
        kableExtra::column_spec(x, 1:4, background = pe_bg)
      } else {
        x
      }
    }
    )() |>
    kableExtra::column_spec(
      header_cols$truth,
      background = table_column_bg("truth")
      ) |>
    kableExtra::collapse_rows(columns = collapse_cols)
}




