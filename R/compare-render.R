# ============================================================================
# compare-render.R — Rendering functions for pseudo-log-likelihood comparison
# (local-only)
# ============================================================================

# ============================================================================
# Shared helpers
# ============================================================================

#' HTML caption wrapper for comparison tables
#'
#' @description
#' Internal helper that constructs a styled HTML caption for
#' `kableExtra` tables. Supports an optional secondary subtitle,
#' typically used to indicate pseudolikelihood type.
#'
#' @details
#' The caption consists of a primary title rendered in bold text, with an
#' optional italicized subtitle displayed on a new line beneath the title.
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


#' Compute background colors by confidence level blocks
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
# Public render functions
# ============================================================================

#' Render point estimate comparison table
#'
#' @keywords internal
#' @noRd
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
    caption = .table_caption("Point Estimates and Uncertainty Measures"),
    stripe_bg = bg,
    collapse_truth = TRUE
  )
}


#' Render interval estimate comparison table
#'
#' @keywords internal
#' @noRd
render_interval_estimates_comparison_table <- function(interval_estimates_df) {
  required <- c(
    "level",
    "pseudolikelihood",
    "interval",
    "length",
    "lower_dev",
    "upper_dev",
    "contains_truth"
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

  n_levels <- dplyr::n_distinct(interval_estimates_df$level)
  diagram_x <- rep(point_estimates, nrow(interval_estimates_raw) / 2)

  df_render <- interval_estimates_df |>
    dplyr::mutate(psi_0 = round(psi_0, 2), .before = interval) |>
    dplyr::mutate(diagram = "", .after = pseudolikelihood) |>
    dplyr::mutate(
      contains_truth = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth ~ "\u2705",
        TRUE ~ "\u274c"
      )
    )

  bg <- .interval_level_bg(df_render$level)

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
    caption = .table_caption("Interval Estimates and Uncertainty Measures"),
    stripe_bg = bg,
    diagram_x = diagram_x,
    diagram_lower = interval_estimates_raw$lower,
    diagram_upper = interval_estimates_raw$upper,
    vline = psi_0,
    include_pl = TRUE,
    collapse_cols = c(1, 9)
  )
}


#' Render combined point and interval comparison table
#'
#' @keywords internal
#' @noRd
render_estimates_comparison_table <- function(
  point_estimates_df,
  interval_estimates_df
) {
  interval_estimates_raw <- attr(
    interval_estimates_df,
    "interval_estimates_raw"
  )
  point_estimates <- attr(interval_estimates_df, "point_estimates")
  psi_0 <- attr(interval_estimates_df, "psi_0")

  n_levels <- dplyr::n_distinct(interval_estimates_df$level)
  diagram_x <- rep(point_estimates, nrow(interval_estimates_raw) / 2)

  estimates_df <- point_estimates_df |>
    dplyr::rename(method = pseudolikelihood) |>
    tidyr::uncount(n_levels) |>
    dplyr::bind_cols(interval_estimates_df) |>
    dplyr::select(
      method,
      se_psi_hat,
      error,
      psi_hat,
      psi_0,
      interval,
      pseudolikelihood,
      length,
      lower_dev,
      upper_dev,
      contains_truth,
      level
    )

  df_render <- estimates_df |>
    dplyr::mutate(diagram = "", .after = "pseudolikelihood") |>
    dplyr::mutate(
      dplyr::across(
        c(psi_hat, error, se_psi_hat),
        ~ paste0(.x, "<span style='display:none'>", method, "</span>")
      ),
      contains_truth = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth ~ "\u2705",
        TRUE ~ "\u274c"
      )
    )

  bg_interval <- .interval_level_bg(df_render$level)
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
          x = diagram_x,
          xmin = interval_estimates_raw$lower,
          xmax = interval_estimates_raw$upper,
          vline = psi_0,
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
    header_cols = list(point = 1:4, truth = 5, interval = 6:13),
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
