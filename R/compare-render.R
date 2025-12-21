# =========================================================================
# compare-render.R — Rendering helpers for pseudo-log-likelihood comparison
# =========================================================================

# ---------------------------------------------------------------------
# Palette
# ---------------------------------------------------------------------

#' Palette for inference kables
#'
#' @description
#' Returns a single, consistent color palette for comparison tables.
#' Designed to be visually restrained, blue-centric, and publication-safe.
#'
#' @return A named list of color values.
#' @keywords internal
comparison_kable_palette <- function() {

  list(
    # Body text colors
    psi_0     = "#7FBF7F",
    psi_hat   = "#81A1C1",
    se        = "#D08770",
    level     = "#8FBCBB",
    interval  = "#5E81AC",
    length    = "#EBCB8B",
    lower_dev = "#D08770",
    upper_dev = "#B48EAD",
    covers_ok = "#A3BE8C",
    covers_no = "#BF616A",

    # Header text
    header_text = "#B9D9EB",

    # Group headers
    bg_group_pe = "#434C5E",
    bg_group_ie = "#3E5C7A",

    # Column headers
    bg_head_pe  = "#4A5568",
    bg_head_ie  = "#4E6A88",

    # Body backgrounds
    bg_body_pe  = "#2E344020",
    bg_body_ie  = "#34495E20",

    # Column-level PE backgrounds
    bg_body_pe_psi0   = "#2E344025",
    bg_body_pe_psihat = "#323A4A25",
    bg_body_pe_se     = "#2C314025",

    # Accents
    separator   = "#4C566A",
    header_row  = "#3B4252",
    header_row_text = "#EAEAEA"
  )
}

# ---------------------------------------------------------------------
# Point estimate comparison table
# ---------------------------------------------------------------------

#' Render point estimate comparison table
#'
#' @param res
#'
#' @return A kableExtra object.
render_point_estimate_comparison <- function(res) {

  point_estimates_df <- get_point_estimates_df(res)

  pal <- comparison_kable_palette()

  caption_html <- paste0(
    "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
    "Point Estimates and Standard Errors<br>",
    "<em>(Integrated Log-Likelihood vs Profile Log-Likelihood)</em>",
    "</span>"
  )

  kableExtra::kbl(
    point_estimates_df,
    col.names = c(
      "$\\psi_0$",
      "Method",
      "$\\hat{\\psi}$",
      "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$"
    ),
    caption = caption_html,
    escape  = FALSE,
    align   = "c"
  ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    kableExtra::row_spec(
      0,
      background = pal$header_row,
      color      = pal$header_row_text
    ) |>
    kableExtra::column_spec(1, color = pal$psi_0)   |>
    kableExtra::column_spec(2, color = pal$psi_hat) |>
    kableExtra::column_spec(3, color = pal$se)
}

# ---------------------------------------------------------------------
# Interval estimates table
# ---------------------------------------------------------------------

#' Render confidence interval table
#'
#' @param interval_estimates_df Data frame produced by
#'   `get_interval_estimates_df()`.
#' @param show_caption Logical; whether to display a caption.
#'
#' @return A kableExtra object.
render_interval_estimates_kable <- function(interval_estimates_df, show_caption = TRUE) {

  type <- attr(interval_estimates_df, "type")

  pal <- inference_kable_palette()

  required <- c(
    "Level", "Interval", "Length",
    "Lower Deviation", "Upper Deviation", "Status"
  )

  stopifnot(all(required %in% names(interval_estimates_df)))

  caption_html <- if (isTRUE(show_caption)) {
    paste0(
      "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
      "Interval Estimates<br>",
      "<em>(", type, " Log-Likelihood)</em>",
      "</span>"
    )
  } else {
    NULL
  }

  kableExtra::kbl(
    interval_estimates_df,
    col.names = c(
      "Confidence<br/>Level",
      "Interval",
      "Length",
      "Lower<br/>Deviation",
      "Upper<br/>Deviation",
      "Covers $\\psi_0$"
    ),
    caption = caption_html,
    escape  = FALSE,
    align   = "c"
  ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    kableExtra::row_spec(
      0,
      background = pal$header_row,
      color      = pal$header_row_text
    ) |>
    kableExtra::column_spec(1, color = pal$level)     |>
    kableExtra::column_spec(2, color = pal$interval)  |>
    kableExtra::column_spec(3, color = pal$length)    |>
    kableExtra::column_spec(4, color = pal$lower_dev) |>
    kableExtra::column_spec(5, color = pal$upper_dev)
}

# ---------------------------------------------------------------------
# Combined inference table
# ---------------------------------------------------------------------

#' Render combined inference table
#'
#' @param inference_df Data frame combining point and interval estimates.
#' @return A kableExtra object.
render_inference_kable <- function(inference_df) {

  type <- attr(inference_df, "type")

  pal <- inference_kable_palette()

  inference_df |>
    dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) |>

    kableExtra::kbl(
      booktabs = TRUE,
      escape   = FALSE,
      align    = "c",
      col.names = c(
        "$\\psi_0$",
        "$\\hat{\\psi}$",
        "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
        "Confidence<br/>Level",
        "Interval",
        "Length",
        "Lower<br/>Deviation",
        "Upper<br/>Deviation",
        "Covers $\\psi_0$"
      ),
      caption = paste0(
        "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
        "Point Estimate and Confidence Intervals<br>",
        "<em>(", type, " Log-Likelihood)</em>",
        "</span>"
      )
    ) |>

    kableExtra::add_header_above(
      c("Point Estimate" = 3, "Interval Estimates" = 6),
      bold       = TRUE,
      background = c(pal$bg_group_pe, pal$bg_group_ie),
      color      = pal$header_text
    ) |>

    kableExtra::kable_material_dark(font_size = 17) |>

    kableExtra::column_spec(
      1:3, background = pal$bg_head_pe,
      bold = TRUE, color = pal$header_text, include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      4:9, background = pal$bg_head_ie,
      bold = TRUE, color = pal$header_text, include_thead = TRUE
    ) |>

    kableExtra::column_spec(1:3, background = pal$bg_body_pe) |>
    kableExtra::column_spec(4:9, background = pal$bg_body_ie) |>

    kableExtra::column_spec(
      3, border_right = paste0("1px solid ", pal$separator)
    ) |>

    kableExtra::column_spec(1, color = pal$psi_0)     |>
    kableExtra::column_spec(2, color = pal$psi_hat)   |>
    kableExtra::column_spec(3, color = pal$se)        |>
    kableExtra::column_spec(4, color = pal$level)     |>
    kableExtra::column_spec(5, color = pal$interval)  |>
    kableExtra::column_spec(6, color = pal$length)    |>
    kableExtra::column_spec(7, color = pal$lower_dev) |>
    kableExtra::column_spec(8, color = pal$upper_dev) |>

    kableExtra::column_spec(
      9,
      color = ifelse(
        inference_df[[9]] %in% c("✅", "✔", "TRUE"),
        pal$covers_ok,
        pal$covers_no
      ),
      width = "3em"
    ) |>

    kableExtra::column_spec(
      1, background = pal$bg_body_pe_psi0,   include_thead = FALSE
    ) |>
    kableExtra::column_spec(
      2, background = pal$bg_body_pe_psihat, include_thead = FALSE
    ) |>
    kableExtra::column_spec(
      3, background = pal$bg_body_pe_se,     include_thead = FALSE
    ) |>

    kableExtra::kable_styling(
      full_width = FALSE,
      position   = "center"
    ) |>
    kableExtra::collapse_rows(columns = 1:3)
}
