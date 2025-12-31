# =====================================================================
# infer-render.R — Rendering helpers for likelihood inference
# =====================================================================

# ---------------------------------------------------------------------
# Palette
# ---------------------------------------------------------------------

#' Palette for inference kables
#'
#' @description
#' Returns a single, consistent color palette for inference tables.
#' Designed to be visually restrained, blue-centric, and publication-safe.
#'
#' @return A named list of color values.
#' @keywords internal
inference_kable_palette <- function() {

  list(
    # --------------------------------------------------
    # Body text colors
    # --------------------------------------------------
    psi_0     = "#6CBF6C",
    psi_hat   = "#6FA3D9",
    error     = "#F0B51A",
    se        = "#E06C75",
    level     = "#6CBF6C",
    interval  = "#6FA3D9",
    diagram   = "#88C0D0",
    length    = "#F0B51A",
    lower_dev = "#E06C75",
    upper_dev = "#C678DD",
    covers_ok = "#A3BE8C",
    covers_no = "#BF616A",

    # --------------------------------------------------
    # Header text
    # --------------------------------------------------
    header_text      = "#D8DEE9",
    header_row_text  = "#EAEAEA",

    # --------------------------------------------------
    # Group headers
    # --------------------------------------------------
    bg_group_pe = "#1B202920",
    bg_group_ie = "#2E3442",

    # --------------------------------------------------
    # Column headers
    # --------------------------------------------------
    bg_head_pe  = "#252A3420",
    bg_head_ie  = "#343A46",

    # --------------------------------------------------
    # Body backgrounds
    # --------------------------------------------------
    bg_body_pe  = "#262B36",
    bg_body_ie  = "#3C4454",

    # --------------------------------------------------
    # Column-level PE backgrounds
    # --------------------------------------------------
    bg_body_pe_psi0    = "#252A3425",
    bg_body_pe_psihat = "#2A304025",
    bg_body_pe_se     = "#22273325",

    # --------------------------------------------------
    # Accents
    # --------------------------------------------------
    separator  = "#4C566A",
    header_row = "#303644"
  )
}

# ---------------------------------------------------------------------
# Point estimate table
# ---------------------------------------------------------------------

#' Render point estimate table
#'
#' @param point_estimate_df Data frame with columns `psi_0`, `psi_hat`,
#'   and `se_psi_hat`.
#' @param show_caption Logical; whether to display a caption.
#'
#' @return A kableExtra object.
render_point_estimate_kable <- function(point_estimate_df, show_caption = TRUE) {

  pal <- inference_kable_palette()

  type <- attr(point_estimate_df, "type")

  caption_html <- if (isTRUE(show_caption)) {
    paste0(
      "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
      "Point Estimate and Uncertainty Measures<br>",
      "<em>(", type, " Log-Likelihood)</em>",
      "</span>"
    )
  } else {
    NULL
  }

  kableExtra::kbl(
    point_estimate_df,
    col.names = c(
      "$\\psi_0$",
      "$\\hat{\\psi}$",
      "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
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
    kableExtra::column_spec(3, color = pal$error) |>
    kableExtra::column_spec(4, color = pal$se)
}

# ---------------------------------------------------------------------
# Interval estimate table
# ---------------------------------------------------------------------

#' Render confidence interval table
#'
#' @param interval_estimate_df Data frame produced by
#'   `get_interval_estimate_df()`.
#' @param show_caption Logical; whether to display a caption.
#'
#' @return A kableExtra object.
render_interval_estimate_kable <- function(interval_estimate_df, show_caption = TRUE) {

  type <- attr(interval_estimate_df, "type")
  interval_estimate_raw <- attr(interval_estimate_df, "interval_estimate_raw")
  point_estimate <- attr(interval_estimate_df, "point_estimate")
  psi_0 <- attr(interval_estimate_df, "psi_0")

  pal <- inference_kable_palette()

  required <- c(
    "Level", "Interval", "Length",
    "Lower Deviation", "Upper Deviation", "Status"
  )
  stopifnot(all(required %in% names(interval_estimate_df)))

  caption_html <- if (isTRUE(show_caption)) {
    paste0(
      "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
      "Interval Estimates and Uncertainty Measures<br>",
      "<em>(", type, " Log-Likelihood)</em>",
      "</span>"
    )
  } else {
    NULL
  }

  interval_estimate_df |>
    dplyr::mutate(Diagram = "", .after = "Interval") |>
    kableExtra::kbl(
      col.names = c(
        "Confidence<br/>Level",
        "Interval",
        "Diagram",
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

    kableExtra::column_spec(1, color = pal$level)    |>
    kableExtra::column_spec(2, color = pal$interval) |>

    # --------------------------------------------------
    # Visualization column (fully palette-driven)
    # --------------------------------------------------
    kableExtra::column_spec(
      3,
      image = kableExtra::spec_pointrange(
        x     = rep(point_estimate, nrow(interval_estimate_raw)),
        xmin  = interval_estimate_raw$lower,
        xmax  = interval_estimate_raw$upper,
        vline = psi_0,
        line_col  = pal$diagram,
        width = 300,
        height = 150,
        cex = 0.6
      )
    ) |>

    kableExtra::column_spec(4, color = pal$length)    |>
    kableExtra::column_spec(5, color = pal$lower_dev) |>
    kableExtra::column_spec(6, color = pal$upper_dev)
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

  interval_estimate_raw <- attr(inference_df, "interval_estimate_raw")
  point_estimate <- inference_df$psi_hat
  psi_0 <- inference_df$psi_0 |> unique()

  pal <- inference_kable_palette()

  inference_df |>
    dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) |>
    dplyr::mutate(Diagram = "", .after = "Interval") |>
    kableExtra::kbl(
      booktabs = TRUE,
      escape   = FALSE,
      align    = "c",
      col.names = c(
        "$\\psi_0$",
        "$\\hat{\\psi}$",
        "$\\mathrm{e}(\\hat{\\psi}; \\psi_0)$",
        "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$",
        "Confidence<br/>Level",
        "Interval",
        "Diagram",
        "Length",
        "Lower<br/>Deviation",
        "Upper<br/>Deviation",
        "Covers $\\psi_0$"
      ),
      caption = paste0(
        "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
        "Estimates and Uncertainty Measures<br>",
        "<em>(", type, " Log-Likelihood)</em>",
        "</span>"
      )
    ) |>

    # --------------------------------------------------
    # Group headers (updated counts)
    # --------------------------------------------------
    kableExtra::add_header_above(
      c("Point Estimate" = 4, "Interval Estimates" = 7),
      bold       = TRUE,
      background = c(pal$bg_group_pe, pal$bg_group_ie),
      color      = pal$header_text
    ) |>

    kableExtra::kable_material_dark(font_size = 17) |>

    # --------------------------------------------------
    # Column headers
    # --------------------------------------------------
    kableExtra::column_spec(
      1:4,
      background = pal$bg_head_pe,
      bold = TRUE, color = pal$header_text, include_thead = TRUE
    ) |>
    kableExtra::column_spec(
      5:11,
      background = pal$bg_head_ie,
      bold = TRUE, color = pal$header_text, include_thead = TRUE
    ) |>

    # --------------------------------------------------
    # Body backgrounds
    # --------------------------------------------------
    kableExtra::column_spec(1:4, background = pal$bg_body_pe)    |>
    kableExtra::column_spec(5:11, background = pal$bg_body_ie) |>

    # --------------------------------------------------
    # Visual separator
    # --------------------------------------------------
    kableExtra::column_spec(
      4, border_right = paste0("1px solid ", pal$separator)
    ) |>

    # --------------------------------------------------
    # Text coloring
    # --------------------------------------------------
    kableExtra::column_spec(1, color = pal$psi_0)    |>
    kableExtra::column_spec(2, color = pal$psi_hat)  |>
    kableExtra::column_spec(3, color = pal$error)    |>
    kableExtra::column_spec(4, color = pal$se)       |>
    kableExtra::column_spec(5, color = pal$level)    |>
    kableExtra::column_spec(6, color = pal$interval) |>

    # --------------------------------------------------
    # Diagram column (CI glyphs)
    # --------------------------------------------------
    kableExtra::column_spec(
      7,
      image = kableExtra::spec_pointrange(
        x     = point_estimate,
        xmin  = interval_estimate_raw$lower,
        xmax  = interval_estimate_raw$upper,
        vline = psi_0,
        line_col = pal$diagram,
        width = 300,
        height = 150,
        cex = 0.6
      )
    ) |>

    kableExtra::column_spec(8, color = pal$length)     |>
    kableExtra::column_spec(9, color = pal$lower_dev)  |>
    kableExtra::column_spec(10, color = pal$upper_dev) |>

    # --------------------------------------------------
    # Coverage column
    # --------------------------------------------------
    kableExtra::column_spec(
      11,
      color = ifelse(
        inference_df[[10]] %in% c("✅", "✔", "TRUE"),
        pal$covers_ok,
        pal$covers_no
      ),
      width = "3em"
    ) |>

    # --------------------------------------------------
    # Styling polish
    # --------------------------------------------------
    kableExtra::kable_styling(
      full_width = FALSE,
      position   = "center"
    ) |>

    kableExtra::column_spec(
      1:11,
      extra_css = "vertical-align: middle;"
    ) |>

    kableExtra::row_spec(
      0:1,
      extra_css = "vertical-align: middle;"
    ) |>

    kableExtra::collapse_rows(columns = 1:4)
}
