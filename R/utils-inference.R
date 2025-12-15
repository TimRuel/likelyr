fit_psi_ll_fn <- function(psi_ll_df) {

  if (!all(c("psi", "loglik") %in% names(psi_ll_df)))
    stop("Spline model requires columns 'psi' and 'loglik'.", call. = FALSE)

  psi_ll_spline <- stats::smooth.spline(psi_ll_df$psi, psi_ll_df$loglik)

  psi_ll_fn <- function(psi) stats::predict(psi_ll_spline, psi)$y

  return(psi_ll_fn)
}

get_psi_ll_max_point <- function(psi_ll_fn, psi_ll_df) {

  opt <- stats::optimize(
    f       = psi_ll_fn,
    lower   = min(psi_ll_df$psi),
    upper   = max(psi_ll_df$psi),
    maximum = TRUE
  )

  psi_ll_max_point <- tibble::tibble(
    argmax = opt$maximum,
    max = opt$objective
  )

  return(psi_ll_max_point)
}

shift_psi_ll_fn <- function(psi_ll_fn, shift_val) {

  shifted_psi_ll_fn <- function(psi) psi_ll_fn(psi) - shift_val

  return(shifted_psi_ll_fn)
}

get_psi_conf_ints_df <- function(zero_max_psi_ll_fn,
                                 psi_ll_argmax,
                                 psi_ll_df,
                                 alpha_levels,
                                 uniroot_expand_factor) {

  psi_grid <- psi_ll_df$psi
  psi_min0 <- min(psi_grid)
  psi_max0 <- max(psi_grid)

  psi_conf_ints_df <- purrr::map_dfr(alpha_levels, function(alpha) {

    crit  <- 0.5 * stats::qchisq(1 - alpha, df = 1)
    confidence <- scales::percent(1 - alpha)

    # ----------------------------------------------------------
    # Expand intervals multiplicatively to improve robustness
    # ----------------------------------------------------------
    left_width   <- psi_ll_argmax - psi_min0
    right_width  <- psi_max0 - psi_ll_argmax

    psi_min <- psi_min0 - left_width  * uniroot_expand_factor
    psi_max <- psi_max0 + right_width * uniroot_expand_factor

    # --- Lower bound ---
    lower <- tryCatch(
      stats::uniroot(
        f        = function(psi) zero_max_psi_ll_fn(psi) + crit,
        interval = c(psi_min, psi_ll_argmax)
      )$root,
      error = function(e) NA_real_
    )

    # --- Upper bound ---
    upper <- tryCatch(
      stats::uniroot(
        f        = function(psi) zero_max_psi_ll_fn(psi) + crit,
        interval = c(psi_ll_argmax, psi_max)
      )$root,
      error = function(e) NA_real_
    )

    tibble::tibble(
      confidence = confidence,
      alpha      = alpha,
      lower      = round(lower, 6),
      upper      = round(upper, 6)
    )
  })

  return(psi_conf_ints_df)
}

get_se_psi_hat <- function(psi_hat, psi_ll_df) {

  psi_vals <- psi_ll_df$psi
  ll_vals <- psi_ll_df$loglik

  # Find index closest to psi_hat
  k <- which.min(abs(psi_vals - psi_hat))

  # Defensive checks
  if (k <= 1L || k >= nrow(psi_ll_df)) {
    stop("psi_hat must lie strictly inside the psi grid to compute curvature-based SE.")
  }

  # Grid spacing
  h <- psi_vals[k + 1L] - psi_vals[k]

  # Second derivative (central difference)
  second_deriv <- (
    ll_vals[k + 1L] -
      2 * ll_vals[k] +
      ll_vals[k - 1L]
  ) / h^2

  # Observed information and SE
  obs_info <- -second_deriv
  se_psi_hat <- 1 / sqrt(obs_info)
  return(se_psi_hat)
}

render_psi_mle_kable <- function(psi_mle_df, mode, show_caption = TRUE) {

  caption_html <- if (show_caption) {
    paste0(
      "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
      "Point Estimate and Standard Error<br>",
      "<em>(", mode, " Likelihood)</em>",
      "</span>"
    )
  } else {
    NULL
  }

  kableExtra::kbl(
    psi_mle_df,
    col.names = c(
      "$\\psi_0$",
      if (mode == "Integrated") "$\\overline{\\psi}$" else "$\\hat{\\psi}$",
      if (mode == "Integrated")
        "$\\widehat{\\mathrm{SE}}(\\overline{\\psi})$"
      else
        "$\\widehat{\\mathrm{SE}}(\\hat{\\psi})$"
    ),
    caption = caption_html,
    escape = FALSE,
    align = "c"
  ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    kableExtra::row_spec(0, background = "#3B4252", color = "#EAEAEA") |>
    kableExtra::column_spec(1, color = "#7FBF7F") |>
    kableExtra::column_spec(2, color = "#7AA6DA") |>
    kableExtra::column_spec(3, color = "#D67272")
}

get_psi_conf_ints_table <- function(psi_conf_ints_df, psi_hat, psi_0 = NA_real_) {

  psi_conf_ints_df |>
    dplyr::mutate(
      Level = confidence,

      Interval = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_character_,
        sprintf("[%.2f, %.2f]", lower, upper)
      ),

      Length = dplyr::if_else(
        is.na(lower) | is.na(upper),
        NA_real_,
        upper - lower
      ),

      `Lower Deviation` = dplyr::if_else(
        is.na(lower),
        NA_real_,
        psi_hat - lower
      ),

      `Upper Deviation` = dplyr::if_else(
        is.na(upper),
        NA_real_,
        upper - psi_hat
      ),

      contains_truth = dplyr::case_when(
        is.na(psi_0) ~ NA,
        TRUE ~ (!is.na(lower) & !is.na(upper) & lower <= psi_0 & upper >= psi_0)
      ),

      Status = dplyr::case_when(
        is.na(contains_truth) ~ NA_character_,
        contains_truth        ~ "✅",
        TRUE                  ~ "❌"
      )
    ) |>
    dplyr::select(
      Level,
      Interval,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status
    ) |>
    # 🔹 ROUND ALL NUMERIC COLUMNS FOR DISPLAY
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric),
        .fns  = ~ round(.x, 2)
      )
    )
}

render_ci_kable <- function(ci_table, mode, show_caption = TRUE) {

  stopifnot(
    all(
      c(
        "Level",
        "Interval",
        "Length",
        "Lower Deviation",
        "Upper Deviation",
        "Status"
      ) %in% names(ci_table)
    )
  )

  caption_html <- if (show_caption) {
    paste0(
      "<span style='color:#2D2D2D; font-size:1.05em; font-weight:500;'>",
      "Interval Estimates<br>",
      "<em>(", mode, " Likelihood)</em>",
      "</span>"
    )
  } else {
    NULL
  }

  kableExtra::kbl(
    ci_table,
    col.names = c(
      "Level",
      "Interval",
      "Length",
      "Lower Deviation",
      "Upper Deviation",
      "Covers $\\psi_0$"
    ),
    caption = caption_html,
    escape = FALSE,
    align = "c"
  ) |>
    kableExtra::kable_material_dark(font_size = 17) |>
    kableExtra::row_spec(
      0,
      background = "#3B4252",
      color = "#EAEAEA"
    ) |>
    # Column color scheme (semantically aligned)
    kableExtra::column_spec(1, color = "#7FBF7F") |>
    kableExtra::column_spec(2, color = "#7AA6DA") |>
    kableExtra::column_spec(3, color = "#EBCB8B") |>
    kableExtra::column_spec(4, color = "#D08770") |>
    kableExtra::column_spec(5, color = "#81A1C1")
}



render_inference_table <- function(mle_kable, ci_kable, mode) {

  htmltools::html_print(
    htmltools::tagList(

      # ---- Shared figure caption ----
      htmltools::div(
        style = "
          text-align: center;
          margin-bottom: 20px;
          color: #2D2D2D;
          font-size: 1.1em;
          font-weight: 500;
        ",
        htmltools::HTML(paste0(
          "Inference Summary<br>",
          "<em>(", mode, " Likelihood)</em>"
        ))
      ),

      # ---- Let kableExtra print these normally ----
      mle_kable,
      htmltools::tags$div(style = "height: 24px;"),
      ci_kable
    )
  )
}
