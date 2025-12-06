# =====================================================================
# utils-tables.R
# Kable tables for MLE and CI summaries
# =====================================================================

#' Construct Caption for Kable Tables
#'
#' @param path Character path containing experiment identifiers.
#'
#' @return HTML string for use as caption.
#' @export
make_kable_caption <- function(path) {

  path_parts <- strsplit(path, "/")[[1]]

  experiment_version <- path_parts[grep("^exp_v", path_parts)]
  sim_num <- path_parts[grep("^sim_", path_parts)]
  iter_num <- path_parts[grep("^iter_", path_parts)]

  subtitle <- paste(
    "Experiment", experiment_version,
    "\u2014 Simulation", sim_num,
    "\u2014 Iteration", iter_num
  )

  title <- "Pseudolikelihood Estimate Comparison Tables"

  paste0(
    "<center><span style='font-size:100%'>",
    title,
    "</span><br><span style='font-size:50%'>",
    subtitle,
    "</span></center>"
  )
}


#' Render Comparison Tables for MLEs and Confidence Intervals
#'
#' @param MLE_data Data frame with MLEs per pseudolikelihood.
#' @param conf_ints Confidence interval table.
#' @param psi_0 True ψ value (optional).
#'
#' @return A list of two HTML tables.
#' @export
render_LL_comparison_tables <- function(MLE_data, conf_ints, psi_0) {

  # ---------------------------
  # MLE table
  # ---------------------------
  mle_table <- MLE_data |>
    dplyr::select(Source, MLE) |>
    dplyr::mutate(MLE = sprintf("%.3f", MLE)) |>
    dplyr::bind_rows(
      tibble::tibble(Source = "Truth", MLE = sprintf("%.3f", psi_0))
    ) |>
    knitr::kbl(
      caption = "MLE Table",
      align = "c",
      escape = FALSE
    ) |>
    kableExtra::kable_material_dark(
      lightable_options = c("hover"),
      position = "center",
      font_size = 18
    ) |>
    kableExtra::row_spec(nrow(MLE_data) + 1, color = "green", bold = TRUE) |>
    kableExtra::column_spec(1:2, extra_css = "vertical-align:middle;")


  # ---------------------------
  # Confidence interval table
  # ---------------------------
  ci_table <- conf_ints |>
    dplyr::mutate(
      contains_truth = !is.na(lower) & !is.na(upper) &
        (lower <= psi_0 & upper >= psi_0),
      Status = ifelse(contains_truth, "✅", "❌"),
      Interval = sprintf("(%.3f, %.3f)", lower, upper),
      Length = ifelse(
        is.na(lower) | is.na(upper),
        "--",
        sprintf("%.3f", upper - lower)
      ),
      `Confidence Level` = confidence
    ) |>
    dplyr::rename(Source = pseudolikelihood) |>
    dplyr::select(`Confidence Level`, Source, Interval, Length, Status)

  # Add truth rows
  confidence_levels <- unique(ci_table$`Confidence Level`)
  truth_rows <- tibble::tibble(
    `Confidence Level` = confidence_levels,
    Source = "Truth",
    Interval = sprintf("%.3f", psi_0),
    Length = "",
    Status = ""
  )

  ci_table <- dplyr::bind_rows(ci_table, truth_rows) |>
    dplyr::arrange(`Confidence Level`, Source)

  ci_kable <- ci_table |>
    knitr::kbl(
      caption = "Confidence Interval Table",
      escape = FALSE,
      align = "c"
    ) |>
    kableExtra::kable_material_dark(
      lightable_options  = c("hover"),
      position = "center",
      font_size = 18,
      html_font = "Arial"
    ) |>
    kableExtra::collapse_rows(columns = 1, valign = "middle") |>
    kableExtra::row_spec(
      which(ci_table$Source == "Truth"),
      bold = TRUE,
      color = "green"
    ) |>
    kableExtra::column_spec(1:4, extra_css = "vertical-align:middle;")

  list(
    MLEs = mle_table,
    CIs  = ci_kable
  )
}
