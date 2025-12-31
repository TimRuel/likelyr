
get_interval_estimates_df <- function(res) {

  interval_estimates_df <- res |>
    purrr::map(\(x) x$inference$interval_estimate_df) |>
    dplyr::bind_rows(.id = "Pseudolikelihood") |>
    dplyr::mutate(
      Pseudolikelihood = dplyr::recode(Pseudolikelihood,
                                       PL = "Profile",
                                       IL = "Integrated")
    ) |>
    dplyr::arrange(Level) |>
    dplyr::select(
      Interval,
      Pseudolikelihood,
      Length,
      `Lower Deviation`,
      `Upper Deviation`,
      Status,
      Level
    )

  attr(interval_estimates_df, "point_estimates") <- res |>
    purrr::map_dbl(\(x) attr(x$inference$interval_estimate_df, "point_estimate"))

  attr(interval_estimates_df, "interval_estimates_raw") <- res |>
    purrr::map(\(x) attr(x$inference$interval_estimate_df, "interval_estimate_raw")) |>
    dplyr::bind_rows(.id = "pseudolikelihood") |>
    dplyr::arrange(-alpha)

  attr(interval_estimates_df, "point_estimate") <- NULL
  attr(interval_estimates_df, "interval_estimate_raw") <- NULL
  attr(interval_estimates_df, "type") <- NULL

  return(interval_estimates_df)
}


