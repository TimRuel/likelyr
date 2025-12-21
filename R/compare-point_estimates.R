
get_point_estimates_df <- function(res) {

  res |>
    purrr::map(\(x) x$inference$point_estimate_df) |>
    dplyr::bind_rows(.id = "method") |>
    mutate(
      method = dplyr::recode(method,
                             PL = "Profile",
                             IL = "Integrated")
    ) |>
    dplyr::add_row(method = "Truth", psi_hat = psi_0) |>
    select(-psi_0)
}

