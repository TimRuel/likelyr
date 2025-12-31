
get_point_estimates_df <- function(res) {

  res |>
    purrr::map(\(x) x$inference$point_estimate_df) |>
    dplyr::bind_rows(.id = "pseudolikelihood") |>
    dplyr::mutate(
      pseudolikelihood = dplyr::recode(pseudolikelihood,
                                       PL = "Profile",
                                       IL = "Integrated")
    ) |>
    dplyr::relocate(pseudolikelihood, .after = se_psi_hat)
}


