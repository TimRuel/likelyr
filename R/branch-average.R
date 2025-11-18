#' Average Branches (Monte Carlo Integrated Likelihood)
#'
#' @description
#' Computes the Monte Carlo estimate of the integrated likelihood by
#' averaging exponentiated branch log-likelihoods and returning results
#' on the log scale using a numerically stable log-sum-exp operation.
#'
#' @param branches A list of data.frames, each with columns `psi` and `value`
#'   where `value` is log-likelihood evaluated at that psi.
#'
#' @return data.frame with columns `psi` and `value` (integrated LL on log scale)
#' @export
average_branches <- function(branches) {
  # 1) merge by psi grid index k
  merged <- Reduce(function(x, y) dplyr::inner_join(x, y, by = "k"), branches)

  # 2) extract psi and value matrix
  psi <- merged$psi
  branch_mat <- merged |>
    dplyr::select(dplyr::starts_with("value")) |>
    as.matrix()

  # 3) log-mean-exp along rows
  R <- ncol(branch_mat)
  log_mean <- matrixStats::rowLogSumExps(branch_mat) - log(R)

  tibble::tibble(psi = psi, value = as.numeric(log_mean))
}
