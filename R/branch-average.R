# ======================================================================
# Average Branches (Monte Carlo Integrated Log-Likelihood)
# ======================================================================

#' Average Branches (Monte Carlo Integrated Log-Likelihood)
#'
#' @description
#' Computes the Monte Carlo integrated log-likelihood curve by averaging
#' branch-specific log-likelihood values using the numerically-stable
#' log-mean-exp identity:
#'
#'   log L̂(ψ_k) = log( (1/R) Σ exp(ℓ_{k,r}) )
#'
#' Diagnostics (ESS, MC variance, outlier rate, etc.) are computed in a
#' separate function and attached as an attribute: `attr(out, "diagnostics")`.
#'
#' @param branches List of branch tibbles, each containing:
#'   * k     — integer ψ-grid index
#'   * psi   — ψ-grid value
#'   * value — log-likelihood ℓ_{k,r}
#'
#' @return tibble(psi, value) with attribute `"diagnostics"`
#'
#' @export
average_branches <- function(branches) {

  # -------------------------------------------------------------
  # 1. Rename "loglik" columns: loglik1, loglik2, ...
  # -------------------------------------------------------------
  renamed <- Map(
    f = function(br, i) dplyr::rename(br, !!paste0("loglik", i) := loglik),
    br = branches,
    i  = seq_along(branches)
  )

  # -------------------------------------------------------------
  # 2. Merge on (k, psi)
  # -------------------------------------------------------------
  merged <- Reduce(
    function(a, b) dplyr::inner_join(a, b, by = c("k", "psi")),
    renamed
  )

  psi <- merged$psi

  # -------------------------------------------------------------
  # 3. Extract matrix (K × R)
  # -------------------------------------------------------------
  branch_mat <- merged |>
    dplyr::select(matches("^loglik\\d+$")) |>
    as.matrix()

  R <- ncol(branch_mat)

  # -------------------------------------------------------------
  # 4. Compute log integrated likelihood via log-mean-exp
  # -------------------------------------------------------------
  log_mean <- matrixStats::rowLogSumExps(branch_mat) - log(R)

  psi_ll_df <- tibble::tibble(
    psi   = psi,
    loglik = as.numeric(log_mean)
  )

  attr(psi_ll_df, "type") <- "Integrated"

  list(psi_ll_df = psi_ll_df,
       branch_mat = branch_mat)
}
