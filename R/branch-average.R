# ======================================================================
# Average Branches (Monte Carlo Integrated Likelihood)
# ======================================================================

#' Average Branches (Monte Carlo Integrated Likelihood)
#'
#' @description
#' Computes the Monte Carlo integrated likelihood curve by averaging
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
  # 1. Rename "value" columns: value1, value2, ...
  # -------------------------------------------------------------
  renamed <- Map(
    f = function(br, i) dplyr::rename(br, !!paste0("value", i) := value),
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
    dplyr::select(matches("^value\\d+$")) |>
    as.matrix()

  R <- ncol(branch_mat)

  # -------------------------------------------------------------
  # 4. Compute log integrated likelihood via log-mean-exp
  # -------------------------------------------------------------
  log_mean <- matrixStats::rowLogSumExps(branch_mat) - log(R)

  df <- tibble::tibble(
    psi   = psi,
    value = as.numeric(log_mean)
  )

  list(df = df,
       branch_mat = branch_mat)
}
