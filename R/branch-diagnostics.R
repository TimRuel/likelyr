# ======================================================================
# Diagnostics for Monte Carlo Integrated Likelihood Branch Averaging
# ======================================================================

#' Compute Diagnostics for Branch-Averaged Integrated Likelihood
#'
#' @description
#' Computes Monte Carlo variance, relative variance, MC SE on log scale,
#' outlier fractions, coefficient of variation squared, effective sample
#' size (ESS), and stability warnings.
#'
#' @param branch_mat Numeric K × R matrix of log-likelihood values
#'
#' @return Named list of diagnostics
#'
#' @keywords internal
compute_branch_diagnostics <- function(log_IL) {

  branch_mat <- log_IL$branch_mat

  K <- nrow(branch_mat)
  R <- ncol(branch_mat)

  # -------------------------------------------------------------
  # Convert to likelihood scale
  # -------------------------------------------------------------
  L_mat <- exp(branch_mat)             # K × R
  L_hat <- matrixStats::rowMeans2(L_mat)

  # -------------------------------------------------------------
  # MC variance of L̂_k
  # -------------------------------------------------------------
  var_L <- matrixStats::rowVars(L_mat) / R   # Var( L_hat )
  se_L  <- sqrt(var_L)

  # -------------------------------------------------------------
  # Relative MC error
  # -------------------------------------------------------------
  rel_se <- se_L / (L_hat + 1e-15)

  # -------------------------------------------------------------
  # MC SE on log-scale (delta method)
  # -------------------------------------------------------------
  se_logL <- rel_se

  # -------------------------------------------------------------
  # Outlier detection: |x - median| > 3 MAD
  # -------------------------------------------------------------
  med_vals <- matrixStats::rowMedians(branch_mat)
  mad_vals <- matrixStats::rowMads(branch_mat)

  outlier_mat  <- abs(branch_mat - med_vals) > (3 * mad_vals)
  outlier_frac <- rowMeans(outlier_mat)

  # -------------------------------------------------------------
  # Coefficient of variation squared (CV²)
  #   CV^2 = Var(L) / mean(L)^2
  # -------------------------------------------------------------
  cv2 <- matrixStats::rowVars(L_mat) / (L_hat^2 + 1e-15)

  # -------------------------------------------------------------
  # Effective sample size for MC integration
  #   ESS = R / (1 + CV²)
  # -------------------------------------------------------------
  ess <- R / (1 + cv2)

  # -------------------------------------------------------------
  # Warnings
  # -------------------------------------------------------------
  warnings <- list()

  if (any(ess < 0.10 * R)) {
    warnings <- c(warnings, "ESS < 10% of R at some ψ values.")
  }
  if (any(outlier_frac > 0.25)) {
    warnings <- c(warnings, "More than 25% outliers at some ψ values.")
  }
  if (any(rel_se > 0.10)) {
    warnings <- c(warnings, "Relative Monte Carlo error > 10% at some ψ.")
  }

  # -------------------------------------------------------------
  # Return diagnostics object
  # -------------------------------------------------------------
  list(
    R                 = R,
    se_L              = se_L,
    rel_se            = rel_se,
    se_logL           = se_logL,
    ess               = ess,
    cv2               = cv2,
    outlier_fraction  = outlier_frac,
    warnings          = warnings,
    summary = list(
      ess_min      = min(ess),
      ess_median   = median(ess),
      rel_se_max   = max(rel_se),
      outlier_max  = max(outlier_frac),
      se_logL_max  = max(se_logL)
    )
  )
}
