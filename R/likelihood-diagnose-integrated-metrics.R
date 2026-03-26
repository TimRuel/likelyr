# ================================================================================
# likelihood-diagnose-integrated-metrics.R
# Modular diagnostics computations
# ================================================================================

#' Likelihood-scale diagnostics for integrated likelihood
#'
#' @description
#' Computes likelihood-scale Monte Carlo diagnostics including standard
#' error, relative standard error, and coefficient of variation squared.
#'
#' @param branch_mat Numeric matrix of log-likelihood evaluations.
#'   Rows correspond to ψ grid points and columns to Monte Carlo draws.
#'
#' @return A named list containing:
#' \describe{
#'   \item{se_L}{Monte Carlo standard error of likelihood estimates.}
#'   \item{rel_se}{Relative Monte Carlo standard error.}
#'   \item{se_logL}{Approximate SE on log-likelihood scale.}
#'   \item{cv2}{Squared coefficient of variation of likelihood estimates.}
#' }
#'
#' @keywords internal
compute_integrated_likelihood_stats <- function(branch_mat) {
  R <- ncol(branch_mat)

  L_mat <- exp(branch_mat)
  L_hat <- matrixStats::rowMeans2(L_mat)

  var_L <- matrixStats::rowVars(L_mat) / R
  se_L <- sqrt(var_L)

  rel_se <- se_L / (L_hat + 1e-15)
  se_logL <- rel_se

  list(
    se_L = se_L,
    rel_se = rel_se,
    se_logL = se_logL,
    cv2 = matrixStats::rowVars(L_mat) / (L_hat^2 + 1e-15)
  )
}

#' Outlier diagnostics for integrated likelihood
#'
#' @description
#' Computes the fraction of Monte Carlo draws that are extreme outliers
#' at each ψ grid point using a MAD-based rule.
#'
#' @param branch_mat Numeric matrix of log-likelihood evaluations.
#'
#' @return Numeric vector of outlier fractions (one per ψ grid point).
#'
#' @keywords internal
compute_integrated_outliers <- function(branch_mat) {
  med_vals <- matrixStats::rowMedians(branch_mat)
  mad_vals <- matrixStats::rowMads(branch_mat)

  outlier_mat <- abs(branch_mat - med_vals) > (3 * mad_vals + 1e-12)
  outlier_frac <- rowMeans(outlier_mat)

  outlier_frac
}

#' Effective sample size (ESS) for integrated likelihood
#'
#' @description
#' Computes effective sample size using the coefficient of variation
#' of Monte Carlo likelihood estimates.
#'
#' @param cv2 Numeric vector of squared coefficients of variation.
#' @param R Integer number of Monte Carlo draws.
#'
#' @return Numeric vector of effective sample sizes.
#'
#' @keywords internal
compute_integrated_ess <- function(cv2, R) {
  R / (1 + cv2)
}

#' Generate diagnostic warnings for integrated likelihood
#'
#' @description
#' Evaluates diagnostic thresholds and produces human-readable warning
#' messages when issues are detected.
#'
#' @param ess Numeric vector of effective sample sizes.
#' @param outlier_frac Numeric vector of outlier fractions.
#' @param rel_se Numeric vector of relative Monte Carlo SEs.
#' @param R Integer number of Monte Carlo draws.
#'
#' @return Character vector of warning messages (possibly empty).
#'
#' @keywords internal
compute_integrated_warnings <- function(ess, outlier_frac, rel_se, R) {
  warnings <- character()

  if (any(ess < 0.10 * R, na.rm = TRUE)) {
    warnings <- c(warnings, "ESS < 10% of R at some ψ values.")
  }

  if (any(outlier_frac > 0.25, na.rm = TRUE)) {
    warnings <- c(warnings, "High outlier fraction at some ψ values.")
  }

  if (any(rel_se > 0.10, na.rm = TRUE)) {
    warnings <- c(
      warnings,
      "Relative Monte Carlo error > 10% at some ψ grid points."
    )
  }

  warnings
}

#' Omega-hat dispersion diagnostics
#'
#' @description
#' Computes covariance eigenvalues and effective rank diagnostics
#' for omega-hat Monte Carlo draws.
#'
#' @param omega_draws List of omega-hat draws (length R),
#'   each element a numeric vector of parameters.
#' @param R Integer number of Monte Carlo draws.
#'
#' @return A named list containing:
#' \describe{
#'   \item{omega_dispersion}{List of eigenvalues, effective rank, center, and collapse flag (or NULL).}
#'   \item{omega_matrix}{Numeric matrix of stacked omega draws (or NULL).}
#'   \item{warnings}{Character vector of additional warnings.}
#' }
#'
#' @keywords internal
compute_integrated_omega_dispersion <- function(omega_draws, R) {
  if (
    is.null(omega_draws) ||
      !is.list(omega_draws) ||
      length(omega_draws) != R ||
      !all(lengths(omega_draws) == length(omega_draws[[1]]))
  ) {
    return(list(
      omega_dispersion = NULL,
      omega_matrix = NULL,
      warnings = character()
    ))
  }

  J <- length(omega_draws[[1]])
  Omega <- do.call(rbind, omega_draws)

  mu <- colMeans(Omega)
  Z <- sweep(Omega, 2, mu)

  S <- crossprod(Z) / max(1, R - 1)
  eig <- eigen(S, symmetric = TRUE)

  values <- pmax(eig$values, 0)
  p <- values / (sum(values) + 1e-15)

  eff_rank <- exp(-sum(p * log(p + 1e-15)))
  collapsed <- eff_rank < max(1, 0.20 * J)

  omega_diag <- list(
    covariance_eigenvalues = values,
    effective_rank = eff_rank,
    center = mu,
    collapsed = collapsed
  )

  extra_warn <- character()
  if (collapsed) {
    extra_warn <- sprintf(
      "ω̂ manifold collapse detected: effective rank = %.2f.",
      eff_rank
    )
  }

  list(
    omega_dispersion = omega_diag,
    omega_matrix = Omega,
    warnings = extra_warn
  )
}

# ================================================================================
# END likelihood-diagnose-integrated-metrics.R
# ================================================================================
