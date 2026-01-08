# ================================================================================
# likelihood-diagnose-integrate.R
# Integrated log-likelihood diagnostics engine
# ================================================================================

#' Integrated log-likelihood diagnostics
#'
#' @description
#' Computes Monte Carlo diagnostics for an integrated log-likelihood result.
#' This function performs **no printing, plotting, or attachment**; it returns
#' a pure diagnostics list to be consumed by `diagnose()` orchestration.
#'
#' @param res An `integrate` result object.
#'
#' @return A named list of diagnostics with class-ready structure.
#'
#' @keywords internal
diagnose_integrate <- function(res) {

  branch_mat  <- res$branch_mat
  omega_draws <- res$omega_draws %||% res$omega_hats

  K <- nrow(branch_mat)
  R <- ncol(branch_mat)

  # ------------------------------------------------------------------
  # Likelihood-scale quantities
  # ------------------------------------------------------------------

  L_mat <- exp(branch_mat)
  L_hat <- matrixStats::rowMeans2(L_mat)

  var_L <- matrixStats::rowVars(L_mat) / R
  se_L  <- sqrt(var_L)

  rel_se  <- se_L / (L_hat + 1e-15)
  se_logL <- rel_se

  # ------------------------------------------------------------------
  # Outlier diagnostics (branch-wise)
  # ------------------------------------------------------------------

  med_vals <- matrixStats::rowMedians(branch_mat)
  mad_vals <- matrixStats::rowMads(branch_mat)

  outlier_mat  <- abs(branch_mat - med_vals) > (3 * mad_vals + 1e-12)
  outlier_frac <- rowMeans(outlier_mat)

  # ------------------------------------------------------------------
  # Effective sample size (ESS)
  # ------------------------------------------------------------------

  cv2 <- matrixStats::rowVars(L_mat) / (L_hat^2 + 1e-15)
  ess <- R / (1 + cv2)

  # ------------------------------------------------------------------
  # Warnings
  # ------------------------------------------------------------------

  warnings <- character()

  if (any(ess < 0.10 * R, na.rm = TRUE))
    warnings <- c(warnings, "ESS < 10% of R at some ψ values.")

  if (any(outlier_frac > 0.25, na.rm = TRUE))
    warnings <- c(warnings, "High outlier fraction at some ψ values.")

  if (any(rel_se > 0.10, na.rm = TRUE))
    warnings <- c(warnings, "Relative Monte Carlo error > 10% at some ψ grid points.")

  # ------------------------------------------------------------------
  # Omega-hat dispersion diagnostics (optional)
  # ------------------------------------------------------------------

  omega_diag   <- NULL
  omega_matrix <- NULL

  if (!is.null(omega_draws) &&
      is.list(omega_draws) &&
      length(omega_draws) == R &&
      all(lengths(omega_draws) == length(omega_draws[[1]]))) {

    J <- length(omega_draws[[1]])
    Omega <- do.call(rbind, omega_draws)
    omega_matrix <- Omega

    mu <- colMeans(Omega)
    Z  <- sweep(Omega, 2, mu)

    S <- crossprod(Z) / max(1, R - 1)
    eig <- eigen(S, symmetric = TRUE)
    values <- pmax(eig$values, 0)

    p <- values / (sum(values) + 1e-15)
    eff_rank <- exp(-sum(p * log(p + 1e-15)))

    collapsed <- eff_rank < max(1, 0.20 * J)

    if (collapsed)
      warnings <- c(
        warnings,
        sprintf(
          "ω̂ manifold collapse detected: effective rank = %.2f.",
          eff_rank
        )
      )

    omega_diag <- list(
      covariance_eigenvalues = values,
      effective_rank         = eff_rank,
      center                 = mu,
      collapsed              = collapsed
    )
  }

  # ------------------------------------------------------------------
  # Output
  # ------------------------------------------------------------------

  out <- list(
    supported = TRUE,
    R         = R,
    se_L      = se_L,
    rel_se    = rel_se,
    se_logL   = se_logL,
    ess       = ess,
    cv2       = cv2,
    outlier_fraction = outlier_frac,
    warnings  = warnings,
    summary = list(
      ess_min     = min(ess, na.rm = TRUE),
      ess_median  = stats::median(ess, na.rm = TRUE),
      rel_se_max  = max(rel_se, na.rm = TRUE),
      outlier_max = max(outlier_frac, na.rm = TRUE),
      se_logL_max = max(se_logL, na.rm = TRUE)
    ),
    omega_dispersion = omega_diag
  )

  if (!is.null(omega_matrix))
    attr(out, "omega_matrix") <- omega_matrix

  out
}

# ================================================================================
# END likelihood-diagnose-integrate.R
# ================================================================================
