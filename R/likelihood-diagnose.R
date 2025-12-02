# ======================================================================
# Diagnostics for Monte Carlo Integrated Likelihood Branch Averaging (v2.0)
# ======================================================================

#' Compute Diagnostics for Branch-Averaged Integrated Likelihood
#'
#' @description
#' Computes:
#'   • Monte Carlo variance and relative error
#'   • log-scale MC SE (delta)
#'   • outlier fractions
#'   • coefficient of variation squared
#'   • effective sample size (ESS)
#'   • **NEW:** manifold-dispersion diagnostics for omega_hat samples
#'
#' If `log_IL$omega_hats` is present, geometric coverage of the
#' ψ-constraint manifold is analyzed via:
#'   • pairwise distances
#'   • covariance eigenvalues
#'   • effective rank
#'   • manifold collapse warnings
#'
#' @param log_IL A list containing at minimum branch_mat (K × R).
#'               Optionally contains `omega_hats`, a list of length R
#'               giving ω̂ for each Monte Carlo branch.
#'
#' @return Named list of diagnostics.
#'
#' @keywords internal
diagnose <- function(log_IL) {

  branch_mat <- log_IL$branch_mat

  K <- nrow(branch_mat)
  R <- ncol(branch_mat)

  # ============================================================
  # Likelihood-scale diagnostics (unchanged from your v1.0)
  # ============================================================

  L_mat <- exp(branch_mat)
  L_hat <- matrixStats::rowMeans2(L_mat)

  var_L <- matrixStats::rowVars(L_mat) / R
  se_L  <- sqrt(var_L)

  rel_se <- se_L / (L_hat + 1e-15)
  se_logL <- rel_se   # delta method

  med_vals <- matrixStats::rowMedians(branch_mat)
  mad_vals <- matrixStats::rowMads(branch_mat)

  outlier_mat  <- abs(branch_mat - med_vals) > (3 * mad_vals)
  outlier_frac <- rowMeans(outlier_mat)

  cv2 <- matrixStats::rowVars(L_mat) / (L_hat^2 + 1e-15)
  ess <- R / (1 + cv2)

  warnings <- list()

  if (any(ess < 0.10 * R))
    warnings <- c(warnings, "ESS < 10% of R at some ψ values.")
  if (any(outlier_frac > 0.25))
    warnings <- c(warnings, "More than 25% outliers at some ψ values.")
  if (any(rel_se > 0.10))
    warnings <- c(warnings, "Relative Monte Carlo error > 10% at some ψ.")

  # ============================================================
  # NEW: Manifold-dispersion diagnostics (if available)
  # ============================================================

  omega_diag <- NULL

  if (!is.null(log_IL$omega_hats)) {

    omega_list <- log_IL$omega_hats   # length R
    J <- length(omega_list[[1]])

    # Build matrix: R × J
    Omega <- do.call(rbind, omega_list)

    # Center
    mu <- colMeans(Omega)
    Z  <- sweep(Omega, 2, mu)

    # Covariance
    S <- crossprod(Z) / (R - 1)

    # Eigen decomposition
    eig <- eigen(S, symmetric = TRUE)
    values <- eig$values

    # Effective rank (Roy & Vetterli heuristic)
    p <- values / sum(values + 1e-15)
    eff_rank <- exp(-sum(p * log(p + 1e-15)))

    # Pairwise distances (summary only)
    # WARNING: O(R^2) — safe for R up to ~200–300
    if (R <= 300) {
      D <- as.matrix(dist(Omega))
      dist_mean <- mean(D)
      dist_min  <- min(D[D > 0])
    } else {
      D <- NULL
      dist_mean <- NA
      dist_min  <- NA
    }

    # Collapse indicator
    collapse_flag <- (eff_rank < max(1, 0.20 * J))

    if (collapse_flag) {
      warnings <- c(
        warnings,
        sprintf("omega_hat manifold collapse detected: effective rank = %.2f.", eff_rank)
      )
    }

    omega_diag <- list(
      covariance_eigenvalues = values,
      effective_rank         = eff_rank,
      mean_pairwise_dist     = dist_mean,
      min_nonzero_dist       = dist_min,
      center                 = mu,
      collapsed              = collapse_flag
    )
  }

  # ============================================================
  # Return full diagnostics
  # ============================================================
  list(
    # Standard IL diagnostics
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
    ),

    # NEW: manifold diagnostics
    omega_dispersion = omega_diag
  )
}
