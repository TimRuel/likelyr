# ================================================================================
# likelihood-diagnose-integrate.R
# Integrated log-likelihood diagnostics engine
# ================================================================================

#' Integrated log-likelihood diagnostics
#'
#' @description
#' Computes Monte Carlo diagnostics for an integrated log-likelihood result
#' and constructs diagnostic plots *as soon as their inputs are available*.
#'
#' Diagnostics are computed via modular metric helpers, and plots are built
#' incrementally during execution. This function performs no printing or
#' attachment; it returns a fully-formed diagnostics object to be consumed
#' by `diagnose()`.
#'
#' @param res An `integrate` result object.
#'
#' @return A named list containing diagnostics metrics, warnings,
#' summaries, and pre-built ggplot objects.
#'
#' @keywords internal
diagnose_integrate <- function(res) {
  branch_mat <- res$branch_mat
  psi_ll_df <- res$psi_ll_df
  omega_draws <- res$omega_draws %||% res$omega_hats

  # ------------------------------------------------------------------
  # Basic validation
  # ------------------------------------------------------------------

  stopifnot(
    is.matrix(branch_mat),
    ncol(branch_mat) > 1
  )

  R <- ncol(branch_mat)

  plots <- list()

  # ------------------------------------------------------------------
  # Omega-hat Log-Likelihood Branches Plot
  # ------------------------------------------------------------------

  if (!is.null(psi_ll_df) && "psi" %in% names(psi_ll_df)) {
    plots$omega_branches <- build_integrate_omega_branch_plot(
      branch_mat,
      as.numeric(psi_ll_df$psi)
    )
  }

  # ------------------------------------------------------------------
  # Likelihood-scale metrics
  # ------------------------------------------------------------------

  ll_stats <- compute_integrate_likelihood_stats(branch_mat)

  # → plots that depend only on ll_stats
  plots$rel_se <- build_integrate_rel_se_plot(ll_stats$rel_se)

  # ------------------------------------------------------------------
  # Outliers
  # ------------------------------------------------------------------

  outlier_frac <- compute_integrate_outliers(branch_mat)

  plots$outliers <- build_integrate_outlier_plot(outlier_frac)

  # ------------------------------------------------------------------
  # ESS
  # ------------------------------------------------------------------

  ess <- compute_integrate_ess(ll_stats$cv2, R)

  plots$ess <- build_integrate_ess_plot(ess)

  # ------------------------------------------------------------------
  # Core warnings
  # ------------------------------------------------------------------

  warn_core <- compute_integrate_warnings(
    ess = ess,
    outlier_frac = outlier_frac,
    rel_se = ll_stats$rel_se,
    R = R
  )

  # ------------------------------------------------------------------
  # Omega dispersion
  # ------------------------------------------------------------------

  omega_res <- compute_integrate_omega_dispersion(
    omega_draws = omega_draws,
    R = R
  )

  if (!is.null(omega_res$omega_dispersion)) {
    plots$omega_eig <- build_integrate_omega_eigen_plot(
      omega_res$omega_dispersion$covariance_eigenvalues
    )
  }

  if (!is.null(omega_res$omega_matrix)) {
    plots$omega_pca <- build_integrate_omega_pca_plot(
      omega_res$omega_matrix
    )
  }

  warnings <- c(warn_core, omega_res$warnings)

  # ------------------------------------------------------------------
  # Assemble output
  # ------------------------------------------------------------------

  out <- list(
    supported = TRUE,
    R = R,
    se_L = ll_stats$se_L,
    rel_se = ll_stats$rel_se,
    se_logL = ll_stats$se_logL,
    ess = ess,
    cv2 = ll_stats$cv2,
    outlier_fraction = outlier_frac,
    warnings = warnings,
    summary = list(
      ess_min = min(ess, na.rm = TRUE),
      ess_median = stats::median(ess, na.rm = TRUE),
      rel_se_max = max(ll_stats$rel_se, na.rm = TRUE),
      outlier_max = max(outlier_frac, na.rm = TRUE),
      se_logL_max = max(ll_stats$se_logL, na.rm = TRUE)
    ),
    omega_dispersion = omega_res$omega_dispersion,
    plots = plots
  )

  if (!is.null(omega_res$omega_matrix)) {
    attr(out, "omega_matrix") <- omega_res$omega_matrix
  }

  # drop any NULL plots (defensive)
  out$plots <- out$plots[
    !vapply(out$plots, is.null, logical(1))
  ]

  out
}

# ================================================================================
# END likelihood-diagnose-integrate.R
# ================================================================================
