# ================================================================================
# likelihood-diagnose-integrated.R
# Integrated likelihood diagnostics engine and plot dispatcher
# ================================================================================

# ================================================================================
# Diagnostics engine (HPC-safe: compute-only)
# ================================================================================

#' Integrated log-likelihood diagnostics
#'
#' @description
#' Computes Monte Carlo diagnostics for an integrated log-likelihood result.
#'
#' This function is **HPC-safe**: it performs **no plotting** and does not
#' require `ggplot2`. Plots are materialized later by `plot(diagnostics)`
#' via `build_diagnostics_plots_integrated()`, which is local-only.
#'
#' @param res An `integrated` result object.
#'
#' @return A named list containing diagnostics metrics, warnings, summaries,
#'   and plot-ready inputs (but no ggplot objects).
#'
#' @keywords internal
diagnose_integrated <- function(res) {
  branch_mat <- res$branch_mat
  psi_loglik_df <- res$psi_loglik_df
  omega_draws <- res$omega_draws %||% res$omega_hats

  # Extract omega hats from branch seeds if not already available
  if (is.null(omega_draws) && !is.null(res$cache$branch_seeds)) {
    omega_draws <- purrr::map(res$cache$branch_seeds, \(s) s$omega_hat)
  }

  stopifnot(
    is.matrix(branch_mat),
    ncol(branch_mat) > 1
  )

  R <- ncol(branch_mat)

  ll_stats <- compute_integrated_likelihood_stats(branch_mat)
  outlier_frac <- compute_integrated_outliers(branch_mat)
  ess <- compute_integrated_ess(ll_stats$cv2, R)
  warn_core <- compute_integrated_warnings(
    ess = ess,
    outlier_frac = outlier_frac,
    rel_se = ll_stats$rel_se,
    R = R
  )
  omega_res <- compute_integrated_omega_dispersion(omega_draws, R)
  warnings <- c(warn_core, omega_res$warnings)

  psi_vals <- if (!is.null(psi_loglik_df) && "psi" %in% names(psi_loglik_df)) {
    as.numeric(psi_loglik_df$psi)
  } else {
    NULL
  }

  plot_data <- list(
    omega_branches = list(branch_mat = branch_mat, psi = psi_vals),
    rel_se = ll_stats$rel_se,
    outliers = outlier_frac,
    ess = ess,
    omega_eigenvalues = omega_res$omega_dispersion$covariance_eigenvalues %||%
      NULL,
    omega_matrix = omega_res$omega_matrix %||% NULL
  )

  list(
    supported = TRUE,
    pseudolikelihood = "integrated",
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
      ess_median = median(ess, na.rm = TRUE),
      rel_se_max = max(ll_stats$rel_se, na.rm = TRUE),
      outlier_max = max(outlier_frac, na.rm = TRUE),
      se_logL_max = max(ll_stats$se_logL, na.rm = TRUE)
    ),
    omega_dispersion = omega_res$omega_dispersion,
    plot_data = plot_data
  )
}

# ================================================================================
# Plot dispatcher (local-only)
# ================================================================================

#' Build diagnostics plots for integrated likelihood
#'
#' @description
#' Called by `build_diagnostic_plots()` and `plot.diagnostic()`.
#' Assumes `.assert_local_plotting()` has already been enforced.
#' Individual plot builders live in likelihood-diagnose-integrated-plots.R.
#'
#' @param diag A `diagnostic` result object for integrated likelihood.
#'
#' @return A named list of ggplot objects.
#'
#' @keywords internal
#' @noRd
build_diagnostics_plots_integrated <- function(diag) {
  .assert_local_plotting()

  if (!isTRUE(diag$supported)) {
    stop("Diagnostics plots not supported for this likelihood.", call. = FALSE)
  }

  pd <- diag$plot_data %||% list()
  plots <- list()

  if (
    !is.null(pd$omega_branches$psi) && !is.null(pd$omega_branches$branch_mat)
  ) {
    plots$omega_branches <- build_integrated_omega_branch_plot(
      pd$omega_branches$branch_mat,
      pd$omega_branches$psi
    )
    plots$branch_distribution <- build_integrated_branch_distribution_plot(
      pd$omega_branches$branch_mat,
      pd$omega_branches$psi
    )
  }

  if (!is.null(pd$rel_se)) {
    plots$rel_se <- build_integrated_rel_se_plot(pd$rel_se)
  }
  if (!is.null(pd$outliers)) {
    plots$outliers <- build_integrated_outlier_plot(pd$outliers)
  }
  if (!is.null(pd$ess)) {
    plots$ess <- build_integrated_ess_plot(pd$ess)
  }
  if (!is.null(pd$omega_eigenvalues)) {
    plots$omega_eig <- build_integrated_omega_eigen_plot(pd$omega_eigenvalues)
  }

  if (!is.null(pd$omega_matrix)) {
    plots$omega_pca <- build_integrated_omega_pca_plot(pd$omega_matrix)
    plots$omega_mahalanobis <- build_integrated_omega_mahalanobis_plot(
      pd$omega_matrix
    )
  }

  plots[!vapply(plots, is.null, logical(1))]
}

# ================================================================================
# END likelihood-diagnose-integrated.R
# ================================================================================
