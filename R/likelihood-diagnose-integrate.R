# ================================================================================
# likelihood-diagnose-integrate.R
# Integrated log-likelihood diagnostics engine (HPC-safe: compute-only)
# ================================================================================

#' Integrated log-likelihood diagnostics
#'
#' @description
#' Computes Monte Carlo diagnostics for an integrated log-likelihood result.
#'
#' This function is **HPC-safe**: it performs **no plotting** and does not
#' require `ggplot2`. Any plots are materialized later by `plot(diagnostics)`
#' via `build_diagnostics_plots()`, which is local-only.
#'
#' @param res An `integrate` result object.
#'
#' @return A named list containing diagnostics metrics, warnings, summaries,
#' and plot-ready inputs (but no ggplot objects).
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

  # ------------------------------------------------------------------
  # Likelihood-scale metrics
  # ------------------------------------------------------------------

  ll_stats <- compute_integrate_likelihood_stats(branch_mat)

  # ------------------------------------------------------------------
  # Outliers
  # ------------------------------------------------------------------

  outlier_frac <- compute_integrate_outliers(branch_mat)

  # ------------------------------------------------------------------
  # ESS
  # ------------------------------------------------------------------

  ess <- compute_integrate_ess(ll_stats$cv2, R)

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

  warnings <- c(warn_core, omega_res$warnings)

  # ------------------------------------------------------------------
  # Plot-ready inputs (no ggplot objects)
  # ------------------------------------------------------------------

  plot_data <- list(
    omega_branches = list(
      branch_mat = branch_mat,
      psi = if (!is.null(psi_ll_df) && "psi" %in% names(psi_ll_df)) {
        as.numeric(psi_ll_df$psi)
      } else {
        NULL
      }
    ),
    rel_se = ll_stats$rel_se,
    outliers = outlier_frac,
    ess = ess,
    omega_eigenvalues = omega_res$omega_dispersion$covariance_eigenvalues %||%
      NULL,
    omega_matrix = omega_res$omega_matrix %||% NULL
  )

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
      ess_median = median(ess, na.rm = TRUE),
      rel_se_max = max(ll_stats$rel_se, na.rm = TRUE),
      outlier_max = max(outlier_frac, na.rm = TRUE),
      se_logL_max = max(ll_stats$se_logL, na.rm = TRUE)
    ),
    omega_dispersion = omega_res$omega_dispersion,
    plot_data = plot_data
  )

  out
}

# ================================================================================
# Plot materialization (local-only)
# ================================================================================
# NOTE:
#   This is called by plot.diagnostics() via build_diagnostics_plots(x).
#   It assumes .assert_local_plotting() enforces local-only execution.

#' @keywords internal
#' @noRd
build_diagnostics_plots_integrate <- function(diag) {
  .assert_local_plotting()

  if (!isTRUE(diag$supported)) {
    stop("Diagnostics plots not supported for this likelihood.", call. = FALSE)
  }

  pd <- diag$plot_data %||% list()
  plots <- list()

  # --------------------------------------------------
  # Omega-hat branches (requires psi + branch_mat)
  # --------------------------------------------------
  if (
    !is.null(pd$omega_branches$psi) && !is.null(pd$omega_branches$branch_mat)
  ) {
    plots$omega_branches <- build_integrate_omega_branch_plot(
      pd$omega_branches$branch_mat,
      pd$omega_branches$psi
    )
  }

  # --------------------------------------------------
  # Relative SE
  # --------------------------------------------------
  if (!is.null(pd$rel_se)) {
    plots$rel_se <- build_integrate_rel_se_plot(pd$rel_se)
  }

  # --------------------------------------------------
  # Outliers
  # --------------------------------------------------
  if (!is.null(pd$outliers)) {
    plots$outliers <- build_integrate_outlier_plot(pd$outliers)
  }

  # --------------------------------------------------
  # ESS
  # --------------------------------------------------
  if (!is.null(pd$ess)) {
    plots$ess <- build_integrate_ess_plot(pd$ess)
  }

  # --------------------------------------------------
  # Omega eigenvalues + PCA
  # --------------------------------------------------
  if (!is.null(pd$omega_eigenvalues)) {
    plots$omega_eig <- build_integrate_omega_eigen_plot(pd$omega_eigenvalues)
  }

  if (!is.null(pd$omega_matrix)) {
    plots$omega_pca <- build_integrate_omega_pca_plot(pd$omega_matrix)
  }

  # drop any NULL plots (defensive)
  plots <- plots[!vapply(plots, is.null, logical(1))]
  plots
}

# ================================================================================
# END likelihood-diagnose-integrate.R
# ================================================================================
