# ================================================================================
# likelihood-diagnostics.R — Unified diagnostics for integrated + profile results
# ================================================================================

# ================================================================================
# Public API
# ================================================================================

#' Diagnostics for Likelyr Results
#'
#' @description
#' Attaches diagnostics to each likelihood result (integrated or profile)
#' stored in a calibrated model. Integrated likelihood receives full Monte
#' Carlo diagnostics; profile likelihood currently receives a placeholder.
#'
#' Diagnostics are attached at:
#' \preformatted{
#'   cal$workspace[[name]]$diagnostics
#' }
#'
#' @param cal A `calibrated` model object with pseudolikelihood results.
#' @param verbose Logical; print diagnostic summaries.
#'
#' @return The same `calibrated` model object with individual pseudolikelihood
#' results each marked as diagnosed.
#'
#' @export
diagnose <- function(cal, verbose = TRUE) {
  UseMethod("diagnose")
}

#' @export
diagnose.default <- function(cal, ...) {
  stop("diagnose() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
diagnose.calibrated <- function(cal, verbose = TRUE) {

  validate_diagnose_input(cal)

  for (name in names(cal$workspace)) {

    res <- cal$workspace[[name]]

    diag_list <- if (inherits(res, "integrate")) {
      .diagnose_integrate_result(res)
    } else if (inherits(res, "profile")) {
      .diagnose_profile_result(res)
    } else {
      stop(
        "diagnose(): Unsupported result type for '", name, "'.",
        call. = FALSE
      )
    }

    diag_obj <- new_diagnostics_result(diag_list)

    # propagate omega matrix if present
    omega_mat <- attr(diag_list, "omega_matrix", exact = TRUE)
    if (!is.null(omega_mat))
      attr(diag_obj, "omega_matrix") <- omega_mat

    res$diagnostics <- diag_obj

    cal$workspace[[name]] <- mark_diagnosed(res)

    if (verbose) {
      cat("\n[diagnose] Diagnostics for result:", name, "\n")
      print(diag_obj)
    }
  }

  cal
}

# ================================================================================
# Validation
# ================================================================================

validate_diagnose_input <- function(cal) {

  if (!is_calibrated(cal))
    stop("diagnose() requires a calibrated model.", call. = FALSE)

  if (is.null(cal$workspace) || length(cal$workspace) == 0)
    stop(
      "diagnose(): No pseudolikelihood results found. ",
      "Run integrate() or profile() first.",
      call. = FALSE
    )

  invisible(TRUE)
}

# ================================================================================
# Diagnostics Engines
# ================================================================================

#' Integrated log-likelihood diagnostics (full)
#'
#' @keywords internal
.diagnose_integrate_result <- function(res) {

  branch_mat  <- res$branch_mat
  omega_draws <- res$omega_draws %||% res$omega_hats

  K <- nrow(branch_mat)
  R <- ncol(branch_mat)

  L_mat <- exp(branch_mat)
  L_hat <- matrixStats::rowMeans2(L_mat)

  var_L <- matrixStats::rowVars(L_mat) / R
  se_L  <- sqrt(var_L)

  rel_se  <- se_L / (L_hat + 1e-15)
  se_logL <- rel_se

  med_vals <- matrixStats::rowMedians(branch_mat)
  mad_vals <- matrixStats::rowMads(branch_mat)

  outlier_mat  <- abs(branch_mat - med_vals) > (3 * mad_vals + 1e-12)
  outlier_frac <- rowMeans(outlier_mat)

  cv2 <- matrixStats::rowVars(L_mat) / (L_hat^2 + 1e-15)
  ess <- R / (1 + cv2)

  warnings <- character()
  if (any(ess < 0.10 * R, na.rm = TRUE))
    warnings <- c(warnings, "ESS < 10% of R at some ψ values.")
  if (any(outlier_frac > 0.25, na.rm = TRUE))
    warnings <- c(warnings, "High outlier fraction at some ψ values.")
  if (any(rel_se > 0.10, na.rm = TRUE))
    warnings <- c(warnings, "Relative Monte Carlo error > 10% at some ψ grid points.")

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
        sprintf("ω̂ manifold collapse detected: effective rank = %.2f.", eff_rank)
      )

    omega_diag <- list(
      covariance_eigenvalues = values,
      effective_rank         = eff_rank,
      center                 = mu,
      collapsed              = collapsed
    )
  }

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

#' Profile log-likelihood diagnostics (placeholder)
#'
#' @keywords internal
.diagnose_profile_result <- function(res) {
  list(
    supported = FALSE,
    message   = "Diagnostics for profile log-likelihood are not yet implemented.",
    warnings  = "No diagnostic computations were performed."
  )
}

# ================================================================================
# S3 Methods
# ================================================================================

# ----------------------------------------------------------------------
# Print
# ----------------------------------------------------------------------

#' @export
print.diagnostics <- function(x, ...) {

  cat("<diagnostics>\n")

  if (!isTRUE(x$supported)) {
    cat("  Type: Profile Log-Likelihood (placeholder)\n")
    cat("  Message: ", x$message, "\n", sep = "")
    return(invisible(x))
  }

  cat("  R (branches): ", x$R, "\n", sep = "")
  cat("  ESS (min):    ", sprintf("%.1f", x$summary$ess_min), "\n", sep = "")
  cat("  ESS (median): ", sprintf("%.1f", x$summary$ess_median), "\n", sep = "")
  cat("  Rel SE max:   ", sprintf("%.3f", x$summary$rel_se_max), "\n", sep = "")
  cat("  Outlier max:  ", sprintf("%.3f", x$summary$outlier_max), "\n", sep = "")

  if (!is.null(x$omega_dispersion)) {
    cat("  Omega-hat manifold:\n")
    cat("    Effective rank: ",
        sprintf("%.2f", x$omega_dispersion$effective_rank), "\n", sep = "")
    cat("    Collapsed:      ",
        if (x$omega_dispersion$collapsed) "YES" else "no", "\n", sep = "")
  }

  if (length(x$warnings) > 0) {
    cat("\n  Warnings:\n")
    for (w in x$warnings)
      cat("   • ", w, "\n", sep = "")
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------

#' @export
summary.diagnostics <- function(object, ...) {

  out <- list(
    supported = object$supported,
    summary   = object$summary %||% NULL,
    warnings  = object$warnings
  )

  class(out) <- "summary_diagnostics"
  out
}

#' @export
print.summary_diagnostics <- function(x, ...) {

  cat("<summary of diagnostics>\n\n")

  if (!isTRUE(x$supported)) {
    cat("Diagnostics not supported for this likelihood.\n")
    return(invisible(x))
  }

  if (!is.null(x$summary)) {
    for (nm in names(x$summary)) {
      cat("• ", nm, ": ", format(x$summary[[nm]]), "\n", sep = "")
    }
  }

  if (length(x$warnings) > 0) {
    cat("\nWarnings:\n")
    for (w in x$warnings)
      cat(" • ", w, "\n", sep = "")
  }

  invisible(x)
}

# ----------------------------------------------------------------------
# Plot (unified with plot utilities)
# ----------------------------------------------------------------------

#' @export
plot.diagnostics <- function(x, ...) {

  if (!isTRUE(x$supported))
    return(list())

  plots <- list()

  idx <- seq_along(x$ess)

  plots$ess <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, ess = x$ess),
      ggplot2::aes(x = idx, y = ess)
    ) +
    ggplot2::labs(title = "Effective Sample Size", x = "Grid index", y = "ESS")

  plots$rel_se <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, rel_se = x$rel_se),
      ggplot2::aes(x = idx, y = rel_se)
    ) +
    ggplot2::labs(title = "Relative Monte Carlo SE", x = "Grid index", y = "Rel SE")

  plots$outliers <- plot_base() +
    ggplot2::geom_line(
      data = tibble::tibble(idx = idx, outlier = x$outlier_fraction),
      ggplot2::aes(x = idx, y = outlier)
    ) +
    ggplot2::labs(title = "Outlier Fraction", x = "Grid index", y = "Fraction")

  omega_diag <- x$omega_dispersion
  Omega      <- attr(x, "omega_matrix", exact = TRUE)

  if (!is.null(omega_diag) && !is.null(omega_diag$covariance_eigenvalues)) {
    eig <- omega_diag$covariance_eigenvalues
    plots$eigenvalues <- plot_base() +
      ggplot2::geom_point(
        data = tibble::tibble(k = seq_along(eig), eig = eig),
        ggplot2::aes(x = k, y = eig)
      ) +
      ggplot2::geom_line(
        data = tibble::tibble(k = seq_along(eig), eig = eig),
        ggplot2::aes(x = k, y = eig)
      ) +
      ggplot2::labs(
        title = "Omega-Hat Covariance Eigenvalues",
        x = "Index", y = "Eigenvalue"
      )
  }

  if (!is.null(Omega) && ncol(Omega) >= 2) {
    pca <- stats::prcomp(Omega, scale. = TRUE)
    plots$omega_pca <- plot_base() +
      ggplot2::geom_point(
        data = tibble::tibble(PC1 = pca$x[,1], PC2 = pca$x[,2]),
        ggplot2::aes(x = PC1, y = PC2)
      ) +
      ggplot2::labs(title = "Omega-Hat PCA Scatter", x = "PC1", y = "PC2")
  }

  plots
}

# ================================================================================
# END likelihood-diagnostics.R
# ================================================================================
