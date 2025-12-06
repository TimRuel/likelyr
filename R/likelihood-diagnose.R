# ======================================================================
# likelihood-diagnostics.R — Unified diagnostics for IL + PL results
# ======================================================================

#' Diagnostics for Likelyr Results (IL or PL)
#'
#' @description
#' Scans all entries of `cal$results`, attaching a diagnostics object to
#' each likelihood result (Integrated or Profile). Integrated Likelihood
#' (IL) receives full Monte Carlo diagnostics; Profile Likelihood (PL)
#' receives a placeholder for now.
#'
#' Diagnostics are stored under:
#' \preformatted{
#'   cal$results[[name]]$diagnostics
#' }
#'
#' @param cal A `calibrated_model` that has undergone integrate() or
#'   profile().
#' @param verbose Logical; print diagnostic summaries.
#'
#' @return The same calibrated model, with class `"likelyr_diagnosed"`
#'   and diagnostics added to each result.
#'
#' @export
diagnose <- function(cal, verbose = TRUE) {
  UseMethod("diagnose")
}

#' @export
diagnose.default <- function(cal, ...) {
  stop("diagnose() requires a calibrated_model.", call. = FALSE)
}

# ======================================================================
# Main dispatcher
# ======================================================================

#' @export
diagnose.calibrated_model <- function(cal, verbose = TRUE) {

  if (!isTRUE(cal$.__calibrated__))
    stop("diagnose() requires a calibrated_model.", call. = FALSE)

  if (is.null(cal$results) || length(cal$results) == 0)
    stop("diagnose(): No likelihood results found. Run integrate() or profile().",
         call. = FALSE)

  # Loop over ALL results
  for (name in names(cal$results)) {

    res <- cal$results[[name]]
    diag_obj <- NULL

    # ------------------------------------------------------------
    # Integrated Likelihood
    # ------------------------------------------------------------
    if (inherits(res, "likelyr_il_result")) {

      diag_list <- .diagnose_il_object(res)

      diag_obj <- new_diagnostics_result(diag_list)

      # Attach omega matrix for PCA plotting
      omega_mat <- attr(diag_list, "omega_matrix", exact = TRUE)
      if (!is.null(omega_mat))
        attr(diag_obj, "omega_matrix") <- omega_mat

      # ------------------------------------------------------------
      # Profile Likelihood
      # ------------------------------------------------------------
    } else if (inherits(res, "likelyr_pl_result")) {

      diag_obj <- .diagnose_pl_object(res)

      # ------------------------------------------------------------
      # Unknown result type
      # ------------------------------------------------------------
    } else {

      warning(sprintf(
        "diagnose(): No diagnostic routine registered for result '%s'. Skipping.",
        name
      ))
      next
    }

    # Attach diagnostics to cal$results
    cal$results[[name]]$diagnostics <- diag_obj

    if (verbose) {
      cat("\n[diagnose] Diagnostics for result:", name, "\n")
      print(diag_obj)
    }
  }

  # Mark the model as diagnosed
  cal <- mark_diagnosed(cal)

  cal
}

# ======================================================================
# IL Diagnostics Engine (full)
# ======================================================================

#' Internal IL diagnostics engine
#'
#' @keywords internal
.diagnose_il_object <- function(il) {

  branch_mat  <- il$branch_mat
  omega_draws <- il$omega_draws %||% il$omega_hats

  K <- nrow(branch_mat)
  R <- ncol(branch_mat)

  # ------------------------------------------------------------
  # 1. Likelihood-scale MC diagnostics
  # ------------------------------------------------------------
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

  # ------------------------------------------------------------
  # 2. Omega-hat manifold diagnostics
  # ------------------------------------------------------------
  omega_diag <- NULL
  omega_matrix <- NULL

  if (!is.null(omega_draws)) {

    if (is.list(omega_draws) &&
        length(omega_draws) == R &&
        all(lengths(omega_draws) == length(omega_draws[[1]]))) {

      J <- length(omega_draws[[1]])

      Omega <- do.call(rbind, omega_draws)  # R × J
      omega_matrix <- Omega

      # Centered
      mu <- colMeans(Omega)
      Z  <- sweep(Omega, 2, mu)

      S <- crossprod(Z) / max(1, (R - 1))
      eig <- eigen(S, symmetric = TRUE)
      values <- pmax(eig$values, 0)

      # Effective rank
      p <- values / (sum(values) + 1e-15)
      eff_rank <- exp(-sum(p * log(p + 1e-15)))

      # Distances
      if (R <= 300) {
        D <- as.matrix(dist(Omega))
        dist_mean <- mean(D)
        dist_min  <- min(D[D > 0])
      } else {
        dist_mean <- NA_real_
        dist_min  <- NA_real_
      }

      collapsed <- eff_rank < max(1, 0.20 * J)

      if (collapsed) {
        warnings <- c(
          warnings,
          sprintf("ω̂ manifold collapse detected: effective rank = %.2f.", eff_rank)
        )
      }

      omega_diag <- list(
        covariance_eigenvalues = values,
        effective_rank         = eff_rank,
        mean_pairwise_dist     = dist_mean,
        min_nonzero_dist       = dist_min,
        center                 = mu,
        collapsed              = collapsed
      )
    }
  }

  out <- list(
    R                = R,
    se_L             = se_L,
    rel_se           = rel_se,
    se_logL          = se_logL,
    ess              = ess,
    cv2              = cv2,
    outlier_fraction = outlier_frac,
    warnings         = warnings,
    summary = list(
      ess_min      = min(ess, na.rm = TRUE),
      ess_median   = stats::median(ess, na.rm = TRUE),
      rel_se_max   = max(rel_se, na.rm = TRUE),
      outlier_max  = max(outlier_frac, na.rm = TRUE),
      se_logL_max  = max(se_logL, na.rm = TRUE)
    ),
    omega_dispersion = omega_diag
  )

  if (!is.null(omega_matrix))
    attr(out, "omega_matrix") <- omega_matrix

  out
}

# ======================================================================
# PL Diagnostics (placeholder)
# ======================================================================

#' Placeholder diagnostics for Profile Likelihood
#'
#' @keywords internal
.diagnose_pl_object <- function(pl) {

  diag <- list(
    type       = "Profile Likelihood",
    supported  = FALSE,
    message    = "Diagnostics for profile likelihood are not yet implemented.",
    warnings   = "No diagnostic computations were performed."
  )

  new_diagnostics_result(diag)
}

# ======================================================================
# S3 Print Method
# ======================================================================

#' @export
print.likelyr_diagnostics <- function(x, ...) {

  cat("<likelyr_diagnostics>\n")

  if (!is.null(x$type) && x$type == "Profile Likelihood") {
    cat("  Type: Profile Likelihood (placeholder)\n")
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

# ======================================================================
# S3 Plot Method
# ======================================================================

#' @export
plot.likelyr_diagnostics <- function(x, ...) {

  plots <- list()

  # PL results are empty
  if (!is.null(x$type) && x$type == "Profile Likelihood") {
    message("No diagnostic plots available for profile likelihood (placeholder).")
    return(invisible(plots))
  }

  R            <- x$R
  ess          <- x$ess
  rel_se       <- x$rel_se
  outlier_frac <- x$outlier_fraction
  omega_diag   <- x$omega_dispersion
  Omega        <- attr(x, "omega_matrix", exact = TRUE)

  # ESS ---------------------------------------------------------
  if (!is.null(ess)) {
    df <- tibble::tibble(idx = seq_along(ess), ess = ess)
    p <- plot_base() +
      ggplot2::geom_line(data = df, ggplot2::aes(x = idx, y = ess), color = "#66d9ef") +
      ggplot2::labs(title = "Effective Sample Size", x = "Grid index", y = "ESS")
    print(p); plots$ess <- p
  }

  # Relative SE ------------------------------------------------
  if (!is.null(rel_se)) {
    df <- tibble::tibble(idx = seq_along(rel_se), rel_se = rel_se)
    p <- plot_base() +
      ggplot2::geom_line(data = df, ggplot2::aes(x = idx, y = rel_se), color = "#a6e22e") +
      ggplot2::labs(title = "Relative Monte Carlo SE", x = "Grid index", y = "Rel SE")
    print(p); plots$rel_se <- p
  }

  # Outliers ----------------------------------------------------
  if (!is.null(outlier_frac)) {
    df <- tibble::tibble(idx = seq_along(outlier_frac), outliers = outlier_frac)
    p <- plot_base() +
      ggplot2::geom_line(data = df, ggplot2::aes(x = idx, y = outliers), color = "#fd971f") +
      ggplot2::labs(title = "Outlier Fraction", x = "Grid index", y = "Fraction")
    print(p); plots$outliers <- p
  }

  # Eigenvalues -------------------------------------------------
  if (!is.null(omega_diag) && !is.null(omega_diag$covariance_eigenvalues)) {
    eig <- omega_diag$covariance_eigenvalues
    df <- tibble::tibble(k = seq_along(eig), eig = eig)
    p <- plot_base() +
      ggplot2::geom_point(data = df, ggplot2::aes(x = k, y = eig), color = "#ff5d00") +
      ggplot2::geom_line(data = df, ggplot2::aes(x = k, y = eig), color = "#ff5d00") +
      ggplot2::labs(title = "Omega-Hat Covariance Eigenvalues",
                    x = "Eigenvalue index", y = "Eigenvalue")
    print(p); plots$eigenvalues <- p
  }

  # PCA Scatter -------------------------------------------------
  if (!is.null(Omega) && ncol(Omega) >= 2) {
    pca <- stats::prcomp(Omega, scale. = TRUE)
    df <- tibble::tibble(PC1 = pca$x[,1], PC2 = pca$x[,2])
    p <- plot_base() +
      ggplot2::geom_point(data = df, ggplot2::aes(x = PC1, y = PC2), color = "#ae81ff") +
      ggplot2::labs(title = "Omega-Hat PCA Scatter", x = "PC1", y = "PC2")
    print(p); plots$omega_pca <- p
  }

  invisible(plots)
}

# ======================================================================
# END likelihood-diagnostics.R
# ======================================================================
