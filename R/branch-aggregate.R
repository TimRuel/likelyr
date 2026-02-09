# ======================================================================
# Aggregate Branches (Monte Carlo Integrated Log-Likelihood)
# ======================================================================

#' Aggregate Branches (Monte Carlo Integrated Log-Likelihood)
#'
#' @description
#' Computes the Monte Carlo integrated log-likelihood curve by aggregating
#' branch-specific log-likelihood values using a ψ-wise log-mean-exp
#' estimator over retained branches:
#'
#' \deqn{
#' \log \hat L(\psi)
#' =
#' \log\left(
#' \frac{1}{|R(\psi)|}
#' \sum_{r \in R(\psi)} \exp\{\ell_r(\psi)\}
#' \right)
#' }
#'
#' The procedure operates in three stages:
#' \enumerate{
#'   \item \strong{Structural filtering} — removes malformed or too-short
#'         branches.
#'   \item \strong{Statistical filtering} — discards branches whose peak
#'         log-likelihood is far below the global peak, using an adaptive
#'         \eqn{\Delta}-cutoff.
#'   \item \strong{ψ-wise aggregation} — computes a log-mean-exp across
#'         branches that support each ψ value.
#' }
#'
#' This yields a stable approximation to the integrated log-likelihood
#' while avoiding numerical dominance by poor or spurious branches.
#'
#' @param branches
#'   List of branch tibbles. Each branch must contain at least:
#'   \itemize{
#'     \item \code{psi} — numeric ψ grid values
#'     \item \code{loglik} — branch-specific log-likelihood values
#'   }
#'   Branches may differ in ψ coverage and length.
#'
#' @param min_points
#'   Integer. Minimum number of ψ points required for a branch to be
#'   considered structurally valid. Shorter branches are discarded before
#'   any statistical filtering.
#'
#' @param q_delta
#'   Numeric in (0, 1). Quantile used to determine the adaptive likelihood
#'   cutoff across branch peak log-likelihoods. Lower values are more
#'   permissive; higher values retain only near-peak branches.
#'
#' @param delta_min
#'   Numeric. Lower bound on the adaptive \eqn{\Delta} threshold used to
#'   filter branches by peak log-likelihood difference from the global
#'   maximum.
#'
#' @param delta_max
#'   Numeric. Upper bound on the adaptive \eqn{\Delta} threshold, preventing
#'   overly permissive retention when branch peaks are highly dispersed.
#'
#' @param min_support
#'   Integer or \code{NULL}. Minimum number of branches that must contribute
#'   finite log-likelihood values at a given ψ for aggregation to occur.
#'   If \code{NULL}, defaults to \code{max(10, ceiling(0.2 * R_eff))}.
#'
#' @return
#' A list with components:
#' \itemize{
#'   \item \code{psi_ll_df} — tibble with columns \code{psi}, \code{loglik},
#'         and \code{n_support}
#'   \item \code{R_eff} — effective number of retained branches after
#'         filtering
#' }
#'
#' Additional metadata is attached as attributes on \code{psi_ll_df}:
#' \itemize{
#'   \item \code{"delta_used"} — final adaptive cutoff value
#'   \item \code{"R_eff"} — effective branch count
#'   \item \code{"type"} — always \code{"integrate"}
#' }
#'
#' @keywords internal
aggregate_branches <- function(
  branches,
  min_points = 10L,
  q_delta = 0.10,
  delta_min = 20,
  delta_max = 80,
  min_support = NULL
) {
  stopifnot(is.list(branches), length(branches) > 0)

  # =============================================================
  # 1. Structural branch filtering
  # =============================================================

  is_valid_branch <- function(br) {
    if (!is.data.frame(br)) {
      return(FALSE)
    }
    if (!all(c("psi", "loglik") %in% names(br))) {
      return(FALSE)
    }
    if (nrow(br) < min_points) {
      return(FALSE)
    }
    if (!all(is.finite(br$loglik))) {
      return(FALSE)
    }
    TRUE
  }

  branches <- Filter(is_valid_branch, branches)

  if (length(branches) == 0) {
    stop("No valid branches after structural filtering.", call. = FALSE)
  }

  # =============================================================
  # 2. Adaptive delta via branch-peak quantile
  # =============================================================

  branch_peaks <- vapply(
    branches,
    function(br) max(br$loglik),
    numeric(1)
  )

  global_peak <- max(branch_peaks)

  cutoff <- stats::quantile(
    branch_peaks,
    probs = q_delta,
    names = FALSE,
    type = 7
  )

  delta <- global_peak - cutoff
  delta <- min(max(delta, delta_min), delta_max)

  keep <- branch_peaks >= (global_peak - delta)

  branches <- branches[keep]
  branch_peaks <- branch_peaks[keep]

  R_eff <- length(branches)

  if (R_eff == 0) {
    stop("All branches discarded by adaptive delta filtering.", call. = FALSE)
  }

  # =============================================================
  # 3. ψ grid = union of retained branches
  # =============================================================

  psi_grid <- sort(unique(unlist(lapply(branches, `[[`, "psi"))))

  if (is.null(min_support)) {
    min_support <- max(10L, ceiling(0.2 * R_eff))
  }

  # =============================================================
  # 4. Build ψ × R matrix (with NA for missing support)
  # =============================================================

  branch_mat_full <- vapply(
    branches,
    function(br) {
      out <- rep(NA_real_, length(psi_grid))
      idx <- match(br$psi, psi_grid)
      out[idx[!is.na(idx)]] <- br$loglik[!is.na(idx)]
      out
    },
    numeric(length(psi_grid))
  )

  colnames(branch_mat_full) <- paste0("branch", seq_len(R_eff))

  # -------------------------------------------------------------
  # ψ-wise support filtering
  # -------------------------------------------------------------

  n_support <- rowSums(is.finite(branch_mat_full))
  keep_psi <- n_support >= min_support

  if (!any(keep_psi)) {
    stop(
      "Integrated likelihood undefined: insufficient branch support at all ψ.",
      call. = FALSE
    )
  }

  psi_used <- psi_grid[keep_psi]
  branch_mat <- branch_mat_full[keep_psi, , drop = FALSE]
  n_support <- n_support[keep_psi]

  # =============================================================
  # 5. ψ-wise log-mean-exp aggregation
  # =============================================================

  loglik <- matrixStats::rowLogSumExps(branch_mat, na.rm = TRUE) -
    log(n_support)

  psi_ll_df <- tibble::tibble(
    psi = psi_used,
    loglik = loglik,
    n_support = n_support
  )

  # =============================================================
  # 6. Attach metadata
  # =============================================================

  attr(psi_ll_df, "type") <- "integrate"
  attr(psi_ll_df, "delta_used") <- delta
  attr(psi_ll_df, "R_eff") <- R_eff

  list(
    psi_ll_df = psi_ll_df,
    branch_mat = branch_mat,
    R_eff = R_eff
  )
}
