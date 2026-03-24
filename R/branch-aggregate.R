# ======================================================================
# branch-aggregate.R — Branch Aggregation
# ======================================================================

#' Aggregate Branches into Integrated Log-Likelihood Curve
#'
#' @description
#' Computes a pointwise average log-likelihood curve from all branches:
#'
#' \deqn{
#'   \log \bar{L}(\psi)
#'   = \log \left(
#'     \frac{1}{R} \sum_{r=1}^{R}
#'     \exp\{\ell_r(\psi)\}
#'   \right)
#' }
#'
#' All branches share a common ψ grid derived by \code{preprocess()},
#' so every branch contributes at every ψ point. The \code{n_support}
#' column in the result records the actual number of contributing
#' branches at each point as a diagnostic.
#'
#' A floor warning is issued when the median \code{n_support} falls
#' below \code{cal$execution$min_branches}.
#'
#' @param cal     A \code{calibrated} model object.
#' @param verbose Logical. Default: \code{FALSE}.
#'
#' @return The SAME \code{calibrated} model object, with
#'   \code{cal$workspace$integrate$result} updated.
#'
#' @importFrom stats median qchisq qchisq
#' @export
aggregate <- function(cal, ...) {
  UseMethod("aggregate")
}

#' @export
aggregate.default <- function(cal, ...) {
  stop("aggregate() requires a 'calibrated' model object.", call. = FALSE)
}

#' @export
aggregate.calibrated <- function(cal, verbose = FALSE, ...) {
  branches <- cal$workspace$integrate$branches %||% NULL
  min_branches <- cal$execution$min_branches

  if (is.null(branches) || length(branches) == 0L) {
    stop(
      "aggregate() requires pre-computed branches.\n",
      "Run integrate(cal) before aggregate().",
      call. = FALSE
    )
  }

  R <- length(branches)

  # -------------------------------------------------------------------
  # 1. Build ψ × R matrix over union of all branches' support
  # -------------------------------------------------------------------
  psi_grid <- sort(unique(unlist(lapply(branches, function(b) b$psi))))

  branch_mat <- vapply(
    branches,
    function(b) {
      out <- rep(NA_real_, length(psi_grid))
      idx <- match(b$psi, psi_grid)
      valid <- !is.na(idx)
      out[idx[valid]] <- b$loglik[valid]
      out
    },
    numeric(length(psi_grid))
  )

  # -------------------------------------------------------------------
  # 2. Pointwise log-mean-exp
  # -------------------------------------------------------------------
  alpha_target <- min(1 - cal$traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * cal$traversal$cutoff_buffer

  n_support <- rowSums(is.finite(branch_mat))
  loglik <- matrixStats::rowLogSumExps(branch_mat, na.rm = TRUE) -
    log(n_support)

  loglik_centered <- loglik - max(loglik, na.rm = TRUE)

  psi_ll_df <- tibble::tibble(
    psi = psi_grid,
    loglik = loglik,
    loglik_centered = loglik_centered,
    above_crit = loglik_centered >= -effective_crit,
    n_support = n_support
  )

  # -------------------------------------------------------------------
  # 3. Floor check
  # -------------------------------------------------------------------
  med_support <- median(n_support)

  if (med_support < min_branches) {
    warning(
      "aggregate(): median branch support (",
      round(med_support, 1),
      ") is below min_branches (",
      min_branches,
      ").\n",
      "Consider running preprocess() with a larger buffer.",
      call. = FALSE
    )
  }

  if (verbose) {
    cat(
      "[aggregate] R = ",
      R,
      " | psi points = ",
      length(psi_grid),
      " | median support = ",
      round(med_support, 1),
      "\n",
      sep = ""
    )
  }

  # -------------------------------------------------------------------
  # 4. Return plain list — wrapping done in integrate()
  # -------------------------------------------------------------------
  cal$workspace$integrate$result <- list(
    psi_ll_df = psi_ll_df,
    R = R,
    med_support = med_support,
    min_branches = min_branches,
    floor_violated = med_support < min_branches
  )

  cal
}
