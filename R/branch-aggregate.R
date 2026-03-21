# ======================================================================
# likelihood-aggregate.R — Branch Aggregation
# ======================================================================

#' Aggregate Branches into Integrated Log-Likelihood Curve
#'
#' @description
#' Filters branches by \code{score_threshold}, then computes a pointwise
#' average log-likelihood curve using all retained branches that have
#' support at each ψ value:
#'
#' \deqn{
#'   \log \bar{L}(\psi)
#'   = \log \left(
#'     \frac{1}{n(\psi)} \sum_{b \,:\, \psi \in \mathrm{supp}(b)}
#'     \exp\{\ell_b(\psi)\}
#'   \right)
#' }
#'
#' where \eqn{n(\psi)} is the number of retained branches with an
#' evaluation at ψ. Coverage varies naturally across ψ — branches
#' with holes drop out at affected grid points rather than being
#' excluded entirely.
#'
#' A floor warning is issued when the median number of branches
#' supporting a ψ point falls below \code{min_branches}, indicating
#' the threshold may be too aggressive.
#'
#' @param cal A \code{calibrated} model object with
#'   \code{cal$workspace$integrate$branches} and
#'   \code{cal$workspace$integrate$scores} populated by
#'   \code{integrate()}.
#' @param score_threshold Numeric scalar in [0, 1]. Branches with score
#'   below this threshold are excluded. Default: \code{0}.
#' @param verbose Logical. Print diagnostics. Default: \code{FALSE}.
#'
#' @return The SAME \code{calibrated} model object, with
#'   \code{cal$workspace$integrate$result} updated.
#'
#' @export
aggregate <- function(cal, ...) {
  UseMethod("aggregate")
}

#' @export
aggregate.default <- function(cal, ...) {
  stop("aggregate() requires a 'calibrated' model object.", call. = FALSE)
}

#' @importFrom stats median
#' @export
aggregate.calibrated <- function(
  cal,
  score_threshold = 0,
  verbose = FALSE,
  ...
) {
  branches <- cal$workspace$integrate$branches %||% NULL
  scores <- cal$workspace$integrate$scores %||% NULL

  if (is.null(branches) || length(branches) == 0L) {
    stop(
      "aggregate() requires pre-computed branches.\n",
      "Run integrate(cal) before aggregate().",
      call. = FALSE
    )
  }

  min_branches <- cal$execution$min_branches

  # -------------------------------------------------------------------
  # 1. Filter by score threshold
  # -------------------------------------------------------------------
  keep <- scores >= score_threshold
  retained <- branches[keep]
  R_eff <- length(retained)

  if (R_eff == 0L) {
    stop(
      "No branches retained at score_threshold = ",
      score_threshold,
      ".\n",
      "Lower the threshold or re-run integrate().",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # 2. Build ψ × R matrix over union of retained support
  # -------------------------------------------------------------------
  psi_grid <- sort(unique(unlist(lapply(retained, function(b) b$psi))))

  branch_mat <- vapply(
    retained,
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
  # 3. Pointwise log-mean-exp
  # -------------------------------------------------------------------
  n_support <- rowSums(is.finite(branch_mat))
  loglik <- matrixStats::rowLogSumExps(branch_mat, na.rm = TRUE) -
    log(n_support)

  psi_ll_df <- tibble::tibble(
    psi = psi_grid,
    loglik = loglik,
    n_support = n_support
  )

  # -------------------------------------------------------------------
  # 4. Floor check — warn if median n_support < min_branches
  # -------------------------------------------------------------------
  med_support <- median(n_support)

  if (med_support < min_branches) {
    warning(
      "aggregate(): median branch support (",
      round(med_support, 1),
      ") is below min_branches (",
      min_branches,
      ").\n",
      "Consider lowering score_threshold.",
      call. = FALSE
    )
  }

  if (verbose) {
    cat(
      "[aggregate] score_threshold = ",
      score_threshold,
      " | retained = ",
      R_eff,
      "/",
      length(branches),
      " | median support = ",
      round(med_support, 1),
      "\n",
      sep = ""
    )
  }

  # -------------------------------------------------------------------
  # 5. Store result
  # -------------------------------------------------------------------
  cal$workspace$integrate$result <- list(
    psi_ll_df = psi_ll_df,
    R_eff = R_eff,
    score_threshold = score_threshold,
    scores_used = scores[keep],
    scores_all = scores,
    med_support = med_support,
    min_branches = min_branches,
    floor_violated = med_support < min_branches
  )

  cal
}
