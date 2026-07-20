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
#' @param model   A calibrated \code{model} object.
#' @param verbose Logical. Default: \code{FALSE}.
#'
#' @return The SAME calibrated \code{model} object, with
#'   \code{model$workspace$integrated} updated.
#'
#' @importFrom stats median
#' @export
aggregate <- function(model, ...) {
  UseMethod("aggregate")
}

#' @export
aggregate.default <- function(model, ...) {
  stop("aggregate() requires a calibrated 'model' object.", call. = FALSE)
}

#' @export
aggregate.model <- function(model, verbose = FALSE, ...) {
  branches <- model$workspace$integrated$cache$branches %||% NULL
  min_branches <- model$sampler$min_branches

  if (is.null(branches) || length(branches) == 0L) {
    stop(
      "aggregate() requires pre-computed branches.\n",
      "Run integrate(model) before aggregate().",
      call. = FALSE
    )
  }

  # R is the true number of INFORMATIVELY-MEASURED nuisance draws, not
  # just the number of branches that got fully traversed. sieve() (v3,
  # 2026-07-20) pre-screens for competitiveness: a "mode_uncompetitive"
  # rejection means the branch's peak was validly measured and known to
  # be negligible, so it's correctly excluded from `branches` (no full
  # traversal needed) — but it must still count in R, or dividing an
  # (essentially unchanged) numerator by an artificially small denominator
  # silently inflates the aggregate, exactly the flaw that made the old
  # mode_too_low screen net-harmful (2026-07-18 ablation). Falls back to
  # length(branches) when cache$n_valid is absent (e.g. hand-built
  # branch_seeds bypassing sieve()), which recovers the pre-v3 behavior.
  R <- model$workspace$integrated$cache$n_valid %||% length(branches)

  # -------------------------------------------------------------------
  # 1. Build ψ × R matrix
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
  alpha_target <- min(1 - model$traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * model$traversal$cutoff_buffer

  n_support <- rowSums(is.finite(branch_mat))

  # Standard Monte Carlo integrated likelihood: divide by the CONSTANT
  # number of branches R, not the per-point support (audit A2). A branch
  # absent at a psi point was trimmed >effective_crit (~10 nats) below its
  # own mode, so its true contribution there is < ~1e-4 relative — the
  # standard exp(-Inf)=0 convention. Dividing by n_support instead
  # averaged over survivors, stepping the tail UP and distorting the
  # above_crit CI mask. n_support is retained below purely as a diagnostic.
  # (This also removes the n_support==0 -> NaN edge, cf. audit A9.)
  loglik <- matrixStats::rowLogSumExps(branch_mat, na.rm = TRUE) - log(R)

  rel_loglik <- loglik - max(loglik, na.rm = TRUE)

  psi_loglik_df <- tibble::tibble(
    psi = psi_grid,
    loglik = loglik,
    rel_loglik = rel_loglik,
    above_crit = rel_loglik >= -effective_crit,
    n_support = n_support
  ) |>
    magrittr::set_attr("pseudolikelihood", "integrated")

  # -------------------------------------------------------------------
  # 3. Floor check
  #
  # Low median support signals branches that are too short ONLY under
  # per-branch extent. Under branch_extent = "global" branches are
  # deliberately trimmed at the shared aggregate-relevant cutoff, so low
  # support is expected and harmless (the aggregate still reaches its CI
  # cutoff — that is the point of the global rule). Skip the warning there.
  # -------------------------------------------------------------------
  med_support <- median(n_support)
  branch_extent <- model$traversal$branch_extent %||% "per_branch"

  if (med_support < min_branches && !identical(branch_extent, "global")) {
    warning(
      "aggregate(): median branch support (",
      round(med_support, 1),
      ") is below min_branches (",
      min_branches,
      ").\n",
      "Consider running preprocess() with a larger buffer, or ",
      "branch_extent = 'global'.",
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
  # 4. Return — cache carried forward; wrapping/marking done in integrate()
  # -------------------------------------------------------------------
  model$workspace$integrated <- list(
    psi_loglik_df = psi_loglik_df,
    psi_hat = psi_grid[which.max(loglik)],
    branch_mat = branch_mat,
    R = R,
    med_support = med_support,
    min_branches = min_branches,
    floor_violated = med_support < min_branches,
    cache = model$workspace$integrated$cache
  )

  model
}
