# ======================================================================
# branch-score.R — Branch Quality Scoring
# ======================================================================

# ----------------------------------------------------------------------
# Branch scoring
# ----------------------------------------------------------------------

#' Score a Branch by Contiguous Coverage
#'
#' @description
#' Assigns a numeric score in [0, 1] to a completed branch based on
#' how much of the branch is contiguously connected to the mode on
#' each side. A score of 1 indicates a perfect branch with no holes.
#'
#' For each side, the score is the ratio of the weighted length of the
#' leading contiguous run (starting from the mode) to the total weighted
#' length of all points on that side. Points in the crit zone
#' (within \code{crit} of the mode log-likelihood) are weighted by
#' \code{w_crit}; points in the buffer zone (between \code{crit} and
#' \code{effective_crit}) are weighted by \code{w_buffer}.
#'
#' The overall score is the mean across sides that have at least one
#' point. One-sided branches (mode at a boundary) are scored on the
#' single existing side only.
#'
#' @param branch_df     Data frame with columns \code{k} and \code{loglik},
#'   as returned by \code{assemble_branch_df()}.
#' @param k_mode        Integer grid index of the branch mode.
#' @param ll_mode       Numeric scalar. Log-likelihood at the mode.
#' @param crit          Numeric scalar. The theoretically required
#'   log-likelihood drop for the tightest requested CI.
#' @param w_crit        Weight for points in the crit zone.
#'   Default: \code{1.0}.
#' @param w_buffer      Weight for points in the buffer zone.
#'   Default: \code{0.5}.
#'
#' @return Numeric scalar in [0, 1].
#'
#' @keywords internal
score_branch <- function(
  branch_df,
  k_mode,
  ll_mode,
  crit,
  w_crit = 1.0,
  w_buffer = 0.5
) {
  .score_side <- function(ks, logliks) {
    if (length(ks) == 0L) {
      return(NULL)
    }

    # Weights for each point based on zone
    crit_boundary <- ll_mode - crit
    weights <- ifelse(logliks >= crit_boundary, w_crit, w_buffer)

    weighted_total <- sum(weights)
    if (weighted_total == 0) {
      return(NULL)
    }

    # Leading contiguous run from the mode edge
    # ks are already sorted outward from mode (ascending for right,
    # descending for left), so check consecutive steps of size 1
    run_length <- 1L
    while (
      run_length < length(ks) &&
        abs(ks[run_length + 1L] - ks[run_length]) == 1L
    ) {
      run_length <- run_length + 1L
    }

    weighted_run <- sum(weights[seq_len(run_length)])
    weighted_run / weighted_total
  }

  # Sort each side outward from mode
  left_df <- branch_df |>
    dplyr::filter(k < k_mode) |>
    dplyr::arrange(desc(k))

  right_df <- branch_df |>
    dplyr::filter(k > k_mode) |>
    dplyr::arrange(k)

  scores <- c(
    .score_side(left_df$k, left_df$loglik),
    .score_side(right_df$k, right_df$loglik)
  )

  if (length(scores) == 0L) {
    return(0)
  }
  mean(scores)
}
