# ======================================================================
# tune.R — Score Threshold Tuning
# ======================================================================

#' Tune Score Threshold for Branch Aggregation
#'
#' @description
#' Sweeps over a grid of score thresholds, calling \code{aggregate()}
#' at each value and collecting diagnostics. Helps the user choose a
#' threshold that balances branch quality against coverage.
#'
#' The best threshold is selected as the one that produces the narrowest
#' two-sided confidence interval at the highest requested confidence
#' level, subject to \code{floor_violated = FALSE}. If no threshold
#' satisfies the floor constraint, the constraint is relaxed and the
#' narrowest CI across all valid thresholds is used.
#'
#' A valid threshold requires the aggregated curve to cross the CI
#' cutoff on both sides of the mode (i.e. a two-sided CI is estimable).
#'
#' Requires \code{integrate()} to have been run successfully. Branch
#' computation is not repeated — only aggregation is re-run at each
#' threshold.
#'
#' @param cal A \code{calibrated} model object with a successful
#'   \code{integrate()} result in \code{cal$workspace$integrate}.
#' @param thresholds Numeric vector of score thresholds to evaluate.
#'   Default: a sequence from 0 to 1 in steps of 0.05.
#' @param verbose Logical. Print progress. Default: \code{FALSE}.
#'
#' @return The SAME \code{calibrated} model object, with
#'   \code{cal$workspace$tune} set to a list containing:
#'   \itemize{
#'     \item \code{$summary}           — data frame with one row per
#'       threshold
#'     \item \code{$best_threshold}    — numeric scalar; selected threshold
#'     \item \code{$thresholds}        — the threshold grid used
#'   }
#'
#' @export
tune <- function(cal, ...) {
  UseMethod("tune")
}

#' @export
tune.default <- function(cal, ...) {
  stop("tune() requires a 'calibrated' model object.", call. = FALSE)
}

#' @importFrom stats qchisq
#' @export
tune.calibrated <- function(
  cal,
  thresholds = seq(0, 1, by = 0.05),
  verbose = FALSE,
  ...
) {
  branches <- cal$workspace$integrate$branches %||% NULL
  scores <- cal$workspace$integrate$scores %||% NULL

  if (is.null(branches) || length(branches) == 0L) {
    stop(
      "tune() requires pre-computed branches.\n",
      "Run integrate(cal) before tune().",
      call. = FALSE
    )
  }

  # Confidence level for CI width computation — use the highest requested
  alpha_target <- min(1 - cal$traversal$confidence_levels)
  ci_crit <- 0.5 * qchisq(1 - alpha_target, df = 1)

  if (verbose) {
    cat(
      "[tune] Sweeping ",
      length(thresholds),
      " thresholds",
      " | CI level = ",
      round(1 - alpha_target, 3),
      "\n",
      sep = ""
    )
  }

  # -------------------------------------------------------------------
  # Sweep thresholds
  # -------------------------------------------------------------------
  rows <- vector("list", length(thresholds))

  for (i in seq_along(thresholds)) {
    t <- thresholds[i]

    result <- tryCatch(
      withCallingHandlers(
        aggregate(
          cal,
          score_threshold = t,
          verbose = FALSE
        )$workspace$integrate$result,
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )

    if (is.null(result)) {
      rows[[i]] <- data.frame(
        score_threshold = t,
        R_eff = NA_integer_,
        med_support = NA_real_,
        floor_violated = NA,
        psi_range = NA_real_,
        ci_width = NA_real_,
        ci_valid = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    psi_ll_df <- result$psi_ll_df

    psi_range <- if (!is.null(psi_ll_df)) {
      diff(range(psi_ll_df$psi))
    } else {
      NA_real_
    }

    # Compute CI width at highest confidence level
    ci <- .compute_ci_width(psi_ll_df, ci_crit)

    rows[[i]] <- data.frame(
      score_threshold = t,
      R_eff = result$R_eff,
      med_support = result$med_support,
      floor_violated = result$floor_violated,
      psi_range = psi_range,
      ci_width = ci$width,
      ci_valid = ci$valid,
      stringsAsFactors = FALSE
    )

    if (verbose) {
      r <- rows[[i]]
      cat(
        "[tune] threshold = ",
        round(t, 3),
        " | R_eff = ",
        r$R_eff,
        " | med_support = ",
        round(r$med_support, 1),
        " | floor_violated = ",
        r$floor_violated,
        " | ci_width = ",
        if (r$ci_valid) round(r$ci_width, 4) else "N/A",
        "\n",
        sep = ""
      )
    }
  }

  summary_df <- do.call(rbind, rows)

  # -------------------------------------------------------------------
  # Select best threshold
  # -------------------------------------------------------------------
  best_threshold <- .select_best_threshold(summary_df)

  if (verbose) {
    cat("[tune] Best threshold: ", best_threshold, "\n", sep = "")
  }

  cal$workspace$integrate$tune <- list(
    summary = summary_df,
    best_threshold = best_threshold,
    thresholds = thresholds
  )

  cal
}

# ======================================================================
# INTERNAL: Compute two-sided CI width from aggregated curve
# ======================================================================

#' @keywords internal
#' @noRd
.compute_ci_width <- function(psi_ll_df, ci_crit) {
  if (is.null(psi_ll_df) || nrow(psi_ll_df) == 0L) {
    return(list(width = NA_real_, valid = FALSE))
  }

  ll <- psi_ll_df$loglik
  psi <- psi_ll_df$psi

  ll_centered <- ll - max(ll, na.rm = TRUE)
  cutoff <- -ci_crit

  i_mode <- which.max(ll)

  # Left root — scan left from mode
  left_side <- ll_centered[seq_len(i_mode)]
  left_psi <- psi[seq_len(i_mode)]
  left_cross <- which(left_side < cutoff)

  if (length(left_cross) == 0L) {
    return(list(width = NA_real_, valid = FALSE))
  }

  i_left <- max(left_cross) + 1L
  if (i_left > i_mode) {
    return(list(width = NA_real_, valid = FALSE))
  }

  ci_left <- tryCatch(
    stats::approx(
      x = left_side[i_left:i_mode],
      y = left_psi[i_left:i_mode],
      xout = cutoff
    )$y,
    error = function(e) NA_real_
  )

  # Right root — scan right from mode
  right_side <- ll_centered[i_mode:length(ll)]
  right_psi <- psi[i_mode:length(psi)]
  right_cross <- which(right_side < cutoff)

  if (length(right_cross) == 0L) {
    return(list(width = NA_real_, valid = FALSE))
  }

  i_right <- min(right_cross) - 1L
  if (i_right < 1L) {
    return(list(width = NA_real_, valid = FALSE))
  }

  ci_right <- tryCatch(
    stats::approx(
      x = right_side[1L:i_right],
      y = right_psi[1L:i_right],
      xout = cutoff
    )$y,
    error = function(e) NA_real_
  )

  if (is.na(ci_left) || is.na(ci_right)) {
    return(list(width = NA_real_, valid = FALSE))
  }

  list(width = ci_right - ci_left, valid = TRUE)
}

# ======================================================================
# INTERNAL: Select best threshold from summary
# ======================================================================

#' @keywords internal
#' @noRd
.select_best_threshold <- function(summary_df) {
  # Restrict to thresholds with valid two-sided CI
  valid <- summary_df[
    isTRUE(summary_df$ci_valid) & !is.na(summary_df$ci_valid),
  ]

  if (nrow(valid) == 0L) {
    warning(
      "tune(): no threshold produced a valid two-sided CI. ",
      "Returning lowest threshold.",
      call. = FALSE
    )
    return(min(summary_df$score_threshold))
  }

  # Prefer thresholds where floor is not violated
  no_floor_violation <- valid[
    !isTRUE(valid$floor_violated) &
      !is.na(valid$floor_violated),
  ]

  candidates <- if (nrow(no_floor_violation) > 0L) {
    no_floor_violation
  } else {
    warning(
      "tune(): all valid thresholds violate the min_branches floor. ",
      "Selecting from all valid thresholds.",
      call. = FALSE
    )
    valid
  }

  # Among candidates, find narrowest CI — break ties by highest threshold
  i_best <- which.min(candidates$ci_width)

  # Break ties
  min_width <- candidates$ci_width[i_best]
  tied <- candidates[abs(candidates$ci_width - min_width) < 1e-10, ]
  best_row <- tied[which.max(tied$score_threshold), ]

  best_row$score_threshold
}
