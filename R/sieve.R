# ======================================================================
# sieve.R — Branch Seed Accumulator (v3 — competitiveness-aware)
#
# Calls model$sampler$draw() repeatedly, probing each candidate, until
# total_seeds COMPETITIVE branch seeds have been accumulated — not just
# total_seeds VALID ones. "Competitive" means the branch's own peak
# log-likelihood is within reach of the best peak seen so far this run
# (the same effective_crit bar branch_extent = "global" uses in
# generate()); a branch that can never rise above the log-sum-exp noise
# floor is identified HERE, cheaply, rather than discovered after a full
# traverse_branch() in generate().
#
# draw() returns a named list:
#   $candidate — numeric vector (omega-hat in logit space)
#   $diag      — named list of draw-level metadata (may be empty)
#
# The $diag fields are merged into the diagnostics log alongside the
# accept/reject outcome for each draw. sieve() is agnostic to which
# fields $diag contains; they pass through as-is.
#
# total_seeds is read from model$sampler$total_seeds (derived during
# calibration from min_branches + branch_buffer for serial, or
# num_workers * chunk_size for parallel).
#
# probe() and sieve() arguments default to values stored on
# model$traversal, with any directly supplied arguments taking precedence.
#
# THE ADAPTIVE THRESHOLD PROBLEM: sieve() doesn't know in advance what the
# eventual best branch will look like, so "competitive" is judged against
# a RUNNING best that only improves as better seeds are found. A seed
# accepted early against a lenient bar can later be knocked below a
# tightened bar once a stronger seed is found — see the demotion logic
# below. This guarantees that by the time sieve() returns, EVERY seed in
# branch_seeds is competitive relative to the FINAL running best, not some
# stale early estimate — no seed downstream in generate()/aggregate() is a
# guess about branch quality that sieve() didn't already resolve.
#
# THE R ACCOUNTING PROBLEM: a candidate rejected for NUMERICAL INVALIDITY
# (mode_locator_failed, mode_infeasible, mode_nonfinite, ...) is missing
# data — the branch's true value was never measured, so it's correctly
# excluded from R in aggregate(). A candidate rejected as
# "mode_uncompetitive" is the opposite: its ll_mode WAS successfully,
# informatively measured, and is known to be negligible. Discarding it
# from the sum is correct (its contribution rounds to ~0), but discarding
# it from R too would silently inflate the aggregate — exactly the flaw
# that made the old "mode_too_low" screen net-harmful in the 2026-07-18
# ablation. sieve() therefore tracks n_valid (every informatively-measured
# draw, competitive or not) separately from n_draws (every attempt,
# including missing ones), and aggregate() must use n_valid as R, not
# length(branch_seeds). See branch-aggregate.R.
#
# rejection_reasons: optional character vector of probe() reason strings
#   to enforce. NULL (default) enables all rejection checks. Recognized
#   values:
#     "empty_restricted_grid"    "no_feasible_grid_point"
#     "mode_on_psi_boundary"     "mode_locator_failed"
#     "mode_eval_failed_after_snap"  "mode_nonfinite"
#     "mode_infeasible"          "mode_uncompetitive"
#     "mode_too_low"             "oscillation"
#     "mode_shift_exhausted"     "jump_left"
#     "jump_right"
# ======================================================================

#' @keywords internal
#' @noRd
.MISSING_REASONS <- c(
  "draw_failed",
  "probe_error",
  "empty_restricted_grid",
  "no_feasible_grid_point",
  "mode_locator_failed",
  "mode_eval_failed_after_snap",
  "mode_nonfinite",
  "mode_infeasible"
)

#' Accumulate Competitive Branch Seeds via Adaptive Rejection Sampling
#'
#' @description
#' Repeatedly calls \code{model$sampler$draw()} and screens each candidate
#' through \code{probe()}, accumulating branch seeds until
#' \code{model$sampler$total_seeds} \emph{competitive} seeds have been
#' collected (or \code{max_draws} is reached) — see the header comment in
#' \code{sieve.R} for what "competitive" means and why a seed's ultimate
#' fate is always resolved here, not discovered later in \code{generate()}.
#' Per-draw arguments default to the corresponding values stored on
#' \code{model$traversal}; any directly supplied argument takes
#' precedence.
#'
#' \code{draw()} returns a named list with \code{$candidate} (a numeric
#' omega-hat in logit space) and \code{$diag} (a named list of draw-level
#' metadata, possibly empty). The \code{$diag} fields are merged into the
#' diagnostics log alongside the accept/reject outcome for each draw;
#' \code{sieve()} is agnostic to which fields \code{$diag} contains.
#'
#' @param model A calibrated \code{model} object (post-\code{preprocess()},
#'   since \code{probe()} requires \code{model$traversal$max_drop_cap} and
#'   \code{model$workspace$profile$ll_at_psi_mle} to be set).
#' @param n_adjacent Optional non-negative integer. Passed to \code{probe()}.
#'   Defaults to \code{model$traversal$n_adjacent}.
#' @param max_mode_shifts Optional non-negative integer. Passed to
#'   \code{probe()}. Defaults to \code{model$traversal$max_mode_shifts}.
#' @param k_recent Optional non-negative integer. Passed to \code{probe()}.
#'   Defaults to \code{model$traversal$k_recent}.
#' @param drop_multiplier Optional positive numeric scalar. Passed to
#'   \code{probe()}. Defaults to \code{model$traversal$drop_multiplier}.
#' @param rejection_reasons Optional character vector of probe rejection
#'   checks to enforce. \code{NULL} (default) falls back to
#'   \code{model$traversal$rejection_reasons}, which enables all checks
#'   when itself \code{NULL} — including \code{"mode_uncompetitive"}. See
#'   \code{\link{traversal_spec}} for the full recognized-values list.
#' @param max_draws Optional positive integer. Hard cap on the number of
#'   \code{draw()} attempts before \code{sieve()} gives up, protecting
#'   against an infinite loop when the accept rate collapses. Defaults to
#'   \code{model$traversal$max_draws} if set, otherwise
#'   \code{50 * total_seeds}. On hitting the cap, \code{sieve()} warns and
#'   proceeds with whatever seeds were accepted; if none were accepted it
#'   errors.
#' @param verbose Logical. Print per-candidate accept/reject diagnostics
#'   and a final summary. Default: \code{FALSE}.
#'
#' @return The same calibrated \code{model} object with
#'   \code{model$workspace$integrated$cache} populated:
#'   \itemize{
#'     \item \code{branch_seeds} — list of \code{probe()} results, ALL
#'       competitive relative to the final running best (see above).
#'     \item \code{total_seeds_requested}, \code{total_seeds_accepted}
#'       (= \code{length(branch_seeds)}).
#'     \item \code{n_draws} — total \code{draw()} attempts made.
#'     \item \code{n_valid} — draws that were informatively measured
#'       (competitive or not), excluding numerical-validity failures.
#'       \strong{This is R, the correct denominator for \code{aggregate()}}
#'       — not \code{total_seeds_accepted}.
#'     \item \code{n_demoted} — seeds provisionally accepted, then later
#'       knocked below the bar as the running best improved.
#'     \item \code{accept_rate} — \code{total_seeds_accepted / n_draws}
#'       (final competitive yield per raw draw).
#'     \item \code{validity_rate} — \code{n_valid / n_draws} (fraction of
#'       draws that were at least numerically valid — low values here
#'       point at the solver/sampler numerics, an "it" problem).
#'     \item \code{competitive_yield} — \code{total_seeds_accepted /
#'       n_valid} (of the validly-measured draws, the fraction actually
#'       competitive — low values here despite high \code{validity_rate}
#'       point at the sampler's proposal distribution, a "the sampler
#'       isn't targeting the right region" problem).
#'     \item \code{diagnostics} — data frame log of every draw's outcome
#'       (demoted seeds are patched to reflect their final fate).
#'     \item \code{failure_summary} — table of rejection reasons, sorted
#'       descending by count.
#'   }
#'
#' @export
sieve <- function(
  model,
  n_adjacent = NULL,
  max_mode_shifts = NULL,
  k_recent = NULL,
  drop_multiplier = NULL,
  rejection_reasons = NULL,
  max_draws = NULL,
  verbose = FALSE
) {
  if (!is_calibrated(model)) {
    stop("sieve() requires a calibrated model.", call. = FALSE)
  }

  n_adjacent <- n_adjacent %||% model$traversal$n_adjacent
  max_mode_shifts <- max_mode_shifts %||% model$traversal$max_mode_shifts
  k_recent <- k_recent %||% model$traversal$k_recent
  drop_multiplier <- drop_multiplier %||% model$traversal$drop_multiplier
  rejection_reasons <- rejection_reasons %||% model$traversal$rejection_reasons

  total_seeds <- as.integer(model$sampler$total_seeds)
  draw <- model$sampler$draw

  max_draws <- as.integer(
    max_draws %||% model$traversal$max_draws %||% (total_seeds * 50L)
  )

  alpha_target <- min(1 - model$traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * model$traversal$cutoff_buffer

  # Provisionally-competitive pool. Grows on acceptance, shrinks on
  # demotion (see below); by loop exit it holds exactly total_seeds
  # entries (or fewer, only if max_draws was hit first).
  pool <- list()
  pool_ll <- numeric(0)
  pool_diag_idx <- integer(0)

  running_best <- -Inf
  diag_log <- list()
  n_draws <- 0L
  n_valid <- 0L
  n_demoted <- 0L

  hit_draw_cap <- FALSE

  while (length(pool) < total_seeds) {
    if (n_draws >= max_draws) {
      hit_draw_cap <- TRUE
      break
    }
    n_draws <- n_draws + 1L

    # -------------------------------------------------------------------
    # Draw candidate
    # -------------------------------------------------------------------
    draw_result <- tryCatch(draw(), error = function(e) NULL)
    if (is.null(draw_result)) {
      diag_log[[length(diag_log) + 1L]] <- list(
        candidate = n_draws,
        accepted = FALSE,
        reason = "draw_failed"
      )
      if (verbose) {
        cat("[sieve] cand ", n_draws, ": REJECT (draw_failed)\n", sep = "")
      }
      next
    }

    candidate <- draw_result$candidate
    draw_diag <- draw_result$diag %||% list()

    # Format cap info for verbose output when present
    cap_info <- if (!is.null(draw_diag$cap) && !is.na(draw_diag$cap)) {
      paste0(
        " [cap ",
        draw_diag$cap,
        if (isTRUE(draw_diag$is_dominant_cap)) "*" else "",
        "]"
      )
    } else {
      ""
    }

    # -------------------------------------------------------------------
    # Probe candidate. running_best participates in the mode_uncompetitive
    # gate inside probe() — a cheap pre-check that short-circuits before
    # the (relatively) expensive adjacent sweep for candidates that are
    # already known to be hopeless.
    # -------------------------------------------------------------------
    result <- tryCatch(
      probe(
        model = model,
        omega_hat = candidate,
        n_adjacent = n_adjacent,
        max_mode_shifts = max_mode_shifts,
        k_recent = k_recent,
        drop_multiplier = drop_multiplier,
        rejection_reasons = rejection_reasons,
        running_best = running_best
      ),
      error = function(e) list(accepted = FALSE, reason = "probe_error")
    )

    reason <- result$reason %||% "unknown"
    is_missing <- reason %in% .MISSING_REASONS

    diag_idx <- length(diag_log) + 1L
    diag_log[[diag_idx]] <- c(
      list(candidate = n_draws, accepted = isTRUE(result$accepted), reason = reason),
      draw_diag
    )

    # A non-"missing" outcome means ll_mode was informatively measured,
    # whether or not this specific candidate ends up in branch_seeds — it
    # counts toward R (see header comment / n_valid in the return value).
    if (!is_missing) {
      n_valid <- n_valid + 1L
    }

    ll_mode <- result$ll_mode
    has_ll <- !is.null(ll_mode) && is.finite(ll_mode)

    if (isTRUE(result$accepted)) {
      pool[[length(pool) + 1L]] <- result
      pool_ll <- c(pool_ll, ll_mode)
      pool_diag_idx <- c(pool_diag_idx, diag_idx)

      # -----------------------------------------------------------------
      # Running-best update + retroactive demotion. running_best is kept
      # EXACTLY equal to max(pool_ll) by construction (it is only ever
      # raised by a candidate that is itself entering the pool, so it can
      # never demote itself) — this is deliberately the pool's own max,
      # not a broader notion incorporating candidates rejected by OTHER
      # gates (mode_too_low / shape checks), since those aren't part of
      # the population branch_seeds is drawing "competitive" from.
      # -----------------------------------------------------------------
      just_demoted <- 0L
      old_running_best <- running_best
      old_bar <- running_best - effective_crit
      new_bar <- old_bar

      if (has_ll && ll_mode > running_best) {
        running_best <- ll_mode
        new_bar <- running_best - effective_crit
        stale <- which(pool_ll < new_bar | !is.finite(pool_ll))
        if (length(stale) > 0L) {
          for (j in stale) {
            diag_log[[pool_diag_idx[j]]]$accepted <- FALSE
            diag_log[[pool_diag_idx[j]]]$reason <- "demoted_uncompetitive"
          }
          just_demoted <- length(stale)
          n_demoted <- n_demoted + just_demoted
          keep <- !(seq_along(pool) %in% stale)
          pool <- pool[keep]
          pool_ll <- pool_ll[keep]
          pool_diag_idx <- pool_diag_idx[keep]
        }
      }

      if (verbose) {
        cat(
          "[sieve] cand ", n_draws, ": ACCEPT", cap_info,
          " — ll_mode=", sprintf("%.2f", ll_mode %||% NA_real_),
          " — ", length(pool), "/", total_seeds, " competitive",
          " (", n_valid, " valid, ", n_demoted, " demoted so far)\n",
          sep = ""
        )
        if (just_demoted > 0L) {
          cat(sprintf(
            "         bar jumped %.2f -> %.2f (running_best %.2f -> %.2f); %d seed%s demoted this step\n",
            old_bar, new_bar, old_running_best, running_best,
            just_demoted, if (just_demoted == 1L) "" else "s"
          ))
        }
      }
    } else {
      if (verbose) {
        cat(
          "[sieve] cand ", n_draws, ": REJECT (", reason, ")", cap_info, "\n",
          sep = ""
        )
        if (reason %in% c("mode_too_low", "mode_uncompetitive")) {
          cat(sprintf(
            "         ll_mode=%.2f | running_best=%.2f | bar=%.2f\n",
            ll_mode %||% NA_real_,
            result$running_best %||% NA_real_,
            (result$running_best %||% NA_real_) - (result$effective_crit %||% NA_real_)
          ))
        }
      }
    }
  }

  branch_seeds <- pool

  diag_df <- if (length(diag_log) > 0) {
    dplyr::bind_rows(lapply(diag_log, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame()
  }

  failure_tab <- if (nrow(diag_df) > 0) {
    sort(
      table(diag_df$reason[!diag_df$accepted]),
      decreasing = TRUE
    )
  } else {
    integer()
  }

  n_accepted <- length(branch_seeds)

  # -------------------------------------------------------------------
  # Draw-cap handling: never spin forever. Break out validity vs
  # competitiveness so the message points at the right fix — more/better
  # numerical conditioning (an "it" problem) vs a sampler that isn't
  # targeting the informative region (a "the sampler" problem).
  # -------------------------------------------------------------------
  if (hit_draw_cap) {
    fail_lines <- if (length(failure_tab) > 0) {
      paste0(
        "\nRejection reasons:\n",
        paste(
          sprintf("  %s: %d", names(failure_tab), as.integer(failure_tab)),
          collapse = "\n"
        )
      )
    } else {
      ""
    }

    rate_lines <- sprintf(
      "\n%d/%d draws were numerically valid (validity_rate = %.1f%%); of those, %d were competitive (competitive_yield = %.1f%%).",
      n_valid,
      n_draws,
      100 * n_valid / n_draws,
      n_accepted,
      if (n_valid > 0) 100 * n_accepted / n_valid else NA_real_
    )

    if (n_accepted == 0L) {
      stop(
        sprintf(
          paste0(
            "sieve(): draw cap (%d) reached with 0 of %d competitive seeds accepted. ",
            "The accept rate has collapsed — inspect the sampler and the ",
            "probe rejection_reasons.%s%s"
          ),
          max_draws,
          total_seeds,
          rate_lines,
          fail_lines
        ),
        call. = FALSE
      )
    }

    warning(
      sprintf(
        paste0(
          "sieve(): draw cap (%d) reached; accepted only %d of %d requested ",
          "competitive seeds. Proceeding with the partial set. Inspect $failure_summary.%s%s"
        ),
        max_draws,
        n_accepted,
        total_seeds,
        rate_lines,
        fail_lines
      ),
      call. = FALSE
    )
  }

  model$workspace$integrated$cache <- list(
    branch_seeds = branch_seeds,
    total_seeds_requested = total_seeds,
    total_seeds_accepted = n_accepted,
    n_draws = n_draws,
    n_valid = n_valid,
    n_demoted = n_demoted,
    accept_rate = n_accepted / n_draws,
    validity_rate = n_valid / n_draws,
    competitive_yield = if (n_valid > 0) n_accepted / n_valid else NA_real_,
    diagnostics = diag_df,
    failure_summary = failure_tab
  )

  if (verbose) {
    cat(
      "[sieve] Accepted ", n_accepted, "/", total_seeds,
      " competitive branch seeds | draws: ", n_draws,
      " | valid: ", n_valid, " (", round(100 * n_valid / n_draws, 1), "%)",
      " | demoted: ", n_demoted,
      "\n",
      sep = ""
    )
    if (length(failure_tab) > 0) {
      cat("[sieve] Failure summary:\n")
      print(failure_tab)
    }
  }

  model
}
