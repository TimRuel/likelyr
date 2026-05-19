# ======================================================================
# sieve.R — Branch Seed Accumulator
#
# Calls model$sampler$draw() repeatedly, probing the returned candidate
# until total_seeds accepted branch seeds have been accumulated.
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
# rejection_reasons: optional character vector of probe() reason strings
#   to enforce. NULL (default) enables all rejection checks. Recognized
#   values:
#     "empty_restricted_grid"    "no_feasible_grid_point"
#     "mode_on_psi_boundary"     "mode_locator_failed"
#     "mode_eval_failed_after_snap"  "mode_too_low"
#     "oscillation"              "mode_shift_exhausted"
#     "jump_left"                "jump_right"
# ======================================================================

#' @export
sieve <- function(
  model,
  n_adjacent = NULL,
  max_mode_shifts = NULL,
  k_recent = NULL,
  drop_multiplier = NULL,
  rejection_reasons = NULL,
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

  branch_seeds <- vector("list", total_seeds)
  diag_log <- list()
  n_accepted <- 0L
  n_draws <- 0L

  while (n_accepted < total_seeds) {
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
    # Probe candidate
    # -------------------------------------------------------------------
    result <- tryCatch(
      probe(
        model = model,
        omega_hat = candidate,
        n_adjacent = n_adjacent,
        max_mode_shifts = max_mode_shifts,
        k_recent = k_recent,
        drop_multiplier = drop_multiplier,
        rejection_reasons = rejection_reasons
      ),
      error = function(e) list(accepted = FALSE, reason = "probe_error")
    )

    if (isTRUE(result$accepted)) {
      n_accepted <- n_accepted + 1L
      branch_seeds[[n_accepted]] <- result
      diag_log[[length(diag_log) + 1L]] <- c(
        list(candidate = n_draws, accepted = TRUE, reason = "ok"),
        draw_diag
      )
      if (verbose) {
        cat(
          "[sieve] cand ",
          n_draws,
          ": ACCEPT",
          cap_info,
          " — ",
          n_accepted,
          "/",
          total_seeds,
          " accepted\n",
          sep = ""
        )
      }
    } else {
      reason <- result$reason %||% "unknown"
      diag_log[[length(diag_log) + 1L]] <- c(
        list(candidate = n_draws, accepted = FALSE, reason = reason),
        draw_diag
      )
      if (verbose) {
        cat(
          "[sieve] cand ",
          n_draws,
          ": REJECT (",
          reason,
          ")",
          cap_info,
          "\n",
          sep = ""
        )
      }
    }
  }

  branch_seeds <- branch_seeds[seq_len(n_accepted)]

  diag_df <- if (length(diag_log) > 0) {
    do.call(rbind, lapply(diag_log, as.data.frame, stringsAsFactors = FALSE))
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

  model$workspace$integrated$cache <- list(
    branch_seeds = branch_seeds,
    total_seeds_requested = total_seeds,
    total_seeds_accepted = n_accepted,
    n_draws = n_draws,
    accept_rate = n_accepted / n_draws,
    diagnostics = diag_df,
    failure_summary = failure_tab
  )

  if (verbose) {
    cat(
      "[sieve] Accepted ",
      n_accepted,
      "/",
      total_seeds,
      " branch seeds | draws: ",
      n_draws,
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
