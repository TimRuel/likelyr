# ======================================================================
# sieve.R — Branch Seed Accumulator
#
# Draws omega-hats from cal$sampler$draw(), expands each to an orbit
# via cal$sampler$expand_orbit(), and calls probe() on each candidate
# until total_seeds accepted branch seeds have been accumulated.
#
# total_seeds is read from cal$execution$total_seeds (derived during
# calibration from min_branches + branch_buffer for serial, or
# num_workers * chunk_size for parallel).
#
# Each orbit consists of one base draw plus its expanded candidates.
# n_orbits counts base draws; n_candidates counts all probed omega-hats.
#
# orbit_size is read from cal$sampler$orbit_size. total_seeds need not
# be a multiple of orbit_size — the final orbit is truncated to fill
# exactly total_seeds branch seeds.
#
# probe() and sieve() arguments default to values stored on
# cal$traversal, with any directly supplied arguments taking precedence.
# ======================================================================

#' @export
sieve <- function(
  cal,
  n_adjacent = NULL,
  max_mode_shifts = NULL,
  k_recent = NULL,
  drop_multiplier = NULL,
  verbose = FALSE
) {
  if (!is_calibrated(cal)) {
    stop("sieve() requires a calibrated model.", call. = FALSE)
  }

  n_adjacent <- n_adjacent %||% cal$traversal$n_adjacent
  max_mode_shifts <- max_mode_shifts %||% cal$traversal$max_mode_shifts
  k_recent <- k_recent %||% cal$traversal$k_recent
  drop_multiplier <- drop_multiplier %||% cal$traversal$drop_multiplier

  total_seeds <- as.integer(cal$execution$total_seeds)
  draw <- cal$sampler$draw
  expand_orbit <- cal$sampler$expand_orbit

  branch_seeds <- vector("list", total_seeds)
  diag_log <- list()
  n_accepted <- 0L
  n_orbits <- 0L
  cand_id <- 0L

  .log <- function(id, accepted, reason) {
    diag_log[[length(diag_log) + 1L]] <<- list(
      candidate = id,
      accepted = accepted,
      reason = reason
    )
    if (verbose) {
      message(
        "[sieve] cand ",
        id,
        ": ",
        if (accepted) "ACCEPT" else "REJECT",
        " (",
        reason,
        ")",
        if (accepted) {
          paste0(" — ", n_accepted, "/", total_seeds, " accepted")
        } else {
          ""
        }
      )
    }
  }

  while (n_accepted < total_seeds) {
    n_orbits <- n_orbits + 1L

    # -------------------------------------------------------------------
    # Draw base omega-hat
    # -------------------------------------------------------------------
    base <- tryCatch(draw(), error = function(e) NULL)
    if (is.null(base)) {
      cand_id <- cand_id + 1L
      .log(cand_id, FALSE, "draw_failed")
      next
    }

    # -------------------------------------------------------------------
    # Expand to orbit, truncating to however many seeds still needed
    # -------------------------------------------------------------------
    candidates <- if (!is.null(expand_orbit)) {
      orbit <- tryCatch(expand_orbit(base), error = function(e) NULL)
      if (is.null(orbit)) {
        list(base)
      } else {
        n_remaining <- total_seeds - n_accepted
        n_take <- min(length(orbit), n_remaining)
        c(list(base), orbit[seq_len(n_take)])
      }
    } else {
      list(base)
    }

    # -------------------------------------------------------------------
    # Probe each candidate
    # -------------------------------------------------------------------
    for (omega in candidates) {
      if (n_accepted >= total_seeds) {
        break
      }

      cand_id <- cand_id + 1L

      result <- tryCatch(
        probe(
          cal = cal,
          omega_hat = omega,
          n_adjacent = n_adjacent,
          max_mode_shifts = max_mode_shifts,
          k_recent = k_recent,
          drop_multiplier = drop_multiplier
        ),
        error = function(e) list(accepted = FALSE, reason = "probe_error")
      )

      if (isTRUE(result$accepted)) {
        n_accepted <- n_accepted + 1L
        branch_seeds[[n_accepted]] <- result
        .log(cand_id, TRUE, "ok")
      } else {
        .log(cand_id, FALSE, result$reason %||% "unknown")
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

  cal$workspace$integrate$branch_seeds <- branch_seeds
  cal$workspace$integrate$sieve <- list(
    total_seeds_requested = total_seeds,
    total_seeds_accepted = n_accepted,
    n_orbits = n_orbits,
    candidates_processed = cand_id,
    accept_rate = n_accepted / cand_id,
    diagnostics = diag_df,
    failure_summary = failure_tab
  )

  if (verbose) {
    message(
      "[sieve] Accepted ",
      n_accepted,
      " / ",
      total_seeds,
      " branch seeds",
      " | orbits: ",
      n_orbits
    )
    if (length(failure_tab) > 0) {
      message("[sieve] Failure summary:")
      print(failure_tab)
    }
  }

  cal
}
