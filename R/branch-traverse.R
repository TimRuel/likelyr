# ======================================================================
# branch-traverse.R — Branch Traversal Strategies
#
# Provides:
#   traverse_branch()           — dispatcher (topdown | leftright)
#   traverse_branch_topdown()   — outward sweep from pre-located mode
#   traverse_branch_leftright() — full left-to-right sweep, post-trim
#   traverse_branch_side()      — one-sided sweep (used by topdown)
#
# All traversal functions return a list with a uniform contract:
#   $branch_df        — tibble(psi, loglik, loglik_centered, k)
#   $psi_hat          — numeric scalar mode (NA for leftright)
#   $traversal_method — character scalar
# ======================================================================

# ======================================================================
# DISPATCHER
# ======================================================================

#' Traverse a Branch on the ψ-Grid
#'
#' @description
#' Dispatches to the appropriate traversal strategy based on
#' \code{traversal$traversal_method}:
#' \itemize{
#'   \item \code{"topdown"} — outward sweep from a pre-located mode.
#'     Requires a fully populated \code{branch_seed} object with \code{psi_mode},
#'     \code{param_mode}, and \code{ll_mode}. This is the default
#'     strategy and requires \code{sieve()} to have been run first.
#'   \item \code{"leftright"} — single left-to-right sweep over the full
#'     search interval, with the cutoff applied retrospectively. Does not
#'     require mode pre-location. Intended as a fallback when mode
#'     location is systematically unreliable.
#' }
#'
#' @param branch_seed A named list from \code{probe()}, containing at minimum:
#'   \itemize{
#'     \item \code{omega_hat}        — numeric vector
#'     \item \code{psi_mode}         — numeric scalar (topdown only)
#'     \item \code{param_mode}       — numeric vector (topdown only)
#'     \item \code{ll_mode}          — numeric scalar (topdown only)
#'     \item \code{probe_evals_df}   — data frame of pre-evaluated points
#'     \item \code{drops_left}       — numeric vector of drops from probe
#'     \item \code{drops_right}      — numeric vector of drops from probe
#'     \item \code{param_left_edge}  — warm-start for leftward traversal
#'     \item \code{param_right_edge} — warm-start for rightward traversal
#'     \item \code{left_boundary}    — logical; TRUE if probe hit left bound
#'     \item \code{right_boundary}   — logical; TRUE if probe hit right bound
#'   }
#' @param traversal        A calibrated \code{traversal_spec} object.
#' @param grid             A ψ-grid object from \code{psi_grid_anchor()}.
#' @param branch_evaluator Function \code{(psi, param_init) -> list(param_hat, branch_val)}.
#' @param effective_crit   Numeric scalar. Log-likelihood drop defining the
#'   branch cutoff: \code{cutoff = ll_mode - effective_crit}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{$branch_df}        — tibble(psi, loglik, loglik_centered, k)
#'     \item \code{$psi_hat}          — numeric scalar (NA for leftright)
#'     \item \code{$traversal_method} — character scalar
#'   }
#'
#' @keywords internal
traverse_branch <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator,
  effective_crit
) {
  method <- traversal$traversal_method %||% "topdown"

  switch(
    method,
    topdown = traverse_branch_topdown(
      branch_seed = branch_seed,
      traversal = traversal,
      grid = grid,
      branch_evaluator = branch_evaluator,
      effective_crit = effective_crit
    ),
    leftright = traverse_branch_leftright(
      branch_seed = branch_seed,
      traversal = traversal,
      grid = grid,
      branch_evaluator = branch_evaluator,
      effective_crit = effective_crit
    ),
    stop(
      "Unknown traversal_method: '",
      method,
      "'. ",
      "Must be 'topdown' or 'leftright'.",
      call. = FALSE
    )
  )
}

# ======================================================================
# STRATEGY 1: Top-Down (outward from pre-located mode)
# ======================================================================

#' Traverse a Branch Outward from a Pre-Located Mode
#'
#' @description
#' Picks up where \code{probe()} left off: reads pre-evaluated points
#' from \code{branch_seed$probe_evals_df}, then extends outward on each
#' side using the edge warm-starts from the seed. Sides that probe()
#' stopped at a boundary are not extended. Drop history from probe() is
#' passed into each side sweep so the drop multiplier check is
#' continuous across the probe/traversal boundary.
#'
#' @inheritParams traverse_branch
#' @keywords internal
traverse_branch_topdown <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator,
  effective_crit
) {
  psi_mode <- branch_seed$psi_mode
  ll_mode <- branch_seed$ll_mode
  probe_evals_df <- branch_seed$probe_evals_df

  branch_cutoff <- ll_mode - effective_crit
  k_recent <- traversal$k_recent
  drop_multiplier <- traversal$drop_multiplier

  # -------------------------------------------------------------------
  # Determine outermost k already evaluated on each side
  # -------------------------------------------------------------------
  k_mode <- round((psi_mode - grid$psi_mle) / grid$increment)

  probe_evals_df_left <- probe_evals_df |> dplyr::filter(side == "left")
  probe_evals_df_right <- probe_evals_df |> dplyr::filter(side == "right")

  k_left_edge <- if (nrow(probe_evals_df_left) > 0L) {
    min(probe_evals_df_left$k)
  } else {
    k_mode
  }
  k_right_edge <- if (nrow(probe_evals_df_right) > 0L) {
    max(probe_evals_df_right$k)
  } else {
    k_mode
  }

  # -------------------------------------------------------------------
  # Extend left side (skip entirely if probe hit left boundary)
  # -------------------------------------------------------------------
  left_df <- if (isTRUE(branch_seed$left_boundary)) {
    tibble::tibble(k = integer(), loglik = numeric())
  } else {
    traverse_branch_side(
      grid = grid,
      k_direction = -1L,
      k_start = k_left_edge - 1L,
      branch_cutoff = branch_cutoff,
      init_guess = branch_seed$param_left_edge,
      init_ll = if (nrow(probe_evals_df_left) > 0L) {
        probe_evals_df_left$loglik[which.min(probe_evals_df_left$k)]
      } else {
        ll_mode
      },
      init_drops = branch_seed$drops_left,
      branch_evaluator = branch_evaluator,
      k_recent = k_recent,
      drop_multiplier = drop_multiplier
    )
  }

  # -------------------------------------------------------------------
  # Extend right side (skip entirely if probe hit right boundary)
  # -------------------------------------------------------------------
  right_df <- if (isTRUE(branch_seed$right_boundary)) {
    tibble::tibble(k = integer(), loglik = numeric())
  } else {
    traverse_branch_side(
      grid = grid,
      k_direction = +1L,
      k_start = k_right_edge + 1L,
      branch_cutoff = branch_cutoff,
      init_guess = branch_seed$param_right_edge,
      init_ll = if (nrow(probe_evals_df_right) > 0L) {
        probe_evals_df_right$loglik[which.max(probe_evals_df_right$k)]
      } else {
        ll_mode
      },
      init_drops = branch_seed$drops_right,
      branch_evaluator = branch_evaluator,
      k_recent = k_recent,
      drop_multiplier = drop_multiplier
    )
  }

  # -------------------------------------------------------------------
  # Combine probe evals df with new traversal points
  # -------------------------------------------------------------------
  branch_df <- probe_evals_df |>
    dplyr::select(k, loglik) |>
    dplyr::bind_rows(left_df, right_df) |>
    assemble_branch_df(grid)

  list(
    branch_df = branch_df,
    psi_hat = psi_mode,
    traversal_method = "topdown"
  )
}

# ======================================================================
# STRATEGY 2: Left-to-Right (full sweep, retrospective trim)
# ======================================================================

#' Traverse a Branch Left to Right Across the Full Search Interval
#'
#' @description
#' Evaluates the branch at every grid point in the search interval from
#' left to right, using warm-start chaining between adjacent points.
#' After the full sweep, the mode is located retrospectively and points
#' below \code{ll_mode - effective_crit} are trimmed.
#'
#' Does not require mode pre-location. Intended as a fallback when
#' mode location is systematically unreliable.
#'
#' @inheritParams traverse_branch
#' @keywords internal
traverse_branch_leftright <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator,
  effective_crit
) {
  omega_hat <- branch_seed$omega_hat
  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper
  psi_mle <- grid$psi_mle
  increment <- grid$increment

  k_min <- ceiling((psi_lower - psi_mle) / increment)
  k_max <- floor((psi_upper - psi_mle) / increment)

  rows <- vector("list", k_max - k_min + 1L)
  current_par <- omega_hat

  for (k in seq(k_min, k_max)) {
    psi_k <- psi_mle + k * increment
    eval <- tryCatch(
      branch_evaluator(psi_k, current_par),
      error = function(e) list(branch_val = NA_real_, param_hat = current_par)
    )
    rows[[k - k_min + 1L]] <- list(k = k, loglik = eval$branch_val)
    if (is.finite(eval$branch_val)) current_par <- eval$param_hat
  }

  df <- dplyr::bind_rows(rows)
  df <- df[is.finite(df$loglik), ]

  if (nrow(df) == 0L) {
    stop(
      "traverse_branch_leftright(): no finite log-likelihood values produced.",
      call. = FALSE
    )
  }

  i_mode <- which.max(df$loglik)
  ll_mode <- df$loglik[i_mode]
  psi_hat <- psi_mle + df$k[i_mode] * increment
  branch_cutoff <- ll_mode - effective_crit

  df <- df[df$loglik >= branch_cutoff, ]
  branch_df <- assemble_branch_df(df, grid)

  list(
    branch_df = branch_df,
    psi_hat = psi_hat,
    traversal_method = "leftright"
  )
}

# ======================================================================
# ONE-SIDED SWEEP (used by topdown)
# ======================================================================

#' One-Sided Branch Sweep Along the ψ-Grid
#'
#' @description
#' Sweeps outward from \code{k_start} in the given direction, evaluating
#' the branch at each grid point. Stops when the log-likelihood drops
#' below \code{branch_cutoff} or a ψ bound is reached.
#'
#' When a point rises above the previous value or produces a jump larger
#' than \code{drop_multiplier} times the recent median drop, it is
#' skipped and the last good \code{param_hat} is reused as the
#' warm-start for the next point, preserving the trajectory. If
#' \code{max_consecutive_skips} holes accumulate in a row, the side
#' is stopped.
#'
#' Returns a \code{$df} of the evaluated points
#'
#' @param grid                  ψ-grid object.
#' @param k_direction           Integer +1 or -1.
#' @param k_start               Integer starting grid index.
#' @param branch_cutoff         Numeric scalar cutoff log-likelihood.
#' @param init_guess            Numeric vector warm-start parameter.
#' @param init_ll               Numeric scalar log-likelihood of the
#'   last accepted point before k_start (used for first drop check).
#' @param init_drops            Numeric vector of drops from probe()
#'   seeding the recent drop history.
#' @param branch_evaluator      Function (psi, param_init) -> list.
#' @param k_recent              Integer. Recent drop window size.
#' @param drop_multiplier       Numeric. Jump detection multiplier.
#' @param max_consecutive_skips Integer. Stop side after this many
#'   consecutive skipped points. Default: 2L.
#'
#' @keywords internal
traverse_branch_side <- function(
  grid,
  k_direction,
  k_start,
  branch_cutoff,
  init_guess,
  init_ll,
  init_drops,
  branch_evaluator,
  k_recent = 3L,
  drop_multiplier = 10.0,
  max_consecutive_skips = 2L
) {
  k_curr <- k_start
  current_par <- init_guess
  current_val <- init_ll
  drops <- init_drops
  consecutive_skips <- 0L

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {
    psi_k <- grid$psi_mle + k_curr * grid$increment

    hit_lower <- !is.null(psi_lower) && psi_k < psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k > psi_upper

    if (hit_lower || hit_upper) {
      break
    }

    eval <- tryCatch(
      branch_evaluator(psi_k, current_par),
      error = function(e) NULL
    )

    skip <- FALSE

    if (is.null(eval) || !is.finite(eval$branch_val)) {
      skip <- TRUE
    } else {
      new_val <- eval$branch_val
      new_drop <- current_val - new_val
      recent <- tail(drops, k_recent)

      # After consecutive skips, widen the drop tolerance proportionally
      # to how many steps were missed — a legitimate drop may span
      # multiple grid points worth of descent
      effective_multiplier <- drop_multiplier * (consecutive_skips + 1L)

      if (
        new_val > current_val ||
          !check_drop(new_drop, recent, effective_multiplier)
      ) {
        skip <- TRUE
      }
    }

    if (skip) {
      # Preserve current_par and current_val so the next point
      # warm-starts from the last good solution
      consecutive_skips <- consecutive_skips + 1L
      if (consecutive_skips >= max_consecutive_skips) break
    } else {
      drops <- c(drops, new_drop)
      df <- dplyr::add_row(df, k = k_curr, loglik = new_val)
      consecutive_skips <- 0L

      if (new_val < branch_cutoff) {
        break
      }

      current_par <- eval$param_hat
      current_val <- new_val
    }

    k_curr <- k_curr + k_direction
  }

  df |>
    dplyr::distinct() |>
    dplyr::arrange(k)
}

# ======================================================================
# INTERNAL HELPERS
# ======================================================================

#' @keywords internal
#' @noRd
assemble_branch_df <- function(df, grid) {
  branch_df <- df |>
    dplyr::arrange(.data$k) |>
    dplyr::distinct() |>
    dplyr::mutate(psi = grid$psi_mle + .data$k * grid$increment) |>
    dplyr::mutate(
      loglik_centered = .data$loglik - max(.data$loglik, na.rm = TRUE)
    )

  attr(branch_df, "mode_index") <- which.max(branch_df$loglik)
  attr(branch_df, "n_points") <- nrow(branch_df)
  attr(branch_df, "psi_MLE") <- grid$psi_mle
  branch_df
}
