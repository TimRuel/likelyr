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
#   $branch_df        — tibble(k, psi, loglik, rel_loglik)
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
#'     Traversal continues until the common interval boundary is reached
#'     on each side. Requires a fully populated \code{branch_seed} from
#'     \code{probe()}.
#'   \item \code{"leftright"} — single left-to-right sweep over the
#'     full grid interval, with a log-likelihood cutoff applied
#'     retrospectively. Intended as a fallback when mode pre-location
#'     is unreliable.
#' }
#'
#' @param branch_seed A named list from \code{probe()}, containing:
#'   \itemize{
#'     \item \code{omega_hat}, \code{psi_mode}, \code{param_mode},
#'       \code{ll_mode}, \code{probe_evals_df},
#'       \code{param_left_edge}, \code{param_right_edge},
#'       \code{left_boundary}, \code{right_boundary}
#'   }
#' @param traversal        A calibrated \code{traversal_spec} object.
#' @param grid             A ψ-grid object from \code{psi_grid_anchor()}.
#'   The \code{psi_lower} and \code{psi_upper} slots define the common
#'   interval boundaries that stop traversal.
#' @param branch_evaluator Function \code{(psi, param_init) ->
#'   list(param_hat, branch_val, psi_residual)}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{$branch_df}        — tibble(k, psi, loglik, rel_loglik)
#'     \item \code{$psi_hat}          — numeric scalar
#'     \item \code{$traversal_method} — character scalar
#'   }
#'
#' @keywords internal
traverse_branch <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator
) {
  method <- traversal$traversal_method %||% "topdown"

  switch(
    method,
    topdown = traverse_branch_topdown(
      branch_seed = branch_seed,
      traversal = traversal,
      grid = grid,
      branch_evaluator = branch_evaluator
    ),
    leftright = traverse_branch_leftright(
      branch_seed = branch_seed,
      traversal = traversal,
      grid = grid,
      branch_evaluator = branch_evaluator
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
#' side using the edge warm-starts from the seed. Traversal continues
#' until the common interval boundary is reached. Sides that
#' \code{probe()} stopped at a boundary are not extended.
#'
#' @inheritParams traverse_branch
#' @keywords internal
traverse_branch_topdown <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator
) {
  psi_mode <- branch_seed$psi_mode
  probe_evals_df <- branch_seed$probe_evals_df

  k_mode <- round((psi_mode - grid$psi_mle) / grid$increment)

  alpha_target <- min(1 - traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * traversal$cutoff_buffer
  branch_cutoff <- branch_seed$ll_mode - effective_crit

  psi_interval <- traversal$psi_interval %||% NULL

  # Read retry settings from traversal spec — same knobs as profile
  max_retries <- traversal$max_retries %||% 0L
  resid_tol <- traversal$resid_tol %||% 1e-3
  max_drop_frac <- traversal$max_drop_frac %||% Inf
  branch_retry_on <- traversal$branch_retry_on %||% character(0)

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
  # Extend left side
  # -------------------------------------------------------------------
  left_df <- if (isTRUE(branch_seed$left_boundary)) {
    tibble::tibble(k = integer(), loglik = numeric())
  } else {
    traverse_branch_side(
      grid = grid,
      k_direction = -1L,
      k_start = k_left_edge - 1L,
      init_guess = branch_seed$param_left_edge,
      branch_evaluator = branch_evaluator,
      branch_cutoff = branch_cutoff,
      psi_interval = psi_interval,
      max_retries = max_retries,
      resid_tol = resid_tol,
      max_drop_frac = max_drop_frac,
      branch_retry_on = branch_retry_on
    )
  }

  # -------------------------------------------------------------------
  # Extend right side
  # -------------------------------------------------------------------
  right_df <- if (isTRUE(branch_seed$right_boundary)) {
    tibble::tibble(k = integer(), loglik = numeric())
  } else {
    traverse_branch_side(
      grid = grid,
      k_direction = +1L,
      k_start = k_right_edge + 1L,
      init_guess = branch_seed$param_right_edge,
      branch_evaluator = branch_evaluator,
      branch_cutoff = branch_cutoff,
      psi_interval = psi_interval,
      max_retries = max_retries,
      resid_tol = resid_tol,
      max_drop_frac = max_drop_frac,
      branch_retry_on = branch_retry_on
    )
  }

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
#' Evaluates the branch at every grid point from left to right using
#' warm-start chaining. After the full sweep, the mode is located
#' retrospectively and points below \code{ll_mode - effective_crit}
#' are trimmed.
#'
#' @inheritParams traverse_branch
#' @keywords internal
traverse_branch_leftright <- function(
  branch_seed,
  traversal,
  grid,
  branch_evaluator
) {
  omega_hat <- branch_seed$omega_hat
  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper
  psi_mle <- grid$psi_mle
  increment <- grid$increment

  alpha_target <- min(1 - traversal$confidence_levels)
  crit <- 0.5 * stats::qchisq(1 - alpha_target, df = 1)
  effective_crit <- crit * traversal$cutoff_buffer

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
    rows[[k - k_min + 1L]] <- list(k = k, psi = psi_k, loglik = eval$branch_val)
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
  psi_hat <- df$psi[i_mode]
  branch_cutoff <- ll_mode - effective_crit

  branch_df <- df |>
    dplyr::filter(loglik > branch_cutoff) |>
    assemble_branch_df(grid)

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
#' the branch at each grid point. Stops when the common interval boundary
#' is reached or the log-likelihood falls below \code{branch_cutoff}.
#'
#' Jitter retries are triggered by any combination of the following,
#' controlled by \code{branch_retry_on}:
#' \enumerate{
#'   \item \code{"monotonicity"}: the proposed step increases the
#'     log-likelihood relative to the previous value.
#'   \item \code{"constraint"}: the psi residual at the returned
#'     solution exceeds \code{resid_tol}.
#'   \item \code{"drop"}: the proposed drop exceeds \code{max_drop_frac}
#'     times the recent median drop (once at least three recent drops
#'     are available).
#' }
#' The warm start chain only advances when \code{psi_resid <= resid_tol},
#' preventing constraint failures from corrupting subsequent steps.
#'
#' @param grid                  ψ-grid object.
#' @param k_direction           Integer +1 or -1.
#' @param k_start               Integer starting grid index.
#' @param init_guess            Numeric vector warm-start parameter.
#' @param branch_evaluator      Function (psi, param_init) -> list.
#' @param branch_cutoff         Numeric scalar. Default: \code{-Inf}.
#' @param psi_interval          A \code{sets::interval} object or NULL.
#' @param max_retries           Non-negative integer. Maximum jitter
#'   retries per step. Default: \code{0L} (no retries).
#' @param resid_tol             Non-negative numeric scalar. Constraint
#'   residual tolerance. Default: \code{1e-3}.
#' @param max_drop_frac         Positive numeric scalar. Drop threshold
#'   multiplier. Set to \code{Inf} to disable. Default: \code{Inf}.
#' @param branch_retry_on       Character vector. Which violations
#'   trigger jitter retries. Any subset of \code{c("monotonicity",
#'   "constraint", "drop")}. Default: \code{character(0)} (no retries).
#' @param max_consecutive_skips Integer. Default: \code{2L}.
#'
#' @keywords internal
traverse_branch_side <- function(
  grid,
  k_direction,
  k_start,
  init_guess,
  branch_evaluator,
  branch_cutoff = -Inf,
  psi_interval = NULL,
  max_retries = 0L,
  resid_tol = 1e-3,
  max_drop_frac = Inf,
  branch_retry_on = character(0),
  max_consecutive_skips = 2L
) {
  k_curr <- k_start
  current_par <- init_guess
  current_ll <- Inf
  consecutive_skips <- 0L
  recent_drops <- numeric(0)

  check_monotonicity <- "monotonicity" %in% branch_retry_on
  check_constraint <- "constraint" %in% branch_retry_on
  check_drop <- "drop" %in% branch_retry_on

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = numeric(), loglik = numeric())

  lower_closed <- !is.null(psi_interval) &&
    is.finite(min(psi_interval)) &&
    sets::interval_is_left_closed(psi_interval)

  upper_closed <- !is.null(psi_interval) &&
    is.finite(max(psi_interval)) &&
    sets::interval_is_right_closed(psi_interval)

  repeat {
    psi_k <- grid$psi_mle + k_curr * grid$increment

    # Use >= / <= so exact boundary grid points are caught correctly
    hit_lower <- !is.null(psi_lower) && psi_k <= psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k >= psi_upper

    if (hit_lower || hit_upper) {
      cutoff_not_reached <- current_ll > branch_cutoff
      boundary_val <- if (hit_lower) psi_lower else psi_upper
      boundary_closed <- if (hit_lower) lower_closed else upper_closed

      if (cutoff_not_reached && boundary_closed) {
        k_boundary <- (boundary_val - grid$psi_mle) / grid$increment
        if (!k_boundary %in% df$k) {
          eval <- tryCatch(
            branch_evaluator(boundary_val, current_par),
            error = function(e) NULL
          )
          if (!is.null(eval) && is.finite(eval$branch_val)) {
            df <- dplyr::add_row(df, k = k_boundary, loglik = eval$branch_val)
          }
        }
      }
      break
    }

    # -------------------------------------------------------------------
    # Evaluate with retry logic
    # -------------------------------------------------------------------
    retry <- 0L
    warm_init <- current_par
    drop <- -Inf

    repeat {
      eval <- tryCatch(
        branch_evaluator(psi_k, warm_init),
        error = function(e) NULL
      )

      if (is.null(eval) || !is.finite(eval$branch_val)) {
        # Non-finite result — count as skip, break retry loop
        eval <- NULL
        break
      }

      drop <- current_ll - eval$branch_val
      psi_resid <- abs(eval$psi_residual %||% (eval$psi_at_hat - psi_k))

      typical_drop <- if (length(recent_drops) >= 3L) {
        median(recent_drops)
      } else {
        Inf
      }

      monotone_ok <- !check_monotonicity || eval$branch_val <= current_ll
      constraint_ok <- !check_constraint || psi_resid <= resid_tol
      drop_ok <- !check_drop ||
        !(is.finite(max_drop_frac) &&
          length(recent_drops) >= 3L &&
          drop > max_drop_frac * typical_drop)

      if ((monotone_ok && constraint_ok && drop_ok) || retry >= max_retries) {
        break
      }

      retry <- retry + 1L
      warm_init <- warm_init +
        stats::rnorm(length(warm_init), sd = 0.1 * retry)
    }

    # -------------------------------------------------------------------
    # Record result or skip
    # -------------------------------------------------------------------
    if (is.null(eval) || !is.finite(eval$branch_val)) {
      consecutive_skips <- consecutive_skips + 1L
      if (consecutive_skips >= max_consecutive_skips) break
    } else {
      df <- dplyr::add_row(df, k = k_curr, loglik = eval$branch_val)
      current_ll <- eval$branch_val
      consecutive_skips <- 0L

      if (drop > 0 && is.finite(drop)) {
        recent_drops <- c(tail(recent_drops, 9L), drop)
      }

      # Advance warm start only when constraint was satisfied
      psi_resid_final <- abs(
        eval$psi_residual %||% (eval$psi_at_hat - psi_k)
      )
      if (psi_resid_final <= resid_tol) {
        current_par <- eval$param_hat
      }
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
    dplyr::arrange(k) |>
    dplyr::distinct() |>
    dplyr::mutate(
      psi = if ("psi" %in% names(df)) {
        psi
      } else {
        grid$psi_mle + k * grid$increment
      }
    ) |>
    dplyr::mutate(
      rel_loglik = loglik - max(loglik, na.rm = TRUE)
    ) |>
    dplyr::select(k, psi, loglik, rel_loglik)

  attr(branch_df, "mode_index") <- which.max(branch_df$loglik)
  attr(branch_df, "n_points") <- nrow(branch_df)
  attr(branch_df, "psi_mle") <- grid$psi_mle
  branch_df
}