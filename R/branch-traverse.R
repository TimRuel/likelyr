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
      init_guess = branch_seed$param_mode,
      warm_init = branch_seed$param_left_edge,
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
      init_guess = branch_seed$param_mode,
      warm_init = branch_seed$param_right_edge,
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
#' the branch at each grid point using a multi-start strategy. At each
#' step, the following starts are tried in order:
#' \enumerate{
#'   \item The current warm start (chained from the previous step).
#'   \item \code{init_guess} — the branch mode parameter, providing a
#'     global anchor that breaks chain dependency when the warm start
#'     has drifted.
#'   \item \code{max_retries} jittered copies of \code{init_guess},
#'     with jitter scale increasing with retry index.
#'   \item A final fresh attempt with the warm start, matching the
#'     fallback pattern of \code{traverse_profile_side()}.
#' }
#' The best feasible result (highest \code{branch_val} among results
#' with \code{psi_resid <= resid_tol}) is selected. If no feasible
#' result is found, the first non-NULL infeasible result is used as a
#' fallback. The warm-start chain advances only from feasible steps,
#' preventing constraint failures from corrupting subsequent steps.
#'
#' Traversal stops when the common interval boundary is reached or the
#' log-likelihood falls below \code{branch_cutoff}. The first point
#' below the cutoff is included, matching the behavior of
#' \code{traverse_profile_side()}.
#'
#' Unlike the profile traversal, all starts failing is a soft skip
#' rather than a hard error — branches can have gaps while the profile
#' cannot.
#'
#' @param grid                  ψ-grid object.
#' @param k_direction           Integer +1 or -1.
#' @param k_start               Integer starting grid index.
#' @param init_guess            Numeric vector. Global anchor warm start
#'   (branch mode parameter). Used at every step as a fallback.
#' @param warm_init             Numeric vector. Local warm start from
#'   the probe edge. Used as the first start at the first step only;
#'   thereafter chained from the previous accepted step.
#' @param branch_evaluator      Function (psi, param_init) -> list.
#' @param branch_cutoff         Numeric scalar. Default: \code{-Inf}.
#' @param psi_interval          A \code{sets::interval} object or NULL.
#' @param max_retries           Non-negative integer. Number of jittered
#'   copies of \code{init_guess} to try beyond the warm-start and
#'   \code{init_guess} itself. Default: \code{0L}.
#' @param resid_tol             Non-negative numeric scalar. Constraint
#'   residual tolerance. Default: \code{1e-3}.
#' @param max_drop_frac         Retained for API compatibility.
#'   Default: \code{Inf}.
#' @param branch_retry_on       Retained for API compatibility.
#' @param max_consecutive_skips Integer. Default: \code{2L}.
#'
#' @keywords internal
traverse_branch_side <- function(
  grid,
  k_direction,
  k_start,
  init_guess,
  warm_init,
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
  current_par <- warm_init
  current_ll <- Inf
  consecutive_skips <- 0L

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = numeric(), loglik = numeric())

  lower_closed <- !is.null(psi_interval) &&
    is.finite(min(psi_interval)) &&
    sets::interval_is_left_closed(psi_interval)

  upper_closed <- !is.null(psi_interval) &&
    is.finite(max(psi_interval)) &&
    sets::interval_is_right_closed(psi_interval)

  # -------------------------------------------------------------------
  # Evaluate a single start safely; returns NULL on error or non-finite
  # -------------------------------------------------------------------
  .try_start <- function(psi_k, start) {
    ev <- tryCatch(
      branch_evaluator(psi_k, start),
      error = function(e) NULL
    )
    if (is.null(ev) || !is.finite(ev$branch_val)) {
      return(NULL)
    }
    ev
  }

  # -------------------------------------------------------------------
  # Multi-start evaluation: warm start, global anchor, jittered anchors,
  # and a final fresh warm-start attempt as last resort.
  # Returns the best feasible result, falling back to the first
  # infeasible result, falling back to a fresh warm-start attempt.
  # -------------------------------------------------------------------
  .best_eval <- function(psi_k, warm) {
    starts <- c(
      list(warm),
      list(init_guess),
      lapply(seq_len(max_retries), function(i) {
        init_guess + stats::rnorm(length(init_guess), sd = 0.3 * i)
      })
    )

    best_feasible <- NULL
    fallback <- NULL

    for (start in starts) {
      ev <- .try_start(psi_k, start)
      if (is.null(ev)) next

      psi_resid <- abs(ev$psi_residual %||% (ev$psi_at_hat - psi_k))
      feasible <- psi_resid <= resid_tol

      if (feasible) {
        if (
          is.null(best_feasible) ||
            ev$branch_val > best_feasible$branch_val
        ) {
          best_feasible <- ev
        }
      } else if (is.null(fallback)) {
        fallback <- ev
      }
    }

    best_feasible %||% fallback %||% .try_start(psi_k, warm)
  }

  repeat {
    psi_k <- grid$psi_mle + k_curr * grid$increment

    hit_lower <- !is.null(psi_lower) && psi_k <= psi_lower
    hit_upper <- !is.null(psi_upper) && psi_k >= psi_upper

    if (hit_lower || hit_upper) {
      cutoff_not_reached <- current_ll > branch_cutoff
      boundary_val <- if (hit_lower) psi_lower else psi_upper
      boundary_closed <- if (hit_lower) lower_closed else upper_closed

      if (cutoff_not_reached && boundary_closed) {
        k_boundary <- (boundary_val - grid$psi_mle) / grid$increment
        if (!k_boundary %in% df$k) {
          ev <- .try_start(boundary_val, current_par)
          if (!is.null(ev)) {
            df <- dplyr::add_row(df, k = k_boundary, loglik = ev$branch_val)
          }
        }
      }
      break
    }

    # -------------------------------------------------------------------
    # Multi-start evaluation
    # -------------------------------------------------------------------
    eval <- .best_eval(psi_k, current_par)

    if (is.null(eval)) {
      consecutive_skips <- consecutive_skips + 1L
      if (consecutive_skips >= max_consecutive_skips) break
      k_curr <- k_curr + k_direction
      next
    }

    # -------------------------------------------------------------------
    # Record point, then check cutoff — include first point below cutoff
    # to match profile traversal behavior
    # -------------------------------------------------------------------
    df <- dplyr::add_row(df, k = k_curr, loglik = eval$branch_val)
    current_ll <- eval$branch_val
    consecutive_skips <- 0L

    if (eval$branch_val < branch_cutoff) break

    # -------------------------------------------------------------------
    # Advance warm start only when constraint was satisfied
    # -------------------------------------------------------------------
    psi_resid_final <- abs(
      eval$psi_residual %||% (eval$psi_at_hat - psi_k)
    )
    if (psi_resid_final <= resid_tol) {
      current_par <- eval$param_hat
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
assemble_branch_df <- function(df, grid, cap_multiplier = 10.0) {
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
    dplyr::select(k, psi, loglik, rel_loglik) |>
    trim_branch_endpoints(cap_multiplier = cap_multiplier)

  attr(branch_df, "mode_index") <- which.max(branch_df$loglik)
  attr(branch_df, "n_points") <- nrow(branch_df)
  attr(branch_df, "psi_mle") <- grid$psi_mle
  branch_df
}