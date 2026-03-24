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
#'   list(param_hat, branch_val)}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{$branch_df}        — tibble(psi, loglik,
#'       loglik_centered, k)
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
  grid,
  branch_evaluator
) {
  psi_mode <- branch_seed$psi_mode
  probe_evals_df <- branch_seed$probe_evals_df

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
      init_guess = branch_seed$param_left_edge,
      branch_evaluator = branch_evaluator
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
      init_guess = branch_seed$param_right_edge,
      branch_evaluator = branch_evaluator
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
#' the branch at each grid point. Stops when the common interval boundary
#' is reached. Points where the solver returns a non-finite value are
#' skipped; if \code{max_consecutive_skips} such failures occur in a
#' row, the side stops early.
#'
#' @param grid                  ψ-grid object. \code{psi_lower} and
#'   \code{psi_upper} define the stopping boundaries.
#' @param k_direction           Integer +1 or -1.
#' @param k_start               Integer starting grid index.
#' @param init_guess            Numeric vector warm-start parameter.
#' @param branch_evaluator      Function (psi, param_init) -> list.
#' @param max_consecutive_skips Integer. Stop side after this many
#'   consecutive solver failures. Default: \code{2L}.
#'
#' @keywords internal
traverse_branch_side <- function(
  grid,
  k_direction,
  k_start,
  init_guess,
  branch_evaluator,
  max_consecutive_skips = 2L
) {
  k_curr <- k_start
  current_par <- init_guess
  consecutive_skips <- 0L

  psi_lower <- grid$psi_lower
  psi_upper <- grid$psi_upper

  df <- tibble::tibble(k = integer(), loglik = numeric())

  repeat {
    psi_k <- grid$psi_mle + k_curr * grid$increment

    if (!is.null(psi_lower) && psi_k < psi_lower) {
      break
    }
    if (!is.null(psi_upper) && psi_k > psi_upper) {
      break
    }

    eval <- tryCatch(
      branch_evaluator(psi_k, current_par),
      error = function(e) NULL
    )

    if (is.null(eval) || !is.finite(eval$branch_val)) {
      consecutive_skips <- consecutive_skips + 1L
      if (consecutive_skips >= max_consecutive_skips) break
    } else {
      df <- dplyr::add_row(df, k = k_curr, loglik = eval$branch_val)
      consecutive_skips <- 0L
      current_par <- eval$param_hat
    }

    k_curr <- k_curr + k_direction
  }

  df |> dplyr::distinct() |> dplyr::arrange(k)
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
