# ======================================================================
# probe.R — Omega-Hat Pre-Check
# ======================================================================

#' @importFrom stats qchisq median
#' @importFrom utils tail
#' @keywords internal
probe <- function(
  model,
  omega_hat,
  n_adjacent = NULL,
  max_mode_shifts = NULL,
  k_recent = NULL,
  drop_multiplier = NULL
) {
  traversal <- model$traversal
  estimand <- model$estimand
  increment <- traversal$increment
  psi_mle <- estimand$psi_mle
  psi_interval <- estimand$psi_interval

  n_adjacent <- n_adjacent %||% traversal$n_adjacent
  max_mode_shifts <- max_mode_shifts %||% traversal$max_mode_shifts
  k_recent <- k_recent %||% traversal$k_recent
  drop_multiplier <- drop_multiplier %||% traversal$drop_multiplier

  max_drop_cap <- traversal$max_drop_cap
  if (is.null(max_drop_cap)) {
    stop(
      "probe() requires model$traversal$max_drop_cap to be set.\n",
      "Run preprocess() before sieve().",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # 1. Compute restricted grid bounds k_min, k_max
  # -------------------------------------------------------------------
  if (is.finite(min(psi_interval)) || is.finite(max(psi_interval))) {
    lo <- min(psi_interval)
    hi <- max(psi_interval)
    lo_open <- !sets::interval_is_left_closed(psi_interval)
    hi_open <- !sets::interval_is_right_closed(psi_interval)

    k_min <- if (isTRUE(lo_open)) {
      floor((lo - psi_mle) / increment) + 1L
    } else {
      ceiling((lo - psi_mle) / increment)
    }

    k_max <- if (isTRUE(hi_open)) {
      ceiling((hi - psi_mle) / increment) - 1L
    } else {
      floor((hi - psi_mle) / increment)
    }

    if (k_min > k_max) {
      return(list(
        accepted = FALSE,
        reason = "empty_restricted_grid",
        omega_hat = omega_hat
      ))
    }
  } else {
    k_min <- -Inf
    k_max <- Inf
  }

  .k_in_grid <- function(k) k >= k_min && k <= k_max

  .eval_safe <- function(branch_evaluator, psi, param_init) {
    tryCatch(
      branch_evaluator(psi, param_init),
      error = function(e) NULL
    )
  }

  .make_probe_evals_df <- function(k_mode, evals_left, ll_mode, evals_right) {
    k_left_seq <- if (length(evals_left) > 0L) {
      (k_mode - length(evals_left)):(k_mode - 1L)
    } else {
      integer(0)
    }
    k_right_seq <- if (length(evals_right) > 0L) {
      (k_mode + 1L):(k_mode + length(evals_right))
    } else {
      integer(0)
    }
    k_seq <- c(k_left_seq, k_mode, k_right_seq)
    data.frame(
      k = k_seq,
      psi = psi_mle + k_seq * increment,
      loglik = c(rev(evals_left), ll_mode, evals_right),
      side = c(
        rep("left", length(evals_left)),
        "mode",
        rep("right", length(evals_right))
      )
    )
  }

  # -------------------------------------------------------------------
  # 2. Locate mode
  # -------------------------------------------------------------------
  mode_result <- tryCatch(
    traversal$locate_mode(omega_hat),
    error = function(e) NULL
  )

  if (is.null(mode_result) || mode_result$status != "success") {
    return(list(
      accepted = FALSE,
      reason = "mode_locator_failed",
      omega_hat = omega_hat
    ))
  }

  psi_mode <- mode_result$psi_hat
  param_mode <- mode_result$param_hat
  ll_mode <- mode_result$loglik_at_mode

  # -------------------------------------------------------------------
  # 3. Snap mode to nearest point in restricted grid
  # -------------------------------------------------------------------
  k_mode_raw <- round((psi_mode - psi_mle) / increment)
  k_mode <- as.integer(min(max(k_mode_raw, k_min), k_max))
  psi_mode <- psi_mle + k_mode * increment

  if (!.k_in_grid(k_mode)) {
    return(list(
      accepted = FALSE,
      reason = "no_feasible_grid_point",
      omega_hat = omega_hat
    ))
  }

  # -------------------------------------------------------------------
  # 4. Re-evaluate at snapped mode
  # -------------------------------------------------------------------
  branch_evaluator <- traversal$branch_binder(omega_hat)

  mode_eval <- .eval_safe(branch_evaluator, psi_mode, param_mode)
  if (is.null(mode_eval)) {
    return(list(
      accepted = FALSE,
      reason = "mode_eval_failed_after_snap",
      psi_mode = psi_mode,
      omega_hat = omega_hat
    ))
  }

  param_mode <- mode_eval$param_hat
  ll_mode <- mode_eval$branch_val

  # -------------------------------------------------------------------
  # 5. Evaluate in lockstep along restricted grid
  # -------------------------------------------------------------------
  n_shifts <- 0L
  ll_left <- ll_mode
  ll_right <- ll_mode
  param_left <- param_mode
  param_right <- param_mode
  drops_left <- numeric(0)
  drops_right <- numeric(0)
  evals_left <- numeric(0)
  evals_right <- numeric(0)
  left_done <- FALSE
  right_done <- FALSE

  step <- 1L
  while (step <= n_adjacent) {
    k_left <- k_mode - step
    k_right <- k_mode + step

    if (!left_done) {
      left_done <- !.k_in_grid(k_left)
    }
    if (!right_done) {
      right_done <- !.k_in_grid(k_right)
    }

    if (left_done && right_done) {
      break
    }

    psi_left <- psi_mle + k_left * increment
    psi_right <- psi_mle + k_right * increment

    res_left <- if (!left_done) {
      .eval_safe(branch_evaluator, psi_left, param_left)
    } else {
      NULL
    }
    res_right <- if (!right_done) {
      .eval_safe(branch_evaluator, psi_right, param_right)
    } else {
      NULL
    }

    new_ll_left <- if (!is.null(res_left)) res_left$branch_val else -Inf
    new_ll_right <- if (!is.null(res_right)) res_right$branch_val else -Inf

    left_rose <- !left_done && new_ll_left > ll_left
    right_rose <- !right_done && new_ll_right > ll_right

    if (left_rose && right_rose) {
      probe_evals_df <- .make_probe_evals_df(
        k_mode,
        c(evals_left, new_ll_left),
        ll_mode,
        c(evals_right, new_ll_right)
      )
      return(list(
        accepted = FALSE,
        reason = "oscillation",
        psi_mode = psi_mode,
        omega_hat = omega_hat,
        step = step,
        probe_evals_df = probe_evals_df,
        drops_left = drops_left,
        drops_right = drops_right,
        new_ll_left = new_ll_left,
        new_ll_right = new_ll_right
      ))
    }

    if (left_rose || right_rose) {
      n_shifts <- n_shifts + 1L
      if (n_shifts > max_mode_shifts) {
        probe_evals_df <- .make_probe_evals_df(
          k_mode,
          evals_left,
          ll_mode,
          evals_right
        )
        return(list(
          accepted = FALSE,
          reason = "mode_shift_exhausted",
          psi_mode = psi_mode,
          omega_hat = omega_hat,
          n_shifts = n_shifts,
          probe_evals_df = probe_evals_df,
          drops_left = drops_left,
          drops_right = drops_right
        ))
      }

      if (left_rose) {
        psi_mode <- psi_left
        param_mode <- res_left$param_hat
        ll_mode <- new_ll_left
        k_mode <- k_left
      } else {
        psi_mode <- psi_right
        param_mode <- res_right$param_hat
        ll_mode <- new_ll_right
        k_mode <- k_right
      }

      ll_left <- ll_mode
      ll_right <- ll_mode
      param_left <- param_mode
      param_right <- param_mode
      drops_left <- numeric(0)
      drops_right <- numeric(0)
      evals_left <- numeric(0)
      evals_right <- numeric(0)
      left_done <- FALSE
      right_done <- FALSE
      step <- 1L
      next
    }

    if (!left_done && !is.null(res_left)) {
      drop_left <- ll_left - new_ll_left
      recent <- tail(drops_left, k_recent)
      if (
        !check_drop(drop_left, recent, drop_multiplier, max_drop_cap, k_recent)
      ) {
        probe_evals_df <- .make_probe_evals_df(
          k_mode,
          c(evals_left, new_ll_left),
          ll_mode,
          evals_right
        )
        return(list(
          accepted = FALSE,
          reason = "jump_left",
          psi_mode = psi_mode,
          omega_hat = omega_hat,
          step = step,
          drop_left = drop_left,
          recent_drops = recent,
          ref_median = median(recent),
          threshold = drop_multiplier * median(recent),
          probe_evals_df = probe_evals_df,
          drops_left = c(drops_left, drop_left),
          drops_right = drops_right
        ))
      }
      drops_left <- c(drops_left, drop_left)
      evals_left <- c(evals_left, new_ll_left)
      ll_left <- new_ll_left
      param_left <- res_left$param_hat
    }

    if (!right_done && !is.null(res_right)) {
      drop_right <- ll_right - new_ll_right
      recent <- tail(drops_right, k_recent)
      if (
        !check_drop(drop_right, recent, drop_multiplier, max_drop_cap, k_recent)
      ) {
        probe_evals_df <- .make_probe_evals_df(
          k_mode,
          evals_left,
          ll_mode,
          c(evals_right, new_ll_right)
        )
        return(list(
          accepted = FALSE,
          reason = "jump_right",
          psi_mode = psi_mode,
          omega_hat = omega_hat,
          step = step,
          drop_right = drop_right,
          recent_drops = recent,
          ref_median = median(recent),
          threshold = drop_multiplier * median(recent),
          probe_evals_df = probe_evals_df,
          drops_left = drops_left,
          drops_right = c(drops_right, drop_right)
        ))
      }
      drops_right <- c(drops_right, drop_right)
      evals_right <- c(evals_right, new_ll_right)
      ll_right <- new_ll_right
      param_right <- res_right$param_hat
    }

    step <- step + 1L
  }

  probe_evals_df <- .make_probe_evals_df(
    k_mode,
    evals_left,
    ll_mode,
    evals_right
  )

  list(
    accepted = TRUE,
    reason = "ok",
    omega_hat = omega_hat,
    psi_mode = psi_mode,
    ll_mode = ll_mode,
    param_mode = param_mode,
    probe_evals_df = probe_evals_df,
    drops_left = drops_left,
    drops_right = drops_right,
    n_shifts = n_shifts,
    param_left_edge = param_left,
    param_right_edge = param_right,
    left_boundary = left_done,
    right_boundary = right_done
  )
}
