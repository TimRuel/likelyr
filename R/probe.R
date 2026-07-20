#' Probe a Candidate Omega-Hat for a Usable Branch Seed
#'
#' @description
#' Locates a candidate omega-hat's branch mode, checks it for numerical
#' validity and (adaptively) competitiveness, and — if it survives — sweeps
#' \code{n_adjacent} grid points on each side to produce a seed ready for
#' \code{traverse_branch()}. Called by \code{sieve()}; not exported.
#'
#' @param model A calibrated \code{model} object, post-\code{preprocess()}.
#' @param omega_hat Numeric vector. The candidate nuisance direction.
#' @param n_adjacent Non-negative integer. Grid points to evaluate on each
#'   side of the mode. Defaults to \code{model$traversal$n_adjacent}.
#' @param max_mode_shifts Non-negative integer. Cap on mode re-centering
#'   during the adjacent sweep. Defaults to
#'   \code{model$traversal$max_mode_shifts}.
#' @param k_recent Non-negative integer. Recent-drops window for the jump
#'   check. Defaults to \code{model$traversal$k_recent}.
#' @param drop_multiplier Positive numeric scalar. Jump-detection
#'   multiplier. Defaults to \code{model$traversal$drop_multiplier}.
#' @param rejection_reasons Optional character vector of checks to
#'   enforce; see \code{\link{traversal_spec}} for recognized values.
#' @param running_best Numeric scalar. The best (highest) \code{ll_mode}
#'   observed so far during the enclosing \code{sieve()} run, used only by
#'   the \code{"mode_uncompetitive"} gate. Default \code{-Inf} makes every
#'   finite \code{ll_mode} pass, i.e. the gate is a no-op unless the caller
#'   participates in the running-best bookkeeping (as \code{sieve()} does).
#' @importFrom stats qchisq median
#' @importFrom utils tail
#' @keywords internal
probe <- function(
  model,
  omega_hat,
  n_adjacent = NULL,
  max_mode_shifts = NULL,
  k_recent = NULL,
  drop_multiplier = NULL,
  rejection_reasons = NULL,
  running_best = -Inf
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
  resid_tol <- traversal$resid_tol %||% 1e-3
  rejection_reasons <- rejection_reasons %||% model$traversal$rejection_reasons

  # NULL means all reasons active; otherwise only listed reasons cause rejection
  .should_reject <- function(reason) {
    is.null(rejection_reasons) || reason %in% rejection_reasons
  }

  max_drop_cap <- traversal$max_drop_cap
  if (is.null(max_drop_cap)) {
    stop(
      "probe() requires model$traversal$max_drop_cap to be set.\n",
      "Run preprocess() before sieve().",
      call. = FALSE
    )
  }

  ll_at_psi_mle <- model$workspace$profile$ll_at_psi_mle
  if (is.null(ll_at_psi_mle)) {
    stop(
      "probe() requires model$workspace$profile$ll_at_psi_mle to be set.\n",
      "Run preprocess() before sieve().",
      call. = FALSE
    )
  }

  alpha_target <- min(1 - traversal$confidence_levels)
  crit <- 0.5 * qchisq(1 - alpha_target, df = 1)
  ll_threshold <- ll_at_psi_mle - traversal$mode_gap_multiplier * crit
  
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

    if (k_min > k_max && .should_reject("empty_restricted_grid")) {
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

  # A located mode is mandatory: every step below (snap, adjacent sweep,
  # seed edges) dereferences it. A locator failure is therefore a HARD
  # reject, NOT subject to rejection_reasons filtering — otherwise a NULL
  # mode_result propagates into numeric(0) / min(numeric(0)) downstream.
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

  if (!.k_in_grid(k_mode) && .should_reject("no_feasible_grid_point")) {
    return(list(
      accepted = FALSE,
      reason = "no_feasible_grid_point",
      omega_hat = omega_hat
    ))
  }

  # -------------------------------------------------------------------
  # 3b. Reject if mode sits on a psi boundary, unless psi_mle is itself
  #     near that boundary. When psi_mle is at or near a boundary (k_max
  #     or k_min close to zero), monotone branch profiles peaking at that
  #     boundary are correct behavior, not a pathology.
  # -------------------------------------------------------------------
  boundary_k_tol <- as.integer(0.1 / increment)

  if (.should_reject("mode_on_psi_boundary")) {
    if (k_mode == k_max && k_max > boundary_k_tol) {
      return(list(
        accepted = FALSE,
        reason = "mode_on_psi_boundary",
        psi_mode = psi_mode,
        k_mode = k_mode,
        k_min = k_min,
        k_max = k_max,
        omega_hat = omega_hat
      ))
    }
    if (k_mode == k_min && k_min < -boundary_k_tol) {
      return(list(
        accepted = FALSE,
        reason = "mode_on_psi_boundary",
        psi_mode = psi_mode,
        k_mode = k_mode,
        k_min = k_min,
        k_max = k_max,
        omega_hat = omega_hat
      ))
    }
  }

  # -------------------------------------------------------------------
  # 4. Re-evaluate at snapped mode
  # -------------------------------------------------------------------
  branch_evaluator <- traversal$branch_binder(omega_hat)

  mode_eval <- .eval_safe(branch_evaluator, psi_mode, param_mode)
  # Hard reject (not filterable): without a mode evaluation there is no
  # param_mode / ll_mode to build a seed from, so guarding this on
  # rejection_reasons would let NULL propagate into the sweep below.
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
  # 4a-B. Numerical-validity gate: the mode log-likelihood must be finite.
  #
  # A NaN / -Inf branch_val is a computational failure, not a shape. Left
  # unchecked it propagates into the drop/threshold comparisons below as
  # NA and surfaces as an opaque "probe_error". This is a validity gate,
  # not a niceness screen.
  # -------------------------------------------------------------------
  if (!is.finite(ll_mode) && .should_reject("mode_nonfinite")) {
    return(list(
      accepted = FALSE,
      reason = "mode_nonfinite",
      psi_mode = psi_mode,
      ll_mode = ll_mode,
      omega_hat = omega_hat
    ))
  }

  # -------------------------------------------------------------------
  # 4a-A. Numerical-validity gate: the mode solve must satisfy the
  # equality constraint psi(theta) = psi_mode. If auglag returned an
  # infeasible point, ll_mode is the log-likelihood at the WRONG psi and
  # the whole seed is numerically bogus. This is the principled
  # replacement for the crude mode_on_psi_boundary screen: a genuine
  # boundary-peaking branch whose solve is FEASIBLE is kept; only an
  # actually-failed solve is rejected. Also logs solver non-convergence
  # (maxeval hit) as a diagnostic without rejecting on it (gate C).
  # -------------------------------------------------------------------
  mode_resid <- abs(
    mode_eval$psi_residual %||% (mode_eval$psi_at_hat - psi_mode)
  )
  if (isTRUE(mode_resid > resid_tol) && .should_reject("mode_infeasible")) {
    return(list(
      accepted = FALSE,
      reason = "mode_infeasible",
      psi_mode = psi_mode,
      psi_residual = mode_resid,
      omega_hat = omega_hat
    ))
  }

  # -------------------------------------------------------------------
  # 4b. Relevance gate: is this branch's peak within reach of the best
  # branch seen so far this sieve() run? A branch whose OWN mode already
  # sits more than effective_crit below running_best can never contribute
  # above the log-sum-exp noise floor at any psi (same reasoning
  # branch_extent = "global" uses post-hoc in generate() — this is that
  # same test applied pre-emptively, before paying for the adjacent sweep
  # below or a full traverse_branch() in generate()).
  #
  # This is NOT a validity gate: ll_mode was successfully, informatively
  # measured. sieve() must still count a mode_uncompetitive rejection
  # toward R in aggregate() — discarding a known-low measurement from BOTH
  # the sum (correctly, its contribution is negligible) AND the count
  # would inflate the aggregate (the mode_too_low screen's undiagnosed
  # flaw, see the 2026-07-18 ablation). Contrast with mode_nonfinite /
  # mode_infeasible / mode_locator_failed above: those are measurement
  # FAILURES (nothing was learned), correctly excluded from R entirely.
  # -------------------------------------------------------------------
  effective_crit <- crit * traversal$cutoff_buffer
  if (
    ll_mode < (running_best - effective_crit) &&
      .should_reject("mode_uncompetitive")
  ) {
    return(list(
      accepted = FALSE,
      reason = "mode_uncompetitive",
      psi_mode = psi_mode,
      ll_mode = ll_mode,
      running_best = running_best,
      effective_crit = effective_crit,
      omega_hat = omega_hat
    ))
  }

  # -------------------------------------------------------------------
  # 4c. Legacy: reject if branch mode log-likelihood is too far below
  # profile MLE (fixed, profile-relative threshold — off by default in
  # favor of the adaptive mode_uncompetitive gate above; retained for
  # backward compatibility).
  # -------------------------------------------------------------------
  if (ll_mode < ll_threshold && .should_reject("mode_too_low")) {
    return(list(
      accepted = FALSE,
      reason = "mode_too_low",
      psi_mode = psi_mode,
      ll_mode = ll_mode,
      ll_threshold = ll_threshold,
      ll_at_psi_mle = ll_at_psi_mle,
      omega_hat = omega_hat
    ))
  }

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

    if (left_rose && right_rose && .should_reject("oscillation")) {
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
      if (
        n_shifts > max_mode_shifts && .should_reject("mode_shift_exhausted")
      ) {
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

      if (.should_reject("mode_on_psi_boundary")) {
        if (k_mode == k_max && k_max > boundary_k_tol) {
          return(list(
            accepted = FALSE,
            reason = "mode_on_psi_boundary",
            psi_mode = psi_mode,
            k_mode = k_mode,
            k_min = k_min,
            k_max = k_max,
            omega_hat = omega_hat
          ))
        }
        if (k_mode == k_min && k_min < -boundary_k_tol) {
          return(list(
            accepted = FALSE,
            reason = "mode_on_psi_boundary",
            psi_mode = psi_mode,
            k_mode = k_mode,
            k_min = k_min,
            k_max = k_max,
            omega_hat = omega_hat
          ))
        }
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
        !check_drop(
          drop_left,
          recent,
          drop_multiplier,
          max_drop_cap,
          k_recent
        ) &&
          .should_reject("jump_left")
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
        !check_drop(
          drop_right,
          recent,
          drop_multiplier,
          max_drop_cap,
          k_recent
        ) &&
          .should_reject("jump_right")
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
