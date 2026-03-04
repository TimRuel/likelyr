# =============================================================================
# screen.R — Omega-hat Screening (grid-aware, mode-stable, diversity-aware)
# =============================================================================

# ---------------------------------------------------------------------
# Helpers (internal)
# ---------------------------------------------------------------------

# Grid anchor is ALWAYS psi_mle + k * increment.
# Always returns 2*n_adjacent + 1 rows, ordered by ascending k.
.build_adjacent_psi_grid_with_mode <- function(
  psi_hat,
  psi_mle,
  increment,
  n_adjacent,
  mode_grid_tol = 1e-10
) {
  k_hat <- (psi_hat - psi_mle) / increment
  k_round <- round(k_hat)
  psi_round <- psi_mle + k_round * increment

  aligned <- is.finite(psi_round) &&
    abs(psi_hat - psi_round) <= mode_grid_tol

  if (aligned) {
    k_mode <- as.integer(k_round)
    psi_mode <- psi_round

    left_k <- seq(from = k_mode - 1L, by = -1L, length.out = n_adjacent)
    right_k <- seq(from = k_mode + 1L, by = 1L, length.out = n_adjacent)

    df <- rbind(
      data.frame(
        k = as.numeric(left_k),
        psi = psi_mle + left_k * increment,
        is_mode = FALSE
      ),
      data.frame(k = as.numeric(k_mode), psi = psi_mode, is_mode = TRUE),
      data.frame(
        k = as.numeric(right_k),
        psi = psi_mle + right_k * increment,
        is_mode = FALSE
      )
    )
  } else {
    k_left <- floor(k_hat)
    left_k <- seq(from = k_left, by = -1L, length.out = n_adjacent)
    right_k <- seq(from = k_left + 1L, by = 1L, length.out = n_adjacent)

    df <- rbind(
      data.frame(
        k = as.numeric(left_k),
        psi = psi_mle + left_k * increment,
        is_mode = FALSE
      ),
      data.frame(k = as.numeric(k_hat), psi = psi_hat, is_mode = TRUE),
      data.frame(
        k = as.numeric(right_k),
        psi = psi_mle + right_k * increment,
        is_mode = FALSE
      )
    )
  }

  k_mode_val <- df$k[df$is_mode]
  df$side <- ifelse(
    df$is_mode,
    "mode",
    ifelse(df$k < k_mode_val, "left", "right")
  )

  df[order(df$k), , drop = FALSE]
}

.eval_branch_grid <- function(branch_fn, psi_df, param_init, psi_hat) {
  res <- vector("list", nrow(psi_df))

  for (i in seq_len(nrow(psi_df))) {
    out <- try(branch_fn(psi_df$psi[i], param_init), silent = TRUE)

    if (
      inherits(out, "try-error") ||
        is.null(out$branch_val) ||
        !is.finite(out$branch_val) ||
        is.null(out$param_hat)
    ) {
      return(list(ok = FALSE, reason = "grid_eval_failed"))
    }

    res[[i]] <- list(param_hat = out$param_hat, loglik = out$branch_val)
  }

  psi_df$param_hat <- lapply(res, `[[`, "param_hat")
  psi_df$loglik <- vapply(res, `[[`, numeric(1), "loglik")
  psi_df$dist_from_mode <- abs(psi_df$psi - psi_hat)

  list(ok = TRUE, df = psi_df[order(psi_df$k), , drop = FALSE])
}

# ---- geometry checks --------------------------------------------------
# Each check returns list(ok, reason). On failure, reason is a specific
# code rather than the generic "geometry_failed", so callers can tally
# exactly which check is responsible for rejections.

.check_side_curvature <- function(grid_df, tol_mag) {
  i_mode <- which(grid_df$is_mode)
  if (length(i_mode) != 1L) {
    return(list(ok = FALSE, reason = "mode_row_missing"))
  }

  ll_left <- grid_df$loglik[seq_len(i_mode)]
  ll_right <- grid_df$loglik[seq(i_mode, nrow(grid_df))]

  # Requires >= 3 points on a side to compute second differences.
  # With n_adjacent = 1 this check is skipped — use n_adjacent >= 2.
  if (length(ll_left) >= 3L && any(diff(diff(ll_left)) > -tol_mag)) {
    return(list(ok = FALSE, reason = "curvature_failed_left"))
  }

  if (length(ll_right) >= 3L && any(diff(diff(ll_right)) > -tol_mag)) {
    return(list(ok = FALSE, reason = "curvature_failed_right"))
  }

  list(ok = TRUE, reason = "ok")
}

.check_side_balance <- function(grid_df, max_ratio = 10) {
  i_mode <- which(grid_df$is_mode)
  if (length(i_mode) != 1L) {
    return(list(ok = FALSE, reason = "mode_row_missing"))
  }

  ll_mode <- grid_df$loglik[i_mode]
  drop_left <- ll_mode - min(grid_df$loglik[seq_len(i_mode)])
  drop_right <- ll_mode - min(grid_df$loglik[seq(i_mode, nrow(grid_df))])

  if (
    !is.finite(drop_left) ||
      !is.finite(drop_right) ||
      drop_left <= 0 ||
      drop_right <= 0
  ) {
    return(list(ok = FALSE, reason = "side_drop_invalid"))
  }

  ratio <- max(drop_left, drop_right) / min(drop_left, drop_right)
  if (!is.finite(ratio) || ratio > max_ratio) {
    return(list(ok = FALSE, reason = "side_imbalance"))
  }

  list(ok = TRUE, reason = "ok")
}

.check_global_quadratic <- function(grid_df, tol) {
  # Requires >= 4 points to be a genuine fit (not exact interpolation).
  # With n_adjacent = 1 only 3 points exist and a degree-2 polynomial
  # interpolates perfectly, making the coefficient check meaningless.
  # Use n_adjacent >= 2 (giving 5+ points) for this to be informative.
  if (nrow(grid_df) < 4L) {
    return(list(ok = TRUE, reason = "skipped_too_few_points"))
  }

  fit <- try(
    stats::lm(grid_df$loglik ~ poly(grid_df$psi, 2, raw = TRUE)),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    return(list(ok = FALSE, reason = "global_quad_fit_failed"))
  }

  cf <- coef(fit)
  if (length(cf) < 3L || !is.finite(cf[3L])) {
    return(list(ok = FALSE, reason = "global_quad_coef_invalid"))
  }

  list(
    ok = (cf[3L] < tol),
    reason = if (cf[3L] < tol) "ok" else "global_quad_failed"
  )
}

.check_mode_dominance <- function(grid_df, eps) {
  i_mode <- which(grid_df$is_mode)
  if (length(i_mode) != 1L) {
    return(list(ok = FALSE, reason = "mode_row_missing"))
  }

  gap <- grid_df$loglik[i_mode] - max(grid_df$loglik[-i_mode])
  if (!is.finite(gap) || gap < eps) {
    return(list(ok = FALSE, reason = "mode_not_dominant"))
  }

  list(ok = TRUE, reason = "ok")
}

.check_side_monotone <- function(grid_df, tol) {
  i_mode <- which(grid_df$is_mode)
  if (length(i_mode) != 1L) {
    return(list(ok = FALSE, reason = "mode_row_missing"))
  }

  ll_left <- grid_df$loglik[seq_len(i_mode)]
  ll_right <- grid_df$loglik[seq(i_mode, nrow(grid_df))]

  # Left side: log-likelihood should increase toward the mode.
  if (length(ll_left) >= 2L && any(diff(ll_left) < -tol)) {
    return(list(ok = FALSE, reason = "monotone_failed_left"))
  }

  # Right side: log-likelihood should decrease away from the mode.
  if (length(ll_right) >= 2L && any(diff(ll_right) > tol)) {
    return(list(ok = FALSE, reason = "monotone_failed_right"))
  }

  list(ok = TRUE, reason = "ok")
}

# ---- diversity helpers ------------------------------------------------

.get_omega_canonicalizer <- function(nuisance) {
  f <- nuisance$omega_hat_canonicalizer
  if (is.function(f)) {
    return(f)
  }
  function(x) as.numeric(x)
}

.omega_dist <- function(a, b) {
  sqrt(sum((as.numeric(a) - as.numeric(b))^2))
}

.min_dist_to_history <- function(omega_can, history_can) {
  if (length(history_can) == 0) {
    return(Inf)
  }
  min(vapply(history_can, function(h) .omega_dist(omega_can, h), numeric(1)))
}

# ---------------------------------------------------------------------
# screen() API
# ---------------------------------------------------------------------

#' @export
screen <- function(cal, ...) UseMethod("screen")

#' @export
screen.default <- function(cal, ...) {
  stop("screen() requires a calibrated model object.", call. = FALSE)
}

#' @export
screen.calibrated <- function(
  cal,
  R = NULL,
  max_trials = NULL,
  n_adjacent = 3L,
  curvature_tol = -1e-3,
  mode_dominance_eps = NULL,
  monotone_tol = 0,
  balance_max_ratio = 10,
  mode_grid_tol = NULL,
  max_mode_shifts = 5L,
  min_omega_dist = NULL,
  min_omega_dist_from_mle = NULL,
  max_loglik_drop = NULL,
  permute_omega = c("none", "after_accept"),
  verbose = FALSE,
  trace_failures = FALSE,
  ...
) {
  if (!is_calibrated(cal)) {
    stop("screen() requires a calibrated model.", call. = FALSE)
  }
  validate_integrate_input(cal)

  permute_omega <- match.arg(permute_omega)

  nuisance <- cal$nuisance
  estimand <- cal$estimand
  optimizer <- cal$optimizer
  execution <- cal$execution
  parameter <- cal$parameter

  R <- as.integer(R %||% execution$total_branches %||% 50L)
  max_trials <- as.integer(max_trials %||% (10L * R))

  psi_mle <- estimand$psi_mle
  increment <- estimand$increment
  mode_grid_tol <- mode_grid_tol %||% (increment * 1e-8)
  mode_dominance_eps <- mode_dominance_eps %||% abs(curvature_tol)

  d_dim <- parameter$param_dim %||% length(parameter$param_mle)
  min_omega_dist <- min_omega_dist %||% (0.05 * sqrt(d_dim))
  min_omega_dist_from_mle <- min_omega_dist_from_mle %||% (0.02 * sqrt(d_dim))

  loglik_mle <- parameter$loglik_mle %||% estimand$loglik_mle %||% NULL

  if (!is.null(max_loglik_drop) && is.null(loglik_mle)) {
    warning(
      "screen(): max_loglik_drop specified but loglik_mle not found on ",
      "parameter or estimand — filter will be skipped.",
      call. = FALSE
    )
    max_loglik_drop <- NULL
  }

  if (
    !is.null(nuisance$omega_hat_initgen) &&
      !is.null(nuisance$omega_hat_sampler)
  ) {
    initgen <- nuisance$omega_hat_initgen
    sampler <- nuisance$omega_hat_sampler
    permuter <- nuisance$omega_hat_permuter
    source <- "nuisance"
  } else {
    initgen <- make_omega_hat_initgen(cal)
    sampler <- make_omega_hat_sampler(cal)
    permuter <- NULL
    source <- "legacy"
  }

  canonicalize <- .get_omega_canonicalizer(nuisance)

  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = cal$likelihood,
    estimand = estimand,
    nuisance = nuisance,
    optimizer = optimizer
  )

  # ---- diagnostics ----
  diag <- list()
  .log <- function(id, accepted, reason) {
    diag[[length(diag) + 1L]] <<- data.frame(
      candidate = id,
      accepted = accepted,
      reason = reason,
      stringsAsFactors = FALSE
    )
    if (verbose || (trace_failures && !accepted)) {
      msg <- if (accepted) "ACCEPT" else "REJECT"
      message("[screen] cand ", id, ": ", msg, " (", reason, ")")
    }
  }

  branch_seeds <- list()
  history <- list()
  history_can <- list()

  queue <- list()
  .enqueue <- function(omega, parent = NA_integer_) {
    queue[[length(queue) + 1L]] <<- list(omega = omega, parent = parent)
  }

  n_ok <- 0L
  n_try <- 0L
  cand_id <- 0L

  while (n_ok < R && (n_try < max_trials || length(queue) > 0L)) {
    if (length(queue) > 0L) {
      cand <- queue[[1L]]
      queue <- queue[-1L]
      omega <- cand$omega
      origin <- "permute"
      psi_hint <- NULL # permuted candidates have no intent object
    } else {
      n_try <- n_try + 1L
      origin <- "sample"

      init <- try(initgen(history = history), silent = TRUE)
      if (inherits(init, "try-error")) {
        cand_id <- cand_id + 1L
        .log(cand_id, FALSE, "initgen_failed")
        next
      }

      omega <- try(sampler(init), silent = TRUE)
      if (inherits(omega, "try-error")) {
        cand_id <- cand_id + 1L
        .log(cand_id, FALSE, "sampler_failed")
        next
      }
      # Sampler may return list(par, convergence, value) when
      # attach_diagnostics = TRUE — extract par before proceeding.
      if (is.list(omega)) {
        omega <- omega$par
      }

      # Extract psi_target from the intent object if initgen embedded one
      # (psi_spread = TRUE). Passed to the mode locator as a warm-start
      # hint so it begins searching near the ψ value actually enforced by
      # the sampler, rather than always starting from psi_mle. Reduces
      # unnecessary mode shifts for candidates sampled far from psi_mle.
      psi_hint <- if (is.list(init) && !is.null(init$psi_target)) {
        init$psi_target
      } else {
        NULL
      }
    }

    cand_id <- cand_id + 1L
    omega <- as.numeric(omega)
    if (any(!is.finite(omega))) {
      .log(cand_id, FALSE, "omega_nonfinite")
      next
    }

    omega_can <- canonicalize(omega)

    if (!is.null(parameter$param_mle)) {
      if (.omega_dist(omega, parameter$param_mle) < min_omega_dist_from_mle) {
        .log(cand_id, FALSE, "omega_too_close_to_mle")
        next
      }
    }

    if (.min_dist_to_history(omega_can, history_can) < min_omega_dist) {
      .log(cand_id, FALSE, "omega_duplicate")
      next
    }

    # Pass n_adjacent to the locator so its grid snap window matches
    # screen()'s geometry grid — eliminating the mismatch that causes
    # repeated mode shifts.
    mode <- try(
      optimizer$branch_mode_locator(
        omega,
        psi_hint = psi_hint,
        n_adjacent = n_adjacent
      ),
      silent = TRUE
    )
    if (inherits(mode, "try-error") || !is.finite(mode$psi_hat)) {
      .log(cand_id, FALSE, "mode_locator_failed")
      next
    }

    psi_current <- mode$psi_hat
    param_init <- mode$param_hat %||% omega
    branch_fn <- try(branch_fn_factory(omega), silent = TRUE)
    if (inherits(branch_fn, "try-error")) {
      .log(cand_id, FALSE, "branch_fn_factory_failed")
      next
    }

    stable <- FALSE
    fail_reason <- "geometry_failed"
    shift_count <- 0L

    repeat {
      grid_df <- .build_adjacent_psi_grid_with_mode(
        psi_current,
        psi_mle,
        increment,
        n_adjacent,
        mode_grid_tol
      )
      grid_res <- .eval_branch_grid(branch_fn, grid_df, param_init, psi_current)
      if (!grid_res$ok) {
        fail_reason <- grid_res$reason
        break
      }

      g <- grid_res$df
      i_mode <- which(g$is_mode)
      i_max <- which.max(g$loglik)

      if (i_max != i_mode) {
        shift_count <- shift_count + 1L
        if (shift_count > max_mode_shifts) {
          fail_reason <- "mode_shift_exhausted"
          break
        }
        psi_current <- g$psi[i_max]
        param_init <- g$param_hat[[i_max]]
        next
      }

      if (!is.null(max_loglik_drop)) {
        ll_at_mode <- g$loglik[i_mode]
        if (
          is.finite(ll_at_mode) && ll_at_mode < loglik_mle - max_loglik_drop
        ) {
          fail_reason <- "loglik_mode_too_low"
          break
        }
      }

      res <- .check_mode_dominance(g, mode_dominance_eps)
      if (!res$ok) {
        fail_reason <- res$reason
        break
      }

      res <- .check_side_monotone(g, monotone_tol)
      if (!res$ok) {
        fail_reason <- res$reason
        break
      }

      res <- .check_side_curvature(g, abs(curvature_tol))
      if (!res$ok) {
        fail_reason <- res$reason
        break
      }

      res <- .check_side_balance(g, balance_max_ratio)
      if (!res$ok) {
        fail_reason <- res$reason
        break
      }

      res <- .check_global_quadratic(g, curvature_tol)
      if (!res$ok) {
        fail_reason <- res$reason
        break
      }

      stable <- TRUE
      break
    }

    if (!stable) {
      .log(cand_id, FALSE, fail_reason)
      next
    }

    n_ok <- n_ok + 1L
    branch_seeds[[n_ok]] <- list(
      omega_hat = omega,
      psi_hat = psi_current,
      param_hat = g$param_hat[[i_mode]],
      loglik_at_mode = g$loglik[i_mode],
      origin = origin
    )

    history[[n_ok]] <- omega
    history_can[[n_ok]] <- omega_can
    .log(cand_id, TRUE, "ok")

    if (permute_omega == "after_accept" && is.function(permuter)) {
      for (p in permuter(omega)) {
        .enqueue(p$omega_hat, parent = n_ok)
      }
    }
  }

  diagnostics <- if (length(diag) > 0) do.call(rbind, diag) else data.frame()
  failure_tab <- if (NROW(diagnostics) > 0) {
    sort(table(diagnostics$reason[!diagnostics$accepted]), decreasing = TRUE)
  } else {
    integer()
  }

  cal$workspace$integrate$branch_seeds <- branch_seeds
  cal$workspace$integrate$screen <- list(
    source = source,
    R_requested = R,
    R_accepted = n_ok,
    trials = n_try,
    candidates_processed = cand_id,
    accept_rate = if (n_try > 0) n_ok / n_try else NA_real_,
    min_omega_dist = min_omega_dist,
    min_omega_dist_from_mle = min_omega_dist_from_mle,
    permute_omega = permute_omega,
    diagnostics = diagnostics,
    failure_summary = failure_tab
  )

  if (verbose && length(failure_tab) > 0) {
    message("[screen] Failure summary:")
    print(failure_tab)
  }

  cal
}
