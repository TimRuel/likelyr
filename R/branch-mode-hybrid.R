# ======================================================================
# branch-mode-hybrid.R — Hybrid Branch Mode Locator
#
# Two-phase approach:
#   1. Bracket  — step outward from psi_hint until a unimodal triplet
#                 is found; falls back to coarse grid scan if needed
#   2. Refine   — golden section search within the bracket to
#                 continuous precision
#   3. Snap     — recheck the nearest n_adjacent grid points on each
#                 side and return the best, ensuring the returned
#                 psi_hat is grid-aligned with screen()'s geometry grid
# ======================================================================

branch_mode_locator_hybrid <- function(
  branch_fn,
  psi_init,
  search_interval,
  param_init,
  psi_mle = NULL,
  increment = NULL,
  n_adjacent = 3L, # MUST match screen()'s n_adjacent; snap window
  # is ±n_adjacent increments so the locator and
  # screen agree on what constitutes the mode before
  # screen runs any geometry checks
  max_bracket_steps = 20L, # max outward steps per side in bracket phase
  n_fallback_grid = 25L, # coarse grid points used in fallback
  gss_tol = NULL # golden section convergence tolerance;
  # defaults to increment / 10
) {
  # ---------------------------------------------------------------
  # Defensive construction — never hard-error at factory time
  # ---------------------------------------------------------------
  valid_grid <- is.finite(psi_mle) &&
    is.finite(increment) &&
    increment > 0

  if (!valid_grid) {
    return(function(omega_hat, ...) {
      make_branch_mode_result(
        psi_hat = NA_real_,
        param_hat = param_init,
        loglik_at_mode = -Inf,
        status = "invalid_grid_params"
      )
    })
  }

  gss_tol <- gss_tol %||% (increment / 10)

  # ---------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------

  # Evaluate branch at a single psi; returns -Inf on failure.
  .eval <- function(psi) {
    out <- try(branch_fn(psi, param_init), silent = TRUE)
    if (
      inherits(out, "try-error") ||
        is.null(out$branch_val) ||
        !is.finite(out$branch_val)
    ) {
      return(-Inf)
    }
    out$branch_val
  }

  # Evaluate branch and return full result; returns NULL on failure.
  .eval_full <- function(psi) {
    out <- try(branch_fn(psi, param_init), silent = TRUE)
    if (
      inherits(out, "try-error") ||
        is.null(out$branch_val) ||
        !is.finite(out$branch_val)
    ) {
      return(NULL)
    }
    list(psi = psi, loglik = out$branch_val, param_hat = out$param_hat)
  }

  # ---------------------------------------------------------------
  # Phase 1a: Bracket from a starting point
  #
  # Begins with a triplet (left, mid, right) where mid = psi_start
  # and the flanking points are one increment away. Steps outward
  # on whichever side is higher until the middle is above both
  # neighbors (unimodal triplet found) or max_bracket_steps is hit.
  #
  # Returns list(lo, hi) or NULL if bracketing fails.
  # ---------------------------------------------------------------
  .bracket_from <- function(psi_start) {
    lo <- psi_start - increment
    mid <- psi_start
    hi <- psi_start + increment

    # Clip to search interval
    lo <- max(lo, search_interval[1])
    hi <- min(hi, search_interval[2])

    v_lo <- .eval(lo)
    v_mid <- .eval(mid)
    v_hi <- .eval(hi)

    for (step in seq_len(max_bracket_steps)) {
      # Interior maximum found
      if (v_mid >= v_lo && v_mid >= v_hi) {
        return(list(lo = lo, hi = hi))
      }

      # Both neighbors higher than mid — not unimodal, give up
      if (v_lo > v_mid && v_hi > v_mid) {
        return(NULL)
      }

      # Expand toward the higher side
      if (v_lo > v_hi) {
        new_lo <- lo - increment
        if (new_lo < search_interval[1]) {
          break
        }
        hi <- mid
        v_hi <- v_mid
        mid <- lo
        v_mid <- v_lo
        lo <- new_lo
        v_lo <- .eval(lo)
      } else {
        new_hi <- hi + increment
        if (new_hi > search_interval[2]) {
          break
        }
        lo <- mid
        v_lo <- v_mid
        mid <- hi
        v_mid <- v_hi
        hi <- new_hi
        v_hi <- .eval(hi)
      }
    }

    # One last check after loop
    if (v_mid >= v_lo && v_mid >= v_hi) {
      return(list(lo = lo, hi = hi))
    }

    NULL
  }

  # ---------------------------------------------------------------
  # Phase 1b: Coarse grid fallback
  #
  # Used when bracket phase fails. Scans n_fallback_grid points
  # over the full search interval and returns a bracket around the
  # best interior point, or NULL if the maximum is at a boundary.
  # ---------------------------------------------------------------
  .coarse_bracket <- function() {
    psi_grid <- make_coarse_psi_grid(search_interval, n = n_fallback_grid)
    vals <- vapply(psi_grid, .eval, numeric(1))
    idx <- safe_which_max(vals)

    if (is.na(idx)) {
      return(NULL)
    }

    # Boundary maximum — no interior bracket possible
    if (idx == 1L || idx == length(psi_grid)) {
      return(list(
        lo = psi_grid[idx],
        hi = psi_grid[idx],
        boundary_psi = psi_grid[idx],
        boundary_val = vals[idx],
        is_boundary = TRUE
      ))
    }

    list(
      lo = psi_grid[max(1L, idx - 1L)],
      hi = psi_grid[min(length(psi_grid), idx + 1L)],
      is_boundary = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Phase 2: Golden section search
  #
  # Maximizes branch_fn over [lo, hi] to within gss_tol.
  # Returns list(psi, loglik) or NULL on failure.
  # ---------------------------------------------------------------
  .golden_section <- function(lo, hi) {
    phi <- (sqrt(5) - 1) / 2 # golden ratio conjugate ≈ 0.618

    x1 <- hi - phi * (hi - lo)
    x2 <- lo + phi * (hi - lo)
    f1 <- .eval(x1)
    f2 <- .eval(x2)

    iter <- 0L
    max_iter <- ceiling(log(gss_tol / (hi - lo)) / log(phi)) + 10L

    while ((hi - lo) > gss_tol && iter < max_iter) {
      iter <- iter + 1L
      if (f1 < f2) {
        lo <- x1
        x1 <- x2
        f1 <- f2
        x2 <- lo + phi * (hi - lo)
        f2 <- .eval(x2)
      } else {
        hi <- x2
        x2 <- x1
        f2 <- f1
        x1 <- hi - phi * (hi - lo)
        f1 <- .eval(x1)
      }
    }

    psi_opt <- (lo + hi) / 2
    val_opt <- .eval(psi_opt)

    if (!is.finite(val_opt)) {
      return(NULL)
    }
    list(psi = psi_opt, loglik = val_opt)
  }

  # ---------------------------------------------------------------
  # Phase 3: Grid snap
  #
  # Re-evaluates ±n_adjacent grid points around psi_cont and returns
  # the best, ensuring the returned psi_hat is grid-aligned.
  #
  # The window matches screen()'s n_adjacent so the locator and
  # screen agree on what constitutes the mode: if the best point
  # within ±n_adjacent is at the center, screen will not shift.
  # ---------------------------------------------------------------
  .grid_snap <- function(psi_cont) {
    k_hat <- round((psi_cont - psi_mle) / increment)
    k_vec <- seq(k_hat - n_adjacent, k_hat + n_adjacent)
    psi_vec <- psi_mle + k_vec * increment

    # Clip to search interval
    psi_vec <- psi_vec[
      psi_vec >= search_interval[1] & psi_vec <= search_interval[2]
    ]

    results <- Filter(Negate(is.null), lapply(psi_vec, .eval_full))

    if (length(results) == 0L) {
      return(NULL)
    }

    ll <- vapply(results, `[[`, numeric(1), "loglik")
    i_best <- which.max(ll)
    results[[i_best]]
  }

  # ---------------------------------------------------------------
  # Main locator closure
  # ---------------------------------------------------------------
  function(omega_hat, ...) {
    # ------------------------------------------------------------
    # Phase 1: Find bracket
    # ------------------------------------------------------------

    # Try hint first (psi_target from psi_spread, or psi_mle)
    bracket <- .bracket_from(psi_init)
    used_fallback <- FALSE

    if (is.null(bracket)) {
      # Also try psi_mle if hint differs from it
      if (!isTRUE(all.equal(psi_init, psi_mle))) {
        bracket <- .bracket_from(psi_mle)
      }
    }

    if (is.null(bracket)) {
      bracket <- .coarse_bracket()
      used_fallback <- TRUE
    }

    if (is.null(bracket)) {
      return(make_branch_mode_result(
        psi_hat = NA_real_,
        param_hat = param_init,
        loglik_at_mode = -Inf,
        status = "bracket_failed"
      ))
    }

    # Boundary maximum — can't refine, return directly with snap attempt
    if (isTRUE(bracket$is_boundary)) {
      snap <- .grid_snap(bracket$boundary_psi)
      if (!is.null(snap)) {
        return(make_branch_mode_result(
          psi_hat = snap$psi,
          param_hat = snap$param_hat,
          loglik_at_mode = snap$loglik,
          status = "no_interior_mode_boundary"
        ))
      }
      return(make_branch_mode_result(
        psi_hat = bracket$boundary_psi,
        param_hat = param_init,
        loglik_at_mode = bracket$boundary_val,
        status = "no_interior_mode_boundary"
      ))
    }

    # ------------------------------------------------------------
    # Phase 2: Golden section refinement
    # ------------------------------------------------------------
    gss <- .golden_section(bracket$lo, bracket$hi)

    if (is.null(gss)) {
      # GSS failed — snap directly from bracket midpoint
      snap <- .grid_snap((bracket$lo + bracket$hi) / 2)
      if (!is.null(snap)) {
        return(make_branch_mode_result(
          psi_hat = snap$psi,
          param_hat = snap$param_hat,
          loglik_at_mode = snap$loglik,
          status = if (used_fallback) "gss_failed_fallback" else "gss_failed"
        ))
      }
      return(make_branch_mode_result(
        psi_hat = NA_real_,
        param_hat = param_init,
        loglik_at_mode = -Inf,
        status = "refinement_failed"
      ))
    }

    # ------------------------------------------------------------
    # Phase 3: Grid snap
    # ------------------------------------------------------------
    snap <- .grid_snap(gss$psi)

    if (is.null(snap)) {
      return(make_branch_mode_result(
        psi_hat = gss$psi,
        param_hat = param_init,
        loglik_at_mode = gss$loglik,
        status = "grid_snap_failed"
      ))
    }

    make_branch_mode_result(
      psi_hat = snap$psi,
      param_hat = snap$param_hat,
      loglik_at_mode = snap$loglik,
      status = if (used_fallback) "success_fallback" else "success"
    )
  }
}
