# ======================================================================
# spec-traversal.R — Traversal Specification (v2.3)
# ======================================================================

#' Specify the Branch Traversal Strategy
#'
#' @param increment Required. Positive numeric scalar, ψ-grid spacing.
#' @param traversal_method Character scalar. One of \code{"topdown"},
#'   \code{"leftright"}. Default: \code{"topdown"}.
#' @param warmstart_fn Optional function
#'   \code{(psi_curr, psi_next, param_curr) -> numeric vector}.
#'   When supplied, called at each profile traversal step to predict a
#'   warm start for the next constrained optimization. A typical choice
#'   is the implicit function theorem tangent predictor:
#'   \deqn{
#'     \hat{\theta}_{k+1} \approx \hat{\theta}_k +
#'     \frac{\delta\psi}{\|\nabla\psi(\hat{\theta}_k)\|^2}
#'     \nabla\psi(\hat{\theta}_k)
#'   }
#'   When \code{NULL} (default), the previous \code{param_hat} is used
#'   directly as the warm start.
#' @param mode_locator_fn Optional constructor function returning a
#'   closure \code{function(omega_hat) -> mode result}. Always used for
#'   integrated likelihood branch mode location. Optionally used for
#'   the profile when \code{use_mode_locator_for_profile = TRUE}.
#'   When \code{NULL}, the built-in \code{bracket_gss} locator is used
#'   for the integrated likelihood.
#' @param confidence_levels Numeric vector in (0, 1).
#'   Default: \code{c(0.90, 0.95, 0.99)}.
#' @param cutoff_buffer Positive numeric scalar. Each branch is required
#'   to extend this multiple of the theoretical minimum cutoff distance.
#'   Default: \code{1.5}.
#' @param n_adjacent Non-negative integer. Number of grid points to
#'   evaluate on each side of the mode during \code{probe()}.
#'   Default: \code{3L}.
#' @param max_mode_shifts Non-negative integer. Maximum number of mode
#'   shifts permitted during \code{probe()} before rejecting.
#'   Default: \code{20L}.
#' @param k_recent Non-negative integer. Number of recent drops used
#'   as reference in the \code{probe()} drop magnitude check.
#'   Default: \code{3L}.
#' @param drop_multiplier Positive numeric scalar. A new drop exceeding
#'   this multiple of the recent median drop is flagged as a jump.
#'   Default: \code{2.0}.
#' @param cap_multiplier Positive numeric scalar. The absolute drop cap
#'   is set to \code{cap_multiplier} times the median per-step drop
#'   observed along the profile likelihood. Computed and stored on
#'   \code{model$traversal$max_drop_cap} during \code{preprocess()}.
#'   Default: \code{10.0}.
#' @param mode_gap_multiplier Positive numeric scalar. A branch whose
#'   mode log-likelihood is more than \code{mode_gap_multiplier *
#'   effective_crit} below the profile log-likelihood at the MLE is
#'   rejected as uninformative. Default: \code{1.0}.
#' @param interval_buffer Positive numeric scalar. Multiplicative
#'   expansion factor applied to the profile likelihood half-width
#'   when computing the common ψ interval during \code{preprocess()}.
#'   Default: \code{1.0}.
#' @param max_drop_frac Positive numeric scalar. During profile
#'   traversal, a proposed drop exceeding \code{max_drop_frac} times
#'   the recent median drop is treated as a suspected catastrophic local
#'   optimum and retried with jitter. Set to \code{Inf} to disable.
#'   Default: \code{10.0}.
#' @param resid_tol Non-negative numeric scalar. Constraint residual
#'   tolerance used during profile traversal. Steps exceeding the
#'   tolerance trigger jitter retries and do not advance the warm start
#'   chain. Default: \code{1e-3}.
#' @param profile_retry_on Character vector. Which violations trigger
#'   jitter retries during profile traversal. Any subset of
#'   \code{c("monotonicity", "constraint", "drop")}. Default: all three.
#' @param branch_retry_on Character vector. Which violations trigger
#'   jitter retries during integrated-likelihood branch traversal.
#'   Default: \code{character(0)} (none).
#' @param max_retries Optional non-negative integer scalar. Number of
#'   jittered warm-start restarts attempted per grid point during
#'   integrated-likelihood branch traversal (the \code{.best_eval}
#'   multistart in \code{traverse_branch_side()}). When \code{NULL}
#'   (default), \code{calibrate_traversal()} inherits the solver's
#'   \code{max_retries} so branches and the profile share a retry budget.
#'   Under \code{branch_selection = "envelope"} a large budget drives
#'   branches toward the (discontinuous) global envelope, so prefer small
#'   values there; under \code{"continuation"} the extra starts only
#'   rescue a failed warm start, so a larger budget is harmless.
#' @param branch_selection Character scalar. How the branch sweep chooses
#'   among multi-start results. \code{"envelope"} (default) takes the
#'   highest feasible \code{branch_val} over all starts — the historical
#'   behavior, which traces the global upper envelope and is discontinuous
#'   where competing optima cross. \code{"continuation"} prefers the
#'   chained warm start whenever feasible (riding a single principal
#'   branch), using restarts only to rescue a failed warm start and
#'   soft-skipping points where no feasible solve exists rather than
#'   recording an off-constraint value. \code{"continuation"} yields
#'   smoother branches at the cost of tracking a local rather than global
#'   optimum. Ignored by \code{traversal_method = "leftright"}, which is
#'   always a pure continuation.
#' @param branch_extent Character scalar controlling how far each branch is
#'   traversed outward from its mode. \code{"per_branch"} (default) stops
#'   each branch when it falls \code{effective_crit} below its \emph{own}
#'   mode. \code{"global"} stops it when it falls \code{effective_crit}
#'   below the \emph{aggregate} maximum (estimated as the max branch-mode
#'   log-likelihood across seeds) — i.e. it computes each branch only where
#'   it can still matter to the log-sum-exp average, which is both more
#'   efficient (low-mode branches barely traverse) and makes the aggregated
#'   curve reach its CI cutoff by construction. The aggregate-max estimate
#'   underestimates the true aggregate max, so \code{"global"} errs toward
#'   traversing slightly too far (safe: it never truncates the CI region).
#'   Requires the aggregate-max estimate to be supplied at generate time;
#'   falls back to \code{"per_branch"} behavior when it is not.
#' @param use_mode_locator_for_profile Logical. When \code{TRUE} and
#'   \code{mode_locator_fn} is supplied, the mode locator is called with
#'   the profile reference \code{omega_hat} to find the true profile
#'   mode before sweeping outward. Useful when the profile maximum does
#'   not coincide with \code{psi_mle} — e.g. when the parameter of
#'   interest is defined conditionally at a reference covariate and the
#'   surrogate objective is not perfectly aligned with the observed
#'   log-likelihood. Default: \code{FALSE}.
#' @param rejection_reasons Optional character vector of probe rejection
#'   checks to enforce during \code{sieve()}. \code{NULL} (default)
#'   enables all checks. Recognized values:
#'   \itemize{
#'     \item \code{"empty_restricted_grid"}
#'     \item \code{"no_feasible_grid_point"}
#'     \item \code{"mode_on_psi_boundary"}
#'     \item \code{"mode_locator_failed"}
#'     \item \code{"mode_eval_failed_after_snap"}
#'     \item \code{"mode_nonfinite"} — numerical-validity gate: mode
#'       log-likelihood is NaN/Inf.
#'     \item \code{"mode_infeasible"} — numerical-validity gate: mode solve
#'       violated the equality constraint (\code{|psi_residual| > resid_tol});
#'       the principled replacement for \code{"mode_on_psi_boundary"}.
#'     \item \code{"mode_uncompetitive"} — relevance gate (distinct from the
#'       validity gates above): rejects a validly-located branch whose
#'       \code{ll_mode} sits more than \code{effective_crit} (the same
#'       \code{cutoff_buffer}-scaled threshold \code{branch_extent =
#'       "global"} uses) below the best \code{ll_mode} seen so far during
#'       \code{sieve()}. Unlike the other gates, a rejection here still
#'       counts toward the branch's contribution to \code{R} in
#'       \code{aggregate()} — the branch was validly measured, just known to
#'       be numerically negligible, which is different from a validity
#'       failure where nothing was measured at all. See \code{sieve()}.
#'     \item \code{"mode_too_low"} — legacy fixed-threshold relevance gate
#'       (profile-relative, not adaptive to the accumulating ensemble).
#'       Prefer \code{"mode_uncompetitive"}; retained for backward
#'       compatibility.
#'     \item \code{"oscillation"}
#'     \item \code{"mode_shift_exhausted"}
#'     \item \code{"jump_left"}
#'     \item \code{"jump_right"}
#'   }
#' @param profile_selection Character scalar controlling how the profile
#'   traversal treats a monotonicity-violating candidate (\code{branch_val}
#'   higher than the current chain value). Unlike a branch, the profile's
#'   recorded value IS the objective being directly optimized at each ψ, so
#'   its true value is provably non-increasing away from the mode — a
#'   violation is trustworthy evidence that the current warm-start chain is
#'   stuck on a suboptimal constrained optimum, not just numerical noise.
#'   \code{"envelope"} (default) is the historical behavior: record the
#'   violating value (it IS the higher of the two, so still the best
#'   available estimate at that ψ) but never advance the warm-start chain
#'   from it, so the next point reverts to the pre-violation trajectory —
#'   this can leave an isolated "hitch": one point pops up, the next snaps
#'   back down. \code{"adopt"} additionally allows the chain to permanently
#'   switch onto the better branch when the improvement is large relative
#'   to recent local step sizes (\code{> adopt_mult * median} of the last
#'   \code{k_recent} declining steps) — small, ambiguous improvements
#'   (indistinguishable from solver noise) still fall back to the
#'   \code{"envelope"} behavior and are not adopted. Validated
#'   2026-08-03: a naive "always adopt" policy over-corrected on small
#'   violations and could land on a worse branch further out; the
#'   magnitude threshold is required.
#' @param adopt_mult Positive numeric scalar. Only used when
#'   \code{profile_selection = "adopt"}. An improvement is adopted only if
#'   it exceeds \code{adopt_mult} times the median of the last
#'   \code{k_recent} accepted declining step sizes. Default: \code{1.2}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{traversal_spec} object.
#' @export
traversal_spec <- function(
  increment,
  traversal_method = "topdown",
  warmstart_fn = NULL,
  mode_locator_fn = NULL,
  confidence_levels = c(0.90, 0.95, 0.99),
  cutoff_buffer = 1.5,
  n_adjacent = 3L,
  max_mode_shifts = 20L,
  k_recent = 3L,
  drop_multiplier = 2.0,
  cap_multiplier = 10.0,
  mode_gap_multiplier = 1.0,
  interval_buffer = 1.0,
  max_drop_frac = 10.0,
  resid_tol = 1e-3,
  profile_retry_on = c("monotonicity", "constraint", "drop"),
  branch_retry_on = character(0),
  max_retries = NULL,
  branch_selection = "envelope",
  branch_extent = "per_branch",
  profile_selection = "envelope",
  adopt_mult = 1.2,
  use_mode_locator_for_profile = FALSE,
  rejection_reasons = NULL,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<traversal>",
    increment = increment,
    traversal_method = traversal_method,
    warmstart_fn = warmstart_fn,
    mode_locator_fn = mode_locator_fn,
    confidence_levels = confidence_levels,
    cutoff_buffer = cutoff_buffer,
    n_adjacent = n_adjacent,
    max_mode_shifts = max_mode_shifts,
    k_recent = k_recent,
    drop_multiplier = drop_multiplier,
    cap_multiplier = cap_multiplier,
    mode_gap_multiplier = mode_gap_multiplier,
    interval_buffer = interval_buffer,
    max_drop_frac = max_drop_frac,
    resid_tol = resid_tol,
    profile_retry_on = profile_retry_on,
    branch_retry_on = branch_retry_on,
    max_retries = max_retries,
    profile_selection = profile_selection,
    adopt_mult = adopt_mult,
    branch_selection = branch_selection,
    branch_extent = branch_extent,
    use_mode_locator_for_profile = use_mode_locator_for_profile,
    rejection_reasons = rejection_reasons,
    max_drop_cap = NULL, # populated by preprocess()
    extra = list(...)
  )

  x <- new_traversal_spec(x)
  .validate_traversal_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_traversal_spec <- function(x) .new_spec(x, "traversal_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_traversal_spec <- function(x) {
  # increment ---------------------------------------------------------
  if (
    !is.numeric(x$increment) || length(x$increment) != 1L || x$increment <= 0
  ) {
    stop("increment must be a positive numeric scalar.", call. = FALSE)
  }

  # traversal method --------------------------------------------------
  x$traversal_method <- match.arg(x$traversal_method, c("topdown", "leftright"))

  # warmstart_fn ------------------------------------------------------
  if (!is.null(x$warmstart_fn) && !is.function(x$warmstart_fn)) {
    stop(
      "warmstart_fn must be NULL or a function(psi_curr, psi_next, param_curr).",
      call. = FALSE
    )
  }

  # mode_locator_fn ---------------------------------------------------
  if (!is.null(x$mode_locator_fn) && !is.function(x$mode_locator_fn)) {
    stop("mode_locator_fn must be NULL or a function.", call. = FALSE)
  }
  if (x$traversal_method == "leftright" && !is.null(x$mode_locator_fn)) {
    cat(
      "traversal_spec: mode_locator_fn is ignored when ",
      "traversal_method = \"leftright\"."
    )
  }

  # confidence levels -------------------------------------------------
  cl <- x$confidence_levels
  if (!is.numeric(cl) || any(cl <= 0 | cl >= 1)) {
    stop(
      "confidence_levels must be numeric values strictly between 0 and 1.",
      call. = FALSE
    )
  }
  if (anyDuplicated(cl)) {
    stop("confidence_levels must not contain duplicates.", call. = FALSE)
  }

  # cutoff_buffer -----------------------------------------------------
  if (
    !is.numeric(x$cutoff_buffer) ||
      length(x$cutoff_buffer) != 1L ||
      x$cutoff_buffer <= 0
  ) {
    stop("cutoff_buffer must be a positive numeric scalar.", call. = FALSE)
  }

  # n_adjacent --------------------------------------------------------
  if (
    !is.numeric(x$n_adjacent) || length(x$n_adjacent) != 1L || x$n_adjacent < 0
  ) {
    stop("n_adjacent must be a non-negative integer scalar.", call. = FALSE)
  }
  x$n_adjacent <- as.integer(x$n_adjacent)

  # max_mode_shifts ---------------------------------------------------
  if (
    !is.numeric(x$max_mode_shifts) ||
      length(x$max_mode_shifts) != 1L ||
      x$max_mode_shifts < 0
  ) {
    stop(
      "max_mode_shifts must be a non-negative integer scalar.",
      call. = FALSE
    )
  }
  x$max_mode_shifts <- as.integer(x$max_mode_shifts)

  # k_recent ----------------------------------------------------------
  if (!is.numeric(x$k_recent) || length(x$k_recent) != 1L || x$k_recent < 0) {
    stop("k_recent must be a non-negative integer scalar.", call. = FALSE)
  }
  x$k_recent <- as.integer(x$k_recent)

  # drop_multiplier ---------------------------------------------------
  if (
    !is.numeric(x$drop_multiplier) ||
      length(x$drop_multiplier) != 1L ||
      x$drop_multiplier <= 0
  ) {
    stop("drop_multiplier must be a positive numeric scalar.", call. = FALSE)
  }

  # cap_multiplier ----------------------------------------------------
  if (
    !is.numeric(x$cap_multiplier) ||
      length(x$cap_multiplier) != 1L ||
      x$cap_multiplier <= 0
  ) {
    stop("cap_multiplier must be a positive numeric scalar.", call. = FALSE)
  }

  # mode_gap_multiplier -----------------------------------------------
  if (
    !is.numeric(x$mode_gap_multiplier) ||
      length(x$mode_gap_multiplier) != 1L ||
      x$mode_gap_multiplier <= 0
  ) {
    stop(
      "mode_gap_multiplier must be a positive numeric scalar.",
      call. = FALSE
    )
  }

  # interval_buffer ---------------------------------------------------
  if (
    !is.numeric(x$interval_buffer) ||
      length(x$interval_buffer) != 1L ||
      x$interval_buffer <= 0
  ) {
    stop("interval_buffer must be a positive numeric scalar.", call. = FALSE)
  }

  # max_drop_frac -----------------------------------------------------
  if (
    !is.numeric(x$max_drop_frac) ||
      length(x$max_drop_frac) != 1L ||
      x$max_drop_frac <= 0
  ) {
    stop(
      "max_drop_frac must be a positive numeric scalar (use Inf to disable).",
      call. = FALSE
    )
  }

  # resid_tol ---------------------------------------------------------
  if (
    !is.numeric(x$resid_tol) || length(x$resid_tol) != 1L || x$resid_tol < 0
  ) {
    stop("resid_tol must be a non-negative numeric scalar.", call. = FALSE)
  }

  # profile_retry_on --------------------------------------------------
  .valid_retry <- c("monotonicity", "constraint", "drop")
  if (!is.character(x$profile_retry_on) || length(x$profile_retry_on) == 0L) {
    stop(
      "profile_retry_on must be a non-empty character vector.",
      call. = FALSE
    )
  }
  unknown_retry <- setdiff(x$profile_retry_on, .valid_retry)
  if (length(unknown_retry) > 0L) {
    stop(
      "Unknown profile_retry_on values: ",
      paste(unknown_retry, collapse = ", "),
      ".\nValid values: ",
      paste(.valid_retry, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # branch_retry_on ---------------------------------------------------
  if (!is.character(x$branch_retry_on)) {
    stop(
      "branch_retry_on must be a character vector (use character(0) for none).",
      call. = FALSE
    )
  }

  # branch_selection --------------------------------------------------
  x$branch_selection <- match.arg(
    x$branch_selection,
    c("envelope", "continuation")
  )

  # branch_extent -----------------------------------------------------
  x$branch_extent <- match.arg(
    x$branch_extent,
    c("per_branch", "global")
  )

  # profile_selection ---------------------------------------------------
  x$profile_selection <- match.arg(
    x$profile_selection,
    c("envelope", "adopt")
  )

  # adopt_mult ----------------------------------------------------------
  if (
    !is.numeric(x$adopt_mult) || length(x$adopt_mult) != 1L || x$adopt_mult <= 0
  ) {
    stop("adopt_mult must be a positive numeric scalar.", call. = FALSE)
  }

  # max_retries -------------------------------------------------------
  if (!is.null(x$max_retries)) {
    if (
      !is.numeric(x$max_retries) ||
        length(x$max_retries) != 1L ||
        !is.finite(x$max_retries) ||
        x$max_retries < 0 ||
        x$max_retries != as.integer(x$max_retries)
    ) {
      stop(
        "max_retries must be NULL or a non-negative integer scalar.",
        call. = FALSE
      )
    }
    x$max_retries <- as.integer(x$max_retries)
  }

  # use_mode_locator_for_profile --------------------------------------
  if (
    !is.logical(x$use_mode_locator_for_profile) ||
      length(x$use_mode_locator_for_profile) != 1L
  ) {
    stop(
      "use_mode_locator_for_profile must be a single logical value.",
      call. = FALSE
    )
  }

  # rejection_reasons -------------------------------------------------
  .valid_reasons <- c(
    "empty_restricted_grid",
    "no_feasible_grid_point",
    "mode_on_psi_boundary",
    "mode_locator_failed",
    "mode_eval_failed_after_snap",
    "mode_nonfinite",
    "mode_infeasible",
    "mode_uncompetitive",
    "mode_too_low",
    "oscillation",
    "mode_shift_exhausted",
    "jump_left",
    "jump_right"
  )
  if (!is.null(x$rejection_reasons)) {
    if (!is.character(x$rejection_reasons)) {
      stop(
        "rejection_reasons must be NULL or a character vector.",
        call. = FALSE
      )
    }
    unknown <- setdiff(x$rejection_reasons, .valid_reasons)
    if (length(unknown) > 0L) {
      stop(
        "Unknown rejection_reasons: ",
        paste(unknown, collapse = ", "),
        ".\nValid values: ",
        paste(.valid_reasons, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.traversal_spec <- function(x, ...) {
  cat("# Traversal Specification\n")
  cat("- Name:                          ", x$name, "\n", sep = "")
  cat("- Increment:                     ", x$increment, "\n", sep = "")
  cat("- Traversal method:              ", x$traversal_method, "\n", sep = "")
  cat(
    "- Warm start:                    ",
    if (!is.null(x$warmstart_fn)) {
      "custom (warmstart_fn supplied)"
    } else {
      "previous param_hat (default)"
    },
    "\n",
    sep = ""
  )
  cat(
    "- Mode locator:                  ",
    if (!is.null(x$mode_locator_fn)) {
      "custom (mode_locator_fn supplied)"
    } else {
      "built-in default"
    },
    "\n",
    sep = ""
  )
  cat(
    "- Use mode locator for profile:  ",
    if (isTRUE(x$use_mode_locator_for_profile)) "TRUE" else "FALSE",
    "\n",
    sep = ""
  )
  cat(
    "- CI levels:                     ",
    paste(format(x$confidence_levels), collapse = ", "),
    "\n",
    sep = ""
  )
  cat("- Cutoff buffer:                 ", x$cutoff_buffer, "\n", sep = "")
  cat("- n_adjacent:                    ", x$n_adjacent, "\n", sep = "")
  cat("- max_mode_shifts:               ", x$max_mode_shifts, "\n", sep = "")
  cat("- k_recent:                      ", x$k_recent, "\n", sep = "")
  cat("- drop_multiplier:               ", x$drop_multiplier, "\n", sep = "")
  cat("- cap_multiplier:                ", x$cap_multiplier, "\n", sep = "")
  cat(
    "- mode_gap_multiplier:           ",
    x$mode_gap_multiplier,
    "\n",
    sep = ""
  )
  cat("- interval_buffer:               ", x$interval_buffer, "\n", sep = "")
  cat("- max_drop_frac:                 ", x$max_drop_frac, "\n", sep = "")
  cat("- resid_tol:                     ", x$resid_tol, "\n", sep = "")
  cat(
    "- profile_retry_on:              ",
    paste(x$profile_retry_on, collapse = ", "),
    "\n",
    sep = ""
  )
  cat(
    "- branch_retry_on:               ",
    if (length(x$branch_retry_on)) {
      paste(x$branch_retry_on, collapse = ", ")
    } else {
      "none"
    },
    "\n",
    sep = ""
  )
  cat(
    "- max_retries:                   ",
    x$max_retries %||% "NULL  (inherits solver$max_retries)",
    "\n",
    sep = ""
  )
  cat(
    "- branch_selection:              ",
    x$branch_selection %||% "envelope",
    "\n",
    sep = ""
  )
  cat(
    "- branch_extent:                 ",
    x$branch_extent %||% "per_branch",
    "\n",
    sep = ""
  )
  cat(
    "- profile_selection:             ",
    x$profile_selection %||% "envelope",
    "\n",
    sep = ""
  )
  cat("- adopt_mult:                    ", x$adopt_mult %||% 1.2, "\n", sep = "")
  if (!is.null(x$rejection_reasons)) {
    cat(
      "- rejection_reasons:             ",
      paste(x$rejection_reasons, collapse = ", "),
      "\n",
      sep = ""
    )
  } else {
    cat("- rejection_reasons:              NULL  (all checks active)\n")
  }
  if (!is.null(x$max_drop_cap)) {
    cat(
      "- max_drop_cap:                  ",
      round(x$max_drop_cap, 6),
      "  (set by preprocess())\n",
      sep = ""
    )
  } else {
    cat("- max_drop_cap:                   NULL  (set by preprocess())\n")
  }
  invisible(x)
}
