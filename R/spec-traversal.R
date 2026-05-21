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
#'     \item \code{"mode_too_low"}
#'     \item \code{"oscillation"}
#'     \item \code{"mode_shift_exhausted"}
#'     \item \code{"jump_left"}
#'     \item \code{"jump_right"}
#'   }
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
