# ======================================================================
# spec-sampler.R — Sampler Specification (v2.1)
#
# Governs how omega-hat candidates are generated during screening.
#
# Two optional slots:
#   sampler_fn        — constructor returning function(history) -> omega_hat
#   orbit_expander_fn — constructor returning function(omega_hat) -> list
#
# When sampler_fn is NULL, calibrate_sampler() falls back to the
# built-in gaussian initgen + feasibility projection machinery.
# When orbit_expander_fn is NULL, no orbit expansion is performed.
#
# min_branches, branch_buffer, and total_seeds live here because they
# govern how many omega-hat candidates are requested — a sampling
# concern, not an execution concern. total_seeds is computed and stored
# during calibrate_sampler().
# ======================================================================

#' Specify the Omega-Hat Sampling Strategy
#'
#' @description
#' Defines how candidate nuisance parameter vectors (omega-hats) are
#' generated during screening. The spec is intentionally minimal —
#' all sampling logic lives in the constructor functions supplied by
#' the user, not in the spec itself.
#'
#' @section Sampler constructor:
#' \code{sampler_fn} must be a constructor — a function that accepts
#' named arguments from the calibrated model (any subset of
#' \code{param_mle}, \code{param_dim}, \code{psi_fn}, \code{psi_jac},
#' \code{psi_mle}, \code{param_lower}, \code{param_upper},
#' \code{eq_fn}, \code{eq_jac}, \code{ineq_fn}, \code{ineq_jac},
#' \code{solver}) and returns a closure
#' \code{function(history = NULL) -> numeric vector}.
#'
#' The \code{history} argument receives a list of previously accepted
#' omega-hats, which the sampler may use for adaptive exploration.
#' It should be ignored if not needed.
#'
#' When \code{sampler_fn} is \code{NULL}, the built-in gaussian
#' initgen + feasibility projection sampler is used.
#'
#' @section Orbit expander constructor:
#' \code{orbit_expander_fn} must be a constructor accepting the same
#' named arguments as \code{sampler_fn} and returning a closure
#' \code{function(omega_hat) -> list of numeric vectors}.
#'
#' Each orbit consists of a base draw plus all of its permuted variants.
#' \code{orbit_sample_size} controls how many candidates — including the
#' base draw — are randomly selected from the full orbit and screened
#' per base draw. \code{NULL} means the full orbit is screened.
#' Candidates within each orbit are screened in a random order.
#' Once \code{total_seeds} candidates have been accepted across all
#' orbits, screening stops immediately even if the current orbit has
#' not been fully processed.
#'
#' @param sampler_fn Optional constructor function. When supplied,
#'   replaces the built-in sampling machinery entirely.
#' @param orbit_expander_fn Optional constructor function for orbit
#'   expansion. When supplied, each base draw is expanded to its full
#'   orbit and \code{orbit_sample_size} candidates are sampled and
#'   screened.
#' @param orbit_sample_size Optional positive integer. Number of
#'   candidates to sample and screen per orbit, including the base
#'   draw. \code{NULL} means screen the full orbit.
#' @param min_branches Positive integer. Minimum number of branches
#'   to retain after aggregation filtering.
#' @param branch_buffer Non-negative integer. Number of additional seeds
#'   to evaluate beyond \code{min_branches}. Total seeds evaluated =
#'   \code{min_branches + branch_buffer}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{sampler_spec} object.
#' @export
sampler_spec <- function(
  sampler_fn = NULL,
  orbit_expander_fn = NULL,
  orbit_sample_size = NULL,
  min_branches,
  branch_buffer = 0L,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<sampler>",
    sampler_fn = sampler_fn,
    orbit_expander_fn = orbit_expander_fn,
    orbit_sample_size = orbit_sample_size,
    min_branches = min_branches,
    branch_buffer = branch_buffer,
    extra = list(...)
  )

  x <- new_sampler_spec(x)
  .validate_sampler_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_sampler_spec <- function(x) .new_spec(x, "sampler_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_sampler_spec <- function(x) {
  if (!is.null(x$sampler_fn) && !is.function(x$sampler_fn)) {
    stop("sampler_fn must be a function.", call. = FALSE)
  }

  if (!is.null(x$orbit_expander_fn) && !is.function(x$orbit_expander_fn)) {
    stop("orbit_expander_fn must be a function.", call. = FALSE)
  }

  if (!is.null(x$orbit_sample_size)) {
    if (
      !is.numeric(x$orbit_sample_size) ||
        length(x$orbit_sample_size) != 1L ||
        x$orbit_sample_size < 1L
    ) {
      stop(
        "orbit_sample_size must be a positive integer scalar.",
        call. = FALSE
      )
    }
    x$orbit_sample_size <- as.integer(x$orbit_sample_size)
  }

  if (
    !is.numeric(x$min_branches) ||
      length(x$min_branches) != 1L ||
      !is.finite(x$min_branches) ||
      x$min_branches < 1 ||
      x$min_branches != as.integer(x$min_branches)
  ) {
    stop(
      "sampler_spec(): min_branches must be a positive integer.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(x$branch_buffer) ||
      length(x$branch_buffer) != 1L ||
      !is.finite(x$branch_buffer) ||
      x$branch_buffer < 0 ||
      x$branch_buffer != as.integer(x$branch_buffer)
  ) {
    stop(
      "sampler_spec(): branch_buffer must be a non-negative integer.",
      call. = FALSE
    )
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.sampler_spec <- function(x, ...) {
  cat("# Sampler Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  cat(
    "- Sampler:              ",
    if (!is.null(x$sampler_fn)) {
      "custom (sampler_fn supplied)"
    } else {
      "built-in default"
    },
    "\n",
    sep = ""
  )
  if (!is.null(x$orbit_expander_fn)) {
    cat(
      "- Orbit expansion:      yes (orbit_sample_size = ",
      x$orbit_sample_size %||% "unlimited",
      ")\n",
      sep = ""
    )
  } else {
    cat("- Orbit expansion:      none\n")
  }
  cat("- Min branches:        ", x$min_branches, "\n", sep = "")
  cat("- Branch buffer:       ", x$branch_buffer, "\n", sep = "")
  if (!is.null(x$total_seeds)) {
    cat("- Total seeds:         ", x$total_seeds, "\n", sep = "")
  }
  invisible(x)
}
