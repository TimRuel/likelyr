# ======================================================================
# spec-sampler.R — Sampler Specification (v2.2)
#
# Governs how omega-hat candidates are generated during screening.
#
# One optional slot:
#   sampler_fn — constructor returning function(history) -> list
#
# When sampler_fn is NULL, calibrate_sampler() falls back to the
# built-in gaussian initgen + feasibility projection machinery.
#
# min_branches, branch_buffer, and total_seeds live here because they
# govern how many omega-hat candidates are requested — a sampling
# concern, not an execution concern. total_seeds is computed and stored
# during calibrate_sampler().
#
# Any additional arguments passed via ... are stored in extra and
# forwarded to sampler_fn by calibrate_sampler() when building the
# draw closure.
# ======================================================================

#' Specify the Omega-Hat Sampling Strategy
#'
#' @description
#' Defines how candidate nuisance parameter vectors (omega-hats) are
#' generated during screening. The spec is intentionally minimal —
#' all sampling logic lives in the constructor function supplied by
#' the user, not in the spec itself.
#'
#' @section Sampler constructor:
#' \code{sampler_fn} must be a constructor — a function that accepts
#' named arguments from the calibrated model (any subset of
#' \code{param_mle}, \code{param_dim}, \code{psi_fn}, \code{psi_jac},
#' \code{psi_mle}, \code{param_lower}, \code{param_upper},
#' \code{eq_fn}, \code{eq_jac}, \code{ineq_fn}, \code{ineq_jac},
#' \code{solver}) and returns a closure
#' \code{function(history = NULL) -> list(candidate, diag)}.
#'
#' \code{$candidate} is a numeric vector (omega-hat in logit space).
#' \code{$diag} is a named list of draw-level metadata that
#' \code{sieve()} merges into its diagnostics log alongside the
#' accept/reject outcome. Samplers with nothing to report should
#' return \code{diag = list()}.
#'
#' The \code{history} argument receives a list of previously accepted
#' omega-hats, which the sampler may use for adaptive exploration.
#' It should be ignored if not needed.
#'
#' When \code{sampler_fn} is \code{NULL}, the built-in gaussian
#' initgen + feasibility projection sampler is used.
#'
#' @param sampler_fn Optional constructor function. When supplied,
#'   replaces the built-in sampling machinery entirely.
#' @param min_branches Positive integer. Minimum number of branches
#'   to retain after aggregation filtering.
#' @param branch_buffer Non-negative integer. Number of additional seeds
#'   to evaluate beyond \code{min_branches}. Total seeds evaluated =
#'   \code{min_branches + branch_buffer}.
#' @param name Optional descriptive name.
#' @param ... Additional arguments forwarded to \code{sampler_fn} by
#'   \code{calibrate_sampler()} when building the draw closure.
#'
#' @return A \code{sampler_spec} object.
#' @export
sampler_spec <- function(
  sampler_fn = NULL,
  min_branches,
  branch_buffer = 0L,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<sampler>",
    sampler_fn = sampler_fn,
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
  cat("- Min branches:        ", x$min_branches, "\n", sep = "")
  cat("- Branch buffer:       ", x$branch_buffer, "\n", sep = "")
  if (!is.null(x$total_seeds)) {
    cat("- Total seeds:         ", x$total_seeds, "\n", sep = "")
  }
  invisible(x)
}
