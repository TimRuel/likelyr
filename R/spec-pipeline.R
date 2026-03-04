# ======================================================================
# Pipeline Specification (v1.0)
#
# Governs *how* the integrated likelihood is computed, independently
# of the model structure (parameter, likelihood, estimand) and the
# inner solver (solver_spec) and resource allocation (execution_spec).
#
# Responsibilities:
#   • ψ grid: search interval and spacing
#   • Omega-hat generation strategy
#   • Branch mode location method
#   • Branch walk strategy
#   • Branch scoring / quality control
#   • Inference tuning (confidence levels, cutoffs)
# ======================================================================

#' Specify the Integrated Likelihood Computation Pipeline
#'
#' @description
#' Defines all algorithmic choices that govern how the integrated
#' log-likelihood is computed, from omega-hat generation through to
#' confidence interval construction.
#'
#' This spec is intentionally separate from:
#' \itemize{
#'   \item \code{parameter_spec}, \code{likelihood_spec},
#'     \code{estimand_spec} — which describe the model
#'   \item \code{solver_spec} — which controls the inner auglag solver
#'   \item \code{execution_spec} — which controls parallelism and R
#' }
#'
#' @section ψ grid:
#' \code{search_interval_fn} defines the range of ψ values to explore.
#' \code{increment} controls the spacing of the ψ grid. Together they
#' determine how finely and how widely branches are evaluated.
#'
#' @section Omega-hat strategy:
#' Controls how candidate nuisance parameter vectors are generated.
#' Built-in methods:
#' \itemize{
#'   \item \code{"gaussian"} — Gaussian perturbation of param_mle in
#'     the tangent space of the constraint surface (legacy default)
#'   \item \code{"shape_family"} — k-dominant canonical probability
#'     vectors that analytically satisfy ψ(x0) = ψ_mle; provides
#'     structural diversity beyond permutation orbits
#'   \item \code{"custom"} — user supplies
#'     \code{function(cal, n) -> list of omega_hats} via
#'     \code{omega_hat_args$fn}
#' }
#'
#' @section Branch mode locator:
#' Controls how the branch mode ψ̂ is located for each omega-hat.
#' Built-in methods: \code{"hybrid"}, \code{"brent"},
#' \code{"grid_scan"}, \code{"multiplier_root"}.
#'
#' @section Branch walker:
#' Controls how branch values are evaluated across the ψ grid,
#' starting from the located mode. Built-in methods:
#' \itemize{
#'   \item \code{"outward"} — evaluates outward from the mode with
#'     warm-start chaining; stops when the branch drops below cutoff
#'   \item \code{"custom"} — user supplies a walker function via
#'     \code{walker_args$fn}
#' }
#'
#' @section Branch scorer:
#' Assigns a numeric quality score in [0, 1] to each completed branch.
#' Branches with score below \code{min_score} are excluded from
#' aggregation. Setting \code{min_score = 0} includes all branches.
#' Built-in methods:
#' \itemize{
#'   \item \code{"monotone"} — checks that branch values decrease
#'     monotonically away from the mode on both sides
#'   \item \code{"curvature"} — checks that second differences are
#'     negative (concave down) near the mode
#'   \item \code{"none"} — all branches score 1; no filtering
#'   \item \code{"custom"} — user supplies
#'     \code{function(branch) -> numeric scalar in [0,1]} via
#'     \code{scorer_args$fn}
#' }
#'
#' @section Inference tuning:
#' \code{confidence_levels} controls which CIs are computed.
#' \code{gamma}, \code{cutoff_buffer}, and \code{uniroot_expand_factor}
#' tune branch filtering and CI root-finding behaviour.
#'
#' @param search_interval_fn Required. Function(param_mle, data) →
#'   numeric vector of length 2 giving the ψ search interval
#'   \code{c(lower, upper)}.
#' @param increment Required. Positive numeric scalar giving the ψ-grid
#'   spacing Δψ.
#' @param omega_hat_method Character scalar selecting the omega-hat
#'   generation strategy. One of \code{"gaussian"}, \code{"shape_family"},
#'   \code{"custom"}. Default: \code{"gaussian"}.
#' @param omega_hat_args Named list of method-specific arguments passed
#'   to the omega-hat generator. Default: \code{list()}.
#' @param mode_locator_method Character scalar selecting the branch mode
#'   locator. One of \code{"hybrid"}, \code{"brent"},
#'   \code{"grid_scan"}, \code{"multiplier_root"}.
#'   Default: \code{"hybrid"}.
#' @param mode_locator_args Named list of method-specific arguments
#'   passed to the mode locator factory. Default: \code{list()}.
#' @param walker_method Character scalar selecting the branch walk
#'   strategy. One of \code{"outward"}, \code{"custom"}.
#'   Default: \code{"outward"}.
#' @param walker_args Named list of method-specific arguments passed to
#'   the walker. Default: \code{list()}.
#' @param scorer_method Character scalar selecting the branch scoring
#'   strategy. One of \code{"monotone"}, \code{"curvature"},
#'   \code{"none"}, \code{"custom"}. Default: \code{"monotone"}.
#' @param scorer_args Named list of method-specific arguments passed to
#'   the scorer. Default: \code{list()}.
#' @param min_score Numeric scalar in [0, 1]. Branches scoring below
#'   this threshold are excluded from aggregation. Default: \code{0.5}.
#' @param confidence_levels Numeric vector of confidence levels,
#'   each strictly in (0, 1). Default: \code{c(0.90, 0.95)}.
#' @param gamma Numeric scalar in (0, 1] tempering the conservativeness
#'   of branch cutoffs for integrated likelihood. Default: \code{0.5}.
#' @param cutoff_buffer Nonnegative numeric scalar added to the branch
#'   cutoff as a safety margin. Default: \code{0.1}.
#' @param uniroot_expand_factor Nonnegative numeric scalar controlling
#'   multiplicative expansion of CI root-finding search bounds.
#'   Default: \code{0.02}.
#' @param stop_at_bounds Logical. Stop branch walk when ψ reaches
#'   \code{psi_lower} or \code{psi_upper}. Default: \code{TRUE}.
#' @param eval_at_bounds Logical. Evaluate the branch at the boundary
#'   point before stopping. Default: \code{TRUE}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{pipeline_spec} object.
#' @export
pipeline_spec <- function(
  search_interval_fn,
  increment,
  omega_hat_method = "gaussian",
  omega_hat_args = list(),
  mode_locator_method = "hybrid",
  mode_locator_args = list(),
  walker_method = "outward",
  walker_args = list(),
  scorer_method = "monotone",
  scorer_args = list(),
  min_score = 0.5,
  confidence_levels = c(0.90, 0.95),
  gamma = 0.5,
  cutoff_buffer = 0.1,
  uniroot_expand_factor = 0.02,
  stop_at_bounds = TRUE,
  eval_at_bounds = TRUE,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<pipeline>",
    search_interval_fn = search_interval_fn,
    increment = increment,
    omega_hat_method = omega_hat_method,
    omega_hat_args = omega_hat_args,
    mode_locator_method = mode_locator_method,
    mode_locator_args = mode_locator_args,
    walker_method = walker_method,
    walker_args = walker_args,
    scorer_method = scorer_method,
    scorer_args = scorer_args,
    min_score = min_score,
    confidence_levels = confidence_levels,
    gamma = gamma,
    cutoff_buffer = cutoff_buffer,
    uniroot_expand_factor = uniroot_expand_factor,
    stop_at_bounds = stop_at_bounds,
    eval_at_bounds = eval_at_bounds,
    extra = list(...)
  )

  x <- new_pipeline_spec(x)
  .validate_pipeline_spec(x)
  x
}

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_pipeline_spec <- function(x) {
  # search interval ------------------------------------------------
  if (!is.function(x$search_interval_fn)) {
    stop(
      "search_interval_fn must be a function(param_mle, data) -> c(lower, upper).",
      call. = FALSE
    )
  }

  # increment ------------------------------------------------------
  if (
    !is.numeric(x$increment) || length(x$increment) != 1 || x$increment <= 0
  ) {
    stop("increment must be a positive numeric scalar.", call. = FALSE)
  }

  # omega-hat method -----------------------------------------------
  x$omega_hat_method <- match.arg(
    x$omega_hat_method,
    c("gaussian", "shape_family", "custom")
  )

  if (!is.list(x$omega_hat_args)) {
    stop("omega_hat_args must be a list.", call. = FALSE)
  }

  if (x$omega_hat_method == "custom" && !is.function(x$omega_hat_args$fn)) {
    stop(
      'omega_hat_method = "custom" requires omega_hat_args$fn to be a function.',
      call. = FALSE
    )
  }

  # mode locator ---------------------------------------------------
  x$mode_locator_method <- match.arg(
    x$mode_locator_method,
    c("hybrid", "brent", "grid_scan", "multiplier_root")
  )

  if (!is.list(x$mode_locator_args)) {
    stop("mode_locator_args must be a list.", call. = FALSE)
  }

  # walker ---------------------------------------------------------
  x$walker_method <- match.arg(
    x$walker_method,
    c("outward", "custom")
  )

  if (!is.list(x$walker_args)) {
    stop("walker_args must be a list.", call. = FALSE)
  }

  if (x$walker_method == "custom" && !is.function(x$walker_args$fn)) {
    stop(
      'walker_method = "custom" requires walker_args$fn to be a function.',
      call. = FALSE
    )
  }

  # scorer ---------------------------------------------------------
  x$scorer_method <- match.arg(
    x$scorer_method,
    c("monotone", "curvature", "none", "custom")
  )

  if (!is.list(x$scorer_args)) {
    stop("scorer_args must be a list.", call. = FALSE)
  }

  if (x$scorer_method == "custom" && !is.function(x$scorer_args$fn)) {
    stop(
      'scorer_method = "custom" requires scorer_args$fn to be a function.',
      call. = FALSE
    )
  }

  # min_score ------------------------------------------------------
  if (
    !is.numeric(x$min_score) ||
      length(x$min_score) != 1 ||
      x$min_score < 0 ||
      x$min_score > 1
  ) {
    stop("min_score must be a numeric scalar in [0, 1].", call. = FALSE)
  }

  # confidence levels ----------------------------------------------
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

  # gamma ----------------------------------------------------------
  if (
    !is.numeric(x$gamma) || length(x$gamma) != 1 || x$gamma <= 0 || x$gamma > 1
  ) {
    stop("gamma must be a numeric scalar in (0, 1].", call. = FALSE)
  }

  # cutoff_buffer --------------------------------------------------
  if (
    !is.numeric(x$cutoff_buffer) ||
      length(x$cutoff_buffer) != 1 ||
      x$cutoff_buffer < 0
  ) {
    stop("cutoff_buffer must be a non-negative numeric scalar.", call. = FALSE)
  }

  # uniroot_expand_factor ------------------------------------------
  if (
    !is.numeric(x$uniroot_expand_factor) ||
      length(x$uniroot_expand_factor) != 1 ||
      x$uniroot_expand_factor < 0
  ) {
    stop(
      "uniroot_expand_factor must be a non-negative numeric scalar.",
      call. = FALSE
    )
  }

  # stop/eval at bounds --------------------------------------------
  if (!is.logical(x$stop_at_bounds) || length(x$stop_at_bounds) != 1) {
    stop("stop_at_bounds must be a logical scalar.", call. = FALSE)
  }

  if (!is.logical(x$eval_at_bounds) || length(x$eval_at_bounds) != 1) {
    stop("eval_at_bounds must be a logical scalar.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.pipeline_spec <- function(x, ...) {
  cat("# Pipeline Specification\n")
  cat("- Name:                  ", x$name, "\n", sep = "")
  cat("- Increment:             ", x$increment, "\n", sep = "")
  cat("- Omega-hat method:      ", x$omega_hat_method, "\n", sep = "")
  cat("- Mode locator:          ", x$mode_locator_method, "\n", sep = "")
  cat("- Walker:                ", x$walker_method, "\n", sep = "")
  cat(
    "- Scorer:                ",
    x$scorer_method,
    "  (min_score = ",
    x$min_score,
    ")\n",
    sep = ""
  )
  cat(
    "- CI levels:             ",
    paste(format(x$confidence_levels), collapse = ", "),
    "\n",
    sep = ""
  )
  cat("- Gamma:                 ", x$gamma, "\n", sep = "")
  cat("- Cutoff buffer:         ", x$cutoff_buffer, "\n", sep = "")
  cat("- uniroot expand factor: ", x$uniroot_expand_factor, "\n", sep = "")
  cat(
    "- Bounds:                stop =",
    x$stop_at_bounds,
    " eval =",
    x$eval_at_bounds,
    "\n"
  )
  invisible(x)
}
