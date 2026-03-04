# ======================================================================
# calibrate-pipeline.R — Pipeline Calibration (v1.0)
#
# Consolidates responsibilities previously spread across:
#   • calibrate-estimand.R  — search interval computation
#   • calibrate-nuisance.R  — omega-hat closure construction
#   • calibrate-optimizer.R — mode locator resolution
#
# After calibration, pipeline holds:
#   • $search_interval       — clipped to ψ bounds
#   • $omega_hat_initgen     — closure()     → x0 candidate
#   • $omega_hat_sampler     — closure(x0)   → omega_hat
#   • $omega_hat_permuter    — closure(omega_hat) → list of omega_hats
#   • $branch_mode_locator   — closure(omega_hat, n_adjacent) → mode
# ======================================================================

#' Calibrate Pipeline Component
#'
#' @description
#' Computes the ψ search interval, builds omega-hat generation closures,
#' and resolves the branch mode locator. All outputs are stored on the
#' pipeline spec and ready for use in \code{generate()} without further
#' setup.
#'
#' @param pipeline  A \code{pipeline_spec} object.
#' @param parameter Calibrated \code{parameter_spec}.
#' @param likelihood Calibrated \code{likelihood_spec}.
#' @param estimand  Calibrated \code{estimand_spec}.
#' @param solver    A \code{solver_spec} object.
#' @param data      User data.
#'
#' @return The SAME \code{pipeline_spec} object, augmented with:
#'   \itemize{
#'     \item \code{$search_interval}
#'     \item \code{$omega_hat_initgen}
#'     \item \code{$omega_hat_sampler}
#'     \item \code{$omega_hat_permuter}
#'     \item \code{$branch_mode_locator}
#'   }
#'
#' @keywords internal
calibrate_pipeline <- function(
  pipeline,
  parameter,
  likelihood,
  estimand,
  solver,
  data
) {
  stopifnot(
    inherits(pipeline, "pipeline_spec"),
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec")
  )

  # -------------------------------------------------------------------
  # 1. Compute and clip search interval
  # -------------------------------------------------------------------
  si <- pipeline$search_interval_fn(parameter$param_mle, data)

  if (
    !is.numeric(si) || length(si) != 2L || any(!is.finite(si)) || si[1] >= si[2]
  ) {
    stop(
      "search_interval_fn(param_mle, data) must return c(lower, upper) ",
      "with finite lower < upper.",
      call. = FALSE
    )
  }

  if (!is.null(estimand$psi_lower)) {
    si[1] <- max(si[1], estimand$psi_lower)
  }
  if (!is.null(estimand$psi_upper)) {
    si[2] <- min(si[2], estimand$psi_upper)
  }

  if (si[1] >= si[2]) {
    stop("Search interval collapses after applying ψ bounds.", call. = FALSE)
  }

  pipeline$search_interval <- si

  # -------------------------------------------------------------------
  # 2. Build omega-hat closures
  # -------------------------------------------------------------------

  # Shared pool of calibrated quantities available to all constructors.
  base_args <- list(
    param_mle = parameter$param_mle,
    param_dim = parameter$param_dim,
    param_lower = parameter$param_lower,
    param_upper = parameter$param_upper,
    psi_fn = estimand$psi_fn,
    psi_jac = estimand$psi_jac,
    psi_mle = estimand$psi_mle,
    psi_lower = estimand$psi_lower,
    psi_upper = estimand$psi_upper,
    eq_fn = parameter$eq,
    eq_jac = parameter$eq_jac,
    ineq_fn = parameter$ineq,
    ineq_jac = parameter$ineq_jac,
    solver = solver
  )

  # Helper: call a constructor with a merged args list, filtering to
  # declared formals unless the constructor accepts `...`.
  .call_constructor <- function(fn, args) {
    fmls <- names(formals(fn))
    if ("..." %in% fmls) {
      do.call(fn, args)
    } else {
      do.call(fn, args[names(args) %in% fmls])
    }
  }

  omega_hat_args <- pipeline$omega_hat_args %||% list()

  # ---- initgen -------------------------------------------------------
  initgen_constructor <- .resolve_initgen(
    pipeline$omega_hat_method,
    omega_hat_args
  )
  initgen_args <- c(base_args, omega_hat_args$initgen %||% list())
  pipeline$omega_hat_initgen <- .call_constructor(
    initgen_constructor,
    initgen_args
  )

  # ---- sampler -------------------------------------------------------
  # Sampler is always the standard feasibility-projection sampler;
  # omega_hat_method only affects initgen.
  if (!is.null(omega_hat_args$sampler_fn)) {
    sampler_constructor <- omega_hat_args$sampler_fn
  } else {
    sampler_constructor <- omega_hat_sampler # package built-in
  }
  sampler_args <- c(base_args, omega_hat_args$sampler %||% list())
  pipeline$omega_hat_sampler <- .call_constructor(
    sampler_constructor,
    sampler_args
  )

  # ---- permuter ------------------------------------------------------
  if (
    !is.null(omega_hat_args$permuter_fn) || !is.null(omega_hat_args$permuter)
  ) {
    permuter_constructor <- omega_hat_args$permuter_fn %||% omega_hat_permuter
    permuter_args <- c(base_args, omega_hat_args$permuter %||% list())
    pipeline$omega_hat_permuter <- .call_constructor(
      permuter_constructor,
      permuter_args
    )
  } else {
    pipeline$omega_hat_permuter <- NULL
  }

  # -------------------------------------------------------------------
  # 3. Resolve branch mode locator
  # -------------------------------------------------------------------
  locator_factory <- get_branch_mode_locator(pipeline$mode_locator_method)
  locator_args <- pipeline$mode_locator_args %||% list()

  factory_formals_all <- names(formals(locator_factory))
  factory_formals <- setdiff(factory_formals_all, "...")
  required_formals <- Filter(
    function(nm) identical(formals(locator_factory)[[nm]], quote(expr = )),
    factory_formals
  )

  # Build branch function factory — produces branch_fn(psi, param_init)
  # for a given omega_hat. NOTE: build_branch_fn_factory() will need its
  # signature updated once branch-factory.R is aligned to the new specs
  # (E_loglik now lives on likelihood, not nuisance).
  branch_fn_factory <- build_branch_fn_factory(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    solver = solver,
    pipeline = pipeline
  )

  search_interval <- pipeline$search_interval

  # n_adjacent is injected from the caller (generate() / screen()) at
  # runtime rather than from YAML, so the locator snap window always
  # matches the geometry grid used by the scorer.
  pipeline$branch_mode_locator <- function(omega_hat, n_adjacent = 3L) {
    branch_fn <- branch_fn_factory(omega_hat)

    arg_pool <- c(
      list(
        branch_fn = branch_fn,
        psi_init = estimand$psi_mle,
        search_interval = search_interval,
        param_init = omega_hat,
        psi_mle = estimand$psi_mle,
        increment = pipeline$increment
      ),
      locator_args,
      list(n_adjacent = n_adjacent) # always overrides any YAML value
    )

    missing_args <- setdiff(required_formals, names(arg_pool))
    if (length(missing_args) > 0) {
      stop(
        "branch_mode_locator '",
        pipeline$mode_locator_method,
        "' missing required arguments: ",
        paste(missing_args, collapse = ", "),
        call. = FALSE
      )
    }

    call_args <- if ("..." %in% factory_formals_all) {
      arg_pool
    } else {
      arg_pool[intersect(factory_formals, names(arg_pool))]
    }

    locator <- do.call(locator_factory, call_args)

    if (!is.function(locator)) {
      stop(
        "branch_mode_locator_factory '",
        pipeline$mode_locator_method,
        "' did not return a function.",
        call. = FALSE
      )
    }

    locator(omega_hat)
  }

  pipeline
}

# ======================================================================
# INTERNAL: Resolve initgen constructor from method string
# ======================================================================

#' @keywords internal
#' @noRd
.resolve_initgen <- function(method, omega_hat_args) {
  switch(
    method,
    gaussian = omega_hat_initgen_gaussian,
    shape_family = omega_hat_initgen_shape,
    custom = {
      fn <- omega_hat_args$initgen_fn
      if (!is.function(fn)) {
        stop(
          'omega_hat_method = "custom" requires omega_hat_args$initgen_fn ',
          "to be a function.",
          call. = FALSE
        )
      }
      fn
    },
    stop("Unknown omega_hat_method: '", method, "'.", call. = FALSE)
  )
}
