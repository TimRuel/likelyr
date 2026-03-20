# ======================================================================
# calibrate-sampler.R — Sampler Calibration (v2.0)
#
# Resolves a sampler_spec into closed-over functions ready for use
# by screen() and inspect_seeds(). After calibration, cal$sampler holds:
#
#   $draw          — closure(history = NULL) -> numeric omega-hat
#   $expand_orbit  — closure(omega_hat)      -> list of numeric vectors
#                    NULL if no orbit_expander_fn supplied
# ======================================================================

#' @keywords internal
calibrate_sampler <- function(
  sampler,
  parameter,
  estimand,
  solver
) {
  stopifnot(
    inherits(sampler, "sampler_spec"),
    inherits(parameter, "parameter_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec")
  )

  # -------------------------------------------------------------------
  # Shared pool of calibrated quantities available to constructors
  # -------------------------------------------------------------------
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

  .call_constructor <- function(fn, extra = list()) {
    args <- c(base_args, extra)
    fmls <- names(formals(fn))
    if ("..." %in% fmls) {
      do.call(fn, args)
    } else {
      do.call(fn, args[names(args) %in% fmls])
    }
  }

  # -------------------------------------------------------------------
  # 1. draw
  # -------------------------------------------------------------------
  sampler$draw <- if (!is.null(sampler$sampler_fn)) {
    .call_constructor(sampler$sampler_fn)
  } else {
    .build_default_sampler(base_args)
  }

  # -------------------------------------------------------------------
  # 2. expand_orbit (optional)
  # -------------------------------------------------------------------
  sampler$expand_orbit <- if (!is.null(sampler$orbit_expander_fn)) {
    .call_constructor(
      sampler$orbit_expander_fn,
      list(orbit_size = sampler$orbit_size)
    )
  } else {
    NULL
  }

  # -------------------------------------------------------------------
  # 3. Drop raw constructors — no longer needed after calibration
  # -------------------------------------------------------------------
  sampler$sampler_fn <- NULL
  sampler$orbit_expander_fn <- NULL

  sampler
}

# ======================================================================
# INTERNAL: Default sampler (gaussian initgen + feasibility projection)
# ======================================================================

#' @keywords internal
#' @noRd
.build_default_sampler <- function(base_args) {
  initgen_args <- base_args[
    names(base_args) %in% names(formals(omega_hat_initgen_gaussian))
  ]
  initgen <- do.call(omega_hat_initgen_gaussian, initgen_args)

  proj_args <- base_args[
    names(base_args) %in% names(formals(omega_hat_sampler))
  ]
  proj <- do.call(omega_hat_sampler, proj_args)

  function(history = NULL) {
    init <- initgen(history = history)
    proj(init)
  }
}
