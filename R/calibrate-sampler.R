# ======================================================================
# calibrate-sampler.R — Sampler Calibration (v2.4)
#
# Resolves a sampler_spec into a closed-over draw function ready for
# use by sieve(). After calibration, model$sampler holds:
#
#   $draw        — closure(history = NULL) ->
#                  list(candidate, diag)
#                  where $candidate is a numeric omega-hat and $diag
#                  is a named list of draw-level metadata (may be empty)
#   $total_seeds — integer: min_branches + branch_buffer
#                  used by sieve() to know how many to collect, and
#                  by calibrate_execution() to derive chunk_size
#
# The base_args pool passes all calibrated quantities to the constructor.
# Constructors declare only the arguments they need — matching is done
# via formals(). The pool is application-agnostic; application-specific
# logic belongs entirely in the sampler_fn, not here.
# ======================================================================

#' @keywords internal
calibrate_sampler <- function(
  sampler,
  parameter,
  estimand,
  solver,
  data
) {
  stopifnot(
    inherits(sampler, "sampler_spec"),
    inherits(parameter, "parameter_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec")
  )

  # -------------------------------------------------------------------
  # Shared pool of calibrated quantities available to constructors.
  # Constructors receive only the arguments they declare in formals().
  # Application-specific extraction (e.g. counts, design matrices)
  # belongs in the sampler_fn, not here.
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
    solver = solver,
    data = data
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
    .call_constructor(sampler$sampler_fn, extra = sampler$extra)
  } else {
    .build_default_sampler(base_args)
  }

  # -------------------------------------------------------------------
  # 2. total_seeds — computed here so sieve() and calibrate_execution()
  #    can both read it from model$sampler
  # -------------------------------------------------------------------
  sampler$total_seeds <- as.integer(
    sampler$min_branches + sampler$branch_buffer
  )

  # -------------------------------------------------------------------
  # 3. Drop raw constructors — no longer needed after calibration
  # -------------------------------------------------------------------
  sampler$sampler_fn <- NULL

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
    list(candidate = proj(init), diag = list())
  }
}
