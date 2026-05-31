# ======================================================================
# calibrate-traversal.R — Traversal Calibration (v2.1)
#
# After calibration, model$traversal holds:
#   $branch_binder  — closure(omega_hat) -> branch_evaluator
#   $locate_mode    — closure(omega_hat, psi_hint) -> mode result
#                     NULL if traversal_method = "leftright"
#   $psi_interval   — sets::interval or NULL, copied from estimand
# ======================================================================

#' @keywords internal
calibrate_traversal <- function(
  traversal,
  parameter,
  likelihood,
  estimand,
  solver,
  data
) {
  stopifnot(
    inherits(traversal, "traversal_spec"),
    inherits(parameter, "parameter_spec"),
    inherits(likelihood, "likelihood_spec"),
    inherits(estimand, "estimand_spec"),
    inherits(solver, "solver_spec")
  )

  # -------------------------------------------------------------------
  # 1. Store psi_interval from estimand for use during traversal
  # -------------------------------------------------------------------
  traversal$psi_interval <- estimand$psi_interval %||% NULL

  # -------------------------------------------------------------------
  # 2. Build branch binder — shared by locate_mode and sieve()
  # -------------------------------------------------------------------
  traversal$branch_binder <- branch_binder_constructor(
    parameter = parameter,
    likelihood = likelihood,
    estimand = estimand,
    solver = solver
  )

  # -------------------------------------------------------------------
  # 3. Build locate_mode (topdown only)
  # -------------------------------------------------------------------
  if (traversal$traversal_method == "leftright") {
    traversal$locate_mode <- NULL
    return(traversal)
  }

  base_args <- list(
    param_mle = parameter$param_mle,
    param_dim = parameter$param_dim,
    omega_dim = parameter$omega_dim %||% parameter$param_dim,
    psi_mle = estimand$psi_mle,
    psi_interval = estimand$psi_interval,
    psi_fn = estimand$psi_fn,
    psi_jac = estimand$psi_jac,
    E_loglik_grad = likelihood$E_loglik_grad,
    increment = traversal$increment,
    branch_binder = traversal$branch_binder,
    solver = solver,
    data = data
  )

  .call_constructor <- function(fn) {
    fmls <- names(formals(fn))
    if ("..." %in% fmls) {
      do.call(fn, base_args)
    } else {
      do.call(fn, base_args[names(base_args) %in% fmls])
    }
  }

  traversal$locate_mode <- if (!is.null(traversal$mode_locator_fn)) {
    .call_constructor(traversal$mode_locator_fn)
  } else {
    .build_default_locator(base_args)
  }

  traversal$mode_locator_fn <- NULL

  traversal
}

# ======================================================================
# INTERNAL: Default mode locator (bracket_gss)
# ======================================================================

#' @keywords internal
#' @noRd
.build_default_locator <- function(base_args) {
  locator_factory <- get_branch_mode_locator("bracket_gss")
  factory_formals_all <- names(formals(locator_factory))
  factory_formals <- setdiff(factory_formals_all, "...")
  required_formals <- Filter(
    function(nm) identical(formals(locator_factory)[[nm]], quote(expr = )),
    factory_formals
  )

  branch_binder <- base_args$branch_binder
  param_mle <- base_args$param_mle
  psi_mle <- base_args$psi_mle
  increment <- base_args$increment
  same_space <- isTRUE(base_args$omega_dim == base_args$param_dim)

  function(omega_hat, psi_hint = NULL) {
    branch_evaluator <- branch_binder(omega_hat)

    # When omega_hat and param share the same space, omega_hat is itself
    # a valid warm start. Otherwise use param_mle, which is always a
    # valid element of the model parameter space.
    param_init <- if (same_space) omega_hat else param_mle

    arg_pool <- list(
      branch_evaluator = branch_evaluator,
      psi_init = psi_hint %||% psi_mle,
      param_init = param_init,
      psi_mle = psi_mle,
      increment = increment
    )

    missing_args <- setdiff(required_formals, names(arg_pool))
    if (length(missing_args) > 0) {
      stop(
        "Default mode locator (bracket_gss) missing required arguments: ",
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
        "Default mode locator (bracket_gss) did not return a function.",
        call. = FALSE
      )
    }

    locator(omega_hat)
  }
}
