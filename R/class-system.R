# ======================================================================
# class-system.R — Unified Class System (S3, domain-first)
# ======================================================================

# ======================================================================
# Utilities
# ======================================================================

.prepend_class <- function(x, class_name) {
  class(x) <- unique(c(class_name, class(x)))
  x
}

# ======================================================================
# SPECIFICATION OBJECTS (IDENTITY)
# ======================================================================

.new_spec <- function(x, class_name) {
  if (!is.list(x)) {
    stop("'", class_name, "' object must be a list.", call. = FALSE)
  }
  structure(x, class = c(class_name, "likelyr"))
}

new_parameter_spec <- function(x) .new_spec(x, "parameter_spec")
new_likelihood_spec <- function(x) .new_spec(x, "likelihood_spec")
new_estimand_spec <- function(x) .new_spec(x, "estimand_spec")
new_pipeline_spec <- function(x) .new_spec(x, "pipeline_spec")
new_solver_spec <- function(x) .new_spec(x, "solver_spec")
new_execution_spec <- function(x) .new_spec(x, "execution_spec")

new_serial_spec <- function(x) {
  if (!is.list(x)) {
    stop("'serial_spec' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("serial_spec", "execution_spec", "likelyr"))
}

new_parallel_spec <- function(x) {
  if (!is.list(x)) {
    stop("'parallel_spec' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("parallel_spec", "execution_spec", "likelyr"))
}

# ======================================================================
# MODEL OBJECTS (IDENTITY)
# ======================================================================

new_model_spec <- function(x = list()) {
  if (!is.list(x)) {
    stop("'model_spec' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("model_spec", "likelyr"))
}

new_calibrated_model <- function(x) {
  if (!is.list(x)) {
    stop("'calibrated' model object must be a list.", call. = FALSE)
  }
  structure(x, class = c("calibrated", "likelyr"))
}

# ======================================================================
# RESULTS CONTAINER (WORKSPACE)
# ======================================================================

new_workspace <- function(x = list()) {
  if (!is.list(x)) {
    stop("'workspace' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("workspace", "likelyr"))
}

# ======================================================================
# RESULT OBJECTS (IDENTITY + CONTRACT)
# ======================================================================

new_result <- function(x) {
  if (!is.list(x)) {
    stop("'result' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("result", "likelyr"))
}

.new_typed_result <- function(x, type, validator = NULL) {
  if (!is.null(validator)) {
    validator(x)
  }
  x <- new_result(x)
  .prepend_class(x, type)
}

new_profile_result <- function(x) {
  .new_typed_result(x, "profile", validate_profile_result)
}

new_integrate_result <- function(x) {
  .new_typed_result(x, "integrate", validate_integrate_result)
}

new_comparison_result <- function(x) {
  .new_typed_result(x, "comparison", validate_comparison_result)
}

# ----------------------------------------------------------------------
# Sub-results
# ----------------------------------------------------------------------

# ---- diagnostics (UNIFIED) ----
# Likelihood-specific behavior is stored as data, not class

new_diagnostics_result <- function(x, pseudolikelihood) {
  if (!is.character(pseudolikelihood) || length(pseudolikelihood) != 1L) {
    stop("'likelihood' must be a single character string.", call. = FALSE)
  }

  if (!pseudolikelihood %in% c("integrate", "profile")) {
    stop(
      "Invalid diagnostics likelihood: '",
      pseudolikelihood,
      "'.",
      call. = FALSE
    )
  }

  x$pseudolikelihood <- pseudolikelihood

  .new_typed_result(x, "diagnostics", validate_diagnostics_result)
}

# ---- inference ----

new_inference_result <- function(x) {
  validate_inference_result(x)
  .prepend_class(x, "inference")
}

# ======================================================================
# ADJECTIVAL STATE MARKERS (PIPELINE PROGRESSION)
# ======================================================================

# ---- model-level ----

mark_profiled <- function(x) {
  if (!inherits(x, "calibrated")) {
    stop("mark_profiled() requires a calibrated model.", call. = FALSE)
  }
  .prepend_class(x, "profiled")
}

mark_integrated <- function(x) {
  if (!inherits(x, "calibrated")) {
    stop("mark_integrated() requires a calibrated model.", call. = FALSE)
  }
  .prepend_class(x, "integrated")
}

# ---- result-level ----

mark_inferred <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_inferred() requires a result object.", call. = FALSE)
  }
  .prepend_class(x, "inferred")
}

mark_diagnosed <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_diagnosed() requires a result object.", call. = FALSE)
  }
  .prepend_class(x, "diagnosed")
}

# ---- workspace-level ----

mark_compared <- function(x) {
  if (!inherits(x, "workspace")) {
    stop("mark_compared() requires a workspace.", call. = FALSE)
  }
  .prepend_class(x, "compared")
}

# ======================================================================
# STATE QUERIES
# ======================================================================

is_calibrated <- function(x) inherits(x, "calibrated")
is_profiled <- function(x) inherits(x, "profiled")
is_integrated <- function(x) inherits(x, "integrated")

is_workspace <- function(x) inherits(x, "workspace")
is_compared <- function(x) inherits(x, "compared")

is_result <- function(x) inherits(x, "result")

is_profile <- function(x) inherits(x, "profile")
is_integrate <- function(x) inherits(x, "integrate")

is_inferred <- function(x) inherits(x, "inference")
is_diagnosed <- function(x) inherits(x, "diagnostics")

is_comparison <- function(x) inherits(x, "comparison")

# ======================================================================
# END class-system.R
# ======================================================================
