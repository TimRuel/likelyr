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
new_nuisance_spec <- function(x) .new_spec(x, "nuisance_spec")
new_optimizer_spec <- function(x) .new_spec(x, "optimizer_spec")
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

# ---- sub-results (inherit parent result) ----

new_diagnostics_integrate_result <- function(x) {
  .new_typed_result(x, "diagnostics_integrate", validate_diagnostics_result)
}

new_diagnostics_profile_result <- function(x) {
  .new_typed_result(x, "diagnostics_profile", validate_diagnostics_result)
}

new_inference_result <- function(x) {
  validate_inference_result(x)
  .prepend_class(x, "inference")
}

# ======================================================================
# ADJECTIVAL STATE MARKERS (PIPELINE PROGRESSION)
# ======================================================================

# ---- model-level ----

#' Mark a calibrated model as profiled
#'
#' @param x A calibrated model object.
#' @return The same object with class \code{"profiled"} prepended.
#' @keywords internal
mark_profiled <- function(x) {
  if (!inherits(x, "calibrated")) {
    stop("mark_profiled() requires a calibrated model.", call. = FALSE)
  }
  .prepend_class(x, "profiled")
}

#' Mark a calibrated model as integrated
#'
#' @param x A calibrated model object.
#' @return The same object with class \code{"integrated"} prepended.
#' @keywords internal
mark_integrated <- function(x) {
  if (!inherits(x, "calibrated")) {
    stop("mark_integrated() requires a calibrated model.", call. = FALSE)
  }
  .prepend_class(x, "integrated")
}

# ---- result-level ----

#' Mark a result as inferred
#'
#' @param x A result object.
#' @return The same object with class \code{"inference"} prepended.
#' @keywords internal
mark_inferred <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_inferred() requires a result object.", call. = FALSE)
  }
  .prepend_class(x, "inferred")
}

#' Mark a result as diagnosed
#'
#' @param x A result object.
#' @return The same object with class \code{"diagnostics"} prepended.
#' @keywords internal
mark_diagnosed <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_diagnosed() requires a result object.", call. = FALSE)
  }
  .prepend_class(x, "diagnosed")
}

# ---- workspace-level ----

#' Mark a workspace as compared
#'
#' @param x A workspace object.
#' @return The same object with class \code{"compared"} prepended.
#' @keywords internal
mark_compared <- function(x) {
  if (!inherits(x, "workspace")) {
    stop("mark_compared() requires a workspace.", call. = FALSE)
  }
  .prepend_class(x, "compared")
}

# ======================================================================
# STATE QUERIES
# ======================================================================

#' Test if object is calibrated
#' @keywords internal
is_calibrated <- function(x) inherits(x, "calibrated")

#' Test if object is profiled
#' @keywords internal
is_profiled <- function(x) inherits(x, "profiled")

#' Test if object is integrated
#' @keywords internal
is_integrated <- function(x) inherits(x, "integrated")

#' Test if object is a workspace
#' @keywords internal
is_workspace <- function(x) inherits(x, "workspace")

#' Test if workspace has been compared
#' @keywords internal
is_compared <- function(x) inherits(x, "compared")

#' Test if object is a result
#' @keywords internal
is_result <- function(x) inherits(x, "result")

#' Test if result is profile likelihood
#' @keywords internal
is_profile <- function(x) inherits(x, "profile")

#' Test if result is integrated likelihood
#' @keywords internal
is_integrate <- function(x) inherits(x, "integrate")

#' Test if result has inference attached
#' @keywords internal
is_inferred <- function(x) inherits(x, "inference")

#' Test if result has diagnostics attached
#' @keywords internal
is_diagnosed <- function(x) inherits(x, "diagnostics")

#' Test if result is a comparison object
#' @keywords internal
is_comparison <- function(x) inherits(x, "comparison")

# ======================================================================
# END class-system.R
# ======================================================================
