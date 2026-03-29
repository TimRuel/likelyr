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
new_sampler_spec <- function(x) .new_spec(x, "sampler_spec")
new_traversal_spec <- function(x) .new_spec(x, "traversal_spec")
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
# MODEL OBJECTS
# ======================================================================

# Pre-calibration: class = c("model_spec", "likelyr")
new_model_spec <- function(x = list()) {
  if (!is.list(x)) {
    stop("'model_spec' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("model_spec", "likelyr"))
}

# Post-calibration: class = c("model", "likelyr")
new_model <- function(x) {
  if (!is.list(x)) {
    stop("'model' object must be a list.", call. = FALSE)
  }
  structure(x, class = c("model", "likelyr"))
}

# ======================================================================
# WORKSPACE (RESULTS CONTAINER)
#
# User-facing container holding at most one result of each top-level
# type. Slots are fixed and declared explicitly at construction; they
# never appear dynamically. Inference and diagnostic results attach to
# the profile/integrated result objects themselves, not to the workspace.
#
# Slots:
#   $profile    — a profile result, or NULL
#   $integrated — an integrated_cache (after preprocess()), an
#                 integrated result (after integrate()), or NULL
#   $comparison — a comparison result, or NULL
# ======================================================================

new_workspace <- function(x = list()) {
  if (!is.list(x)) {
    stop("'workspace' object must be a list.", call. = FALSE)
  }
  defaults <- list(profile = NULL, integrated = NULL, comparison = NULL)
  x <- utils::modifyList(defaults, x)
  structure(x, class = c("workspace", "likelyr"))
}

# ======================================================================
# INTEGRATED CACHE
#
# Transient object occupying $workspace$integrated between preprocess()
# and integrate(). Replaced by an integrated result once integrate()
# completes. Does not carry the "result" class.
# ======================================================================

new_integrated_cache <- function(x) {
  if (!is.list(x)) {
    stop("'integrated_cache' object must be a list.", call. = FALSE)
  }
  structure(x, class = "integrated_cache")
}

is_integrated_cache <- function(x) inherits(x, "integrated_cache")

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

# profile result    — class: c("profile",    "result", "likelyr")
new_profile_result <- function(x) {
  .new_typed_result(x, "profile", validate_profile_result)
}

# integrated result — class: c("integrated", "result", "likelyr")
new_integrated_result <- function(x) {
  .new_typed_result(x, "integrated", validate_integrated_result)
}

# inference result  — class: c("inference",  "result", "likelyr")
# Attaches to a profile or integrated result; does not live on workspace.
new_inference_result <- function(x) {
  .new_typed_result(x, "inference", validate_inference_result)
}

# diagnostic result — class: c("diagnostic", "result", "likelyr")
# Attaches to a profile or integrated result; does not live on workspace.
new_diagnostic_result <- function(x, pseudolikelihood) {
  .new_typed_result(x, "diagnostic", validate_diagnostic_result)
}

# comparison result — class: c("comparison", "result", "likelyr")
new_comparison_result <- function(x) {
  .new_typed_result(x, "comparison", validate_comparison_result)
}

# ======================================================================
# ADJECTIVAL STATE MARKERS (PIPELINE PROGRESSION)
# ======================================================================

# ---- model-level: require model ----

#' Mark a model as having been preprocessed
#' @keywords internal
mark_preprocessed <- function(x) {
  if (!is_model(x)) {
    stop("mark_preprocessed() requires a 'model' object.", call. = FALSE)
  }
  cls <- class(x)
  profiled_idx <- match("profiled", cls)
  if (!is.na(profiled_idx)) {
    class(x) <- append(cls, "preprocessed", after = profiled_idx)
  } else {
    class(x) <- c("preprocessed", cls)
  }
  x
}

#' Mark a model as having a profile likelihood computed
#' @keywords internal
mark_profiled <- function(x) {
  if (!is_model(x)) {
    stop("mark_profiled() requires a 'model' object.", call. = FALSE)
  }
  .prepend_class(x, "profiled")
}

#' Mark a model as having an integrated likelihood computed.
#' Strips "preprocessed" from the class vector — preprocessing is a
#' transient state that is superseded once integration is complete.
#' @keywords internal
mark_integrated <- function(x) {
  if (!is_model(x)) {
    stop("mark_integrated() requires a 'model' object.", call. = FALSE)
  }
  class(x) <- setdiff(class(x), "preprocessed")
  .prepend_class(x, "integrated")
}

#' Mark a model as having a comparison computed
#' @keywords internal
mark_compared <- function(x) {
  if (!is_model(x)) {
    stop("mark_compared() requires a 'model' object.", call. = FALSE)
  }
  .prepend_class(x, "compared")
}

# ---- result-level: require result ----

#' Mark a result object as having inference computed
#' @keywords internal
mark_inferred <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_inferred() requires a 'result' object.", call. = FALSE)
  }
  .prepend_class(x, "inferred")
}

#' Mark a result object as having diagnostics computed
#' @keywords internal
mark_diagnosed <- function(x) {
  if (!inherits(x, "result")) {
    stop("mark_diagnosed() requires a 'result' object.", call. = FALSE)
  }
  .prepend_class(x, "diagnosed")
}

# ======================================================================
# STATE QUERIES
# ======================================================================

# ---- model identity ----

#' Is this a model object (pre- or post-calibration)?
is_model <- function(x) inherits(x, "model_spec") || inherits(x, "model")

#' Is this a fully calibrated model?
is_calibrated <- function(x) inherits(x, "model")

# ---- model state ----

#' Has preprocess() been run on this model?
is_preprocessed <- function(x) inherits(x, "preprocessed")

#' Has profile() been run on this model?
is_profiled <- function(x) inherits(x, "profiled")

#' Has integrate() been run on this model?
#' NOTE: "integrated" also appears as a result type; is_model() disambiguates.
is_integrated <- function(x) inherits(x, "integrated") && is_model(x)

#' Has infer() been run on this result?
is_inferred <- function(x) inherits(x, "inferred")

#' Has diagnose() been run on this result?
is_diagnosed <- function(x) inherits(x, "diagnosed")

#' Has compare() been run on this model?
is_compared <- function(x) inherits(x, "compared")

# ---- result identity ----

#' Is this a profile likelihood result?
is_profile_result <- function(x) inherits(x, "profile") && is_result(x)

#' Is this an integrated likelihood result?
#' NOTE: "integrated" also appears as a model state marker; is_result() disambiguates.
is_integrated_result <- function(x) inherits(x, "integrated") && is_result(x)

#' Is this an inference result?
is_inference_result <- function(x) inherits(x, "inference") && is_result(x)

#' Is this a diagnostic result?
is_diagnostic_result <- function(x) inherits(x, "diagnostic") && is_result(x)

#' Is this a comparison result?
is_comparison_result <- function(x) inherits(x, "comparison") && is_result(x)

# ---- result state ----

is_result <- function(x) inherits(x, "result")

# ---- workspace identity ----

is_workspace <- function(x) inherits(x, "workspace")

# ---- workspace slot queries ----

#' Does the workspace hold a profile likelihood result?
has_profile_result <- function(ws) {
  is_workspace(ws) && is_profile_result(ws$profile)
}

#' Does the workspace hold an integrated likelihood result?
has_integrated_result <- function(ws) {
  is_workspace(ws) && is_integrated_result(ws$integrated)
}

#' Does the workspace hold a comparison result?
has_comparison_result <- function(ws) {
  is_workspace(ws) && is_comparison_result(ws$comparison)
}

# ======================================================================
# END class-system.R
# ======================================================================
