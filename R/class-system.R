# ======================================================================
# class-system.R — Unified Class System (Constructors + Class Tools Only)
# ======================================================================

# ======================================================================
# Utility: Add / Drop / Check Classes
# ======================================================================

.add_class <- function(x, class_name) {
  class(x) <- unique(c(class_name, class(x)))
  x
}

.drop_class <- function(x, class_name) {
  class(x) <- setdiff(class(x), class_name)
  x
}

.has_classes <- function(x, classes) {
  all(classes %in% class(x))
}

# ======================================================================
# SPECIFICATION CLASS CONSTRUCTORS
# ======================================================================

new_likelihood_spec <- function(x) {
  class(x) <- c("likelihood_spec", "likelyr")
  x
}

new_estimand_spec <- function(x) {
  class(x) <- c("estimand_spec", "likelyr")
  x
}

new_nuisance_spec <- function(x) {
  class(x) <- c("nuisance_spec", "likelyr")
  x
}

new_optimizer_spec <- function(x) {
  class(x) <- c("optimizer_spec", "likelyr")
  x
}

new_execution_spec <- function(x, serial = FALSE) {
  if (serial)
    class(x) <- c("serial_spec", "execution_spec", "likelyr")
  else
    class(x) <- c("parallel_spec", "execution_spec", "likelyr")
  x
}

# ======================================================================
# MODEL SPECIFICATION OBJECT
# ======================================================================

new_model_spec <- function(x = list()) {
  class(x) <- c("model_spec", "likelyr")
  x
}

# ======================================================================
# CALIBRATED MODEL
# ======================================================================

new_calibrated_model <- function(x = list()) {
  class(x) <- c("calibrated_model", "likelyr")
  x
}

# ======================================================================
# RESULT OBJECT CLASSES
# ======================================================================

new_likelyr_result <- function(x = list()) {
  class(x) <- c("likelyr_result", "likelyr")
  x
}

new_il_result <- function(x = list()) {
  class(x) <- c("likelyr_il_result", "likelyr_result", "likelyr")
  x
}

new_pl_result <- function(x = list()) {
  class(x) <- c("likelyr_pl_result", "likelyr_result", "likelyr")
  x
}

new_diagnostics_result <- function(x = list()) {
  class(x) <- c("likelyr_diagnostics", "likelyr_result", "likelyr")
  x
}

new_inference_result <- function(x = list()) {
  class(x) <- c("likelyr_inference", "likelyr_result", "likelyr")
  x
}

new_comparison_result <- function(x = list()) {
  class(x) <- c("likelyr_comparison", "likelyr_result", "likelyr")
  x
}

# ======================================================================
# STATE MARKERS
# ======================================================================

mark_integrated <- function(x) .add_class(x, "likelyr_integrated")
mark_profiled   <- function(x) .add_class(x, "likelyr_profiled")
mark_diagnosed  <- function(x) .add_class(x, "likelyr_diagnosed")
mark_inferred   <- function(x) .add_class(x, "likelyr_inferred")
mark_compared   <- function(x) .add_class(x, "likelyr_compared")

is_integrated <- function(x) inherits(x, "likelyr_integrated")
is_profiled   <- function(x) inherits(x, "likelyr_profiled")
is_diagnosed  <- function(x) inherits(x, "likelyr_diagnosed")
is_inferred   <- function(x) inherits(x, "likelyr_inferred")
is_compared   <- function(x) inherits(x, "likelyr_compared")

# ======================================================================
# END class-system.R
# ======================================================================
