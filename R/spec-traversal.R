# ======================================================================
# spec-traversal.R — Traversal Specification (v2.0)
# ======================================================================

#' Specify the Branch Traversal Strategy
#'
#' @param increment Required. Positive numeric scalar, ψ-grid spacing.
#' @param traversal_method Character scalar. One of \code{"topdown"},
#'   \code{"leftright"}. Default: \code{"topdown"}.
#' @param mode_locator_fn Optional constructor function returning a
#'   closure \code{function(omega_hat) -> mode result}. When \code{NULL},
#'   the built-in \code{bracket_gss} locator is used.
#' @param confidence_levels Numeric vector in (0, 1).
#'   Default: \code{c(0.90, 0.95, 0.99)}.
#' @param cutoff_buffer Positive numeric scalar. Each branch is required
#'   to extend this multiple of the theoretical minimum cutoff distance.
#'   Default: \code{1.5}.
#' @param n_adjacent Non-negative integer. Number of grid points to
#'   evaluate on each side of the mode during \code{probe()}.
#'   Default: \code{3L}.
#' @param max_mode_shifts Non-negative integer. Maximum number of mode
#'   shifts permitted during \code{probe()} before rejecting.
#'   Default: \code{20L}.
#' @param k_recent Non-negative integer. Number of recent drops used
#'   as reference in the \code{probe()} drop magnitude check.
#'   Default: \code{3L}.
#' @param drop_multiplier Positive numeric scalar. A new drop exceeding
#'   this multiple of the recent median drop is flagged as a jump.
#'   Default: \code{2.0}.
#' @param cap_multiplier Positive numeric scalar. The absolute drop cap
#'   is set to \code{cap_multiplier} times the median per-step drop
#'   observed along the profile likelihood. Computed and stored on
#'   \code{model$traversal$max_drop_cap} during \code{preprocess()}.
#'   A step drop exceeding this cap is always rejected regardless of
#'   recent drop history. Larger values are more permissive; the default
#'   of \code{10.0} accommodates branch curvature that is moderately
#'   steeper than the profile without letting through genuine
#'   discontinuities. Default: \code{10.0}.
#' @param name Optional descriptive name.
#' @param ... Additional metadata stored but unused internally.
#'
#' @return A \code{traversal_spec} object.
#' @export
traversal_spec <- function(
  increment,
  traversal_method = "topdown",
  mode_locator_fn = NULL,
  confidence_levels = c(0.90, 0.95, 0.99),
  cutoff_buffer = 1.5,
  n_adjacent = 3L,
  max_mode_shifts = 20L,
  k_recent = 3L,
  drop_multiplier = 2.0,
  cap_multiplier = 10.0,
  name = NULL,
  ...
) {
  x <- list(
    name = name %||% "<traversal>",
    increment = increment,
    traversal_method = traversal_method,
    mode_locator_fn = mode_locator_fn,
    confidence_levels = confidence_levels,
    cutoff_buffer = cutoff_buffer,
    n_adjacent = n_adjacent,
    max_mode_shifts = max_mode_shifts,
    k_recent = k_recent,
    drop_multiplier = drop_multiplier,
    cap_multiplier = cap_multiplier,
    max_drop_cap = NULL, # populated by preprocess()
    extra = list(...)
  )

  x <- new_traversal_spec(x)
  .validate_traversal_spec(x)
  x
}

# ======================================================================
# INTERNAL CONSTRUCTOR
# ======================================================================

#' @keywords internal
#' @noRd
new_traversal_spec <- function(x) .new_spec(x, "traversal_spec")

# ======================================================================
# INTERNAL VALIDATOR
# ======================================================================

#' @keywords internal
#' @noRd
.validate_traversal_spec <- function(x) {
  # increment ---------------------------------------------------------
  if (
    !is.numeric(x$increment) ||
      length(x$increment) != 1L ||
      x$increment <= 0
  ) {
    stop("increment must be a positive numeric scalar.", call. = FALSE)
  }

  # traversal method --------------------------------------------------
  x$traversal_method <- match.arg(
    x$traversal_method,
    c("topdown", "leftright")
  )

  # mode_locator_fn ---------------------------------------------------
  if (!is.null(x$mode_locator_fn) && !is.function(x$mode_locator_fn)) {
    stop("mode_locator_fn must be NULL or a function.", call. = FALSE)
  }

  if (x$traversal_method == "leftright" && !is.null(x$mode_locator_fn)) {
    cat(
      "traversal_spec: mode_locator_fn is ignored when ",
      "traversal_method = \"leftright\"."
    )
  }

  # confidence levels -------------------------------------------------
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

  # cutoff_buffer -----------------------------------------------------
  if (
    !is.numeric(x$cutoff_buffer) ||
      length(x$cutoff_buffer) != 1L ||
      x$cutoff_buffer <= 0
  ) {
    stop("cutoff_buffer must be a positive numeric scalar.", call. = FALSE)
  }

  # n_adjacent --------------------------------------------------------
  if (
    !is.numeric(x$n_adjacent) ||
      length(x$n_adjacent) != 1L ||
      x$n_adjacent < 0
  ) {
    stop("n_adjacent must be a non-negative integer scalar.", call. = FALSE)
  }
  x$n_adjacent <- as.integer(x$n_adjacent)

  # max_mode_shifts ---------------------------------------------------
  if (
    !is.numeric(x$max_mode_shifts) ||
      length(x$max_mode_shifts) != 1L ||
      x$max_mode_shifts < 0
  ) {
    stop(
      "max_mode_shifts must be a non-negative integer scalar.",
      call. = FALSE
    )
  }
  x$max_mode_shifts <- as.integer(x$max_mode_shifts)

  # k_recent ----------------------------------------------------------
  if (
    !is.numeric(x$k_recent) ||
      length(x$k_recent) != 1L ||
      x$k_recent < 0
  ) {
    stop("k_recent must be a non-negative integer scalar.", call. = FALSE)
  }
  x$k_recent <- as.integer(x$k_recent)

  # drop_multiplier ---------------------------------------------------
  if (
    !is.numeric(x$drop_multiplier) ||
      length(x$drop_multiplier) != 1L ||
      x$drop_multiplier <= 0
  ) {
    stop("drop_multiplier must be a positive numeric scalar.", call. = FALSE)
  }

  # cap_multiplier ----------------------------------------------------
  if (
    !is.numeric(x$cap_multiplier) ||
      length(x$cap_multiplier) != 1L ||
      x$cap_multiplier <= 0
  ) {
    stop("cap_multiplier must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHOD
# ======================================================================

#' @export
print.traversal_spec <- function(x, ...) {
  cat("# Traversal Specification\n")
  cat("- Name:              ", x$name, "\n", sep = "")
  cat("- Increment:         ", x$increment, "\n", sep = "")
  cat("- Traversal method:  ", x$traversal_method, "\n", sep = "")
  cat(
    "- Mode locator:       ",
    if (!is.null(x$mode_locator_fn)) {
      "custom (mode_locator_fn supplied)"
    } else {
      "built-in default"
    },
    "\n",
    sep = ""
  )
  cat(
    "- CI levels:          ",
    paste(format(x$confidence_levels), collapse = ", "),
    "\n",
    sep = ""
  )
  cat("- Cutoff buffer:     ", x$cutoff_buffer, "\n", sep = "")
  cat("- n_adjacent:        ", x$n_adjacent, "\n", sep = "")
  cat("- max_mode_shifts:   ", x$max_mode_shifts, "\n", sep = "")
  cat("- k_recent:          ", x$k_recent, "\n", sep = "")
  cat("- drop_multiplier:   ", x$drop_multiplier, "\n", sep = "")
  cat("- cap_multiplier:    ", x$cap_multiplier, "\n", sep = "")
  if (!is.null(x$max_drop_cap)) {
    cat(
      "- max_drop_cap:      ",
      round(x$max_drop_cap, 6),
      "  (set by preprocess())\n",
      sep = ""
    )
  } else {
    cat("- max_drop_cap:       NULL  (set by preprocess())\n")
  }
  invisible(x)
}
