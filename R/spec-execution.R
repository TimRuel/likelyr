# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# Unified class system (v3.3)
# ======================================================================

# ======================================================================
# SERIAL EXECUTION
# ======================================================================

#' Serial Execution Specification
#'
#' @description
#' Runs Monte Carlo branch generation \strong{in serial} (no parallelism).
#'
#' @param packages Character vector of packages to load.
#' @param name Optional identifier.
#'
#' @return A \code{serial_spec} object.
#' @export
serial_spec <- function(
  packages = character(),
  name = "serial"
) {
  x <- list(
    name = name,
    mode = "serial",
    packages = packages
  )

  x <- new_serial_spec(x)
  .validate_execution_serial(x)
  x
}

# ======================================================================
# PARALLEL EXECUTION
# ======================================================================

#' Parallel Execution Specification (foreach + %dofuture%)
#'
#' @description
#' Runs Monte Carlo branch generation in parallel via
#' \code{foreach} + \code{\%dofuture\%}. The chunk size is derived
#' during calibration from \code{sampler$total_seeds} as the smallest
#' integer \code{k} such that \code{k * num_workers >= total_seeds}.
#'
#' @param num_workers Positive integer. Number of parallel workers.
#' @param packages Character vector of packages to load on workers.
#' @param name Optional identifier.
#'
#' @return A \code{parallel_spec} object.
#' @export
parallel_spec <- function(
  num_workers,
  packages = character(),
  name = "parallel"
) {
  x <- list(
    name = name,
    mode = "parallel",
    num_workers = num_workers,
    packages = packages
  )

  x <- new_parallel_spec(x)
  .validate_execution_parallel(x)
  x
}

# ======================================================================
# VALIDATORS
# ======================================================================

#' @keywords internal
#' @noRd
.validate_execution_serial <- function(x) {
  if (!is.character(x$packages)) {
    stop("serial_spec(): packages must be a character vector.", call. = FALSE)
  }

  invisible(x)
}

#' @keywords internal
#' @noRd
.validate_execution_parallel <- function(x) {
  if (
    !is.numeric(x$num_workers) ||
      length(x$num_workers) != 1L ||
      !is.finite(x$num_workers) ||
      x$num_workers != as.integer(x$num_workers) ||
      x$num_workers < 1
  ) {
    stop(
      "parallel_spec(): num_workers must be a positive integer.",
      call. = FALSE
    )
  }

  if (!is.character(x$packages)) {
    stop("parallel_spec(): packages must be a character vector.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# PRINT METHODS
# ======================================================================

#' @export
print.serial_spec <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat(
    "- Packages:    ",
    if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.parallel_spec <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat("- Workers:     ", x$num_workers, "\n", sep = "")
  cat(
    "- Packages:    ",
    if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
    "\n",
    sep = ""
  )
  if (!is.null(x$chunk_size)) {
    cat("- Chunk size:  ", x$chunk_size, "\n", sep = "")
  }
  invisible(x)
}
