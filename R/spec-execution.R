# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# Unified class system (v3.2)
# ======================================================================

# ======================================================================
# SERIAL EXECUTION
# ======================================================================

#' Serial Execution Specification
#'
#' @description
#' Runs Monte Carlo branch generation \strong{in serial} (no parallelism).
#'
#' @param min_branches Positive integer. Minimum number of branches
#'   to retain after aggregation filtering.
#' @param branch_buffer Non-negative integer. Number of additional seeds
#'   to evaluate beyond \code{min_branches}. Total seeds evaluated =
#'   \code{min_branches + branch_buffer}.
#' @param seed Logical or numeric seed for reproducibility.
#' @param packages Character vector of packages to load.
#' @param name Optional identifier.
#'
#' @return A \code{serial_spec} object.
#' @export
serial_spec <- function(
  min_branches,
  branch_buffer = 0L,
  seed = TRUE,
  packages = character(),
  name = "serial"
) {
  x <- list(
    name = name,
    mode = "serial",
    min_branches = min_branches,
    branch_buffer = branch_buffer,
    seed = seed,
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
#' during calibration as the smallest integer \code{k} such that
#' \code{k * num_workers >= min_branches + branch_buffer}.
#'
#' @param num_workers Positive integer. Number of parallel workers.
#' @param min_branches Positive integer. Minimum number of branches
#'   to retain after aggregation filtering.
#' @param branch_buffer Non-negative integer. Extra seeds to evaluate
#'   beyond \code{min_branches}. The actual number of seeds evaluated
#'   will be \code{num_workers * chunk_size >= min_branches + branch_buffer}.
#' @param seed Logical or numeric seed for reproducibility.
#' @param packages Character vector of packages to load on workers.
#' @param name Optional identifier.
#'
#' @return A \code{parallel_spec} object.
#' @export
parallel_spec <- function(
  num_workers,
  min_branches,
  branch_buffer = 0L,
  seed = TRUE,
  packages = character(),
  name = "parallel"
) {
  x <- list(
    name = name,
    mode = "parallel",
    num_workers = num_workers,
    min_branches = min_branches,
    branch_buffer = branch_buffer,
    seed = seed,
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
  if (
    !is.numeric(x$min_branches) ||
      length(x$min_branches) != 1L ||
      !is.finite(x$min_branches) ||
      x$min_branches < 1 ||
      x$min_branches != as.integer(x$min_branches)
  ) {
    stop(
      "serial_spec(): min_branches must be a positive integer.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(x$branch_buffer) ||
      length(x$branch_buffer) != 1L ||
      !is.finite(x$branch_buffer) ||
      x$branch_buffer < 0 ||
      x$branch_buffer != as.integer(x$branch_buffer)
  ) {
    stop(
      "serial_spec(): branch_buffer must be a non-negative integer.",
      call. = FALSE
    )
  }

  if (!is.logical(x$seed) && !is.numeric(x$seed)) {
    stop("serial_spec(): seed must be logical or numeric.", call. = FALSE)
  }

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
      x$num_workers < 1 ||
      x$num_workers != as.integer(x$num_workers)
  ) {
    stop(
      "parallel_spec(): num_workers must be a positive integer.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(x$min_branches) ||
      length(x$min_branches) != 1L ||
      !is.finite(x$min_branches) ||
      x$min_branches < 1 ||
      x$min_branches != as.integer(x$min_branches)
  ) {
    stop(
      "parallel_spec(): min_branches must be a positive integer.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(x$branch_buffer) ||
      length(x$branch_buffer) != 1L ||
      !is.finite(x$branch_buffer) ||
      x$branch_buffer < 0 ||
      x$branch_buffer != as.integer(x$branch_buffer)
  ) {
    stop(
      "parallel_spec(): branch_buffer must be a non-negative integer.",
      call. = FALSE
    )
  }

  if (!is.logical(x$seed) && !is.numeric(x$seed)) {
    stop("parallel_spec(): seed must be logical or numeric.", call. = FALSE)
  }

  if (!is.character(x$packages)) {
    stop("parallel_spec(): packages must be a character vector.", call. = FALSE)
  }

  invisible(x)
}

# ======================================================================
# total_seeds() — S3 generic
# ======================================================================

#' Total Number of Seeds to Evaluate
#'
#' @description
#' Returns the actual number of branch seeds to request from
#' \code{sieve()} and evaluate. For serial execution this is
#' \code{min_branches + branch_buffer}. For parallel execution this
#' is \code{num_workers * chunk_size} (derived during calibration),
#' which is >= \code{min_branches + branch_buffer}.
#'
#' Requires \code{calibrate_execution()} to have been run for
#' parallel specs (to derive \code{chunk_size}).
#'
#' @keywords internal
total_seeds <- function(x) UseMethod("total_seeds")

#' @keywords internal
total_seeds.serial_spec <- function(x) x$min_branches + x$branch_buffer

#' @keywords internal
total_seeds.parallel_spec <- function(x) {
  if (is.null(x$chunk_size)) {
    stop(
      "total_seeds() requires calibrated parallel_spec. ",
      "Run calibrate_execution() first.",
      call. = FALSE
    )
  }
  x$num_workers * x$chunk_size
}

#' @keywords internal
total_seeds.default <- function(x) {
  stop("total_seeds() expects an execution_spec object.")
}

# ======================================================================
# PRINT METHODS
# ======================================================================

#' @export
print.serial_spec <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- Name:          ", x$name, "\n", sep = "")
  cat("- Min branches:  ", x$min_branches, "\n", sep = "")
  cat("- Branch buffer: ", x$branch_buffer, "\n", sep = "")
  cat("- Total seeds:   ", total_seeds(x), "\n", sep = "")
  cat("- Seed:          ", x$seed, "\n", sep = "")
  cat(
    "- Packages:      ",
    if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.parallel_spec <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Name:          ", x$name, "\n", sep = "")
  cat("- Workers:       ", x$num_workers, "\n", sep = "")
  cat("- Min branches:  ", x$min_branches, "\n", sep = "")
  cat("- Branch buffer: ", x$branch_buffer, "\n", sep = "")
  if (!is.null(x$chunk_size)) {
    cat("- Chunk size:    ", x$chunk_size, "\n", sep = "")
    cat("- Total seeds:   ", total_seeds(x), "\n", sep = "")
  }
  cat("- Seed:          ", x$seed, "\n", sep = "")
  cat(
    "- Packages:      ",
    if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
    "\n",
    sep = ""
  )
  invisible(x)
}
