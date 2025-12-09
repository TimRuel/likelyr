# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# Unified class system (v3.1)
# ======================================================================

# ======================================================================
# SERIAL EXECUTION
# ======================================================================

#' Serial Execution Specification
#'
#' @description
#' Runs Monte Carlo branch generation **in serial** (no parallelism).
#'
#' @param R Positive integer: number of Monte Carlo branches.
#' @param seed Logical or numeric seed for reproducibility.
#' @param packages Character vector of packages to load.
#' @param name Optional identifier.
#'
#' @return A `serial_spec` object.
#' @export
serial_spec <- function(R,
                        seed     = TRUE,
                        packages = character(),
                        name     = "serial") {

  x <- list(
    name     = name,
    mode     = "serial",
    R        = R,
    seed     = seed,
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
#' @param num_workers Positive integer: number of parallel workers.
#' @param chunk_size Positive integer: branches per worker.
#' @param seed Logical or numeric seed for reproducibility.
#' @param packages Character vector of packages to load on workers.
#' @param name Optional identifier.
#'
#' @return A `parallel_spec` object.
#' @export
parallel_spec <- function(num_workers,
                          chunk_size = 1L,
                          seed       = TRUE,
                          packages   = character(),
                          name       = "parallel") {

  x <- list(
    name        = name,
    mode        = "parallel",
    num_workers = num_workers,
    chunk_size  = chunk_size,
    seed        = seed,
    packages    = packages
  )

  x <- new_parallel_spec(x)
  .validate_execution_parallel(x)
  x
}

# ======================================================================
# VALIDATORS
# ======================================================================

.validate_execution_serial <- function(x) {

  # R: number of serial MC branches
  if (!is.numeric(x$R) ||
      length(x$R) != 1 ||
      !is.finite(x$R) ||
      x$R < 1 ||
      x$R != as.integer(x$R))
    stop("serial_spec(): R must be a positive integer.", call. = FALSE)

  # seed: logical or numeric
  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("serial_spec(): seed must be logical or numeric.", call. = FALSE)

  # packages: character vector
  if (!is.character(x$packages))
    stop("serial_spec(): packages must be a character vector.", call. = FALSE)

  invisible(x)
}

.validate_execution_parallel <- function(x) {

  # workers
  if (!is.numeric(x$num_workers) ||
      length(x$num_workers) != 1 ||
      !is.finite(x$num_workers) ||
      x$num_workers < 1 ||
      x$num_workers != as.integer(x$num_workers))
    stop("parallel_spec(): num_workers must be a positive integer.", call. = FALSE)

  # chunk size
  if (!is.numeric(x$chunk_size) ||
      length(x$chunk_size) != 1 ||
      !is.finite(x$chunk_size) ||
      x$chunk_size < 1 ||
      x$chunk_size != as.integer(x$chunk_size))
    stop("parallel_spec(): chunk_size must be a positive integer.", call. = FALSE)

  # seed
  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("parallel_spec(): seed must be logical or numeric.", call. = FALSE)

  # packages
  if (!is.character(x$packages))
    stop("parallel_spec(): packages must be a character vector.", call. = FALSE)

  invisible(x)
}

# ======================================================================
# total_branches() — S3 generic
# ======================================================================

#' Total Number of Monte Carlo Branches
#' @export
total_branches <- function(x) {
  UseMethod("total_branches")
}

#' @export
total_branches.serial_spec <- function(x) x$R

#' @export
total_branches.parallel_spec <- function(x) {
  x$num_workers * x$chunk_size
}

#' @export
total_branches.default <- function(x) {
  stop("total_branches() expects an execution_spec object.")
}

# ======================================================================
# PRINT METHODS
# ======================================================================

#' @export
print.serial_spec <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat("- Branches:    ", total_branches(x), "\n", sep = "")
  cat("- Seed:        ", x$seed, "\n", sep = "")
  cat("- Packages:    ",
      if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
      "\n", sep = "")
  invisible(x)
}

#' @export
print.parallel_spec <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Name:        ", x$name, "\n", sep = "")
  cat("- Workers:     ", x$num_workers, "\n", sep = "")
  cat("- Chunk size:  ", x$chunk_size, "\n", sep = "")
  cat("- Branches:    ", total_branches(x), "\n", sep = "")
  cat("- Seed:        ", x$seed, "\n", sep = "")
  cat("- Packages:    ",
      if (length(x$packages)) paste(x$packages, collapse = ", ") else "<none>",
      "\n", sep = "")
  invisible(x)
}
