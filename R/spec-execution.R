# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# Polished for v3.0 model_spec pipeline
# ======================================================================

# ----------------------------------------------------------------------
# Serial Execution Specification
# ----------------------------------------------------------------------

#' Serial Execution Specification
#'
#' @description
#' Runs Monte Carlo computation **in serial**. No parallel workers are used.
#'
#' @param R Positive integer: number of Monte Carlo branches.
#' @param seed Logical or integer seed.
#' @param packages Character vector of required packages.
#' @param name Optional name/label.
#'
#' @return A `serial_spec` (also inherits `execution_spec`).
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

  class(x) <- c("serial_spec", "execution_spec")
  .validate_execution_serial(x)
  x
}

# ----------------------------------------------------------------------
# Parallel Execution Specification
# ----------------------------------------------------------------------

#' Parallel Execution Specification (foreach + %dofuture%)
#'
#' @description
#' Runs Monte Carlo computation **in parallel** using a configured
#' future backend. Total branches = num_workers * chunk_size.
#'
#' @param num_workers Positive integer number of workers.
#' @param chunk_size  Positive integer chunk size per worker.
#' @param seed Logical or integer.
#' @param packages Character vector.
#' @param name Optional label.
#'
#' @return A `parallel_spec` (also inherits `execution_spec`).
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

  class(x) <- c("parallel_spec", "execution_spec")
  .validate_execution_parallel(x)
  x
}

# ======================================================================
# INTERNAL VALIDATORS (tightened)
# ======================================================================

#' @keywords internal
.validate_execution_serial <- function(x) {

  if (!is.numeric(x$R) || length(x$R) != 1 || is.na(x$R) || x$R < 1)
    stop("serial_spec(): `R` must be a positive integer >= 1.", call. = FALSE)

  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("serial_spec(): `seed` must be logical or numeric.", call. = FALSE)

  if (!is.character(x$packages))
    stop("serial_spec(): `packages` must be a character vector.", call. = FALSE)

  invisible(x)
}

#' @keywords internal
.validate_execution_parallel <- function(x) {

  if (!is.numeric(x$num_workers) ||
      length(x$num_workers) != 1 ||
      is.na(x$num_workers) ||
      x$num_workers < 1)
    stop("parallel_spec(): `num_workers` must be a positive integer >= 1.",
         call. = FALSE)

  if (!is.numeric(x$chunk_size) ||
      length(x$chunk_size) != 1 ||
      is.na(x$chunk_size) ||
      x$chunk_size < 1)
    stop("parallel_spec(): `chunk_size` must be a positive integer >= 1.",
         call. = FALSE)

  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("parallel_spec(): `seed` must be logical or numeric.", call. = FALSE)

  if (!is.character(x$packages))
    stop("parallel_spec(): `packages` must be a character vector.", call. = FALSE)

  invisible(x)
}

# ======================================================================
# total_branches() — S3 generic
# ======================================================================

#' Total Number of Monte Carlo Branches
#'
#' @description
#' Returns the total number of Monte Carlo branches implied by an
#' execution_spec (serial or parallel).
#'
#' @param x An execution_spec.
#' @return Integer number of branches.
#' @export
total_branches <- function(x) {
  UseMethod("total_branches")
}

#' @export
total_branches.serial_spec <- function(x) x$R

#' @export
total_branches.parallel_spec <- function(x) x$num_workers * x$chunk_size

#' @export
total_branches.default <- function(x) {
  stop("total_branches() expects an execution_spec object.")
}

# ======================================================================
# PRINT METHODS (polished)
# ======================================================================

#' @export
print.serial_spec <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- Name:         ", x$name,     "\n", sep = "")
  cat("- # Branches:   ", total_branches(x), "\n", sep="")
  cat("- Seed:         ", x$seed,     "\n", sep = "")
  cat("- Packages:     ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.parallel_spec <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Name:         ", x$name,          "\n", sep = "")
  cat("- # Workers:    ", x$num_workers,   "\n", sep = "")
  cat("- Chunk size:   ", x$chunk_size,    "\n", sep = "")
  cat("- # Branches:   ", total_branches(x), "\n", sep="")
  cat("- Seed:         ", x$seed,          "\n", sep = "")
  cat("- Packages:     ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}
