# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# ======================================================================

#' Serial Execution Specification
#'
#' @description
#' Declares that Monte Carlo branch computations should run **in serial**
#' using a single R process. This mode still uses `foreach` internally,
#' but no parallel workers are spawned.
#'
#' @param R Positive integer. Number of Monte Carlo branches.
#' @param seed Logical or integer. Controls reproducible RNG.
#' @param packages Character vector of packages needed during evaluation.
#' @param name Optional label.
#'
#' @return An object of class `likelihood_execution_serial`.
#' @export
serial_spec <- function(R,
                        seed = TRUE,
                        packages = character(),
                        name = "serial") {

  x <- list(
    name     = name,
    mode     = "serial",
    R        = R,
    seed     = seed,
    packages = packages
  )

  class(x) <- c("likelihood_execution_serial", "likelihood_execution")
  .validate_execution_serial(x)
  x
}


# ======================================================================
# Parallel Execution Specification
# ======================================================================

#' Parallel Execution Specification (foreach + %dofuture%)
#'
#' @description
#' Declares that Monte Carlo branches should run **in parallel**, where
#' users must configure a `future` backend before calling `generate()`.
#'
#' Total branches is:
#'
#' \deqn{
#' R = \text{num\_workers} \times \text{chunk\_size}.
#' }
#'
#' @param num_workers Positive integer. Number of workers.
#' @param chunk_size Positive integer. Batch size per worker.
#' @param seed Logical or integer. RNG configuration.
#' @param packages Character vector.
#' @param name Optional label.
#'
#' @return An object of class `likelihood_execution_parallel`.
#' @export
parallel_spec <- function(num_workers,
                          chunk_size = 1L,
                          seed = TRUE,
                          packages = character(),
                          name = "parallel") {

  x <- list(
    name        = name,
    mode        = "parallel",
    num_workers = num_workers,
    chunk_size  = chunk_size,
    seed        = seed,
    packages    = packages
  )

  class(x) <- c("likelihood_execution_parallel", "likelihood_execution")
  .validate_execution_parallel(x)
  x
}


# ======================================================================
# INTERNAL VALIDATORS
# ======================================================================

#' @keywords internal
.validate_execution_serial <- function(x) {

  if (is.null(x$R) || !is.numeric(x$R) || length(x$R) != 1 || x$R < 1)
    stop("`R` must be a positive integer for serial_spec().", call. = FALSE)

  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("`seed` must be logical or numeric.", call. = FALSE)

  if (!is.character(x$packages))
    stop("`packages` must be a character vector.", call. = FALSE)

  invisible(x)
}

#' @keywords internal
.validate_execution_parallel <- function(x) {

  if (is.null(x$num_workers) ||
      !is.numeric(x$num_workers) ||
      length(x$num_workers) != 1 ||
      x$num_workers < 1)
    stop("`num_workers` must be a positive integer.", call. = FALSE)

  if (is.null(x$chunk_size) ||
      !is.numeric(x$chunk_size) ||
      length(x$chunk_size) != 1 ||
      x$chunk_size < 1)
    stop("`chunk_size` must be a positive integer.", call. = FALSE)

  if (!is.logical(x$seed) && !is.numeric(x$seed))
    stop("`seed` must be logical or numeric.", call. = FALSE)

  if (!is.character(x$packages))
    stop("`packages` must be a character vector.", call. = FALSE)

  invisible(x)
}


# ======================================================================
# PRINT METHODS
# ======================================================================

#' @export
print.likelihood_execution_serial <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- Branches (R): ", x$R, "\n", sep = "")
  cat("- Seed:         ", x$seed, "\n", sep = "")
  cat("- Packages:     ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.likelihood_execution_parallel <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Workers:    ", x$num_workers, "\n", sep = "")
  cat("- Chunk size: ", x$chunk_size, "\n", sep = "")
  cat("- Seed:       ", x$seed, "\n", sep = "")
  cat("- Packages:   ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}



