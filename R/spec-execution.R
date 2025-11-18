# ======================================================================
# Execution Mode Specifications (Serial / Parallel)
# ======================================================================

#' Serial Execution Specification
#'
#' @description
#' Declares that branch computation will run in serial using a single R process.
#' Still uses foreach + %dofuture%, but no parallel workers are spawned.
#'
#' @param R Integer, required. Number of Monte Carlo branches to compute.
#' @param seed Logical or integer. If TRUE (default), reproducible RNG is enabled.
#' @param packages Character vector of package names required during computation.
#' @param name Optional label, defaults to "serial".
#'
#' @return An execution-spec object of class `likelihood_execution_serial`.
#' @export
serial_spec <- function(R,
                        seed = TRUE,
                        packages = character(),
                        name = "serial") {

  # ---- validation ----
  if (missing(R) || !is.numeric(R) || R < 1)
    stop("For serial execution, `R` must be a positive integer.", call. = FALSE)

  x <- list(
    name     = name,
    mode     = "serial",
    R        = as.integer(R),
    seed     = seed,
    packages = packages
  )

  class(x) <- c("likelihood_execution_serial", "likelihood_execution")
  x
}


#' Parallel Execution Specification (foreach + %dofuture% backend)
#'
#' @description
#' Declares that branch computation should run in parallel via foreach with
#' the `%dofuture%` operator. The user **must** set a future plan beforehand,
#' e.g. `future::plan(multisession)` or `future::plan(cluster, workers = 8)`.
#'
#' @param num_workers Integer. Number of workers (e.g. CPU cores, cluster nodes).
#' @param chunk_size Integer. Workload chunk size per worker, default = 1.
#' @param seed Logical or integer controlling reproducible RNG. Default = TRUE.
#' @param packages Character vector of package names required on workers.
#' @param name Optional label, defaults to "parallel".
#'
#' @return An execution-spec object of class `likelihood_execution_parallel`.
#' @export
parallel_spec <- function(num_workers,
                          chunk_size = 1L,
                          seed = TRUE,
                          packages = character(),
                          name = "parallel") {

  # ---- validation ----
  if (missing(num_workers) || !is.numeric(num_workers) || num_workers < 1)
    stop("`num_workers` must be a positive integer.", call. = FALSE)

  if (!is.numeric(chunk_size) || chunk_size < 1)
    stop("`chunk_size` must be >= 1.", call. = FALSE)

  x <- list(
    name        = name,
    mode        = "parallel",
    num_workers = as.integer(num_workers),
    chunk_size  = as.integer(chunk_size),
    seed        = seed,
    packages    = packages
  )

  class(x) <- c("likelihood_execution_parallel", "likelihood_execution")
  x
}


# ======================================================================
# Print Methods
# ======================================================================

#' @export
print.likelihood_execution_serial <- function(x, ...) {
  cat("# Execution Mode: SERIAL\n")
  cat("- R (branches): ", x$R, "\n", sep = "")
  cat("- Seed:         ", x$seed, "\n", sep = "")
  cat("- Packages:     ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.likelihood_execution_parallel <- function(x, ...) {
  cat("# Execution Mode: PARALLEL (foreach + %dofuture%)\n")
  cat("- Workers:   ", x$num_workers, "\n", sep = "")
  cat("- Chunk size:", x$chunk_size, "\n", sep = "")
  cat("- Seed:      ", x$seed, "\n", sep = "")
  cat("- Packages:  ", paste(x$packages, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @keywords internal
compute_num_branches <- function(exec) {
  if (inherits(exec, "likelihood_execution_serial"))
    return(exec$R)

  if (inherits(exec, "likelihood_execution_parallel"))
    return(exec$num_workers * exec$chunk_size)

  stop("Unknown execution specification.", call. = FALSE)
}

