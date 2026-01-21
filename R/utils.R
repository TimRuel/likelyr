#' Null-coalescing operator
#'
#' @description
#' Returns \code{y} if \code{x} is \code{NULL}, otherwise returns \code{x}.
#' This is a small convenience helper used throughout the codebase
#' to provide default values.
#'
#' @param x Primary value.
#' @param y Fallback value used when \code{x} is \code{NULL}.
#'
#' @return Either \code{x} or \code{y}.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Bundle a function with its helper dependencies
#'
#' @description
#' Recursively injects all non-base global dependencies of a function into
#' the function's own environment, so it can be serialized and run on a
#' parallel worker without relying on \code{.GlobalEnv}.
#'
#' @param fun A function.
#' @param cache An environment used internally to avoid rebundling
#'   the same function multiple times.
#'
#' @return A function with a self-contained environment.
#'
#' @keywords internal
#' @noRd
.bundle_fun_env <- function(fun, seen = new.env(parent = emptyenv())) {
  stopifnot(is.function(fun))

  # prevent infinite loops
  key <- paste0("f_", digest::digest(fun))
  if (exists(key, seen, inherits = FALSE)) {
    return(fun)
  }
  assign(key, TRUE, seen)

  globs <- future::getGlobalsAndPackages(
    expr = fun,
    envir = environment(fun),
    tweak = TRUE
  )$globals

  keep <- !vapply(
    globs,
    function(x) {
      is.function(x) && identical(environment(x), baseenv())
    },
    logical(1)
  )
  globs <- unclass(globs[keep])

  # create new env inheriting from original
  env_new <- new.env(parent = environment(fun))

  for (nm in names(globs)) {
    obj <- globs[[nm]]

    # recurse ONLY to inject deps, never wrap
    if (is.function(obj)) {
      obj <- .bundle_fun_env(obj, seen)
    }

    env_new[[nm]] <- obj
  }

  environment(fun) <- env_new
  fun
}

#' Bind data to a user function and bundle helper dependencies
#'
#' @description
#' Produces a \code{(param, omega_hat)} closure by binding \code{data} into
#' the call to a user function \code{fun(param, omega_hat, data)} and
#' recursively bundling its helper dependencies so it is safe to ship to
#' future workers.
#'
#' @param fun A function with signature \code{(param, omega_hat, data)}.
#' @param data Data object to bind.
#'
#' @return A function with signature \code{(param, omega_hat)}.
#'
#' @keywords internal
#' @noRd
.bind_data_env <- function(fun, data) {
  # bundle full helper graph
  fun <- .bundle_fun_env(fun)

  f <- fun
  d <- data

  wrapper <- function(param, omega_hat) {
    f(param, omega_hat, d)
  }

  env_wrap <- new.env(parent = baseenv())
  env_wrap$f <- f
  env_wrap$d <- d
  environment(wrapper) <- env_wrap

  wrapper
}
