#' Nuisance parameter specification
#'
#' @description Defines how nuisance parameter proposals are drawn and then
#' projected into the constraint set ψ(θ) = ψ̂.
#'
#' @param sampler Function(n) → proposal vector.
#' @param init_strategy String: "random", "user", "from_model", "custom".
#' @param name Optional label.
#' @param ... Extra fields.
#'
#' @return S3 object of class `likelihood_nuisance`.
#' @export
nuisance_spec <- function(init_guess_sampler,
                          name = NULL,
                          ...) {

  x <- list(
    name                 = name %||% "<nuisance>",
    init_guess_sampler   = init_guess_sampler,
    extra                = list(...)
  )
  class(x) <- "likelihood_nuisance"
  validate_nuisance_spec(x)
  x
}

validate_nuisance_spec <- function(x) {
  if (!is.function(x$init_guess_sampler))
    stop("`init_guess_sampler` must be a function.", call. = FALSE)

  invisible(x)
}

#' @export
print.likelihood_nuisance <- function(x, ...) {
  cat("# Nuisance Specification\n")
  cat("- Name:                ", x$name, "\n", sep = "")
  invisible(x)
}
