#' Generate Gamma Distribution Starting Values
#'
#' Generates starting values from a gamma distribution to estimate a parameter. The mode of the distribution
#' is on the parameter value.
#'
#' @param x A single numeric parameter value.
#' @param alpha Shape parameter of the gamma distribution.
#' @param seed Seed of the random number generation (for reproducibility).
#'
#' @return A single numeric starting value for the parameter.
#' @export
gamma_start <- function(x, alpha = 3, seed) {

  if(!missing(seed)) set.seed(seed)

  start <- rgamma(1, shape = alpha, scale = x/(alpha - 1))

  return(start)
}
