#' Uniform Response Model for depmixS4
#'
#' Creates a uniform response model to be used in \code{\link[depmixS4]{makeDepmix}}
#' as part of \code{\link{HMM_classify}}.
#'
#' @param y Numeric vector containing a response variable.
#'
#' @return An S4 object of class 'unif'.
#' @slot parameters List of length two containing the minimum and maximum parameters
#' of the uniform distribution.
#' @slot fixed Logical vector indicating which parameters are fixed.
#' @slot y Response variable.
#' @slot x Covariate variables (or just an intercept).
#' @slot npar Number of parameters.
#' @slot constr Vector defining the parameter constraints.
#'
#' @name unif
#' @importFrom methods new
#' @importFrom stats dunif runif
NULL

#' @rdname unif
#'
setClass("unif", contains = "response")


#' @rdname unif
#'
setGeneric("unif", function(y, ...) standardGeneric("unif"))


#' @rdname unif
#'
setMethod("unif",
          signature(y = "ANY"),
          function(y, fixed = NULL, ...) {

            y <- matrix(y, length(y))
            x <- matrix(1)
            parameters <- list()
            npar <- 2 # min and max

            if(is.null(fixed)) fixed <- as.logical(rep(0,npar))

            parameters$min <- min(y)
            parameters$max <- max(y)

            mod <- new("unif", parameters = parameters, fixed = fixed, x = x, y = y, npar = npar)

            return(mod)
          }
)
