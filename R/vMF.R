#' Von Mises Response Model for depmixS4
#'
#' Creates a von Mises response model to be used in \code{\link[depmixS4]{makeDepmix}}
#' as part of \code{\link{HMM_classify}}.
#'
#' @param y Numeric vector containing a response variable.
#'
#' @return An S4 object of class 'vMF'.
#' @slot parameters List of length two containing the mean and concentration parameters
#' of the von Mises distribution.
#' @slot fixed Logical vector indicating which parameters are fixed.
#' @slot y Response variable.
#' @slot x Covariate variables (or just an intercept).
#' @slot npar Number of parameters.
#' @slot constr Vector defining the parameter constraints.
#'
#' @name vMF
#' @importFrom methods new
#' @importFrom CircStats dvm rvm
NULL


#' @rdname vMF
#'
setClass("vMF", contains = "response")


#' @rdname vMF
#'
setGeneric("vMF", function(y, pstart = NULL, fixed = NULL, ...) standardGeneric("vMF"))


#' @rdname vMF
#'
setMethod("vMF",
          signature(y = "ANY"),
          function(y, pstart=NULL, fixed=NULL, ...) {

            y <- matrix(y, length(y))
            x <- matrix(1)
            parameters <- list()
            npar <- 2 # mu and kappa

            if(is.null(fixed)) fixed <- as.logical(rep(0, npar))

            if(!is.null(pstart)) {

              if(length(pstart) != npar) stop("length of 'pstart' must be ", npar)

              parameters$mu <- pstart[1]
              parameters$kappa <- log(pstart[2])
            }

            mod <- new("vMF", parameters = parameters, fixed = fixed, x = x, y = y, npar = npar)

            return(mod)
          }
)
