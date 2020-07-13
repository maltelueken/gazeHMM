#' Methods for Additional Response Models
#'
#' Various methods for uniform, von Mises, and gamma response models to be used with \code{depmixS4}.
#'
#' @seealso \code{\link[depmixS4]{show}}.
#'
#' @include unif.R vMF.R gamma2.R
#' @name response-methods
NULL


# Uniform Response Model --------------------------------------------------

#' @rdname response-methods
#'
setMethod("show", "unif",
          function(object) {

            cat("Model of type uniform\n")
            cat("Parameters: \n")
            cat("min: ", object@parameters$min, "\n")
            cat("max: ", object@parameters$max, "\n")
          }
)


#' @rdname response-methods
#'
setMethod("dens", "unif",
          function(object, log = FALSE) {

            dens <- ifelse(is.na(object@y), 1, dunif(object@y, min = object@parameters$min, max = object@parameters$max, log = log))

          }
)


#' @rdname response-methods
#'
setMethod("getpars", "response",
          function(object, which = "pars", ...) {

            switch(which,
                   "pars" = {
                     parameters <- numeric()
                     parameters <- unlist(object@parameters)
                     pars <- parameters
                   },
                   "fixed" = {
                     pars <- object@fixed
                   }
            )
            return(pars)
          }
)


#' @rdname response-methods
#'
setMethod("setpars", "unif",
          function(object, values, which = "pars", ...) {

            npar <- npar(object)

            if(length(values) != npar) stop("length of 'values' must be", npar)

            nms <- names(object@parameters)

            switch(which,
                   "pars"= {
                     object@parameters$min <- values[1]
                     object@parameters$max <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   }
            )
            names(object@parameters) <- nms

            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("fit", "unif",
          function(object, w) {

            if(missing(w)) w <- NULL

            y <- object@y
            pars <- c(min(y[!is.na(y)]), max(y[!is.na(y)]))
            object <- setpars(object, pars)

            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("predict", "unif",
          function(object) {

            ret <- sample(object@y, 1)

            return(ret)
          }
)


#' @rdname response-methods
#'
setMethod("simulate", "unif",
          function(object, nsim = 1, seed) {

            if(!is.null(seed)) set.seed(seed)

            nt <- nrow(object@y)

            sim <- runif(n = nt*nsim, min = object@parameters$min, max = object@parameters$max)

            return(as.matrix(sim))
          }
)


# Von Mises Response Model ------------------------------------------------

#' @rdname response-methods
#'
setMethod("show", "vMF",
          function(object) {
            cat("Model of type vMF\n")
            cat("Parameters: \n")
            cat("mu: ", object@parameters$mu, "\n")
            cat("kappa: ", exp(object@parameters$kappa), "\n")
          }
)


#' @rdname response-methods
#'
setMethod("dens","vMF",
          function(object) {

            dens <- ifelse(is.na(object@y), 1, dvm(object@y, mu = object@parameters$mu, kappa = exp(object@parameters$kappa)))

          }
)


#' @rdname response-methods
#'
setMethod("getpars","response",
          function(object,which="pars",...) {

            switch(which,
                   "pars" = {
                     parameters <- numeric()
                     parameters <- unlist(object@parameters)
                     pars <- parameters
                   },
                   "fixed" = {
                     pars <- object@fixed
                   }
            )

            return(pars)
          }
)


#' @rdname response-methods
#'
setMethod("setpars","vMF",
          function(object, values, which="pars", ...) {

            npar <- npar(object)

            if(length(values)!=npar) stop("length of 'values' must be",npar)

            nms <- names(object@parameters)

            switch(which,
                   "pars"= {
                     object@parameters$mu <- values[1]
                     object@parameters$kappa <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   }
            )

            names(object@parameters) <- nms

            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("fit","vMF",
          function(object,w) {

            if(missing(w)) w <- NULL

            y <- object@y


            # Create weighted log-likelihood function for von-Mises distribution

            ll_vMF <- function(par, y, w) {

              if(is.null(w)) w <- 1 # no weights: all values weighted equally (= 1)

              miss <- is.na(y)

              dens <- ifelse(miss, 1, dvm(y, mu = par[1], kappa = exp(par[2])))

              return(-sum(w*log(dens)))
            }

            init <- c(object@parameters$mu, object@parameters$kappa) # start values


            # Optimize weighted ll for von-Mises distribution

            fit <- BB::BBoptim(par = init, fn = ll_vMF, y = y, w = w,
                               lower = c(-Inf, -Inf), upper = c(Inf, Inf), quiet = T)

            pars <- fit$par
            object <- setpars(object,pars)

            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("predict","vMF",
          function(object) {

            ret <- object@parameters$mu

            return(ret)
          }
)


#' @rdname response-methods
#'
setMethod("simulate", signature(object = "vMF"),
          function(object, nsim = 1, seed) {

            if(!is.null(seed)) set.seed(seed)

            nt <- nrow(object@y)

            sim <- rvm(nt*nsim, mean = object@parameters$mu, k = exp(object@parameters$kappa))

            return(as.matrix(sim))
          }
)


# Gamma Response Model ----------------------------------------------------

#' @rdname response-methods
#'
setMethod("show","gamma2",
          function(object) {
            cat("Model of type gamma2 (see ?gamma2mlss for details) \n")
            cat("Parameters: \n")
            cat("shape: ", exp(object@parameters$shape), "\n")
            cat("scale: ", exp(object@parameters$scale), "\n")
          }
)


#' @rdname response-methods
#'
setMethod("dens","gamma2",
          function(object,log=FALSE) {

            dens <- ifelse(is.na(object@y), 1, dgamma(object@y, shape = exp(object@parameters$shape),
                                                      scale = exp(object@parameters$scale),log = log))

          }
)


#' @rdname response-methods
#'
setMethod("getpars","response",
          function(object,which="pars",...) {
            switch(which,
                   "pars" = {
                     parameters <- numeric()
                     parameters <- unlist(object@parameters)
                     pars <- parameters
                   },
                   "fixed" = {
                     pars <- object@fixed
                   }
            )
            return(pars)
          }
)


#' @rdname response-methods
#'
setMethod("setpars","gamma2",
          function(object, values, which="pars", ...) {
            npar <- npar(object)
            if(length(values)!=npar) stop("length of 'values' must be",npar)
            # determine whether parameters or fixed constraints are being set
            nms <- names(object@parameters)
            switch(which,
                   "pars"= {
                     object@parameters$shape <- values[1]
                     object@parameters$scale <- values[2]
                   },
                   "fixed" = {
                     object@fixed <- as.logical(values)
                   }
            )
            names(object@parameters) <- nms
            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("fit","gamma2",
          function(object,w) {

            if(missing(w)) w <- NULL

            y <- object@y


            # Create weighted log-likelihood function for gamma distribution

            ll_gamma <- function(par, y, w) {

              if(is.null(w)) w <- 1 # no weights: all values weighted equally (= 1)

              # w[w == 0] <- 1e-5

              miss <- is.na(y)

              dens <- ifelse(miss, log(1), dgamma(y, shape = exp(par[1]), scale = exp(par[2]), log = T))

              return(-sum(w*dens))
            }


            init <- c(object@parameters$shape, object@parameters$scale) # start values


            # Optimize weighted ll for gamma distribution

            fit <- BB::BBoptim(par = init, fn = ll_gamma, y = y, w = w,
                               lower = c(-Inf, -Inf), upper = c(Inf, Inf), quiet = T)

            pars <- fit$par
            object <- setpars(object, pars)

            return(object)
          }
)


#' @rdname response-methods
#'
setMethod("predict","gamma2",
          function(object) {
            ret <- object@parameters$shape*object@parameters$scale
            return(ret)
          }
)


#' @rdname response-methods
#'
setMethod("simulate", signature(object = "gamma2"),
          function(object, nsim = 1, seed) {

            if(!is.null(seed)) set.seed(seed)

            nt <- nrow(object@y)

            sim <- rgamma(nt*nsim, shape = exp(object@parameters$shape), scale = exp(object@parameters$scale))

            return(as.matrix(sim))
          }
)
