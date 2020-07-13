#' Classify Gaze Data
#'
#' Classifies gaze data into eye movement events by using a hidden Markov model from the package depmixS4.
#'
#' @param data Data frame with preprocessed gaze samples as returned by \code{\link{preprocess}}.
#' The object must contain the variables \code{vel}, \code{acc}, and \code{angle}.
#' @param nstates Number of states in the hidden Markov model.
#' @param respstart Starting values for the response model in the hidden Markov model.
#' @param trstart Starting values for the transition model in the hidden Markov model.
#' @param instart Starting values for the initial state model in the hidden Markov model.
#' @param sf Vector of length two indicating by which factor velocity and acceleration data will be divided.
#' @param fit.control List of settings for the EM algorithm as returned by \code{\link[depmixS4]{em.control}}.
#'
#' @return An object of class \code{\link[depmixS4]{depmix.fitted}}.
#' @import depmixS4
#' @export
HMM_classify <- function(data, nstates, respstart, trstart, instart,
                         sf = c(10, 10),
                         fit.control = em.control(maxit = 5000, random.start = F)) {

  # Downsample velocity and acceleration data

  if(all(sf > 0)) {

    data$vel <- data$vel/sf[1]
    data$acc <- data$acc/sf[2]

  }


  # Create response model

  resp <- list(list(gamma2(data$vel, pstart = respstart[[1]][[1]]),
                    gamma2(data$acc, pstart = respstart[[1]][[2]]),
                    unif(data$angle)))

  for (s in 2:nstates) {

    resp[[s]] <- list(gamma2(data$vel, pstart = respstart[[s]][[1]]),
                      gamma2(data$acc, pstart = respstart[[s]][[2]]),
                      vMF(data$angle, pstart = respstart[[s]][[3]]))

  }


  # Create state transition model

  trans <- list()

  for (s in 1:nstates) {

    trans[[s]] <- transInit(~ 1, nstates = nstates, data = data, pstart = c(trstart[s,]))

  }


  # Create initial state model

  init <- transInit(~ 1, nstates = nstates, pstart = instart, family = multinomial("identity"), data = data.frame(1))


  # Combine models

  model <- makeDepmix(response = resp, transition = trans, prior = init, homogeneous = F)


  # Fit model

  time <- Sys.time()

  model.fit <- fit(model, emcontrol = fit.control)

  cat("Computation time: ", Sys.time() - time, "\n")


  return(model.fit)
}
