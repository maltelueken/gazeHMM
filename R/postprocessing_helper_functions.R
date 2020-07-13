#' Retrieve Last State
#'
#' Retrieves the last different state in a sequence of estimated states.
#' Used as an internal function in \code{\link{postprocess}}.
#'
#' @param state Vector with estimated states for each gaze sample.
#' @param s Index of the current gaze sample in the state sequence.
#'
#' @return A single element indicating the label of the last different state.
last_state <- function(state, s) {
  if(state[s-1] != state[s] || (s-1) == 1) {

    return(state[s-1])
  } else {

    return(last_state(state, s-1))
  }
}


#' Relabel State Sequence
#'
#' Relabels the state sequence estimated by a hidden Markov model with a recursive algorithm.
#' Used as an internal function in \code{\link{postprocess}}.
#'
#' @param state Integer vector with estimated states for each gaze sample as
#' returned by \code{\link{HMM_classify}}.
#' @param t Numeric vector of time stamps for each gaze sample.
#' @param min.sac Minimum saccade duration (in s).
#'
#' @return A data frame with the relabeled states and event number for each gaze sample.
reclassify <- function(state, t, min.sac) {

  # Assign labels to events

  FIX <- 1
  SAC <- 2
  PSO <- 3
  SP <- 4


  # Relabel single sample fixations and smooth pursuit

  number <- integer(length(state))

  number[1] <- 1

  counter <- 1

  N <- length(state)

  for (s in 2:(N-1)) {

    if(state[s] %in% c(FIX, SP) && state[s-1] != state[s] && state[s+1] != state[s]) {

      state[s] <- state[s-1]

    }


    # Count consecutive samples with same label (events)

    if(state[s] != state[s-1]) counter <- counter + 1

    number[s] <- counter
  }

  number[length(number)] <- max(number)


  # Relabel saccades with duration below threshold

  dur <- numeric(max(number))
  event <- integer(max(number))

  for (n in unique(number)) {

    if(n > 1) {
      dur[n] <- max(t[number == n], na.rm = T) - max(t[number == (n-1)], na.rm = T)
    } else {
      dur[n] <- max(t[number == n], na.rm = T) - min(t[number == n], na.rm = T)
    }
    event[n] <- max(state[number == n])

    if(event[n] == SAC && dur[n] < min.sac && n > 1) {

      state[number == n] <- last_state(event, n)

    }
  }


  # Relabel PSOs that do not occur immediately after saccade

  for (s in 2:(N-1)) {
    if(state[s] == PSO && state[s+1] == SAC) {

      state[s] <- state[s+1]

    } else if (state[s] == PSO && state[s-1] %in% c(FIX, SP) && state[s+1] != SAC) {

      state[s] <- state[s-1]

    }
  }


  # Check whether all relabeling conditions are met

  noSingleSamples <- T
  allPSOsafterSaccades <- T
  minDuration <- T

  number <- integer(length(state))
  number[1] <- 1
  counter <- 1

  for (s in 2:(N-1)) {
    if(state[s] %in% c(FIX, SP ) && state[s-1] != state[s] && state[s+1] != state[s]) {

      noSingleSamples <- F

    } else if(state[s] == PSO && state[s+1] == SAC || state[s] == PSO && state[s-1] %in% c(FIX, SP) && state[s+1] != SAC) {

      allPSOsafterSaccades <- F

    }

    if(state[s] != state[s-1]) counter <- counter + 1

    number[s] <- counter
  }

  number[length(number)] <- max(number)

  dur <- numeric(max(number))
  event <- integer(max(number))

  for (n in unique(number)) {

    if(n > 1) {
      dur[n] <- max(t[number == n], na.rm = T) - max(t[number == (n-1)], na.rm = T)
    } else {
      dur[n] <- max(t[number == n], na.rm = T) - min(t[number == n], na.rm = T)
    }
    event[n] <- max(state[number == n])

    if(event[n] == SAC && dur[n] < min.sac && n > 1) {

      minDuration <- F

    }
  }


  # Return output if conditions are met, otherwise repeat relabeling

  output <- data.frame(label = state, number = number)

  if(all(noSingleSamples, allPSOsafterSaccades, minDuration)) {

    return(output)

  } else {

    return(reclassify(state, t, min.sac))

  }
}
