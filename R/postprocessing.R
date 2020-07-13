#' Postprocess Gaze Data Classification
#'
#' Applies a postprocessing routine to classified gaze data as part of \code{\link{gazeHMM}}.
#' Relabels samples that violate constraints and computes eye movement event metrics.
#'
#' @param data Data frame with preprocessed gaze samples as returned by \code{\link{preprocess}}.
#' The object must contain the variables \code{t}, \code{vel}, \code{acc}, and \code{angle}.
#' @param state Vector of state labels for each sample as returned by \code{\link{HMM_classify}}.
#' @param fr Sampling rate of the eye-tracker (in Hz).
#' @param min.sac Minimum saccade duration (in s).
#'
#' @return A list with two objects: The preprocessed data with postprocessed sample labels and
#' a list with data frames for each classified event containing the event metrics (e.g., fixation duration).
#' @export
postprocess <- function(data, state, fr, min.sac = 0.01) {

  # Assign labels to events

  FIX <- 1
  SAC <- 2
  PSO <- 3
  SP <- 4


  # Mark invalid samples

  valid <- !is.na(data$vel) & !is.na(data$acc) & !is.na(data$angle)

  state[!valid] <- 0


  # Relabel samples

  df <- reclassify(state, data$t, min.sac)

  label <- df$label
  number <- df$number

  warning(paste(sum(label != state), " samples were relabeled during postprocessing!"))


  # Multiply acceleration by sampling rate

  data$acc <- data$acc * fr


  # Initialize output event metrics

  dur <- numeric(max(number))
  event <- integer(max(number))

  x <- numeric(max(number))
  y <- numeric(max(number))

  start.x <- numeric(max(number))
  end.x <- numeric(max(number))
  start.y <- numeric(max(number))
  end.y <- numeric(max(number))

  amp <- numeric(max(number))

  max.vel <- numeric(max(number))
  avg.vel <- numeric(max(number))
  max.acc <- numeric(max(number))
  avg.acc <- numeric(max(number))

  dir <- numeric(max(number))


  # Calculate output event metrics

  for (n in unique(number)) {

    # Duration

    if(n > 1) {
      dur[n] <- max(data$t[number == n], na.rm = T) - max(data$t[number == (n-1)], na.rm = T)
    } else {
      dur[n] <- max(data$t[number == n], na.rm = T) - min(data$t[number == n], na.rm = T)
    }
    event[n] <- max(label[number == n])

    if(event[n] == FIX) {

      # Fixation position

      x[n] <- ifelse(sum(valid[number == n]) == 0, NA, mean(data$x[number == n & valid], trim = 0.2, na.rm = T))
      y[n] <- ifelse(sum(valid[number == n]) == 0, NA, mean(data$y[number == n & valid], trim = 0.2, na.rm = T))

    } else {

      # Start and end position

      start.x[n] <- ifelse(sum(valid[number == n]) == 0, NA, data$x[number == n & valid][1])
      end.x[n] <- ifelse(sum(valid[number == n]) == 0, NA, data$x[number == n & valid][length(data$x[number == n & valid])])
      start.y[n] <- ifelse(sum(valid[number == n]) == 0, NA, data$y[number == n & valid][1])
      end.y[n] <- ifelse(sum(valid[number == n]) == 0, NA, data$y[number == n & valid][length(data$y[number == n & valid])])

      dx <- end.x[n] - start.x[n]
      dy <- end.y[n] - start.y[n]


      # Amplitude

      amp[n] <- ifelse(sum(valid[number == n]) == 0, NA, sqrt(dx^2 + dy^2))


      # Velocity and acceleration

      max.vel[n] <- ifelse(sum(valid[number == n]) == 0, NA, max(data$vel[number == n], na.rm = T))
      avg.vel[n] <- ifelse(sum(valid[number == n]) == 0, NA, mean(data$vel[number == n], na.rm = T))
      max.acc[n] <- ifelse(sum(valid[number == n]) == 0, NA, max(data$acc[number == n], na.rm = T))
      avg.acc[n] <- ifelse(sum(valid[number == n]) == 0, NA, mean(data$acc[number == n], na.rm = T))


      # Direction

      dir[n] <- ifelse(sum(valid[number == n]) == 0, NA, atan2(dy, dx))

    }
  }

  data$state <- state
  data$label <- label
  data$event <- number


  # Prepare output

  fixations <- data.frame(cbind(x, y, dur))[event == FIX,]
  saccades <- data.frame(cbind(dur, amp, max.vel, max.acc, avg.vel, avg.acc, dir, start.x, start.y, end.x, end.y))[event == SAC,]
  psos <- data.frame(cbind(dur, amp, max.vel, max.acc, avg.vel, avg.acc, start.x, start.y, end.x, end.y))[event == PSO,]
  sps <- data.frame(cbind(dur, amp, max.vel, max.acc, avg.vel, avg.acc, dir, start.x, start.y, end.x, end.y))[event == SP,]

  output <- list(samples = as.data.frame(data),
                 events = list(fixations = fixations, saccades = saccades, PSOs = psos, smooth.pursuits = sps))

  return(output)
}
