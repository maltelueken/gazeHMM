#' Preprocess Gaze Data
#'
#' Prepares raw gaze data for classification by a hidden Markov model as part of \code{\link{gazeHMM}}.
#'
#' @param x Numeric vector of raw x coordinates for each gaze samples.
#' @param y Numeric vector of raw y coordinates for each gaze samples.
#' @param t Numeric vector of time stamps for each gaze sample.
#' @param unit Character string indicating the unit of coordinates (either 'px' or 'va').
#' @param res Screen resolution (in px).
#' @param dim Screen dimensions (in mm).
#' @param dist Distance between subject and screen (in mm).
#' @param fr Sampling rate of the eye-tracker (in Hz).
#' @param blink Either a numeric vector of length two indicating the x and y coordinates for blink samples
#' or a logical vector with the same length as \code{x}, \code{y}, and \code{t} indicating blink samples.
#' @param b.win Time window around blink samples that are to be ignored (i.e., set to NA; in s).
#' @param sg.order Order of the Savitzky-Golay filter.
#' @param sg.length Length of the Savitzky-Golay filter (must be odd).
#'
#' @return A data frame with x and y gaze coordinates in degrees of visual angle, time stamps,
#' velocity, accelelration, sample-to-sample angle, and an initial label for each sample.
#' @importFrom signal sgolayfilt
#' @export
preprocess <- function(x, y, t, unit = "px", res, dim, dist, fr, blink = NULL, b.win = 0.05,
                       sg.order = 3, sg.length = sg.order + 3 - sg.order%%2) {

  # Initialize sample metrics

  vel <- rep(NA, length(x))
  acc <- rep(NA, length(x))
  angle <- rep(NA, length(x))
  label <- rep(NA, length(x)) # for blinks


  # Check for NA and Inf and (0,0)

  valid <- ifelse(is.na(x) | is.na(y) |
                    is.infinite(x) | is.infinite(y) |
                    (x == 0 & y == 0), F, T)


  # Initial label blinks

  inib <- rep(NA, length(x))

  if(is.numeric(blink) && length(blink) == 2) {

    inib[x == blink[1] & y == blink[2]] <- 0

  } else if(is.logical(blink) && length(blink) == length(t)) {

    inib[blink] <- 0

  }


  # Convert pixels to degrees of visual angle

  res.va <- c(px2va(res[1], res = res[1], dim = dim[1], dist = dist),
              px2va(res[2], res = res[2], dim = dim[2], dist = dist))

  if(unit == "px") {

    x.va <- px2va(x, res = res[1], dim = dim[1], dist = dist)
    y.va <- px2va(y, res = res[2], dim = dim[2], dist = dist)

  } else {

    x.va <- x
    y.va <- y

  }


  # Check if samples are on screen

  valid[valid] <- ifelse(x.va[valid] < -res.va[1] | x.va[valid] > res.va[1] |
                    y.va[valid] < -res.va[2] | y.va[valid] > res.va[2], F, valid[valid])


  # Calculate velocity and acceleration

  x.vel <- sgolayfilt(x.va[valid], m = 1, p = sg.order, n = sg.length)
  y.vel <- sgolayfilt(y.va[valid], m = 1, p = sg.order, n = sg.length)
  vel[valid] <- sqrt(x.vel^2 + y.vel^2) * fr

  x.acc <- sgolayfilt(x.va[valid], m = 2, p = sg.order, n = sg.length)
  y.acc <- sgolayfilt(y.va[valid], m = 2, p = sg.order, n = sg.length)
  acc[valid] <- sqrt(x.acc^2 + y.acc^2) * fr

  valid[valid] <- ifelse(is.na(vel[valid]) | is.na(acc[valid]), F, valid[valid])


  # Nudge zero velocities and accelerations

  nudge <- 0.01

  vel <- ifelse(vel == 0, nudge, vel)
  acc <- ifelse(acc == 0, nudge, acc)


  # Set +/- 50 ms around blinks to invalid

  for(i in 1:length(inib)) {
    if(!is.na(inib[i])) {

      valid[t > (t[i]-b.win) & t <= (t[i]+b.win)] <- F
      label[t > (t[i]-b.win) & t <= (t[i]+b.win)] <- 0

    }
  }


  # Remove outliers

  max.vel <- 1e3
  max.acc <- 1e2

  valid[valid] <- ifelse(vel[valid] > max.vel | acc[valid] > max.acc, F, valid[valid])


  # Calculate angle

  angle[valid] <- calc_theta(x.va[valid], y.va[valid])

  valid[valid] <- ifelse(is.na(angle[valid]), F, valid[valid])


  # Print number of excluded frames

  cat(paste(sum(!valid), "samples were labeled as 'invalid' during preprocessing!"))


  # Set invalid samples to NA (if not already NA)

  vel[!valid] <- NA
  acc[!valid] <- NA
  angle[!valid] <- NA


  # Combine metrics in output data frame

  output <- data.frame(frame = 1:length(x), x = x.va, y = y.va, t = t, vel, acc, angle, label) # [valid,]

  return(output)
}
