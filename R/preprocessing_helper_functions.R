#' Convert Gaze Coordinates
#'
#' Converts raw gaze coordinates from pixels into degrees of visual angle.
#'
#' @param x Numeric vector of raw coordinates for each gaze sample.
#' @param res Screen resolution (in px).
#' @param dim Screen dimensions (in mm).
#' @param dist Distance between subject and screen (in mm).
#'
#' @return A numeric vector with coordiantes in degrees of visual angle centered at (0,0) for each gaze sample.
#' @export
px2va <- function(x, res, dim, dist) {

  x_centered <- x - res/2 # transform to centered coordinate system (0,0)

  radian <- atan(x_centered/(dist*(res/dim))) # visual angle in radians

  degree <- radian*(180/pi) # visual angle in degrees

  return(degree)
}


# Function to compute the relative angle between subsequent samples

#' Calculate Sample-to-sample Angle
#'
#' Calculates the relative angle between subsequent gaze samples.
#' Uses the backward difference to compute the absolute angle between two samples and
#' the forward difference for the change in angle at each sample.
#'
#' @param x Numeric vector of x coordinates for each gaze sample.
#' @param y Numeric vector of y coordinates for each gaze sample.
#'
#' @return A vector containing the sample-to-sample angle (in radians) for each gaze sample.
#' @importFrom dplyr lead lag
#' @export
calc_theta <- function(x, y) { # x-pos, y-pos

  diff_x <- x - lag(x) # x_t - x_t-1
  diff_y <- y - lag(y) # y_t - y_t-1

  angle <- atan2(diff_y, diff_x) # absolute angle of vector xy_t - xy_t-1
  theta <- lead(angle) - angle # relative angle of two vectors at sample t
  theta.mirrored <- ifelse(theta < 0, theta + 2*pi, theta) # mirror negative relative angles to positive range

  return(theta.mirrored)
}
