#' Calculate  coefficient of variation
#'
#' @param x nunerical vector
#'
#' @return
#' numeric
#' @export
#'
CV <- function(x){
  (stats::sd(x)/mean(x))
}


#' Calculate the mode
#'
#' Calculate the mode of a numerical vector. The function will return a named
#' numerical vector with the mode of x calculated in three different ways.
#' Mode1 is stats::median(x) / (cv * cv + 1) as recommended by Atso but this
#' method seems to give to low values if the cv is high. Mode2 is found by
#' tabulating the data and returning the one with highest number of occurrences.
#' Mode3 is calculated with the density() function and is probably the one we
#  should use on the posterior smolt estimate.
#'
#'
#' @param x numerical vector
#'
#' @return
#' a named numerical vector with mode estimates.
#' @export
#'
MODE <- function(x) {
  cv <- CV(x)
  m1 <- stats::median(x) / (cv * cv + 1)
  uniqv <- unique(x)
  m2 <- uniqv[which.max(tabulate(match(x, uniqv)))]
  d <- density(x)
  m3 <- d$x[which.max(d$y)]
  res <- c(m1, m2, m3)
  names(res) <- c("Mode1", "Mode2", "Mode3")
  return(res)
}

