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
#' numerical vector with the mode of x calculated in four different ways.
#' Mode1 is stats::median(x) / (cv * cv + 1) as recommended by Atso but this
#' method seems to give to low values if the cv is high. Mode4 is calculated with
#' the density() function and is probably the one we should use on the posterior
#' smolt estimate.
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
  m2 <- as.numeric(names(which.max(table(x))))
  uniqv <- unique(x)
  m3 <- uniqv[which.max(tabulate(match(x, uniqv)))]
  d <- density(x)
  m4 <- d$x[which.max(d$y)]
  res <- c(m1, m2, m3, m4)
  names(res) <- c("Mode1", "Mode2", "Mode3", "Mode4")
  return(res)
}

