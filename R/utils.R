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
#' @param x numerical vector
#'
#' @return
#' numeric
#' @export
#'
MODE <- function(x) {
  cv <- CV(x)
  m1 <- round(stats::median(x) / (cv * cv + 1), digits = 0)
  m2 <- as.numeric(names(which.max(table(x))))
  res <- c(m1, m2)
  names(res) <- c("Mode1", "Mode2")
  return(res)
}

