#' Calculate  coefficient af variation
#'
#' @param x nunerical vector
#'
#' @return
#' @export
#'
#' @examples
CV <- function(x){
  (sd(x)/mean(x))
}


#' Calculate the mode
#'
#' @param x numerical vector
#'
#' @return
#' @export
#'
#' @examples
MODE <- function(x) {
  cv <- CV(x)
  m1 <- round(median(x) / (cv * cv + 1), digits = 0)
  m2 <- as.numeric(names(which.max(table(x))))
  res <- c(m1, m2)
  names(res) <- c("Mode1", "Mode2")
  return(res)
}

