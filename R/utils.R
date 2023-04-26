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
  uniqv <- unique(x)
  m3 <- uniqv[which.max(tabulate(match(x, uniqv)))]
  res <- c(m1, m2, m3)
  names(res) <- c("Mode1", "Mode2", "Mode3")
  return(res)
}

