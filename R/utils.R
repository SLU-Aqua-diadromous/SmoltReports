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
  d <- stats::density(x)
  m3 <- d$x[which.max(d$y)]
  res <- c(m1, m2, m3)
  names(res) <- c("Mode1", "Mode2", "Mode3")
  return(res)
}

#' Selects strings that match one on several prefixes
#'
#' Match strings for the prefixes using base::startsWith(). Return a logical
#' vector that indicate all matches.
#'
#' @param strings character vector to match
#' @param prefixes character vector with prefixes
#'
#' @return
#' A logical vector with the same length as `strings`
#' @export
#'
#' @examples
#' x1 <- c("Foobar", "bla bla", "something", "another", "blu", "brown", "blau blüht der Enzian")
#' p <- c("Foo", "bl")
#' startsWith_multiple(x1, p)
#'
startsWith_multiple <- function(strings, prefixes) {
  ll <- lapply(prefixes, function(x) {
    startsWith(strings, x)} # returns a list matches for each prefix
  )
  res <- Reduce(f = "|", ll)
  return(res)
}

#' Select a subset of variables from a coda object
#'
#' Select a subset of variables from an [coda::mcmc()] or [coda::mcmc.list()]
#' object. Variables with names starting with one of the strings in `prefixes`
#' are selected.
#'
#' @param x a `mcmc` object
#' @param prefixes character vector
#'
#' @return
#' A `mcmc` object with only the variables
#' @export
#'
#' @examples
#' require(coda)
#' data(line) # data from package coda
#' coda::varnames(line)
#' new_line <- select_from_mcmc(line, c("al", "si"))
#' coda::varnames(new_line)
#'
select_from_mcmc <- function(x, prefixes) {
  all_names <- coda::varnames(x)
  var_index <- startsWith_multiple(all_names, prefixes)
  new_mcmc <- x[, var_index, drop = FALSE]
  return(new_mcmc)
}

#' Calculate the proportion of variables in an mcmc object that have "good" gelman.diag
#'
#' Calculate the proportion of variables in an mcmc object that have an
#' [coda::gelman.diag()] value below or equal psrf_limit (defalt = 1.10)
#'
#' @param x an mcmc object
#' @param psrf_limit numeric the limit to classify the diag as good
#'
#' @return
#' A numeric vector of length two. The first number is the proportion of
#' gelman.diag() <= `psrf_limit` the second number is the number of variables
#' in `x`.
#' @export
#'
#' @examples
#' require(coda)
#' data(line) # data from package coda
#' gelman_good_proportion(line)
#'
gelman_good_proportion <- function(x, psrf_limit = 1.10) {
  res <- coda::gelman.diag(x)$psrf[,1]
  N <- length(res)
  good_gelman <- res <= psrf_limit
  good_proportion <- sum(good_gelman) / length(good_gelman)
  res <- c(good_proportion, N)
  names(res) <- c("Proportion", "N_variables")
  return(res)
}
