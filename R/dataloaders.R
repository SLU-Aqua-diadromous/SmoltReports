#' Read data used for Smolt mark recaputure model
#'
#' @param river
#' @param year
#' @param species
#'
#' @return
#' Returns a data structure with the data from a smolt trap in river in year for species.
#' @export
#'
#' @examples
read_rdata <- function(fname = "RData_dump.RData") {
  # fname <- system.file(file.path("smoltdata", river, year, species), "RData_dump.RData",
  #                      package = "SmoltReports", mustWork = TRUE)
  load(fname)
  return(rdata)
}


#' Read results from smolt model
#'
#' @param river
#' @param year
#' @param species
#'
#' @return
#' Returns a data frame with the results from a smolt trap in river in year for species.
#' @export
#'
#' @examples
read_model_result <- function(fname = "model_results.xlsx") {
  # fname <- system.file(file.path("smoltdata", river, year, species), "model_results.xlsx",
  #                      package = "SmoltReports", mustWork = TRUE)
  d <- readxl::read_excel(fname , sheet = "stats",
                          col_types = c("text", "numeric",  "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric"))
  names(d) <- make.names(names(d))
  names(d)[5] <- "Q2.5"
  names(d)[7] <- "Q97.5"
  return(d)
}

#' Read the CU (smolt estimate coda) from the smolt model results
#'
#' @param river
#' @param year
#' @param species
#'
#' @return
#' Returns a data frame with the columns iteration, chain1 and chain2.
#' @export
#'
#' @examples
read_CU <- function(fname = "model_results.xlsx") {
  # fname <- system.file(file.path("smoltdata", river, year, species), "model_results.xlsx",
  #                      package = "SmoltReports", mustWork = TRUE)
  d <- readxl::read_excel(fname , sheet = "CU")
  d$chain1 <- as.numeric(d$chain1)
  d$chain2 <- as.numeric(d$chain2)
  return(d)
}
