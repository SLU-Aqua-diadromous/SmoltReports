#' Read data used for Smolt mark recaputure model
#'
#' @param fname character Default "RData_dump.RData" in current directory.
#'
#' @return
#' Returns a data structure with the data from a smolt trap in river in year for species.
#' @export
#'
read_rdata <- function(fname = "RData_dump.RData") {
  myenv <- environment()
  load(fname, envir = myenv)
  if (!exists("rdata", myenv)) stop(paste0(fname, ' must contain list "rdata".'))
  res <- get("rdata", envir = myenv)
  return(res)
}


#' Read results from smolt model
#'
#' @param fname character Default "model_results.xlsx" in current directory.
#'
#' @return
#' Returns a data frame with the results from a smolt trap in river in year for species.
#' @export
#'
read_model_result <- function(fname = "model_results.xlsx") {
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
#' @param fname character Default "model_results.xlsx" in current directory.
#'
#' @return
#' Returns a data frame with the columns iteration, chain1 and chain2. Chain1 and chain2 are
#' samples (posterior) of the estimated CU (total smolt run).
#'
#' @export
#'
read_CU <- function(fname = "model_results.xlsx") {
  # fname <- system.file(file.path("smoltdata", river, year, species), "model_results.xlsx",
  #                      package = "SmoltReports", mustWork = TRUE)
  d <- readxl::read_excel(fname , sheet = "CU")
  d$chain1 <- as.numeric(d$chain1)
  d$chain2 <- as.numeric(d$chain2)
  return(d)
}
