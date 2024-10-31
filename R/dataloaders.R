#' Read data used for Smolt mark recaputure model
#'
#' Read the rdata file saved by söte2bugs. If vectors c, m or r contains -9
#' they are replaced with NA.
#'
#' @param directory character default current directory "."
#' @param rdatafile character Default "RData_dump.RData" in current directory.
#'
#' @return
#' Returns a data structure with the data from a smolt trap in river in year for species.
#' @export
#'
read_rdata <- function(directory = ".",  rdatafile = "RData_dump.RData") {
  fname <- file.path(directory, rdatafile)
  myenv <- environment()
  load(fname, envir = myenv)
  if (!exists("rdata", myenv)) stop(paste0(fname, ' must contain list "rdata".'))
  res <- get("rdata", envir = myenv)
  res$c[res$c == -9] <- NA
  res$m[res$m == -9] <- NA
  res$r[res$r == -9] <- NA
  return(res)
}


#' Read summarized results from smolt model
#'
#' @param directory character default current directory "."
#' @param resultfile character Default "model_results.xlsx" in current directory.
#'
#' @return
#' Returns a data frame with the results from a smolt trap in river in year for species.
#' @export
#'
read_model_result <- function(directory = ".", resultfile = "model_results.xlsx") {
  fname <- file.path(directory, resultfile)
  d <- readxl::read_excel(fname , sheet = "stats",
                          col_types = c("text", "numeric",  "numeric", "numeric", "numeric",
                                        "numeric", "numeric", "numeric", "numeric"))
  names(d) <- make.names(names(d))
  names(d)[5] <- "Q2.5"
  names(d)[7] <- "Q97.5"
  return(d)
}

#' Read the CU (smolt estimate coda) from the smolt model results.
#'
#' @param directory character default current directory "."
#' @param resultfile character filename of saved results. Default "model_results.xlsx"
#'
#' @return
#' Returns a data frame with the columns iteration, chain1 and chain2. Chain1 and chain2 are
#' samples (posterior) of the estimated CU (total smolt run).
#'
#' @export
#'
read_CU <- function(directory = ".", resultfile = "model_results.xlsx") {
  fname <- file.path(directory, resultfile)
  d <- readxl::read_excel(fname , sheet = "CU")
  d$chain1 <- as.numeric(d$chain1)
  d$chain2 <- as.numeric(d$chain2)
  return(d)
}

#' Read coda saved in BlackBox txt format and return a mcmc.list
#'
#'
#' The function will read three files saved as text from BlackBox with Inference/Samples.
#' All three files must be in the same directory.
#'
#' @param directory character default current directory "."
#' @param index character name of index file default "index.txt"
#' @param chain1 character name of file with first chain default "chain1.txt"
#' @param chain2 character name of file with second chain default "chain2.txt"
#'
#' @return
#' return an object of class mcmc.list
#'
#' @export
#'
#'
read_blackbox_coda <- function(directory = ".", index = "index.txt",
                              chain1 = "chain1.txt", chain2 = "chain2.txt") {
  index  <- utils::read.table(file.path(directory, index))
  chain1  <- utils::read.table(file.path(directory, chain1))
  chain2  <- utils::read.table(file.path(directory, chain2))
  n_samp <- index[1, 3]
  n_var  <- nrow(index)
  chain_start <- chain1[1, 1]
  chain_thin <- chain1[2, 1] - chain1[1, 1]

  mat1  <- matrix(nrow = n_samp, ncol = n_var)
  mat2  <- matrix(nrow = n_samp, ncol = n_var)
  for (i in 1:n_var) {
    start <- index[i, 2]
    stop <- index[i, 3]
    mat1[, i]  <- chain1[start:stop, 2]
    mat2[, i]  <- chain2[start:stop, 2]
  }
  colnames(mat1)  <- index[, 1]
  colnames(mat2)  <- index[, 1]
  res <- coda::mcmc.list(coda::mcmc(data= mat1, start = chain_start, thin = chain_thin),
                   coda::mcmc(data= mat2, start = chain_start, thin = chain_thin))
  return(res)
}
