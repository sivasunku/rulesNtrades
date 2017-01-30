#' Onload package that gets executed whenever package is loaded.
#' 
#' @description - Defines the objects gTrades,gTrxns,gStats 
#' the objects that are visible to this package
#' 
#' 
#' constants
#' 
#' Constants used within this package. These are available only in this package
#' 
#'


zzz <- function(){
  return (NULL)
}

cacheEnv  <- new.env(hash = TRUE)


.onAttach <- function(libname = find.package("myIndicators"), pkgname = "myIndicators") {
  #Load the constants
  data("sysdata",envir=environment())
  
}

