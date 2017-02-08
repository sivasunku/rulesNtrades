#' rules - Environment for executing rules N trades
#' 
#' clean up all the rules environment and create a new environment
#'
#' @author Siva Sunku
#' @keywords rulesenvironment
#' @note
#' 
.onLoad <- function(lib, pkg) {
  if(!exists('.rules'))  {
    .rules <<- new.env()
  }
}
