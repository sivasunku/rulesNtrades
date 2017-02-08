#' Create, Delete and Update a Portfolio 
#' 
#' initportfolio & getportfolio defaults the portfolio and retrieves the working environment.
#' 
#' @author Siva Sunku
#' @keywords portfolio
#' @note
#'
#' This will have details of a portfolio
#' @details portfolio - will initialize all the objects required for building the portfolio
#' @param pf- name of the portfolio
#' @param n - number of trades in portfolio
#' @param Fee - Fees for each of the transaction in terms of percent. Default 0.05 (0.0005 X amount)
#' @param charexpr - character expression to be evaluated by update.portfolio
#' @return portfolio - returns name of the portfolio
#' @rdname portfolio
#' @export
portfolio <- function(pf = "default",n=1000,Fee = 0.05){

  if( is.valid.portfolio(pf) ){
    stop("A portfolio exists by given name already")
  }
  
  p = new.env(hash = TRUE)
  p$trxnRow <- 1
  p$tradeRow <- 1
  p$trxnFee  <- Fee / 100
  p$trxns    <- trxns(n=n)
  p$trades   <- trades(n=n)
  class(p) <- append("portfolio",class(p))
  assign(pf,p,envir = .rules)
  return(pf)
}

#' @details is.portfolio - will return if a portfolio exists of that name & checks if given object is portfolio/not
#' @return  is.portfolio - checks if the given name is a portfolio or not
#' @rdname  portfolio
#' @export
is.portfolio <- function(pf){
  temp <- get(pf,envir = .rules)
  return ( inherits(temp,"portfolio") )
}

#' @details is.valid.portfolio - will return if a portfolio exists of that name
#' @return  is.valid.portfolio - Returns TRUE/FALSE
#' @rdname portfolio
#' @export
is.valid.portfolio <- function(pf="default"){
  if( exists(pf, envir=.rules,inherits=TRUE) && is.portfolio(pf) ){
    return(TRUE)
  }
  return(FALSE)
}

#' @details delete.portfolio - will delete a given portfolio if it exists and is valid one
#' @return  delete.portfolio - Returns the portfolio that is deleted or stops by error
#' @rdname portfolio
#' @export
delete.portfolio <- function(pf = "default"){
  if ( !is.valid.portfolio(pf) ){
    s <- sprintf("delete.portfolio - could not find the portfolio %s",pf)
    stop(s)
  }
  evalq(rm( list = ls()),envir = .rules)
}

#' @details update.portfolio - will update any value inside a portfolio. 
#' @return update.portfolio - returns none or error from eval statement
#' @rdname portfolio
#' @export
update.portfolio <- function(pf = "default",charexpr){
  if ( !is.valid.portfolio(pf) ){
    s <- sprintf("update.portfolio - could not find the portfolio %s",pf)
    stop(s)
  }
  ipf <- get(pf,envir = .rules)
  eval(parse(text = charexpr),envir = ipf)
}

#' @details set.fee.portfolio - will set the transaction fee given in input. If 0.05 given, it is marked as 0.0005
#' @return set.fee.portfolio  - returns new transaction fee
#' @rdname portfolio
#' @export
set.fee.portfolio <- function(pf = "default",Fee = 0.05){
  if ( !is.valid.portfolio(pf) ){
    s <- sprintf("update.portfolio - could not find the portfolio %s",pf)
    stop(s)
  }
  ipf <- get(pf,envir = .rules)
  ipf$trxnFee <- Fee / 100
  return (ipf$trxnFee)
}