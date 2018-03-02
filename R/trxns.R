#' Transactions(trxns)
#' 
#' A transaction is a buy/sell of a position. Two transactions make a trade(buy/sell).
#'
#' @author Siva Sunku
#' @keywords transactions
#' @note
#'
#' 
#' @details  - This function returns the dataframe of 'n' blank transactions.
#' @param  n - no. of rows of blank transaction dataframe.
#' @rdname transactions
#' @export
trxns <- function(n = 1) {
  if ( !is.numeric(n) ) { n <- 1 }
  t <- data.frame(symbols = character(n),
                     date  = as.POSIXct("1950-01-01"),
                     price = double(n),
                     qty   = double(n),
                     type  = character(n),
                     fees  = double(n),
                     stringsAsFactors = FALSE
  )
  class(t) <- append("trxns",class(t))
  return(t)
}

#' checks if given object is trxns class/not
#' @rdname transactions
#' @export
is.trxns <- function(t){
  return ( inherits(t,"trxns") )
}

#' @details  add.trxns.position - adds a given position to the transaction table of a given portfolio
#' @param  pf - portfolio where the transactions to be added. Default - default portfolio
#' @param  pos - position to be added
#' @param  type - OPEN - transaction is added for opening position, CLOSE transaction is added while closing position
#' @rdname transactions
#' @return recently added transaction
#' @export
add.trxns.position <- function (pf = "default",pos, type = c('OPEN','CLOSE') ) {
  if ( !is.valid.portfolio(pf) ) {
    stop("add.trxns.position - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  
  if ( !is.position(pos) ) {
    stop("add.trxns.position - pos is not a position class")
  }
  type <- match.arg( type, c('OPEN','CLOSE') )
  
  if ( !islong(pos) && !isshort(pos) ){
    stop("add.trxn.position - pos is neither long nor short" )
  }
  
  if  ( ( !isopen(pos)   &&  (type == 'OPEN')  ) ||
        ( !isclosed(pos) &&  (type == 'CLOSE') ) ) {
    stop("add.trxn.position - Trying to add position either not closed or not opened")
  }
  
  ipf <- get(pf,envir = .rules)
  n <- ipf$trxnRow
  if ( type == "OPEN"){
    ipf$trxns[n,]$symbols <- pos$symbol
    ipf$trxns[n,]$date    <- pos$openDate
    ipf$trxns[n,]$price   <- pos$openPrice
    ipf$trxns[n,]$type    <- pos$direction
    ipf$trxns[n,]$qty     <- pos$openQty
    ipf$trxns[n,]$fees    <- abs( qty(pos) ) * pos$openPrice * ipf$trxnFee
    ipf$trxnRow <- ipf$trxnRow + 1
  } #End of Open type
  
  if ( type == "CLOSE"){
    ipf$trxns[n,]$symbols <- pos$symbol
    ipf$trxns[n,]$date    <- pos$closeDate
    ipf$trxns[n,]$price   <- pos$closePrice
    ipf$trxns[n,]$type    <- ifelse(pos$direction == "LONG","SHORT","LONG")
    ipf$trxns[n,]$qty     <- pos$closeQty
    ipf$trxns[n,]$fees    <- abs(pos$closeQty) * pos$openPrice * ipf$trxnFee
    ipf$trxnRow <- ipf$trxnRow + 1
  } #End of close type
  return(ipf$trxns[n,])
}

#' @param pf - default is "default" portfolio
#' @return returns the trades/trxns data frame
#' @rdname transactions
#' @export
get.trxns <- function( pf = "default"){
  if ( !is.valid.portfolio(pf) ) {
    stop("add.trxns.position - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  ipf <- get(pf,envir = .rules)
  return (ipf$trxns[1:(ipf$trxnRow-1),])
}
