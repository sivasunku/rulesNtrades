#' Constructs the data containers used to store transactions and resulting trades
#' 
#' initObjects will create all the required objects before starting the analysis
#' 
#' @author Siva Sunku
#' @keywords bars
#' @note

#' This will initialize all the objects required for the trade
#' @param n- No. of rows to initialize trxns/trades. Default 1000
#' @return nothing
#' @rdname initObjects
#' @export
#' 
initObjects <- function(n=1000 ){
  #initialize the transaction table, trade table
  cacheEnv$trxns <- trxns(n=n)
  cacheEnv$trades <- trades(n=n)
  cacheEnv$trxnRow <- 1
  cacheEnv$tradeRow <- 1
  cacheEnv$trxnFee  <- 0.1
}

#' 
#' @param  in - percentage of fee to be added for each transaction.
#' @return trxnFee
#' @rdname initObjects
#' @export
#' 
setTrxnFee <- function(n = 0.1 ){
  cacheEnv$trxnFee  <- n / 100
  return ( cacheEnv$trxnFee )
}

#' This will set the transaction fee provided in pct terms.
#' @param  in - percentage of fee to be added for each transaction.
#' @return trxnFee
#' @rdname initObjects
#' @export
getTrxnFee <- function( ){
  return ( cacheEnv$trxnFee )
}

