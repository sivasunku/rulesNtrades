#' Add transactions to a portfolio.
#' 
#' When a trade or adjustment is made to the Portfolio, the addTxn function 
#' calculates the value and average cost of the transaction,  the change in 
#' position, the resulting positions average cost, and any realized profit 
#' or loss (net of fees) from the transaction. Then it stores the transaction 
#' and calculations in the Portfolio object.
#'
#' Fees are indicated as negative values and will be
#' subtracted from the transaction value. TxnFees can either
#' be a fixed numeric amount, or a function (or charavcter
#' name of a function) in which case the function is
#' evaluated to determine the fee amount.
#'
#' The \code{\link{pennyPerShare}} function provides a simple
#' example of a transaction cost function.
#'
#' Transactions which would cross the position through zero
#' will be split into two transactions, one to flatten the
#' position, and another to initiate a new position on the
#' opposite side of the market.  The new (split) transaction
#' will have its timestamp incremented by \code{eps} to
#' preserve ordering.
#'
#' This transaction splitting vastly simplifies realized P&L
#' calculations elsewhere in the code. Such splitting also
#' mirrors many execution platforms and brokerage
#' requirements in particular asset classes where the side
#' of a trade needs to be specified with the order.
#'
#' The \code{addTxns} function allows you to add multiple
#' transactions to the portfolio, which is much faster than
#' adding them one at a time. The \code{TxnData} object must
#' have "TxnQty" and "TxnPrice" columns, while the "TxnFees"
#' column is optional.
#' 
#' If \code{TxnFees} is the name of a function, the function 
#' will be called with \code{TxnFees(TxnQty, TxnPrice, Symbol)}
#' so a user supplied fee function must at the very least take 
#' dots to avoid an error. We have chosen not to use named arguments 
#' to reduce issues from user-supplied fee functions. 
#' 
#' @param Portfolio  A portfolio name that points to a portfolio object structured with \code{initPortf()}
#' @param Symbol An instrument identifier for a symbol included in the portfolio, e.g., "IBM"
#' @param TxnDate  Transaction date as ISO 8601, e.g., '2008-09-01' or '2010-01-05 09:54:23.12345'
#' @param TxnQty Total units (such as shares or contracts) transacted.  Positive values indicate a 'buy'; negative values indicate a 'sell'
#' @param TxnPrice  Price at which the transaction was done
#' @param \dots Any other passthrough parameters
#' @param TxnFees Fees associated with the transaction, e.g. commissions., See Details
#' @param allowRebates whether to allow positive (rebate) TxnFees, default FALSE
#' @param ConMult Contract/instrument multiplier for the Symbol if it is not defined in an instrument specification
#' @param verbose If TRUE (default) the function prints the elements of the transaction in a line to the screen, e.g., "2007-01-08 IBM 50 @@ 77.6". Suppress using FALSE.
#' @param eps value to add to force unique indices
#' @param TxnData  An xts object containing all required txn fields (for addTxns)
#' @note 
#' The addTxn function will eventually also handle other transaction types, 
#' such as adjustments for corporate actions or expire/assign for options. 
#' See \code{\link{addDiv}} 
#'
#' @seealso \code{\link{addTxns}}, \code{\link{pennyPerShare}}, \code{\link{initPortf}}
#' @author Peter Carl, Brian G. Peterson
#' @export addTxn
#' @export addTxns
#' transactions
#' 
#' Functions related to all transactions and trades
#' 
#' @author Siva Sunku
#' @keywords position
#' @note
#' 
#' 
#' This will create a data holder for all the transactions
#' The data series stored here can be an irregular time series.
#' @details  - This function returns the dataframe of 'n' transaction holder.
#' @param  n - no. of rows of transaction dataframe.
#' @param  type - OPEN - transaction is added for opening position, CLOSE transaction is added while closing position
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

#' @export
is.trxns <- function(t){
  return ( inherits(t,"trxns") )
}

#' @details  - This function adds the position to a transaction entry table
#' @param  pos - position to be added
#' @param  type - OPEN - transaction is added for opening position, CLOSE transaction is added while closing position
#' @rdname transactions
#' @return recently added transaction
#' @export
add.trxns.position <- function (pos, type = c('OPEN','CLOSE') ) {
  if ( !is.position(pos) ) {
    stop("pos is not a position class")
  }
  type <- match.arg( type, c('OPEN','CLOSE') )
  
  if ( !islong(pos) && !isshort(pos) ){
    stop("In add.trxn.position pos is neither long nor short" )
  }
  
  if  ( ( !isopen(pos)   &&  (type == 'OPEN')  ) ||
        ( !isclosed(pos) &&  (type == 'CLOSE') ) ) {
    stop("addPosToTxn: Trying to add position either not closed or not opened")
  }
  
  
  #Add open position to transactions
  n <- cacheEnv$trxnRow
  if ( type == "OPEN"){
    cacheEnv$trxns[n,]$symbols <- pos$symbol
    cacheEnv$trxns[n,]$date    <- pos$openDate
    cacheEnv$trxns[n,]$price   <- pos$openPrice
    cacheEnv$trxns[n,]$type    <- pos$direction
    cacheEnv$trxns[n,]$qty     <- qty(pos)
    cacheEnv$trxns[n,]$fees    <- abs( qty(pos) ) * pos$openPrice * cacheEnv$trxnFee
    cacheEnv$trxnRow <- cacheEnv$trxnRow + 1
  } #End of Open type
  
  if ( type == "CLOSE"){
    cacheEnv$trxns[n,]$symbols <<- pos$symbol
    cacheEnv$trxns[n,]$date    <<- pos$closeDate
    cacheEnv$trxns[n,]$price   <<- pos$closePrice
    cacheEnv$trxns[n,]$type    <<- ifelse(pos$direction == "LONG","SHORT","LONG")
    cacheEnv$trxns[n,]$qty     <<- -1 * qty(pos)
    cacheEnv$trxns[n,]$fees    <<- abs(pos$closeQty) * pos$openPrice * gTxnFee
    cacheEnv$trxnRow <- cacheEnv$trxnRow + 1
  } #End of close type
  retrun(cacheEnv[n,])
}

#' @param instr - default NULL, otherwise trades/trxns of particular instrument
#' @return returns the trades/trxns data frame
#' @author Siva Sunku
#' @export
#' @rdname transactions
#' 
get.trxns <- function( instr = NULL ){
  if ( ! missing(instr) ){
    return ( cacheEnv$trxns[1:cacheEnv$trxnRow,] %>% dplyr::filter(symbol != instr ) )
  } else {
    return ( cacheEnv$trxns[1:cacheEnv$trxnRow,] )
  }
}
