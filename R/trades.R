#' Trades in a portfolio
#' 
#' A trade is two transactions a buy & sell transaction.
#' 
#' @author Siva Sunku
#' @keywords trades
#' @note
#' 
#' This will create a trade object
#' @details  trades - Returns the empty trades data frame with given 'n' objects
#' @param  n - no. of rows
#' @rdname trades
#' @return blank trades dataframe
#' @export
trades <- function(n=1){
  if ( !is.numeric(n) ) { n <- 1 }
  
  t <- data.frame(symbols = character(n),
                       direction = character(n),
                       entryTime = as.POSIXct("1950-01-01"),
                       entryPrice = double(n),
                       entryQty = double(n),
                       entryFee = double(n),
                       exitTime = as.POSIXct("1950-01-01"),
                       exitPrice = double(n),
                       exitQty = double(n),
                       exitFee = double(n),
                       closeReason = character(n),
                       openReason = character(n),
                       grossP = double(n),
                       netP  = double(n),
                       barCount = double(n),
                       stringsAsFactors = FALSE
  )
  class(t) <- append("trades",class(t))
  return(t)
}


#' checks if given object is of trades class/not
#' @rdname trades
#' @export
is.trades <- function(t){
  return ( inherits(t,"trades") )
}

#' @details add.trades.position - Function to add given position to trades table in a given portfolio
#' @param  pf - portfolio name. default is "default"
#' @param  pos - position to be added
#' @rdname trades
#' @return recently added trade
#' @export
add.trades.position <- function (pf = "default", pos){
  
  if ( !is.valid.portfolio(pf) ) {
    stop("add.trades.position - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }

  if ( !is.position(pos) ) {
    stop("add.trades.position - pos is not a position class")
  }
  
  if ( !isclosed(pos) ) {
    stop("add.trades.position - Trying to add position which is not closed")
  }
  
  #Add open position to transactions
  ipf <- get(pf,envir = .rules)
  n <- ipf$tradeRow
  ipf$trades[n,]$symbols    <- pos$symbol
  ipf$trades[n,]$direction  <- pos$direction
  ipf$trades[n,]$barCount   <- pos$barCount
  
  ipf$trades[n,]$entryTime  <- pos$openDate
  ipf$trades[n,]$entryPrice <- pos$openPrice
  
  #It should be a closeQty, coz, trade can be a partial Exit
  ipf$trades[n,]$entryQty   <- pos$closeQty
  ipf$trades[n,]$entryFee   <- abs(pos$closeQty) * pos$openPrice * ipf$trxnFee
  
  ipf$trades[n,]$exitTime   <- pos$closeDate
  ipf$trades[n,]$exitPrice  <- pos$closePrice
  ipf$trades[n,]$exitQty    <- pos$closeQty
  ipf$trades[n,]$exitFee    <- abs(pos$closeQty) * pos$closePrice * ipf$trxnFee
  
  ipf$trades[n,]$closeReason  <- pos$closeReason
  ipf$trades[n,]$openReason   <- pos$openReason
  
  #Calculate the grossP based on direction
  # use closeQty only, as trade is added for each closed position. openqty & closeqty may vary
  ipf$trades[n,]$grossP  <-  ifelse(islong(pos),
                                         (   (pos$openPrice  * pos$closeQty  * -1) + 
                                             (pos$closePrice * pos$closeQty  *  1) ),
                                         
                                         (   (pos$openPrice  * pos$closeQty  *  1) + 
                                             (pos$closePrice * pos$closeQty  * -1) )
  )
  
  ipf$trades[n,]$netP   <-   ipf$trades[n,]$grossP  - ipf$trades[n,]$entryFee - ipf$trades[n,]$exitFee
  ipf$tradeRow <- ipf$tradeRow + 1
  
  return(ipf$trades[n,])
}

#' @details get.trades - gets the trades from a default portfolio
#' @param pf - default "default"
#' @return returns the trades/trxns data frame
#' @rdname trades
#' @export
get.trades <- function( pf = "default"){
  if ( !is.valid.portfolio(pf) ) {
    stop("add.trades.position - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  ipf <- get(pf,envir = .rules)
  return( ipf$trades[1:(ipf$tradeRow-1),] )
}


#' @details as.xts.trades - Convert the trades from dataframe to xts. Since xts is a strict matrix, all string columns are removed.
#' Index of the returned xts object is open date on which trade is initially opened.
#' @param t - trades
#' @return returns the trades/trxns data frame
#' @rdname trades
#' @export
as.xts.trades <- function(t){
  if ( !is.trades(t) ) {
    stop("as.xts.trades - t is not a trades object.")
  }
  t$symbols <- NULL
  t$direction <- ifelse(t$direction=="LONG",1,-1)
  t$exitTime <- NULL
  t$closeReason <- NULL
  t$openReason <- NULL
  
  xts(t[,-2],order.by = t[,2])
}

