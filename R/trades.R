#' This will have all the required functions related to trades.
#' 
#' 
#' 
#' @author Siva Sunku
#' @keywords position
#' @note
#' 
#' This will create an object of trades class with given n rows
#' @details  - This function returns the trades data frame with given 'n' objects
#' @param  n - no. of rows
#' @rdname transactions
#' @return trades dataframe
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

#' @export
is.trades <- function(t){
  return ( inherits(t,"trades") )
}

#' This function adds the position to a trade.
#' @param  pos - position to be added
#' @rdname trades
#' @return recently added trade
#' @export
add.trades.position <- function (pos){
  
  if ( !is.position(pos) ) {
    stop("pos is not a position class")
  }
  if ( !isclosed(pos) ) {
    stop("position.addTrade: Trying to add position which is not closed")
  }
  #Add open position to transactions
  n <- cacheEnv$tradeRow
  cacheEnv$trades[n,]$symbols    <- pos$symbol
  cacheEnv$trades[n,]$direction  <- pos$direction
  cacheEnv$trades[n,]$barCount   <- pos$barCount
  
  cacheEnv$trades[n,]$entryTime  <- pos$openDate
  cacheEnv$trades[n,]$entryPrice <- pos$openPrice
  #It should be a closeQty, coz, trade can be a partial Exit
  cacheEnv$trades[n,]$entryQty   <- pos$closeQty
  cacheEnv$trades[n,]$entryFee   <- abs(pos$closeQty) * pos$openPrice * cacheEnv$trxnFee
  
  cacheEnv$trades[n,]$exitTime   <- pos$closeDate
  cacheEnv$trades[n,]$exitPrice  <- pos$closePrice
  cacheEnv$trades[n,]$exitQty    <- pos$closeQty
  cacheEnv$trades[n,]$exitFee    <- abs(pos$closeQty) * pos$closePrice * cacheEnv$trxnFee
  
  cacheEnv$trades[n,]$closeReason  <- pos$closeReason
  cacheEnv$trades[n,]$openReason   <- pos$openReason
  
  #Calculate the grossP based on direction
  # use closeQty only, as trade is added for each closed position. openqty & closeqty may vary
  cacheEnv$trades[n,]$grossP  <-  ifelse(islong(pos),
                                         (   (pos$openPrice  * pos$closeQty  * -1) + 
                                               (pos$closePrice * pos$closeQty  *  1) ),
                                         (   (pos$openPrice  * pos$closeQty  *  1) + 
                                               (pos$closePrice * pos$closeQty  * -1) )
  )
  
  cacheEnv$trades[n,]$netP   <-   cacheEnv$trades[n,]$grossP 
  + cacheEnv$trades[n,]$entryFee 
  + cacheEnv$trades[n,]$exitFee
  cacheEnv$tradeRow <- cacheEnv$tradeRow + 1
  
  return(cacheEnv$trades[n,])
}

#' @param instr - default NULL, otherwise trades/trxns of particular instrument
#' @dir   dir - 1 for LONG, -1 for SHORT trades, default all
#' @return returns the trades/trxns data frame
#' @author Siva Sunku
#' @export
#' @rdname trades
#' 
get.trades <- function( instr = NULL , dir = NULL){
  t <- cacheEnv$trades[1:cacheEnv$tradeRow,]
  
  if ( !missing(dir) ){
    t <- t %>% dplyr::filter(t$direction)
  }
  
  if ( !missing(instr) || !is.null(instr) ){
    t <- t %>% dplyr::filter(symbol != instr )
  }
  return (t)
}


#' @param  t - table
#' @rdname positions
#' @return xts object of trades
#' @export
as.xts.trades <- function(t){
  t$symbols <- NULL
  t$direction <- ifelse(t$direction=="LONG",1,-1)
  t$exitTime <- NULL
  t$closeReason <- NULL
  t$openReason <- NULL
  
  xts(t[,-2],order.by = t[,2])
}

