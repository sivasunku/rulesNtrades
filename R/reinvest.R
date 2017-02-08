#' reinvest - reinvested trades
#' 
#' This will provide the trades & their corresponding quantities, how they would be if all the capital was reinvested.
#' Capital gained can be utilized to increase the traded quantity according to the given trade parameters.
#' 
#'
#' @author Siva Sunku
#' @keywords graph,plot
#' @note
#' 
#' @param  initCapital - Initial capital required
#' @param  stopCapital - Capital threshhold, below which trading would be stopped & warning would be added
#' @param  buffCapital - buffer capital, This would be saved before increasing the trade quantity
#' @param  incrCapital - One Unit of incremental Capital, Each unit of incrCapital gives capacity to increase one unit of incrQty
#' @param  initQty     - Initial Quantity
#' @param  incrQty     - One unit of incremental quantity
#' @param  maxQty      - maximum quantity that can be traded. It will act as a roof irrespective of capital
#' @return - Returns the new trades that would have been executed with incremental capital. New stats to be applied on this if stats to be observed.
#' @rdname reinvest
#' @export
#' 
reinvest.trades <- function( t, 
                             initCapital, stopCapital, buffCapital = 0, incrCapital,
                             initQty,incrQty,maxQty = 10000 ){
  if ( !is.trades(t) ) {
    stop("reinvest.trades - t is not a trades object. Get the trades from a portfolio using get.trades")
  }

  t <- t[order(t[,3]),]
  ret <- t[FALSE,]

  for ( i in 1:nrow(t)){
    currCap <- initCapital + cumsum(t[1:i,]$netP)[i]
    if (currCap <= stopCapital){
      warning( sprintf("Capital crunch in reinvest.trades at ith row %s",i) )
    }
    
    #Get extraQty Factor
    extraFactor <- 1
    ret[i,] <- t[i,]
    
    if ( currCap > initCapital){
      extraLots   <- floor( (currCap - initCapital - buffCapital) / incrCapital )
      extraQty    <- extraLots * incrQty
      tradeQty    <- initQty + extraQty
      tradeQty    <- min( maxQty, tradeQty )
      extraFactor <- floor( round( tradeQty / initQty ) )
    }
    
    if (extraFactor > 1) {
      ret[i,]$entryQty <- ret[i,]$entryQty * extraFactor
      ret[i,]$entryFee <- ret[i,]$entryFee * extraFactor
      ret[i,]$exitQty  <- ret[i,]$exitQty  * extraFactor
      ret[i,]$exitFee  <- ret[i,]$exitFee  * extraFactor
      ret[i,]$grossP   <- ret[i,]$grossP   * extraFactor
      ret[i,]$netP     <- ret[i,]$netP     * extraFactor
    }
  }
  
  return(ret)
}