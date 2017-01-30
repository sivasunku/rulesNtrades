trades.entryExits <- function(g = NULL){
  ###############################################################################
  #This function builds the xts object, which will have longEntry,LongExit,
  #  ShortEntry,ShortExits table & their corresponding prices
  # Input : x - transactions (gTrades)
  # Calculation: cPos - 1 - Long, -1 Short,0 - none
  #              nL - net Loss (sum of all losses)
  # gaintopain = nR/abs(nL)
  ###############################################################################
  if (missing(g)) { g <- gTrades }
  
  #A dummy xts with initial entry/exit signals
  t1 <- xts(order.by = gTrades$entryTime)
  t2 <- xts(order.by = gTrades$exitTime)
  t1 <- merge(t1,longEntry = NA,longExit = NA,shortEntry = NA, shortExit = NA)
  t2 <- merge(t2,longEntry = NA,longExit = NA,shortEntry = NA, shortExit = NA)
  t  <- rbind(t1,t2)
  
  for (i in 1:nrow(g)){
    n <- g[i,]
    eT <- as.POSIXct(n$entryTime)
    xT <- as.POSIXct(n$exitTime)
    eP <- as.numeric(n$entryPrice)
    xP <- as.numeric(n$exitPrice)
    
    #long Entry & exit
    if (n$direction == "LONG"  ) {
      t[eT,]$longEntry <- eP
      t[xT,]$longExit  <- xP
    }
    
    if (n$direction == "SHORT"  ) {
      t[eT,]$shortEntry <- eP
      t[xT,]$shortExit  <- xP
    }
  } #for loop end
  return(t)
} #end of entryExits function

trades.stat <- function(inTrades = NULL){
  #############################################################################
  #    trxn.stat.trades ==> Get the requird statistics from the trades
  # Input - Table containing Transactions, default - gTrxns
  #############################################################################
  if (missing(inTrades)){
    t <- trades.toxts(gTrades)
  } else {
    t <- trades.toxts(inTrades)
  }
  gStats <<- emptyStats
  allTrades   <- t
  longTrades  <- t[(t$direction == 1),]
  shortTrades <- t[(t$direction == -1),]
  a <- b <- c <- NULL
  a <- trades.stat.sub(allTrades)
  b <- trades.stat.sub(longTrades)
  c <- trades.stat.sub(shortTrades)
  stats <- cbind(a,b,c)
  colnames(stats) <- c("TOTAL","LONG","SHORT")
  return(stats)
} #end of trxn.stat

###################################################################
# This function adjusts the gTrades according to capital
#   This doesn't rerun the backtesting algorithm, 
#    but adjusts the gTrades as if the qty was increased according
#        to increased capital
###################################################################
position.reInvTrades   <- function (lTrades = NULL) {
  
  if (missing(lTrades)){
    t <- gTrades
  } else {
    t <- lTrades
  }
  #Order by the entry date
  t <- t[order(t[,3]),]
  ret <- t[FALSE,]
  if (!gCapital$reInvFlag){
    return (TRUE)
  }
  
  for ( i in 1:nrow(t)){
    currCap <- gCapital$initCapital + cumsum(t[1:i,]$netP)[i]
    if (currCap <= gCapital$stopCapital){
      warning( sprintf("Capital crunch at ith row %s",i) )
    } #Stop for capital crunch
    
    #Get extraQty Factor
    extraFactor <- 1
    ret[i,] <- t[i,]
    
    if ( currCap > gCapital$initCapital){
      extraLots   <- floor((currCap - gCapital$initCapital - gCapital$buffCapital)/gCapital$incrCapital)
      extraQty    <- extraLots * gCapital$incrQty
      tradeQty    <- gCapital$initQty + extraQty
      extraFactor <- round(tradeQty/gCapital$initQty)
      maxExtraFactor   <- floor(gCapital$maxQty/gCapital$initQty)
      extraFactor <- min(extraFactor,maxExtraFactor)
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
} #end of adjustCapital