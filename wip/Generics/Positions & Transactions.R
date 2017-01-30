#############################################################################
#This script will have all functions related to positions.
#                & write the output to 
# Use addDelta.R to add new rows.
#############################################################################

#############################################################################
# Create a blank positon object and initialize the same.
#############################################################################
position <- function(symbols = NULL){
  if (missing(symbols)) { 
    symbols = gSymbols
    }
  #Create a basic data structure object - a list that keeps track of positions
  me <- list(
            symbol = symbols,
            direction = NULL,
            #toOpen - flags to open the position, but not actually opened
            justOpened = FALSE,

            openFlag = FALSE,
            openDate = as.POSIXct("1950-01-01"),
            openPrice = 0,
            openQty = 0,
            openReason = "None",
            openCase   = "case1",

            closeFlag = FALSE,
            closeDate = as.POSIXct("1950-01-01"),
            closePrice = 0,
            closeQty = 0,
            closeReason = "None",
            #
            trailPrice = 0,
            slpPrice   = 0,
            barCount   = 0,
            #profit booking
            pbPrice    = 0,
            pbLot      = 0,
            pbCount    = 0
  )
  class(me) <- append(class(me),"position")
  return(me)
}

position.isLong <- function(pos){
  #############################################################################
  # Check if position is Long/short
  #############################################################################
  if ( position.isOpen(pos) &&  (pos$direction == 'LONG') ){
    return(TRUE)
  }
  return(FALSE)
}
position.isShort <- function(pos){
  if ( position.isOpen(pos) && (pos$direction == 'SHORT') ){
    return(TRUE)
  }
  return(FALSE)
}

position.isOpen <- function(pos){
  return(pos$openFlag)
}
position.isClosed <- function(pos){
  return(pos$closeFlag)
}
position.addPos   <- function(a,b){
  #Add position a & b return c. c = a+b
  c <- a
  if (a$direction != b$direction){
    stop("In position.addPos a & b direction is different")
  }
  c$openPrice <- ( (a$openPrice * a$openQty) + (b$openPrice * b$openQty) )/(a$openQty + b$openQty)
  c$openQty   <- a$openQty + b$openQty
  return (c)
}
position.reducePos   <- function(a,b){
  #Add position a & b return c. c = a+b
  c <- a
  if (a$direction != b$direction){
    stop("In position.addPos a & b direction is different")
  }
  c$openQty   <- a$openQty - b$openQty
  return (c)
}

position.addTxn <- function (pos, type = 'OPEN'){
  #############################################################################
  # Add position to Transactions - gTrxns
  #############################################################################

  if ( match('position',class(pos), nomatch = 0) == 0){
    stop("addPosToTxn: pos should be of class position")
  }
  if ( type != 'OPEN' && type != 'CLOSE' ){
    stop("addPosToTxn: type should be either OPEN or CLOSE")
  }
  if ( !(position.isLong(pos) || position.isShort(pos) )) {
    stop("in position.addTxn direction is neither Long nor short" )
  }
  if  ( ((pos$openFlag == FALSE) && (type == 'OPEN')) ||
        ((pos$closeFlag == FALSE) && (type == 'CLOSE')) ) {
    print(type)
    stop("addPosToTxn: Trying to add position either not closed or not opened")
  }
  

  #Add open position to transactions
  n <- nrow(gTrxns) + 1
  if ( type == "OPEN"){
    gTrxns[n,]$symbols <<- pos$symbol
    gTrxns[n,]$date    <<- pos$openDate
    gTrxns[n,]$price   <<- pos$openPrice
    gTrxns[n,]$type    <<- pos$direction
    gTrxns[n,]$qty     <<- ifelse(pos$direction == "LONG",pos$openQty,-1 * pos$openQty)
    gTrxns[n,]$fees    <<- abs(pos$openQty) * pos$openPrice * gTxnFee
  } #End of Open type
  
  if ( type == "CLOSE"){
    gTrxns[n,]$symbols <<- pos$symbol
    gTrxns[n,]$date    <<- pos$closeDate
    gTrxns[n,]$price   <<- pos$closePrice
    gTrxns[n,]$type    <<- ifelse(pos$direction == "LONG","SHORT","LONG")
    gTrxns[n,]$qty     <<- ifelse(pos$direction =="LONG",-1 * pos$closeQty,pos$closeQty)
    gTrxns[n,]$fees    <<- abs(pos$closeQty) * pos$openPrice * gTxnFee
  } #End of close type
  
}

#############################################################################
# Add complete trade to dataframe, entry, exit ,fee all
#############################################################################
position.addTrade <- function (pos){
  
  if ( match('position',class(pos), nomatch = 0) == 0){
    stop("position.addTrade: pos should be of class position")
  }
  if ( pos$closeFlag == FALSE )  {
    stop("position.addTrade: Trying to add position which is not closed")
  }
  
  n <- nrow(gTrades) + 1
  gTrades[n,]$symbols    <<- pos$symbol
  gTrades[n,]$direction  <<- pos$direction
  gTrades[n,]$barCount   <<- pos$barCount
  
  gTrades[n,]$entryTime  <<- pos$openDate
  gTrades[n,]$entryPrice <<- pos$openPrice
  #It should be a closeQty, coz, trade can be a partial Exit
  gTrades[n,]$entryQty   <<- pos$closeQty
  gTrades[n,]$entryFee   <<- abs(pos$openQty) * pos$openPrice * gTxnFee
  
  gTrades[n,]$exitTime  <<- pos$closeDate
  gTrades[n,]$exitPrice <<- pos$closePrice
  gTrades[n,]$exitQty   <<- pos$closeQty
  gTrades[n,]$exitFee   <<- abs(pos$closeQty) * pos$openPrice * gTxnFee
  
  gTrades[n,]$closeReason <<- pos$closeReason
  gTrades[n,]$openReason  <<- pos$openReason
  
  #Calculate the grossP based on direction
  # use closeQty only, as trade is added for each closed position. openqty & closeqty may vary
  gTrades[n,]$grossP  <<-  ifelse(position.isLong(pos),
                            ( (pos$openPrice  * pos$closeQty  * -1) + 
                              (pos$closePrice * pos$closeQty *  1) ),
                            ( (pos$openPrice  * pos$closeQty  *  1) + 
                              (pos$closePrice * pos$closeQty * -1) )
  )
  
  gTrades[n,]$netP   <<- gTrades[n,]$grossP + gTrades[n,]$entryFee + gTrades[n,]$exitFee
  
}

#############################################################################
# This file will have all the required functions for transactions (trxn)
#    trxn.add ==> Add Transaction to global variable gTrxn
#    trxn.blot ==> convert the gTrxn to blotter position
#    
#############################################################################
trxn <- function(symbols = NULL){
  if (missing(symbols)) {
    symbols <- gSymbols
  }
  #Create a basic data structure object - a list that keeps track of positions
  me <- list(
    symbol = symbols,
    date = as.POSIXct("1950-01-01"),
    price = 0,
    qty = 0,
    type = "B",
    fees = 0
  )
  class(me) <- append(class(me),"trxn")
  return(me)
}
#############################################################################
#    trxn.add ==> Add a Transaction to global variable gTrxn
#############################################################################
trxn.add <- function(symbols = NULL,
                     date = Sys.Date(),
                     price = 0,
                     qty = 0,
                     type = NULL,
                     fees = 0) {
  
}
#############################################################################
#    trxn.blot ==> Add the transactions from global (gTrxns) to blot
#    p - portfolio
#    t - transaction table, default gTrxns
#############################################################################
trxn.blot <- function(p, t = NULL) {
  if (missing(t)){ t <- gTrxns }
  
  for( i in 1:nrow(t)) {
    addTxn(p,
           Symbol = t[i,]$symbols,
           TxnDate = t[i,]$date,
           TxnQty = t[i,]$qty,
           TxnPrice = t[i,]$price,
           TxnFees = t[i,]$fees,
           verbose = verbose)
  }
  return(p)
} #end of function

#############################################################################
#    trxn.stat ==> Get the requird statistics from the trxns.
# Input - Table containing Transactions, default - gTrxns
#############################################################################
trxn.stat <- function(t = NULL){
  if(missing(t)) {
    t <- gTrxns
  }
  p <- "DummyPortfolio"
  rm.strat(p)
  initPortf(p, symbols=symbols, currency='USD')
  trxn.blot(p,gTrxns)
  getTxns(p,symbols) -> t1
  
  toFrame(t1) -> f
  grossP <- filter(f,Net.Txn.Realized.PL >0 ) %>% summarise(sum(Net.Txn.Realized.PL))
  grossL <- filter(f,Net.Txn.Realized.PL <0 ) %>% summarise(sum(Net.Txn.Realized.PL))
  netP <- grossP + grossL
  
  tradesP <- filter(f,Net.Txn.Realized.PL >0 ) %>% summarise(n())
  tradesL <- filter(f,Net.Txn.Realized.PL <0 ) %>% summarise(n())
  totTrades <- tradesP + tradesL
  profitPerTrade <- netP/totTrades
  ptg <- netP/abs(grossL)
  
  
  d <- trxn.tranDrawdowns(t1)
  list(gainToPain = ptg,
       drawdowns = d,
       trxns = t1,
       grossProfit = grossP,
       grossLoss = grossL,
       netProfit = netP,
       profitTrades = tradesP,
       lossTrades = tradesL,
       profitPerTrade = profitPerTrade,
       totalTrades = totTrades
       )
} #end of trxn.stat

###############################################################################
#This getS the drawdowns in descending order (Worst to best)
# Input : x - This 
###############################################################################
trxn.tranDrawdowns <- function(x ) {
  if( match('transactions',class(x))){
    series <- as.vector(x$Net.Txn.Realized.PL)
    len <- nrow(x)
  }
  else {
    if (is.xts(x)){
      series <- x
      len <- nrow(x)
    }
    if (is.vector(x)){
      series <- x
      len <- length(x)
    }
  }
  
  if (!exists("series")){
    stop("object should be either of class transactions or vector")
  }
  
  d <- tempSum <- 0
  for (i in 1:len){
    isProfit <- ifelse(series[i]>0,TRUE,FALSE)
    if (isProfit){
      d <- append(d,tempSum)
      tempSum <- 0
    } else {
      tempSum <- tempSum +series[i]
    }
  }
  
  sort(d)[1:30]
} #End of drawdowns

###############################################################################
#This function calculates gain to pain ratio for transactions object
# Input : x - transactions (which are output from blotter package)
# Calculation: nR - net Returns (Total profit - total loss - brokerage)
#              nL - net Loss (sum of all losses)
# gaintopain = nR/abs(nL)
###############################################################################
trxn.gainToPain <- function(x ) {
  if( !match('transactions',class(x))){
    stop("tranDrawdowns: object should be either of class transactions")
  }
  nR <- sum(x$Net.Txn.Realized.PL)
  nL <- sum(x[which(x$Net.Txn.Realized.PL <0),]$Net.Txn.Realized.PL)
  round(nR/(abs(nL)),2)
} #End of drawdowns


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

trades.stat.sub <- function(allTrades){
#This function returns stats for a given set of trades, can be long, short or all
  
  stats <- gStats
  if (nrow(allTrades) >0){
    stats["GrossProfit","TOTAL"]    <- sum(allTrades$grossP)
    stats["Comissions","TOTAL"]     <- sum(allTrades$entryFee+allTrades$exitFee)
    stats["NetProfit","TOTAL"]      <- sum(allTrades$netP)
    stats["NoTotalTrades","TOTAL"]  <- nrow(allTrades)
    stats["ProfitPerTrade","TOTAL"] <- sum(allTrades$netP)/nrow(allTrades)
    stats["PainToGain","TOTAL"]     <- sum(allTrades$netP)/abs(sum(allTrades[(allTrades$netP<0),]$netP))
    stats["ProfitFactor","TOTAL"]   <- sum(allTrades[(allTrades$netP>0),]$netP)/abs(sum(allTrades[(allTrades$netP<0),]$netP))
    stats["WinningPercentage","TOTAL"] <- round(nrow(allTrades[(allTrades$netP>0),])/nrow(allTrades),digits = 2)
    stats["avgBars","TOTAL"]           <- round(mean(allTrades$barCount),digits = 2)
    
    stats["NoLosingTrades","TOTAL"]     <- nrow(allTrades[(allTrades$netP<=0),])
    stats["LosingTradesProfit","TOTAL"] <- sum(allTrades[(allTrades$netP<=0),]$netP)
    stats["avgLossBars","TOTAL"]         <- round(mean(allTrades[allTrades$netP<0,]$barCount),digits = 2)
    stats["avgLoss","TOTAL"]            <- mean(allTrades[(allTrades$netP<=0),]$netP)
    stats["stdDevLoss","TOTAL"]         <- sd(allTrades[(allTrades$netP<=0),]$netP)
    stats["maxDrawdown","TOTAL"]        <- fTrading::maxDrawDown(allTrades$netP)$maxdrawdown * -1
    stats["maxLosingStreak","TOTAL"]    <- trades.tranDrawdowns(allTrades)[1]
    stats["maxPainSum","TOTAL"]         <- min(cumsum(allTrades$netP))
    stats["maxPossibleLoss","TOTAL"]    <- min(stats["maxDrawdown","TOTAL"],
                                               stats["maxLosingStreak","TOTAL"],
                                               stats["maxPainSum","TOTAL"],
                                               na.rm = TRUE)
    
    stats["NoWinningTrades","TOTAL"]     <- nrow(allTrades[(allTrades$netP>0),])
    stats["WinningTradesProfit","TOTAL"] <- sum(allTrades[(allTrades$netP > 0),]$netP)
    stats["avgWin","TOTAL"]              <- mean(allTrades[(allTrades$netP> 0),]$netP)
    stats["stdDevWin","TOTAL"]           <- sd(allTrades[(allTrades$netP > 0),]$netP)
    stats["avgWinBars","TOTAL"]          <- round(mean(allTrades[allTrades$netP>0,]$barCount),digits = 2)
    
    stats["requiredCapital","TOTAL"]     <- abs(stats["maxPossibleLoss","TOTAL"]) * 2 + gCapital$cushCapital
    stats["ReturnPct","TOTAL"]           <- sum(allTrades$netP)/stats["requiredCapital","TOTAL"] *100
    
  }
  
  return(stats)
}

trades.toxts <- function(t){
  #############################################################################
  # trxn.toXts -> convert the trades dataframe to XTS
  #############################################################################
  t$symbols <- NULL
  t$direction <- ifelse(t$direction=="LONG",1,-1)
  t$exitTime <- NULL
  t$closeReason <- NULL
  t$openReason <- NULL
  
  xts(t[,-2],order.by = t[,2])
  
} #end of trade.toxts
trades.tranDrawdowns <- function(x ) {
  ###############################################################################
  #This getS the drawdowns in descending order (Worst to best)
  # Input : x - This 
  ###############################################################################
  
  series <- as.numeric(x$netP)
  if (length(series) ==0 ){return(0)}
  len <- length(series)
  d <- tempSum <- 0
  for (i in 1:len){
    isProfit <- ifelse(series[i]>0,TRUE,FALSE)
    if (isProfit){
      d <- append(d,tempSum)
      tempSum <- 0
    } else {
      tempSum <- tempSum +series[i]
    }
  }
  
  sort(d)[1:30]
} #End of drawdowns

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