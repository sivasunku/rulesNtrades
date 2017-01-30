#BUILD INDICATORS & SIGNALS
hma.buildHiLo  <- function(m,
                               n=3,
                               type=c("hilo","avg"),
                               freq=c("minutes","hours","days","weeks","months","years")
                               ){
  #############################################################################
  # Builds the HiLo Activator values for the given price table.
  # m - price table
  # n = 3 (consider 2 prev bars to find the mean/smooth)
  # freq = hourly/daily/weekly/monthly etc supported by split/endpoints
  # type = if hilo, only high/low is considered for the given frequency
  #        if avg, avg of highs inside frequency are considered. 
  #        e.g. if m is daily, f is weekly all daily bars are avg'ed
  #############################################################################
  if (!is.xts(m)){
    stop(" : m is not xts in hma.buildHiLo")
  }
  if (nrow(m) < n ) {
    stop("hma.buildHiLo : m should be atleast %s samples(rows)",n)
  }
  type <- match.arg(type)
  freq <- match.arg(freq)

  #if type is avg
  #sp <- (ep + 1)[-length(ep)]
  #ep <- ep[-1]
  sp <- c(0,xts:::startof(m,by = freq))
  ep <- endpoints(m,on=freq)

  if ( type == "hilo") {
    avgHi <- period.apply(m,ep,FUN = function(x) { max(Hi(x))} )
    avgLo <- period.apply(m,ep,FUN = function(x) { min(Lo(x))} )
  } else {
    avgHi <- period.apply(m,ep,FUN = function(x) { mean(Hi(x))} )
    avgLo <- period.apply(m,ep,FUN = function(x) { mean(Lo(x))} )
  }

  res           <- cbind(avgHi,avgLo)
  index(res)    <- index(m[sp,])
  colnames(res) <- c("H","L")

  res$avgH <- rollmean(res$H,n,align = "right")
  res$avgL <- rollmean(res$L,n,align = "right")

  res$H <- NULL
  res$L <- NULL

  return(res)
} #End of buildHiLo

#STRATEGY & BACKTEST
hma.strategy        <- function(pos,m,i) {
  ###############################################################################
  #This is the function where the strategy is applied for that candle
  # Resultant transactions are written to gTrxns.
  # Strategy:
  #  CheckExit to close the position. 
  #  checkOpen - if we need to open short/long position
  #  checkFilter - check if the actvity should be filtered.
  ###############################################################################
  n <- m[i,]
  pos <- hma.checkExit(pos,m,i)
  
  if (position.isClosed(pos)) {
    #browser()
    position.addTxn(pos,type = 'CLOSE')
    position.addTrade(pos)
    
    pos$openQty   <- pos$openQty - pos$closeQty
    pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
    
    position.adjustCapital()
    #Create new dummy position if position becomes complete zero
    if ( pos$openQty == 0 ){ pos <- position() }
  }

  pos <- hma.checkEntry(pos,m,i)
  #openNow will be True, if position to be opened. Otherwise this will be False
  if (pos$justOpened == TRUE){
    pos$openFlag <- TRUE
    pos <- hma.calcLimits(pos)
    position.addTxn(pos,type = 'OPEN')
  }

  pos
} #end of applyMyStrategy
hma.calcLimits      <- function(pos){
  #############################################################################
  # This function calculates the initial trail prices, slp prices, pb priceetc
  #############################################################################
  if (position.isLong(pos)){
    pos$trailPrice <- (1 - (gCapital$trlPct/100) ) * pos$openPrice
    pos$trailPrice <- round(pos$trailPrice,1)
    
    pos$slpPrice <- (1 - (gCapital$slpPct/100) ) * pos$openPrice
    pos$slpPrice <- round(pos$slpPrice,1)
    
    pos$pbPrice <- (1 + (gCapital$pbPct/100) ) * pos$openPrice
    pos$pbPrice <- round(pos$pbPrice,1)
    
    pos$pbLot <- min(pos$openQty,gCapital$pbQty)
  }
  
  if (position.isShort(pos)) {
    pos$trailPrice <- (1 + (gCapital$trlPct/100) ) * pos$openPrice
    pos$trailPrice <- round(pos$trailPrice,1)
    
    pos$slpPrice <- (1 + (gCapital$slpPct/100) ) * pos$openPrice
    pos$slpPrice <- round(pos$slpPrice,1)
    
    pos$pbPrice <- (1 - (gCapital$pbPct/100) ) * pos$openPrice
    pos$pbPrice <- round(pos$pbPrice,1)
    
    pos$pbLot <- min(pos$openQty,gCapital$pbQty)
  }
  return(pos)
} #end of calcLimits
hma.backtest        <- function(m) {
  ###############################################################################
  #This is the function where the strategy is applied for all the candles
  # Resultant transactions are written to gTrxns.
  # Strategy:
  #  CheckExit to close the position. 
  #  checkOpen - if we need to open short/long position
  #  checkFilter - check if the actvity should be filtered.
  ###############################################################################
  pos <- position()
  gTrxns <<- gTrxns[FALSE,]
  gTrades <<- gTrades[FALSE,]
  
  for (i in 3:nrow(m)){
    pos <- hma.strategy(pos,m,i)
    }
} # end of backtest

#PLOT
hma.plot       <- function(a,file=NULL,by = 'quarters'){
  ###############################################################################
  #This function plots the chartSeries & the longEntry,exits etc for each quarter
  # Input : p - price
  #         t - XTS with longentry,longexit,shortentry,shortexits
  #         f - filePath
  #         pE  - print Entries or exits flag.
  # Logic : merge the price & t
  #         divide it to quarters
  #         draw each quarter into a pdf
  #         return the pdf path
  # 
  ###############################################################################
  if (!is.OHLC(a)){
    stop("hma.trxn.plot - a is not OHLCV")
  }
  if(is.OHLCV(a)){
    p <- OHLC(a)
  } else {
    p <- OHLC(a)
  }
  
  if (by == "halfyear"){
    by <- "months"
    k  <- 6
  } else { 
    k <- 1
  }
  
  p  <- split.xts(p,f = by,k=k)
  p[length(p)] <- NULL
  
  eXall <- trades.entryExits()
  colnames(eXall) <- c("lE","lX","sE","sX")
  eXall[!is.na(eXall)] <- 1
  
  if (missing(file)){  file <- "tempGraph.pdf" }
  if (file.exists(file)) { 
    file.rename(file,paste(file,"bkup",sep = ""))
  }
  
  temp.plots <- vector(length(p),mode = 'list')
  for (i in 1:length(p)) {
    c  <- p[[i]]
    hw <- a[index(c)][,c("hiloHWkly")]
    lw <- a[index(c)][,c("hiloLWkly")]
    hma <- a[index(c)][,c("hmaLong")]
    ex <- eXall[index(c)]
    
    
    
    
    chartSeries(c,type = 'bars')
    plot(addTA(hw,type='l',on=1,col=c("blue")))
    plot(addTA(lw,type='l',on=1,col=c("orange")))
    plot(addTA(hma,type='l',on=1,col=c("purple")))
    
    
    x <- getTickedPrice(max(a[index(c)]$High),n=10)
    if (nrow(ex) > 0 ){
      plot(addTA(ex$lE*a$High + x, on=1, type='p', pch=24, cex=2,col="blue", bg="blue"))
      plot(addTA(ex$lX*a$High + x, on=1, type='p', pch=25, cex=2,col="blue", bg="blue"))
      plot(addTA(ex$sE*a$Low  - x, on=1, type='p', pch=24, cex=2,col="red",  bg="red"))
      plot(addTA(ex$sX*a$Low  - x, on=1, type='p', pch=25, cex=2,col="red",  bg="red"))
    }
    
    
    temp.plots[[i]] <- recordPlot()
    print(i)
  }
  
  #print to pdf
  pdf(file,onefile = TRUE)
  for(i in temp.plots){
    replayPlot(i)
  }
  graphics.off()
}

# ENTRY / EXIT CRITERIAS
hma.checkEntry      <- function(pos,m,i){
  ###############################################################################
  # Checks for position to open or not based on criteria
  # Returns :  position
  ###############################################################################
  entry <- DONOTOPEN
  n <- m[i,]
  pn <- m[(i-1),]
  
  hlHW     <- as.numeric(n$hiloHWkly)
  hlLW     <- as.numeric(n$hiloLWkly)
  hlHWSteps <- as.numeric(n$hiloHWklySteps)
  hlLWSteps <- as.numeric(n$hiloLWklySteps)
  
  highP    <- as.numeric(Hi(n))
  lowP     <- as.numeric(Lo(n))
  openP    <- as.numeric(Op(n))
  closeP   <- as.numeric(Cl(n))
  
  stochK  <- as.numeric(n$fastK)
  tickP   <- getTickedPrice(closeP,n=gTicks)
  
  hmaLong  <- as.numeric(n$hmaLong)
  hmaSlope <- as.numeric(n$hmaSlope)
  #If already Open, return not to enter new trade
  if (  position.isOpen(pos)  )  {
    pos$justOpened <- FALSE
    return(pos)
  }

  if ( (hlLWSteps == STEPSUP) &&
       (closeP >= hlHW)       &&
       (closeP >= hmaLong)    &&
       (stochK < 0.85 )       &&
       (TRUE) )
  {
    pos$justOpened  <- TRUE
    pos$openQty     <- gCapital$tradeQty
    pos$direction   <- 'LONG'
    pos$openPrice   <- closeP
    pos$openDate    <- index(n)
    pos$openReason  <- "Long Entry Condition - I"
    return(pos)
  }
  
  #Long Entry
  if ( isPriceXed(hlLW,n,d = LONG)  && 
       (TRUE)
  ){
    pos$justOpened  <- TRUE
    pos$openQty     <- gCapital$tradeQty
    pos$direction   <- 'LONG'
    pos$openPrice   <- closeP
    pos$openDate    <- index(n)
    pos$openReason  <- "Long Entry Condition - II"
    return(pos)
  }

  #Short Entry
  if ( (hlHWSteps == STEPSDOWN) &&
       (closeP    <= hlLW)      &&
       (closeP <= hmaLong)      &&
       (stochK > 0.15)          &&
       (TRUE)
  ){
    pos$justOpened  <- TRUE
    pos$openQty     <- gCapital$tradeQty
    pos$direction   <- 'SHORT'
    pos$openPrice   <- closeP
    pos$openDate    <- index(n)
    pos$openReason  <- "Short Entry Condition - I"
    return(pos)
  }
  
  #Short
  if ( isPriceXed(hlHW,n,d = SHORT)  &&
       (TRUE)
  ){
    pos$justOpened  <- TRUE
    pos$openQty   <- gCapital$tradeQty
    pos$direction <- 'SHORT'
    pos$openPrice <- closeP
    pos$openDate  <- index(n)
    pos$openReason <- "Short Entry Condition - II"
    return(pos)
  }
  
  return(pos)
  
} #End CheckEntry
hma.checkLongExit   <- function(pos,m,i){ 
  ###############################################################################
  #Checks if Long position would be closed or not in backtesting
  # Returns : position
  ###############################################################################
  n <- m[i,]
  pn <- m[(i-1),]
  
  hlH      <- as.numeric(n$hiloH)
  hlL      <- as.numeric(n$hiloL)
  hlHW     <- as.numeric(n$hiloHWkly)
  hlLW     <- as.numeric(n$hiloLWkly)
  
  highP    <- as.numeric(Hi(n))
  lowP     <- as.numeric(Lo(n))
  openP    <- as.numeric(Op(n))
  closeP   <- as.numeric(Cl(n))
  pCloseP  <- as.numeric(Cl(pn))
  
  hmaLong  <- as.numeric(n$hmaLong)
  tickP    <- getTickedPrice(closeP,n=gTicks)

  #If bar crosses downwards on weekly High
  if ( isPriceXed(hlHW - tickP,n,d = SHORT) ){
    pos$closePrice  <- closeP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Long Exit - bar crosses downwards on weekly High")
    return(pos)
  }
  
  #Profit booking & increasing the slp
  if (  isPriceHit(pos$pbPrice,n) ) {
    pos$closePrice  <- pos$pbPrice
    pos$pbCount     <- pos$pbCount + 1
    pos$closeQty    <- pos$pbLot
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Long Exit - %sth Profit booking",pos$pbCount)
    
    ##Adjust slp if incrslpflag is true. Otherwise fixed slp
    if ( gCapital$incrSlpFlag ){
      pos$slpPrice <- (1 - (gCapital$slpPct/100) ) * pos$pbPrice
      pos$slpPrice <- round(pos$slpPrice,1)
    }
    
    #Increase the pbPrice to next level
    pos$pbPrice     <- round((1+(gCapital$pbPct/100)) * pos$pbPrice,1)
    return(pos)
  }
  
  #Exit when open price itself is below slp
  if (  openP <= pos$slpPrice  ) {
    pos$closePrice  <- openP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Long Exit - slpPercent hit at open Price ")
    return(pos)
  } 
  
  #Exit when slp percent hit
  if (  isPriceHit(pos$slpPrice,n) ) {
    pos$closePrice  <- pos$slpPrice
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Long Exit - slpPercent hit ")
    return(pos)
  } 
  
  #Exit when price below Weekly hlL
  if ( isPriceXed(hlLW - tickP,n,d = SHORT) ) {
    pos$closePrice  <- closeP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Long Exit price below Weekly Low - hlLW")
    return(pos)
  }

  return(pos)
} #End checklongexit
hma.checkShortExit  <- function(pos,m,i){ 
  n <- m[i,]
  pn <- m[(i-1),]
  
  hlH      <- as.numeric(n$hiloH)
  hlL      <- as.numeric(n$hiloL)
  hlHW     <- as.numeric(n$hiloHWkly)
  hlLW     <- as.numeric(n$hiloLWkly)
  
  highP    <- as.numeric(Hi(n))
  lowP     <- as.numeric(Lo(n))
  openP    <- as.numeric(Op(n))
  closeP   <- as.numeric(Cl(n))
  pCloseP  <- as.numeric(Cl(pn))
  
  tickP    <- getTickedPrice(closeP,n=gTicks)
  hmaLong  <- as.numeric(n$hmaLong)
  
  #If bar crosses upwards on weekly Low
  if ( isPriceXed(hlLW+tickP,n,d = LONG) ){
    pos$closePrice  <- closeP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Short Exit - bar crosses upwards on weekly Low")
    return(pos)
  }

  #Profit booking & adjust slp at same time
  if (  isPriceHit(pos$pbPrice,n) ) {
    pos$closePrice  <- pos$pbPrice
    pos$pbCount     <- pos$pbCount + 1
    pos$closeQty    <- pos$pbLot
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Short Exit - %sth Profit booking",pos$pbCount)
    ##Adjust slp if incrslpflag is true. Otherwise fixed slp
    if ( gCapital$incrSlpFlag ){
      pos$slpPrice <- (1 + (gCapital$slpPct/100) ) * pos$pbPrice
      pos$slpPrice <- round(pos$slpPrice,1)
    }
    #Find the next profit booking price
    pos$pbPrice     <- round((1-(gCapital$pbPct/100)) * pos$pbPrice,1)
    return(pos)
  }

  #Exit - Open is more than stop loss price
  if (  openP >= pos$slpPrice ) {
    pos$closePrice  <- openP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Short Exit - Open is more than stop loss price")
    return(pos)
  }

  #Exit when slp percent hit
  if (  isPriceHit(pos$slpPrice,n) ) {
    pos$closePrice  <- pos$slpPrice
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Short Exit - Regular slp hit ")
    return(pos)
  }

  #Exit when price hits Weekly hlhigh
  if ( isPriceXed(hlHW + tickP,n,d = LONG) ) {
    pos$closePrice  <- closeP
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(n)
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("Short Exit - price above weekly high")
    return(pos)
  }

  return(pos)
} #End checkShortExit
hma.checkExit       <- function(pos,m,i){ 
  ###############################################################################
  #Checks for position to open or not based on criteria
  # Returns : TRUE - Exit the position
  #         : FALSE - do nothing
  ###############################################################################
  n <- m[i,]
  
  pos$closeFlag <- FALSE
  
  #If position is not open - return FALSE
  if (!position.isOpen(pos)){
    pos$closeFlag <- FALSE
    return(pos)
  }
  
  if (position.isLong(pos)){
    pos <- hma.checkLongExit(pos,m,i)
    return(pos)
  }
  
  if (position.isShort(pos)) {
    pos <- hma.checkShortExit(pos,m,i)
    return(pos)
  }
  
  return(pos)
} #End checkExit
hma.buildInput      <- function(){

  #krausz Hi Lo calculators of weekly
  hlw           <- hma.buildHiLo(d,n=hlNW,freq = "weeks",type = "hilo")
  hlw$avgHSteps <- rollapply(hlw$avgH,width = hlNW,FUN=findDirection,align = "right")
  hlw$avgLSteps <- rollapply(hlw$avgL,width = hlNW,FUN=findDirection,align = "right")
  colnames(hlw) <- c("hiloHWkly","hiloLWkly","hiloHWklySteps","hiloLWklySteps")
  hlw           <- lag.xts(hlw)
  hlw           <- applyWklyToDly(d,hlw)
  
  #Stochastic indicator for daily, all indicators
  ind             <- stoch(HLC(d),nFastK=hmaLN)
  colnames(ind)   <- c("fastK","fastD","lagD")
  ind$hmaLong     <- HMA(d$Close,n=hmaLN)
  ind$hmaSlope    <- ROC(ind$hmaLong,type = "discrete")*100
  #ind             <- lag.xts(ind)
  
  #hma
  all             <- merge.xts(d,hlw,ind)
  all             <- na.trim(all)
  return(all)
}        #Builds the input required for backtesting
