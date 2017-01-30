isPriceHit <- function(p,c){
  ###############################################################################
  #Checks if the given price has been hit, which means you could have bought/sold
  #  the stock with the given price
  #   p - price, c - candle
  # Returns : TRUE - if price is in range, FALSE otherwise
  ###############################################################################
  if (!is.OHLC(c))  {
    stop("In priceHit: c is not a OHLC candle")
  }
  
  l <- as.double(Lo(c))
  h <- as.double(Hi(c))
  return ( (p <= h) && (p >= l) )
} #End of ispricehit
isPriceXed <- function(p,c,d=1){
  ###############################################################################
  #Checks if the given price is crossed by a candle in the given direction
  # Input: p - price, c - candles, d - direction (1 - LONG,-1 SHORT),
  # Returns : TRUE - if the price crossed 
  ###############################################################################
  if ( !is.OHLC(c) )  {
    stop("In priceXed: c is not a OHLC candle")
  }
  if (!is.numeric(p)){
    stop("In priceXed: p is not numeric")
  }
  if ( (!is.numeric(d)) ||  ( (d != 1) && (d != -1) ) ){
    stop("In priceXed:d is not ideal")
  }
  
  c <- OHLC(c)
  l <- min(c)
  h <- max(c)
  
  if ( (l<=p) && (h>=p) ){
    if ( (d == LONG)  && (Cl(c) >= p) ) { return (TRUE) }
    if ( (d == SHORT) && (Cl(c) <= p) ) { return (TRUE)  }
  }
  return(FALSE)
}#End of ispricexed
findSteps <- function(x,p=0.5){
  #This function will return 1,0,-1 based on the price sequence up or down
  # If t+1 > t with 0.5%, then return STEPUP,
  # If t+1 < t with 0.5% then return STEPSDOWN, else ZERO
  p <- try.xts(p, error = as.matrix)
  if (is.xts(p)) {
    t <- ROC(p,type = "discrete")*100
    colnames(t) <- t("col1")
    t$steps <- NA
    t[t$col1>n,"steps"] <- STEPSUP
    t[t$col1<(-1*n),"steps"] <- STEPSDOWN
    t[is.na(t$steps),"steps"] <- 0
    return(t$steps)
  }
  else {
    t <- ROC(p,type = "discrete")*100
    t[t>n,] <- STEPSUP
    t[t<(-1*n),] <- STEPSDOWN
    t[is.na(t)] <- 0
    return(t)
  }
} #End of findSteps
getTradeLot <- function(equity = 100000L){
  ###############################################################################
  #This will return the number of lots to be traded
  ###############################################################################
  x <- 1
  x
}
getTickedPrice <- function(p,n=2){
  ###############################################################################
  #This will return price after considering ticks
  # Input: p - price, n- No. of ticks, 
  #  gives the ticked price for a given price to be considered
  ###############################################################################
  
  if (! is.numeric(n)){
    stop("in getTickedPrice , n is not ideal")
  }
  if (! is.numeric(p)){
    stop("in getTickedPrice , p is not ideal")
  }
  
  if ( p > 5000 ){
    t <- 1
  } else if (p>1000){
    t <- 0.75
  } else if (p>500){
    t <- 0.5
  } else if (p>250){
    t <- 0.2
  } else if (p>100){
    t <- 0.1
  } else {
    t <- 0.05
  }
  return(t*n)
} #end of gettickedprice
lastDayOfMth <- function(x , d = NULL){
  #############################################################################
  #getLastDayinMth - This gets the Last day in that particular month
  # say - Last Thursday in that month
  #############################################################################
  #d - is the number 01 - 07 (00 - Sun , 07 - Sat)
  if ((( d <= 0) || ( d >= 8)) && (!missing(d))) {
    stop("d should be 01 - 07 in getLastDayInMth")
  }
  if ( !is.Date(x)){
    stop("x should be a valid date in lastDayofMonth")
  }
  
  f <- floor_date(x,"month")
  f <- f + months(1)
  if ( missing(d)){
    b <- 1
  } else {
    b <- (wday(f) - d)
    if ( b <= 0) { b <- b + 7 }
  }
  f <- (f - days(b))
  return(f)
}
weekNoOfMth <- function(x){
  #############################################################################
  #getWeekDayinMth - This gets the Last day in that particular month
  # say - Last Thursday in that month
  #############################################################################
  #if ( !is.Date(x)){
  #  stop("x should be a valid date in weekNoOfMth")
  #}
  as.numeric(format(x,"%U")) - as.numeric(format(floor_date(x,"month"),"%U")) + 1
}
price.isLess <- function(x,y,p = NULL){
  #This function returns TRUE if x is less than (y - 0.1%y). 
  #This add porosity of 0.1% by default
  if (missing(p)) {
    p <- gPorosity
  }
  x <- as.numeric(x)
  y <- as.numeric(y)
  return(x < (100 - p)/100 * y)
}
price.isGreat <- function(x,y,p = NULL){
  #This function returns TRUE if x is less than (y - 0.1%y). This add porosity of 0.1% by default
  if (missing(p)) {
    p <- gPorosity
  }
  x <- as.numeric(x)
  y <- as.numeric(y)
  return(x > (100 + p)/100 * y)
}
toFrame <- function(x){
  #converts xts to data frame
  data.frame(Date = index(x),coredata(x))
}
normalize <- function(value,min=NULL,max=NULL){
  #Function to normalize the data
  x <- max(value,na.rm = T) 
  m <- min(value,na.rm = T)
  (value - m) / (x-m)
}
denormalize <- function(value,min,max){
  #This function denormalizes the given value to original , used for ANN
  value * (max - min) + min
}
changePct <- function(from,to){
  #This function denormalizes the given value to original
  return((to - from)/abs(from)*100)
}
applyWklyToDly <- function(d,w,parms = NULL){
  ###############################################################################
  # This function will apply higher interval to lower interval
  # Input: daily data, weekly data
  ###############################################################################
  #Make a new table with start and end dates of larger time series
  t       <- xts(order.by = index(w))
  t$start <- as.numeric( as.POSIXct(index(w)) )
  t$end   <- lag.xts(t$start,k = -1)
  
  #when lagged, last entry will be NA. Add the difference unit to last row
  diff <- as.numeric(t[nrow(t)-1,]$end  - t[nrow(t)-1,]$start)
  t[nrow(t),]$end <- t[nrow(t),]$start + diff
  
  if (missing(parms)){
    parms <- c(colnames(w))
  }
  
  res <- xts(order.by = index(d),tzone = Sys.timezone())
  for (i in 1:length(parms)){
    res <- merge(res,NA)
  }
  colnames(res) <- parms
  
  maxWklyrow <- nrow(w)
  maxDlyrow  <- nrow(d)
  wklyRow <- 1
  dlyRow <- 1
  
  for(wklyRow in 1:maxWklyrow){
    st <- as.numeric(t[wklyRow,]$start)
    en <- as.numeric(t[wklyRow,]$end)
    dt <- as.numeric( as.POSIXct(index(d[dlyRow,])) )
    if ((dt >= st )  && (dt < en) ){
      res[dlyRow,parms] <- w[wklyRow,parms]
    }
    while ( (dlyRow < maxDlyrow) && (dt < en) ){
      dlyRow <- dlyRow + 1
      dt <- as.numeric(as.POSIXct(index(d[dlyRow,])))
      if ((dt >= st )  && (dt < en) ){
        res[dlyRow,parms] <- w[wklyRow,parms]
      }
    }
  }# end of for loop
  return(res)
}
