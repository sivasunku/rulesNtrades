
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

toFrame <- function(x){
  #converts xts to data frame
  data.frame(Date = index(x),coredata(x))
}
changePct <- function(from,to){
  #This function denormalizes the given value to original
  return((to - from)/abs(from)*100)
}

