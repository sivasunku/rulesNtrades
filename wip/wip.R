findRangeFlag <- function(p,r, t = 0){
  ###############################################################################
  # For a given xts finds the pct of periods within the range xts
  # p - price , range - ranges, t - ticks that can be applied to see if in range
  # Returns : xts to say if it is within range or not
  ###############################################################################
  tickP <- 0
  if (t != 0 ) {
    tickP <- getTickedPrice(p,t)
  }
  if (!is.xts(p)){
    stop("In findRange p is not xts")
  }
  if (ncol(p) > 1){
    stop("In findRange p col is more than 1")
  }
  if (ncol(r) != 2){
    stop("In findRange r col is not 2")
  }
  if (nrow(p) != nrow(r)){
    stop("In findRange p,r have different rows")
  }
  
  colnames(p) <- c("Price")
  #Find which is ulimit/llimit in r
  if ( max(r[,1] > r[,2]) ){
    colnames(r) <- c("Ulimit","Llimit")
  }else{
    colnames(r) <- c("Llimit","Ulimit")
  }
  
  #Increase the ulimit by ticks & reduces the llimit by ticks
  r$Ulimit  <- r$Ulimit + tickP
  r$Llimit  <- r$Llimit - tickP
  
  temp <- merge.xts(r,p)
  temp$rangeFlag <- NA
  temp$rangeFlag <- ifelse( (temp$Price <= temp$Ulimit) & (temp$Price >= temp$Llimit),0,NA)
  temp$rangeFlag <- ifelse( is.na(temp$rangeFlag) & (temp$Price < temp$Llimit), -1, temp$rangeFlag)
  temp$rangeFlag <- ifelse( is.na(temp$rangeFlag) & (temp$Price > temp$Ulimit),  1, temp$rangeFlag)
  
  return (temp$rangeFlag)
}
findRangePct <- function(r,v = 0,n=13){
  ###############################################################################
  # For a given range r, finds the pct of v alue in n periods
  # Returns : xts to say if it is within range or not
  ###############################################################################
  if (!is.xts(r)){
    stop("In findRangePct r is not xts")
  }
  if (ncol(r) > 1){
    stop("In findRange p col is more than 1")
  }
  res <- xts(order.by = index(r))
  res <- merge.xts(res,count = NA)
  for ( i in n:nrow(r)){
    res[i,]$count <- sum(ifelse(temp[(i-n+1):i,]$rangeFlag == v,1,0)) / n
  }
  return(res * 100) 
}

