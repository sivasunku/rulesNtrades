#' The sample programs for longEntry,longExit,shortEntry,shortExit & calcLimits are given.
#' These can be copied and changed for specific logic.
#'
#' @note
#' @export
longEntrySample <- function(parms,m,i,...) {
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  cPos <- position(instr = parms$instrument)
  
  ##  --- Check the Entry Condition -------------------------------------------------
  if (bu > be){
    cPos$openDate   <- index(bar)
    cPos$direction  <- "LONG"
    cPos$justOpened <- TRUE
    cPos$openPrice  <- Cl
    cPos$openQty    <- parms$qty
    cPos$openFlag   <- TRUE
    cPos$openReason <- "Case#1"
    cPos$openCase   <- "Case#1"
    return(cPos)
  }
  
  ## --- End -----
  return(cPos)
}

#' @export
longExitSample <- function(cPos,parms,m,i,...){
  
  if (! isopen(cPos) ) { return(cPos)}
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  
  
  ##  --- Actual Inidcator check ---------------------------------------------------------
  if (bu < be){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Cl
    cPos$closeReason <- "Case#1"
    return(cPos)
  }
  
  
  ##  --- SLP check ---------------------------------------------------------
  if ( (parms$slpFlag) && (Lo < cPos$slpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "SLP hit"
    return(cPos)
  }
  
  ##  --- Trailing SLP check -------------------------------------------------
  if ( (parms$trlFlag) && (Lo < cPos$trailSlpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$trailSlpPrice
    cPos$closeReason <- "TrailingSLP hit"
    return(cPos)
  }
  
  ##  --- Profit Booking check -------------------------------------------------
  if ( (parms$slpFlag) && (Hi > cPos$pbPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$pbQty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "Profit Booking"
    return(cPos)
  }
  
  return(cPos)
}

#' @export
shortEntrySample <- function(parms,m,i,...){
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  cPos <- position(instr = parms$instrument)
  
  ##  --- Check the Short Entry Condition -------------------------------------------------
  if (be > bu){
    cPos$openDate   <- index(bar)
    cPos$direction  <- "SHORT"
    cPos$justOpened <- TRUE
    cPos$openPrice  <- Cl
    cPos$openQty    <- parms$qty
    cPos$openFlag   <- TRUE
    cPos$openReason <- "Case#1"
    cPos$openCase   <- "Case#1"
    return(cPos)
  }
  
  ## --- Return --------------
  return(cPos)
  
}

#' @export
shortExitSample <- function(cPos,parms,m,i,...){
  if (! isopen(cPos) ) { return(cPos)}
  
  ##  --- Set the Variables ---------------------------------------------------------
  bar <- m[i,]
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  bu <- as.numeric(m[i,]$bullFlow)
  be <- as.numeric(m[i,]$bearFlow)
  
  
  ##  --- Actual Inidcator check ---------------------------------------------------------
  if (bu > be){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- Cl
    cPos$closeReason <- "Case#2"
    return(cPos)
  }
  
  ##  --- SLP check ---------------------------------------------------------
  if ( (parms$slpFlag) && (Hi > cPos$slpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "SLP hit"
    return(cPos)
  }
  
  ##  --- Trailing SLP check -------------------------------------------------
  if ( (parms$trlFlag) && (Hi > cPos$trailSlpPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$qty
    cPos$closePrice <- cPos$trailSlpPrice
    cPos$closeReason <- "TrailingSLP hit"
    return(cPos)
  }
  
  ##  --- Profit Booking check -------------------------------------------------
  if ( (parms$slpFlag) && (Lo < cPos$pbPrice) ){
    cPos$closeDate  <- index(bar)
    cPos$closeFlag  <- TRUE
    cPos$closeQty   <- parms$pbQty
    cPos$closePrice <- cPos$slpPrice
    cPos$closeReason <- "Profit Booking"
    return(cPos)
  }
  
  ## ---- Return ----------
  return(cPos)
}

#' @export
calcLimitsSample      <- function(pos,tradeParms,bar,...) {
  t <- tradeParms
  ##  --- Set the Variables ---------------------------------------------------------
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  ## --- For Long positions -------------------------------
  if ( islong(pos) ) {
    ## SLP
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 - (t$slpAmt/100) ) ,
                            pos$openPrice - t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    
    ## PB
    pos$pbPrice  <- ifelse( t$pctFlag,
                            pos$openPrice * (1 + (t$pbAmt/100) )  ,
                            pos$openPrice + t$pbAmt )
    pos$pbPrice <- round(pos$pbPrice,2)
    
    
    
    #Initial trailing Price os calculated based on the openPrice, subsequent once based on the existing trailingPrice.
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 + (t$trlAmt/100) ) , 
                                pos$openPrice + t$trlAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
    } else {
      #This is to be trailed only if Price is hit by the candle.
      if ( Hi >= pos$trailPrice) {
        temp <- ifelse( t$pctFlag,
                        pos$trailPrice * (1 + (t$trlAmt/100) ) , 
                        pos$trailPrice + t$trlAmt)
        pos$trailPrice <- round(temp,digits = 2)
      }
    } #trailPrice End
    
    pos$trailSlpPrice <- ifelse( t$pctFlag,
                                 pos$trailPrice * (1 - (t$trlSlpAmt/100)),
                                 pos$trailPrice - t$trlSlpAmt )
    pos$trailSlpPrice <- round(pos$trailSlpPrice,digits = 2)
    
  }
  
  ## --- For Short positions -------------------------------
  if ( isshort(pos) ) {
    ## SLP
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 + (t$slpAmt/100) ) ,
                            pos$openPrice + t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    
    ## PB
    pos$pbPrice  <- ifelse( t$pctFlag,
                            pos$openPrice * (1 - (t$pbAmt/100) )  ,
                            pos$openPrice - t$pbAmt )
    pos$pbPrice <- round(pos$pbPrice,2)
    
    
    #Initial trailing Price os calculated based on the openPrice, subsequent once based on the existing trailingPrice.
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 - (t$trlAmt/100) ) , 
                                pos$openPrice - t$trlAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
    } else {
      #This is to be trailed only if Price is hit by the candle.
      if ( Lo <= pos$trailPrice) {
        temp <- ifelse( t$pctFlag,
                        pos$trailPrice * (1 - (t$trlAmt/100) ) , 
                        pos$trailPrice - t$trlAmt)
        pos$trailPrice <- round(temp,digits = 2)
      }
    } #trailPrice End
    
    pos$trailSlpPrice <- ifelse( t$pctFlag,
                                 pos$trailPrice * (1 + (t$trlSlpAmt/100)),
                                 pos$trailPrice + t$trlSlpAmt )
    pos$trailSlpPrice <- round(pos$trailSlpPrice,digits = 2)
    
  } #End of Short position adjustments
  
  return(pos)
}
