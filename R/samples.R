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

