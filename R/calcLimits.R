#' calcLimits for the given position
#'  Calculates the profitbooking price, pB slp, regular SLP, trailing target & trailing SLP etc
#'
#' @note
#' 
#' @param pos - current position
#' @param tradeParms - current tradeParms
#' @param m  - dataset of the prices
#' @param i - the current bar in evaluation
#' @return the position with updated prices
#' @export
calcLimits      <- function(pos,tradeParms,m,i,...) {
  t <- tradeParms
  
  bar <- m[i,]
  pBar <- make.candle(m[((i-1):i),])
  
  ##  --- Set the Variables ---------------------------------------------------------
  Op <- as.numeric(bar$Open)
  Hi <- as.numeric(bar$High)
  Lo <- as.numeric(bar$Low)
  Cl <- as.numeric(bar$Close)
  
  ## --- For Long positions -------------------------------
  if ( islong(pos) ) {
    
    ## --- SLP ------------------------------------------------
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 - (t$slpAmt/100) ) ,
                            pos$openPrice - t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    
    
    
    
    ## --- PB Long ---------------------------------------------
    if ( is.na(pos$pbPrice) ) {
      pos$pbPrice  <- ifelse( t$pctFlag,
                              pos$openPrice * (1 + (t$pbAmt/100) )  ,
                              pos$openPrice + t$pbAmt )
      pos$pbPrice <- round(pos$pbPrice,2)
    } else {
      if (is.price.hit(pos$pbPrice,pBar) ) {
        temp  <- ifelse( t$pctFlag,
                         pos$pbPrice * (1 + (t$pbAmt/100) )  ,
                         pos$pbPrice + t$pbAmt )
        temp <- round(temp,digits = 2)
        pos$pbPrice <- max(pos$pbPrice,temp)
      }
    }
    
    
    
    ## --- TrailPrice, TrailSLP --------------------------------------
    #Set the trailPrice, trailSlp price initial ones
    #For every trlAmt move upwards, trlSlpamt also move upward.
    #initally trlSlp is same as regular slp
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 + (t$trlInitAmt/100) ) , 
                                pos$openPrice + t$trlInitAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
      
      #set first time trailing slp
      pos$trailSlpPrice <- ifelse( t$pctFlag,
                                   pos$trailPrice * (1 - (t$trlIncrAmt/100) ) , 
                                   pos$openPrice - t$trlIncrAmt)
    } else {
      if ( is.price.hit(pos$trailPrice,pBar) ) { pos$trailTrigFlag <- TRUE  }
      
      # If price hit the trail price, New trail Price will be moved up by pos$trailTrigFlag
      if ( (pos$trailTrigFlag) && (is.price.hit(pos$trailPrice,pBar)) ) {
        temp <- ifelse( t$pctFlag,
                        pos$trailPrice * (1 + (t$trlIncrAmt/100) ) , 
                        pos$trailPrice + t$trlIncrAmt)
        pos$trailPrice <- round(temp,digits = 2)
        temp  <- ifelse( t$pctFlag,
                         pos$trailSlpPrice * (1+t$trlIncrAmt/100),
                         pos$trailSlpPrice + t$trlIncrAmt )
        pos$trailSlpPrice  <- round(temp,digits = 2)
      }
    }#End of trailPrice
  } #end of isLong
  
  
  ## --- For Short positions -------------------------------
  if ( isshort(pos) ) {
    
    ## --- SLP ------------------------------------------------
    pos$slpPrice <- ifelse( t$pctFlag,
                            pos$openPrice * (1 + (t$slpAmt/100) ) ,
                            pos$openPrice + t$slpAmt )
    pos$slpPrice <- round(pos$slpPrice,2)
    
    
    ## --- PB Short ---------------------------------------------
    if ( is.na(pos$pbPrice) ) {
      pos$pbPrice  <- ifelse( t$pctFlag,
                              pos$openPrice * (1 - (t$pbAmt/100) )  ,
                              pos$openPrice - t$pbAmt )
      pos$pbPrice <- round(pos$pbPrice,2)
    } else {
      if (is.price.hit(pos$pbPrice,pBar) ) {
        temp  <- ifelse( t$pctFlag,
                         pos$pbPrice * (1 - (t$pbAmt/100) )  ,
                         pos$pbPrice - t$pbAmt )
        temp <- round(temp,digits = 2)
        pos$pbPrice <- max(pos$pbPrice,temp)
      }
    }
    
    
    ## --- TrailPrice, TrailSLP  Short --------------------------------------
    #Set the trailPrice, trailSlp price initial ones
    #For every trlAmt move upwards, trlSlpamt also move upward.
    #initally trlSlp is same as regular slp
    if ( is.na(pos$trailPrice)) {
      pos$trailPrice <- ifelse( t$pctFlag,
                                pos$openPrice * (1 - (t$trlInitAmt/100) ) , 
                                pos$openPrice - t$trlInitAmt)
      pos$trailPrice <- round(pos$trailPrice,digits = 2)
      
      #set first time trailing slp
      pos$trailSlpPrice <- ifelse( t$pctFlag,
                                   pos$trailPrice * (1 + (t$trlIncrAmt/100) ) , 
                                   pos$openPrice + t$trlIncrAmt)
      
      
    } else {
      if ( is.price.hit(pos$trailPrice,pBar) ) { pos$trailTrigFlag <- TRUE  }
      
      if ( (pos$trailTrigFlag) && (is.price.hit(pos$trailPrice,pBar)) ) {
        temp <- ifelse( t$pctFlag,
                        pos$trailPrice * (1 - (t$trlIncrAmt/100) ) , 
                        pos$trailPrice - t$trlIncrAmt)
        pos$trailPrice <- round(temp,digits = 2)
        
        
        temp  <- ifelse( t$pctFlag,
                         pos$trailSlpPrice * (1 - t$trlIncrAmt/100),
                         pos$trailSlpPrice - t$trlIncrAmt )
        pos$trailSlpPrice  <- round(temp,digits = 2)
      }
      
    }
    
    
  } #End of Short position adjustments
  
  return(pos)
}

