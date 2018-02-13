#' backtest the given strategies & provide the 
#'
#' @note
#' 
#' @param pf - portfolio to be tested
#' @param dataset - OHLCV with price objects, preferably xts
#' @param parms      - parameters defining slp, profit booking etc. see tradeparms function
#' @param startat    - From which row of the dataset to start testing. default 3
#' @param longE - A function which has conditions for long entry
#' @param longX  - A function that has conditions for long exit
#' @param shortE - A function which has conditions for short entry
#' @param shortX  - A function that has conditions for short exit
#' @param calcLimits - This is sample function given in this package, this can be overwritten by providing as parameter
#' @param ...     - Can pass instrument names like instr & other things needed by longE,shortE functions etc.
#' @return portfolio name that has all the trades,transactions.
#' 
#' @export
backtest  <- function(pf,dataset,parms,startat = 2,
                      longE,longX,
                      shortE,shortX,
                      calcLimits = calcLimits,
                      ...) {
  if (!is.OHLC(dataset) || !is.xts(dataset) ){
    stop("backtest - dataset is not OHLC")
  }
  maxR <- nrow(dataset)
  i <- max(2,startat)
  
  #default the instrument
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  ## --- Long Positions Check --------------------------------------------------
  if (parms$longTrades) {
    pos <- position(instr)
    for (i in 1:maxR){
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- longX(pos,parms,dataset,i,...)
        if ( isclosed(pos) ) {
          add.trxns.position(pf,pos,type = 'CLOSE')
          add.trades.position(pf,pos)
          
          pos$openQty   <- pos$openQty - pos$closeQty
          pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
          #Create new dummy position if position becomes complete zero
          if ( isclosed(pos) ){ pos <- position(instr) }
        }
      }  
      
      tempPos <- longE(parms,dataset,i,...)
      if ( (!isopen(pos)) && islong(tempPos) ){
        pos <- tempPos
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      
      # if ( islong(pos) && islong(tempPos) && (tradeParms$pyramidFlag == TRUE) ){
      #   pod <- pos + tempPos
      #   add.trxns.position(pf,tempPos,type = 'OPEN')
      # }
      pos <- calcLimits(pos,parms,dataset[i,])
    } #End of Long positions For loop
  }

  ## --- Short Positions Check --------------------------------------------------
  if (parms$shortTrades) {
    pos <- position(instr)
    for (i in 1:maxR){
      if ( isopen(pos) ){
        pos$barCount <- pos$barCount + 1
        pos <- shortX(pos,parms,dataset,i,...)
        if ( isclosed(pos) ) {
          add.trxns.position(pf,pos,type = 'CLOSE')
          add.trades.position(pf,pos)
          
          pos$openQty   <- pos$openQty - pos$closeQty
          pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
          #Create new dummy position if position becomes complete zero
          if ( isclosed(pos) ){ pos <- position(instr) }
        }
      }  
      
      tempPos <- shortE(parms,dataset,i,...)
      if ( (!isopen(pos)) && isshort(tempPos) ){
        pos <- tempPos
        add.trxns.position(pf,tempPos,type = 'OPEN')
      }
      
      pos <- calcLimits(pos,parms,dataset[i,])
    }
  }
}

#' Calculate the price limits like slp, trailing slp, etc
#' @export
calcLimits      <- function(pos,tradeParms,bar,...) {
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
