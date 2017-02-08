#' backtest the given strategies & provide the 
#'
#' All the rules that needs to be tested for evaluating a strategy. It initializes the trade table, trxn table.
#' Requires 4 functions as default for executing longEntry,longExit, shortEntry & shortExit trades.
#' Intra day conditions needs to be evaluated within the function code.
#' 
#' @author Siva Sunku
#' @keywords backtest
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
#' @param ...     - Can pass instrument names like instr & other things needed by longE,shortE functions etc.
#' @return portfolio name that has all the trades,transactions.
#' 
#' @export
backtest  <- function(pf,dataset,parms,startat = 2,
                      longE,longX,
                      shortE,shortX,
                      ...) {
  if (!is.OHLC(dataset) || !is.xts(dataset) ){
    stop("backtest - dataset is not OHLC")
  }
  maxR <- nrow(dataset)
  i <- max(2,startat)
  
  #default the instrument
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  #Test the long positions
  pos <- position(instr)
  for (i in 1:maxR){
    if ( isopen(pos) ){
      pos$barCount <- pos$barCount + 1
      pos <- longX(pos,dataset,i,...)
      if ( isclosed(pos) ) {
        add.trxns.position(pf,pos,type = 'CLOSE')
        add.trades.position(pf,pos)
        
        pos$openQty   <- pos$openQty - pos$closeQty
        pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
        #Create new dummy position if position becomes complete zero
        if ( isclosed(pos) ){ pos <- position(instr) }
      }
    }  
    
    tempPos <- longE(dataset,i,...)
    if ( (!isopen(pos)) && islong(tempPos) ){
      pos <- tempPos
      add.trxns.position(pf,tempPos,type = 'OPEN')
    }
    
    # if ( islong(pos) && islong(tempPos) && (tradeParms$pyramidFlag == TRUE) ){
    #   pod <- pos + tempPos
    #   add.trxns.position(pf,tempPos,type = 'OPEN')
    # }
    pos <- .calcLimits(pos,parms)
  }

  #Test the Short positions
  pos <- position(instr)
  for (i in 1:maxR){
    if ( isopen(pos) ){
      pos$barCount <- pos$barCount + 1
      pos <- shortX(pos,dataset,i,...)
      if ( isclosed(pos) ) {
        add.trxns.position(pf,pos,type = 'CLOSE')
        add.trades.position(pf,pos)
        
        pos$openQty   <- pos$openQty - pos$closeQty
        pos$closeFlag <- ifelse(pos$openQty == 0,TRUE,FALSE)
        #Create new dummy position if position becomes complete zero
        if ( isclosed(pos) ){ pos <- position(instr) }
      }
    }  
    
    tempPos <- shortE(dataset,i,...)
    if ( (!isopen(pos)) && isshort(tempPos) ){
      pos <- tempPos
      add.trxns.position(pf,tempPos,type = 'OPEN')
    }
    
    pos <- .calcLimits(pos,parms)
  }
}

.calcLimits      <- function(pos,tradeParms) {
  if ( islong(pos) ) {
    pos$trailPrice <- (1 - (tradeParms$trlPct/100) ) * pos$openPrice
    pos$trailPrice <- round(pos$trailPrice,1)
    
    pos$slpPrice <- (1 - (tradeParms$slpPct/100) ) * pos$openPrice
    pos$slpPrice <- round(pos$slpPrice,1)
    
    pos$pbPrice <- (1 + (tradeParms$pbPct/100) ) * pos$openPrice
    pos$pbPrice <- round(pos$pbPrice,1)
    
    pos$pbLot <- min(pos$openQty,tradeParms$pbQty)
  }
  
  if ( isshort(pos) ) {
    pos$trailPrice <- (1 + (tradeParms$trlPct/100) ) * pos$openPrice
    pos$trailPrice <- round(pos$trailPrice,1)
    
    pos$slpPrice <- (1 + (tradeParms$slpPct/100) ) * pos$openPrice
    pos$slpPrice <- round(pos$slpPrice,1)
    
    pos$pbPrice <- (1 - (tradeParms$pbPct/100) ) * pos$openPrice
    pos$pbPrice <- round(pos$pbPrice,1)
    
    pos$pbLot <- min(pos$openQty,tradeParms$pbQty)
  }
  return(pos)
}
