#' graph - plot the graph into a given pdf
#' 
#' This will plot the OHLC bars & corresponding trades performed. 
#' 
#' Green uptriangle & Green downtriangle represents the Long Entry & Long Exits. Longs are drawn above the corresponding bar.
#' 
#' Red   uptriangle & red   downtriangle represents the short Entry & short Exits. Shorts are drawn below the corresponding bar.
#'
#' @author Siva Sunku
#' @keywords graph,plot
#' @note
#' 
#' @param  pf - portfolio which needs to be graphed
#' @param  prices    - OHLC of the shares/equity
#' @param  by        - frequncy for each page (whether monthly, weekly, quarterly) etc. 
#' e.g. monthly makes each page of pdf to contain one months data. 
#' All values accepted by endpoints function are available + halfyear. default - quarters.
#' @param  file      - filename where pdf to be stored. Please be advised, it will overwrite if any file exists.
#' @param  inTrades  - if given only these trades are graphed. portfolio is ignored.
#' @param  overFun   - overFun is a overlay function that helps to overlay graph wiht any of the indicators or values. 
#' @details overFun - is a function supplied by user. This is executed in the graph function environment, so all the variables
#' available for the graph function can be used by overFun. However caution to be exercised not to stop the function abruptly
#' @rdname graph
#' @export
#' 
graph <- function(pf, prices, file, by = 'quarters',inTrades = NULL,overFun = NULL,...) {
  
  width <- 5
  if ( !is.valid.portfolio(pf) ) {
    stop("graph - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  if ( missing(inTrades) ){
    inTrades <- get.trades(pf)
  }
  if (file.exists(file)) { 
    msg <- paste( "File", file, " Exists already. It is overwritten",sep="")
    warnings(msg)
  }
  if ( !missing(overFun) && !is.function(overFun) ){
    stop("graph - overFun should be a function")
  }
  k <- 1
  if (by == "halfyear"){
    by <- "months"
    k  <- 6
  }
  
  splitPrices  <- split.xts(prices,f = by, k=k)

  #Make EntryExit table
  eXall <- .entryexit(inTrades,prices)

  temp.plots <- vector(length(splitPrices),mode = 'list')
  for (i in 1:length(splitPrices)) {
    c  <- splitPrices[[i]]
    a  <- OHLC(splitPrices[[i]])
    ex <- eXall[index(c)]
    chartSeries(c,type = 'bars')
    
    if ( all( is.na(ex$lE) ) != TRUE ){
      plot(addTA(ex$lE*a$High + width, on=1, type='p', pch=24, cex=2,col="blue", bg="blue"))
    }
    if ( all( is.na(ex$lX) ) != TRUE ){
      plot(addTA(ex$lX*a$High + width, on=1, type='p', pch=25, cex=2,col="blue", bg="blue"))
    }
    
    if ( all( is.na(ex$sE) ) != TRUE ){
      plot(addTA(ex$sE*a$Low  - width, on=1, type='p', pch=24, cex=2,col="red",  bg="red"))
    }
    if ( all( is.na(ex$sX) ) != TRUE ){
      plot(addTA(ex$sX*a$Low  - width, on=1, type='p', pch=25, cex=2,col="red",  bg="red"))
    }

    if ( !missing(overFun) ){
      environment(overFun) <- environment()
      overFun()
    }
    
    temp.plots[[i]] <- recordPlot()
  }
  
  #print to pdf
  pdf(file,onefile = TRUE)
  for(i in temp.plots){
    replayPlot(i)
  }
  graphics.off()
  
} #end of function graph


#' @details .entryexit - internal function to return 4 columns with '1' in corresponding rows where trade is done.
#' @param  t - trades
#' @param  p - prices,OHLC
#' @return returns xts which has 4 columns - lE,lX,sE,sX longEntry,longExit,shortEntry,shortExit etc
#'
.entryexit <- function(t,p){
  
  res <- xts( order.by = index(p) )
  
  #long Trades
  lT <- t[t$direction == "LONG",]
  sT <- t[t$direction == "SHORT",]
  
  if (nrow(lT) > 0 ){
    #Long Entries
    lE <- xts(order.by = lT$entryTime)
    lE <- merge(lE,lE = 1)
    
    #Long Exits
    lX <- xts(order.by = lT$exitTime)
    lX <- merge(lX,lX = 1)
    
    res <- merge.xts(res,lE,lX)
  }
  
  if(nrow(sT) > 1){
    #Short Entries
    sE <- xts(order.by = sT$entryTime)
    sE <- merge(sE,sE = 1)
    
    #short Exits
    sX <- xts(order.by = sT$exitTime)
    sX <- merge(sX,sX = 1)
    res <- merge.xts(res,sE,sX)
  }
  
  return(res)
}
