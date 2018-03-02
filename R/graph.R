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
#' @parms = tra
#' @param  file      - filename where pdf to be stored. Please be advised, it will overwrite if any file exists.
#' @param  inTrades  - if given only these trades are graphed. portfolio is ignored.
#' @param  inpositions - positions table for every day
#' @param  overFun   - overFun is a overlay function that helps to overlay graph wiht any of the indicators or values. 
#' @details overFun - is a function supplied by user. This is executed in the graph function environment, so all the variables
#' available for the graph function can be used by overFun. However caution to be exercised not to stop the function abruptly
#' @rdname graph
#' @export
#' 
graph <- function(pf, 
                  prices, 
                  file = NULL, 
                  by = 'quarters',
                  parms = NULL,
                  inTrades = NULL,
                  inPositions = NULL,
                  overFun = NULL,...) {
  
  width <- 5
  if ( !is.valid.portfolio(pf) ) {
    stop("graph - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  if ( missing(inTrades) ){
    inTrades <- get.trades(pf)
  }
  if ( missing(inPositions)){
    inPositions <- get.positions.table(pf)
  }
  if (!is.null(file) && file.exists(file)) { 
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
  indexTZ(eXall) <- indexTZ(prices)
  
  #Add columns if not present
  cnames <- colnames(eXall)
  if (!("lE" %in% cnames) ) {eXall$lE <- NA}
  if (!("lX" %in% cnames) ) {eXall$lX <- NA}
  if (!("sE" %in% cnames) ) {eXall$sE <- NA}
  if (!("sX" %in% cnames) ) {eXall$sX <- NA}
  
  
  #Make the slp/pb price table
  pbPrices <- .makePBtable(inPositions,parms)
  
  pieces <- length(splitPrices)
  temp.plots <- vector(pieces,mode = 'list')
  
  p <- 0
  i <- 1
  for (i in 1:(pieces)){
    c  <- splitPrices[[i]]
    a  <- OHLC(splitPrices[[i]])
    ex <- eXall[index(c)]
    pb <- pbPrices[index(c)]
    
    #Draw the Longs
    chartSeries(c,type = 'bars')
    
    if ( (all( is.na(ex) ) != TRUE) ) {
      ex$lE  <- ex$lE * 40
      ex$lX  <- ex$lX * 30
      ex$sE  <- ex$sE * 20
      ex$sX  <- ex$sX * 10
      #ex[is.na(ex)] <- 0
      #plot(addTA(shortE, on=1, type='p', pch=24, cex=0.5,col="red",  bg="red"))
      
      #plot the buy/sell trades
      plot(addTA( ex,
                  pch = c(24,25,24,25),
                  col = c("blue","blue","red","red"),
                  bg  = c("blue","blue","red","red"),
                  cex = c(1,1,1,1),
                  type = c('p','p','p','p')
      ) )
    }
    plot(addTA(res[,c("bullFlow","bearFlow")],col=c("green","red")))
    
    #plot the PB price, trail price etc
    plot(addTA(pb[,c("pbPrice","slpPrice","trailSlpPrice","trailPrice")],
               col = c("green","red","blue","orange"),
               type = c('o','o','o','o')
               #on = 3
    ))
    temp.plots[[i]] <- recordPlot()
  }
  
  #print to pdf
  if ( !is.null(file) ) {
    pdf(file,onefile = TRUE)
    for(i in temp.plots){
      replayPlot(i)
    }
    graphics.off()
  } 
  
} #end of function graph

#' @details .entryexit - internal function to return 4 columns with '1' in corresponding rows where trade is done.
#' @param  t - trades
#' @param  p - tradeparms
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

#' @details .makePBtable - internal function to return profit booking,slp, trailing prices
#' @param  p - trades
#' @param parms - tradeparms
#' @return returns xts which has 4 columns - lE,lX,sE,sX longEntry,longExit,shortEntry,shortExit etc
#'
.makePBtable <- function(p,parms){
  temp <- p[c("onDate","pbPrice","slpPrice","trailPrice","trailSlpPrice")]
  if (anyDuplicated(temp$onDate) > 0){
    stop("in makePBtable index is duplicated")
  }
  res <- xts(x = temp[,2:5],order.by = temp[,1])
  if (parms$pbFlag == FALSE)     {  res$pbPrice <- NA  }
  if (parms$slpFlag == FALSE)    {  res$slpPrice <- NA }
  if (parms$trlFlag == FALSE)    {  res$trailPrice <- res$trailSlpPrice <- NA }
  
  return(res)
}
