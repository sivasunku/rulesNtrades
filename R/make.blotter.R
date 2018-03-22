#' make.blotter - Add all the transactions to a blotter package
#' 
#' This will add the transactions in a given rulesNtrades portfolio to blotter package
#'  If you want to remove anything from blotter, use below commans
#'  rm(list=ls(envir=.blotter), envir = .blotter) 
#'  rm("portfolio.CUD",pos=.blotter)
#'  rm("account.CUD",pos=.blotter)  
#'  rm("order_book.CUD",pos=.strategy)  
#' 
#' @author Siva Sunku
#' @keywords graph,plot
#' @note
#' @param in.port - rulesNtrades portfolio from which transactions to be obtained(input)
#' @param out.port - blotter portfolio to which rules to be added
#' @param cur - currency . Usually leave it as "USD"
#' @param symbol - stock which needs to be added (It won't filter the symbols from in.port. Create a different portfolio in rulesNtrades for each)
#' @param forceClose - Default FALSE. if already out.port is there, delete the portfolio. Remember all portfolios are deleted with this.
#' 
#' @return portfolio name that has all the trades,transactions.
#' 
#' @export
make.blotter <- function(in.port, 
                         out.port = "temp.blot",
                         cur = "USD",
                         symbol = "SBIN",
                         initEq = 100000,
                         initDate='1950-12-31',
                         verboseFlag = TRUE,
                         forceClose = FALSE){
  
  if( is.portfolio(out.port) ) {
    if ( forceClose == TRUE){
      rm(list=ls(envir=.blotter,pattern = out.port), envir = .blotter)
      rm(list=ls(envir=.strategy,pattern = out.port), envir = .strategy)
    }
    tempStr <- sprintf("%s - portfolio is already present. Delete it before proceeding",out.port)
    stop(tempStr)
  }
  
  currency(cur)
  stock(symbol, currency=cur, multiplier=1)
  initPortf(out.port, symbols=symbol, initDate=initDate)
  initAcct(out.port, portfolios=out.port, initDate=initDate)
  initOrders(portfolio=out.port, initDate=initDate)
  initDate='1950-12-31'
  tx <- get.trxns(in.port)
  #Check if apply function can be used. They are losing the consistency
  for ( i in 1:nrow(tx) ){
    x <- tx[i,]
    addTxn(out.port,
           Symbol    = x$symbols,
           TxnDate   = x$date,
           TxnPrice  = x$price,
           TxnQty    = ifelse(x$type == "SHORT",-1 * x$qty, x$qty),
           TxnFees   = -1 * x$fees,
           verbose  = verboseFlag
    )
    print(i)
  }
  #apply(tx,MARGIN = 1,FUN=addTxn.blotter,out.port)
  #addTxn(out.port, Symbol='SBIN',TxnDate='2016-03-21', TxnPrice=196.75, TxnQty = -3000,TxnFees=0,verbose=TRUE)
  return(out.port)
}

 