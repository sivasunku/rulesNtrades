#' Trading parameters 
#'
#' All the trading parameters like slp, trailing slp etc. These are used in backtesting. Any parameters needed for execution
#' to be added and used in the functions.
#' 
#' @author Siva Sunku
#' @keywords parms
#' @note
#' 
#' @details  - tradeparms - returns the trading parameters required
#' @rdname tradeparms
#' @return list object
#' @export
tradeParms <- function(descr){
  t <- list( instrument   = "SBIN", #Name of the stock
             incrSlpFlag  = TRUE,   #Incrase the slp according to trl Pct
             pyramidFlag  = FALSE,  #Do you want to add to existing position
             qty = 80,              #trading quantity
             longTrades  = TRUE,    #If long trades to be permitted
             shortTrades = TRUE,    #If short trades to be permitted
             
             
             pctFlag     = FALSE,  #This flag represent the Amounts mentioned are in Pct or static values. If True all pbAmt,slpAmt are static
             #Parameters related to Profit booking
             pbFlag      = FALSE,  #If Profit booking to be taken, make it True
             pbAmt       = 1,      #Profit booking Amount(not in percent)
             
             #Parms related to stop loss
             slpFlag     = FALSE,  #If slp to be considered
             slpAmt      = 1,      #Stop loss percent for capital
             
             trlFlag     = FALSE,  #If trailing to be done on a position
             trlAmt      = 1,      #Percent after which slp is reset
             trlSlpAmt   = 1,      #Stop loss after trailing
             
             intraday    = FALSE,   #If strategy is intraday
             idStartTime = "09:15", #Start time for intraday trades
             idEndTime   = "15:30", #End time for intraday trades
             
             description = "default"
  )
  return(t)
}