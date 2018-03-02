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
             pyramidFlag  = FALSE,  #Do you want to add to existing position
             
             qty = 80,              #trading quantity
             
             longTrades  = TRUE,    #If long trades to be permitted
             shortTrades = TRUE,    #If short trades to be permitted
             sameDayFlag = TRUE,    #If True, on the same day if position is closed, open a new position according to rules
             
             pctFlag     = FALSE,  #This flag represent the Amounts mentioned are in Pct or static values. If True all pbAmt,slpAmt are static
             
             #Parameters related to Profit booking
             pbFlag      = FALSE,  #If Profit booking to be taken, make it True
             pbQty       = NA,     #No. of units to be booked if PB is hit
             pbAmt       = NA,     #Profit booking Amount(not in percent)
             pbRunQty    = 0,      #If position is above this qty, pb is checked. if position is eq/below this qty PB is not checked
             
             #Parms related to stop loss
             slpFlag     = FALSE,  #If slp to be considered
             slpAmt      = NA,      #Stop loss percent for capital
             
             #Trailing Related parameters
             trlFlag     = FALSE,  #If trailing to be done on a position
             trlInitAmt  = NA,     #Initial trailing price amount.
             trlIncrAmt  = NA,     #After hitting, this price is increased for the target
             trlSlpAmt   = NA,     #Not in use currently
             
             #These are used for intraday strategies
             intraday    = FALSE,   #If strategy is intraday
             idStartTime = "09:15", #Start time for intraday trades
             idEndTime   = "15:30", #End time for intraday trades
             
             description = "default"
  )
  return(t)
}