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
             
             
             pbFlag      = FALSE,  #If Profit booking to be taken
             pbPct       = 1,      #Profit booking percent
             
             slpFlag     = FALSE,  #If slp to be considered
             slpPct      = 1,      #Stop loss percent for capital
             
             trlFlag     = FALSE,  #If trailing to be done
             trlPct      = 1,      #Percent after which slp is reset
             incrSlpPct  = 1,      #Stop loss after taking profit booking
             
             intraday    = FALSE,   #If strategy is intraday
             idStartTime = "09:15", #Start time for intraday trades
             idEndTime   = "15:30", #End time for intraday trades
             description = "default"
  )
  return(t)
}