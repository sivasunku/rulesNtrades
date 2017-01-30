#' stats - Functions related to all stats - a trade is two transactions
#' @author Siva Sunku
#' @keywords position
#' @note
#' 
#' @details  - This function returns a list of statistics object
#' @param  n - no. of rows
#' @rdname stats
#' @return trades dataframe
#' @export
statObject <- function(){
   t <- list("GrossProfit" = NA,
          "Comissions" = NA,
          "NetProfit" = NA,
          "NoTotalTrades"=NA,
          
          "ProfitPerTrade"=NA,
          "PainToGain"=NA, 
          "ProfitFactor"=NA, #$Wins/$Losses
          "WinningPercentage"=NA, 
          "ReturnPct" = NA,
          "avgBars"=NA,
          
          "NoLosingTrades"=NA,
          "LosingTradesProfit"=NA,
          "avgLossBars"=NA,
          "avgLoss"=NA,
          "stdDevLoss"=NA,
          
          "maxDrawdown"=NA,
          "maxLosingStreak"=NA,
          "maxPainSum"=NA,   #max Pain is the point that has highest losspoint
          "maxPossibleLoss"=NA,
          
          "NoWinningTrades"=NA,
          "WinningTradesProfit"=NA,
          "maxWinningStreak"=NA,
          "avgWin"=NA,
          "stdDevWin"=NA,
          "avgWinBars"=NA,
          "requiredCapital" = NA
   )
   
   return(t)
}


#' @details  - This function gets the statistics of trades
#' @description  - drawdown - is sum of sequence of all losing trades. netProfit is considered for calculating the same.
#' @param  t - trades table
#' @rdname stats
#' @return numeric vector of drawdowns
#' @export
stats.trades <- function(t) {
  #Profitable/loss trades
  lt <- t %>% dplyr::filter(netP <= 0)
  pt <- t %>% dplyr::filter(netP >  0)

  s <- statObject()
  if (nrow(t) >0){
    s$GrossProfit       <- sum(t$grossP)
    s$Comissions        <- sum(t$entryFee+t$exitFee)
    s$NetProfit         <- sum(t$netP)
    s$NoTotalTrades     <- nrow(t)
    s$ProfitPerTrade    <- sum(t$netP)  / s$NoTotalTrades
    s$PainToGain        <- s$NetProfit  / abs( sum(lt$netP) )
    s$ProfitFactor      <- sum(pt$netP) / abs( sum(lt$netP) )
    s$WinningPercentage <- round( nrow(pt) / nrow(t),digits = 3 )
    s$avgBars           <- round( mean(t$barCount),digits = 2 )
    
    s$NoLosingTrades        <- nrow(lt)
    s$LosingTradesProfit    <- sum(lt$netP)
    s$avgLossBars           <- round( mean(lt$barCount), digits = 2 )
    s$avgLoss               <- mean(lt$netP,digits = 2)
    s$stdDevLoss            <- sd(lt$netP)
    s$maxDrawdown           <- fTrading::maxDrawDown(t$netP)$maxdrawdown * -1
    s$maxLosingStreak       <- drawdowns(at$netP,n=1)
    s$maxPainSum            <- min(cumsum(at$netP))
    s$maxPossibleLoss       <- min (s$maxDrawdown, s$maxPainSum,s$maxPossibleLoss)
    
    s$NoWinningTrades       <- nrow(pt)
    s$WinningTradesProfit   <- sum(pt$netP)
    s$avgWin                <- mean(pt$netP)
    s$stdDevWin             <- sd(pt$netP)
    s$avgWinBars            <- round(mean(pt$barCount), digits = 2)
    
    s$requiredCapital       <- abs( s$maxPossibleLoss * 2 + cacheEnv$cushCapital )
    s$ReturnPct             <- round(s$NetProfit / s$requiredCapital * 100,digits = 2)
  }
  
  return(s)
}
