#' Statistics of all the trades
#'
#' All the statistics of trades executed in a given portfolio
#' 
#' @author Siva Sunku
#' @keywords stats
#' @note
#' 
#' @details  - statObject - returns the statistics object
#' @rdname stats
#' @return list object
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


#' @details  stats.trades - Calculates the statistics of a given trade table. 
#' If t is null, trades in portfolio are considered for calculation
#' @param  t - trades table. If t is provided, portfolio given will not be considered.
#' @rdname stats
#' @return numeric vector of drawdowns
#' @export
stats.trades <- function(t) {
  if ( !is.trades(t) ) {
    stop("stats.trades - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  
  #Profitable/loss trades
  lt <- t %>% dplyr::filter(netP <= 0)
  pt <- t %>% dplyr::filter(netP >  0)

  s <- statObject()
  if (nrow(t) >0){
    s$GrossProfit       <- sum(t$grossP)
    s$Comissions        <- sum(t$entryFee+t$exitFee)
    s$NetProfit         <- sum(t$grossP) - s$Comissions
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
    s$maxLosingStreak       <- drawdowns(t$netP,n=1)
    s$maxPainSum            <- min(cumsum(t$netP))
    s$maxPossibleLoss       <- min(s$maxDrawdown, s$maxPainSum,s$maxLosingStreak)
    
    s$NoWinningTrades       <- nrow(pt)
    s$WinningTradesProfit   <- sum(pt$netP)
    s$avgWin                <- mean(pt$netP)
    s$stdDevWin             <- sd(pt$netP)
    s$avgWinBars            <- round(mean(pt$barCount), digits = 2)
    
    s$requiredCapital       <- abs( s$maxPossibleLoss * 2 )
    s$ReturnPct             <- round(s$NetProfit / s$requiredCapital * 100,digits = 2)
  }
  
  return(s)
}

#' @details  tabulate.trades - Tabulates the trades according to year, month etc
#' If t is null, trades in portfolio are considered for calculation
#' @param  t - trades table. If t is provided, portfolio given will not be considered
#' @rdname stats
#' @return Returns a list object that contains Description & xtab object next to it
#' @export
tabulate.trades <- function(t){
  
  if ( !is.trades(t) ) {
    stop("tabulate.trades - trades is not a valid trade data.")
  }
  
  res <- list()
  
  #Tabulate the profits by year vs qtr vs month
  temp <- aggregate(t$netP,by = list(as.yearmon(as.Date(t$entryTime))),FUN = sum)
  temp$Year  <- year(temp$Group.1)
  temp$Month <- lubridate::month(temp$Group.1,label=TRUE)
  temp$Qtr   <- quarter(temp$Group.1)
  
  res$YearlyProfit <- xtabs(x~Year,data=temp)
  res$MonthlyProfit <- xtabs(x~Month+Year,data=temp)
  res$QrtrlyProfit <- xtabs(x~Qtr+Year,data=temp)
  
  #Tabulate the profits by openReason vs closeReason
  temp1 <- t[,c("closeReason","openReason","netP","direction")]
  res$OpenReason  <- xtabs(netP ~ openReason + temp1$direction ,data = temp1)
  res$LongReason  <- xtabs(netP ~ closeReason + openReason,data = temp1[temp1$direction == "LONG",])
  res$ShortReason <- xtabs(netP ~ closeReason + openReason,data = temp1[temp1$direction == "SHORT",])
  
  return(res)
}
