#' This file will have all the constants required for this package.
#' This file is stored as constants.Rda to be loaded.
#' save.image will save all these into a required Rda file

cBullishTrend  <- 1
cNoTrend       <- 0
cBearishTrend  <- -1
cStepsUp       <- 1
cStepsDown     <- -1

cGapUp         <- 1
cGapDown       <- -1
cNoGap         <- 0
cGapClosed     <- 2
cGapNarrowed   <- 3
cGapRanAway    <- 4

cLong          <- 1
cShort         <- -1

cOpenLong      <- 1
cCloseLong     <- 2
cOpenShort     <- -1
cCloseShort    <- -2
cHold          <- 3
cDoNotOpen     <- 0
cExitPos       <- 4
cDoNotExit     <- 5

#################################################################################
# Global objects
#################################################################################
gTrxns <- data.frame(symbols = character(1),
                    date = as.POSIXct("1950-01-01"),
                    price = double(1),
                    qty = double(1),
                    type = character(1),
                    fees = double(1),
                    stringsAsFactors = FALSE
                    )
gTrxns <- gTrxns[-1,]

gTrades <- data.frame(symbols = character(1),
                     direction = character(1),
                     entryTime = as.POSIXct("1950-01-01"),
                     entryPrice = double(1),
                     entryQty = double(1),
                     entryFee = double(1),
                     exitTime = as.POSIXct("1950-01-01"),
                     exitPrice = double(1),
                     exitQty = double(1),
                     exitFee = double(1),
                     closeReason = character(1),
                     openReason = character(1),
                     grossP = double(1),
                     netP  = double(1),
                     barCount = double(1),
                     stringsAsFactors = FALSE
)
reInvTrades <- gTrades <- gTrades[FALSE,]


gStats <- matrix(vector(mode = "double"),26,1)
colnames(gStats) <- c("TOTAL")
rownames(gStats) <- c("GrossProfit",
                      "Comissions",
                      "NetProfit",
                      "NoTotalTrades",
                      
                      "ProfitPerTrade",
                      "PainToGain", 
                      "ProfitFactor", #$Wins/$Losses
                      "WinningPercentage", 
                      #"PayoutRatio", #AvgWin/AvgLoss
                      #"Expectancy", #AvgTrade/AvgLoss
                      "ReturnPct",
                      "avgBars",
                      
                      "NoLosingTrades",                      
                      "LosingTradesProfit",
                      "avgLossBars",
                      "avgLoss",
                      "stdDevLoss",
                      
                      "maxDrawdown",
                      "maxLosingStreak",
                      "maxPainSum",   #max Pain is the point that has highest losspoint
                      "maxPossibleLoss",
                      
                      "NoWinningTrades",
                      "WinningTradesProfit",
                      "maxWinningStreak",
                      "avgWin",
                      "stdDevWin",
                      "avgWinBars",
                      "requiredCapital"

)
emptyStats <- gStats
