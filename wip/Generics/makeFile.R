initDate="2000-01-01"
#This is to stop scientific print
options("scipen"=100, "digits"=4)
verbose=FALSE

currency('USD')
Sys.setenv(TZ="Asia/Kolkata")
instrument(symbols, currency="USD", multiplier=1, assign_i=TRUE,tick_size = 1)

#################################################################################
#Data Directory & Functions to be loaded
#Naming conventions
# functions - lowerCamelCase
# objects - UpperCamelCase
# constants - Capital Case
# Global - gUpperCamelCase

setwd("~/R/Analyze")
source("code/Generics/functions.R")
source("code/Generics/bars.R")
source("code/Generics/Positions & Transactions.R")
source("code/Generics/myIndicators.R")
load("lib/plot.nnet")

#################################################################################
# CONSTANTS
#################################################################################


gDebug <- FALSE
gTrailClosePct <- 0
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

gTrxns <- gTrxns[FALSE,]

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





