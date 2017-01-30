##############################################################################
#Functions related to bars/candles
##############################################################################
bar.isGreen <- function(b){
  if (!is.OHLC(b)){  stop(" in bar.isGreen b is not an OHLC")  }
  b <- OHLC(b)
  return( Cl(b) > Op(b))
}
bar.isRed   <- function(b){
  if (!is.OHLC(b)){  stop(" in bar.isRed b is not an OHLC")  }
  b <- OHLC(b)
  return( Cl(b) <= Op(b))
}

#Returns if Cl - Op is less than 4/5 ticked prices
bar.isDoji  <- function(b){
  if (!is.OHLC(b)){  stop(" in bar.isRed b is not an OHLC")  }
  b <- OHLC(b)
  return(abs(Cl(b) - Op(b)) <= (getTickedPrice(Cl(b)) * gTicks))
}