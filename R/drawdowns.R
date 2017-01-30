#'
#' drawdowns - The sum of losing streaks in descending order
#' @details  - This function gets the drawdowns in descending order
#' @description  - drawdown - is sum of sequence of all losing trades. netProfit is considered for calculating the same.
#' @param  t - sequence of numbers to find drawdown
#' @return numeric vector of drawdowns
#' @rdname  drawdowns
#' @export
drawdowns.trades <- function(t,n=1){
  if ( !is.numeric(n) || (n<=0) ) { n <- 1 }
  series <- as.numeric(t)
  
  len <- length(series)
  if (len ==0 ){return(0)}
  
  d <- tempSum <- 0
  for (i in 1:len){
    isProfit <- ifelse(series[i]>0,TRUE,FALSE)
    if (isProfit){
      d <- append(d,tempSum)
      tempSum <- 0
    } else {
      tempSum <- tempSum +series[i]
    }
  }
  
  sort(d)[1:n]
}