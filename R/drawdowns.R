#' Calculate the drawdowns in a series of profit
#' 
#' drawdowns - Returns the drawdowns from highest losing streak to lowest losing streak. Sums the sequence of losses & returns
#' the same in descending order restricted by 'n' elements
#' @param  t - sequence of numbers 
#' @param  n - max number of drawdowns, default - 1
#' @return vector
#' @export
drawdowns <- function(t,n=1){
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