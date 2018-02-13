#' position create an empty position Object
#' 
#' A position object will store the details of trade opening, price, qty, closing etc. 
#' Once a position is opened, till close the same object can be carried forward for further operations.
#' 
#' @author Siva Sunku
#' @keywords stats
#' @note
#' 
#' @details position - Creates an empty position object
#' @param instr - instrument to be carried with position
#' @return returns position object
#' @rdname position
#' @export
position <- function(instr = "default"){
  
me <- list(
  symbol = instr,
  direction = NULL,
  justOpened = FALSE,
  
  openFlag = FALSE,
  openDate = as.POSIXct("1950-01-01"),
  openPrice = 0,
  openQty = 0,
  openReason = "None",
  openCase   = "None",
  
  closeFlag = FALSE,
  closeDate = as.POSIXct("1950-01-01"),
  closePrice = 0,
  closeQty = 0,
  closeReason = "None",
  #
  slpPrice   = NA,   #slpPrice created based on openPrice
  
  #
  trailPrice = NA,    #Price when this is hit, trailSlpPrice is adjusted
  trailSlpPrice = NA,

  #profit booking
  pbPrice    = NA,
  pbQty      = 0,
  pbCount    = 0,
  
  barCount   = 0
)
class(me) <- append( "position",class(me) )
return(me)
}

#' checks if given object is position type/class
#' @rdname position
#' @export
is.position <- function(pos){
  return ( inherits(pos,"position") )
}

#' check if position is open/closed
#' @rdname position
#' @export
isopen <- function(x) UseMethod("isopen")

#' @rdname position
#' @export
#' @return returns TRUE if position is opened, FALSE otherwise
isopen.position <- function(pos){
  return(pos$openFlag)
}

#' @rdname position
#' @export
isclosed <- function(x) UseMethod("isclosed")

#' @details isclosed - gives TRUE if qty is zero and position is opened
#' @rdname position
#' @export
isclosed.position <- function(pos){
  if ( isopen(pos) && (qty(pos) == 0) ) { return (TRUE)}
  return(pos$closeFlag)
}

#' @details islong - checks if position is long/short
#' @rdname position
#' @export
islong <- function(x) UseMethod("islong")

#' @rdname position
#' @export
islong.position <- function(pos){
  if ( isopen(pos) && ( !is.null(pos$direction) ) && (pos$direction == 'LONG') ){
    return(TRUE)
  }
  return(FALSE)
}

#' @details  isshort - checks if position is long/short
#' @rdname position
#' @export
isshort <- function(x) UseMethod("isshort")

#' @rdname position
#' @export
isshort.position <- function(pos){
  if ( isopen(pos) && ( !is.null(pos$direction) ) && (pos$direction == 'SHORT') ){
    return(TRUE)
  }
  return(FALSE)
}

#' @details qty - Gives the quantity of a position, according to the direction of the position. Returns +ve qty if long, -ve otherwise
#' @rdname position
#' @export
qty <- function(x) UseMethod("qty")


#' @rdname position
#' @export
qty.position <- function(pos){
  if ( !is.position(pos) ) {
    stop(" in qty.position pos is not of class position")
  }
  
  if ( isopen(pos) ){ 
    
    if ( islong(pos) ) {
      return( pos$openQty )
    } else {
      return( -1 * pos$openQty )
    }
  }  else {
    return (0)
  }
  
}


#' @details "+" - Quantities are added & openprice is averaged
#' @rdname position
#' @export
'+.position' <- function(posA,posB){
  if ( ! ( is.position(posA) && is.position(posB) ) ){
    stop("a/b is not a position object")
  }
  
  if ( (posA$direction != posB$direction) || 
       (posA$symbol    != posB$symbol   ) ) {
    stop("In +.position a & b direction/symbol is different")
  }
  
  c <- posA
  #Average the openprice
  c$openPrice <- ( (posA$openPrice * posA$openQty) + (posB$openPrice * posB$openQty) )/(posA$openQty + posB$openQty)
  #Add the qty
  c$openQty   <- posA$openQty + posB$openQty
  
  return (c)
}

#' @details "-" - only quantities are substracted
#' @rdname position
#' @export
'-.position' <- function(posA,posB) {
  if ( ! ( is.position(posA) && is.position(posB) ) ){
    stop("a/b is not a position object")
  }
  
  if ( (posA$direction != posB$direction) || 
       (posA$symbol    != posB$symbol   ) ) {
    stop("In -.position a & b direction/symbol is different")
  }
  
  c <- posA
  c$openQty   <- posA$openQty - posB$openQty
  
  if ( c$openQty < 0 ) {
    warnings("position Qty went below zero")
  }
  
  return (c)
}

#' @details positions.table used to create a dataframe structure for storing positions.
#' @rdname  positions.table
#' @export
positions.table <- function(n = 1000){
  if ( !is.numeric(n) ) { n <- 1 }

  t <- data.frame(onDate  = as.POSIXct("1950-01-01"),
                  
                  barOpen = double(n),
                  barHigh = double(n),
                  barLow  = double(n),
                  barClose = double(n),
                  
                  symbols = character(n),
                  direction = character(n),
                  justOpened = logical(n),
                  
                  openFlag   = logical(n),
                  openDate   = as.POSIXct("1950-01-01"),
                  openPrice  = double(n),
                  openQty    = double(n),
                  openReason = character(n),
                  openCase   = character(n),
                  
                  closeFlag  = logical(n),
                  closeDate  = as.POSIXct("1950-01-01"),
                  closePrice = double(n),
                  closeQty   = double(n),
                  closeReason = character(n),
                  
                  slpPrice    = double(n),
                  trailPrice  = double(n),
                  trailSlpPrice = double(n),
                  pbPrice = double(n),
                  
                  pbQty   = double(n),
                  pbCount = double(n),
                  barCount = double(n),
                  stringsAsFactors = FALSE
                  )
  class(t) <- append("position",class(t))
  return(t)
}

#' @details add.table.positions - Function to add given position on a particular day.
#' @param  pf - portfolio name. default is "default"
#' @param  bar - candle that needs to be added
#' @param  pos - position to be added
#' @rdname add.table.positions
#' @return recently added position
#' @export
add.table.positions <- function (pf = "default", bar, pos){
  
  if ( !is.valid.portfolio(pf) ) {
    stop("add.table.positions - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  
  if ( !is.position(pos) ) {
    stop("add.table.positions - pos is not a position class")
  }
  
  if ( !is.OHLC(bar)){
    stop("add.table.positions - bar is not OHLC")
  }
  
  #Add open position to transactions
  ipf <- get(pf,envir = .rules)
  n <- ipf$positionsRow
  
  ipf$positions[n,]$onDate    <- index(bar)
  ipf$positions[n,]$barOpen   <- bar$Open
  ipf$positions[n,]$barHigh   <- bar$High
  ipf$positions[n,]$barLow    <- bar$Low
  ipf$positions[n,]$barClose  <- bar$Close
  
  ipf$positions[n,]$symbols    <- pos$symbol
  ipf$positions[n,]$direction  <- ifelse(is.null(pos$direction),NA,pos$direction)
  ipf$positions[n,]$justOpened <- pos$justOpened
  
  ipf$positions[n,]$openFlag   <- pos$openFlag
  ipf$positions[n,]$openDate   <- pos$openDate
  ipf$positions[n,]$openPrice  <- pos$openPrice
  ipf$positions[n,]$openQty    <- pos$openQty
  ipf$positions[n,]$openReason <- pos$openReason
  ipf$positions[n,]$openCase   <- pos$openCase
  
  ipf$positions[n,]$closeFlag    <- pos$closeFlag
  ipf$positions[n,]$closeDate    <- pos$closeDate
  ipf$positions[n,]$closePrice   <- pos$closePrice
  ipf$positions[n,]$closeQty     <- pos$closeQty
  ipf$positions[n,]$closeReason  <- pos$closeReason
  
  ipf$positions[n,]$slpPrice        <- pos$slpPrice
  ipf$positions[n,]$trailPrice      <- pos$trailPrice
  ipf$positions[n,]$trailSlpPrice   <- pos$trailSlpPrice
  ipf$positions[n,]$pbPrice         <- pos$pbPrice
  ipf$positions[n,]$pbQty           <- pos$pbQty
  ipf$positions[n,]$pbCount         <- pos$pbCount
  ipf$positions[n,]$barCount        <- pos$barCount
  
  ipf$positionsRow <- n + 1
  
  return(ipf$positions[n,])
}

#' @details get.positions.table
#' @param pf - default "default"
#' @return returns the positions for each and every execution
#' @rdname position
#' @export
get.positions.table <- function( pf = "default"){
  if ( !is.valid.portfolio(pf) ) {
    stop("get.positions.table - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  ipf <- get(pf,envir = .rules)
  return( ipf$positions[1:(ipf$positionsRow-1),] )
}