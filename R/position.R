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

