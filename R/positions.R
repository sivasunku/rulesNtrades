#' positions
#' 
#' Functions related to the positions
#'
#' @author Siva Sunku
#' @keywords position
#' @note
#'
#' \code{position} returns empty position object that is not opened yet. position class object
#'
#' @param instr - instrument that is being traded. Default "SHARE"
#' @return returns TRUE/FALSE
#' @rdname positions
#' @export
position <- function(instr = "SHARE"){
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
  trailPrice = 0,
  slpPrice   = 0,
  barCount   = 0,
  #profit booking
  pbPrice    = 0,
  pbLot      = 0,
  pbCount    = 0
)
class(me) <- append( "position",class(me) )
return(me)
}

#' @export
isopen <- function(x) UseMethod("isopen")
#' @param pos - position
#' @return returns TRUE if position is opened, FALSE otherwise
#' @rdname positions
#' @export
isopen.position <- function(pos){
  return(pos$openFlag)
}

#' @export
isclosed <- function(x) UseMethod("isclosed")
#' @param pos - position
#' @return returns TRUE if position is opened, FALSE otherwise
#' @rdname positions
#' @export
isclosed.position <- function(pos){
  return(pos$closeFlag)
}

#' @export
islong <- function(x) UseMethod("islong")
#' @param instr - instrument that is being traded. 
#' @return returns TRUE/FALSE
#' @rdname positions
#' @export
islong.position <- function(pos){
  if ( isopen(pos) &&  (pos$direction == 'LONG') ){
    return(TRUE)
  }
  return(FALSE)
}
#' @export
is.position <- function(pos){
  return ( inherits(pos,"position") )
}

#' @export
isshort <- function(x) UseMethod("isshort")
#' @param instr - instrument that is being traded. 
#' @rdname positions
#' @return returns TRUE/FALSE
#' @export
isshort.position <- function(pos){
  if ( isopen(pos) && (pos$direction == 'SHORT') ){
    return(TRUE)
  }
  return(FALSE)
}

#' @export
qty <- function(x) UseMethod("qty")

#' @param pos - position
#' @rdname positions
#' @return returns openqty if position is in open & long. returns -ve qty if position is short
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

#' @details  - This functions sums up two position objects
#' @description  - The function averages the open price & adds the open Qty of both a & c. Remaining fields will be same as first object 'a;
#' @param  a,b - two positions that needs to be added
#' @rdname positions
#' @return returns new position which is sum of a and b
#' @export
'+.position' <- function(a,b){
  if ( ! ( is.position(a) && is.position(b) ) ){
    stop("a/b is not a position object")
  }
  
  if (a$direction != b$direction){
    stop("In +.position a & b direction is different")
  }
  
  c <- a
  #Average the openprice
  c$openPrice <- ( (a$openPrice * a$openQty) + (b$openPrice * b$openQty) )/(a$openQty + b$openQty)
  #Add the qty
  c$openQty   <- a$openQty + b$openQty
  
  return (c)
}

#' @details  - This functions reduces the qty in b object from a
#' @description  - The function reduces the qty in b oject from a . Rest all fields will remain same
#' @param  a,b - two positions that needs to be added
#' @rdname positions
#' @return returns new position which is a - b
#' @export
'-.position' <- function(a,b) {
  if ( ! ( is.position(a) && is.position(b) ) ){
    stop("a/b is not a position object")
  }
  
  if (a$direction != b$direction){
    stop("In -.position a & b direction is different")
  }
  
  c <- a
  c$openQty   <- a$openQty - b$openQty
  
  if ( c$openQty < 0 ) {
    warnings("position Qty went below zero")
  }
  
  return (c)
}

