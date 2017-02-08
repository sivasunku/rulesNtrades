longEntry  <- function(m,i,...){
  n  <- as.data.frame(m[i,])
  pn <- as.data.frame(m[(i-1),])
  #default the instrument
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  tempPos <- position(instr)
  
  if ( (i %% 4) == 0 ){
    tempPos$openFlag    <- TRUE
    tempPos$openQty     <- 200
    tempPos$direction   <- 'LONG'
    tempPos$openPrice   <- n$Close
    tempPos$openDate    <- index(m[i,])
    tempPos$openReason  <- "strg7 Long Entry"
    return(tempPos)
  }
  
  return(tempPos)
  
}

longExit   <- function(pos,m,i,...){ 
  
  n  <- as.data.frame(m[i,])
  pn <- as.data.frame(m[(i-1),])
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  if ( (i %% 4) == 1) {
    pos$closePrice  <- n$Open
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(m[i,])
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("strg7 Long Exit")
    return(pos)
  } 
  
  return(pos)
} 


shortEntry  <- function(m,i,...){
  n  <- as.data.frame(m[i,])
  pn <- as.data.frame(m[(i-1),])
  #default the instrument
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  tempPos <- position(instr)
  
  if ( (i %% 4) == 2 ){
    tempPos$openFlag    <- TRUE
    tempPos$openQty     <- 200
    tempPos$direction   <- 'SHORT'
    tempPos$openPrice   <- n$Close
    tempPos$openDate    <- index(m[i,])
    tempPos$openReason  <- "strg7 Short Entry"
    return(tempPos)
  }
  
  return(tempPos)
  
}

shortExit   <- function(pos,m,i,...){ 
  
  n  <- as.data.frame(m[i,])
  pn <- as.data.frame(m[(i-1),])
  dots  <- list(...)
  instr <- ifelse(is.null(dots$instr),"default",dots$instr)
  
  if ( (i %% 4) == 3) {
    pos$closePrice  <- n$Close
    pos$closeQty    <- pos$openQty
    pos$closeDate   <- index(m[i,])
    pos$closeFlag   <- TRUE
    pos$closeReason <- sprintf("strg7 Short Exit")
    return(pos)
  } 
  
  return(pos)
} 

