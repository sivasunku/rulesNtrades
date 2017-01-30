##################################################################
#This script will load the initial tables 
# Pre tasks for futMin
# 1. a. Download the file for futures data from googledrive.
#    b. name the file as "futMinDelta.txt"
#    c. Run the code of FutMin
#    d. Check the head & tail of futMin, confirm that it is good.
# Pre tasks for equity Daily
# 1. a. Download the file for eqDly data from nse
#    b. name the file as "eqDlyDelta.csv"
#    c. Run the code of Equities Daily
#    d. Check the head & tail of eqDly, confirm that it is good.
# 
# Use addDelta.R to add new rows.
##################################################################
#Initial environment setup
symbols <- 'CENTURYTEX'
source("code/makeFile.R")

#Data Directory
baseDir <- "~/R/Analyze/"
dataDir <- paste(baseDir,"data",sep = "/")
setwd(dataDir)

####################################################################
#   Process Futures Minute data & others                           #
####################################################################
fileFutMin <- "Futures/futMinBase.csv"
fileFutMinDelta <- "Futures/futMinDelta.txt"

futMin <- as.xts(read.zoo(
                fileFutMin,
                header=TRUE,
                sep=",",
                FUN=function(x) as.POSIXlt(strptime(x,"%Y-%m-%d %H:%M:%S")),
                index.column = 1
                )
)

tempA <- read.table(
                fileFutMinDelta,
                sep=",",
                stringsAsFactors = FALSE
                )
tempDelta <- as.xts(read.zoo(
                tempA[,2:8], 
                header=TRUE,
                FUN = function(x) strptime(paste(x[,1],x[,2]),"%Y%m%d %H:%M"),
                index.column = c(1,2)
                ) 
              )
colnames(tempDelta) <- c('Open','High','Low','Close','Volume')
#Check manually if tempDelta is correct.
futMin <- rbind(futMin,tempDelta)
futMin <- futMin[ ! duplicated( index(futMin), fromLast = TRUE ),  ]
#Check manually if futMin is correct.

if (file.exists(fileFutMin)){
  file.rename(fileFutMin,paste(fileFutMin,".Bkup",sep=""))
}

write.zoo(futMin,file = fileFutMin,sep=",")
rm(tempA,tempDelta)

futDly <- to.daily(futMin)
colnames(futDly) <- c("Open","High","Low","Close","Volume")
futHly <- to.hourly(futMin,indexAt = "endof")
colnames(futHly) <- c("Open","High","Low","Close","Volume")
futMin30 <- to.minutes30 (futMin,indexAt = "endof")
colnames(futMin30) <- c("Open","High","Low","Close","Volume")
futMin10 <- to.minutes10(futMin,indexAt = "endof")
colnames(futMin10) <- c("Open","High","Low","Close","Volume")
futMin15 <- to.minutes15(futMin,indexAt = "endof")
colnames(futMin15) <- c("Open","High","Low","Close","Volume")

####################################################################
# Process EquDaily - one from NSE site                             #
####################################################################
fileEqDly <- "CENTURYTEXDly.csv"
fileEqDlyDelta <- "Equities/TCSD.csv"

eqDly <- as.xts(read.zoo(
                      fileEqDly,
                      header=TRUE,
                      sep=",",
                      #FUN=function(x) as.POSIXlt(strptime(x,"%Y-%m-%d")),
                      FUN=function(x) as.POSIXlt(strptime(x,"%m/%d/%Y")),
                      index.column = 1)
                    )

tempA <- read.table(
          fileEqDlyDelta,
          sep="," ,
          header = TRUE,
          stringsAsFactors = FALSE
          )
tempDelta <- as.xts(read.zoo(
              tempA[,1:6], 
              header=TRUE,
              FUN = function(x) strptime(x,"%d-%b-%Y"),
              index.column = 1
              )
            )
colnames(tempDelta) <- c('Open','High','Low','Close','Volume')
eqDly <- rbind(eqDly,tempDelta)
eqDly <- eqDly[ ! duplicated( index(eqDly), fromLast = TRUE ),  ]

if (file.exists(fileEqDly)){
  file.rename(fileEqDly,paste(fileEqDly,".Bkup",sep=""))
}
write.zoo(eqDly,file = fileEqDly,sep=",")

#Read the minute wise from Google downloads
as.xts(read.zoo(
  f,
  header=TRUE,
  sep=",",
  #FUN=function(x) as.POSIXlt(strptime(x,"%m/%d/%Y %H:%M")),
  FUN=function(x) as.POSIXlt(strptime(paste(x[,1],x[,2]),"%Y%m%d %H:%M")),
  index.column = 1:2)) -> temp
colnames(temp) <- c("Open","High","Low","Close")
to.minutes10(temp,indexAt = 'startof') -> NIFTY10M
to.hourly(temp,indexAt = 'startof') -> NIFTYHly
to.daily(temp,indexAt = 'startof') -> NIFTYDly

colnames(NIFTY10M) <- c("Open","High","Low","Close")
colnames(NIFTYHly) <- c("Open","High","Low","Close")
colnames(NIFTYDly) <- c("Open","High","Low","Close")

SBIN10M <- temp
SBINHly <- temp1
save(SBIN10M,SBINHly,file = "data/SBINMin.rda")


#Read the minute wise from zerodha pi
as.xts(read.zoo(
  f,
  header=TRUE,
  sep=",",
  FUN=function(x) as.POSIXlt(strptime(x,"%m/%d/%Y %H:%M")),
  index.column = 1)
) -> temp
colnames(temp) <- c("Open","High","Low","Close","Volume")
to.hourly(temp,indexAt = 'startof') -> temp1
SBIN10M <- temp
SBINHly <- temp1
save(SBIN10M,SBINHly,file = "data/SBINMin.rda")

#daily
as.xts(read.zoo(
  f,
  header=FALSE,
  sep=",",
  FUN=function(x) as.POSIXlt(strptime(x,"%m/%d/%Y")),
  index.column = 1)
) -> temp
rbind(SBINDly,temp) -> temp
temp[!duplicated(index(temp))] -> temp
to.weekly(temp,indexAt = 'startof') -> temp1
colnames(temp) <- c("Open","High","Low","Close","Volume")
colnames(temp1) <- c("Open","High","Low","Close","Volume")
SBINDly <- temp
SBINWkly <- temp1 
save(SBINDly,SBINWkly,file = "data/SBINDly.rda")



#Read from google drive download
f <- "/Users/admin/Documents/R/Analyze/Rawdata/output/all.txt"
read.csv2(f,
          header=FALSE,
          sep=",") -> t
t[,1] <- NULL
t <- t[t[,1]!=0,]
t <- t[!is.na(t[,1]),]

read.zoo(
  t,
  header=FALSE,
  sep=",",
  FUN=function(x) as.POSIXlt(strptime(paste(x[,1],x[,2]),"%Y%m%d %H:%M")),
  index.column = 1:2) -> temp

#From NSE
read.zoo(
  f,
  header=TRUE,
  sep=",",
  #NSE - 02-Jan-2007
  FUN=function(x) as.POSIXlt(strptime(x,"%d-%b-%Y")),
  index.column = 1) -> temp


#Delete NIFTY string if possible to make it numeric
temp <- temp[ ! duplicated( index(temp), fromLast = TRUE ),  ]
temp <- as.xts(temp)
xts::tzone(temp) <- Sys.timezone()
colnames(temp) <- c("Open","High","Low","Close")
storage.mode(temp) <- "numeric"

to.minutes10(temp,indexAt = 'startof') -> BNF10M
10M <- rbind(NIFTY10M,NIFTY10MDelta)
colnames(NIFTY10M) <- colnames(temp)
NIFTY10M <- NIFTY10M[ ! duplicated( index(NIFTY10M), fromLast = TRUE ),  ]

to.hourly(temp,indexAt = 'startof') -> NIFTYHlyDelta
NIFTYHly <- rbind(NIFTYHly,NIFTYHlyDelta)
colnames(NIFTYHly) <- colnames(temp)
NIFTYHly <- NIFTYHly[ ! duplicated( index(NIFTYHly), fromLast = TRUE ),  ]
save(NIFTY10M,NIFTYHly,file = "data/NIFTYMin.rda")

#Daily,Weekly, Monthly
to.daily(temp,indexAt = 'startof') -> NIFTYDlyDelta
NIFTYDly <- rbind(NIFTYDly,NIFTYDlyDelta)
NIFTYDly <- NIFTYDly[ ! duplicated( index(NIFTYDly), fromLast = TRUE ),  ]
colnames(NIFTYDly) <- colnames(temp)

to.weekly(temp,indexAt = 'startof') -> NIFTYWklyDelta
NIFTYWkly <- rbind(NIFTYWkly,NIFTYWklyDelta)
NIFTYWkly <- NIFTYWkly[ ! duplicated( index(NIFTYWkly), fromLast = TRUE ),  ]
colnames(NIFTYWkly) <- colnames(temp)

to.monthly(temp,indexAt = 'startof') -> NIFTYMthlyDelta
NIFTYMthly <- rbind(NIFTYMthly,NIFTYMthlyDelta)
colnames(NIFTYMthly) <- colnames(temp)
NIFTYMthly <- NIFTYMthly[ ! duplicated( index(NIFTYMthly), fromLast = TRUE ),  ]
save(NIFTYDly,NIFTYWkly,NIFTYMthly,file = "data/NIFTYDly.rda")

