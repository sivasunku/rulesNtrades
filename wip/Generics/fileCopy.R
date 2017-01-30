f1 <- list.files(
  path = "C:/Users/admin/Documents/R/Analyze/Rawdata",
  pattern = "^NIFTY.txt",
  recursive = TRUE,
  include.dirs = TRUE,
  full.names = TRUE,
  ignore.case = TRUE)
toDir = "C:/Users/admin/Documents/R/Analyze/Rawdata/output"
toFile = "C:/Users/admin/Documents/R/Analyze/Rawdata/output/all.txt"
for ( i in 1:length(f1)){
  
  #toFile <- paste(toDir,"/Bank_V",i,".txt",sep = "")
  print(sprintf("FromFile %s : toFile %s",f1[i],toFile))
  #file.copy(f1[i],toFile,overwrite = FALSE)
  file.append(toFile,f1[i])
}
