library(tidyverse)
library(openxlsx)
library(here)
kTotalTitle = "Total Bytes Transferred"
# ------ function ------
getTargetRows <- function(inputCsv, targetTitle, rowCount, columnNames){
  checkTitle <- map_chr(inputCsv, function(x){ str_detect(x, targetTitle) })
  titleRow <- which(checkTitle == TRUE)
  targetStartRow <- titleRow + 2
  targetEndRow <- titleRow + rowCount + 1
  temp_target <- inputCsv[targetStartRow:targetEndRow] %>%
    map(function(x){ str_split(x, ",") }) %>%
    map(function(x){unlist(x)}) %>%
    do.call(function(...) rbind(data.frame(), ...), .)
  names(temp_target) <- columnNames
  return(temp_target)
}
getInputCsv <- function(targetFilePath, yyyymm){
  inputCsv <- ReadLog(targetFilePath) %>% str_replace_all(pattern="(?<=[0-9]),(?=[0-9])", replacement="") %>%  # Remove commas for digits
    gsub(pattern="\"", replacement="", x=., fixed=T) %>%
    str_replace_all(pattern='^"|"$', replacement="")  # Remove double quotes at the beginning and end of sentence
  dfTotal <- getTargetRows(inputCsv,
                           "###Traffic Statistics###",
                           8,
                           c("ID","Summary","Statistics")) %>% filter(Summary==kTotalTitle)
  dfTotal$bandwidth <- map_chr(dfTotal$Statistics, calcByte) %>% as.numeric()
  addTotalRow <- c(kTotalTitle, dfTotal[1, "Statistics"], dfTotal[1, "bandwidth"], yyyymm)
  dfTop30 <- getTargetRows(inputCsv,
                           "###Top 30 Applications by Bandwidth and Sessions###",
                           30,
                           c("ID", "application", "バンド幅", "セッション"))
  dfTop30$yyyymm <- yyyymm
  dfTop30$bandwidth <- map_chr(dfTop30$バンド幅, calcByte) %>% as.numeric()
  dfTop30 <- dfTop30 %>% select("application", "バンド幅", "bandwidth", "yyyymm") %>% rbind(addTotalRow)
  return(dfTop30)
}
getTargetCsvName <- function(targetPath){
  temp_filenames <- list.files(targetPath) %>% str_subset("Bandwidth and Applications Report without guest-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{4}.*csv$")
  return(temp_filenames)
}
# Align the units to megabytes.
calcByte <- function(targetStr){
  numTarget <- targetStr %>% str_replace(pattern="MB|GB|TB", "") %>% as.numeric()
  if (str_detect(targetStr, "MB")){
    unitValue <- 1
  } else if (str_detect(targetStr, "GB")){
    unitValue <- 1024^1
  } else if (str_detect(targetStr, "TB")){
    unitValue <- 1024^2
  }
  temp <- numTarget * unitValue
  return(temp)
}
# ------ main ------
source(file.path(here(), "programs", "common_function.R"))
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  # target the previous month of the execution date
  yyyymm <- Sys.Date() %>% format("%Y-%m-01") %>% as.Date() %>% {. - 1} %>% format("%Y%m")
}
yyyy <- str_sub(yyyymm, 1, 4)
mm <- str_sub(yyyymm, 5, 6)
numMm <- as.numeric(mm)
if (numMm <= 3){
  temp_targetYyyymm <- 1:numMm %>% as.character() %>% str_pad(2, pad=0) %>% str_c(yyyy, .)
  yyyy <- as.numeric(yyyy) - 1
  yyyy <- as.character(yyyy)
  numMm <- 12
} else {
  if (exists("temp_targetYyyymm")){
    rm(temp_targetYyyymm)
  }
}
# Covering the current year
targetYyyymm <- 4:numMm %>% as.character() %>% str_pad(2, pad=0) %>% str_c(yyyy, .)
if (exists("temp_targetYyyymm")){
  targetYyyymm <- c(targetYyyymm, temp_targetYyyymm)
}
# file path
here() %>% setwd()
setwd("..")
targetFolderName <- str_c("UTM Logs ", targetYyyymm)
parentPath <- str_c(getwd(), "/レポート/")
targetYyyymmFolderPath <- str_c(parentPath, targetFolderName)
inputPath <- str_c(targetYyyymmFolderPath, "/input/")
outputPath <- str_c(targetYyyymmFolderPath, "/output/")
extPath <- str_c(targetYyyymmFolderPath, "/ext/")
targetFilePath <- map(inputPath, getTargetCsvName) %>% str_c(inputPath, .)
#
outputTop30Df <- data.frame(matrix(rep(NA), ncol=4, nrow=1))[numeric(0), ]
for (i in 1:length(targetYyyymm)){
  temp <- getInputCsv(targetFilePath[i], targetYyyymm[i])
  tempYM <- temp[1 ,"yyyymm"]
  outputTop30Df <- rbind(outputTop30Df, temp)
}
# ranking
applications <- unique(outputTop30Df$application)
applicationsRank <- map_int(applications, function(x){
  temp <- outputTop30Df %>% filter(application == x) %>% nrow()
  return(temp)
})
applicationList <- data.frame(applications, applicationsRank)
applicationList$applicationsRank <- ifelse(applicationList$applications == kTotalTitle, 99, applicationList$applicationsRank)
outputDf <- left_join(outputTop30Df, applicationList, by=c("application"="applications")) %>% arrange(desc(applicationsRank), application, yyyymm)
# output Excel file
template_wb <- str_c(extPath[length(extPath)], "/bandwidthTemplate.xlsx") %>% loadWorkbook(file=.)
outputSheetName <- "Sheet1"
writeData(template_wb, sheet=outputSheetName, x=outputDf, withFilter=F, sep=",", colNames=T, startCol=1, startRow=1)
saveWorkbook(template_wb, str_c(outputPath[length(outputPath)], "bandwidth_", yyyymm, ".xlsx"), overwrite=T)
