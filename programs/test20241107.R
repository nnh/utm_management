#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(here)
library(openxlsx)
# ------ constants ------
source(here("programs", "test_common.R"), encoding="UTF-8")
source(here("programs", "test_get_xml.R"), encoding="UTF-8")
source(here("programs", "get_device_list.R"), encoding="UTF-8")
kOutputFilename <- "不正アクセスチェックレポート "
# ------ functions ------
GetIpAddressesAndDomains <- function(tables) {
  tables_vec <- tables %>% map( ~ {
    table <- .
    res <- table %>% map( ~ unlist(.)) %>% unlist()
    return(res)
  }) %>% unlist()
  ipAddresses <- tables_vec %>% str_extract(kIpAddr) %>% na.omit() %>% unique() %>% tibble(ip=.)
  domains <- tables_vec %>% str_extract("[a-z0-9.-]+\\.[a-z]{2,}") %>% na.omit() %>% unique() %>% tibble(ip=.)
  res <- ipAddresses %>% bind_rows(domains)
  return(res)
}
JoinReportAndUserInfo <- function(target, itemName, key) {
  temp <- target[[itemName]] %>% left_join(userInfo, by=key)
  target[[itemName]] <- temp
  return(target)
}
JoinReportAndUserInfoByTable <- function(tables, tableInfo) {
  tableName <- tableInfo$tableName
  targetItemAndColumn <- tableInfo$targetItemAndColumn
  targetTable <- tables[[tableName]]
  for (i in 1:length(targetItemAndColumn)) {
    itemName <- targetItemAndColumn[[i]]$itemName
    columnName <- targetItemAndColumn[[i]]$columnName
    key <- targetItemAndColumn[[i]]$key
    if (tableName == kAdminAndSystemEvents) {
      targetTable[[itemName]][[key]] <- targetTable[[itemName]] %>% .[ , columnName, drop=T] %>% str_extract(kIpAddr)
    }
    targetTable <- targetTable %>% JoinReportAndUserInfo(itemName, key)
  }
  tables[[tableName]] <- targetTable
  return(tables)
}
JoinUserInfo <- function(ipAddresses, deviceList) {
  # domain
  domainList <- ipAddresses %>% filter(!str_detect(ip, kIpAddr))
  deviceDomainList <- deviceList %>% filter(!str_detect(ip, kIpAddr)) 
  userDomainInfo <- domainList$ip %>% map_df( ~ {
    target <- .
    res <- deviceDomainList %>% map_dfc( ~ NA)
    for (i in 1:nrow(deviceDomainList)) {
      if (str_detect(target, str_c(deviceDomainList[i, "ip"], "$"))) {
        res <- deviceDomainList[i, ]
        break
      }
    }
    res$ip <- target
    return(res)
  })
  # ip
  ipList <- ipAddresses %>% filter(str_detect(ip, kIpAddr))
  deviceIpList <- deviceList %>% filter(str_detect(ip, kIpAddr))
  userIpInfo <- ipList %>% left_join(deviceIpList, by="ip")
  res <- userIpInfo %>% bind_rows(userDomainInfo)
  return(res)
}
SetTableInfo <- function() {
  target <- kTargetFiles %>% map( ~ list(tableName=.,targetItemAndColumn=NA))
  target[[1]]$targetItemAndColumn <- list(
    list(itemName="Login Summary", columnName="Login_Interface", key="ip"),
    list(itemName="List of Failed Logins", columnName="Login_Source", key="ip")
  )
  target[[2]]$targetItemAndColumn <- list(
    list(itemName="Top 30 Users by Bandwidth and Sessions", columnName=NULL, key=c("User_or_IP_"="ip")),
    list(itemName="Top 30 Destination by Bandwidth and Sessions", columnName=NULL, key=c("Hostname_or_IP_"="ip"))
  )
  target[[3]]$targetItemAndColumn <- list(
    list(itemName="レピュテーションスコアの上位ユーザー", columnName=NULL, key=c("User__or_IP_"="ip")),
    list(itemName=" 直近2期間にスコアが増加した上位ユーザー", columnName=NULL, key=c("User__or_IP_"="ip")),
    list(itemName="レピュテーションスコアの大きい上位デバイス", columnName=NULL, key=c("Device2"="ip"))
  )
  kListOfTerminalsTarget <- list(
    list(itemName="Top 100 Users by Bandwidth and Sessions", columnName=NULL, key=c("User_or_IP_"="ip"))
  )
  target[[4]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[5]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[6]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[7]]$targetItemAndColumn <- list(
    list(itemName="top10Destinations", columnName=NULL, key=c("Destination"="ip"))
  )
  return(target)
}
# ------ main ------
#target_yyyymm <- "201906"
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  last_month <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  yyyymm <- str_c(format(last_month, "%Y"), format(last_month, "%m"))
}
utm_dir_name <- str_c("UTM Logs ", yyyymm)
input_path <- home_dir %>% file.path("Downloads", "input")
output_path <- home_dir %>% file.path("Downloads", "output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
tables <- input_path %>% GetInputTables()
ipAddresses <- tables %>% GetIpAddressesAndDomains()
deviceList <- GetDeviceList()
userInfo <- JoinUserInfo(ipAddresses, deviceList)
tableInfoList <- SetTableInfo()
tablesJoinUserInfo <- tables
for (i in 1:length(tablesJoinUserInfo)) {
  tablesJoinUserInfo <- tableInfoList[[i]] %>% JoinReportAndUserInfoByTable(tablesJoinUserInfo, .)
  for (j in 1:length(tablesJoinUserInfo[[i]])) {
    tablesJoinUserInfo[[i]][[j]] <- tablesJoinUserInfo[[i]][[j]] %>% select(where(~ !all(is.na(.))))
  }
}
output_wb <- createWorkbook()
kTitleStyle <- createStyle(fontName="Calibri", fontSize=18)
kHeaderStyle <- createStyle(fontName="Calibri", fontSize=16)
kBodyStyle <- createStyle(fontName="Calibri", fontSize=11)
WriteDataToWorkbook <- function(wb, sheetname, table, currentRow) {
  outputRow <- currentRow + 2
  for (i in 1:length(table)) {
    outputDf <- table[[i]]
    names(table)[[i]] %>% writeData(wb, sheet=sheetname, x=., withFilter=F, startRow=outputRow, sep="\t")
    addStyle(wb, sheet=sheetname, kHeaderStyle, rows=outputRow, cols=1)
    outputRow <- outputRow + 1
    outputDf %>% writeData(wb, sheet=sheetname, x=., withFilter=F, startRow=outputRow, sep="\t")
    outputRow <- outputRow + nrow(outputDf) + 2
  }
  
}
for (i in 1:length(tablesJoinUserInfo)) {
  outputReportName <- names(tablesJoinUserInfo)[i]
  outputTableList <- tablesJoinUserInfo[[i]]
  outputRow <- 1
  addWorksheet(output_wb, i)
  outputReportName %>% writeData(output_wb, sheet=i, x=., withFilter=F, startRow=outputRow, sep="\t")  
  addStyle(output_wb, sheet=i, kBodyStyle, rows=1:110, cols=1:8, gridExpand=T)
  addStyle(output_wb, sheet=i, kTitleStyle, rows=outputRow, cols=1)
  WriteDataToWorkbook(output_wb, i, outputTableList, outputRow)
}
saveWorkbook(output_wb, str_c(output_path, "/", kOutputFilename, yyyymm, ".xlsx"), overwrite=T)  

  titles <- names(tables)
  i <- 1
  sheetname <- titles[i]
  addWorksheet(output_wb, sheetname)
  outputRow <- 1
  writeData(output_wb, sheet=sheetname, x=titles[i], withFilter=F, startRow=outputRow, sep="\t")  
  outputRow <- outputRow + 1
  writeData(output_wb, sheet=sheetname, x=tablesJoinUserInfo[[i]][[1]], withFilter=F, sep="\t", startRow=outputRow, colNames=F)
  saveWorkbook(output_wb, str_c(output_path, "/", kOutputFilename, yyyymm, ".xlsx"), overwrite=T)  
  