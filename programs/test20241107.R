#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(here)
# ------ constants ------
source(here("programs", "test_common.R"), encoding="UTF-8")
source(here("programs", "test_get_xml.R"), encoding="UTF-8")
source(here("programs", "get_device_list.R"), encoding="UTF-8")
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
input_path <- home_dir %>% file.path("Downloads", "input")
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
