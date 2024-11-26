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
EditAdminAndSystemEvents <- function(adminAndSystemEvents) {
  adminAndSystemEvents_loginSummary <- adminAndSystemEvents$`Login Summary`
  adminAndSystemEvents_loginSummary$ip <- adminAndSystemEvents_loginSummary$Login_Interface %>% str_extract(kIpAddr)
  adminAndSystemEvents_loginSummary <- adminAndSystemEvents_loginSummary %>% left_join(userInfo, by="ip")
  adminAndSystemEvents_ListOfFailedLogins <- adminAndSystemEvents$`List of Failed Logins`
  adminAndSystemEvents_ListOfFailedLogins$ip <- adminAndSystemEvents$`List of Failed Logins`$Login_Source %>% str_extract(kIpAddr)
  adminAndSystemEvents_ListOfFailedLogins <- adminAndSystemEvents_ListOfFailedLogins %>% left_join(userInfo, by="ip")
  adminAndSystemEvents$`Login Summary` <- adminAndSystemEvents_loginSummary
  adminAndSystemEvents$`List of Failed Logins` <- adminAndSystemEvents_ListOfFailedLogins
  return(adminAndSystemEvents)
}
EditBandwidthAndApplicationsReport <- function(bandwidthAndApplicationsReport) {
  top30User <- bandwidthAndApplicationsReport$`Top 30 Users by Bandwidth and Sessions` %>% left_join(userInfo, by=c("User_or_IP_"="ip"))
  top30Destination <- bandwidthAndApplicationsReport$`Top 30 Destination by Bandwidth and Sessions` %>% 
    left_join(userInfo, by=c("Hostname_or_IP_"="ip"))
  top30Destination$user <- NULL
  top30Destination$description <- NULL
  top30Destination$macAddress <- NULL
  bandwidthAndApplicationsReport$`Top 30 Users by Bandwidth and Sessions` <- top30User
  bandwidthAndApplicationsReport$`Top 30 Destination by Bandwidth and Sessions` <- top30Destination
  return(bandwidthAndApplicationsReport)
}
EditClientReputation <- function(clientReputation) {
  topUsers <- clientReputation$レピュテーションスコアの上位ユーザー %>% left_join(userInfo, by=c("User__or_IP_"="ip"))
  topUsersScoreIncreaseRecent <- clientReputation$` 直近2期間にスコアが増加した上位ユーザー` %>% left_join(userInfo, by=c("User__or_IP_"="ip"))
  topDevice <- clientReputation$レピュテーションスコアの大きい上位デバイス %>% left_join(userInfo, by=c("Device2"="ip"))
  clientReputation$レピュテーションスコアの上位ユーザー <- topUsers
  clientReputation$` 直近2期間にスコアが増加した上位ユーザー` <- topUsersScoreIncreaseRecent
  clientReputation$レピュテーションスコアの大きい上位デバイス <- topDevice
  return(clientReputation)
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
# ------ main ------
input_path <- home_dir %>% file.path("Downloads", "input")
tables <- input_path %>% GetInputTables()
ipAddresses <- tables %>% GetIpAddressesAndDomains()
deviceList <- GetDeviceList()
userInfo <- JoinUserInfo(ipAddresses, deviceList)
adminAndSystemEvents <- tables[[kTargetFiles[1]]] %>% EditAdminAndSystemEvents()
bandwidthAndApplicationsReport <- tables[[kTargetFiles[2]]] %>% EditBandwidthAndApplicationsReport()
clientReputation <- tables[[kTargetFiles[[3]]]]
