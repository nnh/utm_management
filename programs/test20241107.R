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
  
}
# ------ main ------
input_path <- home_dir %>% file.path("Downloads", "input")
tables <- input_path %>% GetInputTables()
ipAddresses <- tables %>% GetIpAddressesAndDomains()
deviceList <- GetDeviceList()
userInfo <- ipAddresses %>% left_join(deviceList, by="ip")
adminAndSystemEvents <- tables[[kTargetFiles[1]]] %>% EditAdminAndSystemEvents()
bandwidthAndApplicationsReport <- tables[[kTargetFiles[2]]] %>% EditBandwidthAndApplicationsReport()
kTargetFiles[[2]]
OtherEvents <- tables
OtherEvents[[kTargetFiles[1]]] <- NULL
