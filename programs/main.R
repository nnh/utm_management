#' main program
#'
#' @file main.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
rm(list = ls())
# ------ libraries ------
library(here)
# ------ constants ------
kOutputFilename <- "不正アクセスチェックレポート "
kUnregistered <- "! SINET未登録端末"
# ------ functions ------
source(here("programs", "common", "common.R"), encoding = "UTF-8")
source(here("programs", "data", "get_xml.R"), encoding = "UTF-8")
source(here("programs", "data", "get_device_list.R"), encoding = "UTF-8")
source(here("programs", "common", "write_workbook.R"), encoding = "UTF-8")
source(here("programs", "common", "main_function.R"), encoding = "UTF-8")
# ------ main ------
input_path <- home_dir %>% file.path("input")
output_path <- home_dir %>% GetOutputPath()
tables <- input_path %>% GetInputTables()
ipAddresses <- tables %>% GetIpAddressesAndDomains()
deviceList <- GetDeviceList()
userInfo <- JoinUserInfo(ipAddresses, deviceList)
whois <- userInfo %>% GetWhoisInfo()
userInfo <- RestructureUserInfo(userInfo, whois)
tableInfoList <- SetTableInfo()
tablesJoinUserInfo <- tables
for (i in seq_along(tablesJoinUserInfo)) {
  tablesJoinUserInfo <- tableInfoList[[i]] %>% JoinReportAndUserInfoByTable(tablesJoinUserInfo, .)
  for (j in seq_along(tablesJoinUserInfo[[i]])) {
    tablesJoinUserInfo[[i]][[j]] <- tablesJoinUserInfo[[i]][[j]] %>% select(where(~ !all(is.na(.))))
  }
}
tablesJoinUserInfo$`User Report without guest`$top10Destinations <-
  tablesJoinUserInfo$`User Report without guest`$top10Destinations %>%
  select(all_of(c(
    "rank", "usage", "ip", "hostName", "user", "description", "macAddress",
    "Destination", "destinationHost", "Bandwidth", "Application"
  )))
tablesJoinUserInfo$`Client Reputation without guest`$`Report Filters(Logic: All)` <-
  tablesJoinUserInfo$`Client Reputation without guest`$`Report Filters(Logic: All)` %>%
  filter(Filter_name != "srcip")
tablesJoinUserInfo %>% CreateOutputWorkbook()
# Output bandwidth report
source(here("programs", "reports", "output_bandwidth_report.R"), encoding = "UTF-8")
