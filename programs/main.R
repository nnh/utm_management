#' main program
#' 
#' @file main.R
#' @author Mariko Ohtsuka
#' @date 2024.12.2
rm(list=ls())
# ------ libraries ------
library(here)
# ------ constants ------
kOutputFilename <- "不正アクセスチェックレポート "
kUnregistered <- "! SINET未登録端末"
# ------ functions ------
source(here("programs", "common.R"), encoding="UTF-8")
source(here("programs", "get_xml.R"), encoding="UTF-8")
source(here("programs", "get_device_list.R"), encoding="UTF-8")
source(here("programs", "write_workbook.R"), encoding="UTF-8")
source(here("programs", "main_function.R"), encoding="UTF-8")
# ------ main ------
input_path <- home_dir %>% file.path("input")
output_path <- home_dir %>% GetOutputPath()
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
tablesJoinUserInfo$`User Report without guest`$top10Destinations <- tablesJoinUserInfo$`User Report without guest`$top10Destinations %>% 
  select(all_of(c("rank", "usage", "ip", "hostName", "user", "description", "macAddress", 
                  "Destination", "destinationHost", "Bandwidth", "Application")))
tablesJoinUserInfo$`Client Reputation without guest`$`Report Filters(Logic: All)` <- tablesJoinUserInfo$`Client Reputation without guest`$`Report Filters(Logic: All)` %>%
  filter(Filter_name != "srcip")
tablesJoinUserInfo %>% CreateOutputWorkbook()