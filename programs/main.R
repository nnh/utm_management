#' main program
#' 
#' @file main.R
#' @author Mariko Ohtsuka
#' @date 2024.11.29
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
utm_dir_name <- str_c("UTM Logs ", yyyymm)
parent_path <- file.path(home_dir, "Downloads")
input_path <- parent_path %>% file.path("input")
output_path <- parent_path %>% GetOutputPath()
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
tablesJoinUserInfo %>% CreateOutputWorkbook()