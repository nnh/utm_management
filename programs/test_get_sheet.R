#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(here)
# ------ constants ------
# ------ functions ------
# ------ main ------
source(here("programs", "test_common.R"), encoding="UTF-8")
gs4_auth(
  email = gargle::gargle_oauth_email(),
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
fortiGateUserInfo <- addressList %>% filter(ID=="fortigate_id") %>% .$Item %>% 
  read_sheet(sheet="FortiGate", na="", col_names=T, skip=2) %>% filter(is.na(削除日) | 削除日 >= Sys.Date())
sinetTable <- addressList %>% filter(ID=="sinet") %>% .$Item %>% read_sheet()
staticIpTable <- addressList %>% filter(ID=="static_ip") %>% .$Item %>% read_sheet()
excludedUrl <- addressList %>% filter(ID=="excluded") %>% .$Item
blockedMacAddressTable <- excludedUrl %>% read_sheet(sheet="blockedDevices")
whitelistTable <- excludedUrl %>% read_sheet(sheet="whitelist")
dummy <- c("fortiGateUserInfo","sinetTable", "staticIpTable", "blockedMacAddressTable", "whitelistTable") %>% map( ~ TableWriteJson(.))
