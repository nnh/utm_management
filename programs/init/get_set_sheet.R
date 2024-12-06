#' Get and Set Sheet Data for FortiGate, Sinet, Static IP, and Excluded URLs
#' This script authenticates with Google Sheets, retrieves data from various sheets related to FortiGate user
#' information, Sinet, static IPs, and excluded URLs, and updates specific sheets like the blacklist
#' and blocked devices.
#' It uses the 'gs4_auth' function for Google Sheets authentication and processes data in JSON format.
#' @file get_set_sheet.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
rm(list = ls())
# ------ libraries ------
library(here)
# ------ constants ------
# ------ functions ------
source(here("programs", "common", "common.R"), encoding = "UTF-8")
# ------ main ------
gs4_auth(
  email = gargle::gargle_oauth_email(),
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
fortiGateUserInfo <- addressList %>%
  filter(ID == "fortigate_id") %>%
  .$Item %>%
  read_sheet(sheet = "FortiGate", na = "", col_names = TRUE, skip = 2) %>%
  filter(is.na(削除日) | 削除日 >= Sys.Date())
sinetTable <- addressList %>%
  filter(ID == "sinet") %>%
  .$Item %>%
  read_sheet()
staticIpTable <- addressList %>%
  filter(ID == "static_ip") %>%
  .$Item %>%
  read_sheet()
excludedUrl <- addressList %>%
  filter(ID == "excluded") %>%
  .$Item
whitelistTable <- excludedUrl %>% read_sheet(sheet = "whitelist")
whois <- excludedUrl %>% read_sheet(sheet = "whois")
dummy <- c("fortiGateUserInfo", "sinetTable", "staticIpTable", "whitelistTable", "whois") %>% map(~ TableWriteJson(.))
blackList <- here(ext_path, str_c(kWriteBlackList, ".json")) %>% fromJSON()
write_sheet(blackList, ss = excludedUrl, sheet = "blacklist")
blockedMacAddress <- here(ext_path, str_c(kBlockedMacAddress, ".json")) %>% fromJSON()
write_sheet(blockedMacAddress, ss = excludedUrl, sheet = "blockedDevices")
