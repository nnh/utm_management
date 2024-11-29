#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
# ------ libraries ------
library(googlesheets4)
library(jsonlite)
library(tidyverse)
library(xml2)
# ------ constants ------
kIpAddr <- "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
kTrafficSummary <- "Traffic Summary"
kTargetFiles <- c("Admin and System Events Report", 
                  "Bandwidth and Applications Report without guest", 
                  "Client Reputation without guest", 
                  "List of terminals connected to DataCenter", 
                  "List of terminals connected to nmccrc", 
                  "List of terminals connected vpn", 
                  "User Report without guest") 
kAdminAndSystemEvents <- kTargetFiles[1]
kUserReport <- kTargetFiles[7]
kNoDhcpMessage <- "DHCPログのホスト名が空白のため詳細確認不可能"
kBlackList <- 'blackList'
kWriteBlackList <- kBlackList %>% str_c("writeSs", .)
kBlockedMacAddress <- 'blockedMacAddressFromConfig'
kDhcpIpRange <- 'dhcpIpRange'

# ------ functions ------
GetHomeDir <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    home_dir <- Sys.getenv("USERPROFILE")
  } else if (os == "Darwin") {
    home_dir <- Sys.getenv("HOME")
  } else {
    stop("Unsupported OS")
  }
  return (home_dir)
}
GetVolumeStr <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    volume_str <- "//aronas"
  } else if (os == "Darwin") {
    volume_str <- "/Volumes"
  } else {
    stop("Unsupported OS")
  }
  return (volume_str)
}
TableWriteJson <- function(tableName) {
  get(tableName) %>% jsonlite::write_json(file.path(ext_path, str_c(tableName, ".json")))
}
GetIpRangeList <- function(startIp, endIp) {
  start_num <- IpToNumber(startIp)
  end_num <- IpToNumber(endIp)
  ip_list <- map_chr(start_num:end_num, NumberToIp)
  return(ip_list)  
}
IpToNumber <- function(ip) {
  parts <- as.numeric(unlist(strsplit(ip, "\\.")))
  sum(parts * c(2^24, 2^16, 2^8, 1))
}

NumberToIp <- function(number) {
  parts <- c(
    number %/% 2^24,
    (number %% 2^24) %/% 2^16,
    (number %% 2^16) %/% 2^8,
    number %% 2^8
  )
  paste(parts, collapse = ".")
}
# ------ main ------
#target_yyyymm <- "201906"
volume_str <- GetVolumeStr()
home_dir <- GetHomeDir()
ext_path <- home_dir %>% file.path("Downloads", "ext")
addressList <- ext_path %>% file.path("sinet.txt") %>% read.csv()
if (!exists("addressList")) {
  stop("sinet.txt is missing.")
}
configFileName <- ext_path %>% list.files() %>% str_extract('[A-Z]{6}[0-9]{2}_[0-9]{8}_[0-9]{4}\\.conf') %>% na.omit()
if (length(configFileName) == 0) {
  stop("config file is missing.")
}
if (length(configFileName) > 1) {
  stop("Only one configuration file should be stored.")
}
configFile <- ext_path %>% file.path(configFileName) %>% read_lines()
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  last_month <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  yyyymm <- str_c(format(last_month, "%Y"), format(last_month, "%m"))
}