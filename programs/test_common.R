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
kUserReport <- "User Report without guest"
kTrafficSummary <- "Traffic Summary"
kTargetFiles <- c("Admin and System Events Report", 
                  "Bandwidth and Applications Report without guest", 
                  "Client Reputation without guest", 
                  "List of terminals connected to DataCenter", 
                  "List of terminals connected to nmccrc", 
                  "List of terminals connected vpn", 
                  "User Report without guest") 
kIpColumns <- c("Login_Interface", "Login_Source", "Destination", "Hostname_or_IP_", "User__or_IP_", "User_or_IP_")
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
TableWriteJson <- function(tableName) {
  get(tableName) %>% jsonlite::write_json(file.path(ext_path, str_c(tableName, ".json")))
}
# ------ main ------
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