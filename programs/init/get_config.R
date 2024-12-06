#' Extracting Key Information from Configuration Files and Exporting to JSON
#' This program reads configuration files to extract specific information and
#' outputs the results in a structured JSON format.
#' @file get_config.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
rm(list = ls())
# ------ libraries ------
library(here)
# ------ constants ------
source(here("programs", "common", "common.R"), encoding = "UTF-8")
source(here("programs", "init", "get_blocked_macaddress.R"), encoding = "UTF-8")
kSetInterFace <- "set interface "
# ------ functions ------
GetBlackList <- function() {
  blackList <- tibble(ip = character(), hostName = character(), macAddress = character())
  blackListRow <- 0
  for (i in seq_len(length(configFile))) {
    if (str_detect(configFile[i], 'edit "Black[0-9]+')) {
      blackListRow <- blackListRow + 1
      blackList[blackListRow, "hostName"] <- configFile[i] %>%
        str_extract('".*"') %>%
        str_remove_all('"')
      temp_row <- i + 1
      while (temp_row < length(configFile)) {
        if (str_detect(configFile[temp_row], "set subnet")) {
          blackList[blackListRow, "ip"] <- configFile[temp_row] %>%
            str_remove("set subnet ") %>%
            str_remove(" 255.255.255.255") %>%
            trimws()
        }
        if (str_detect(configFile[temp_row], "set fqdn")) {
          blackList[blackListRow, "ip"] <- configFile[temp_row] %>% GetConfigValue("set fqdn ")
        }
        if (str_detect(configFile[temp_row], "next")) {
          break
        }
        temp_row <- temp_row + 1
      }
    }
  }
  return(blackList)
}
GetConfigValue <- function(inputStr, removeStr) {
  res <- inputStr %>%
    str_remove(removeStr) %>%
    str_remove_all('"') %>%
    trimws()
  return(res)
}
GetInterFace <- function(inputStr) {
  interFace <- GetConfigValue(inputStr, kSetInterFace)
  return(interFace)
}

GetDhcpRange <- function() {
  dhcpConfigStart <- NA
  dhcpConfigEnd <- NA
  for (i in seq_len(length(configFile))) {
    if (str_detect(configFile[i], "config system dhcp server")) {
      dhcpConfigStart <- i
    }
    if (!is.na(dhcpConfigStart) && str_detect(configFile[i], "^end")) {
      dhcpConfigEnd <- i
      break
    }
  }
  if (is.na(dhcpConfigStart) || is.na(dhcpConfigEnd)) {
    stop("Failed to obtain DHCP address range.")
  }
  dhcpConfig <- configFile[dhcpConfigStart:dhcpConfigEnd]
  interface <- NA
  startIp <- NA
  endIp <- NA
  dhcpIpList <- list()
  for (i in seq_len(length(dhcpConfig))) {
    if (str_detect(dhcpConfig[i], kSetInterFace)) {
      interface <- dhcpConfig[i] %>% GetInterFace()
    }
    if (str_detect(dhcpConfig[i], "set start-ip ")) {
      startIp <- dhcpConfig[i] %>% GetConfigValue("set start-ip ")
    }
    if (str_detect(dhcpConfig[i], "set end-ip ")) {
      endIp <- dhcpConfig[i] %>% GetConfigValue("set end-ip ")
    }
    if (!is.na(startIp) && !is.na(endIp)) {
      ip_list <- GetIpRangeList(startIp, endIp)
      dhcpIpList[[interface]] <- ip_list
      startIp <- NA
      endIp <- NA
    }
  }
  df_dhcp <- map2(
    names(dhcpIpList),
    dhcpIpList,
    ~ tibble(interface = .x, ip = .y)
  ) %>% bind_rows()
  return(df_dhcp)
}

# ------ main ------
blackList <- GetBlackList()
writeSsblackList <- blackList %>%
  select("IP" = "ip", "Description" = "hostName") %>%
  arrange("Description")
blockedMacAddressFromConfig <- GetBlockedMacAddress()
dhcpIpRange <- GetDhcpRange()
dummy <- c(kBlackList, kWriteBlackList, kBlockedMacAddress, kDhcpIpRange) %>%
  map(~ TableWriteJson(.))
