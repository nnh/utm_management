#' Extracting Key Information from Configuration Files and Exporting to JSON
#' This program reads configuration files to extract specific information and outputs the results in a structured JSON format. 
#' @file get_config.R
#' @author Mariko Ohtsuka
#' @date 2024.11.29
rm(list=ls())
# ------ libraries ------
library(here)
# ------ constants ------
source(here("programs", "test_common.R"), encoding="UTF-8")
kSetInterFace <- "set interface "
# ------ functions ------
GetBlackList <- function() {
  blackList <- tibble(ip=character(), hostName=character(), macAddress=character())
  blackListRow <- 0
  for (i in 1:length(configFile)) {
    if (str_detect(configFile[i], 'edit "Black[0-9]+')) {
      blackListRow <- blackListRow + 1
      blackList[blackListRow, "hostName"] <- configFile[i] %>% str_extract('".*"') %>% str_remove_all('"')
      temp_row <- i + 1
      while(temp_row < length(configFile)) {
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
  res <- inputStr %>% str_remove(removeStr) %>% str_remove_all('"') %>% trimws()
  return(res)
}
GetInterFace <- function(inputStr) {
  interFace <- GetConfigValue(inputStr, kSetInterFace)
  return(interFace)
}
GetBlockedMacAddress <- function() {
  dhcpSectionStart <- NA
  dhcpSectionEnd <- NA
  for (i in 1:length(configFile)) {
    if (configFile[i] == "config system dhcp server") {
      dhcpSectionStart <- i
    }
    if (!is.na(dhcpSectionStart) && configFile[i] == "end") {
      dhcpSectionEnd <- i
      break
    }
  }
  dhcpSection <- configFile[dhcpSectionStart:dhcpSectionEnd]
  blocked_f <- F
  blockedMacAddress <- list()
  temp <- list()
  for (i in 1:length(dhcpSection)) {
    if (str_detect(dhcpSection[i], kSetInterFace)) {
      interFace <- dhcpSection[i] %>% GetInterFace()
    }
    if (str_detect(dhcpSection[i], "config reserved-address")) {
      blocked_f <- T
    }
    if (blocked_f) {
      if (str_detect(trimws(dhcpSection[i]), "end")) {
        blocked_f <- F
        temp$interFace <- interFace
        blockedMacAddress[[temp$macAddress]] <- temp
        temp <- list()
      } else {
        if (str_detect(dhcpSection[i], "set ")) {
          if (str_detect(dhcpSection[i], "set mac ")) {
            temp$macAddress <- dhcpSection[i] %>% GetConfigValue("set mac ")
          }
          if (str_detect(dhcpSection[i], "set action block")) {
            temp$action <- dhcpSection[i] %>% GetConfigValue("set action ")
          } 
          if (str_detect(dhcpSection[i], "set description ")) {
            temp$description <- dhcpSection[i] %>% GetConfigValue("set description ")
          } 
          
        }
      }
    }
  }
  return(blockedMacAddress)
}
GetDhcpRange <- function() {
  dhcpConfigStart <- NA
  dhcpConfigEnd <- NA
  for (i in 1:length(configFile)) {
    if (str_detect(configFile[i], "config system dhcp server")) {
      dhcpConfigStart <- i
    }
    if (!is.na(dhcpConfigStart) & str_detect(configFile[i], "^end")) {
      dhcpConfigEnd <- i
      break
    }
  }
  if (is.na(dhcpConfigStart) | is.na(dhcpConfigEnd)) {
    stop("Failed to obtain DHCP address range.")
  }
  dhcpConfig <- configFile[dhcpConfigStart:dhcpConfigEnd]
  interface <- NA
  startIp <- NA
  endIp <- NA
  dhcpIpList <- list()
  for (i in 1:length(dhcpConfig)) {
    if (str_detect(dhcpConfig[i], kSetInterFace)) {
      interface <- dhcpConfig[i] %>% GetInterFace()
    }
    if (str_detect(dhcpConfig[i], "set start-ip ")) {
      startIp <- dhcpConfig[i] %>% GetConfigValue("set start-ip ")
    }
    if (str_detect(dhcpConfig[i], "set end-ip ")) {
      endIp <- dhcpConfig[i] %>% GetConfigValue("set end-ip ")
    }
    if (!is.na(startIp) & !is.na(endIp)) {
      start_num <- IpToNumber(startIp)
      end_num <- IpToNumber(endIp)
      ip_list <- map_chr(start_num:end_num, NumberToIp)
      dhcpIpList[[interface]] <- ip_list
      startIp <- NA
      endIp <- NA
    }
  }
  df_dhcp <- map2(
    names(dhcpIpList),
    dhcpIpList,
    ~ tibble(interface=.x, ip=.y)
  ) %>% bind_rows()
  return(df_dhcp)
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
blackList <- GetBlackList()
writeSsblackList <- blackList %>% select("IP"="ip", "Description"="hostName") %>% arrange("Description")
blockedMacAddressFromConfig <- GetBlockedMacAddress()
dhcpIpRange <- GetDhcpRange()
dummy <- c('blackList', 'writeSsblackList', 'blockedMacAddressFromConfig', 'dhcpIpRange') %>% 
  map( ~ TableWriteJson(.))
