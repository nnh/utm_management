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
          blackList[blackListRow, "ip"] <- configFile[temp_row] %>% str_remove("set fqdn ") %>% str_remove_all('"') %>% trimws()
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
    if (str_detect(dhcpSection[i], "set interface ")) {
      interFace <- dhcpSection[i] %>% str_remove("set interface ") %>% str_remove_all('"') %>% trimws()
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
            temp$macAddress <- dhcpSection[i] %>% str_remove("set mac ") %>% trimws()
          }
          if (str_detect(dhcpSection[i], "set action block")) {
            temp$action <- dhcpSection[i] %>% str_remove("set action ") %>% trimws()
          } 
          if (str_detect(dhcpSection[i], "set description ")) {
            temp$description <- dhcpSection[i] %>% str_remove("set description ") %>% str_remove_all('"') %>% trimws()
          } 
          
        }
      }
    }
  }
  return(blockedMacAddress)
}

# ------ main ------
blackList <- GetBlackList()
writeSsblackList <- blackList %>% select("IP"="ip", "Description"="hostName") %>% arrange("Description")
blockedMacAddressFromConfig <- GetBlockedMacAddress()
dummy <- c('blackList', 'writeSsblackList', 'blockedMacAddressFromConfig') %>% map( ~ TableWriteJson(.))
