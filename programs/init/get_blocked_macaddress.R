#' Extract Blocked MAC Addresses from DHCP Server Configuration
#' This script provides functions to parse a DHCP server configuration file and extract blocked MAC addresses,
#' including details such as interface, action, and description. The extracted data is returned as a structured
#' data frame.
#' @file get_blocked_macaddress.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
# ------ libraries ------
# ------ constants ------
# ------ functions ------
# Helper function to find the DHCP section
FindDhcpSection <- function(configFile) {
  start <- NA
  end <- NA
  for (i in seq_len(length(configFile))) {
    if (configFile[i] == "config system dhcp server") {
      start <- i
    }
    if (!is.na(start) && configFile[i] == "end") {
      end <- i
      break
    }
  }
  return(configFile[start:end])
}

# Helper function to parse a single reserved address block
ParseReservedAddressBlock <- function(block, interFace) {
  temp <- list(interFace = interFace)
  for (line in block) {
    if (str_detect(line, "set mac ")) {
      temp$macAddress <- GetConfigValue(line, "set mac ")
    }
    if (str_detect(line, "set action block")) {
      temp$action <- GetConfigValue(line, "set action ")
    }
    if (str_detect(line, "set description ")) {
      temp$description <- GetConfigValue(line, "set description ")
    }
  }
  return(temp)
}

# Helper function to extract blocked MAC addresses
ExtractBlockedMacAddresses <- function(dhcpSection) {
  blocked_f <- FALSE
  blockedMacAddress <- list()
  temp <- list()
  for (i in seq_len(length(dhcpSection))) {
    if (str_detect(dhcpSection[i], kSetInterFace)) {
      interFace <- dhcpSection[i] %>% GetInterFace()
    }
    if (str_detect(dhcpSection[i], "config reserved-address")) {
      blocked_f <- TRUE
    }
    if (blocked_f) {
      if (str_detect(trimws(dhcpSection[i]), "end")) {
        blocked_f <- FALSE
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

# Helper function to convert blocked MAC addresses to a data frame
ConvertToDataFrame <- function(blockedMacAddress) {
  df <- data.frame()
  colnames <- blockedMacAddress %>%
    map(~ names(.)) %>%
    unique() %>%
    list_c()

  for (row in seq_len(length(blockedMacAddress))) {
    for (col in seq_len(length(blockedMacAddress[[row]]))) {
      df[row, col] <- blockedMacAddress[[row]][[col]]
    }
  }
  colnames(df) <- colnames
  return(df)
}

# Main function
GetBlockedMacAddress <- function() {
  dhcpSection <- FindDhcpSection(configFile)
  blockedMacAddress <- ExtractBlockedMacAddresses(dhcpSection)
  return(ConvertToDataFrame(blockedMacAddress))
}

# ------ main ------
