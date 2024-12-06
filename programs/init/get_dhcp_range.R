#' Extract DHCP IP Ranges from Configuration
#' This script parses a DHCP server configuration file to extract IP ranges assigned to each interface. 
#' The extracted data is returned as a structured data frame, including interfaces and their respective IP ranges.
#' @file get_dhcp_range.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
# ------ libraries ------
# ------ constants ------
# ------ functions ------
GetDhcpSection <- function(configFile) {
  start <- NA
  end <- NA
  for (i in seq_len(length(configFile))) {
    if (str_detect(configFile[i], "config system dhcp server")) {
      start <- i
    }
    if (!is.na(start) && str_detect(configFile[i], "^end")) {
      end <- i
      break
    }
  }
  if (is.na(start) || is.na(end)) {
    stop("Failed to obtain DHCP address range.")
  }
  return(configFile[start:end])
}

ParseDhcpSection <- function(dhcpConfig) {
  interface <- NA
  startIp <- NA
  endIp <- NA
  dhcpIpList <- list()
  
  for (line in dhcpConfig) {
    if (str_detect(line, kSetInterFace)) {
      interface <- GetInterFace(line)
    }
    if (str_detect(line, "set start-ip ")) {
      startIp <- GetConfigValue(line, "set start-ip ")
    }
    if (str_detect(line, "set end-ip ")) {
      endIp <- GetConfigValue(line, "set end-ip ")
    }
    if (!is.na(startIp) && !is.na(endIp)) {
      dhcpIpList[[interface]] <- GetIpRangeList(startIp, endIp)
      startIp <- NA
      endIp <- NA
    }
  }
  return(dhcpIpList)
}

CreateDhcpDataFrame <- function(dhcpIpList) {
  map2(
    names(dhcpIpList),
    dhcpIpList,
    ~ tibble(interface = .x, ip = .y)
  ) %>% bind_rows()
}

GetDhcpRange <- function() {
  dhcpConfig <- GetDhcpSection(configFile)
  dhcpIpList <- ParseDhcpSection(dhcpConfig)
  df_dhcp <- CreateDhcpDataFrame(dhcpIpList)
  return(df_dhcp)
}

# ------ main ------