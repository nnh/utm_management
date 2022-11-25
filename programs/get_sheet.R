# Format UTM log
# Mariko Ohtsuka
# 2019/10/2 created
# 2022/11/25 modified
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(googlesheets4)
library(openxlsx)
library(readxl)
# ------ functions ------
SetBlacklistInfo <- function(config_filename){
  raw_config <- config_filename %>% str_c(ext_path, '/', .) %>% read_file()
  kConfigEditHead <- '(?<=edit\\s"'
  kConfigEditFoot <- '"\\n)[\\s]+set\\suuid.*\\n[\\s]+set\\s'
  kConfigNext <- '(?=\\n[\\s]+next)'
  kConfigRemoveHead <- '^[\\s]+set\\suuid\\s\\S+\\s+set\\s'
  str_extract_blackAddressList <- str_c(kConfigEditHead, 'BlackList', kConfigEditFoot, 'member.*', kConfigNext)
  str_remove_blackAddressList <- str_c(kConfigRemoveHead, 'member\\s')
  black_addresslist <- raw_config %>% str_extract(str_extract_blackAddressList) %>%
    str_remove(str_remove_blackAddressList) %>% str_remove_all('\"') %>% str_split('\\s') %>% .[[1]]
  black_ip_range <- raw_config %>% str_extract('(?<=config\\sfirewall\\saddress\\n)[\\s|\\S]+(?=end\\nconfig\\sfirewall\\smulticast-address)') %>% str_split('next\n')
  temp <- black_ip_range[[1]] %>% str_extract_all('^.*Black[\\s|\\S]*', simplify=T)
  temp <- temp[which(nchar(temp) > 0)]
  Description <- temp %>% str_extract('Black[0-9|-].*(?=\\")')
#  IP <- temp %>% str_extract('(?<=set\\ssubnet\\s)[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+')
  IP <- temp %>% str_extract('(?<=set\\ssubnet\\s)[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+|(?<=set\\sfqdn\\s\").*(?=\")')
  # Delete addresses that are not registered in the group.
  black_address <- tibble(IP, Description) %>% filter(Description %in% black_addresslist)
  return(black_address)
}
#' @title GetMaintenanceIpInfo
#' @param static_ip_table a data frame
#' @param anet_ip_list a vector
#' @return A vector of IP addresses for maintenance
GetMaintenanceIpInfo <- function(static_ip_table, anet_ip_list){
  dhcp_info <- static_ip_table %>% filter((ホスト名 == "DHCP Start" | ホスト名 == "DHCP End") & (セグメント名 == "nmccrc" |セグメント名 == "datacenter")) %>%
    separate(IPアドレス, c("ip1", "ip2", "ip3", "ip4"), sep="\\.") %>%
    arrange(as.numeric(ip1), as.numeric(ip2), as.numeric(ip3), as.numeric(ip4))
  # VPN only connection from nmccrc
  dhcp_info <- dhcp_info %>% filter(ip1 == "192" | (ip1 == "172" & ip3 == 0))
  dhcp_info$end <- ''
  i <- 1
  while(i <= nrow(dhcp_info)){
    dhcp_info[i, "end"] <- dhcp_info[i + 1, "ip4"]
    i <- i + 2
  }
  dhcp_info <- dhcp_info %>% filter(end != '')
  dc_maintenance_ip_range <- NULL
  for (i in 1:nrow(dhcp_info)){
    network <- str_c(dhcp_info[i, "ip1"], dhcp_info[i, "ip2"], dhcp_info[i, "ip3"], sep=".")
    host_range <- as.numeric(dhcp_info[i, "ip4"]):as.numeric(dhcp_info[i, "end"])
    dc_maintenance_ip_range <- c(dc_maintenance_ip_range, str_c(network, host_range, sep="."))
  }
  # Get maintenance company information
  anet_maintenance_ip_range <- map(anet_ip_list, function(x){
    subnet_mask <- str_split(x, "/") %>% unlist()
    if (subnet_mask[2] == 24){
      host <- 0:255
      network <- subnet_mask[1] %>% str_extract("[0-9]+\\.[0-9]+\\.[0-9]+\\.")
      return(str_c(network, host))
    } else {
      return(subnet_mask[1])
    }
  }) %>% unlist()
  maintenance_ip_range <- c(dc_maintenance_ip_range, anet_maintenance_ip_range, "127.0.0.1")
  return(maintenance_ip_range)
}
#' @title readDhcpTxt
#' @param dhcpFileEncoding File Encoding String
#' @param dhcpFileName The file name
#' @return a data frame
readDhcpTxt <- function(dhcpFileEncoding, dhcpFileName){
  return (read.delim(str_c(ext_path, '/', dhcpFileName), header=F, as.is=T, fileEncoding=dhcpFileEncoding))
}
# ------ main ------
source(here("programs", "common.R"), encoding="UTF-8")
address_list <- read.csv(str_c(ext_path, "/sinet.txt"), header=T, as.is=T)
raw_config <- list.files(ext_path) %>% str_extract('[A-Z]{6}[0-9]{2}_[0-9]{8}_[0-9]{4}\\.conf') %>% na.omit()
error_f <- F
if (length(raw_config) == 0){
  error_f <- T
  stop('configを所定の場所に格納して再実行してください')
}
if (!error_f){
  black_address <- SetBlacklistInfo(raw_config)
  if (!exists('black_address')){
    error_f <- T
    stop('error:SetBlacklistInfo')
  }
}
if (!error_f){
  vpn_access_log <- GetVpnLog()
  if (!exists("vpn_access_log")){
    error_f <- T
    stop(str_c("VPN CardLogs ", yyyymm, ".xlsmを所定のフォルダに格納して再実行してください"))
  }
}
if (!error_f){
  # Read utm log
  raw_log_list <- ReadUtmLogs(input_path, kTargetLog)
  if (!exists("raw_log_list")){
    error_f <- T
  }
}
# Get DHCP list
if (!error_f){
  dhcpFileName <- 'dhcp.txt'
  dhcpFileEncoding <- 'utf-8'
  dhcpBlankHostname <- '!blank_hostname'
  tryCatch(
    expr = {
      list_dhcp <- readDhcpTxt(dhcpFileEncoding, dhcpFileName)
    },
    warning = function(e) {
      dhcpFileEncoding <<- 'utf-16'
      list_dhcp <<- readDhcpTxt(dhcpFileEncoding, dhcpFileName)
    }
  )
  if (!exists("list_dhcp")){
    error_f <- T
    stop('error:Get dhcp.txt')
  }
  for (i in 1:nrow(list_dhcp)){
    if (str_detect(list_dhcp[i, 1], '\\s*192|172') && list_dhcp[i, 4] == ''){
      list_dhcp[i, 4] <- dhcpBlankHostname
    }
  }
}

# google authentication
gs4_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
# Get URL list
range_write(ss=filter(address_list, ID == "excluded")$Item, data=black_address, sheet='blacklist', range='A1', col_names=T)
if (!error_f){
  # Get Fortigate users
  fortigate_user_info <- filter(address_list, ID == "fortigate_id")$Item %>% read_sheet(sheet="FortiGate", na="", col_names=T, skip=2) %>% as.data.frame()
  if (!exists("fortigate_user_info")){
    error_f <- T
    stop(str_c("error:Get Fortigate users"))
  } else {
    fortigate_users <- fortigate_user_info[ ,"User", drop=T]
  }
}
if (!error_f){
  # Get PC information
  sinet_table <- filter(address_list, ID == "sinet")$Item %>% read_sheet()
  static_ip_table <- filter(address_list, ID == "static_ip")$Item %>% read_sheet()
  if (!exists("sinet_table")){
    error_f <- T
    stop(str_c("error:Get sinet_table"))
  }
  if (!exists("static_ip_table")){
    error_f <- T
    stop(str_c("error:Get static_ip_table"))
  }
}
# Get maintenance company information
if (!error_f){
  anet_ip_list <- filter(address_list, ID == "anet_ip_list")$Item %>% str_split(",") %>% unlist()
  if (!exists("anet_ip_list")){
    error_f <- T
    stop('error:Get anet ip')
  } else {
    maintenanceIpInfo <- GetMaintenanceIpInfo(static_ip_table, anet_ip_list)
  }
  if (!exists("maintenanceIpInfo")){
    error_f <- T
    stop('error:Get maintenanceIpInfo')
  }
}
# Get Blacklist
if (!error_f){
  blacklist <- filter(address_list, ID == "excluded")$Item %>% read_sheet(sheet="blacklist")
  blacklist$Subnet_mask <- ""
  blacklist <- blacklist %>% select(IP, Subnet_mask, User=Description)
  if (!exists("blacklist")){
    error_f <- T
    stop('error:Get Blacklist')
  }
}
if (!error_f){
  whois_csv <- GetWhoisCsv()
  if (!exists("whois_csv")){
    error_f <- T
    stop('error:Get whois_csv')
  }
}
if (!error_f){
  # Get Checklist
  checklist <- filter(address_list, ID == "excluded")$Item %>% read_sheet(sheet="ChecklistTemplate", col_names=F)
  if (!exists("checklist")){
    error_f <- T
    stop('error:Get Whitelist')
  }
}
if (!error_f){
  print('実行準備が完了しました。')
} else {
  print('実行準備中にエラーが発生しました。')
}
