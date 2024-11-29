#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
# ------ libraries ------
# ------ constants ------
# ------ functions ------
GetDhcp <- function() {
  temp <- file.path(ext_path, "dhcp.txt") %>% read_lines()
  if (!exists("temp")) {
    stop("dhcp.txt is missing.")
  }
  dhcp <- temp %>% 
    trimws() %>% 
    str_extract(str_c("^", kIpAddr, ".*$")) %>% 
    na.omit() %>%
    str_split("\t")
  df_dhcp <- dhcp %>% map( ~ {
    ip <- .[1]
    macAddress <- .[3]
    hostName <- ifelse(.[4] == "", kNoDhcpMessage, .[4])
    res <- tibble(ip, macAddress, hostName)
    return(res)
  }) %>% bind_rows()
  return(df_dhcp)
}

GetDeviceList <- function() {
  staticIp <- GetStaticIpFromJson() %>% filter(hostName != "DHCP Start" & hostName != "DHCP End")
  deviceList <- staticIp
  dhcp <- GetDhcp()
  deviceList <- deviceList %>% bind_rows(dhcp)
  blackList <- "blackList.json" %>% file.path(ext_path, .) %>% fromJSON()
  deviceList <- deviceList %>% bind_rows(blackList)
  whiteList <- "whitelistTable.json" %>% file.path(ext_path, .) %>% fromJSON() %>% select("ip"="domain", "hostName"="Description1")
  deviceList <- deviceList %>% bind_rows(whiteList)
  whois <- file.path(ext_path, "whois.csv") %>% read_csv(show_col_types=F) %>% rename("hostName"="User", "macAddress"="MAC_Address") %>% 
    select("ip", "hostName", "macAddress")
  deviceList <- deviceList %>% bind_rows(whois)
  deviceList <- deviceList %>% add_row(ip="127.0.0.1", hostName="localAddress", macAddress=NA)
  deviceList$tempSeq <- 1:nrow(deviceList)
  uniqueDeviceList <- deviceList %>%
    group_by(ip) %>%
    slice_min(tempSeq) %>% # 行番号の一番若い情報を残す
    ungroup()
  uniqueDeviceList$tempSeq <-NULL
  deviceListHostName <- uniqueDeviceList %>% setDeviceHostName()
  vpnLocalIp <- file.path(ext_path, "vpnLocalIp.json") %>% fromJSON()
  if (length(vpnLocalIp) > 0) {
    df_vpnLocalIp <- tibble(ip=vpnLocalIp)
    df_vpnLocalIp$hostName <- "vpn user"
    deviceListHostName <- deviceListHostName %>% bind_rows(df_vpnLocalIp)
  }
  res <- deviceListHostName %>% arrange(ip)
  return(res)
}
GetStaticIpFromJson <- function() {
  staticIp <- "staticIpTable.json" %>% file.path(ext_path, .) %>% fromJSON() %>% 
    filter(!is.na(ホスト名)) %>%
    select(c("ip"="IPアドレス", "hostName"="ホスト名", "description"="設置場所", 管理者, 用途))
  staticIp$user <- ifelse(!is.na(staticIp$管理者), staticIp$管理者, staticIp$用途)
  staticIp$管理者 <- NULL
  staticIp$用途 <- NULL
  return(staticIp)  
}
setDeviceHostName <- function(deviceList) {
  privateAddresses <- deviceList %>% filter(!is.na(macAddress)) %>% select(-c("user", "description"))
  publicAddresses <- deviceList %>% filter(is.na(macAddress))
  privateHostNames <- privateAddresses$hostName %>% unique() %>% tibble(hostName=., key=tolower(.))
  sinetTable <- "sinetTable.json" %>% file.path(ext_path, .) %>% fromJSON() %>% filter(is.na(廃棄日)) %>% 
    select(c("user"="使用者名", "hostName"="コンピュータ名", "description"="部署名")) 
  sinetTable$key <- sinetTable$hostName %>% tolower()
  sinetTable$hostName <- NULL
  privateHostNameAndUser <- privateHostNames %>% left_join(sinetTable, by="key")
  privateHostNameAndUser$key <- NULL
  privateAddresseInfo <- privateHostNameAndUser %>% inner_join(privateAddresses, by="hostName", relationship = "many-to-many") 
  res <- privateAddresseInfo %>% bind_rows(publicAddresses) %>% arrange("ip")
  return(res)
}
# ------ main ------