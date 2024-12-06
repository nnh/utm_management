#' Script to obtain device information.
#'
#' @file get_device_list.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
# ------ libraries ------
# ------ constants ------
# ------ functions ------
ReadDhcpTxt <- function(dhcpInputPath, dhcpFileEncoding, dhcpFileName) {
  return(read.delim(dhcpInputPath, header = FALSE, as.is = TRUE, fileEncoding = dhcpFileEncoding, encoding = "UTF-8"))
}
GetDhcp <- function() {
  dhcpFileName <- "dhcp.txt"
  dhcpFileEncoding <- "utf-8"
  dhcpInputPath <- file.path(ext_path, "dhcp.txt")
  tryCatch(
    expr = {
      list_dhcp <<- ReadDhcpTxt(dhcpInputPath, dhcpFileEncoding, dhcpFileName)
      dhcp <- list_dhcp %>%
        trimws() %>%
        str_extract(str_c("^", kIpAddr, ".*$")) %>%
        na.omit() %>%
        str_split("\t")
      df_dhcp <- dhcp %>%
        map(~ {
          ip <- .[1]
          macAddress <- .[3]
          hostName <- ifelse(.[4] == "", kNoDhcpMessage, .[4])
          res <- tibble(ip, macAddress, hostName)
          return(res)
        }) %>%
        bind_rows()
    },
    warning = function(e) {
      dhcpFileEncoding <- "utf-16"
      temp <- ReadDhcpTxt(dhcpInputPath, dhcpFileEncoding, dhcpFileName)
      ip <- temp$V1 %>% trimws()
      macAddress <- temp$V3
      hostName <- ifelse(temp$V4 == "", kNoDhcpMessage, temp$V4)
      temp_tibble <- tibble(ip, macAddress, hostName)
      df_dhcp <<- temp_tibble %>% filter(str_detect(ip, kIpAddr))
    }
  )
  if (!exists("df_dhcp")) {
    stop("dhcp.txt is missing.")
  }
  return(df_dhcp)
}

GetDeviceList <- function() {
  staticIp <- GetStaticIpFromJson() %>% filter(hostName != "DHCP Start" & hostName != "DHCP End")
  deviceList <- staticIp
  dhcp <- GetDhcp()
  deviceList <- deviceList %>% bind_rows(dhcp)
  blackList <- "blackList.json" %>%
    file.path(ext_path, .) %>%
    fromJSON()
  deviceList <- deviceList %>% bind_rows(blackList)
  whiteList <- "whitelistTable.json" %>%
    file.path(ext_path, .) %>%
    fromJSON() %>%
    select("ip" = "domain", "hostName" = "Description1")
  deviceList <- deviceList %>% bind_rows(whiteList)
  deviceList <- deviceList %>% add_row(ip = "127.0.0.1", hostName = "localhost", macAddress = NA)
  deviceList$tempSeq <- seq_len(nrow(deviceList))
  uniqueDeviceList <- deviceList %>%
    group_by(ip) %>%
    slice_min(tempSeq) %>% # 行番号の一番若い情報を残す
    ungroup()
  uniqueDeviceList$tempSeq <- NULL
  deviceListHostName <- uniqueDeviceList %>% setDeviceHostName()
  vpn <- file.path(ext_path, "vpn.json") %>% fromJSON()
  if (nrow(vpn) > 0) {
    vpn$hostName <- "VPN接続"
    deviceListHostName <- deviceListHostName %>% bind_rows(vpn)
  }
  vpnLocalIp <- file.path(ext_path, "vpnLocalIp.json") %>% fromJSON()
  if (length(vpnLocalIp) > 0) {
    df_vpnLocalIp <- tibble(ip = vpnLocalIp)
    df_vpnLocalIp$hostName <- "vpn user"
    deviceListHostName <- deviceListHostName %>% bind_rows(df_vpnLocalIp)
  }
  res <- deviceListHostName %>% arrange(ip)
  return(res)
}
GetStaticIpFromJson <- function() {
  staticIp <- "staticIpTable.json" %>%
    file.path(ext_path, .) %>%
    fromJSON() %>%
    filter(!is.na(ホスト名)) %>%
    select(c("ip" = "IPアドレス", "hostName" = "ホスト名", "description" = "設置場所", 管理者, 用途))
  staticIp$user <- ifelse(!is.na(staticIp$管理者), staticIp$管理者, staticIp$用途)
  staticIp$管理者 <- NULL
  staticIp$用途 <- NULL
  return(staticIp)
}
setDeviceHostName <- function(deviceList) {
  privateAddresses <- deviceList %>%
    filter(!is.na(macAddress)) %>%
    select(-c("user", "description"))
  publicAddresses <- deviceList %>% filter(is.na(macAddress))
  privateHostNames <- privateAddresses$hostName %>%
    unique() %>%
    tibble(hostName = ., key = tolower(.))
  sinetTable <- "sinetTable.json" %>%
    file.path(ext_path, .) %>%
    fromJSON() %>%
    filter(is.na(廃棄日)) %>%
    select(c("user" = "使用者名", "hostName" = "コンピュータ名", "description" = "部署名"))
  sinetTable$key <- sinetTable$hostName %>% tolower()
  sinetTable$hostName <- NULL
  privateHostNameAndUser <- privateHostNames %>% left_join(sinetTable, by = "key")
  privateHostNameAndUser$key <- NULL
  privateAddresseInfo <- privateHostNameAndUser %>%
    inner_join(privateAddresses, by = "hostName", relationship = "many-to-many")
  res <- privateAddresseInfo %>%
    bind_rows(publicAddresses) %>%
    arrange("ip")
  res$user <- ifelse(res$hostName == kNoDhcpMessage, kNoDhcpMessage, res$user)
  return(res)
}
# ------ main ------
