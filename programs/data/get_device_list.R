#' Script to obtain device information.
#'
#' @file get_device_list.R
#' @author Mariko Ohtsuka
#' @date 2026.11.4
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
      df_dhcp <<- temp %>% EditDhcpList()
    }
  )
  if (nrow(df_dhcp) == 0) {
    if (nrow(list_dhcp) == 0) {
      stop("list_dhcp has a length of zero.")
    }
    df_dhcp <- list_dhcp %>% EditDhcpList()
  }
  if (!exists("df_dhcp")) {
    stop("dhcp.txt is missing.")
  }
  return(df_dhcp)
}
EditDhcpList <- function(df) {
  ip <- df$V1 %>% trimws()
  checkMacAddressCol <- F
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      if (!is.na(df[i, j])) {
        if (str_detect(df[i, j], "([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}|([0-9A-Fa-f]{4}\\.){2}[0-9A-Fa-f]{4}")) {
          checkMacAddressCol <- T
          break
        }
      }
    }
    if (checkMacAddressCol) {
      break
    }
  }
  if (j == 3) {
    macAddress <- df$V3
    hostName <- ifelse(df$V4 == "", kNoDhcpMessage, df$V4)
  }
  if (j == 2) {
    macAddress <- df$V2
    temp_v3 <- str_split(df$V3, "\\s+")
    hostName <- temp_v3 %>% map_chr(~ {
      if (.[1] == "") {
        return(NA)
      } else {
        return(.[1])
      }
    })
  }
  temp_tibble <- tibble(ip, macAddress, hostName)
  res <- temp_tibble %>% filter(str_detect(ip, kIpAddr))
  return(res)
}

GetDeviceList <- function() {
  staticIp <- GetStaticIpFromJson() %>% filter(!hostName %in% c("DHCP Start", "DHCP End"))
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
  if (!(is.atomic(vpn) && length(vpn) == 1 && is.na(vpn))) {
    if (nrow(vpn) > 0) {
      vpn$hostName <- "VPN接続"
      deviceListHostName <- deviceListHostName %>% bind_rows(vpn)
    }
  }
  vpnLocalIp <- file.path(ext_path, "vpnLocalIp.json") %>% fromJSON()
  if (length(vpnLocalIp) > 0 && !all(is.na(vpnLocalIp))) {
    df_vpnLocalIp <- tibble(ip = vpnLocalIp)
    df_vpnLocalIp$hostName <- "vpn user"
    deviceListHostName <- deviceListHostName %>% bind_rows(df_vpnLocalIp)
  }
  res <- deviceListHostName %>% arrange(ip)
  return(res)
}
GetStaticIpFromJson <- function() {
  items <- addressList %>%
    filter(ID == "static_ip_table_path") %>%
    .$Item %>%
    trimws() %>%
    file.path(GetBoxDir(), .) %>%
    read_json() %>%
    .$value
  staticIp <- items %>%
    map(~ tibble(
      ip = .x$Title %||% NA_character_,
      hostName = .x$DeviceName %||% NA_character_,
      description = .x$Dept$Value %||% NA_character_,
      assignedUser = .x$AssignedUser$DisplayName %||% NA_character_,
      note = .x$Notes %||% NA_character_
    )) %>%
    bind_rows() %>%
    filter(!is.na(description))
  staticIpSheet <- "staticIpSheet.json" %>%
    file.path(ext_path, .) %>%
    fromJSON() %>%
    filter(!is.na(ホスト名)) %>%
    select(c("ip" = "IPアドレス", "hostName" = "ホスト名", "description" = "設置場所", "assignedUser" = "管理者", "note" = "用途"))
  # IPアドレスが重複する場合はstaticIpTable.jsonの情報を残す
  staticIp <- staticIp %>%
    bind_rows(staticIpSheet) %>%
    distinct(ip, .keep_all = TRUE)
  staticIp$user <- ifelse(!is.na(staticIp$assignedUser), staticIp$assignedUser, staticIp$note)
  staticIp$assignedUser <- NULL
  staticIp$note <- NULL
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
  temp_privateHostNameAndUser <- privateHostNameAndUser %>% filter(is.na(user))
  temp_nonNa_privateHostNameAndUser <- privateHostNameAndUser %>% filter(!is.na(user))
  for (i in 1:nrow(temp_privateHostNameAndUser)) {
    for (j in 1:nrow(sinetTable)) {
      if (!is.na(sinetTable[j, "key"]) & !is.na(temp_privateHostNameAndUser[i, "key"])) {
        if (str_starts(sinetTable[j, "key"], temp_privateHostNameAndUser[i, "key"][[1]])) {
          temp_privateHostNameAndUser[i, "user"] <- sinetTable[j, "user"]
          temp_privateHostNameAndUser[i, "description"] <- sinetTable[j, "description"]
          break
        }
      }
    }
  }
  privateHostNameAndUser <- bind_rows(temp_privateHostNameAndUser, temp_nonNa_privateHostNameAndUser)
  privateHostNameAndUser$key <- NULL
  privateAddresseInfo <- privateHostNameAndUser %>%
    inner_join(privateAddresses, by = "hostName", relationship = "many-to-many")
  res <- privateAddresseInfo %>%
    bind_rows(publicAddresses) %>%
    arrange("ip")
  res$user <- ifelse(res$hostName %in% kNoDhcpMessage, kNoDhcpMessage, res$user)
  return(res)
}
# ------ main ------
