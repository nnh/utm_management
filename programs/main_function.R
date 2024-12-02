#' title
#' description
#' @file main_function.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
# ------ libraries ------
# ------ constants ------
# ------ functions ------
GetIpAddressesAndDomains <- function(tables) {
  tables_vec <- tables %>% map( ~ {
    table <- .
    res <- table %>% map( ~ unlist(.)) %>% unlist()
    return(res)
  }) %>% unlist()
  ipAddresses <- tables_vec %>% str_extract(kIpAddr) %>% na.omit() %>% unique() %>% tibble(ip=.)
  domains <- tables_vec %>% str_extract("[a-z0-9.-]+\\.[a-z]{2,}") %>% na.omit() %>% unique() %>% tibble(ip=.)
  res <- ipAddresses %>% bind_rows(domains)
  return(res)
}
JoinReportAndUserInfo <- function(target, itemName, key) {
  temp <- target[[itemName]] %>% left_join(userInfo, by=key)
  target[[itemName]] <- temp
  return(target)
}
JoinReportAndUserInfoByTable <- function(tables, tableInfo) {
  kUsageStr <- "Usage: "
  tableName <- tableInfo$tableName
  targetItemAndColumn <- tableInfo$targetItemAndColumn
  targetTable <- tables[[tableName]]
  for (i in 1:length(targetItemAndColumn)) {
    itemName <- targetItemAndColumn[[i]]$itemName
    columnName <- targetItemAndColumn[[i]]$columnName
    key <- targetItemAndColumn[[i]]$key
    if (nrow(targetTable[[itemName]]) > 0) {
    
      if (tableName == kAdminAndSystemEvents) {
        targetTable[[itemName]][[key]] <- targetTable[[itemName]] %>% .[ , columnName, drop=T] %>% str_extract(kIpAddr)
      }
      if (tableName == kUserReport & !is.null(columnName)) {
        targetTable[[itemName]][[key]] <- targetTable[[itemName]] %>% .[ , columnName, drop=T] %>% str_extract(kIpAddr)
        usage_str <- targetTable[[itemName]] %>% .[ , columnName, drop=T] %>% 
          str_extract(str_c(kUsageStr, "[0-9.]+ [A-Za-z]+")) %>% str_remove(kUsageStr)
        rank_str <- targetTable[[itemName]] %>% .[ , columnName, drop=T] %>% str_split_i(":", 1)
        targetTable[[itemName]]$usage <- usage_str
        targetTable[[itemName]][[columnName]] <- rank_str
      }
      targetTable <- targetTable %>% JoinReportAndUserInfo(itemName, key)
      if (tableName == kUserReport & is.null(columnName)) {
        targetTable[[itemName]]$user <- NULL
        targetTable[[itemName]]$description <- NULL
        targetTable[[itemName]]$macAddress <- NULL
        targetTable[[itemName]] <- targetTable[[itemName]] %>% rename("destinationHost"="hostName")
      }
    }
  }
  
  tables[[tableName]] <- targetTable
  return(tables)
}
SetDhcpReleased <- function(userInfo) {
  dhcpRange <- fromJSON(file.path(ext_path, "dhcpIpRange.json"))
  dhcpReleased <- userInfo %>% inner_join(dhcpRange, by="ip") %>% filter(is.na(hostName))
  dhcpReleased$hostName <- "DHCPリリース済みのため詳細確認不可"
  dhcpReleased$user <- dhcpReleased$hostName
  dhcpReleased$interface <- NULL
  others <- userInfo %>% anti_join(dhcpReleased, by="ip")
  res <- others %>% bind_rows(dhcpReleased) 
  res <- res %>% arrange("ip")
  return(res)    
}
SetUnregistered <- function(userInfo) {
  userInfo$user <- ifelse(!is.na(userInfo$hostName) & !is.na(userInfo$macAddress) & is.na(userInfo$user),
                          kUnregistered, 
                          userInfo$user)
  return(userInfo)
}
JoinUserInfo <- function(ipAddresses, deviceList) {
  # domain
  domainList <- ipAddresses %>% filter(!str_detect(ip, kIpAddr))
  deviceDomainList <- deviceList %>% filter(!str_detect(ip, kIpAddr)) 
  userDomainInfo <- domainList$ip %>% map_df( ~ {
    target <- .
    res <- deviceDomainList %>% map_dfc( ~ NA)
    for (i in 1:nrow(deviceDomainList)) {
      if (str_detect(target, str_c(deviceDomainList[i, "ip"], "$"))) {
        res <- deviceDomainList[i, ]
        break
      }
    }
    res$ip <- target
    return(res)
  })
  ipList <- ipAddresses %>% filter(str_detect(ip, kIpAddr))
  deviceIpList <- deviceList %>% filter(str_detect(ip, kIpAddr))
  userIpInfo <- ipList %>% left_join(deviceIpList, by="ip")
  userIpAndDomainInfo <- userIpInfo %>% bind_rows(userDomainInfo)
  res <- userIpAndDomainInfo %>% SetDhcpReleased()
  res <- res %>% SetUnregistered()
  return(res)
}
SetTableInfo <- function() {
  target <- kTargetFiles %>% map( ~ list(tableName=.,targetItemAndColumn=NA))
  target[[1]]$targetItemAndColumn <- list(
    list(itemName="Login Summary", columnName="Login_Interface", key="ip"),
    list(itemName="List of Failed Logins", columnName="Login_Source", key="ip")
  )
  target[[2]]$targetItemAndColumn <- list(
    list(itemName="Top 30 Users by Bandwidth and Sessions", columnName=NULL, key=c("User_or_IP_"="ip")),
    list(itemName="Top 30 Destination by Bandwidth and Sessions", columnName=NULL, key=c("Hostname_or_IP_"="ip"))
  )
  target[[3]]$targetItemAndColumn <- list(
    list(itemName="レピュテーションスコアの上位ユーザー", columnName=NULL, key=c("User__or_IP_"="ip")),
    list(itemName=" 直近2期間にスコアが増加した上位ユーザー", columnName=NULL, key=c("User__or_IP_"="ip")),
    list(itemName="レピュテーションスコアの大きい上位デバイス", columnName=NULL, key=c("Device2"="ip"))
  )
  kListOfTerminalsTarget <- list(
    list(itemName="Top 100 Users by Bandwidth and Sessions", columnName=NULL, key=c("User_or_IP_"="ip"))
  )
  target[[4]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[5]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[6]]$targetItemAndColumn <- kListOfTerminalsTarget
  target[[7]]$targetItemAndColumn <- list(
    list(itemName="top10Destinations", columnName=NULL, key=c("Destination"="ip")),
    list(itemName="top10Destinations", columnName="rank", key="ip")
  )
  return(target)
}
GetOutputPath <- function(parent_path) {
  output_path <- parent_path %>% file.path("output")
  if (file.exists(output_path) == F) {
    dir.create(output_path)
  }
  return(output_path)
}
# ------ main ------