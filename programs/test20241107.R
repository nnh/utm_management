#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(xml2)
# ------ constants ------
kIpAddr <- "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
kUserReport <- "User Report without guest"
kTrafficSummary <- "Traffic Summary"
kTargetFiles <- c("Admin and System Events Report", 
                  "Bandwidth and Applications Report without guest", 
                  "Client Reputation without guest", 
                  "List of terminals connected to DataCenter", 
                  "List of terminals connected to nmccrc", 
                  "List of terminals connected vpn", 
                  "User Report without guest") 
kIpColumns <- c("Login_Interface", "Login_Source", "Destination", "Hostname_or_IP_", "User__or_IP_", "User_or_IP_")
# ------ functions ------
GetHomeDir <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    home_dir <- Sys.getenv("USERPROFILE")
  } else if (os == "Darwin") {
    home_dir <- Sys.getenv("HOME")
  } else {
    stop("Unsupported OS")
  }
  return (home_dir)
}
GetTagAndNames <- function(xml_data, tagName) {
  target <- xml_data %>% xml_find_all(str_c(".//", tagName))
  target_names <- target %>% xml_attr("name")
  names(target) <- target_names
  return(target)  
}
AddSequenceToDuplicates <- function(vec) {
  # データフレームに変換して処理
  res <- tibble(name = vec) %>%
    group_by(name) %>%
    mutate(name_with_num = if (n() > 1) paste0(name, row_number()) else name) %>%
    ungroup() %>%
    pull(name_with_num)
  return(res)
}
ExtractIdData <- function(table_node) {
  table_name <- table_node %>% xml_attr("name")
  if (table_name == kTrafficSummary) {
    id_tags <- table_node %>% list()
  } else {
    id_tags <- table_node %>% xml_find_all(".//id")
  }
  data_list <- id_tags %>% map_df( ~ {
    # 子タグを取得
    child_tags <- xml_children(.x)
    
    # 子タグの名前と内容をリストに格納
    tag_names <- xml_name(child_tags) %>% AddSequenceToDuplicates()
    tag_values <- xml_text(child_tags)
    df <- tag_values %>% t() %>% as_tibble() %>% setNames(tag_names)
    return(df)
  })
  return(data_list)
}
GetReportTables <- function(xml_data) {
  tables <- xml_data %>% GetTagAndNames("table")
  res <- tables %>% map( ~ ExtractIdData(.))
  return(res)
}
GetBaseName <- function(full_file_path) {
  res <- full_file_path %>% basename() %>% str_remove("-[0-9\\-]+_[0-9]{4}\\.xml")
  return(res)
}
GetTargetFilePath <- function(input_path) {
  inputFiles <- input_path %>% list.files(pattern="*.xml$", full.names=T)
  # 対象外ファイルチェック
  for (i in 1:length(inputFiles)) {
    check_flg <- F
    temp <- inputFiles[i] %>% basename()
    for (j in 1:length(kTargetFiles)) {
      if (str_detect(temp, kTargetFiles[j])) {
        check_flg <- T
        break
      }
    }
    if (!check_flg) {
      print(str_c(temp, "は対象外ファイルです"))
      inputFiles[i] <- NA
    }
  }
  inputFiles <- inputFiles %>% na.omit()
  return(inputFiles)
}
GetRawDataList <- function(filePath) {
  xml_data <- filePath %>% read_xml()
  fileName <- filePath %>% GetBaseName()
  if (fileName == kUserReport) {
    temp <- xml_data %>% xml_find_all(".//Top_5_Users_by_Bandwidth")
    temp_name <- temp %>% xml_attr("name")
    temp_res <- temp %>% map( ~ GetReportTables(.))
    names(temp_res) <- temp_name
    res <- temp_res %>% EditUserReport()
  } else {
    res <- xml_data %>% GetReportTables()
  }
  return(res)
}
EditUserReport <- function(input_list) {
  trafficSummary <- NULL
  top10Destinations <- NULL
  temp_names <- input_list %>% names()
  for (i in 1:length(input_list)) {
    temp <- input_list[[i]]
    trafficSummary <- temp$`Traffic Summary` %>% bind_rows(trafficSummary, .)
    tempTop10Destinations <- temp$`Top 10 Destinations`
    tempTop10Destinations$rank <- temp_names[[i]]
    top10Destinations <- top10Destinations %>% bind_rows(tempTop10Destinations)
  }
  res <- list()
  res$trafficSummary <- trafficSummary
  res$top10Destinations <- top10Destinations
  return(res)
}
CombineRowsIpAddressList <- function(data) {
  res <- data %>%
    group_by(ip) %>%
    summarise(across(everything(), ~ paste(unique(.), collapse = ",")), .groups = "drop")
  res <- res %>% select(-"Duplicate")
  return(res)
}
GetDhcp <- function() {
  dhcp <- file.path(ext_path, "dhcp.txt") %>% read_lines() %>% 
    trimws() %>% 
    str_extract(str_c("^", kIpAddr, ".*$")) %>% 
    na.omit() %>%
    str_split("\t")
  df_dhcp <- dhcp %>% map( ~ {
    ip <- .[1]
    macAddress <- .[3]
    hostName <- ifelse(.[4] == "", "aaa", .[4])
    res <- tibble(ip, macAddress, hostName)
    return(res)
  }) %>% bind_rows()
  return(df_dhcp)
}

GetBlackList <- function() {
  configFileName <- list.files(ext_path) %>% str_extract('[A-Z]{6}[0-9]{2}_[0-9]{8}_[0-9]{4}\\.conf') %>% na.omit()
  configFile <- file.path(ext_path, configFileName) %>% read_lines() %>% trimws()
  blackList <- tibble(ip=character(), hostName=character(), macAddress=character())
  blackListRow <- 0
  for (i in 1:length(configFile)) {
    if (str_detect(configFile[i], "Black[0-9]+")) {
      blackListRow <- blackListRow + 1
      blackList[blackListRow, "hostName"] <- configFile[i] %>% str_extract('".*"') %>% str_remove_all('"')
      temp_row <- i + 1
      while(temp_row < length(configFile)) {
        if (str_detect(configFile[temp_row], "set subnet")) {
          blackList[blackListRow, "ip"] <- configFile[temp_row] %>% str_remove("set subnet ") %>% str_remove(" 255.255.255.255")
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
GetDeviceList <- function() {
  dhcp <- GetDhcp()
  blackList <- GetBlackList()
  deviceList <- dhcp %>% bind_rows(blackList)
  deviceList <- deviceList %>% add_row(ip="127.0.0.1", hostName="localAddress", macAddress=NA)
  res <- deviceList %>% arrange(ip)
  return(res)
}
# ------ main ------
home_dir <- GetHomeDir()
ext_path <- home_dir %>% file.path("Downloads", "ext")
input_path <- home_dir %>% file.path("Downloads", "input")
inputFiles <- input_path %>% GetTargetFilePath()
fileNames <- inputFiles %>% map_chr( ~ GetBaseName(.))
if (!identical(sort(fileNames), sort(kTargetFiles))) {
  stop("入力ファイルが不足しています")
}
tables <- inputFiles %>% map( ~ GetRawDataList(.))
names(tables) <- fileNames
# IPアドレスらしき情報を収集する
ipAddresses <- tables %>% map( ~ {
  table <- .
  res <- table %>% map( ~ unlist(.)) %>% unlist()
  return(res)
}) %>% unlist() %>% str_extract(kIpAddr) %>% na.omit() %>% unique() %>% tibble(ip=.)
# dhcpとか
deviceList <- GetDeviceList()
ipMacHosts <- ipAddresses %>% left_join(deviceList, by="ip")









#temp_ip_list <- ip_list %>% rename("ip"="IP")
#ipAddressList <- temp_ip_list %>% bind_rows(whois_csv) %>% CombineRowsIpAddressList()

#ddd <- tables %>% map( ~ {
#  table <- .
#  res <- table %>% map( ~ {
#    df <- .
#    if (nrow(df) == 0) {
#      return(df)
#    }
#    ipCols <- df %>% select(any_of(kIpColumns))
#    if (ncol(ipCols) != 1) {
#      return(NULL)
#    }
#    targetColName <- colnames(ipCols)
#    ipCols$ip <- ipCols[[targetColName]] %>% str_extract(kIpAddr)
#    temp <- ipCols %>% left_join(ipAddressList, by="ip")
#    res <- df %>% inner_join(temp, by=targetColName) %>% select(-"ip")
#    return(res)
#  })
#})


