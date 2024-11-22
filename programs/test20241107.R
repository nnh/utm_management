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
    hostName <- ifelse(.[4] == "", "DHCPログのホスト名が空白のため詳細確認不可能", .[4])
    res <- tibble(ip, macAddress, hostName)
    return(res)
  }) %>% bind_rows()
  return(df_dhcp)
}

GetDeviceList <- function(blackList, staticIp) {
  deviceList <- staticIp %>% select("ip", "hostName")
  dhcp <- GetDhcp()
  deviceList <- deviceList %>% bind_rows(dhcp)
  deviceList <- deviceList %>% bind_rows(blackList)
  whois <- file.path(ext_path, "whois.csv") %>% read_csv() %>% rename("hostName"="User", "macAddress"="MAC_Address") %>% 
    select("ip", "hostName", "macAddress")
  deviceList <- deviceList %>% bind_rows(whois)
  deviceList <- deviceList %>% add_row(ip="127.0.0.1", hostName="localAddress", macAddress=NA)
  res <- deviceList %>% arrange(ip)
  return(res)
}
# ------ main ------
input_path <- home_dir %>% file.path("Downloads", "input")
inputFiles <- input_path %>% GetTargetFilePath()
fileNames <- inputFiles %>% map_chr( ~ GetBaseName(.))
if (!identical(sort(fileNames), sort(kTargetFiles))) {
  stop("入力ファイルが不足しています")
}
tables <- inputFiles %>% map( ~ GetRawDataList(.))
names(tables) <- fileNames
# IPアドレスらしき情報を収集する
tables_vec <- tables %>% map( ~ {
  table <- .
  res <- table %>% map( ~ unlist(.)) %>% unlist()
  return(res)
}) %>% unlist()
ipAddresses <- tables_vec %>% str_extract(kIpAddr) %>% na.omit() %>% unique() %>% tibble(ip=.)
domains <- tables_vec %>% str_extract("[a-z0-9.-]+\\.[a-z]{2,}") %>% na.omit() %>% unique() %>% tibble(domain=.)
staticIp <- "staticIpTable.json" %>% file.path(ext_path, .) %>% fromJSON() %>% 
  filter(!is.na(ホスト名)) %>%
  select(c("ip"="IPアドレス", "hostName"="ホスト名", "description"="設置場所", "user"="管理者"))
sinetTable <- "sinetTable.json" %>% file.path(ext_path, .) %>% fromJSON() %>%
  select(c("user"="使用者名", "hostName"="コンピュータ名", "登録日", "廃棄日"))

blackList <- "blackList.json" %>% file.path(ext_path, .) %>% fromJSON()
deviceList <- GetDeviceList(blackList, staticIp)

ipMacHosts <- ipAddresses %>% left_join(deviceList, by="ip")








