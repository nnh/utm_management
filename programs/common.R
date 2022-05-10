# ------ constants ------
kTargetLog <- c("Admin and System Events Report",
                "User Report without guest",
                "Bandwidth and Applications Report without guest",
                "Client Reputation without guest",
                "List of terminals connected to DataCenter",
                "List of terminals connected vpn",
                "List of terminals connected to nmccrc")
kIpRegex <- '((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])'
kDomainRegex <- '"[a-z].*\\.[a-z]+"'
kExcludedCsvIpIdx <- 1
kExcludedCsvDomainIdx <- 2
kWhoisCsvName <- 'whois.csv'
# ------ functions ------
#' @title ReadLog
#' @param input_file_path : Full path of csv to read
#' @return String vector
ReadLog <- function(input_file_path){
  os <- .Platform$OS.type  # mac or windows
  con <- file(description=input_file_path, open="rt")
  if (os == "unix"){
    lines <- iconv(readLines(con=con, encoding="utf-8"), from ="utf-8",  to = "utf-8")
  } else{
    lines <- iconv(readLines(con=con, encoding="utf-8"), from ="utf-8",  to = "cp932")
  }
  close(con=con)
  return(lines)
}
#' @title GetLogFullName
#' @param target target file name
#' @param file_list file list
#' @return Full name of target file
GetLogFullName <- function(target, file_list){
  temp_idx <- str_which(file_list, target)
  if (length(temp_idx) > 0){
    return(file_list[temp_idx])
  } else {
    return(NA)
  }
}
#' @title ReadUtmLogs
#' @param input_path Full path of csv to read
#' @param target_names target file names
#' @return List of loaded logs
ReadUtmLogs <- function(input_path, target_names){
  # Read utm log
  file_list <- list.files(input_path)
  target_file_list <- sapply(target_names, GetLogFullName, file_list)
  if (anyNA(target_file_list)) {
    stop(str_c("必要なファイルをダウンロードして再実行してください"))
    return(NA)
  }
  raw_log_list <- sapply(str_c(input_path, "/", target_file_list), ReadLog)
  return(raw_log_list)
}
#' @title ReadExcludedCsv
#' @param ext_path Full path of csv to read
#' @return List of loaded logs
ReadExcludedCsv <- function(ext_path){
  raw_excluded <- read.csv(str_c(ext_path, "/excluded.csv"), as.is=T, na.strings="", fileEncoding="UTF-8")
  if (!exists("raw_excluded")){
    return(NA)
  }
  exclude <- raw_excluded %>% filter(!is.na(Description))
  ip_address <- exclude %>% filter(str_detect(IP, kIpRegex))
  domain <- exclude %>% filter(!str_detect(IP, kIpRegex))
  return(list(ip_address, domain))
}
#' @title GetVpnLog
#' @param none
#' @return VPN connection information data frame.
GetVpnLog <- function(){
  # Read vpn access log
  vpn_access_log <- read_excel(input_vpn_log_path, sheet="connected_from") %>% filter(!is.na(ユーザー)) %>%
    select(IP=接続元IPアドレス, User=ユーザー) %>% distinct_all()
  vpn_access_log$Duplicate <- "FALSE"
  vpn_access_log$Department <- ""
  vpn_access_log$Hostname <- ""
  vpn_access_log$MAC_Address <- ""
  return(vpn_access_log)
}
#' @title GetWhoisCsv
#' @param none
#' @return whois information data frame.
GetWhoisCsv <- function(){
  whois_csv <- read.csv(str_c(ext_path, '/', kWhoisCsvName), na='') %>%
    map( ~ { str_replace_na(.) %>% str_replace_all('NA', '') }) %>% as.data.frame()
  if (!exists("whois_csv")){
    error_f <- T
    stop('error:Get whois_csv')
  }
  return(whois_csv)
}
# ------ Get project path ------
os <- .Platform$OS.type  # mac or windows
parent_path <- ""
if (os == "unix"){
  volume_str <- "/Volumes"
} else{
  volume_str <- "//aronas"
}
#input_parent_path <- paste0(volume_str, "/Archives/ISR/SystemAssistant/monthlyOperations/unauthorizedAccessLogs/")
input_parent_path <- "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/ISR/vpn_utm_log/UTM/unauthorizedAccessLogs/"
#target_yyyymm <- "201906"
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  last_month <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  yyyymm <- str_c(format(last_month, "%Y"), format(last_month, "%m"))
}
utm_dir_name <- str_c("UTM Logs ", yyyymm)
parent_path <- paste0(input_parent_path, utm_dir_name)
input_path <- paste0(parent_path, "/input")
ext_path <- paste0(parent_path, "/ext")
output_path <- paste0(parent_path, "/output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# vpn logs
#input_vpn_log_path <- paste0(volume_str, "/Archives/ISR/SystemAssistant/monthlyOperations/vpnAndRoomAccessLogs/VPN CardLogs ", yyyymm, ".xlsm")
input_vpn_log_path <- paste0("/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/ISR/vpn_utm_log/VPN/VPN CardLogs ", yyyymm, ".xlsm")
if (os == "unix"){
  input_vpn_log_path <- iconv(input_vpn_log_path, from="utf-8", to="utf-8")
} else{
  input_vpn_log_path <- iconv(input_vpn_log_path, from="utf-8", to="cp932")
}
