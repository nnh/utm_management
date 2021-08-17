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
# ------ Get project path ------
os <- .Platform$OS.type  # mac or windows
parent_path <- ""
if (os == "unix"){
  volume_str <- "/Volumes"
} else{
  volume_str <- "//aronas"
}
input_parent_path <- str_c(volume_str, "/Archives/ISR/SystemAssistant/月例・随時作業関連/UTMログ/レポート/")
#target_yyyymm <- "201906"
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  last_month <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  yyyymm <- str_c(format(last_month, "%Y"), format(last_month, "%m"))
}
utm_dir_name <- str_c("UTM Logs ", yyyymm)
parent_path <- str_c(input_parent_path, utm_dir_name)
input_path <- str_c(parent_path, "/input")
ext_path <- str_c(parent_path, "/ext")
output_path <- here("output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# vpn logs
input_vpn_log_path <- str_c(volume_str, "/Archives/ISR/SystemAssistant/月例・随時作業関連/VPN・入退室ログ/VPN CardLogs ", yyyymm, ".xlsm")
