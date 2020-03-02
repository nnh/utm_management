# main.R実行前にget_sheet.Rを実行してください
# ------ function ------
#' @title
#' InputStr
#' @param
#' obj_name : Object name for storing input value
#' str_prompt : String output at the prompt
#' @return
#' No return value
InputStr <- function(obj_name, str_prompt){
  temp <- readline(prompt=str_prompt)
  assign(obj_name, temp, env=.GlobalEnv)
}
#' @title
#' ReadLog
#' @param
#' input_file_path : Full path of csv to read
#' @return
#' String vector
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
#' @title
#' GetLogFullName
#' @param
#' target : target file name
#' file_list : file list
#' @return
#' Full name of target file
GetLogFullName <- function(target, file_list){
  temp_idx <- str_which(file_list, target)
  if (length(temp_idx) > 0){
    return(file_list[temp_idx])
  } else {
    return(NA)
  }
}
#' @title
#' IntToBitVect
#' @param
#' x : Decimal number or string
#' @return
#' Vector of values converted to binary (8bit)
IntToBitVect <- function(x){
  temp <- rev(as.numeric(intToBits(x))[1:8])
  return(temp)
}
#' @title
#' BitVectToInt
#' @param
#' x : Vector of 0 or 1 values
#' @return
#' Integer
BitVectToInt<-function(x) {
  temp <- packBits(rev(c(rep(F, 32 - length(x) %% 32), as.logical(x))), "integer")
  return(temp)
}
#' @title
#' AddUserInfo
#' @param
#' raw_log : log
#' ip_list : private IP lists, whitelists and blacklists
#' @return
#' List of logs
AddUserInfo <- function(raw_log, ip_list){
  output_file <- raw_log %>% str_replace_all(pattern="(?<=[0-9]),(?=[0-9])", replacement="") %>%  # Remove commas for digits
                   gsub(pattern="\"", replacement="", x=., fixed=T) %>%
                     str_replace_all(pattern='^"|"$', replacement="")  # Remove double quotes at the beginning and end of sentence
  partial_ip_list <- ip_list %>% filter(!(str_detect(ip_list$IP, pattern=kIpAddr)))
  for (i in 1:length(output_file)){
    # Determine if an IP address is included
    temp_row <- unlist(strsplit(output_file[i], ",")) %>% str_extract(kIpAddr)
    # Remove duplicate columns
    temp_ip <- temp_row[!is.na(temp_row)] %>% unique
    # If the IP address is included, get the hostname and department
    if (!(identical(temp_ip, character(0)))){
      temp_ip_row <- ip_list %>% filter(IP==temp_ip)
      # Partial match
      if (nrow(temp_ip_row) == 0){
        for (j in 1:nrow(partial_ip_list)){
          if (str_detect(temp_ip, pattern=str_c("^", partial_ip_list[j, "IP"], "\\..*$"))){
            temp_ip_row <- partial_ip_list[j, ]
            break()
          }
        }
      }
      if (nrow(temp_ip_row) == 1){
        output_file[i] <- str_c(output_file[i], "," ,temp_ip_row$Hostname, ",", temp_ip_row$User, "," ,temp_ip_row$Department)
      } else if (nrow(temp_ip_row) > 1){
        # Duplicate host name
        output_file[i] <- str_c(output_file[i], "," ,temp_ip_row[1, "Hostname"], "（ホスト名重複・要確認）")
      }
    }
  }
  return(output_file)
}
#' @title
#' DeleteRows
#' @param
#' target : target vector
#' target_header : Header to delete
#' @return
#' vector
DeleteRows <- function(target, target_header){
  delete_f <- F
  for (i in 1:length(target)){
    if(target[i] %in% target_header){
      delete_f <- T
    } else if (str_trim(target[i], side = "both") == ""){
      if (delete_f){
        target[i] <- NA
      }
      delete_f <- F
    }
    if (delete_f){
      target[i] <- NA
    }
  }
  res <- na.omit(target)
  attr(res, "na.action") <- NULL
  return(res)
}
#' @title
#' OutputWorkbook
#' @param
#' wb : Workbook object(openxlsx)
#' sheetname : Worksheet name
#' df : Data frame to output
#' @return
#' none
OutputWorkbook <- function(wb, sheetname, df, title){
  header_style <- createStyle(fontSize=16)
  addWorksheet(wb, sheetname)
  temp_df <- df
  switch(i,
         # Admin and System Events Report
         "1"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 8), widths = c(18, 30, 25, 35, 22, 52))
           delete_target_header <- c("###Login Summary By Date###", "###Events by Date###")
           temp_df <- DeleteRows(output_list[[i]], delete_target_header)
         },
         # Bandwidth and Applications Report
         "2"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 7), widths = c(25, 20, 15, 20, 80, 40))
           delete_target_header <- c("###Bandwidth Summary###", "###Sessions Summary###", "###Activeユーザー###")
           temp_df <- DeleteRows(output_list[[i]], delete_target_header)
         },
         # Client Reputation
         "3"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6), widths = c(30, 20, 20, 60, 30))
           delete_target_header <- c("###全ユーザー/デバイスのスコアサマリー###",
                                     "###全ユーザー/デバイスのインシデント数###",
                                     "###レピュテーションスコアの大きい上位デバイス###",
                                     "###直近2期間にスコアが増加した上位デバイス###")
           temp_df <- DeleteRows(output_list[[i]], delete_target_header)
         },
         # User Report
         "4"={
           setColWidths(wb, i, cols = c(1, 2, 3, 4, 5), widths = c(90, 20, 20, 80))
         }
  )
  writeData(wb, sheet=sheetname, x=c(title, temp_df), withFilter=F, sep=",")
  pageSetup(wb, sheet=sheetname, orientation="landscape", fitToWidth=T, fitToHeight=F)
  addStyle(wb, sheet=sheetname, header_style, rows=1, cols=1)
}
# ------ Constant definition ------
kTargetLog <- c("Admin and System Events Report without guest",
                "Bandwidth and Applications Report without guest",
                "Client Reputation without guest",
                "User Report without guest")
kIpAddr <- "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
kDhcp_header <- c("IP", "v2", "MAC-Address", "Hostname", "v5", "v6", "v7", "VCI", "v9", "v10", "Expiry")
# ------ Get project path ------
here() %>% setwd()
setwd("..")
#target_yyyymm <- "201906"
if (exists("target_yyyymm")){
  yyyymm <- target_yyyymm
} else{
  last_month <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
  yyyymm <- str_c(format(last_month, "%Y"), format(last_month, "%m"))
}
utm_dir_name <- str_c("UTM Logs ", yyyymm)
parent_path <- str_c(getwd(), "/レポート/", utm_dir_name)
input_path <- str_c(parent_path, "/input")
ext_path <- str_c(parent_path, "/ext")
output_path <- str_c(parent_path, "/output")
if (file.exists(output_path) == F) {
  dir.create(output_path)
}
# ------ Main processing ------
# Read utm log
file_list <- list.files(input_path)
target_file_list <- sapply(kTargetLog, GetLogFullName, file_list)
if (anyNA(target_file_list)) {
  stop(str_c(kTargetLog[i], "をダウンロードして再実行してください"))
  target_file_list <- target_file_list[!is.na(target_file_list)]
}
raw_log_list <- sapply(str_c(input_path, "/", target_file_list), ReadLog)
# Get URL list
address_list <- read.csv(str_c(ext_path, "/sinet.txt"), header=T, as.is=T)
# Get PC information
sinet_table <- filter(address_list, ID == "sinet")$Item %>% read_sheet()
static_ip_table <- filter(address_list, ID == "static_ip")$Item %>% read_sheet()
# Get DHCP list
input_dhcp_login <- filter(address_list, ID == "dhcp")$Item
InputStr("ssh_user", "UTMのユーザー名を入力してください：")
dhcp_login <- str_c(ssh_user, input_dhcp_login)
InputStr("ssh_password", "UTMのパスワードを入力してください：")
session <- ssh_connect(dhcp_login, passwd=ssh_password)
rm(ssh_password)
dhcp_raw <- ssh_exec_internal(session, command = "execute dhcp lease-list")
ssh_disconnect(session)
# Format DHCP list
list_dhcp <- read_lines_raw(dhcp_raw[[2]]) %>%
               lapply(rawToChar) %>%
                 lapply(function(x){str_split_fixed(x, pattern="\t", 11)})
# Remove the space before the IP address
for (i in 1:length(list_dhcp)){
  list_dhcp[[i]][1] <- trimws(list_dhcp[[i]][1])
}
df_dhcp <- unlist(list_dhcp) %>% matrix(nrow=length(list_dhcp), byrow=T) %>% data.frame(stringsAsFactors=F)
colnames(df_dhcp) <- kDhcp_header
# Get owner from hostname
sinet_table <- rename(sinet_table, Hostname="コンピュータ名")
# Check for duplicate hostname
duplicate_hostname <- sinet_table %>%
                        group_by(Hostname) %>%
                          filter(n() > 1) %>%
                            select(Hostname) %>%
                              unique %>%
                                unlist
sinet_table$Duplicate <- ifelse(sinet_table$Hostname %in% duplicate_hostname, T, F)
sinet_table$lower_hostname <- tolower(sinet_table$Hostname)
df_dhcp$lower_hostname <- iconv(df_dhcp$Hostname, "utf-8", "cp932") %>% tolower()
dynamic_ip <- right_join(sinet_table, df_dhcp, by="lower_hostname") %>%
                select(User="使用者名", Department="部署名", Hostname="Hostname.x", "IP", MAC_Address="MAC-Address", "Duplicate")
# Get Static IP list
private_ip <- filter(static_ip_table, !is.na(ホスト名)) %>%
                mutate(MAC_Address="", Duplicate=F) %>%
                  select(User="用途", Department="設置場所", Hostname="ホスト名", IP="IPアドレス", "MAC_Address",
                         "Duplicate") %>%
                    bind_rows(dynamic_ip)
# Get Whitelist and Blacklist
raw_excluded <- read.csv(str_c(ext_path, "/excluded.csv"), as.is=T, na.strings="")
# IP list of network part
excluded <- raw_excluded$IP %>%
              str_split_fixed(pattern="/", n=2) %>%
                data.frame(stringsAsFactors=F) %>%
                  cbind(raw_excluded$Description, stringsAsFactors=F)
colnames(excluded) <- c("IP", "Subnet_mask", "User")
temp_excluded <- excluded %>% filter(Subnet_mask != "")
for (i in 1:nrow(temp_excluded)){
  output_bit_ip <- rep(0, 32)
  # Convert IP address to bit
  bit_ip <- temp_excluded[i, "IP"] %>%
              str_split(pattern="\\." ) %>%
                unlist %>%
                  lapply(IntToBitVect) %>%
                    unlist
  num_subnet_mask <- as.numeric(temp_excluded[i, "Subnet_mask"])
  temp_host <- 8 - (num_subnet_mask %% 8)
  # Get IP address within network part range
  if (temp_host > 0 && temp_host < 8){
    target_octet <- (num_subnet_mask %/% 8) + 1
    temp_end_bit <- target_octet * 8
    temp_start_bit <- temp_end_bit - 7
    temp_bin <- bit_ip[temp_start_bit:num_subnet_mask, drop=F]
    if (sum(temp_bin) == 0){
      temp_min <- BitVectToInt(1)
    } else {
      temp_min <- c(temp_bin, rep(0, temp_host)) %>% BitVectToInt
    }
    temp_max <- c(temp_bin, rep(1, temp_host)) %>% BitVectToInt
    for (j in temp_min:temp_max){
      output_bit_ip[1:temp_end_bit] <- c(bit_ip[1:(temp_start_bit - 1)], IntToBitVect(j))
      if (temp_end_bit < 32) {
        output_bit_ip[(temp_end_bit + 1):32] <- bit_ip[(temp_end_bit + 1):32]
      }
      temp_ip <- str_c(BitVectToInt(output_bit_ip[1:8]), ".",
                         BitVectToInt(output_bit_ip[9:16]), ".",
                         BitVectToInt(output_bit_ip[17:24]), ".",
                         BitVectToInt(output_bit_ip[25:32]))
      excluded <- rbind(excluded, c(temp_ip, "", temp_excluded[i, "User"]))
    }
  }
}
# Delete '.0' in 'x.x.x.0'
excluded$IP <- str_replace_all(excluded$IP, pattern="(\\.0)*\\.0$", replacement="")
# Combine private IP lists with whitelists and blacklists
ip_list <- excluded %>%
             mutate(Department="", Hostname="", MAC_Address="", Duplicate=F) %>%
               select(User, Department, Hostname, IP, MAC_Address, Duplicate) %>%
                 bind_rows(private_ip)
# NA -> ""
ip_list[is.na(ip_list)] <- ""
# Add information such as hostname to the log
output_list <- sapply(raw_log_list, AddUserInfo, ip_list)
# output logs
output_csv_names <- names(output_list) %>% str_extract(pattern="[^\\/]*$")
output_wb <- createWorkbook()
for (i in 1:length(output_list)){
  rownames(output_list[[i]]) <- NULL
  OutputWorkbook(output_wb, i, output_list[[i]], output_csv_names[i])
}
write.csv(df_dhcp, str_c(output_path, "/dhcp.csv"))
sinet_table <- sinet_table %>% select(-"ウィルス対策ソフトのバージョン")
write.table(sinet_table, str_c(output_path, "/sinet_table.csv"), fileEncoding="utf-8")
saveWorkbook(output_wb, str_c(output_path, "/", utm_dir_name, ".xlsx"), overwrite=T)
# Delete all objects
save(output_list, file=str_c(output_path, "/output_list.Rda"))
rm(list = ls())

