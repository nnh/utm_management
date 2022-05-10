# main.R実行前にget_sheet.Rを実行してください
# ------ function ------
#' @title InputStr
#' @param obj_name Object name for storing input value
#' @param str_prompt String output at the prompt
#' @return No return value
InputStr <- function(obj_name, str_prompt){
  temp <- readline(prompt=str_prompt)
  assign(obj_name, temp, env=.GlobalEnv)
}
#' @title AddUserInfo
#' @param raw_log log
#' @param ip_list private IP lists, whitelists and blacklists
#' @param whois_csv
#' @return List of logs
AddUserInfo <- function(raw_log, ip_list, whois_csv){
  output_file <- raw_log %>% str_replace_all(pattern="(?<=[0-9]),(?=[0-9])", replacement="") %>%  # Remove commas for digits
                   gsub(pattern="\"", replacement="", x=., fixed=T) %>%
                     str_replace_all(pattern='^"|"$', replacement="")  # Remove double quotes at the beginning and end of sentence
  kIpDomainRegex <- str_c(kIpRegex, str_remove_all(kDomainRegex, '\"'), sep="|")
  for (i in 1:length(output_file)){
    # Determine if an IP address is included
    temp_row <- unlist(strsplit(output_file[i], ",")) %>% str_extract(kIpDomainRegex)
    # Remove duplicate columns
    temp_ip <- temp_row[!is.na(temp_row)] %>% unique
    # If the IP address is included, get the hostname and department
    if (!(identical(temp_ip, character(0)))){
      temp_ip_row <- whois_csv %>% filter(ip==temp_ip)
      if (nrow(temp_ip_row) != 1){
        temp_ip_row <- whois_csv %>% filter(domain==temp_ip)
      }
      if (nrow(temp_ip_row) != 1){
        temp_ip_row <- ip_list %>% filter(IP==temp_ip)
      }
      if (nrow(temp_ip_row) == 1){
        output_file[i] <- str_c(output_file[i], "," ,temp_ip_row$Hostname, ",", temp_ip_row$User, "," ,temp_ip_row$Department, "," , temp_ip_row$MAC_Address)
      } else if (nrow(temp_ip_row) > 1){
        # Duplicate host name
        output_file[i] <- str_c(output_file[i], "," ,temp_ip_row[1, "Hostname"], "（ホスト名重複・要確認）")
      }
    }
  }
  return(output_file)
}
#' @title OutputWorkbook
#' @param wb Workbook object(openxlsx)
#' @param sheetname Worksheet name
#' @param df Data frame to output
#' @return none
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
           temp_df <- getLoginSummaryInfo(temp_df)
         },
         # User Report
         "2"={
           setColWidths(wb, i, cols = c(1, 2, 3, 4, 5), widths = c(90, 20, 20, 80))
         },
         # Bandwidth and Applications Report
         "3"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 7), widths = c(25, 20, 15, 20, 80, 40))
           delete_target_header <- c("###Bandwidth Summary###", "###Sessions Summary###", "###Active Users###", "###Top 30 Users by Bandwidth and Sessions###")
           temp_df <- DeleteRows(output_list[[i]], delete_target_header)
         },
         # Client Reputation
         "4"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6), widths = c(30, 20, 20, 60, 30))
           delete_target_header <- c("###全ユーザー/デバイスのスコアサマリー###",
                                     "###全ユーザー/デバイスのインシデント数###",
                                     "###レピュテーションスコアの大きい上位デバイス###",
                                     "###直近2期間にスコアが増加した上位デバイス###")
           temp_df <- DeleteRows(output_list[[i]], delete_target_header)
         },
         # List of terminals connected to the DataCenter
         "5"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 7), widths = c(25, 20, 15, 20, 80, 40))
           temp_df <- output_list[[i]]
         },
         # List of terminals connected to the vpn
         "6"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 7), widths = c(25, 20, 15, 20, 80, 40))
           temp_df <- output_list[[i]]
         },
         # List of terminals connected to the nmccrc
         "7"={
           setColWidths(wb, i, cols = c(2, 3, 4, 5, 6, 7), widths = c(25, 20, 15, 20, 80, 40))
           temp_df <- output_list[[i]]
         }
  )
  writeData(wb, sheet=sheetname, x=title, withFilter=F, sep="\t")
  output_list <- EditWriteData(temp_df)
  for (i in 1:length(output_list)){
    output_row <- i + 1
    writeData(wb, sheet=sheetname, x=output_list[[i]], withFilter=F, sep="\t", startRow=output_row, colNames=F)
  }
  pageSetup(wb, sheet=sheetname, orientation="landscape", fitToWidth=T, fitToHeight=F)
  addStyle(wb, sheet=sheetname, header_style, rows=1, cols=1)
}
#' @title EditWriteData
#' @description Get the last column of the data frame that contains data. Delete the columns after the acquired column.
#' Perform processing for each row of the input data frame.
#' @param input_df a data frame
#' @return a list
EditWriteData <- function(input_df){
  output_list <- NULL
  for (i in 1:nrow(input_df)){
    temp_row <- input_df[i, ]
    for (j in ncol(temp_row):1){
      if (str_trim(temp_row[1, j] == "")){
        temp_row <- temp_row[-j]
      } else {
        output_list[[i]] <- temp_row
        break
      }
    }
  }
  return(output_list)
}
#' @title DeleteRows
#' @description Remove lines not to be printed
#' @param input_df a data frame
#' @param condition_str Heading of lines not to output
#' @return a data frame
DeleteRows <- function(input_df, condition_str){
  output_df <- input_df
  delete_f <- F
  for(i in 1:nrow(output_df)){
    if (output_df[i, 1] %in% condition_str){
      delete_f <- T
    } else if (str_trim(output_df[i, 1]) == ""){
      delete_f <- F
    }
    if (delete_f){
      output_df[i, 1] <- kDelStr
    }
  }
  output_df <- output_df %>% filter(col1 != kDelStr)
  return(output_df)
}
#' @title convertFromCharToDf
#' @description Convert a comma-separated vector to a data frame
#' @param input_data : a vector
#' @return a data frame
convertFromCharToDf <- function(input_data){
  comma_count <- str_extract_all(input_data, ",") %>% map(length)
  max_comma_count <- comma_count %>% unlist() %>% max()
  add_comma_count <- map(comma_count, function(x){ max_comma_count - x })
  add_comma <- map(add_comma_count, function(x){ rep(",", x) %>% str_c(collapse = "") })
  combine_comma <- str_c(input_data, add_comma)
  column_names <- str_c("col", 1:(max_comma_count+1))
  output_df <- combine_comma %>% as_tibble() %>% separate("value", column_names, sep=",")
  return(output_df)
}
#' @title getLoginSummaryInfo
#' @param input_df a data frame
#' @return a data frame
getLoginSummaryInfo <- function(input_df){
  temp_df <- input_df
  check_f <- F
  for (i in 1:nrow(temp_df)){
    output_str <- NULL
    if (check_f & temp_df[i, 1] == ""){
      break
    }
    if (check_f){
      # Check User
      if (!(temp_df[i, 2] %in% fortigate_users)){
        output_str <- c(output_str, "未登録ユーザー")
      }
      # Check login Interface
      temp_ip <- str_extract(temp_df[i, 3], "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+")
      if (!is.na(temp_ip)){
        if (!(temp_ip %in% maintenanceIpInfo)){
          output_str <- c(output_str, "範囲外IPアドレス")
        }
      }
      if (is.null(output_str)){
        output_str <- "OK"
      } else {
        output_str <- str_c(output_str, collapse=",")
      }
      temp_df[i, ncol(temp_df) - 1] <- output_str
    }
    if (temp_df[i, 2] == "User Name"){
      check_f <- T
    }
  }
  return(temp_df)
}
# ------ Constant definition ------
kOutputSummary <- "checklist"
kIpAddr <- "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
kDhcp_header_mac <- c("IP", "v2", "MAC-Address", "Hostname", "v5", "v6", "v7", "VCI", "v9", "v10", "Expiry")
kDhcp_header_win <- c("IP", "MAC-Address", "Hostname", "VCI", "Expiry", "v6", "v7", "v8", "v9", "v10")
kDelStr <- "*delete*"
kOutputFilename <- "不正アクセスチェックレポート "
# ------ Main processing ------
# Remove the space before the IP address
for (i in 1:nrow(list_dhcp)){
  list_dhcp[i, 1] <- str_trim(list_dhcp[i, 1])
}
df_dhcp <- list_dhcp
if (ncol(df_dhcp) == 11){
  colnames(df_dhcp) <- kDhcp_header_mac
} else {
  colnames(df_dhcp) <- kDhcp_header_win
}
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
                select(User="使用者名", Department="部署名", Hostname="Hostname.x", "IP", MAC_Address="MAC-Address", "Duplicate", Hostname_y="Hostname.y")
# If the terminal is SINET unregistered, output the hostname in the DHCP log.
for (i in 1:nrow(dynamic_ip)){
  if (is.na(dynamic_ip[i, "Hostname"])){
    dynamic_ip[i, "Hostname"] <- ifelse(dynamic_ip[i, "Hostname_y"] != "", dynamic_ip[i, "Hostname_y"], "! 端末名不明")
    dynamic_ip[i, "User"] <- "! SINET未登録端末"
  }
}
dynamic_ip <- dynamic_ip %>% select(-Hostname_y)
# Get Static IP list
private_ip <- filter(static_ip_table, !is.na(ホスト名) & ホスト名 != 'DHCP Start' & ホスト名 != 'DHCP End') %>%
                mutate(MAC_Address="", Duplicate=F) %>%
                  select(User="用途", Department="設置場所", Hostname="ホスト名", IP="IPアドレス", "MAC_Address",
                         "Duplicate") %>%
                    bind_rows(dynamic_ip)
# For identification of unregistered terminals.
all_terminal <- private_ip
no_hostname <- all_terminal %>% filter(!str_detect(IP, '192\\.168\\.1\\..*')) %>% filter(str_detect(IP, '^[192|172].*$')) %>% filter(is.na(Hostname)) %>% select(MAC_Address)
df_dhcp_excluded_guest <- df_dhcp %>% filter(!str_detect(IP, '192\\.168\\.1\\..*'))
unregistered_list <- left_join(no_hostname, df_dhcp_excluded_guest, by=c("MAC_Address"="MAC-Address")) %>% filter(Hostname != '') %>% distinct(`MAC_Address`, .keep_all=T)
excluded <- blacklist
# Delete '.0' in 'x.x.x.0'
excluded$IP <- str_replace_all(excluded$IP, pattern="(\\.0)*\\.0$", replacement="")
# Combine private IP lists with whitelists and blacklists
ip_list <- excluded %>%
             mutate(Department="", Hostname="", MAC_Address="", Duplicate=F) %>%
               select(User, Department, Hostname, IP, MAC_Address, Duplicate) %>%
                 bind_rows(private_ip)
# Conbine vpn access log
ip_list <- rbind(vpn_access_log, ip_list)
ip_list[is.na(ip_list)] <- ""
# Add information such as hostname to the log
temp_output_list <- sapply(raw_log_list, AddUserInfo, ip_list, whois_csv)
# output logs
output_csv_names <- names(temp_output_list) %>% str_extract(pattern="[^\\/]*$")
# convert from vector to dataframe
output_wb <- createWorkbook()
output_list <- NULL
for (i in 1:length(temp_output_list)){
  output_list[[i]] <- convertFromCharToDf(temp_output_list[[i]])
  rownames(output_list[[i]]) <- NULL
  OutputWorkbook(output_wb, i, output_list[[i]], output_csv_names[i])
}
write.csv(df_dhcp, str_c(output_path, "/dhcp.csv"))
sinet_table <- sinet_table %>% select(-"ウィルス対策ソフトのバージョン")
write.table(sinet_table, str_c(output_path, "/sinet_table.csv"), fileEncoding="utf-8")
saveWorkbook(output_wb, str_c(output_path, "/", kOutputFilename, yyyymm, ".xlsx"), overwrite=T)
# Delete all objects
save(output_list, file=str_c(output_path, "/output_list.Rda"))
# Output checklist workbook
checklist_wb <- createWorkbook()
addWorksheet(checklist_wb, kOutputSummary)
writeData(checklist_wb, sheet=kOutputSummary, x=checklist, colNames=F, rowNames=F)
addStyle(checklist_wb, sheet=kOutputSummary, style=createStyle(wrapText=T), cols=4, rows=1:nrow(checklist))
setColWidths(checklist_wb, sheet=kOutputSummary, cols=c(1:5), widths = c(10, 75, 50, 80, 10))
pageSetup(checklist_wb, sheet=kOutputSummary, orientation="landscape", fitToWidth=T, fitToHeight=F)
saveWorkbook(checklist_wb, str_c(output_path, "/checklist.xlsx"), overwrite=T)
# Output list of unregistered terminals.
write.csv(unregistered_list, str_c(output_path, "/unregistered.csv"))
rm(list = ls())
# Output bandwidth report
source(here("programs", "output_bandwidth_report.R"), encoding="UTF-8")
rm(list = ls())
