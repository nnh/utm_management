#' @file generate_md_content.R
#' @author Mariko Ohtsuka
#' @date 2024.11.21
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(googlesheets4)
library(here)
# ------ constants ------
source(here("programs", "common.R"), encoding="UTF-8")
kCsvData <- read_csv(str_c(ext_path, "/param.csv"), col_names=c("itemName", "data"))
#' Read and Assign CSV Value
#' @param csv_data A data frame containing item names and data
ReadAndAssignCsvValue <- function(csv_data) {
  for (i in 1:nrow(csv_data)) {
    variable_name <- as.character(csv_data[i, 1])
    assign(variable_name, as.character(csv_data[i, 2]), envir=.GlobalEnv)
  }
}
ReadAndAssignCsvValue(kCsvData)
kLastName <- str_split(kApprover, "(\\s|　)")[[1]][1]
kFirstName <- str_split(kApprover, "(\\s|　)")[[1]][2]
# ------ functions ------
#' Get Fiscal Year
#' @param date_str A date string in the format "YYYY年M月"
#' @return The fiscal year as an integer
GetFiscalYear <- function(date_str) {
  date_parts <- unlist(strsplit(date_str, "年|月"))
  year <- as.integer(date_parts[1])
  month <- as.integer(date_parts[2])
  if (month >= 4) {
    return(year)
  } else {
    return(year - 1)
  }
}
#' Read Google Sheet
#' @param url The URL of the Google Sheet
#' @param sheet_name The name of the sheet to read
#' @return A data frame with the specified columns
ReadGoogleSheet <- function(url, sheet_name) {
  gs4_auth(
    email = gargle::gargle_oauth_email(),
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL)
  sheet <- url %>% read_sheet(sheet=sheet_name, skip=3, col_names=F, col_types="c") %>% select(c("...1", "...11", "...13"))
  colnames(sheet) <- c("targetYm", "name", "eventDate")
  sheet$fiscalYear <- sheet$targetYm %>% map( ~ { GetFiscalYear(.) })
  nowFiscalYear <- GetFiscalYear(str_c(format(Sys.Date(), "%Y"), "年", format(Sys.Date(), "%m"), "月"))
  targetSheet <- sheet %>% filter(fiscalYear == nowFiscalYear)
  return(targetSheet)
}
#' Get File List In Directory
#' @param folder_path The path to the folder
#' @param file_extension The file extension to filter by
#' @return A vector of year-month values
GetFileListInDirectory <- function(folder_path, file_extension) {
  target_Filename <- str_c(kFileNameHeader, " \\d{6}\\",  file_extension)
  target_files <- list.files(path=folder_path, pattern=target_Filename,
                             full.names=F)
  year_months <- target_files %>% map( ~{gsub(str_c(kFileNameHeader, " (\\d{6})\\", file_extension), "\\1", .)} ) %>% unlist
  return(year_months)
}
#' Create MD Files
#' @param year_month The year-month value
#' @param createName The name of the creator
#' @param review_year The year of review
#' @param review_month The month of review
#' @param review_day The day of review
CreateMdFiles <- function(year_month, createName, review_year, review_month, review_day) {
  dayOfWeek <- ifelse(!is.na(review_month) & !is.na(review_day),
                      GetWeekdayInJapanese(review_year, review_month, review_day),
                      "")
  year <- str_sub(year_month, 1, 4)
  month <- str_sub(year_month, 5, str_length(year_month))
  review_month <- ifelse(!is.na(review_month), review_month, " ")
  review_day <- ifelse(!is.na(review_day), review_day, " ")
  textData <- str_c(
    "様式番号:ISF27-8<br><br>  ",
    "# UTMレポートレビュー記録  ",
    str_c("承認:", kLastName, "<br>"),
    str_c("確認:", kLastName, "<br>"),
    str_c("作成:", createName, "<br>"),
    "## 実施日時",
    str_c(review_year, "年",
          review_month , "月",
          review_day, "日", "（", dayOfWeek, "）13:30〜14:00"),
    "## 参加者",
    str_c(kApprover, "、", kParticipantsList),
    "## 対象期間",
    str_c(year, "年", month, "月"),
    "## 問題点",
    "定型の手順に沿って確認、対応を実施した。特記すべき問題点は存在しなかった。",
    "## 改善点・対応策",
    "特になし。",
    "## 今後の対応課題・対応中の課題",
    "特になし",
    "## 責任者確認",
    str_c(review_year, "年", review_month, "月", review_day, "日　", kLastName),
    sep="\n"
  )
  filename <- str_c(kFileNameHeader, " ", year, month, ".md")
  write_lines(textData, str_c(kMdOutputPath, filename))
}
#' Get Weekday In Japanese
#' @param year The year
#' @param month The month
#' @param day The day
#' @return The weekday in Japanese
GetWeekdayInJapanese <- function(year, month, day) {
  date <- as.Date(paste(year, month, day, sep = "-"))
  weekday_num <- weekdays(date)
  weekday_japanese <- switch(weekday_num,
                             "Sunday" = "日",
                             "Monday" = "月",
                             "Tuesday" = "火",
                             "Wednesday" = "水",
                             "Thursday" = "木",
                             "Friday" = "金",
                             "Saturday" = "土")
  return(weekday_japanese)
}
#' Find Missing Year-Months
#' @param spreadSheet The Google Sheet data frame
#' @param existing_year_months A vector of existing year-month values
#' @return A data frame with missing year-months
FindMissingYearMonths <- function(spreadSheet, existing_year_months) {
  spreadSheet$year <- sub("年.*", "", spreadSheet$targetYm)
  spreadSheet$month <- as.integer(sub(".*?(\\d+)月", "\\1", spreadSheet$targetYm)) %>% sprintf("%02d", .)
  spreadSheet$target_year_months <- str_c(spreadSheet$year, spreadSheet$month)
  for (i in 1:nrow(spreadSheet)) {
    temp <- spreadSheet[i, "eventDate"] %>% str_split("/") %>% unlist()
    spreadSheet[i, "event_month"] <- temp[1]
    spreadSheet[i, "event_day"] <- temp[2]
  }
  if (is.null(existing_year_months)) {
    return(spreadSheet)
  }
  df_existing_year_months <- data.frame(target_year_months=existing_year_months)
  df_target <- spreadSheet %>% anti_join(df_existing_year_months, by="target_year_months")
#  df_target <- df_target %>% filter(!is.na(eventDate))
  return(df_target)
}
# ------ main ------
spreadSheet <- ReadGoogleSheet(kGoogleSheetUrl, kGoogleSheetName)
existing_year_months <- GetFileListInDirectory(kMdOutputPath, ".md")
create_md_year_months <- FindMissingYearMonths(spreadSheet, existing_year_months)
if (nrow(create_md_year_months) > 0) {
  for (i in 1:nrow(create_md_year_months)) {
    CreateMdFiles(create_md_year_months[i, "target_year_months"],
                  kCreatorLastName,
                  create_md_year_months[i, "year"],
                  create_md_year_months[i, "event_month"],
                  create_md_year_months[i, "event_day"])
  }
}
