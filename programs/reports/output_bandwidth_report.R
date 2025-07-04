#' UTM Log Analysis and Monthly Bandwidth Report Generation Script
#' This script analyzes UTM (Unified Threat Management) logs to generate a monthly bandwidth usage report.
#' @file output_bandwidth_report.R
#' @author Mariko Ohtsuka
#' @date 2024.12.6
rm(list = ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(rmarkdown)
library(knitr)
library(ggplot2)
# ------ constants ------
kTotalTitle <- "Total Bytes Transferred"
kUtmLog <- "UTM Logs "
# ------ function ------
#' @title getTargetRows
#' @description Return data frame filtered by condition
#' @param inputCsv Target csv
#' @param targetTitle Target header
#' @param rowCount Number of rows
#' @param columnNames Target column name
#' @return A data frame
getTargetRows <- function(inputCsv, targetTitle, rowCount, columnNames) {
  checkTitle <- map_chr(inputCsv, ~ as.character(str_detect(., targetTitle)))
  titleRow <- which(checkTitle == TRUE)
  targetStartRow <- titleRow + 2
  targetEndRow <- titleRow + rowCount + 1
  temp_target <- inputCsv[targetStartRow:targetEndRow] %>%
    map(function(x) {
      str_split(x, ",")
    }) %>%
    map(function(x) {
      unlist(x)
    }) %>%
    do.call(function(...) rbind(data.frame(), ...), .)
  names(temp_target) <- columnNames
  return(temp_target)
}
#' @title getInputCsv
#' @description Get information on "###Top 30 Applications by Bandwidth and Sessions###"
#' @param targetFilePath Path of the input file
#' @param yyyymm Target year and month
#' @return A data frame
getInputCsv <- function(targetFilePath, yyyymm) {
  inputCsv <- ReadLog(targetFilePath) %>%
    str_replace_all(pattern = "(?<=[0-9]),(?=[0-9])", replacement = "") %>% # Remove commas for digits
    gsub(pattern = "\"", replacement = "", x = ., fixed = TRUE) %>%
    str_replace_all(pattern = '^"|"$', replacement = "") # Remove double quotes at the beginning and end of sentence
  dfTotal <- getTargetRows(
    inputCsv,
    "###Traffic Statistics###",
    8,
    c("ID", "Summary", "Statistics")
  ) %>% filter(Summary == kTotalTitle)
  dfTotal$bandwidth <- map_chr(dfTotal$Statistics, ~ as.character(calcByte(.))) %>%
    as.numeric()
  addTotalRow <- c(kTotalTitle, dfTotal[1, "Statistics"], dfTotal[1, "bandwidth"], yyyymm)
  dfTop30 <- getTargetRows(
    inputCsv,
    "###Top 30 Applications by Bandwidth and Sessions###",
    30,
    c("ID", "application", "バンド幅", "セッション")
  )
  dfTop30$yyyymm <- yyyymm
  dfTop30$bandwidth <- map_chr(dfTop30$バンド幅, ~ as.character(calcByte(.))) %>% as.numeric()
  dfTop30 <- dfTop30 %>%
    select("application", "バンド幅", "bandwidth", "yyyymm") %>%
    rbind(addTotalRow)
  return(dfTop30)
}
#' @title getTargetFileName
#' @description Get the file name of "Bandwidth and Applications Report without guest" for the target year and month.
#' @param targetPath  Path of the input file
#' @return String of the file name
getTargetFileName <- function(targetPath) {
  kBandwidthReport <- "Bandwidth and Applications Report without guest-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{4}"
  res <- list.files(targetPath) %>% str_subset(kBandwidthReport)
  return(res)
}
#' @title calcByte
#' @description  Align the units to megabytes.
#' @param targetStr String of the bandwidth
#' @return String of the result of the unit conversion
calcByte <- function(targetStr) {
  numTarget <- targetStr %>%
    str_replace(pattern = "MB|GB|TB", "") %>%
    as.numeric()
  if (str_detect(targetStr, "MB")) {
    unitValue <- 1
  } else if (str_detect(targetStr, "GB")) {
    unitValue <- 1024^1
  } else if (str_detect(targetStr, "TB")) {
    unitValue <- 1024^2
  }
  temp <- numTarget * unitValue
  return(temp)
}
#' @title ReadLog
#' @param input_file_path : Full path of csv to read
#' @return String vector
ReadLog <- function(input_file_path) {
  os <- .Platform$OS.type # mac or windows
  con <- file(description = input_file_path, open = "rt")
  if (os == "unix") {
    lines <- iconv(readLines(con = con, encoding = "utf-8"), from = "utf-8", to = "utf-8")
  } else {
    lines <- iconv(readLines(con = con, encoding = "utf-8"), from = "utf-8", to = "utf-8")
  }
  close(con = con)
  return(lines)
}
# ------ main ------
source(here("programs", "common", "common.R"), encoding = "UTF-8")
source(here("programs", "data", "get_xml.R"), encoding = "UTF-8")
if (exists("target_yyyymm")) {
  yyyymm <- target_yyyymm
} else {
  # target the previous month of the execution date
  yyyymm <- Sys.Date() %>%
    format("%Y-%m-01") %>%
    as.Date() %>%
    {
      . - 1
    } %>%
    format("%Y%m")
}
yyyy <- str_sub(yyyymm, 1, 4)
mm <- str_sub(yyyymm, 5, 6)
lastyear <- (as.numeric(yyyy) - 1) %>% as.character()
lastyear_mm <- mm %>%
  as.numeric() %>%
  .:12 %>%
  as.character() %>%
  str_pad(2, pad = 0)
thisyear_mm <- mm %>%
  as.numeric() %>%
  1:. %>%
  as.character() %>%
  str_pad(2, pad = 0)
lastyear_yyyymm <- str_c(lastyear, lastyear_mm)
thisyear_yyyymm <- str_c(yyyy, thisyear_mm)
# Covering the same month of the previous year or later
targetYyyymm <- c(lastyear_yyyymm, thisyear_yyyymm)
# file path
targetFolderName <- str_c(kUtmLog, targetYyyymm)
input_parent_path <- home_dir %>% str_remove(str_c(kUtmLog, yyyymm))
targetYyyymmFolderPath <- str_c(input_parent_path, targetFolderName)
input_path <- str_c(targetYyyymmFolderPath, "/input/")
ext_path <- str_c(targetYyyymmFolderPath, "/ext/")
targetFilePath <- map(input_path, getTargetFileName) %>% str_c(input_path, .)
# Get "###Top 30 Applications by Bandwidth and Sessions###"
outputTop30Df <- NULL
for (i in seq_along(targetYyyymm)) {
  if (as.numeric(targetYyyymm[i]) < 202411) {
    temp <- getInputCsv(targetFilePath[i], targetYyyymm[i])
    tempYM <- temp[1, "yyyymm"]
  } else {
    temp <- GetRawDataList(targetFilePath[i])
    temp_total <- temp$`Traffic Statistics`
    total <- tibble()
    total[1, "Application"] <- kTotalTitle
    total[1, "Bandwidth"] <- temp_total %>%
      filter(Summary == kTotalTitle) %>%
      .[, "Statistics", drop = TRUE]
    total[1, "Sessions"] <- NA
    temp_top30 <- temp$`Top 30 Applications by Bandwidth and Sessions`
    bandwidth_col <- GetBandwidthCol(temp_top30)
    if (bandwidth_col == kBandwidthColname$bytes) {
      temp_top30_mod <- temp_top30 %>% rename(!!kBandwidthColname$bandwidth := !!sym(kBandwidthColname$bytes))
    } else {
      temp_top30_mod <- temp_top30
    }
    temp_df <- total %>% bind_rows(temp_top30_mod)
    temp_df$Sessions <- NULL
    temp_df$Bandwidth <- temp_df$Bandwidth %>% str_remove_all(",")
    temp_df$temp_bandwidth <- temp_df$Bandwidth %>% map_chr(~ calcByte(.) %>% as.character(.))
    temp_df$bandwidth <- temp_df$temp_bandwidth %>% as.numeric()
    temp_df$temp_bandwidth <- NULL
    colnames(temp_df) <- c("application", "バンド幅", "bandwidth")
    temp_df$yyyymm <- targetYyyymm[i]
    temp <- temp_df
  }
  outputTop30Df <- rbind(outputTop30Df, temp)
}
# ranking
applications <- unique(outputTop30Df$application)
applicationsRank <- map_int(applications, function(x) {
  temp <- outputTop30Df %>%
    filter(application == x) %>%
    nrow()
  return(temp)
})
applicationList <- data.frame(applications, applicationsRank)
applicationList$applicationsRank <- ifelse(applicationList$applications == kTotalTitle,
  99,
  applicationList$applicationsRank
)
outputDf <- left_join(outputTop30Df, applicationList, by = c("application" = "applications")) %>%
  arrange(desc(applicationsRank), application, yyyymm)
# Create a line chart.
plot_df <- outputDf
plot_total <- kTotalTitle
plot_target <- c("IKE", "SSH", "HTTPS", "HTTP", "udp/19305", "udp/443", "udp/8801", "tcp/8052")
plot_df$bandwidth <- {
  as.numeric(plot_df$bandwidth) / 1024
}
df_plot <- plot_df %>% filter(application %in% c(plot_total, plot_target))
df_other <- plot_df %>% filter(!(application %in% c(plot_total, plot_target)))
df_other <- summarise(group_by(df_other, yyyymm), bandwidth = sum(bandwidth))
df_other$application <- "others"
df_plot <- bind_rows(df_plot, df_other)
lineplot <- ggplot(df_plot, aes(x = yyyymm, y = bandwidth, color = application, group = application)) +
  geom_line() +
  geom_point() +
  labs(title = kTotalTitle, x = "Target date", y = "Bandwidth(GB)")
output_bandwidth <- str_c("bandwidth_", yyyymm, ".html")
render(here("programs", "reports", "output_bandwidth_report.Rmd"),
  output_format = html_document(),
  output_dir = here(),
  output_file = output_bandwidth
)
file.copy(here(output_bandwidth), str_c(volume_str, "/Archives/Log/UTM/"))

file.remove(here(output_bandwidth))
