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
kUserReport <- "User Report without guest"
kTrafficSummary <- "Traffic Summary"
kTargetFiles <- c("Admin and System Events Report", 
                  "Bandwidth and Applications Report without guest", 
                  "Client Reputation without guest", 
                  "List of terminals connected to DataCenter", 
                  "List of terminals connected to nmccrc", 
                  "List of terminals connected vpn", 
                  "User Report without guest") 
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
# ------ main ------
home_dir <- GetHomeDir()
input_path <- home_dir %>% file.path("Downloads")
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

fileNameAndTables <- inputFiles %>% map( ~ {
  filePath <- .
  xml_data <- filePath %>% read_xml()
  fileName <- filePath %>% GetBaseName()
  if (fileName == kUserReport) {
    temp <- xml_data %>% xml_find_all(".//Top_5_Users_by_Bandwidth")
    res <- temp %>% map( ~ GetReportTables(.))
  } else {
    res <- xml_data %>% GetReportTables()
  }
  return(list(fileName, res))
})
fileNames <- fileNameAndTables %>% map( ~ .[1]) %>% list_c()
#tables <- fileNameAndTables %>% map( ~ .[2]) %>% list_c()
#names(tables) <- fileNames
