#' Process XML Data for Report Generation
#' This script reads and processes XML files to generate reports based on predefined structures. 
#' It includes functions to validate the presence of required input files, extract relevant data from XML tables, 
#' and compile them into structured lists for further processing. 
#' @file get_xml.R
#' @author Mariko Ohtsuka
#' @date 2024.11.29
# ------ libraries ------
# ------ constants ------
# ------ functions ------
GetInputTables <- function(input_path) {
  inputFiles <- input_path %>% GetTargetFilePath()
  fileNames <- inputFiles %>% map_chr( ~ GetBaseName(.))
  if (!identical(sort(fileNames), sort(kTargetFiles))) {
    stop("入力ファイルが不足しています")
  }
  tables <- inputFiles %>% map( ~ GetRawDataList(.))
  names(tables) <- fileNames
  return(tables)  
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
GetReportTables <- function(xml_data) {
  tables <- xml_data %>% GetTagAndNames("table")
  res <- tables %>% map( ~ ExtractIdData(.))
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
    temp_t <- tag_values %>% t()
    temp_col <- 1:ncol(temp_t) %>% str_c("tempCol_", .)
    colnames(temp_t) <- temp_col
    df <- temp_t %>% as_tibble() %>% setNames(tag_names)
    return(df)
  })
  return(data_list)
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
GetTagAndNames <- function(xml_data, tagName) {
  target <- xml_data %>% xml_find_all(str_c(".//", tagName))
  target_names <- target %>% xml_attr("name")
  names(target) <- target_names
  return(target)  
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
# ------ main ------