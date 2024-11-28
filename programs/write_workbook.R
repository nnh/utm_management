#' title
#' description
#' @file xxx.R
#' @author Mariko Ohtsuka
#' @date YYYY.MM.DD
# ------ libraries ------
library(openxlsx)
# ------ constants ------
kTitleStyle <- createStyle(fontName="Calibri", fontSize=18)
kHeaderStyle <- createStyle(fontName="Calibri", fontSize=16)
kBodyStyle <- createStyle(fontName="Calibri", fontSize=11)
# ------ functions ------
WriteDataToWorkbook <- function(wb, sheetname, table, currentRow) {
  outputRow <- currentRow + 2
  for (i in 1:length(table)) {
    outputDf <- table[[i]]
    names(table)[[i]] %>% writeData(wb, sheet=sheetname, x=., withFilter=F, startRow=outputRow, sep="\t")
    addStyle(wb, sheet=sheetname, kHeaderStyle, rows=outputRow, cols=1)
    outputRow <- outputRow + 1
    outputDf %>% writeData(wb, sheet=sheetname, x=., withFilter=F, startRow=outputRow, sep="\t")
    outputRow <- outputRow + nrow(outputDf) + 2
  }
}
CreateOutputWorkbook <- function(tables) {
  output_wb <- createWorkbook()
  for (i in 1:length(tables)) {
    outputReportName <- names(tables)[i]
    outputTableList <- tables[[i]]
    outputRow <- 1
    addWorksheet(output_wb, i)
    outputReportName %>% writeData(output_wb, sheet=i, x=., withFilter=F, startRow=outputRow, sep="\t")  
    addStyle(output_wb, sheet=i, kBodyStyle, rows=1:110, cols=1:8, gridExpand=T)
    addStyle(output_wb, sheet=i, kTitleStyle, rows=outputRow, cols=1)
    pageSetup(output_wb, sheet=i, orientation="landscape", fitToWidth=T, fitToHeight=F)
    WriteDataToWorkbook(output_wb, i, outputTableList, outputRow)
  }
  setColWidths(output_wb, "1", cols=1:7, widths=c(50, 30, 30, 40, 25, 17, 20))
  setColWidths(output_wb, "2", cols=1:7, widths=c(52, 20, 10, 52, 20, 45, 20))
  setColWidths(output_wb, "3", cols=1:7, widths=c(40, 20, 52, 52, 40, 40, 20))
  for (i in 4:6) {
    setColWidths(output_wb, i, cols=1:7, widths=c(20, 10, 10, 52, 52, 52, 20))
  }
  setColWidths(output_wb, "7", cols=1:11, widths=c(30, 10, 17, 20, 30, 20, 17, 17, 70, 10, 10))
  saveWorkbook(output_wb, str_c(output_path, "/", kOutputFilename, yyyymm, ".xlsx"), overwrite=T)  
}
# ------ main ------
