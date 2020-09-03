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
