#' Extracting Source IP Addresses and Usernames from VPN Connection Logs
#' This program processes VPN connection log files to extract source IP addresses and corresponding usernames.
#' @file get_entry_exit.R
#' @author Mariko Ohtsuka
#' @date 2025.5.1
rm(list = ls())
# ------ libraries ------
library(here)
library(jsonlite)
# ------ constants ------
kMonthAbbr <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
# ------ functions ------
source(here("programs", "common", "common.R"), encoding = "UTF-8")
GetYm <- function(targetDate) {
  year <- targetDate %>% year()
  month <- targetDate %>% month()
  res <- str_c(year, sprintf("%02d", month))
  return(res)
}
GetNextMonth <- function(year, month) {
  current_date <- ymd(paste(year, month, "01", sep = "-"))
  next_month <- current_date %m+% months(1)
  ym <- next_month %>% GetYm()
  return(ym)
}
GetCurrentMonth <- function(year, month) {
  current_date <- ymd(paste(year, month, "01", sep = "-"))
  ym <- current_date %>% GetYm()
  return(ym)
}
GetPreviousMonth <- function(year, month) {
  current_date <- ymd(paste(year, month, "01", sep = "-"))
  previous_month <- current_date %m-% months(1)
  ym <- previous_month %>% GetYm()
  return(ym)
}
GetRoomLog <- function() {
  room_log_path <- str_c(volume_str, "/Archives/Log/DC入退室/rawdata/", yyyymm)
  roomCsv <- room_log_path %>%
    list.files(pattern = ".csv", full.names = TRUE) %>%
    map(~ read.csv(., skip = 3, fileEncoding = "cp932")) %>%
    bind_rows()
  return(roomCsv)
}
GetVpnLog <- function() {
  vpn_log_path <- str_c(volume_str, "/Archives/Log/VPN/rawdata/")
  targetVpnFiles <- vpn_log_path %>%
    list.files(full.names = TRUE) %>%
    keep(~ any(str_detect(.x, targetYm)))
  vpn_files <- targetVpnFiles %>% map_df(~ read_lines(.) %>% tibble(v1 = .))
  return(vpn_files)
}
GetTargetVpnList <- function(vpn_files, kTargetStr) {
  targetVpnRows <- vpn_files %>%
    filter(str_detect(v1, str_c(kTargetStr, collapse = "|")))
  targetVpnList <- targetVpnRows$v1 %>% str_split("\\s+")
  targetVpnList <- targetVpnList %>%
    map(~ {
      res <- .
      month <- res[1] %>%
        unlist() %>%
        match(kMonthAbbr)
      if (month %in% targetMonth[1]) {
        res[1] <- month
        return(res)
      } else {
        return(NULL)
      }
    }) %>%
    compact()
  if (length(targetVpnList) == 0) {
    return(NA)
  }
  temp <- targetVpnList %>%
    map(~ {
      res <- t(.) %>% data.frame()
      return(res)
    }) %>%
    bind_rows() %>%
    arrange(X1, X2)
  return(temp)
}

EditVpnLog <- function(vpn_files, kTargetStr) {
  vpn <- GetTargetVpnList(vpn_files, kTargetStr)
  if (is.atomic(vpn) && length(vpn) == 1 && is.na(vpn)) {
    return(NA)
  }
  for (i in 1:(nrow(vpn) - 1)) {
    if (is.na(vpn[i, "X10"]) && !is.na(vpn[i + 1, "X10"])) {
      vpn[i, "X10"] <- vpn[i + 1, "X10"]
    }
  }
  vpn <- vpn %>%
    filter(X7 == "connected") %>%
    select(c("ip" = "X9", "user" = "X10")) %>%
    distinct()
  return(vpn)
}
GetVpnLocalIp <- function(vpn_files, kTargetStr) {
  vpn <- GetTargetVpnList(vpn_files, kTargetStr)
  if (is.atomic(vpn) && length(vpn) == 1 && is.na(vpn)) {
    return(NA)
  }
  ip <- vpn$X11 %>% str_remove("\\)")
  res <- ip %>% unique()
  return(res)
}
# ------ main ------
currentYm <- GetCurrentMonth(str_sub(yyyymm, 1, 4), str_sub(yyyymm, 5, 6))
previousYm <- GetPreviousMonth(str_sub(yyyymm, 1, 4), str_sub(yyyymm, 5, 6))
nextYm <- GetNextMonth(str_sub(yyyymm, 1, 4), str_sub(yyyymm, 5, 6))
targetYm <- c(currentYm, previousYm, nextYm, "^.*access.log$")
targetMonth <- c(currentYm, previousYm, nextYm) %>%
  str_sub(5, 6) %>%
  as.numeric()
vpn_files <- GetVpnLog()
vpnLocalIp <- c("PPP/IPCP up") %>% GetVpnLocalIp(vpn_files, .)
vpn <- c("connected from", "Call detected from user") %>% EditVpnLog(vpn_files, .)
dummy <- c("vpnLocalIp", "vpn") %>% map(~ TableWriteJson(.))
