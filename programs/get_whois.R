#' @file get_whois.R
#' @author Mariko Ohtsuka
#' @date 2022.5.10
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(curl)
library(Rwhois)
library(openxlsx)
library(readxl)
# ------ constants ------
kPrivateIpRange_1 <- '^172\\..*$'
kPrivateIpRange_2 <- '^192\\.168\\..*$'
kLoopbackIp <- '127.0.0.1'
kPublicIPColNames <- c('domain', 'ip')
kTemplateFolderName <- 'UTM Logs template'
# ------ functions ------
getTargetList <- function(input, targetStr){
  temp <- input %>% map( ~ {
    target <- .
    temp <- target %>% str_extract(targetStr)
    res <- temp[!is.na(temp)]
    return(res)
  }) %>% unlist()
  return(temp)
}
GetWhoisInfoByServer <- function(hostname, server='whois.nic.ad.jp'){
  Sys.sleep(3)
  conn <- make.socket(server, 43)
  write.socket(conn, hostname)
  write.socket(conn, "\r\n")
  data <- ""
  res <- 'test'
  while (res != ''){
    tryCatch(
      res <- read.socket(conn),
      error = function(e){
        print(paste0("Error (WHOIS Server: ", server, "; Hostname Input: ", hostname))
        print(e)
        res <- ''
      }
    )
    if(res != ''){
      data <- paste0(c(data, res), collapse='')
    }
  }
  close.socket(conn)
  res <- iconv(data, from='ISO-2022-JP', to='utf-8')
  return(res)
}
# ------ main ------
source(here("programs", "common.R"), encoding="UTF-8")
whois_csv <- GetWhoisCsv()
# read utm logs
utm_log_raw <- ReadUtmLogs(input_path, kTargetLog)
domain_list <- getTargetList(utm_log_raw, kDomainRegex) %>% unique() %>% str_remove_all('\"')
# Obtaining an IP address from a domain name.
domainAndIp <- domain_list %>% map( ~ {
  ip <- NA
  tryCatch({
    ip <- nslookup(.)
  }, error = function(e){ ip <- NA })
  return (c(., ip))
})
# list -> dataframe
df_domainAndIp <- reduce(domainAndIp, rbind) %>% data.frame()
colnames(df_domainAndIp) <- kPublicIPColNames
df_domainAndIp <- df_domainAndIp %>% filter(!is.na(ip))
# ipv4
ip_address_list <- getTargetList(utm_log_raw, kIpRegex)
public_ip_list <- ip_address_list %>% str_remove(kPrivateIpRange_1) %>% str_remove(kPrivateIpRange_2) %>% str_remove(kLoopbackIp)
public_ip_list <- public_ip_list[-which(public_ip_list == '')] %>% unique()
df_public_ip <- data.frame(NA, public_ip_list)
colnames(df_public_ip) <- kPublicIPColNames
target_public_list <- bind_rows(df_domainAndIp, df_public_ip)
# vpn接続情報と突き合わせ
vpn_access_log <- GetVpnLog()
temp <- left_join(target_public_list, vpn_access_log, by=c('ip'='IP'))
vpn_access <- temp %>% filter(!is.na(User))
external_address <- temp %>% filter(is.na(User))
# ipv6 is excluded from the check.
check_target_address <- external_address %>% filter(!str_detect(ip, ':'))
# sort
wk_sort <- check_target_address$ip %>% str_split('\\.') %>% reduce(rbind) %>% data.frame()
colnames(wk_sort) <- c('oct1', 'oct2', 'oct3', 'oct4')
check_target_address <- check_target_address %>% bind_cols(wk_sort) %>%
  arrange(as.numeric(oct1), as.numeric(oct2), as.numeric(oct3), as.numeric(oct4))
check_target_address <- check_target_address %>% select(-c(oct1, oct2, oct3, oct4))
check_target_address <- check_target_address %>% anti_join(whois_csv, by='ip') %>% anti_join(vpn_access, by='ip')
# search whois
whois_info <- map2(check_target_address[ , 'ip'], check_target_address[ , 'domain'], ~ {
  res <- whois_query(.x)
  return(list(.x, res, .y))
})
res_whois <- whois_info %>% map( ~ {
  kOrgname_1 <- c('org-name|OrgName')
  kOrgname_2 <- c('descr')
  temp <- .
  ip <- temp[[1]]
  key_and_val <- temp[[2]]
  # orgname
  orgname <- key_and_val %>% filter(str_detect(key, kOrgname_1))
  if (nrow(orgname) == 0){
    orgname <- key_and_val %>% filter(str_detect(key, kOrgname_2))
  } else if (nrow(orgname) > 1){
    orgname <- unique(orgname)
  }
  orgname <- orgname$val %>% str_c(collapse=' ')
  # country
  temp_country <- key_and_val %>% filter(str_detect(key, 'Country|country'))
  check_jp <- temp_country %>% filter(str_detect(val, 'jp|JP|Jp'))
  if (nrow(check_jp) > 0){
    country <- 'JP'
  } else {
    country <- temp_country[1, 'val']
  }
  return(list(., orgname, country))
})
output_whois <- res_whois %>% map( ~ {
  c(.[[1]][1], .[2], .[3], .[[1]][3])
}) %>% reduce(rbind) %>% data.frame()
colnames(output_whois) <- c('ip', 'orgname', 'country', 'domain')
whois_not_jp <- output_whois %>% filter(country != 'JP')
whois_jp <- output_whois %>% filter(country == 'JP')
output_whois_jp <- map2(whois_jp$ip, whois_jp$domain, ~ {
  res <- GetWhoisInfoByServer(.x)
  return(list(.x, res, .y))
})
jp_whois_info <- output_whois_jp %>% map( ~ {
  temp <- .
  orgnames <- temp[[2]] %>% str_extract('(?<=\\[組織名\\]).*(?=\n)') %>% str_replace('\\s*', '')
  return(list(temp[[1]], orgnames, 'JP', temp[[3]]))
}) %>% reduce(rbind) %>% data.frame()
colnames(jp_whois_info) <- c('ip', 'orgname', 'country', 'domain')
jp_whois_info <- jp_whois_info %>% filter(!is.na(orgname))
output_jp <- jp_whois_info %>% bind_rows(anti_join(whois_jp, jp_whois_info, by='ip'))
temp <- bind_rows(output_jp, whois_not_jp)
temp_whois <- data.frame(ip=unlist(temp$ip), User=unlist(temp$orgname), domain=unlist(temp$domain))
template_whois <- whois_csv %>% bind_rows(temp_whois)
write_csv(template_whois, file=str_c(input_parent_path, kTemplateFolderName, '/ext/', kWhoisCsvName))
output_whois <- template_whois %>% bind_rows(vpn_access)
write_csv(output_whois, file=str_c(ext_path, '/', kWhoisCsvName))
