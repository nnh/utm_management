#' @file set_blacklist.R
#' @author Mariko Ohtsuka
#' @date 2022.1.6
rm(list=ls())
# ------ libraries ------
library(tidyverse)
library(here)
library(googlesheets4)
# ------ constants ------
kConfigEditHead <- '(?<=edit\\s"'
kConfigEditFoot <- '"\\n)[\\s]+set\\suuid.*\\n[\\s]+set\\s'
kConfigNext <- '(?=\\n[\\s]+next)'
kConfigRemoveHead <- '^[\\s]+set\\suuid\\s\\S+\\s+set\\s'
# ------ main ------
source(here("programs", "common.R"), encoding="UTF-8")
address_list <- read.csv(str_c(ext_path, "/sinet.txt"), header=T, as.is=T)
raw_config <- list.files(ext_path) %>% str_extract('[A-Z]{6}[0-9]{2}_[0-9]{8}_[0-9]{4}\\.conf') %>% na.omit()
str_extract_blackAddressList <- str_c(kConfigEditHead, 'BlackList', kConfigEditFoot, 'member.*', kConfigNext)
str_remove_blackAddressList <- str_c(kConfigRemoveHead, 'member\\s')
black_addresslist <- raw_config %>% str_extract(str_extract_blackAddressList) %>%
  str_remove(str_remove_blackAddressList) %>% str_remove_all('\"') %>% str_split('\\s') %>% .[[1]]
black_ip_range <- raw_config %>% str_extract('(?<=config\\sfirewall\\saddress\\n)[\\s|\\S]+(?=end\\nconfig\\sfirewall\\smulticast-address)') %>% str_split('next\n')
temp <- black_ip_range[[1]] %>% str_extract_all('^.*Black[\\s|\\S]*', simplify=T)
temp <- temp[which(nchar(temp) > 0)]
Description <- temp %>% str_extract('Black[0-9|-].*(?=\\")')
IP <- temp %>% str_extract('(?<=set\\ssubnet\\s)[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+')
# Delete addresses that are not registered in the group.
black_address <- tibble(IP, Description) %>% filter(Description %in% black_addresslist)
if (exists('black_address')){
  # google authentication
  gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL)
  # Get URL list
  range_write(ss=filter(address_list, ID == "excluded")$Item, data=black_address, sheet='blacklist', range='A1', col_names=T)
  gs4_deauth()
} else {
  print('configを所定の場所に格納して再実行してください')
}
