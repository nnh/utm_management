# Format UTM log
# Mariko Ohtsuka
# 2019/10/2 created
# 2020/3/2 modified
# ------ library ------
library("stringr")
library("dplyr")
library("tidyr")
library("readr")
library("googledrive")
library("googlesheets4")
library("ssh")
library("here")
library("openxlsx")
# google authentication
sheets_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
