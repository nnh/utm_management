# Format UTM log
# Mariko Ohtsuka
# 2019/10/2 created
# 2021/8/17 modified
# ------ library ------
library("tidyverse")
library("googlesheets4")
library("here")
library("openxlsx")
# google authentication
gs4_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
