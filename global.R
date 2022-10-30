##############################
# global.R
#
# 
# 
##############################

library(shiny)
library(bs4Dash)
library(fresh)
library(bslib)
library(shinyWidgets)
library(thematic)
library(dplyr)
library(shinyalert)
library(RSQLite)
library(DBI)
library(tidyr)
library(lubridate)
library(shinydashboard)
library(chron)
library(data.table)
library(parallel)
library(tidymodels)
library(themis)
library(xgboost)
library(tidyverse)
library(workflows)
library(tune)
library(purrr)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(plotly)
library(polished)
library(config)
library(markdown)
library(maptools)
library(maps)
library(mapdata)
library(moments)
library(shinyBS)

app_config <- config::get()

# configure polished
global_sessions_config(
  app_name = "fisheye",
  api_key = app_config$api_key
)

list.files("modules") %>% purrr::map(~ source(paste0("modules/", .)))
list.files("R") %>% purrr::map(~ source(paste0("R/", .)))
list.files("functions") %>% purrr::map(~ source(paste0("functions/", .)))

