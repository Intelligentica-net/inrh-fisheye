##############################
# func_validate_year_month_of_effort_sim.R
#
# This function validates if year and months of simulation are possible
# given the latest dates of oceano data. Returns True or False
##############################

validate_year_month_effort_simulation <- function(year, months){
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  
  date_sst <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_sst_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  date_biology <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_biology_south_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  date_ekman <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_ekman_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  date_meteo <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_meteo_south_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  date_nao <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_nao_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  date_physique <- DBI::dbGetQuery(conn = conn, statement = "select max(Date) as Date from oceano_physique_south_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>% tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    select(Date) %>% dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  months <- strsplit(x = months, split = "_") %>% unlist() %>% as.numeric() %>% max()
  max_sim_date <- lubridate::date(x = paste(year,months,1,sep = "-"))
  
  DBI::dbDisconnect(conn = conn)
}