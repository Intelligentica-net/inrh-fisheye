##############################
# func_join_oceano_for_negro.R
#
# 
# 
##############################

add_oceano_negro <- function(data){
  
  data_oceano <- data %>%
    rename(DATE = Date) %>%
    dplyr::mutate(DateKey1 = grade_date(x = (DATE-1*7),dx = 8)) %>%
    dplyr::mutate(DateKey2 = grade_date(x = (DATE-2*7),dx = 8)) %>%
    dplyr::mutate(DateKey3 = grade_date(x = (DATE-3*7),dx = 8)) %>%
    dplyr::mutate(DateKey4 = grade_date(x = (DATE-4*7),dx = 8)) %>%
    dplyr::mutate(DateKey5 = grade_date(x = (DATE-5*7),dx = 8)) %>%
    dplyr::rename(Date = DATE, Depth = DEPTH)
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  
  ### Add SST data
  sst_data <- DBI::dbGetQuery(conn = conn,statement = 'select *  from oceano_sst_table') %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_oceano)
  lat <- data_oceano$Latitude
  
  f <- function(i){
    if(is.null(sst_data[[dat[i]]])){
      return(NA)
    }else{
      ix <- which.min(abs(sst_data[[dat[i]]]$Latitude - lat[i]))
      sst_data[[dat[i]]]$indice_upw_sst[ix]
    }
  }
  
  dat <- as.character(data_oceano$DateKey1)
  data_oceano$SST1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### Add NAO
  nao_data <- dbGetQuery(conn = conn,statement = "select * from oceano_nao_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(NAO_mean = median(NAO_mean),
              NAO_std = median(NAO_std))%>%
    ungroup()
  
  data_oceano <- data_oceano %>%
    dplyr::left_join(nao_data, by = c("DateKey1" = "Date"),suffix = c("","1"))
  
  data_oceano <- data_oceano %>%
    dplyr::left_join(nao_data, by = c("DateKey2" = "Date"),suffix = c("","2"))
  
  data_oceano <- data_oceano %>%
    dplyr::left_join(nao_data, by = c("DateKey3" = "Date"),suffix = c("","3"))
  
  data_oceano <- data_oceano %>%
    dplyr::left_join(nao_data, by = c("DateKey4" = "Date"),suffix = c("","4"))
  
  data_oceano <- data_oceano %>%
    dplyr::left_join(nao_data, by = c("DateKey5" = "Date"),suffix = c("","5"))
  
  data_oceano <- data_oceano %>%
    dplyr::rename(NAO_mean1 = NAO_mean, NAO_std1 = NAO_std) %>%
    dplyr::select(-NAO_std2, -NAO_std3, -NAO_std4, -NAO_std5)
  
  ### Adding Biology
  
  ### Adding Biology data
  bio_data <- dbGetQuery(conn = conn,statement = "select * from oceano_biology_med_table") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  ## O2_mean
  
  N <- nrow(data_oceano)
  lat <- data_oceano$Latitude
  lon <- data_oceano$Longitude
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$O2_mean[io]
    }
  }
  
  dat <- as.character(data_oceano$DateKey1)
  data_oceano$O2_mean1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### PP_mean
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$PP_mean[io]
    }
  }
  
  dat <- as.character(data_oceano$DateKey1)
  data_oceano$PP_mean1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- as.character(data_oceano$DateKey3)
  data_oceano$PP_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- as.character(data_oceano$DateKey5)
  data_oceano$PP_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  data_oceano_final <- data_oceano %>%
    dplyr::select(-DateKey1,-DateKey2,-DateKey3,-DateKey4,-DateKey5)
  
  DBI::dbDisconnect(conn)
  
  return(data_oceano_final)
}