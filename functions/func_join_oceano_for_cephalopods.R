##############################
# func_join_oceano_for_cephalopod.R
#
# 
# 
##############################

add_oceano_to_cephalopod <- function(data){
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  
  data_oceano <- data %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    dplyr::mutate(DateKey0 = grade_date(x = (DATE-0*30),dx = 8)) %>%
    dplyr::mutate(DateKey1 = grade_date(x = (DATE-1*30),dx = 8)) %>%
    dplyr::mutate(DateKey2 = grade_date(x = (DATE-2*30),dx = 8)) %>%
    dplyr::mutate(DateKey3 = grade_date(x = (DATE-3*30),dx = 8)) %>%
    dplyr::mutate(DateKey4 = grade_date(x = (DATE-4*30),dx = 8)) %>%
    dplyr::mutate(DateKey5 = grade_date(x = (DATE-5*30),dx = 8))
  
  ### Add SST data
  sst_data <- DBI::dbGetQuery(conn = conn,statement = 'select *  from oceano_sst_table where Latitude between 20 and 27.5') %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  
  f <- function(i){
    if(is.null(sst_data[[dat[i]]])){
      return(NA)
    }else{
      ix <- which.min(abs(sst_data[[dat[i]]]$Latitude - lat[i]))
      sst_data[[dat[i]]]$indice_upw_sst[ix]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$SST2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### Add NAO
  nao_data <- dbGetQuery(conn = conn,statement = "select * from oceano_nao_table") %>%
    mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    mutate(Date = grade_date(x = Date,dx = 8)) %>%
    group_by(Date) %>%
    summarise(NAO_mean = median(NAO_mean),
              NAO_std = median(NAO_std))%>%
    ungroup()
  
  data_oceano <- data_oceano %>%
    left_join(nao_data, by = c("DateKey0" = "Date"))
  
  data_oceano <- data_oceano %>%
    left_join(nao_data, by = c("DateKey4" = "Date"),suffix = c("","4"))
  
  data_oceano <- data_oceano %>%
    left_join(nao_data, by = c("DateKey5" = "Date"),suffix = c("","5"))
  
  data_oceano <- data_oceano %>%
    select(-NAO_mean,-NAO_std,-NAO_mean4,-NAO_mean5)
  
  #### Add Ekman
  
  ekman_data <- dbGetQuery(conn = conn,statement = "select * from oceano_ekman_table") %>%
    mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  
  f <- function(i){
    if(is.null(ekman_data[[dat[i]]])){
      return(NA)
    }else{
      ix <- which.min(abs(ekman_data[[dat[i]]]$Latitude - lat[i]))
      ekman_data[[dat[i]]]$Indice_upw_Ekman[ix]
    }
  }
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$EKMAN5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### Adding Biology data
  bio_data <- dbGetQuery(conn = conn,statement = "select * from oceano_biology_south_table where Latitude between 20 and 27.5") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  ## O2_mean
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$O2_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey1 %>% as.character()
  data_oceano$O2_mean1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$O2_mean2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$O2_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### o2_std
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$O2_std[io]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$O2_std2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey4 %>% as.character()
  data_oceano$O2_std4 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$O2_std5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### PP_mean
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$PP_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey1 %>% as.character()
  data_oceano$PP_mean1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$PP_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey4 %>% as.character()
  data_oceano$PP_mean4 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$PP_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## PP_std
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$PP_std[io]
    }
  }
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$PP_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### Adding Physical
  
  phys_data <- dbGetQuery(conn = conn,statement = "select * from oceano_physique_south_table where Latitude between 20 and 27.5") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  ## Temp_mean
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$TEMP_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey0 %>% as.character()
  data_oceano$TEMP_mean0 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey1 %>% as.character()
  data_oceano$TEMP_mean1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$TEMP_mean2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$TEMP_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey4 %>% as.character()
  data_oceano$TEMP_mean4 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$TEMP_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()

  ### TEMP_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$TEMP_std[io]
    }
  }
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$TEMP_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$TEMP_std5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### SAL_mean
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$SAL_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$SAL_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$SAL_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### sal_std
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$SAL_std[io]
    }
  }
  
  dat <- data_oceano$DateKey4 %>% as.character()
  data_oceano$SAL_std4 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### U_mean
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$U_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$U_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$U_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### U_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$U_std[io]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$U_std2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### V_mean
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$V_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$V_mean2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$V_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$V_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### V_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$V_std[io]
    }
  }
  
  dat <- data_oceano$DateKey1 %>% as.character()
  data_oceano$V_std1 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$V_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$V_std5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  #### Adding Meteo Indicators ###################################################
  
  meteo_data <- dbGetQuery(conn = conn,statement = "select * from oceano_meteo_south_table where Latitude between 20 and 27.5") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_oceano)
  lat <- data_oceano$LAT
  lon <- data_oceano$LON
  dat <- data_oceano$DateKey0
  
  ## Pres_mean
  ## Pres_mean
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Pres_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$Pres_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoU_mean
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$U_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$MeteoU_mean2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$MeteoU_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoV_mean
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$V_mean[io]
    }
  }
  
  dat <- data_oceano$DateKey2 %>% as.character()
  data_oceano$MeteoV_mean2 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_oceano$DateKey5 %>% as.character()
  data_oceano$MeteoV_mean5 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoV_std
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$V_std[io]
    }
  }
  
  dat <- data_oceano$DateKey3 %>% as.character()
  data_oceano$MeteoV_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  data_oceano <- data_oceano %>%
    dplyr::select(-DateKey0,-DateKey1,-DateKey2,-DateKey3,-DateKey4,-DateKey5)
  
  DBI::dbDisconnect(conn)
  
  return(data_oceano)
}