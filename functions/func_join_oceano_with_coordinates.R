##############################
# func_join_oceano_with_coordinates.R
#
# 
# 
##############################

add_oceano_to_coordinates <- function(coordinates){
  
  data_10nm_weekly <- coordinates %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    dplyr::mutate(DateKey0 = grade_date(x = (DATE-0*30),dx = 8)) %>%
    dplyr::mutate(DateKey3 = grade_date(x = (DATE-3*30),dx = 8)) %>%
    dplyr::mutate(DateKey6 = grade_date(x = (DATE-6*30),dx = 8)) %>%
    dplyr::mutate(DateKey9 = grade_date(x = (DATE-9*30),dx = 8)) %>%
    dplyr::mutate(DateKey12 = grade_date(x = (DATE-12*30),dx = 8)) %>%
    dplyr::mutate(DateKey15 = grade_date(x = (DATE-15*30),dx = 8)) %>%
    dplyr::mutate(DateKey18 = grade_date(x = (DATE-18*30),dx = 8)) %>%
    dplyr::mutate(DateKey21 = grade_date(x = (DATE-21*30),dx = 8)) %>%
    dplyr::mutate(DateKey24 = grade_date(x = (DATE-24*30),dx = 8)) %>%
    dplyr::mutate(DateKey27 = grade_date(x = (DATE-27*30),dx = 8)) %>%
    dplyr::mutate(DateKey30 = grade_date(x = (DATE-30*30),dx = 8)) %>%
    dplyr::mutate(DateKey33 = grade_date(x = (DATE-33*30),dx = 8)) %>%
    dplyr::mutate(DateKey36 = grade_date(x = (DATE-36*30),dx = 8)) %>%
    dplyr::mutate(DateKey39 = grade_date(x = (DATE-39*30),dx = 8)) %>%
    dplyr::mutate(DateKey42 = grade_date(x = (DATE-42*30),dx = 8)) %>%
    dplyr::mutate(DateKey45 = grade_date(x = (DATE-45*30),dx = 8)) %>%
    dplyr::mutate(DateKey48 = grade_date(x = (DATE-48*30),dx = 8))
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  
  ### Add SST data
  sst_data <- DBI::dbGetQuery(conn = conn,statement = 'select *  from oceano_sst_table where Latitude between 20 and 27.5') %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  
  f <- function(i){
    if(is.null(sst_data[[dat[i]]])){
      return(NA)
    }else{
      ix <- which.min(abs(sst_data[[dat[i]]]$Latitude - lat[i]))
      sst_data[[dat[i]]]$indice_upw_sst[ix]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$SST3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$SST9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey12 %>% as.character()
  data_10nm_weekly$SST12 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey15 %>% as.character()
  data_10nm_weekly$SST15 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey21 %>% as.character()
  data_10nm_weekly$SST21 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$SST42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$SST48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
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
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey0" = "Date"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey6" = "Date"),suffix = c("","6"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey12" = "Date"),suffix = c("","15"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey15" = "Date"),suffix = c("","18"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey27" = "Date"),suffix = c("","30"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    left_join(nao_data, by = c("DateKey45" = "Date"),suffix = c("","45"))
  
  data_10nm_weekly <- data_10nm_weekly %>%
    dplyr::select(-NAO_mean,-NAO_std,-NAO_std6,-NAO_std15,-NAO_std18,-NAO_std30,-NAO_std45)
  
  #### Add Ekman
  
  ekman_data <- dbGetQuery(conn = conn,statement = "select * from oceano_ekman_table") %>%
    mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  
  f <- function(i){
    if(is.null(ekman_data[[dat[i]]])){
      return(NA)
    }else{
      ix <- which.min(abs(ekman_data[[dat[i]]]$Latitude - lat[i]))
      ekman_data[[dat[i]]]$Indice_upw_Ekman[ix]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$EKMAN3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey18 %>% as.character()
  data_10nm_weekly$EKMAN18 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### Adding Biology data
  bio_data <- dbGetQuery(conn = conn,statement = "select * from oceano_biology_south_table where Latitude between 20 and 27.5") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::filter(Latitude>18,Latitude<28.5) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  ## O2_mean
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  lon <- data_10nm_weekly$LON
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$O2_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$O2_mean9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey24 %>% as.character()
  data_10nm_weekly$O2_mean24 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$O2_mean33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey36 %>% as.character()
  data_10nm_weekly$O2_mean36 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$O2_mean48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### o2_std
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$O2_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey18 %>% as.character()
  data_10nm_weekly$O2_std18 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$O2_std42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  
  ### PP_mean
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  lon <- data_10nm_weekly$LON
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$PP_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$PP_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$PP_mean6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$PP_mean9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$PP_mean33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## PP_std
  
  f <- function(i){
    if(is.null(bio_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(bio_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      bio_data[[dat[i]]]$PP_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$PP_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character()
  data_10nm_weekly$PP_std27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()

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
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  lon <- data_10nm_weekly$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$TEMP_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$TEMP_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$TEMP_mean6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$TEMP_mean9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey36 %>% as.character()
  data_10nm_weekly$TEMP_mean36 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$TEMP_mean39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### TEMP_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$TEMP_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character.Date()
  data_10nm_weekly$TEMP_std3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character.Date()
  data_10nm_weekly$TEMP_std9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character.Date()
  data_10nm_weekly$TEMP_std27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### SAL_mean
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  lon <- data_10nm_weekly$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$SAL_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$SAL_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$SAL_mean6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey12 %>% as.character()
  data_10nm_weekly$SAL_mean12 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey15 %>% as.character()
  data_10nm_weekly$SAL_mean15 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey24 %>% as.character()
  data_10nm_weekly$SAL_mean24 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey30 %>% as.character()
  data_10nm_weekly$SAL_mean30 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$SAL_mean33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$SAL_mean42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey45 %>% as.character()
  data_10nm_weekly$SAL_mean45 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$SAL_mean48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### sal_std
  
  N <- nrow(data_10nm_weekly)
  lat <- data_10nm_weekly$LAT
  lon <- data_10nm_weekly$LON
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$SAL_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$SAL_std9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character()
  data_10nm_weekly$SAL_std27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$SAL_std33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey36 %>% as.character()
  data_10nm_weekly$SAL_std36 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$SAL_std39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey45 %>% as.character()
  data_10nm_weekly$SAL_std45 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### U_mean
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$U_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$U_mean6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey18 %>% as.character()
  data_10nm_weekly$U_mean18 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey45 %>% as.character()
  data_10nm_weekly$U_mean45 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### U_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$U_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$U_std6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey15 %>% as.character()
  data_10nm_weekly$U_std15 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character()
  data_10nm_weekly$U_std27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$U_std33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### V_mean
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$V_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$V_mean39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$V_mean48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ### V_std
  
  f <- function(i){
    if(is.null(phys_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(phys_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      phys_data[[dat[i]]]$V_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey18 %>% as.character()
  data_10nm_weekly$V_std18 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey24 %>% as.character()
  data_10nm_weekly$V_std24 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$V_std42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  #### Adding Meteo Indicators ###################################################
  
  meteo_data <- dbGetQuery(conn = conn,statement = "select * from oceano_meteo_south_table where Latitude between 20 and 27.5") %>%
    dplyr::mutate(Date = fromChronToDate(Date)) %>%
    tidyr::separate(col = Date,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    dplyr::select(-Time) %>%
    dplyr::mutate(Date = grade_date(x = Date,dx = 8)) %>%
    data.table::as.data.table() %>%
    split(by = "Date")
  
  ## MeteoU_mean
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$U_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey12 %>% as.character()
  data_10nm_weekly$MeteoU_mean12 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character()
  data_10nm_weekly$MeteoU_mean27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey36 %>% as.character()
  data_10nm_weekly$MeteoU_mean36 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$MeteoU_mean39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoU_std
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$U_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey21 %>% as.character()
  data_10nm_weekly$MeteoU_std21 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey27 %>% as.character()
  data_10nm_weekly$MeteoU_std27 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$MeteoU_std48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoV_mean
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$V_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$MeteoV_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey12 %>% as.character()
  data_10nm_weekly$MeteoV_mean12 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$MeteoV_mean39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## MeteoV_std
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$V_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey30 %>% as.character()
  data_10nm_weekly$MeteoV_std30 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$MeteoV_std33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$MeteoV_std39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Hs_mean
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Hs_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey3 %>% as.character()
  data_10nm_weekly$Hs_mean3 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey15 %>% as.character()
  data_10nm_weekly$Hs_mean15 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Hs_std
  
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Hs_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey36 %>% as.character()
  data_10nm_weekly$Hs_std36 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Per_mean
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Per_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey48 %>% as.character()
  data_10nm_weekly$Per_mean48 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Per_std
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Per_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$Per_std6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$Per_std42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Pres_mean
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Pres_mean[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$Pres_mean9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey12 %>% as.character()
  data_10nm_weekly$Pres_mean12 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey33 %>% as.character()
  data_10nm_weekly$Pres_mean33 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$Pres_mean39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Pres_std
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Pres_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey6 %>% as.character()
  data_10nm_weekly$Pres_std6 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$Pres_std9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey18 %>% as.character()
  data_10nm_weekly$Pres_std18 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey21 %>% as.character()
  data_10nm_weekly$Pres_std21 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey24 %>% as.character()
  data_10nm_weekly$Pres_std24 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey30 %>% as.character()
  data_10nm_weekly$Pres_std30 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey39 %>% as.character()
  data_10nm_weekly$Pres_std39 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  ## Dir_std
  f <- function(i){
    if(is.null(meteo_data[[dat[i]]])){
      return(NA)
    }else{
      io <- which.min(sp::spDistsN1(as.matrix(meteo_data[[dat[i]]][,c("Longitude","Latitude")]),cbind(lon[i],lat[i]),longlat = F))
      meteo_data[[dat[i]]]$Dir_std[io]
    }
  }
  
  dat <- data_10nm_weekly$DateKey9 %>% as.character()
  data_10nm_weekly$Dir_std9 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  dat <- data_10nm_weekly$DateKey42 %>% as.character()
  data_10nm_weekly$Dir_std42 <- parallel::mclapply(1:N, f,mc.cores = 7) %>% unlist()
  
  datelags <- c("DateKey0" , "DateKey3", "DateKey6", "DateKey9", "DateKey12", "DateKey15",
                "DateKey18", "DateKey21", "DateKey24", "DateKey27", "DateKey30", "DateKey33",
                "DateKey36", "DateKey39", "DateKey42", "DateKey45", "DateKey48")
  
  data_10nm_weekly <- data_10nm_weekly %>%
    select(-all_of(datelags))  
  
  DBI::dbDisconnect(conn = conn)
  
  return(data_10nm_weekly)
  
}
