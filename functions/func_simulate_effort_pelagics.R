##############################
# func_simulate_effort_pelagics.R
#
# This function simulates fishing effort
# for small pelagics tab.
##############################

func_simulate_effort_pelagics <- function(data_list){
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  
  env_weight <- data_list$env_weight
  dist_weight <- data_list$dist_weight
  hist_weight <- data_list$hist_weight
  
  fishing_direction <- data_list$fishing_direction
  sim_year <- data_list$sim_year
  sim_months <- data_list$sim_months
  sim_ref_year <- data_list$sim_ref_year
  espece_cible <- data_list$espece_cible
  
  cpue <- data_list$cpue
  
  ### Calculate the probability of visiting a square based on historical data
  if(sim_ref_year == "Mean(All)"){
    prob_hist <- DBI::dbGetQuery(conn, statement = "select * from effort_historical_fishing_effort_by_vessel_year_month_position_table") %>%
      dplyr::filter(Month %in% sim_months) %>%
      dplyr::group_by(Latitude,Longitude,Month) %>%
      dplyr::summarise(Effort = mean(Effort)) %>% dplyr::ungroup() %>%
      dplyr::group_by(Month) %>% dplyr::summarise(Latitude = Latitude,Longitude = Longitude,Prob_history = Effort/sum(Effort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(lon_key = stringr::str_sub(string = as.character(Longitude*10000),start = 1,end = 6)) %>%
      dplyr::mutate(lat_key = stringr::str_sub(string = as.character(Latitude*10000),start = 1,end = 6))
  }else{
    prob_hist <- DBI::dbGetQuery(conn, statement = "select * from effort_historical_fishing_effort_by_vessel_year_month_position_table") %>%
      dplyr::filter(Month %in% sim_months, Year == sim_ref_year) %>%
      dplyr::group_by(Month) %>% dplyr::summarise(Latitude = Latitude,Longitude = Longitude,Prob_history = Effort/sum(Effort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(lon_key = stringr::str_sub(string = as.character(Longitude*10000),start = 1,end = 6)) %>%
      dplyr::mutate(lat_key = stringr::str_sub(string = as.character(Latitude*10000),start = 1,end = 6))
  }
  
  ### Calculate the probability of visiting a square based on distance from dakhla and direction
  if(fishing_direction == "sud_dakhla"){
    dist <- DBI::dbGetQuery(conn = conn,statement = "select * from coordinates_south_27N_Distance_Dakhla where Direction = 'Nord Dakhla'")
    minNord <- quantile(dist$Distance_Dakhla,0.1)
    
    dist <- DBI::dbGetQuery(conn = conn,statement = "select * from coordinates_south_27N_Distance_Dakhla") %>%
      dplyr::mutate(Distance_Dakhla = ifelse(test = Direction == "Nord Dakhla",yes = minNord - Distance_Dakhla,no = Distance_Dakhla)) %>%
      dplyr::mutate(Distance_Dakhla = ifelse(test = Distance_Dakhla >= 0, yes = Distance_Dakhla,no = 0))
  }else{
    dist <- DBI::dbGetQuery(conn = conn,statement = "select * from coordinates_south_27N_Distance_Dakhla where Direction = 'Sud Dakhla'")
    minSud <- quantile(dist$Distance_Dakhla,0.1)
    
    dist <- DBI::dbGetQuery(conn = conn,statement = "select * from coordinates_south_27N_Distance_Dakhla") %>%
      dplyr::mutate(Distance_Dakhla = ifelse(test = Direction == "Sud Dakhla",yes = minSud - Distance_Dakhla,no = Distance_Dakhla)) %>%
      dplyr::mutate(Distance_Dakhla = ifelse(test = Distance_Dakhla >= 0, yes = Distance_Dakhla,no = 0))
  }
  
  prob_dist <- dist %>% dplyr::mutate(Prob_Distance = Distance_Dakhla / sum(Distance_Dakhla)) %>%
    dplyr::mutate(lon_key = stringr::str_sub(string = as.character(Longitude*10000),start = 1,end = 6)) %>%
    dplyr::mutate(lat_key = stringr::str_sub(string = as.character(Latitude*10000),start = 1,end = 6))
  
  ### Calculate the probability of visiting a square based on environment data using CPUE predictions
  prob_env <- cpue %>%
    dplyr::select(date, lon, lat, pred_weight_t) %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::mutate(month = ifelse(test = month < min(sim_months),yes = min(sim_months),no = month)) %>%
    dplyr::mutate(month = ifelse(test = month > max(sim_months),yes = max(sim_months),no = month)) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(Prob_Environment = pred_weight_t / sum(pred_weight_t)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lon_key = stringr::str_sub(string = as.character(lon*10000),start = 1,end = 6)) %>%
    dplyr::mutate(lat_key = stringr::str_sub(string = as.character(lat*10000),start = 1,end = 6))
  
  ### Joining the three probabilities in one table (mainly by week to account for environment prob)
  
  prob_join <- prob_env %>%
    dplyr::left_join(dplyr::select(prob_hist,Month,lon_key, lat_key, Prob_history), by = c("month"="Month", "lon_key"="lon_key","lat_key"="lat_key")) %>%
    dplyr::left_join(dplyr::select(prob_dist,lon_key,lat_key,Prob_Distance), by = c("lon_key"="lon_key","lat_key"="lat_key")) %>%
    dplyr::mutate(Prob_join = (Prob_Environment*env_weight + Prob_history*hist_weight + Prob_Distance*dist_weight)/(env_weight+hist_weight+dist_weight))
  
  ### Simulate Effort
  seed <- round(runif(n = 1,min = 10000,max = 99999))
  
  ### Get the list of vessels to simulate from
  Vessels <- DBI::dbGetQuery(conn,statement = "select distinct(I_NCEE) from effort_tracks_by_vessel_to_simulate_from_table")
  
  ### Number of tracks made by the vessels during the simulation months
  if(sim_ref_year == "Mean(All)"){
    Nbr_Maree_Per_Vessel_Per_Month <- DBI::dbGetQuery(conn = conn,statement = "select * from effort_count_tracks_per_vessel_year_month_table") %>%
      dplyr::filter(vessREF %in% Vessels$I_NCEE) %>%
      dplyr::group_by(Month, vessREF) %>%
      dplyr::summarise(NbrMareePerVessel = round(mean(NbrMareePerVessel))) %>% dplyr::ungroup() %>%
      dplyr::filter(Month %in% sim_months) %>%
      dplyr::select(Month, vessREF, NbrMareePerVessel)
  }else{
    Nbr_Maree_Per_Vessel_Per_Month <- DBI::dbGetQuery(conn = conn,statement = "select * from effort_count_tracks_per_vessel_year_month_table") %>%
      dplyr::filter(Year == sim_ref_year, Month %in% sim_months) %>%
      dplyr::select(Month, vessREF, NbrMareePerVessel)
  }
  
  ### Get the list of vessel that were active during the simulation months
  Nbr_Vessels_Active <- Nbr_Maree_Per_Vessel_Per_Month %>%
    dplyr::group_by(Month) %>%
    dplyr::summarise(N_Active = n_distinct(vessREF)) %>%
    dplyr::mutate(N_Active = ifelse(N_Active > 24, 24, N_Active)) %>%
    dplyr::ungroup()
  
  ### Sample from the list of all vessels the number of active vessels by simulation month
  list_vessels <- character()
  set.seed(seed = seed)
  for(i in 1:length(sim_months)){
    list_vessels = c(list_vessels,sample(x = Vessels$I_NCEE,size = Nbr_Vessels_Active$N_Active[i],replace = F))
  }
  
  ### Calculate the number of tracks to do during simulation based on the number of tracks made in the reference year
  if(length(unique(Nbr_Maree_Per_Vessel_Per_Month$vessREF))>24){
    set.seed(seed = seed)
    vessel_samp <- sample(x = unique(Nbr_Maree_Per_Vessel_Per_Month$vessREF),size = 24,replace = FALSE)
    maree_per_vessel_per_month <- Nbr_Maree_Per_Vessel_Per_Month %>%
      dplyr::filter(vessREF %in% vessel_samp) %>%
      dplyr::select(NbrMareePerVessel)
  }else{
    maree_per_vessel_per_month <- Nbr_Maree_Per_Vessel_Per_Month %>%
      dplyr::select(NbrMareePerVessel)
  }
  
  ### Create a dataframe of the list of vessel and the corresponding number of tracks during simulation months
  df <- data.frame("Matricule" = list_vessels, NrMaree = maree_per_vessel_per_month$NbrMareePerVessel)
  
  ### Get all historical tracks
  all_tracks <- DBI::dbGetQuery(conn = conn,statement = "select *  from effort_tracks_by_vessel_to_simulate_from_table")
  
  ### Sample tracks for each vessels from its historical list of tracks
  list_tracks <- list()
  set.seed(seed = seed)
  for(i in 1:nrow(df)){
    reff <- df$Matricule[i]
    d <- all_tracks %>% dplyr::filter(I_NCEE == reff)
    list_tracks[[i]] <- sample(x = d$T_NUM,size = df$NrMaree[i],replace = T)
  }
  
  ### Simulate dates distribution from MAIA
  if(sim_ref_year ==  "Mean(All)"){
    set.seed(seed = seed)
    sampled_year <- sample(x = 2014:2019,size = 1,replace = F)
    
    logbook <- DBI::dbGetQuery(conn = conn,statement = "select * from effort_logbooks_2014_2019_table") %>% 
      dplyr::mutate(DateAggr = fromChronToDate(DateAggr)) %>%
      tidyr::separate(DateAggr, into = c("DateAggr","Time"),sep = " ") %>%
      dplyr::mutate(DateAggr = as.Date(DateAggr,format = "%m/%d/%Y")) %>%
      dplyr::select(-Time) %>%
      dplyr::mutate(Year = lubridate::year(DateAggr)) %>%
      dplyr::filter(Year == sampled_year) %>%
      dplyr::filter(Month %in% sim_months) %>%
      dplyr::group_by(Month,DateAggr) %>%
      dplyr::summarise(Marees = n()) %>%
      dplyr::group_by(Month) %>%
      dplyr::mutate(prop = Marees/sum(Marees),
             cumprop = cumsum(prop)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Month,DateAggr,prop) %>%
      dplyr::mutate(prop = round(prop,digits = 4))
  }else{
    logbook <- DBI::dbGetQuery(conn = conn,statement = "select * from effort_logbooks_2014_2019_table") %>% 
      dplyr::mutate(DateAggr = fromChronToDate(DateAggr)) %>%
      tidyr::separate(DateAggr, into = c("DateAggr","Time"),sep = " ") %>%
      dplyr::mutate(DateAggr = as.Date(DateAggr,format = "%m/%d/%Y")) %>%
      dplyr::select(-Time) %>%
      dplyr::mutate(Year = lubridate::year(DateAggr)) %>%
      dplyr::filter(Year == sim_ref_year) %>%
      dplyr::filter(Month %in% sim_months) %>%
      dplyr::group_by(Month,DateAggr) %>%
      dplyr::summarise(Marees = n()) %>%
      dplyr::group_by(Month) %>%
      dplyr::mutate(prop = Marees/sum(Marees),
             cumprop = cumsum(prop)) %>%
      dplyr::ungroup() %>%
      dplyr::select(Month,DateAggr,prop) %>%
      dplyr::mutate(prop = round(prop,digits = 4))
  }
  
  inter_fishing <- DBI::dbGetQuery(conn = conn,statement = "select *  from effort_vms_interpolated_tracks_table") 
  
  simulation <- data.frame(
    i_id = numeric(),
    I_NCEE = character(),
    LAT = numeric(),
    LON = numeric(),
    DATE = numeric(),
    SPE = numeric(),
    HEA = numeric(),
    W_HARB = integer(),
    T_NUM = integer(),
    P_ID =  numeric(),
    P_INT = numeric(),
    T_ID = numeric(),
    DEPTH = numeric(),
    Month = numeric()
  )
  months_sim_long <- rep(sim_months,Nbr_Vessels_Active$N_Active)
  for(i in 1:nrow(df)){
    tmp <- inter_fishing %>%
      dplyr::filter(I_NCEE == df$Matricule[i],T_NUM %in% list_tracks[[i]])
    
    tmp$Month <- rep(months_sim_long[i],nrow(tmp))
    simulation <- simulation %>%
      dplyr::bind_rows(tmp)
  }
  
  simulation <- simulation  %>%
    mutate(DATE = fromChronToDate(DATE)) %>%
    tidyr::separate(col = DATE,into = c("Date","Time"),sep = " ") %>%
    dplyr::mutate(Date = as.Date(Date, format  = "%m/%d/%Y"))
  
  nbr_maree_per_month <- simulation %>% dplyr::group_by(Month) %>% dplyr::summarise(Tot_Maree = n_distinct(I_NCEE,T_NUM))
  
  logbook <- logbook %>%
    dplyr::left_join(nbr_maree_per_month,by = "Month")
  
  logbook <- logbook %>%
    dplyr::mutate(NbrMaree = round(Tot_Maree * prop)) %>%
    dplyr::group_by(Month) %>%
    dplyr::mutate(CumNbrMaree = cumsum(NbrMaree))
  
  for(i in 2:nrow(logbook)){
    if(i == nrow(logbook)){
      logbook$NbrMaree[i] <- logbook$NbrMaree[i] - (logbook$CumNbrMaree[i] - logbook$Tot_Maree[i])
    }else{
      if(logbook$Month[i] != logbook$Month[i-1]){
        logbook$NbrMaree[i-1] <- logbook$NbrMaree[i-1] - (logbook$CumNbrMaree[i-1] - logbook$Tot_Maree[i-1])
      }
    }
  }
  
  logbook <- logbook %>%
    ungroup() %>%
    dplyr::select(Month,DateAggr,NbrMaree) %>%
    dplyr::mutate(CumNbrMaree = cumsum(NbrMaree))
  
  key <- simulation %>% dplyr::mutate(key = paste(simulation$I_NCEE,simulation$T_NUM,simulation$Month,sep = "_")) %>% 
    dplyr::select(Month,key) %>% dplyr::distinct(key,.keep_all = TRUE)
  
  dates <- numeric()
  for(i in 1:nrow(logbook)){
    dates <- c(dates, rep(logbook$DateAggr[i],logbook$NbrMaree[i]))
  }
  
  key <- key %>%
    dplyr::mutate(Week = fromChronToDate(dates)) %>%
    tidyr::separate(col = Week,into = c("Week","Tm"),sep = " ") %>%
    dplyr::mutate(Week = as.Date(Week,format = "%m/%d/%Y")) %>%
    dplyr::select(-Tm,-Month)
  
  simulation <- simulation %>%
    dplyr::mutate(key = paste(I_NCEE,T_NUM,Month,sep = "_")) %>%
    dplyr::left_join(key,by = "key") %>%
    dplyr::select(I_NCEE,Week,Month,LAT,LON,DEPTH,T_NUM,SPE,HEA) %>%
    dplyr::rename(DATE = Week,VessREF = I_NCEE)
  
  ### Assigning the coordinates
  set.seed(seed = seed)
  numlinesSim <- nrow(simulation)
  sampling <- sample(x = 1:nrow(prob_join),size = numlinesSim,replace = TRUE,prob = prob_join$Prob_join)
  
  simulation$LAT <- prob_join$lat[sampling]
  simulation$LON <- prob_join$lon[sampling]
  
  simulation$Effort <- 50
  
  simulation_10nm <- simulation %>%
    dplyr::select(VessREF,DATE,T_NUM,LAT,LON,DEPTH,Effort) %>%
    dplyr::mutate(LON = grade(LON, 0.1666667),
           LAT = grade(LAT, 0.1666667)) %>%
    dplyr::group_by(DATE,VessREF,T_NUM,LON, LAT) %>%
    dplyr::summarise(DEPTH = mean(DEPTH),
              Effort = sum(Effort)) %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    dplyr::ungroup()
  
  simulation_10nm <- adjust_dates(data = simulation_10nm, sim_year = sim_year, year_to_simulate_form = sim_ref_year)
  
  simulation_10nm_weekly <- simulation_10nm %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    dplyr::mutate(DATE = grade_date(x = (DATE-0*30),dx = 8)) %>%
    dplyr::mutate(Key = paste(VessREF,T_NUM,sep = "_")) %>%
    mutate_if(is.character, factor) %>%
    dplyr::select(-T_NUM) %>%
    dplyr::group_by(DATE, LON, LAT) %>%
    dplyr::summarise(
      Marees = n_distinct(Key),
      Vessels = n_distinct(VessREF),
      DEPTH = mean(DEPTH),
      Effort = sum(Effort)
    ) %>%
    dplyr::filter(lubridate::month(DATE) %in% sim_months) %>%
    dplyr::ungroup() 
  
  DBI::dbDisconnect(conn = conn)
  
  return(simulation_10nm_weekly)
}

