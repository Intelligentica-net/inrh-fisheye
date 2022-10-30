##############################
# func_effort_prep_pelagics_cpue.R
#
# 
# 
##############################

create_effort_data_pelagics_cpue <- function(sim_year, sim_months){
  
  conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
  coordinates <- DBI::dbGetQuery(conn = conn,"select * from coordinates_south_27N")
  DBI::dbDisconnect(conn = conn)
  
  month_days_vec <- rep(c(1,9,17,25),length(sim_months))
  months_vec <- rep(sim_months,each = 4)
  
  ### Create simulation dates
  sim_dates <- character()
  for(i in 1:length(months_vec)){
    sim_dates <- c(sim_dates, paste(sim_year,months_vec[i],month_days_vec[i],sep = "-"))
  }
  
  ### Create simulation effort data
  latitude <- rep(coordinates$Latitude, length(sim_dates))
  longitude <- rep(coordinates$Longitude, length(sim_dates))
  depth <- rep(coordinates$DEPTH, length(sim_dates))
  sim_dates <- rep(sim_dates, each = length(coordinates$Latitude))
  
  effort_table <- data.frame(DATE = sim_dates, 
                             LAT = latitude,  
                             LON = longitude, 
                             Effort = 60,  
                             Depth = depth)
  
  ### Aggregate to 10 nautical miles
  effort_table_10nm <- effort_table %>%
    mutate(LON = grade(LON, 0.1666667),
           LAT = grade(LAT, 0.1666667)) %>%
    group_by(DATE,LON, LAT) %>%
    summarise(Depth = mean(Depth),
              Effort = mean(Effort)) %>%
    mutate(DATE = as.Date(DATE)) %>%
    ungroup() 
  
  return(effort_table_10nm)
}