##############################
# func_adjust_date_difference.R
#
# 
# 
##############################

adjust_dates <- function(data, sim_year, year_to_simulate_form){
  data$DATE <- as.Date(data$DATE)
  
  year_to_simulate_form <- lubridate::year(data$DATE[1])
  diff_years <- sim_year - year_to_simulate_form 
  data$DATE <- ymd(data$DATE) %m+% months(diff_years*12)
  
  return(data) 
}