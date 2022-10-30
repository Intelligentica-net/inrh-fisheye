##############################
# func_predict_negro_attaque.R
#
#
#
##############################

predictAttaqueNegro <- function(data_list){
  model <- readRDS(file = "models/negro/negro_final_model_for_prediction.rds")
  
  data_list <- data_list %>%
    dplyr::mutate(Date = as.Date(Date)) %>%
    dplyr::mutate(Month = lubridate::month(Date,label = T,abbr = T),Quarter = lubridate::quarter(Date))
  
  preds <- predict(object = model, new_data =  data_list,type = "prob")
  
  predictions <- data_list %>%
    dplyr::select(Date, Longitude, Latitude) %>%
    dplyr::bind_cols(preds) %>%
    dplyr::rename(Pred_Attaque = .pred_Attaque, DATE = Date, LON = Longitude, LAT = Latitude) %>%
    dplyr::select(-.pred_NoAttaque)
  
  weeks <- unique(predictions$DATE)
  Nweeks <- length(weeks)
  
  l <- list("data" = predictions, "weeks"  = weeks, "Nweeks" = Nweeks)
  l
}
