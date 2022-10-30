##############################
# func_predict_cephalopod.R
#
#
#
##############################

f <- function(x,maxp1i){1/(maxp1i - x)}

predict_fishing_cephalopods <- function(data_list){
  
  leftskews_poulpe <- readRDS(file = "models/Poulpe/poulpe_regression_maxP1_o2.rds")
  features_poulpe <- readRDS(file = "models/Poulpe/poulpe_regression_f20_features.rds")
  recipe_poulpe <- readRDS(file = "models/Poulpe/poulpe_xgb_regression_recipe_f20.rds")
  model_poulpe <- readRDS(file = "models/Poulpe/poulpe_xgb_regression_final_best_f20.rds")
  
  leftskews_calmar <- readRDS(file = "models/Calmar/calmar_regression_maxP1_o2.rds")
  features_calmar <- readRDS(file = "models/Calmar/Calmar_regression_f20_features.rds")
  recipe_calmar <- readRDS(file = "models/Calmar/calmar_xgb_regression_recipe_f20.rds")
  model_calmar <- readRDS(file = "models/Calmar/calmar_xgb_regression_final_best_f20.rds")
  
  leftskews_seiche <- readRDS(file = "models/Seiche/choco_regression_maxP1_o2.rds")
  features_seiche <- readRDS(file = "models/Seiche/Seiche_regression_f20_features.rds")
  recipe_seiche <- readRDS(file = "models/Seiche/choco_xgb_regression_recipe_f20.rds")
  model_seiche <- readRDS(file = "models/Seiche/choco_xgb_regression_final_best_f20.rds")
  
  nWeeks <- data_list$nWeeks
  
  data_poulpe <- data_list$data %>%
    janitor::clean_names() %>%
    dplyr::rename(week_cpue_lag = week_cpue_lag_poulpe) %>%
    dplyr::select(all_of(features_poulpe)) %>%
    dplyr::mutate(o2_mean2 = f(o2_mean2, leftskews_poulpe[1])) %>%
    dplyr::mutate(o2_mean5 = f(o2_mean5, leftskews_poulpe[2]))
  
  data_calmar <- data_list$data %>%
    janitor::clean_names() %>%
    dplyr::rename(week_cpue_lag = week_cpue_lag_calmar) %>%
    dplyr::select(all_of(features_calmar)) %>%
    dplyr::mutate(o2_mean1 = f(o2_mean1, leftskews_calmar))
  
  data_seiche <- data_list$data %>%
    janitor::clean_names() %>%
    dplyr::rename(week_cpue_lag = week_cpue_lag_seiche) %>%
    dplyr::select(all_of(features_seiche)) %>%
    dplyr::mutate(o2_mean2 = f(o2_mean2, leftskews_seiche))
  
  cpue_poulpe_all <- data.frame(date = Date(),week = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
  cpue_calmar_all <- data.frame(date = Date(),week = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
  cpue_seiche_all <- data.frame(date = Date(),week = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
  
  ### Case when Poulpe Species is Selected
  
  if(data_list$poulpeSelected){
    fcastData_poulpe_prep <- recipe_poulpe %>% recipes::prep() %>% recipes::bake(new_data = data_poulpe[,features_poulpe])
    
    for(i in 1:nWeeks){
      if(i ==1){
        preds_poulpe_tmp <- 
          predict(model_poulpe, new_data = fcastData_poulpe_prep %>% dplyr::filter(week==i))%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_poulpe %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_poulpe_all <- cpue_poulpe_all %>%
          dplyr::bind_rows(preds_poulpe_tmp)
      }else{
        preds_poulpe_prev_month <- cpue_poulpe_all %>%
          dplyr::filter(week == i-1) 
        
        mean_cpue_poulpe_prev_week <- mean(preds_poulpe_prev_month$pred_weight_t)
        
        data_poulpe_curr_week <- fcastData_poulpe_prep %>% 
          dplyr::filter(week == i) %>%
          dplyr::mutate(week_cpue_lag = mean_cpue_poulpe_prev_week)
        
        preds_poulpe_tmp <- 
          predict(model_poulpe, new_data = data_poulpe_curr_week)%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_poulpe %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_poulpe_all <- cpue_poulpe_all %>%
          dplyr::bind_rows(preds_poulpe_tmp)
      }
    }
    cpue_poulpe_all <- cpue_poulpe_all %>%
      dplyr::mutate(pred_weight_t = pred_weight_t^(1/0.275))
  }else{
    cpue_poulpe_all <- 0
  }
  
  ### Case when Calmar Species is Selected
  
  if(data_list$calmarSelected){
    fcastData_calmar_prep <- recipe_calmar %>% recipes::prep() %>% recipes::bake(new_data = data_calmar[,features_calmar])
    
    for(i in 1:nWeeks){
      if(i ==1){
        preds_calmar_tmp <- 
          predict(model_calmar, new_data = fcastData_calmar_prep %>% dplyr::filter(week==i))%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_calmar %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_calmar_all <- cpue_calmar_all %>%
          dplyr::bind_rows(preds_calmar_tmp)
      }else{
        preds_calmar_prev_month <- cpue_calmar_all %>%
          dplyr::filter(week == i-1) 
        
        mean_cpue_calmar_prev_week <- mean(preds_calmar_prev_month$pred_weight_t)
        
        data_calmar_curr_week <- fcastData_calmar_prep %>% 
          dplyr::filter(week == i) %>%
          dplyr::mutate(week_cpue_lag = mean_cpue_calmar_prev_week)
        
        preds_calmar_tmp <- 
          predict(model_calmar, new_data = data_calmar_curr_week)%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_calmar %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_calmar_all <- cpue_calmar_all %>%
          dplyr::bind_rows(preds_calmar_tmp)
      }
    }
    cpue_calmar_all <- cpue_calmar_all %>%
      dplyr::mutate(pred_weight_t = pred_weight_t^(1/0.075))
  }else{
    cpue_calmar_all <- 0
  }
  
  ### Case when Seiche Species is Selected
  
  if(data_list$seicheSelected){
    fcastData_seiche_prep <- recipe_seiche %>% recipes::prep() %>% recipes::bake(new_data = data_seiche[,features_seiche])
    
    for(i in 1:nWeeks){
      if(i ==1){
        preds_seiche_tmp <- 
          predict(model_seiche, new_data = fcastData_seiche_prep %>% dplyr::filter(week==i))%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_seiche %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_seiche_all <- cpue_seiche_all %>%
          dplyr::bind_rows(preds_seiche_tmp)
      }else{
        preds_seiche_prev_month <- cpue_seiche_all %>%
          dplyr::filter(week == i-1) 
        
        mean_cpue_seiche_prev_week <- mean(preds_seiche_prev_month$pred_weight_t)
        
        data_seiche_curr_week <- fcastData_seiche_prep %>% 
          dplyr::filter(week == i) %>%
          dplyr::mutate(week_cpue_lag = mean_cpue_seiche_prev_week)
        
        preds_seiche_tmp <- 
          predict(model_seiche, new_data = data_seiche_curr_week)%>%
          dplyr::transmute(pred_weight_t = .pred) %>%
          dplyr::bind_cols(data_seiche %>% dplyr::filter(week==i) %>% dplyr::select(date,week,lon,lat), .)
        
        cpue_seiche_all <- cpue_seiche_all %>%
          dplyr::bind_rows(preds_seiche_tmp)
      }
    }
    cpue_seiche_all <- cpue_seiche_all %>%
      dplyr::mutate(pred_weight_t = pred_weight_t^(1/0.3))
  }else{
    cpue_seiche_all <- 0
  }
  
  catch_all <- list("poulpe" = cpue_poulpe_all, "calmar" = cpue_calmar_all, "seiche" = cpue_seiche_all)
  return(catch_all)
  
}