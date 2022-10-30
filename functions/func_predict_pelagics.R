##############################
# func_predict_catch_cpue_pelagics.R
#
#
#
##############################
f <- function(x,maxp1i){1/(maxp1i - x)}
predict_pelagics <- function(data_list){

  ### Prepare sardine data
  if(data_list$sardine_selected){
    model_sard <- readRDS(file = "models/sardine/sardine_final_best_model_workflow.rds")
    features_sard <- readRDS(file = "models/sardine/features_sard_before_recipe.rds")
    sard_regression_maxP1_o2 <- readRDS(file = "models/sardine/sard_regression_maxP1_o2_new_04Sept2021_40F.rds")
    
    sim_months <- data_list$sim_months
    
    data_sard <- data_list$data %>%
      janitor::clean_names() %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(all_of(features_sard))
    
    data_sard$lag_monthly_catch_t <- data_list$sardine_last_month_catch
    data_sard$month <- lubridate::month(data_sard$date,label = TRUE,abbr = TRUE)
    data_sard <- data_sard %>%
      dplyr::mutate(o2_mean24 = f(o2_mean24,sard_regression_maxP1_o2[1])) %>%
      dplyr::mutate(o2_mean33 = f(o2_mean33,sard_regression_maxP1_o2[2])) %>%
      dplyr::mutate(o2_mean36 = f(o2_mean36,sard_regression_maxP1_o2[3])) %>%
      dplyr::mutate(o2_mean48 = f(o2_mean48,sard_regression_maxP1_o2[4]))
    
    if(length(sim_months)==1){
      catch_sard_all <-
        predict(model_sard, new_data = data_sard %>% dplyr::filter(month(date)==sim_months))%>%
        dplyr::transmute(pred_weight_t = .pred^8) %>%
        dplyr::bind_cols(data_sard %>% dplyr::filter(lubridate::month(date)==sim_months) %>% dplyr::select(date,effort,lon,lat), .)
      
    }else{ # Case of multiple months
      
      catch_sard_all <- data.frame(date = Date(),effort = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
      data_sard <- data_sard %>%
        janitor::clean_names() %>%
        dplyr::arrange(date) %>%
        dplyr::select(all_of(c(features_sard,"month","lag_monthly_catch_t"))) %>%
        dplyr::mutate(lag_date = dplyr::case_when(lubridate::month(date,label = F)==1 ~ date - 31,
                                                  lubridate::month(date,label = F)==2 & lubridate::leap_year(lubridate::year(date)) ~ date - 29,
                                                  lubridate::month(date,label = F)==2 & !lubridate::leap_year(lubridate::year(date)) ~ date - 28,
                                                  lubridate::month(date,label = F)==3 ~ date - 31,
                                                  lubridate::month(date,label = F)==4 ~ date - 30,
                                                  lubridate::month(date,label = F)==5 ~ date - 31,
                                                  lubridate::month(date,label = F)==6 ~ date - 30,
                                                  lubridate::month(date,label = F)==7 ~ date - 31,
                                                  lubridate::month(date,label = F)==8 ~ date - 31,
                                                  lubridate::month(date,label = F)==9 ~ date - 30,
                                                  lubridate::month(date,label = F)==10 ~ date - 31,
                                                  lubridate::month(date,label = F)==11 ~ date - 30,
                                                  TRUE ~ date - 31,
        )) %>%
        dplyr::mutate(year = lubridate::year(date),month = lubridate::month(date,label = TRUE)) %>%
        dplyr::mutate(key =  paste(lubridate::year(lag_date),lubridate::month(lag_date,label = F),sep = "_")) %>%
        dplyr::relocate(key,date,year,month,lat,lon,depth)
      
      for(i in 1:length(sim_months)){
        if(i==1){
          xgb_preds_sard_tmp <-
            predict(model_sard, new_data = data_sard %>% dplyr::filter(month(date)==sim_months[i]))%>%
            dplyr::transmute(pred_weight_t = .pred^8) %>%
            dplyr::bind_cols(data_sard %>% dplyr::filter(month(date)==sim_months[i]) %>% dplyr::select(date,effort,lon,lat), .)
          
          catch_sard_all <- catch_sard_all %>%
            dplyr::bind_rows(xgb_preds_sard_tmp)
          
        }else{
          xgb_preds_sard_prev_month <- catch_sard_all %>%
            dplyr::filter(month(date) == sim_months[i-1])
          
          ### select average effort in this month according to 2019
          if(data_list$type == "cpue"){
            conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
            effort <- DBI::dbGetQuery(conn = conn, statement = "select * from effort_avg_by_month_2019") %>%
              dplyr::filter(Month == sim_months[i-1])
            DBI::dbDisconnect(conn = conn)
            
            tot_month_sard_prev_month <- mean(xgb_preds_sard_prev_month$pred_weight_t)*effort$Effort[1]
          }else{
            tot_month_sard_prev_month <- sum(xgb_preds_sard_prev_month$pred_weight_t)
          }
          
          data_sard_curr_month <- data_sard %>%
            dplyr::filter(month(date) == sim_months[i]) %>%
            dplyr::mutate(lag_monthly_catch_t = tot_month_sard_prev_month)
          
          xgb_preds_sard_tmp <-
            predict(model_sard, new_data = data_sard_curr_month)%>%
            dplyr::transmute(pred_weight_t = .pred^8) %>%
            dplyr::bind_cols(data_sard %>% dplyr::filter(lubridate::month(date)==sim_months[i]) %>% select(date,effort,lon,lat), .)
          
          catch_sard_all <- catch_sard_all %>%
            dplyr::bind_rows(xgb_preds_sard_tmp)
        }
      }
    }
  }else{
    catch_sard_all <- 0
  }
  
  ### Predictions for Scomber Colias (Maquereau)
  if(data_list$maquereau_selected){
    model_maqr <- readRDS(file = "models/maquereau/maquereau_final_best_model_workflow.rds")
    features_maqr <- readRDS(file = "models/maquereau/features_maqr_before_recipe.rds")
    maqr_regression_maxP1_o2 <- readRDS(file = "models/maquereau/maqr_regression_maxP1_o2_new_04Sept2021_50F.rds")
    
    sim_months <- data_list$sim_months
    
    data_maqr <- data_list$data %>%
      janitor::clean_names() %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(all_of(features_maqr))
    
    data_maqr$lag_monthly_catch_t <- data_list$maquereau_last_month_catch
    data_maqr$month <- lubridate::month(data_maqr$date,label = TRUE,abbr = TRUE)
    data_maqr <- data_maqr %>%
      dplyr::mutate(o2_mean9 = f(o2_mean9,maqr_regression_maxP1_o2))
    
    if(length(sim_months)==1){
      catch_maqr_all <-
        predict(model_maqr, new_data = data_maqr %>% dplyr::filter(month(date)==sim_months))%>%
        dplyr::transmute(pred_weight_t = .pred^10) %>%
        dplyr::bind_cols(data_maqr %>% dplyr::filter(lubridate::month(date)==sim_months) %>% dplyr::select(date,effort,lon,lat), .)
      
    }else{ # Case of multiple months
      
      catch_maqr_all <- data.frame(date = Date(),effort = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
      data_maqr <- data_maqr %>%
        janitor::clean_names() %>%
        dplyr::arrange(date) %>%
        dplyr::select(all_of(c(features_maqr,"month","lag_monthly_catch_t"))) %>%
        dplyr::mutate(lag_date = dplyr::case_when(lubridate::month(date,label = F)==1 ~ date - 31,
                                                  lubridate::month(date,label = F)==2 & lubridate::leap_year(lubridate::year(date)) ~ date - 29,
                                                  lubridate::month(date,label = F)==2 & !lubridate::leap_year(lubridate::year(date)) ~ date - 28,
                                                  lubridate::month(date,label = F)==3 ~ date - 31,
                                                  lubridate::month(date,label = F)==4 ~ date - 30,
                                                  lubridate::month(date,label = F)==5 ~ date - 31,
                                                  lubridate::month(date,label = F)==6 ~ date - 30,
                                                  lubridate::month(date,label = F)==7 ~ date - 31,
                                                  lubridate::month(date,label = F)==8 ~ date - 31,
                                                  lubridate::month(date,label = F)==9 ~ date - 30,
                                                  lubridate::month(date,label = F)==10 ~ date - 31,
                                                  lubridate::month(date,label = F)==11 ~ date - 30,
                                                  TRUE ~ date - 31,
        )) %>%
        dplyr::mutate(year = lubridate::year(date),month = lubridate::month(date,label = TRUE)) %>%
        dplyr::mutate(key =  paste(lubridate::year(lag_date),lubridate::month(lag_date,label = F),sep = "_")) %>%
        dplyr::relocate(key,date,year,month,lat,lon,depth)
      
      for(i in 1:length(sim_months)){
        if(i==1){
          xgb_preds_maqr_tmp <-
            predict(model_maqr, new_data = data_maqr %>% dplyr::filter(lubridate::month(date)==sim_months[i]))%>%
            dplyr::transmute(pred_weight_t = .pred^10) %>%
            dplyr::bind_cols(data_maqr %>% dplyr::filter(lubridate::month(date)==sim_months[i]) %>% dplyr::select(date,effort,lon,lat), .)
          
          catch_maqr_all <- catch_maqr_all %>%
            dplyr::bind_rows(xgb_preds_maqr_tmp)
          
        }else{
          xgb_preds_maqr_prev_month <- catch_maqr_all %>%
            dplyr::filter(month(date) == sim_months[i-1])
          
          ### select average effort in this month according to 2019
          if(data_list$type == "cpue"){
            conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
            effort <- DBI::dbGetQuery(conn = conn, statement = "select * from effort_avg_by_month_2019") %>%
              dplyr::filter(Month == sim_months[i-1])
            DBI::dbDisconnect(conn = conn)
            
            tot_month_maqr_prev_month <- mean(xgb_preds_maqr_prev_month$pred_weight_t)*effort$Effort[1]
          }else{
            tot_month_maqr_prev_month <- sum(xgb_preds_maqr_prev_month$pred_weight_t)
          }

          
          data_maqr_curr_month <- data_maqr %>%
            dplyr::filter(month(date) == sim_months[i]) %>%
            dplyr::mutate(lag_monthly_catch_t = tot_month_maqr_prev_month)
          
          xgb_preds_maqr_tmp <-
            predict(model_maqr, new_data = data_maqr_curr_month)%>%
            dplyr::transmute(pred_weight_t = .pred^10) %>%
            dplyr::bind_cols(data_maqr %>% dplyr::filter(lubridate::month(date)==sim_months[i]) %>% select(date,effort,lon,lat), .)
          
          catch_maqr_all <- catch_maqr_all %>%
            dplyr::bind_rows(xgb_preds_maqr_tmp)
        }
      }
    }
  }else{
    catch_maqr_all <- 0
  }
  
  ### Predictions for Trachurus Trachurus (Chinchard)
  if(data_list$chinchard_selected){
    model_chin <- readRDS(file = "models/chinchard/chinchard_final_best_model_workflow.rds")
    features_chin <- readRDS(file = "models/chinchard/features_chin_before_recipe.rds")
    chin_regression_maxP1_o2 <- readRDS(file = "models/chinchard/chin_regression_maxP1_o2_new_04Sept2021_50F.rds")
    
    sim_months <- data_list$sim_months
    
    data_chin <- data_list$data %>%
      janitor::clean_names() %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(all_of(features_chin))
    
    data_chin$lag_monthly_catch_t <- data_list$chinchard_last_month_catch
    data_chin$month <- lubridate::month(data_chin$date,label = TRUE,abbr = TRUE)
    data_chin <- data_chin %>%
      dplyr::mutate(o2_mean9 = f(o2_mean9,chin_regression_maxP1_o2))
    
    if(length(sim_months)==1){
      catch_chin_all <-
        predict(model_chin, new_data = data_chin %>% dplyr::filter(month(date)==sim_months))%>%
        dplyr::transmute(pred_weight_t = .pred^13.33333) %>%
        dplyr::bind_cols(data_chin %>% dplyr::filter(month(date)==sim_months) %>% dplyr::select(date,effort,lon,lat), .)
      
    }else{ # Case of multiple months
      
      catch_chin_all <- data.frame(date = Date(),effort = numeric(),lon = numeric(),lat = numeric(), pred_weight_t = numeric())
      data_chin <- data_chin %>%
        janitor::clean_names() %>%
        dplyr::arrange(date) %>%
        dplyr::select(all_of(c(features_chin,"month","lag_monthly_catch_t"))) %>%
        dplyr::mutate(lag_date = dplyr::case_when(lubridate::month(date,label = F)==1 ~ date - 31,
                                                  lubridate::month(date,label = F)==2 & lubridate::leap_year(lubridate::year(date)) ~ date - 29,
                                                  lubridate::month(date,label = F)==2 & !lubridate::leap_year(lubridate::year(date)) ~ date - 28,
                                                  lubridate::month(date,label = F)==3 ~ date - 31,
                                                  lubridate::month(date,label = F)==4 ~ date - 30,
                                                  lubridate::month(date,label = F)==5 ~ date - 31,
                                                  lubridate::month(date,label = F)==6 ~ date - 30,
                                                  lubridate::month(date,label = F)==7 ~ date - 31,
                                                  lubridate::month(date,label = F)==8 ~ date - 31,
                                                  lubridate::month(date,label = F)==9 ~ date - 30,
                                                  lubridate::month(date,label = F)==10 ~ date - 31,
                                                  lubridate::month(date,label = F)==11 ~ date - 30,
                                                  TRUE ~ date - 31,
        )) %>%
        dplyr::mutate(year = lubridate::year(date),month = lubridate::month(date,label = TRUE)) %>%
        dplyr::mutate(key =  paste(lubridate::year(lag_date),lubridate::month(lag_date,label = F),sep = "_")) %>%
        dplyr::relocate(key,date,year,month,lat,lon,depth)
      
      for(i in 1:length(sim_months)){
        if(i==1){
          xgb_preds_chin_tmp <-
            predict(model_chin, new_data = data_chin %>% dplyr::filter(lubridate::month(date)==sim_months[i]))%>%
            dplyr::transmute(pred_weight_t = .pred^13.33333) %>%
            dplyr::bind_cols(data_chin %>% dplyr::filter(lubridate::month(date)==sim_months[i]) %>% dplyr::select(date,effort,lon,lat), .)
          
          catch_chin_all <- catch_chin_all %>%
            dplyr::bind_rows(xgb_preds_chin_tmp)
          
        }else{
          xgb_preds_chin_prev_month <- catch_chin_all %>%
            dplyr::filter(month(date) == sim_months[i-1])
          
          ### select average effort in this month according to 2019
          if(data_list$type == "cpue"){
            conn <- DBI::dbConnect(RSQLite::SQLite(),"database/fisheyeDB")
            effort <- DBI::dbGetQuery(conn = conn, statement = "select * from effort_avg_by_month_2019") %>%
              dplyr::filter(Month == sim_months[i-1])
            DBI::dbDisconnect(conn = conn)
            
            tot_month_chin_prev_month <- mean(xgb_preds_chin_prev_month$pred_weight_t)*effort$Effort[1]
          }else{
            tot_month_chin_prev_month <- sum(xgb_preds_chin_prev_month$pred_weight_t)
          }
          
          data_chin_curr_month <- data_chin %>%
            dplyr::filter(month(date) == sim_months[i]) %>%
            dplyr::mutate(lag_monthly_catch_t = tot_month_chin_prev_month)
          
          xgb_preds_chin_tmp <-
            predict(model_chin, new_data = data_chin_curr_month)%>%
            dplyr::transmute(pred_weight_t = .pred^13.33333) %>%
            dplyr::bind_cols(data_chin %>% dplyr::filter(lubridate::month(date)==sim_months[i]) %>% select(date,effort,lon,lat), .)
          
          catch_chin_all <- catch_chin_all %>%
            dplyr::bind_rows(xgb_preds_chin_tmp)
        }
      }
    }
  }else{
    catch_chin_all <- 0
  }
  
  catch_all <- list("sardine" = catch_sard_all, "maquereau" = catch_maqr_all, "chinchard" = catch_chin_all)
  
  return(catch_all)    
}
