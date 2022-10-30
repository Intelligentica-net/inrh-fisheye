# library(fs)
# library(tidyverse)
# library(stringr)
# 
# dir_files <- "~/Gentica/Clients/INRH/BDFA/Ocean/bio2008_2021/"
# 
# bio_data_list <- dir_files %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl <- bio_data_list %>%
#   set_names(dir_ls(dir_files)) %>%
#   bind_rows(.id = "file_path") %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 76,end = 85)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   filter(Profondeur <= 0.52) %>%
#   select(-Profondeur,-O2_min,-O2_max,-CHL_min,-CHL_max,-PP_min,-PP_max,-file_path,-Date) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) %>%
#   select(Date,Longitude,Latitude,O2_mean,O2_std,CHL_mean,CHL_std,PP_mean,PP_std)
# 
# dir_files2 <- "~/Gentica/Clients/INRH/BDFA/Ocean/ocean_data/bio/"
# 
# bio_data_list2 <- dir_files2 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl2 <- bio_data_list2 %>%
#   set_names(dir_ls(dir_files2)) %>%
#   bind_rows(.id = "file_path") %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 78,end = 87)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-O2_min,-O2_max,-CHL_min,-CHL_max,-PP_min,-PP_max,-file_path,-Date) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) %>%
#   select(Date,Longitude,Latitude,O2_mean,O2_std,CHL_mean,CHL_std,PP_mean,PP_std)
# 
# bio_data <- bio_data_tbl %>%
#   bind_rows(bio_data_tbl2)
# 
# write.csv(x = bio_data,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/bio_data_2008_2021.csv",row.names = F)
# 
# 
# #### Données physique
# 
# dir_my1 <- "~/Gentica/Clients/INRH/BDFA/Ocean/Phys/"
# 
# bio_data_list1 <- dir_my1 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl1 <- bio_data_list1 %>%
#   set_names(dir_ls(dir_my1)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_1 <- bio_data_tbl1 %>%
#   filter(Profondeur < 0.5) %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 69,end = 78)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-TEMP_min,-TEMP_max,-Profondeur,-SAL_min,-SAL_max,-U_min,-U_max,-file_path,-Date,-V_min,-V_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) %>%
#   rename(TEMP_mean = OTEMP_mean)
# 
# write.csv(x = bio_data_tbl1_1,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/physique_2008_2018.csv",row.names = F)
# 
# 
# dir_my2 <- "~/Gentica/Clients/INRH/BDFA/Ocean/Ocean_Data_2019/Phys/"
# 
# bio_data_list2 <- dir_my2 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl2 <- bio_data_list2 %>%
#   set_names(dir_ls(dir_my2)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_2 <- bio_data_tbl2 %>%
#   filter(Profondeur < 0.5) %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 85,end = 94)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-TEMP_min,-TEMP_max,-Profondeur,-SAL_min,-SAL_max,-U_min,-U_max,-file_path,-Date,-V_min,-V_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) 
# 
# write.csv(x = bio_data_tbl1_2,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/physique_2019_Jun2019.csv",row.names = F)
# 
# 
# #####
# 
# dir_my3 <- "~/Gentica/Clients/INRH/BDFA/Ocean/ocean_data/phys/"
# 
# bio_data_list3 <- dir_my3 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl3 <- bio_data_list3 %>%
#   set_names(dir_ls(dir_my3)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_3 <- bio_data_tbl3 %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 80,end = 89)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-TEMP_min,-TEMP_max,-SAL_min,-SAL_max,-U_min,-U_max,-file_path,-Date,-V_min,-V_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) 
# 
# write.csv(x = bio_data_tbl1_3,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/physique_2019Juil_2021.csv",row.names = F)
# 
# 
# 
# #### Temperature
# dir_my1 <- "~/Gentica/Clients/INRH/BDFA/Ocean/Meteo/"
# 
# bio_data_list1 <- dir_my1 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl1 <- bio_data_list1 %>%
#   set_names(dir_ls(dir_my1)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_1 <- bio_data_tbl1 %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 71,end = 80)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-Pres_min,-Pres_max,-U_min,-U_max,-V_min,-V_max,-file_path,-Date,-Hs_min,-Hs_max,-Per_min,-Per_max,-Dir_min,-Dir_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) 
# 
# write.csv(x = bio_data_tbl1_1,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/meteo_2008_2018.csv",row.names = F)
# 
# 
# ###
# dir_my2 <- "~/Gentica/Clients/INRH/BDFA/Ocean/Ocean_Data_2019/Meteo/"
# 
# bio_data_list2 <- dir_my2 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# bio_data_tbl2 <- bio_data_list2 %>%
#   set_names(dir_ls(dir_my2)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_2 <- bio_data_tbl2 %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 87,end = 96)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-Pres_min,-Pres_max,-U_min,-U_max,-V_min,-V_max,-file_path,-Date,-Hs_min,-Hs_max,-Per_min,-Per_max,-Dir_min,-Dir_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) 
# 
# write.csv(x = bio_data_tbl1_2,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/meteo_2019_2019Jun.csv",row.names = F)
# 
# #####
# dir_my3 <- "~/Gentica/Clients/INRH/BDFA/Ocean/ocean_data/meteo/"
# 
# bio_data_list2 <- dir_my3 %>%
#   dir_ls() %>%
#   map(.f = function(path){
#     read_csv(path)
#   })
# 
# 
# bio_data_tbl2 <- bio_data_list2 %>%
#   set_names(dir_ls(dir_my3)) %>%
#   bind_rows(.id = "file_path")
# 
# bio_data_tbl1_2 <- bio_data_tbl2 %>%
#   mutate(Date_bis = str_sub(string = file_path,start = 82,end = 91)) %>%
#   relocate(Date_bis,.after = Date) %>%
#   select(-Pres_min,-Pres_max,-U_min,-U_max,-V_min,-V_max,-file_path,-Date,-Hs_min,-Hs_max,-Per_min,-Per_max,-Dir_min,-Dir_max) %>%
#   rename(Date = Date_bis) %>%
#   mutate(Date = as.Date(Date,format = "%Y.%m.%d")) 
# 
# write.csv(x = bio_data_tbl1_2,file = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/meteo_Jul2019_2021.csv",row.names = F)
# 
# 
# 
# ###########################
# setwd(dir = "~/Gentica/Clients/INRH/BDFA/Ocean/final_files/")
# physique1 <- read_csv(file = "physique_2008_2018.csv")
# physique2 <- read_csv(file = "physique_2019_Jun2019.csv")
# physique3 <- read_csv(file = "physique_2019Juil_2021.csv")
# 
# colsi <- names(physique1)
# 
# physique2 <- physique2 %>%
#   select(all_of(colsi))
# 
# physique3 <- physique3 %>%
#   select(all_of(colsi))
# 
# physique <- physique1 %>%
#   bind_rows(physique2) %>%
#   bind_rows(physique3)
# 
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# 
# oceano_physique_south_table <- physique %>%
#   filter(Latitude <= 28)
# 
# oceano_physique_med_table <- physique %>%
#   filter(Latitude >= 34)
# 
# dbWriteTable(conn = conn,name = "oceano_physique_south_table",value = oceano_physique_south_table, overwrite = TRUE)
# dbWriteTable(conn = conn,name = "oceano_physique_med_table",value = oceano_physique_med_table, overwrite = TRUE)
# 
# dbDisconnect(conn)
# ### Meteo
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# 
# meteo1 <- read_csv(file = "meteo_2008_2018.csv")
# meteo2 <- read_csv(file = "meteo_2019_2019Jun.csv")
# meteo3 <- read_csv(file = "meteo_Jul2019_2021.csv")
# 
# colsi <- names(meteo1)
# 
# meteo2 <- meteo2 %>%
#   select(all_of(colsi))
# 
# meteo3 <- meteo3 %>%
#   select(all_of(colsi))
# 
# meteo <- meteo1 %>%
#   bind_rows(meteo2) %>%
#   bind_rows(meteo3)
# 
# oceano_meteo_south_table <- meteo %>%
#   filter(Latitude <= 28)
# 
# oceano_meteo_med_table <- meteo %>%
#   filter(Latitude >= 34)
# 
# dbWriteTable(conn = conn,name = "oceano_meteo_south_table",value = oceano_meteo_south_table, overwrite = TRUE)
# dbWriteTable(conn = conn,name = "oceano_meteo_med_table",value = oceano_meteo_med_table, overwrite = TRUE)
# 
# dbDisconnect(conn)
# 
# ####
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# biology <- read_csv(file = "bio_data_2008_2021.csv")
# 
# oceano_biology_south_table <- biology %>%
#   filter(Latitude <= 28)
# 
# oceano_biology_med_table <- biology %>%
#   filter(Latitude >= 34)
# 
# dbWriteTable(conn = conn,name = "oceano_biology_south_table",value = oceano_biology_south_table, overwrite = TRUE)
# dbWriteTable(conn = conn,name = "oceano_biology_med_table",value = oceano_biology_med_table, overwrite = TRUE)
# 
# dbDisconnect(conn)
# 
# ###
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# 
# oceano_sst_table <- read_csv(file = "indice_upwelling_sst_hebdo_2008_2019.csv")
# 
# dbWriteTable(conn = conn,name = "oceano_sst_table",value = oceano_sst_table, overwrite = TRUE)
# 
# dbDisconnect(conn)
# 
# ####
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# oceano_ekman_table <- read_csv(file = "indice_upwelling_ekman_hebdo_2008_2019.csv")
# 
# dbWriteTable(conn = conn,name = "oceano_ekman_table",value = oceano_ekman_table, overwrite = TRUE)
# dbDisconnect(conn)
# 
# ###
# conn <- DBI::dbConnect(RSQLite::SQLite(),"~/FisherApp/database/fisheyeDB")
# oceano_nao_table <- read_csv(file = "indice_nao_hebdo_2008_2021.csv")
# 
# dbWriteTable(conn = conn,name = "oceano_nao_table",value = oceano_nao_table, overwrite = TRUE)
# 
# 
