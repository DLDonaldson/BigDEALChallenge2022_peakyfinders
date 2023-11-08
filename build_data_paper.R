library(tidyverse)
library(dplyr)
library(tibbletime) # used for rollify function
library(readxl) # Used to read in the excel data
library(lubridate) # Used for make_datetime
library(Metrics)
library(data.table)
library(gbm)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./create_explanatory_variables_paper.R")


# Load historical demand and temperature
rnd1_hist <- read_excel('../data/Round1.xlsx')
rnd2_hist <- read_excel('../data/Round2.xlsx')
rnd3_hist <- read_excel('../data/Round3.xlsx')
rnd4_hist <- read_excel('../data/Round4.xlsx')
rnd5_hist <- read_excel('../data/Round5.xlsx')
rnd6_hist <- read_excel('../data/Round6.xlsx')
rnd6_act <- read_excel('../data/Round6_actuals.xlsx')
rnd1_fcst<- read_excel('../data/Round1.xlsx',sheet = 'temperature forecast')
rnd2_fcst<- read_excel('../data/Round2.xlsx',sheet = 'temperature forecast')
rnd3_fcst<- read_excel('../data/Round3.xlsx',sheet = 'temperature forecast')
rnd4_fcst<- read_excel('../data/Round4.xlsx',sheet = 'temperature forecast')
rnd5_fcst<- read_excel('../data/Round5.xlsx',sheet = 'temperature forecast')
rnd6_fcst<- read_excel('../data/Round6.xlsx',sheet = 'temperature forecast')
colnames(rnd1_hist) = gsub("LDC", "Zone", colnames(rnd1_hist)) 
colnames(rnd5_hist) = gsub("LDC", "Zone", colnames(rnd5_hist))
colnames(rnd6_act) = gsub("LDC", "Zone", colnames(rnd6_act))

hist1 <- rbind(rnd1_hist)
rnd1_fcst$Zone1 <- rnd2_hist$Zone1
rnd1_fcst$Zone2 <- rnd2_hist$Zone2
rnd1_fcst$Zone3 <- rnd2_hist$Zone3
rnd1 <- rbind(hist1, rnd1_fcst) 

hist2 <- rbind(hist1,rnd2_hist)
rnd2_fcst$Zone1 <- rnd3_hist$Zone1
rnd2_fcst$Zone2 <- rnd3_hist$Zone2
rnd2_fcst$Zone3 <- rnd3_hist$Zone3
rnd2 <- rbind(hist2, rnd2_fcst) 

hist3 <- rbind(hist2,rnd3_hist)
rnd3_fcst$Zone1 <- rnd4_hist$Zone1
rnd3_fcst$Zone2 <- rnd4_hist$Zone2
rnd3_fcst$Zone3 <- rnd4_hist$Zone3
rnd3 <- rbind(hist3, rnd3_fcst) 

hist4 <- rbind(hist3,rnd4_hist)
rnd4_fcst$Zone1 <- rnd5_hist$Zone1
rnd4_fcst$Zone2 <- rnd5_hist$Zone2
rnd4_fcst$Zone3 <- rnd5_hist$Zone3
rnd4 <- rbind(hist4, rnd4_fcst) 

hist5 <- rbind(hist4,rnd5_hist)
rnd5_fcst$Zone1 <- rnd6_hist$Zone1
rnd5_fcst$Zone2 <- rnd6_hist$Zone2
rnd5_fcst$Zone3 <- rnd6_hist$Zone3
rnd5 <- rbind(hist5, rnd5_fcst) 

hist6 <- rbind(hist5,rnd6_hist)
rnd6_fcst$Zone1 <- rnd6_act$Zone1
rnd6_fcst$Zone2 <- rnd6_act$Zone2
rnd6_fcst$Zone3 <- rnd6_act$Zone3
rnd6 <- rbind(hist6, rnd6_fcst) 

saveRDS(rnd1, "../data/round1_data.rds")
saveRDS(rnd2, "../data/round2_data.rds")
saveRDS(rnd3, "../data/round3_data.rds")
saveRDS(rnd4, "../data/round4_data.rds")
saveRDS(rnd5, "../data/round5_data.rds")
saveRDS(rnd6, "../data/round6_data.rds")

# Weather station 1 is best for zone 1, 3 for zone 2 and 5 for zone 3
lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd1,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd1_vars.rds"))
  }
}

lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd2,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd2_vars.rds"))
  }
}

lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd3,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd3_vars.rds"))
  }
}

lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd4,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd4_vars.rds"))
  }
}

lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd5,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd5_vars.rds"))
  }
}

lead_lag <- c("lead","lag","none")
for (weather_id in c(1,3,5)){
  for(ll_id in 1:3){
    df_vars <- build_vars(rnd6,24,3,weather_id,lead_lag[ll_id])
    saveRDS(df_vars,paste0("../data/w",weather_id,"_",lead_lag[ll_id],"_rnd6_vars.rds"))
  }
}

