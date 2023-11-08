library(tidyverse)
library(dplyr)
library(tibbletime) # used for rollify function
library(readxl) # Used to read in the excel data
library(lubridate) # Used for make_datetime
library(Metrics)
library(data.table)
library(gbm)
library(caret)
library(randomForest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./scoring_functions_paper.R")

data_save <- "../data/results/"

# Set up variables for each round
weather_stations <- c(1,3,5)
rounds <- c(1,2,3,4,5,6)
round_id <-1
date_cutoffs <- c('2018-01-01','2018-03-01','2018-06-01','2018-08-01','2018-09-01','2018-11-01')
lead_lag <- c("lead","lag","none")
results_all <- data.frame()
for(round_id in 1:6){
  results_round <- data.frame()
# Iterate over zones
  for(zone_id in 1:3){
    weather_id <- weather_stations[zone_id]
    df_data <- readRDS(paste0('../data/features/w',weather_id,'_none_rnd',round_id,'_vars.rds'))
    df_data$Load <- df_data[,paste0('Zone',zone_id)]
    date_filter <- date_cutoffs[round_id]
    dtrain <- filter(df_data, DATE < date_filter)
    dtest <- filter(df_data, DATE >= date_filter)
    c_train <- dtrain[,c("Year","Month","Day","Hour","Load")]
    c_test <- dtest[,c("Year","Month","Day","Hour","Load")]
    for(ll_id in 1:3){
      df_data <- readRDS(paste0('../data/features/w',weather_id,'_',lead_lag[ll_id],'_rnd',round_id,'_vars.rds'))
      df_data$Load <- df_data[,paste0('Zone',zone_id)]
      df_data <- df_data[,c("DATE","Year","Month","Day","Hour","Load","Trend","WDay",
                            "T_use","THlag1","THlag2","THlag3","THlag4","THlag5","THlag6","TDsma1",
                            "THlag9","THlag12","THlag15","THlag18","THlag21","THlag24","TDsma2","TDsma3",
                            "Tes995","Tes99","Holiday")]
      dtrain <- filter(df_data, DATE < date_filter)
      dtest <- filter(df_data, DATE >= date_filter)
      model_name <- paste0("mlr_",lead_lag[ll_id],zone_id)
      
      mlr_mod <-lm(log1p(Load) ~ Trend + poly(T_use,3) + Hour + Month + WDay + 
                    poly(T_use,3)*Hour + 
                    poly(T_use,3)*Month +
                    poly(THlag1,3)*Hour +
                    poly(THlag1,3)*Month +
                    poly(THlag2,3)*Hour +
                    poly(THlag2,3)*Month +
                    poly(THlag3,3)*Hour +
                    poly(THlag3,3)*Month +
                    poly(THlag6,3)*Hour +
                    poly(THlag6,3)*Month +
                    poly(TDsma1,3)*Hour +
                    poly(TDsma1,3)*Month + 
                    poly(Tes995, 3)*Hour+
                    poly(Tes99, 3)*Hour+
                    WDay*Hour + c(Holiday), data=dtrain)
      c_train[paste0("mlr_",lead_lag[ll_id])] <- exp(predict(mlr_mod,dtrain))
      c_test[paste0("mlr_",lead_lag[ll_id])] <- exp(predict(mlr_mod,dtest))

      cdt <- data.table(dtrain)
      setDT(cdt)[, Max:= which.max(Load), DATE]
      cdt[,wt:=dnorm(as.numeric(Hour), Max, 1.5)]
      
      model_name <- paste0("mlrwt_",lead_lag[ll_id],zone_id)
      mlr_wt <-lm(log1p(Load) ~ Trend + poly(T_use,3) + Hour + Month + WDay + 
                     poly(T_use,3)*Hour + 
                     poly(T_use,3)*Month +
                     poly(THlag1,3)*Hour +
                     poly(THlag1,3)*Month +
                     poly(THlag2,3)*Hour +
                     poly(THlag2,3)*Month +
                     poly(THlag3,3)*Hour +
                     poly(THlag3,3)*Month +
                     poly(THlag6,3)*Hour +
                     poly(THlag6,3)*Month +
                     poly(TDsma1,3)*Hour +
                     poly(TDsma1,3)*Month + 
                     poly(Tes995, 3)*Hour+
                     poly(Tes99, 3)*Hour+
                     WDay*Hour + c(Holiday), data=cdt, weights=wt)
      c_train[paste0("mlrwt_",lead_lag[ll_id])] <- exp(predict(mlr_wt,dtrain))
      c_test[paste0("mlrwt_",lead_lag[ll_id])] <- exp(predict(mlr_wt,dtest))
            
      cpk <- data.table(dtrain)
      setDT(cpk)[, Max:= max(Load), DATE]
      setDT(cpk)[, Min:= min(Load), DATE]
      cpk[,wt:=((Load-Min)/(Max-Min))]
      
      model_name <- paste0("mlrlwt_",lead_lag[ll_id],zone_id)
      mlr_lwt <-lm(log1p(Load) ~ Trend + poly(T_use,3) + Hour + Month + WDay + 
                     poly(T_use,3)*Hour + 
                     poly(T_use,3)*Month +
                     poly(THlag1,3)*Hour +
                     poly(THlag1,3)*Month +
                     poly(THlag2,3)*Hour +
                     poly(THlag2,3)*Month +
                     poly(THlag3,3)*Hour +
                     poly(THlag3,3)*Month +
                     poly(THlag6,3)*Hour +
                     poly(THlag6,3)*Month +
                     poly(TDsma1,3)*Hour +
                     poly(TDsma1,3)*Month + 
                     poly(Tes995, 3)*Hour+
                     poly(Tes99, 3)*Hour+
                     WDay*Hour + c(Holiday), data=cpk, weights=wt)
      c_train[paste0("mlrlwt_",lead_lag[ll_id])] <- exp(predict(mlr_lwt,dtrain))
      c_test[paste0("mlrlwt_",lead_lag[ll_id])] <- exp(predict(mlr_lwt,dtest))
      
      # Test GBM
      set.seed(123)
      mod_gbm <- gbm(
        formula = log1p(Load) ~ Trend + T_use + Hour + Month + WDay + 
          Holiday + THlag1+ THlag2+ THlag3+ THlag4+ THlag5+ THlag6
        + THlag9+ THlag12+ THlag15 + THlag18+ THlag21+ THlag24
        +TDsma1+TDsma2+TDsma3+Tes99+Tes995,
        distribution = "laplace",
        data = dtrain,
        n.trees = 2000,
        n.minobsinnode = 300,
        interaction.depth = 3,
        bag.fraction = .8,
        shrinkage = 0.1,
        cv.folds = 5,
        n.cores = 12, # will use all cores by default
        verbose = FALSE
      )
      c_train[paste0("gbm_",lead_lag[ll_id])] <- exp(predict(mod_gbm, dtrain))
      c_test[paste0("gbm_",lead_lag[ll_id])] <- exp(predict(mod_gbm, dtest))
      
      c_train[paste0("avg_",lead_lag[ll_id])] <- (c_train[paste0("mlr_",lead_lag[ll_id])]+ 
                                                    c_train[paste0("gbm_",lead_lag[ll_id])])/2
      
      c_test[paste0("avg_",lead_lag[ll_id])] <- (c_test[paste0("mlr_",lead_lag[ll_id])]+ 
                                                   c_test[paste0("gbm_",lead_lag[ll_id])])/2
      
      c_train[paste0("avgwt_",lead_lag[ll_id])] <- (c_train[paste0("mlrwt_",lead_lag[ll_id])]+ 
                                                       c_train[paste0("gbm_",lead_lag[ll_id])])/2
      
      c_test[paste0("avgwt_",lead_lag[ll_id])] <- (c_test[paste0("mlrwt_",lead_lag[ll_id])]+ 
                                                      c_test[paste0("gbm_",lead_lag[ll_id])])/2
      
      c_train[paste0("avglwt_",lead_lag[ll_id])] <- (c_train[paste0("mlrlwt_",lead_lag[ll_id])]+ 
                                                       c_train[paste0("gbm_",lead_lag[ll_id])])/2
      
      c_test[paste0("avglwt_",lead_lag[ll_id])] <- (c_test[paste0("mlrlwt_",lead_lag[ll_id])]+ 
                                                      c_test[paste0("gbm_",lead_lag[ll_id])])/2
      
      models <- c("mlr","mlrwt","mlrlwt","gbm","avg","avgwt","avglwt")
      for (i in 1:length(models)){
        mape_train <- mape(dtrain$Load, c_train[[paste0(models[i],"_",lead_lag[ll_id])]])
        mape_test <- mape(dtest$Load, c_test[[paste0(models[i],"_",lead_lag[ll_id])]])
        model_name <- paste0(models[i],"_",lead_lag[ll_id])
        # Track 1 Scoring
        m1 <- score_T1(dtrain,c_train,model_name)
        m2 <- score_T1(dtest,c_test,model_name)
        # Track 2 Scoring
        h1 <- score_T2(dtrain,c_train,model_name)
        h2 <- score_T2(dtest,c_test,model_name)
        # Track 3 Scoring
        s1 <- score_T3(dtrain,c_train,model_name)
        s2 <- score_T3(dtest,c_test,model_name)
        temp <- data.frame(Zone=zone_id, model_name=model_name, mape_train=mape_train, mape_test=mape_test, round=round_id,
                           T1_train= m1, T1_test=m2,
                           T2_train= h1, T2_test=h2, 
                           T3_train=s1, T3_test=s2)
        results_round <- rbind(results_round, temp)
      }
    }
    # Create mlr avg
    c_train$avg_mlr <- (c_train$mlr_lag+c_train$mlr_lead+c_train$mlr_none)/3
    c_test$avg_mlr <- (c_test$mlr_lag+c_test$mlr_lead+c_test$mlr_none)/3
    
    c_train$avg_mlrwt <- (c_train$mlrwt_lag+c_train$mlrwt_lead+c_train$mlrwt_none)/3
    c_test$avg_mlrwt <- (c_test$mlrwt_lag+c_test$mlrwt_lead+c_test$mlrwt_none)/3
    
    c_train$avg_mlrlwt <- (c_train$mlrlwt_lag+c_train$mlrlwt_lead+c_train$mlrlwt_none)/3
    c_test$avg_mlrlwt <- (c_test$mlrlwt_lag+c_test$mlrlwt_lead+c_test$mlrlwt_none)/3
    
    # Create gbm avg
    c_train$avg_gbm <- (c_train$gbm_lag+c_train$gbm_lead+c_train$gbm_none)/3
    c_test$avg_gbm <- (c_test$gbm_lag+c_test$gbm_lead+c_test$gbm_none)/3
    
    # Create overall avgs
    c_train$avg3 <- (c_train$avg_lag+c_train$avg_lead+c_train$avg_none)/3
    c_test$avg3 <- (c_test$avg_lag+c_test$avg_lead+c_test$avg_none)/3
    
    c_train$avg3wt <- (c_train$avgwt_lag+c_train$avgwt_lead+c_train$avgwt_none)/3
    c_test$avg3wt <- (c_test$avgwt_lag+c_test$avgwt_lead+c_test$avgwt_none)/3
    
    c_train$avg3lwt <- (c_train$avglwt_lag+c_train$avglwt_lead+c_train$avglwt_none)/3
    c_test$avg3lwt <- (c_test$avglwt_lag+c_test$avglwt_lead+c_test$avglwt_none)/3
    
    # mape_train <- mape(dtrain$Load, c_train$avg3)
    # mape_test <- 999
    models2 <- c("avg_mlr","avg_mlrwt","avg_mlrlwt","avg_gbm","avg3","avg3wt","avg3lwt")
    for (i in 1:length(models2)){
      model_name <- paste0(models2[i])
      # Track 1 Scoring
      m1 <- score_T1(dtrain,c_train,model_name)
      m2 <- score_T1(dtest,c_test,model_name)
      # Track 2 Scoring
      h1 <- score_T2(dtrain,c_train,model_name)
      h2 <- score_T2(dtest,c_test,model_name)
      # Track 3 Scoring
      s1 <- score_T3(dtrain,c_train,model_name)
      s2 <- score_T3(dtest,c_test,model_name)
      temp <- data.frame(Zone=zone_id, model_name=model_name, mape_train=mape_train, mape_test=mape_test, round=round_id,
                         T1_train= m1, T1_test=m2,
                         T2_train= h1, T2_test=h2,
                         T3_train=s1, T3_test=s2)
      results_round <- rbind(results_round, temp)
    }
    # results_hourly <- rbind(c_train,c_test)
    # write.csv(results_hourly,paste0(data_save,"Zone_",zone_id,"_R",round_id,"_hourly_results_paper.csv"))
  }
  write.csv(results_round,paste0(data_save,"R",round_id,"_results_paper.csv"))
  results_all <- rbind(results_all,results_round)
}
write.csv(results_all,paste0(data_save,"Full_results_paper.csv"))