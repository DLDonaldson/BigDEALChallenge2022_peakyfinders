library("tis")
add_calendar_var <- function(competition_data) {
  df <- competition_data %>% mutate(Year = year(competition_data$DATE))
  df <- df %>% mutate(Month = month(competition_data$DATE))
  df <- df %>% mutate(Day = day(competition_data$DATE))
  df <- df %>% mutate(Date_Time = make_datetime(Year, Month, Day, Hour))
  df$Month <- as.factor(df$Month)
  df$Weekday <- wday(df$DATE)
  df$WDay <- as.factor(df$Weekday)
  df$Hour <- as.factor(df$Hour)
  df$Hour_num <- as.numeric(df$Hour)
  df$DOY <- as.numeric(format(df$Date_Time,"%j"))
  df
}

add_trend_var <- function(df1) {
  df1$Trend <- 1:nrow(df1)
  df1 <- as.data.table(df1)
  df1[,t := .I/.N] # Same as trend but from 0 to 1
  df1 <- as.data.frame(df1)
  df1
}

add_temp_hlags <- function(df1, n_hlag) {
  for(i in 1:n_hlag){
    df1[[paste0("THlag",i)]] <- lag(df1$T_use,i)
  }
  df1 <- df1 %>% fill(colnames(df1), .direction = "up")
  df1
}
add_smoothed_temp <- function(df1, smooth_param){
  # Exponentially smooth temps
  df1 <- as.data.table(df1)
  
  df1[1,Tes99 :=T_use]
  df1[1,Tes995 :=T_use]
  df1[1,Tes95 :=T_use]
  df1[1,Tes875 :=T_use]
  df1[1,Tes75 :=T_use]
  for(i in 2:nrow(df1)){# Assuming no missing rows/timesteps!
    df1[i,Tes75 := (1-.75)*T_use+.75*df1$Tes95[i-1]]
    df1[i,Tes875 := (1-.875)*T_use+.875*df1$Tes95[i-1]]
    df1[i,Tes95 := (1-.95)*T_use+.95*df1$Tes95[i-1]]
    df1[i,Tes99 := (1-.99)*T_use+.99*df1$Tes99[i-1]]
    df1[i,Tes995 := (1-.995)*T_use+.995*df1$Tes995[i-1]]
  }
  df1 <- as.data.frame(df1)
  df1
}
add_temp_dlags <- function(df1, n_dlag) {
  for(i in 1:n_dlag){
    rolling_mean <- rollify(mean, window = i*24)
    df1[[paste0("TDsma",i)]] <- rolling_mean(df1$T_use)
  }
  df1 <- df1 %>% fill(colnames(df1), .direction = "up")
  df1
}
add_temp_minmax <- function(df1) {
  for(i in 1:7){
    rolling_max <- rollify(max, window = i*24)
    df1[[paste0("Tmax",i,"D")]] <- rolling_max(df1$T_use)
    rolling_min <- rollify(min, window = i*24)
    df1[[paste0("Tmin",i,"D")]] <- rolling_min(df1$T_use)
    rolling_sd <- rollify(sd, window = i*24)
    df1[[paste0("Tsd",i,"D")]] <- rolling_sd(df1$T_use)
  }
  df1 <- df1 %>% fill(colnames(df1), .direction = "up")
  df1
}
add_folds <- function(df1){
  df1 <- df1 %>% mutate(kfold = Year-2014) # set so there is a fold for each year
  df1
}

create_holidays <- function(df1){
  df1 <- df1 %>% mutate(Holiday = as.factor(isHoliday(Date_Time)))
  df1$WDayH <- df1$WDay
  df1[df1$Holiday == TRUE, "WDayH"] <- "1"
  df1
}

  
build_vars <- function(df_init, n_hlag, n_dlag, stat_sel, type_run){
  df_init$T_use = df_init[[paste0("T",stat_sel)]]
  if(type_run=="lag"){
    df_init$T_use <- lag(df_init$T_use,1)
    df_init <- df_init %>% fill(T_use, .direction = "up")
  }
  if(type_run=="lag2"){
    df_init$T_use <- lag(df_init$T_use,2)
    df_init <- df_init %>% fill(T_use, .direction = "up")
  }
  if(type_run=="lead"){
    df_init$T_use <- lead(df_init$T_use,1)
    df_init <- df_init %>% fill(T_use, .direction = "down")
  }
  if(type_run=="lead2"){
    df_init$T_use <- lead(df_init$T_use,2)
    df_init <- df_init %>% fill(T_use, .direction = "down")
  }
  df_var <- add_calendar_var(df_init)
  df_var <- add_trend_var(df_var)
  
  df_var <- add_temp_hlags(df_var,n_hlag)
  df_var <- add_temp_dlags(df_var,n_dlag)
  df_var <- add_temp_minmax(df_var)
  df_var <- add_folds(df_var)
  df_var <- create_holidays(df_var)
  df_var <- add_smoothed_temp(df_var,0.95)
  df_var
}
