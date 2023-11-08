library(zoo)
get_pkhr <- function(df,cname){
  df <- df %>% mutate(Date_Time = make_datetime(Year, as.numeric(Month), Day, as.numeric(Hour)-1))
  df <- df[, c("Year","Month","Day","Hour","Date_Time",cname)]
  df$Actual <- df[[cname]]
  dft <- data.table(df)
  dft[, peak_ind := 0 ]
  dft[,Date:=as.Date(Date_Time)]
  dft[dft[, .I[which.max(Actual)], by=Date]$V1]$peak_ind <- 1
  dft <- data.frame(dft)
  df_pk <- dft %>% filter(peak_ind==1)
  as.numeric(df_pk$Hour)
}

get_pk <- function(df,cname){
  df <- df %>% mutate(Date_Time = make_datetime(Year, as.numeric(Month), Day, as.numeric(Hour)-1))
  df <- df[, c("Year","Month","Day","Hour","Date_Time",cname)]
  df$pred <- df[[cname]]
  dft <- data.table(df)
  dft[, peak_ind := 0 ]
  dft[,Date:=as.Date(Date_Time)]
  dft[dft[, .I[which.max(pred)], by=Date]$V1]$peak_ind <- 1
  dft <- data.frame(dft)
  df_pk <- dft %>% filter(peak_ind==1)
  df_pk$pred
}

get_pk_shape <- function(df,cname){
  # df <- df %>% mutate(Year = year(df$DATE))
  # df <- df %>% mutate(Month = month(df$DATE))
  # df <- df %>% mutate(Day = day(df$DATE))
  df <- df %>% mutate(Date_Time = make_datetime(Year, as.numeric(Month), Day, as.numeric(Hour)-1))
  df <- df[, c("Year","Month","Day","Hour","Date_Time",cname)]
  df$pred <- df[[cname]]
  dft <- data.table(df)
  dft[,Date:=as.Date(Date_Time)]
  dft[, peak_i := 0 ]
  dft[dft[, .I[which.max(pred)], by=Date]$V1]$peak_i <- 1
  # Get index for peak hour, and create vectors with these indexes + (-2,-1,0,1,2)
  # NB: this assumes there are no missing timestamps!!! Could do it based on timestamp to be robust to missing rows if necessary 
  shape_is <- dft[,rep(which(peak_i==1),each=5)+-2:2] 
  shape_is <- shape_is[shape_is <= nrow(dft)]
  shape_is <- shape_is[shape_is >= 0]
  # Add column - 1 = included in shape eval
  dft[, shape := 0 ]
  dft[shape_is,shape:=1]
  df_ret <- data.frame(dft)
  normed <- rollapply(df_ret[[cname]], 24, by = 24, function(i) i/max(i))
  df_ret[paste0(cname,'_normed')] <- melt(t(normed))$value
  df_ret  <- df_ret[, c("shape",paste0(cname,'_normed'))]
}

score_T1 <- function(df_act,df_pred,mod_col_name){
  p1 <- get_pk(df_act,"Load")
  p2 <- get_pk(df_pred,mod_col_name)
  err_pk_mape <- mape(p1,p2)
}
score_T2 <- function(df_act,df_pred,mod_col_name){
  ph1 <- get_pkhr(df_act,"Load")
  ph2 <- get_pkhr(df_pred,mod_col_name)
  delta_pk_hr <- abs(ph1 - ph2)
  delta_pk_hr[delta_pk_hr>1] <- delta_pk_hr[delta_pk_hr>1]*2
  delta_pk_hr[delta_pk_hr>10] <- 10
  sum(delta_pk_hr)
}
# Track 3 Scoring
score_T3 <- function(df_act,df_pred,mod_col_name){
  ph1 <- get_pk_shape(df_act,"Load")
  ph2 <- get_pk_shape(df_pred,mod_col_name)
  peak_shape_comb <- data.frame(cbind(ph1,ph2))
  pred_col <- paste0(mod_col_name,"_normed")
  peak_shape_comb$delta <- abs(peak_shape_comb$Load_normed - peak_shape_comb[[paste0(mod_col_name,"_normed")]])
  delta_pk_shape <- peak_shape_comb%>% filter(shape==1)
  sum(delta_pk_shape$delta)
}


