# Code to evaluate the peak hour variability of various loads
library(readxl) # Used to read in the excel data
library(ggplot2)#
library(tidyverse)
library(extrafont)
library(gridExtra)
library(zoo)
loadfonts()

library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Case 1: 3 years of load history for case study
historical_actuals <- read_excel('../data/Full_Rounds.xlsx',sheet = 'load and temperature history ')
competition_fcst <- read_excel('../data/Full_Rounds.xlsx',sheet = 'temperature forecast')

df <- historical_actuals %>% mutate(Year = year(historical_actuals$DATE))
df <- df %>% mutate(Month = month(historical_actuals$DATE))
df <- df %>% mutate(Day = day(historical_actuals$DATE))
df <- df %>% mutate(Date_Time = make_datetime(Year, Month, Day, Hour))

Figure1a <- ggplot(df, aes(x = Date_Time, y = LDC3)) +
  
  geom_vline(xintercept=as.POSIXct(as.Date(c("2018-01-01","2018-03-01","2018-06-01",
                                             "2018-08-01","2018-09-01","2018-11-01"))),
             linetype='dashed',color = "orange", linewidth=.5) + theme_bw(base_size=14,base_family="Times New Roman")+
  geom_line(color="black") +  
  annotate("text",x=as.POSIXct(as.Date(c("2016-01-01"))),y=2500,label="History", size=6)+
  annotate("text",x=as.POSIXct(as.Date(c("2018-09-01"))),y=2500,label="Rounds", size=6)+
  theme(axis.text.y=element_text(size=14, colour= "black")) +
  theme(axis.text.x=element_text(size=14, colour= "black")) +
  theme(axis.title.x=element_text(size=14, colour = "black")) +
  theme(axis.title.y=element_text(size=14, colour = "black")) +
  labs(y= "Load LDC3", x = "Date Time")

png("../data/Figure1a.png", width = 17.5, height = 9, units = 'cm', res = 300)
Figure1a
dev.off()

Figure1b <- ggplot(df, aes(x=((T1-32)*(5/9)), y=LDC3)) + 
  geom_point(size=0.5)+ theme_bw(base_size=14,base_family="Times New Roman")+
  theme(axis.text.y=element_text(size=14, colour= "black")) +
  theme(axis.text.x=element_text(size=14, colour= "black")) +
  theme(axis.title.x=element_text(size=14, colour = "black")) +
  theme(axis.title.y=element_text(size=14, colour = "black"))+
  labs(y= "Load LDC3", x = "Temperature (\u00B0C)")

png("../data/Figure1b.png", width = 17.5, height = 9, units = 'cm', res = 300)
Figure1b
dev.off()

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

pk_hr_train <- as.data.frame(get_pkhr(df,"LDC1"))

dft = data.table(df)
daily_pk <- dft[,max(LDC1),by=floor_date(DATE)]
daily_pk$hr <- pk_hr_train
daily_pk$dsd <- rollapply(daily_pk$hr,width=30,FUN=sd,fill=0,align="r")
daily_pk$month <- floor_date(daily_pk$floor_date, "month")
monthly_std_pk <- daily_pk %>%
  group_by(month) %>%
  summarize(LDC1 = sd(hr))

pk_hr_train <- as.data.frame(get_pkhr(df,"LDC2"))

dft = data.table(df)
daily_pk <- dft[,max(LDC2),by=floor_date(DATE)]
daily_pk$hr <- pk_hr_train
daily_pk$dsd <- rollapply(daily_pk$hr,width=30,FUN=sd,fill=0,align="r")
daily_pk$month <- floor_date(daily_pk$floor_date, "month")
monthly_std_pk$LDC2 <- daily_pk %>%
  group_by(month) %>%
  summarize(hr_std = sd(hr)) %>% pull(hr_std)

pk_hr_train <- as.data.frame(get_pkhr(df,"LDC3"))

dft = data.table(df)
daily_pk <- dft[,max(LDC3),by=floor_date(DATE)]
daily_pk$hr <- pk_hr_train
daily_pk$dsd <- rollapply(daily_pk$hr,width=30,FUN=sd,fill=0,align="r")
daily_pk$month <- floor_date(daily_pk$floor_date, "month")
monthly_std_pk$LDC3 <- daily_pk %>%
  group_by(month) %>%
  summarize(hr_std = sd(hr)) %>% pull(hr_std)

monthly_std_pk_long <- melt(monthly_std_pk, id.vars = 'month',variable.name = 'LDC',value.name = 'Load')

group.colors <- c(LDC1 = "#FF6600", LDC2 = "#339999", LDC3 ="black")

Figure6 <- 
  ggplot(monthly_std_pk_long, aes(x = month, y = Load, color = LDC)) + geom_line(linewidth=1) +
    geom_vline(xintercept=as.POSIXct(as.Date(c("2018-01-01","2018-03-01","2018-06-01",
                                             "2018-08-01","2018-09-01","2018-11-01"))),
             linetype='dashed',color = "orange", linewidth=.5) + theme_bw(base_size=14,base_family="Times New Roman")+
  theme(axis.text.y=element_text(size=14, colour= "black")) +
  theme(axis.text.x=element_text(size=14, colour= "black")) +
  theme(axis.title.x=element_text(size=14, colour = "black")) +
  theme(axis.title.y=element_text(size=14, colour = "black")) +
  labs(y= "Monthly Standard Deviation of Peak Hour", x = "Date Time") + 
  scale_colour_manual(values=group.colors)

png("../data/Figure6.png", width = 17.5, height = 9, units = 'cm', res = 300)
Figure6
dev.off()


Figure1a <- ggplot(df, aes(x = Date_Time, y = LDC3)) +
  
  geom_vline(xintercept=as.POSIXct(as.Date(c("2018-01-01","2018-03-01","2018-06-01",
                                             "2018-08-01","2018-09-01","2018-11-01"))),
             linetype='dashed',color = "orange", linewidth=.5) + theme_bw(base_size=14,base_family="Times New Roman")+
  geom_line(color="black") +  
  annotate("text",x=as.POSIXct(as.Date(c("2016-01-01"))),y=2500,label="History", size=6)+
  annotate("text",x=as.POSIXct(as.Date(c("2018-09-01"))),y=2500,label="Rounds", size=6)+
  theme(axis.text.y=element_text(size=14, colour= "black")) +
  theme(axis.text.x=element_text(size=14, colour= "black")) +
  theme(axis.title.x=element_text(size=14, colour = "black")) +
  theme(axis.title.y=element_text(size=14, colour = "black")) +
  labs(y= "Load LDC3", x = "Date Time")

png("../data/Figure1a.png", width = 17.5, height = 9, units = 'cm', res = 300)
Figure1a
dev.off()


df_shapes_compare <- df
df_shapes_compare$day_type <- as.factor('none')
setDT(df_shapes_compare)[DATE=="2015-01-03",  day_type := 'dual' ]
winter <- df_shapes_compare[day_type=='dual',]
winter$LDC3 <- winter$LDC3/max(winter$LDC3)

df_shapes_compare <- df
df_shapes_compare$day_type <- as.factor(0)
setDT(df_shapes_compare)[DATE>="2015-08-01" & DATE <= "2015-08-02",  day_type := 'single' ]
summer <- df_shapes_compare[day_type=='single',]
summer$LDC3 <- summer$LDC3/max(summer$LDC3)


days_compare <- rbind(winter,summer) %>% select(c('Hour','LDC3','day_type'))

group.colors1 <- c(single = "#FF6600", dual = "#339999")
FigureShape <- ggplot(days_compare, aes(x = Hour, y = LDC3, color = day_type)) +
  geom_line() + theme_bw(base_size=14,base_family="Times New Roman")+
  theme(axis.text.y=element_text(size=14, colour= "black")) +
  theme(axis.text.x=element_text(size=14, colour= "black")) +
  theme(axis.title.x=element_text(size=14, colour = "black")) +
  theme(axis.title.y=element_text(size=14, colour = "black")) +
  labs(y= "Load Shape (pu)", x = "Hour")+ 
  ylim(0,1)+
  scale_x_continuous(minor_breaks = seq(1, 24, 1), breaks = seq(1, 24, 3)) +
  scale_colour_manual(values=group.colors1)+
  theme(legend.position=c(0.9, 0.2))

png("../data/FigureShape.png", width = 17.5, height = 9, units = 'cm', res = 300)
FigureShape
dev.off()