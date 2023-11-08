rnd_1 <- read.csv('../data/results/R1_results_paper.csv')
rnd_2 <- read.csv('../data/results/R2_results_paper.csv')
rnd_3 <- read.csv('../data/results/R3_results_paper.csv')
rnd_4 <- read.csv('../data/results/R4_results_paper.csv')
rnd_5 <- read.csv('../data/results/R5_results_paper.csv')
rnd_6 <- read.csv('../data/results/R6_results_paper.csv')
reporting <- c('GBM','GBM-T','MLR','MLR-T','MLR-L','MLR-W','MLR-TL','MLR-TW',
               'ENS','ENS-L','ENS-W','ENS-T','ENS-TL','ENS-TW')

score_table <- function(rnd_data,round_id,days_round, report_names){
  if(round_id == 1){
  r_out <- rnd_data %>% group_by(model_name) %>% summarise(
          T1_train = round(mean(T1_train)*100,2),
          T1 = round(mean(T1_test)*100,2),
          T2_train = round(mean(T2_train)/1096,2),
          T2 = round(mean(T2_test)/days_round,2),
          T3_train = round(mean(T3_train)/1096,3),
          T3 = round(mean(T3_test)/days_round,3),
  ) %>%
    mutate(model_name = recode(model_name, mlr_none = 'MLR', avg_mlr = 'MLR-T', mlrwt_none =  'MLR-W', mlrlwt_none =  'MLR-L',
                              avg_mlrlwt =  'MLR-TL',avg_mlrwt =  'MLR-TW', gbm_none = 'GBM', avg_gbm = 'GBM-T', 
                               avg_none = 'ENS', avgwt_none= 'ENS-W', avglwt_none = 'ENS-L', 
                               avg3 = 'ENS-T', avg3wt= 'ENS-TW', avg3lwt= 'ENS-TL' ))}
  else{
    r_out <- rnd_data %>% group_by(model_name) %>% summarise(
      T1 = round(mean(T1_test)*100,2),
      T2 = round(mean(T2_test)/days_round,2),
      T3 = round(mean(T3_test)/days_round,3)
    ) %>%
      mutate(model_name = recode(model_name, mlr_none = 'MLR', avg_mlr = 'MLR-T', mlrwt_none =  'MLR-W', mlrlwt_none =  'MLR-L',
                                 avg_mlrlwt =  'MLR-TL',avg_mlrwt =  'MLR-TW', gbm_none = 'GBM', avg_gbm = 'GBM-T', 
                                 avg_none = 'ENS', avgwt_none= 'ENS-W', avglwt_none = 'ENS-L', 
                                 avg3 = 'ENS-T', avg3wt= 'ENS-TW', avg3lwt= 'ENS-TL' ))
      }
  r_out %>% subset(model_name %in% report_names) %>% slice(order(factor(model_name, levels = report_names)))
  }

t1 <- score_table(rnd_1,1,59,reporting)
t1 <- t1 %>% rename("T1_R1"="T1","T2_R1" ="T2", "T3_R1" ="T3") 
t2 <- score_table(rnd_2,2,92,reporting)
t2 <- t2 %>% rename("T1_R2" ="T1","T2_R2" ="T2", "T3_R2" ="T3") 
t3 <- score_table(rnd_3,3,61,reporting)
t3 <- t3 %>% rename("T1_R3" ="T1","T2_R3" ="T2", "T3_R3" ="T3") 
t4 <- score_table(rnd_4,4,31,reporting)
t4 <- t4 %>% rename("T1_R4" ="T1","T2_R4" ="T2", "T3_R4" ="T3") 
t5 <- score_table(rnd_5,5,61,reporting)
t5 <- t5 %>% rename("T1_R5" ="T1","T2_R5" ="T2", "T3_R5" ="T3") 
t6 <- score_table(rnd_6,6,61,reporting)
t6 <- t6 %>% rename("T1_R6" ="T1","T2_R6" ="T2", "T3_R6" ="T3") 


Track_1_table <- merge(t1[c('model_name','T1_train','T1_R1')],t2[c('model_name','T1_R2')],by="model_name") %>%  
  merge(t3[c('model_name','T1_R3')],by="model_name")%>%  
  merge(t4[c('model_name','T1_R4')],by="model_name")%>%  
  merge(t5[c('model_name','T1_R5')],by="model_name")%>%  
  merge(t6[c('model_name','T1_R6')],by="model_name") %>% 
  subset(model_name %in% reporting) %>% slice(order(factor(model_name, levels = reporting)))
Track_1_table$avg = round(rowMeans(Track_1_table[,c('T1_R1','T1_R2','T1_R3','T1_R4','T1_R5','T1_R6')]),2)
write.csv(Track_1_table,"../data/results/Track1_results_paper.csv")


Track_2_table <- merge(t1[c('model_name','T2_train','T2_R1')],t2[c('model_name','T2_R2')],by="model_name") %>%  
  merge(t3[c('model_name','T2_R3')],by="model_name")%>%  
  merge(t4[c('model_name','T2_R4')],by="model_name")%>%  
  merge(t5[c('model_name','T2_R5')],by="model_name")%>%  
  merge(t6[c('model_name','T2_R6')],by="model_name") %>% 
  subset(model_name %in% reporting) %>% slice(order(factor(model_name, levels = reporting)))
Track_2_table$avg = round(rowMeans(Track_2_table[,c('T2_R1','T2_R2','T2_R3','T2_R4','T2_R5','T2_R6')]),2)
write.csv(Track_2_table,"../data/results/Track2_results_paper.csv")

Track_3_table <- merge(t1[c('model_name','T3_train','T3_R1')],t2[c('model_name','T3_R2')],by="model_name") %>%  
  merge(t3[c('model_name','T3_R3')],by="model_name")%>%  
  merge(t4[c('model_name','T3_R4')],by="model_name")%>%  
  merge(t5[c('model_name','T3_R5')],by="model_name")%>%  
  merge(t6[c('model_name','T3_R6')],by="model_name") %>% 
  subset(model_name %in% reporting) %>% slice(order(factor(model_name, levels = reporting))) 
Track_3_table$avg = round(rowMeans(Track_3_table[,c('T3_R1','T3_R2','T3_R3','T3_R4','T3_R5','T3_R6')]),3)
write.csv(Track_3_table,"../data/results/Track3_results_paper.csv")
                                           