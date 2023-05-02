




# load packages----
packages= c("openxlsx","dplyr", "tidyverse","knitr", "tidyr", "gtsummary",
            "fastDummies", "rstatix", "reshape2", "ggpubr", "flextable",
            "plotly","summarytools","rrtable","hablar",
            "gtable","egg", "gridExtra","grid","moderndive",
            "tinytex","lavaan","robustbase","robustlmm",
            "GGally","mice","effectsize", "RHRV")



invisible(lapply(packages, library, character.only = TRUE))     


# HRV preprocessing script----

#data 1 example----

# The process is repeated throughout all observations

#rest condition

hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)
hrv.data = LoadBeatRR(hrv.data, scale =  0.001, "1.txt")

hrv.data = BuildNIHR(hrv.data)
PlotNIHR(hrv.data)

hrv.data = FilterNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)


hrv.data1.time= CreateTimeAnalysis(hrv.data)

hrv.data1.freq = CreateFreqAnalysis(hrv.data)

hrv.data1.freq = CalculatePSD(hrv.data1.freq, indexFreqAnalysis = 1,
                              method = "lomb", doPlot = F)

hrv.data1.freq = InterpolateNIHR(hrv.data1.freq)

hrv.data1.freq = CreateFreqAnalysis(hrv.data1.freq)

hrv.data1.freq = CalculatePSD(hrv.data1.freq, indexFreqAnalysis = 2,
                              method = "ar", doPlot = F)

hrv.data1.freq = CreateFreqAnalysis(hrv.data1.freq)


hrv.data1.freq = CalculatePSD(hrv.data1.freq, indexFreqAnalysis = 3,
                              method = "pgram", doPlot = F)


hrv.data1.freq = CreateFreqAnalysis(hrv.data1.freq)


hrv.data1.freq = CalculatePowerBand(hrv.data1.freq,indexFreqAnalysis = 4, size = 600, shift = 30)


hrv.data1.freq = CreateFreqAnalysis(hrv.data1.freq)


hrv.data1.freq = CalculatePowerBand(hrv.data1.freq, indexFreqAnalysis = 5,type = "wavelet",bandtolerance = 0.01)
hrv.data1.freq_wavelet_HF=data.frame(hrv.data1.freq$FreqAnalysis[[5]]$HF)  


#stroop condition

hrv.datac = CreateHRVData()
hrv.datac = SetVerbose(hrv.datac, TRUE)
hrv.datac = LoadBeatRR(hrv.datac, scale =  0.001, "1.txt")

hrv.datac = BuildNIHR(hrv.datac)
PlotNIHR(hrv.datac)

hrv.datac = FilterNIHR(hrv.datac)
hrv.datac = FilterNIHR(hrv.datac)

hrv.data1.stroop = ExtractTimeSegment(hrv.datac, 59.660, 119.738)

hrv.data1.stroop= CreateTimeAnalysis(hrv.data1.stroop)

#recovery condition

hrv.datab = CreateHRVData()
hrv.datab = SetVerbose(hrv.datab, TRUE)
hrv.datab = LoadBeatRR(hrv.datab, scale =  0.001, "1.txt")

hrv.datab = BuildNIHR(hrv.datab)
PlotNIHR(hrv.datab)

hrv.datab = FilterNIHR(hrv.datab)
hrv.datab = FilterNIHR(hrv.datab)



hrv.data1.timeb= CreateTimeAnalysis(hrv.datab)

hrv.data1.freqb = CreateFreqAnalysis(hrv.datab)

hrv.data1.freqb = CalculatePSD(hrv.data1.freqb, indexFreqAnalysis = 1,
                               method = "lomb", doPlot = F)

hrv.data1.freqb = InterpolateNIHR(hrv.data1.freqb)

hrv.data1.freqb = CreateFreqAnalysis(hrv.data1.freqb)

hrv.data1.freqb = CalculatePSD(hrv.data1.freqb, indexFreqAnalysis = 2,
                               method = "ar", doPlot = F)

hrv.data1.freqb = CreateFreqAnalysis(hrv.data1.freqb)


hrv.data1.freqb = CalculatePSD(hrv.data1.freqb, indexFreqAnalysis = 3,
                               method = "pgram", doPlot = F)


hrv.data1.freqb = CreateFreqAnalysis(hrv.data1.freqb)


hrv.data1.freqb = CalculatePowerBand(hrv.data1.freqb,indexFreqAnalysis = 4, size = 600, shift = 30)


hrv.data1.freqb = CreateFreqAnalysis(hrv.data1.freqb)


hrv.data1.freqb = CalculatePowerBand(hrv.data1.freqb, indexFreqAnalysis = 5,type = "wavelet",bandtolerance = 0.01)
hrv.data1.freqb_wavelet_HF=data.frame(hrv.data1.freqb$FreqAnalysis[[5]]$HF)  



#Transform list in a data frame


hrv.data1.time_analisys=data.frame(hrv.data1.time$TimeAnalysis)
hrv.data1.FFT=data.frame(hrv.data1.freq$FreqAnalysis[[4]])

hrv.data1.time_analisysb=data.frame(hrv.data1.timeb$TimeAnalysis)
hrv.data1.FFTb=data.frame(hrv.data1.freqb$FreqAnalysis[[4]])

hrv.data1.stroop_analysis=data.frame(hrv.data1.stroop$TimeAnalysis)

#Join data frames

#rest
hrv.data1.time_analisys=hrv.data1.time_analisys %>% 
  select(rMSSD,SDNN,HRVi) %>% 
  add_column(id = c("1"),
             time = c("t1"),
             group = c("control"),
             condition = c ("rest"),
             Heart_Rate = c(71),
             Resp_Rate = c(16)) %>% 
  relocate(any_of(c("id", "time", "group", "condition", "Heart_Rate", "Resp_Rate"))) %>% 
  mutate_if(is.numeric, round, 2) 


#stroop
hrv.data1.stroop_analysis=hrv.data1.stroop_analysis %>% 
  select(rMSSD,SDNN,HRVi) %>% 
  add_column(id = c("1"),
             time = c("t1"),
             group = c("control"),
             condition = c ("stroop"),
             Heart_Rate = c(70),
             Resp_Rate = c(21)) %>% 
  relocate(any_of(c("id", "time", "group", "condition", "Heart_Rate", "Resp_Rate"))) %>% 
  mutate_if(is.numeric, round, 2) 

#recovery
hrv.data1.time_analisysb=hrv.data1.time_analisysb %>% 
  select(rMSSD,SDNN,HRVi) %>% 
  add_column(id = c("1"),
             time = c("t1"),
             group = c("control"),
             condition = c ("recovery"),
             Heart_Rate = c(71),
             Resp_Rate = c(19)) %>% 
  relocate(any_of(c("id", "time", "group", "condition", "Heart_Rate", "Resp_Rate"))) %>% 
  mutate_if(is.numeric, round, 2) 



#rest
hrv.data1.FFT=hrv.data1.FFT %>% 
  select(HRV,VLF,LF,HF,LFHF,LFmin,LFmax,HFmin,HFmax) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(power_nu=(HF+LF)) %>%
  mutate(HF_nu=(HF/power_nu), LF_nu=(LF/power_nu)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(LF_FFT=LF, HF_FFT=HF, LFHF_FFT=LFHF, 
         LFmin_FFT=LFmin, LFmax_FFT=LFmax,
         HFmin_FFT=HFmin, HFmax_FFT=HFmax)


#recovery
hrv.data1.FFTb=hrv.data1.FFTb %>% 
  select(HRV, VLF, LF,HF,LFHF,LFmin,LFmax,HFmin,HFmax) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(power_nu=(HF+LF)) %>%
  mutate(HF_nu=(HF/power_nu), LF_nu=(LF/power_nu)) %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(LF_FFT=LF, HF_FFT=HF, LFHF_FFT=LFHF, 
         LFmin_FFT=LFmin, LFmax_FFT=LFmax,
         HFmin_FFT=HFmin, HFmax_FFT=HFmax)


#Join df rest and recovery analysis

#rest
hrv.data1.df=hrv.data1.time_analisys %>% 
  bind_cols(hrv.data1.FFT)

#recovery
hrv.data1.dfb=hrv.data1.time_analisysb %>% 
  bind_cols(hrv.data1.FFTb)

#rest, recovery and stroop

hrv.data1.dfab=hrv.data1.df %>% 
  bind_rows(hrv.data1.stroop_analysis, hrv.data1.dfb)




# Data frame----

cog.HRV=read.xlsx("cog.HRV.xlsx")

cog.HRV=cog.HRV %>% 
  mutate(HF_FFT_t1_rest_s = scale(HF_FFT_t1_rest), HF_FFT_t2_rest_s = scale(HF_FFT_t2_rest),
         rMSSD_t1_recovery_s = scale(rMSSD_t1_recovery), rMSSD_t2_recovery_s = scale(rMSSD_t2_recovery),
         flanker_total_accuracy_t1_s = scale(flanker_total_accuracy_t1),
         flanker_total_accuracy_t2_s = scale(flanker_total_accuracy_t2),
         stroop_time_W_t1_s = scale(stroop_time_W_t1),stroop_time_W_t2_s = scale(stroop_time_W_t2),
         stroop_time_C_t1_s = scale(stroop_time_C_t1),stroop_time_C_t2_s = scale(stroop_time_C_t2))

# Variables distribution----

# HRV

cog.HRV %>%  
  select(group,rMSSD_t1_rest, rMSSD_t1_stroop, rMSSD_t1_recovery,
         HF_FFT_t1_rest, HF_FFT_t1_recovery,
         rMSSD_t2_rest, rMSSD_t2_stroop, rMSSD_t2_recovery,
         HF_FFT_t2_rest, HF_FFT_t2_recovery) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())



# Attention


cog.HRV %>%  
  select(group, flanker_total_accuracy_t1,flanker_total_accuracy_t2,
         flanker_mean_response_t1,flanker_mean_response_t2,
         flanker_conflict_cost_t1,flanker_conflict_cost_t2,
         stroop_time_W_t1,stroop_time_W_t2,
         stroop_time_C_t1,stroop_time_C_t2,total_stroop_error_t1,
         total_stroop_error_t2,stroop_interf_t1,stroop_interf_t2 ) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

# Descriptive statistics----

cog.HRV %>% 
  select(-id) %>% 
  tbl_summary(by = group, missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_n(last = "TRUE")

cog.HRV %>% 
  select(-id) %>% 
  tbl_summary(by = group, missing = "no",
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})")) %>% 
  add_n(last = "TRUE")


# Test efficiency of the cognitive stress induction----
cog.HRV %>%
  select(id, group,rMSSD_t1_stroop,rMSSD_t1_rest ) %>% 
  pivot_longer(cols = c("rMSSD_t1_stroop","rMSSD_t1_rest"), names_to = "rmssd" ,values_to = "value") %>% 
  group_by(rmssd) %>% 
  drop_na() %>% 
  summarise(mean=mean(value), sd=sd(value))


cog.HRV %>%
  select(id, group,rMSSD_t1_stroop,rMSSD_t1_rest ) %>% 
  pivot_longer(cols = c("rMSSD_t1_stroop","rMSSD_t1_rest"), names_to = "rmssd" ,values_to = "value") %>% 
  wilcox_test(value~rmssd, paired = TRUE, detailed = TRUE) 


cog.HRV %>%
  select(id, group,rMSSD_t1_stroop,rMSSD_t1_rest ) %>% 
  pivot_longer(cols = c("rMSSD_t1_stroop","rMSSD_t1_rest"), names_to = "rmssd" ,values_to = "value") %>% 
  t_test(value~rmssd, paired = TRUE, detailed = TRUE) 



# Multiple Linear Regression Analyses----

#change depression level order

cog.HRV$depression = factor(cog.HRV$depression, levels = c("yes","no"))  

#Rest condition----

#RMSSD----
HRV_rest=lm(rMSSD_t2_rest~rMSSD_t1_rest+group, data = cog.HRV) 
summary(HRV_rest)
HRV_rest.tab=tidy(HRV_rest, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.33, term = "RMSSD Rest") 

cohens_f_squared(HRV_rest)    

par(mfrow=c(2,2))
plot(HRV_rest)


#RMSSD adj depression 


HRV_rest.dep=lm(rMSSD_t2_rest~rMSSD_t1_rest+group+depression, data = cog.HRV) 
summary(HRV_rest.dep)
HRV_rest.tab.dep=tidy(HRV_rest.dep, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.31, term = "RMSSD Rest") 


par(mfrow=c(2,2))
plot(  HRV_rest.dep)

cohens_f_squared(HRV_rest.dep)  


#HF----

HRV_rest.HF=lm(HF_FFT_t2_rest~HF_FFT_t1_rest+group, data = cog.HRV) 
summary(HRV_rest.HF)
cohens_f_squared(HRV_rest.HF)

HRV_rest.HF.tab=tidy(  HRV_rest.HF, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.42, term = "HF Rest", f2 = 0.16) 


par(mfrow=c(2,2))
plot(  HRV_rest.HF)



#HF adj depression  
HRV_rest.HF.dep=lm(HF_FFT_t2_rest~HF_FFT_t1_rest+group+depression, data = cog.HRV) 
summary(HRV_rest.HF.dep)
cohens_f_squared(HRV_rest.HF.dep)

HRV_rest.HF.dep.tab=tidy(HRV_rest.HF.dep, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.45, term = "HF Rest", f2 =0.22) 


par(mfrow=c(2,2))
plot(HRV_rest.HF.dep)


# Stroop condition----

#RMSSD----
HRV_stroop=lm(rMSSD_t2_stroop~rMSSD_t1_stroop+group, data = cog.HRV) 
summary(HRV_stroop)
HRV_stroop.tab=tidy(HRV_stroop, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.33, term = "RMSSD stroop") 


par(mfrow=c(2,2))
plot(HRV_stroop)


#RMSSD adj depression
HRV_stroop.dep=lm(rMSSD_t2_stroop~rMSSD_t1_stroop+group+depression, data = cog.HRV) 
summary(HRV_stroop.dep)
HRV_stroop.dep.tab=tidy(HRV_stroop.dep, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.37, term = "RMSSD stroop") 


par(mfrow=c(2,2))
plot(HRV_stroop)


# Recovery condition----


# RMSSD----

HRV_recovery=lm(rMSSD_t2_recovery~rMSSD_t1_recovery+group, data = cog.HRV) 
summary(HRV_recovery)

cohens_f_squared(HRV_recovery)  

HRV_recovery.tab=tidy(HRV_recovery, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.36, term = "RMSSD recovery", f2 =0.14) 


par(mfrow=c(2,2))
plot(HRV_recovery)



# RMSSD adj depression

HRV_recovery.dep=lm(rMSSD_t2_recovery~rMSSD_t1_recovery+group+depression, data = cog.HRV) 
summary(HRV_recovery.dep)

cohens_f_squared(HRV_recovery.dep) 

HRV_recovery.dep.tab=tidy(HRV_recovery.dep, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.31, term = "RMSSD recovery", f2 =0.14) 


par(mfrow=c(2,2))
plot(HRV_recovery)


#HF----
HRV_HF.recovery=lmrob(HF_FFT_t2_recovery~HF_FFT_t1_recovery+group, data = cog.HRV) 
summary(HRV_HF.recovery)
HRV_HF.recovery.tab=tidy(HRV_HF.recovery, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.45, term = "HF recovery") 


par(mfrow=c(2,2))
plot(HRV_HF.recovery)


#HF adj depression

HRV_HF.recovery.dep=lmrob(HF_FFT_t2_recovery~HF_FFT_t1_recovery+group+depression, data = cog.HRV) 
summary(HRV_HF.recovery.dep)
HRV_HF.recovery.dep.tab=tidy(HRV_HF.recovery.dep, conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.45, term = "HF recovery") 


par(mfrow=c(2,2))
plot(HRV_HF.recovery)


#Flanker accuracy----

flk_accu=lm(flanker_total_accuracy_t2~flanker_total_accuracy_t1+group, data = cog.HRV) 
summary(flk_accu)
cohens_f_squared(flk_accu)

flk_accu.tab=tidy(flk_accu, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.22, term = "Flanker accuracy", f2 =0.10) 


par(mfrow=c(2,2))
plot(flk_accu)


# Flanker accuracy adj depression


flk_accu.dep=lm(flanker_total_accuracy_t2~flanker_total_accuracy_t1+group+depression, data = cog.HRV) 
summary(flk_accu.dep)
cohens_f_squared(flk_accu.dep)

flk_accu.dep.tab=tidy(flk_accu.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.25, term = "Flanker accuracy", f2 =0.10) 


par(mfrow=c(2,2))
plot(flk_accu.dep)


# Flanker congruent----


flk_congruent=lmrob(flanker_mean_congruent_t2~flanker_mean_congruent_t1+group, data = cog.HRV) 
summary(flk_congruent)


flk_congruent.tab=tidy(flk_congruent, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.61, term = "Flanker congruent") 

cohens_f_squared(flk_congruent) 

#Flanker congruent adj depression

flk_congruent.dep=lmrob(flanker_mean_congruent_t2~flanker_mean_congruent_t1+group+depression, data = cog.HRV) 
summary(flk_congruent.dep)

flk_congruent.dep.tab=tidy(flk_congruent.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.60, term = "Flanker congruent") 



#Flanker incongruent----


flk_incongruent=lm(flanker_mean_incongruent_t2~flanker_mean_incongruent_t1+group, data = cog.df) 
summary(flk_incongruent)


flk_incongruent.tab=tidy(flk_incongruent, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.54, term = "Flanker incongruent") 


cohens_f_squared(flk_incongruent) 

#Flanker incongruent adj depression----

flk_incongruent.dep=lmrob(flanker_mean_incongruent_t2~flanker_mean_incongruent_t1+group+depression, data = cog.df) 
summary(flk_incongruent.dep)

flk_incongruent.dep.tab=tidy(flk_incongruent.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.54, term = "Flanker incongruent") 

#Flanker mean response----

sort(cog.df$flanker_mean_response_t2)
flk_mean=lmrob(flanker_mean_response_t2~flanker_mean_response_t1+group, data = cog.df) 
summary(flk_mean)

flk_mean.tab=tidy(flk_mean, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.65, term = "Flanker mean") 


par(mfrow=c(2,2))
plot(flk_mean)

#Flanker mean response adj depression----

flk_mean.dep=lmrob(flanker_mean_response_t2~flanker_mean_response_t1+group+depression, data = cog.df) 
summary(flk_mean.dep)

flk_mean.dep.tab=tidy(flk_mean.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.65, term = "Flanker mean") 


par(mfrow=c(2,2))
plot(flk_mean.dep)

#follow-up

flk_mean2=lm(flanker_mean_response_t3~flanker_mean_response_t1+group, data = cog.df2[-c(49),]) 
summary(flk_mean2)

par(mfrow=c(2,2))
plot(flk_mean2)

#Flanker conflict cost (incongruent mean - congruent mean)----
#Positive numbers means higher cost


sort(cog.df$flanker_conflict_cost_t2)

flk_conflict=lmrob(flanker_conflict_cost_t2~flanker_conflict_cost_t1+group, data = cog.df) 
summary(flk_conflict)

flk_conflict.tab=tidy(flk_conflict, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.11, term = "Flanker conflict cost") 

par(mfrow=c(2,2))
plot(flk_conflict)


cohens_f_squared(flk_conflict) 

#T test
cog.df %>% 
  select(id, group,flanker_conflict_cost_t2 ) %>% 
  t_test(flanker_conflict_cost_t2~group ,detailed = TRUE, ref.group = "control") 

#Flanker conflict cost adj depression----

flk_conflict.dep=lmrob(flanker_conflict_cost_t2~flanker_conflict_cost_t1+group+depression, data = cog.df) 
summary(flk_conflict.dep)

flk_conflict.dep.tab=tidy(flk_conflict.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.05, term = "Flanker conflict cost") 

par(mfrow=c(2,2))
plot(flk_conflict.dep)

#Stroop---- 

#time D----

#post
cog.df=cog.df%>% 
  convert(num(stroop_time_D_t1, stroop_time_D_t2))

stroop_D=lm(stroop_time_D_t2~stroop_time_D_t1+group, data = cog.df[-c(16),]) 
summary(stroop_D)

stroop_D.tab=tidy(stroop_D, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.46, term = "Stroop dot") 


par(mfrow=c(2,2))
plot(stroop_D)

cohens_f_squared(stroop_D) 

#time D adj depression----

stroop_D.dep=lm(stroop_time_D_t2~stroop_time_D_t1+group+depression, data = cog.df[-c(16),]) 
summary(stroop_D.dep)

stroop_D.dep.tab=tidy(stroop_D.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.45, term = "Stroop dot") 


par(mfrow=c(2,2))
plot(stroop_D.dep)


#time W----
cog.df=cog.df%>% 
  convert(num(stroop_time_W_t1, stroop_time_W_t2))

stroop_W=lm(stroop_time_W_t2~stroop_time_W_t1+group, data = cog.df) 
summary(stroop_W)
cohens_f_squared(stroop_W)

stroop_W.tab=tidy(stroop_W, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.83, term = "Stroop words", f2= 0.10) 

par(mfrow=c(2,2))
plot(stroop_W)

cohens_f_squared(stroop_W) 

#time W adj depression----


stroop_W.dep=lm(stroop_time_W_t2~stroop_time_W_t1+group+depression, data = cog.df) 
summary(stroop_W.dep)
cohens_f_squared(stroop_W.dep)

stroop_W.dep.tab=tidy(stroop_W.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.82, term = "Stroop words", f2= 0.10) 

par(mfrow=c(2,2))
plot(stroop_W.dep)


#conflict response----


stroop_c=lm(stroop_time_C_t2~stroop_time_C_t1+group, data = cog.df) 
summary(stroop_c)

stroop_c.tab=tidy(stroop_c, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.83, term = "Stroop conflict") 

par(mfrow=c(2,2))
plot(stroop_c)

cohens_f_squared(stroop_c)

#conflict response adj depression----


stroop_c.dep=lm(stroop_time_C_t2~stroop_time_C_t1+group+depression, data = cog.df) 
summary(stroop_c.dep)
cohens_f_squared(stroop_c.dep)

stroop_c.dep.tab=tidy(stroop_c.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.63, term = "Stroop conflict", f2=0.07) 

par(mfrow=c(2,2))
plot(stroop_c.dep)



#stroop Interference----


stroop_interf=lmrob(stroop_interf_t2~stroop_interf_t1+group, data = cog.df) 
summary(stroop_interf)

stroop_interf.tab=tidy(stroop_interf, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.21, term = "Stroop interference") 


par(mfrow=c(2,2))
plot(stroop_interf)


cohens_f_squared(stroop_c)

#adj depression----

stroop_interf.dep=lmrob(stroop_interf_t2~stroop_interf_t1+group+depression, data = cog.df) 
summary(stroop_interf.dep)

stroop_interf.dep.tab=tidy(stroop_interf.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.16, term = "Stroop interference") 


par(mfrow=c(2,2))
plot(stroop_D.dep)



#error total response----

sort(cog.df$intrusions_t2)
stroop_t_error=lm(total_stroop_error_t2~total_stroop_error_t1+group, data = cog.df[-c(31),]) 
summary(stroop_t_error)


stroop_t_error.tab=tidy(stroop_t_error, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.47, term = "Stroop total error") 

par(mfrow=c(2,2))
plot(stroop_t_error)

cohens_f_squared(stroop_t_error)
#error total response adj depression----


stroop_t_error.dep=lm(total_stroop_error_t2~total_stroop_error_t1+group+depression, data = cog.df[-c(31),]) 
summary(stroop_t_error.dep)


stroop_t_error.dep.tab=tidy(stroop_t_error.dep, conf.int = TRUE) %>% 
  filter(term=="group_intervention") %>% 
  mutate(r2.adj=0.47, term = "Stroop total error") 

par(mfrow=c(2,2))
plot(stroop_t_error.dep)


#TABLE COGNITIVE TASKS----


cog.tab=flk_accu.tab  %>%   
  bind_rows(flk_mean.tab, flk_conflict.tab, stroop_D.tab,
            stroop_W.tab, stroop_c.tab, stroop_t_error.tab)%>% 
  mutate_if(is.numeric, round,2) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  autofit() %>% 
  save_as_docx( path = "cog_models_tab.docx")



cog.tab.dep=flk_accu.dep.tab  %>%   
  bind_rows(flk_mean.dep.tab, flk_conflict.dep.tab, stroop_D.dep.tab,
            stroop_W.dep.tab, stroop_c.dep.tab, stroop_t_error.dep.tab)%>% 
  mutate_if(is.numeric, round,2) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  autofit() %>% 
  save_as_docx( path = "cog_models_tab.dep.docx")







#percentage of change----

perc.response=cog.HRV %>% 
  select(id, group,stroop_time_W_t2,stroop_time_W_t1,
         rMSSD_t1_rest,rMSSD_t2_rest,
         rMSSD_t1_stroop,rMSSD_t2_stroop,
         rMSSD_t2_recovery,rMSSD_t1_recovery,
         HF_FFT_t1_rest,HF_FFT_t2_rest, 
         stroop_time_C_t2,
         stroop_time_C_t1,flanker_total_accuracy_t2,flanker_total_accuracy_t1) %>% 
  mutate (stroop.change =  stroop_time_W_t2 - stroop_time_W_t1,
          stroop.change.inc = stroop_time_C_t2 - stroop_time_C_t1,
          flk.change = flanker_total_accuracy_t2 - flanker_total_accuracy_t1,
          RMSSD.rest.change = rMSSD_t2_rest - rMSSD_t1_rest,
          RMSSD.change = rMSSD_t2_recovery - rMSSD_t1_recovery,
          HF.change = HF_FFT_t2_rest - HF_FFT_t1_rest,
          RMSSD.stroop.change = rMSSD_t2_stroop- rMSSD_t1_stroop)


#stroop----
perc.stroop=perc.response %>% 
  select(id, group,stroop.change) %>% 
  mutate(p.response.stroop = case_when(stroop.change >  0 ~ "no_response",
                                       stroop.change < 0 ~ "response")) %>% 
  group_by(p.response.stroop,group)

perc.stroop.c=perc.response %>% 
  select(id, group,stroop.change.inc) %>% 
  mutate(p.response.stroop.inc = case_when(stroop.change.inc >  0 ~ "no_response",
                                           stroop.change.inc < 0 ~ "response")) %>% 
  group_by(p.response.stroop.inc,group)




perc.stroop %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 2)) %>% 
  filter(p.response.stroop%in% c("response")) %>% 
  select(group,freq) %>% 
  pivot_wider(names_from = group, values_from = freq) 



#flanker----
perc.flk=perc.response %>% 
  select(id, group,flk.change) %>% 
  mutate(p.response.flk = case_when(flk.change <=  0 ~ "no_response",
                                    flk.change > 0 ~ "response")) %>% 
  group_by(p.response.flk,group)

perc.flk %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 2)) %>% 
  filter(p.response.flk%in% c("response")) %>% 
  select(group,freq) %>% 
  pivot_wider(names_from = group, values_from = freq) 
#RMSSD----  

perc.rmssd=perc.response %>% 
  select(id,group,RMSSD.change) %>% 
  mutate(p.response.rmssd = case_when(RMSSD.change >  0 ~ "response",
                                      RMSSD.change <= 0 ~ "no_response")) %>% 
  group_by(p.response.rmssd,group)


#RMSSD stroop----  

perc.rmssd.stroop=perc.response %>% 
  select(id, group,RMSSD.stroop.change) %>% 
  mutate(p.response.rmssd.stroop = case_when(RMSSD.stroop.change >  0 ~ "response",
                                             RMSSD.stroop.change <= 0 ~ "no_response")) %>% 
  group_by(p.response.rmssd.stroop,group)


#Rest HF----

perc.HF=perc.response %>% 
  select(id, group,HF.change) %>% 
  mutate(p.response.HF = case_when(HF.change >  0 ~ "response",
                                   HF.change <= 0 ~ "no_response")) %>% 
  group_by(p.response.HF,group)


#mean by group of change----

perc.change=perc.flk%>%
  full_join(perc.stroop, by = c("id", "group")) %>% 
  full_join(perc.stroop.c, by = c("id", "group")) %>% 
  full_join(perc.rmssd, by = c("id", "group")) %>% 
  full_join(perc.HF, by = c("id", "group")) %>% 
  full_join(perc.rmssd.stroop, by = c("id", "group"))


#mean HRV by flk----

perc.change %>% 
  filter(p.response.flk=="response") %>% 
  select(id,group, p.response.flk, RMSSD.change,RMSSD.stroop.change, HF.change) %>% 
  drop_na() %>% 
  group_by(group,p.response.flk) %>% 
  summarise(mean.RMSSD = mean(RMSSD.change), 
            mean.RMSSD.stroop = mean(RMSSD.stroop.change),
            mean.HF = mean(HF.change)) 

#mean HRV by stroop W----

perc.change %>% 
  filter(p.response.stroop=="response") %>% 
  select(id,group, p.response.stroop, RMSSD.change,RMSSD.stroop.change, HF.change) %>% 
  drop_na() %>% 
  group_by(group,p.response.stroop) %>% 
  summarise(mean.RMSSD = mean(RMSSD.change), 
            mean.RMSSD.stroop = mean(RMSSD.stroop.change),
            mean.HF = mean(HF.change))


#stroop c----

perc.change %>% 
  filter(p.response.stroop.inc=="response") %>% 
  select(id,group, p.response.stroop.inc, RMSSD.change,RMSSD.stroop.change, HF.change) %>% 
  drop_na() %>% 
  group_by(group,p.response.stroop.inc) %>% 
  summarise(mean.RMSSD = mean(RMSSD.change), 
            mean.RMSSD.stroop = mean(RMSSD.stroop.change),
            mean.HF = mean(HF.change))


#MEDIATOR ANALYSIS----

#MBI-->Stroop w--->RMSSD recovery


MIB_stroop_RMSSD1= '

# Path c

rMSSD_t2_recovery_s~c*group+rMSSD_t1_recovery_s


# Path a

stroop_time_W_t2_s~a*group+stroop_time_W_t1_s


# Path b

rMSSD_t2_recovery_s~b*stroop_time_W_t2_s+stroop_time_W_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_stroop_RMSSD.mod1=sem(MIB_stroop_RMSSD1, cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_stroop_RMSSD.mod1=summary(MIB_stroop_RMSSD.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_stroop_RMSSD.mod.par1=parameterEstimates(MIB_stroop_RMSSD.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")



MIB_stroop_RMSSD.index.mod.stand1=standardizedSolution(MIB_stroop_RMSSD.mod1)


#MBI-->Flanker accurary--->RMSSD recovery

MIB_flk_RMSSD1= '

# Path c

rMSSD_t2_recovery_s~c*group+rMSSD_t1_recovery_s


# Path a

flanker_total_accuracy_t2_s~a*group+flanker_total_accuracy_t1_s


# Path b

rMSSD_t2_recovery_s~b*flanker_total_accuracy_t2_s+flanker_total_accuracy_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_flk_RMSSD.mod1=sem(MIB_flk_RMSSD1, cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_flk_RMSSD.mod1=summary(MIB_flk_RMSSD.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_flk_RMSSD.mod.par1=parameterEstimates(MIB_flk_RMSSD.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")



MIB_flk_RMSSD.index.mod.stand1=standardizedSolution(MIB_flk_RMSSD.mod1)



#MBI-->Stroop W--->Rest HF


MIB_stroop_HF1= '

# Path c

HF_FFT_t2_rest_s~c*group+HF_FFT_t1_rest_s


# Path a

stroop_time_W_t2_s~a*group+stroop_time_W_t1_s


# Path b

HF_FFT_t2_rest_s~b*stroop_time_W_t2_s+stroop_time_W_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_stroop_HF.mod1=sem(MIB_stroop_HF1, cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_stroop_HF.mod1=summary(MIB_stroop_HF.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_stroop_HF.mod.par1=parameterEstimates(MIB_stroop_HF.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")


#MBI-->Flanker accuracy-->Rest HF


MIB_flk_HF1= '

# Path c

HF_FFT_t2_rest_s~c*group+HF_FFT_t1_rest_s


# Path a

flanker_total_accuracy_t2_s~a*group+flanker_total_accuracy_t1_s


# Path b

HF_FFT_t2_rest_s~b*flanker_total_accuracy_t2_s+flanker_total_accuracy_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_flk_HF.mod1=sem(MIB_flk_HF1, cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_flk_HF.mod1=summary(MIB_flk_HF.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_flk_HF.mod.par1=parameterEstimates(MIB_flk_HF.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")

#MBI-->Stroop W--->Affective Pain


MIB_stroop_pain.affect1= '


# Path c

pain.affective_t2_s~c*group+pain.affective_t1_s


# Path a

stroop_time_W_t2_s~a*group+stroop_time_W_t1_s


# Path b

pain.affective_t2_s~b*stroop_time_W_t2_s+stroop_time_W_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_stroop_pain.affect.mod1=sem(MIB_stroop_pain.affect1,cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_stroop_pain.affect.mod1=summary(MIB_stroop_pain.affect.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_stroop_pain.affect.mod.par1=parameterEstimates(MIB_stroop_pain.affect.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")

#MBI-->Flanker accuracy-->Affective Pain


MIB_flk_pain.affect1= '

# Path c

pain.affective_t2_s~c*group+pain.affective_t1_s


# Path a

flanker_total_accuracy_t2_s~a*group+flanker_total_accuracy_t1_s


# Path b

pain.affective_t1_s~b*flanker_total_accuracy_t2_s+flanker_total_accuracy_t1_s



ab:=a*b

'

# Fit/estimate the model
set.seed(2021)

MIB_flk_pain.affect.mod1=sem(MIB_flk_pain.affect1, cog.HRV, se="bootstrap", bootstrap= 2000)

# Summarize the results/output

sum.MIB_flk_pain.affect.mod1=summary(MIB_flk_pain.affect.mod1, fit.measures=TRUE, standardized= TRUE, rsquare= TRUE)


MIB_flk_pain.affect.mod.par1=parameterEstimates(MIB_flk_pain.affect.mod1, ci=TRUE, level=0.95, boot.ci.type= "perc")








