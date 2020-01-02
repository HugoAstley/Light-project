#####Phase shift#####



##The existing models, but on region 1 specifically to account for solar noon; sunset; and sunrise time
#make a list of the procedure/diagnoses/primary diagnosis for region 1 specifically
for(i in 1:length(ICD_9_code_groups)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]] <- primary.diagnosis.ICD_9_code_groups.REGION1[[i]] %>% left_join(primary.diagnosis.ICD_9_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups.REGION1[[i]] <- ICD_9_code_groups.REGION1[[i]] %>% left_join(ICD_9_code_groups.admission.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}
for(i in 1:length(procedure_groups)){procedure_groups.REGION1[[i]] <- procedure_groups.REGION1[[i]] %>% left_join(procedure_groups.counts[[i]], by= c("SVYEAR", "ADM_MON", "REGION"))}

save(ICD_9_code_groups.REGION1, file= "ICD_9_code_groups.REGION1.RData")
#The models for each



#####The model for phase: all diagnoses#####

#Convert the catagorical variables into factors
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$RACE <- factor(ICD_9_code_groups.REGION1[[i]]$RACE)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$OWNER <- factor(ICD_9_code_groups.REGION1[[i]]$OWNER)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$SEX <- factor(ICD_9_code_groups.REGION1[[i]]$SEX)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$ESOP1 <- factor(ICD_9_code_groups.REGION1[[i]]$ESOP1)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$ADM_TYPE <- factor(ICD_9_code_groups.REGION1[[i]]$ADM_TYPE)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$ASOURCE <- factor(ICD_9_code_groups.REGION1[[i]]$ASOURCE)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$DISCSTAT <- factor(ICD_9_code_groups.REGION1[[i]]$DISCSTAT)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$MARSTAT <- factor(ICD_9_code_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$MARSTAT <- factor(ICD_9_code_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(ICD_9_code_groups.REGION1)){ICD_9_code_groups.REGION1[[i]]$REGION <- factor(ICD_9_code_groups.REGION1[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output <- list()

#The model. Multiple regression.
for(i in 1:86){model_output_all_diagnoses_REGION1.DOC[[i]] <- lm(ICD_9_code_groups.REGION1[[i]]$DOC ~ ICD_9_code_groups.REGION1[[i]]$n + 
                                                                   ICD_9_code_groups.REGION1[[i]]$AGE +
                                                                   ICD_9_code_groups.REGION1[[i]]$SEX + 
                                                                   ICD_9_code_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                                   ICD_9_code_groups.REGION1[[i]]$SVYEAR + 
                                                                   ICD_9_code_groups.REGION1[[i]]$BEDSIZE +
                                                                   ICD_9_code_groups.REGION1[[i]]$RACE+
                                                                   ICD_9_code_groups.REGION1[[i]]$OWNER+
                                                                   ICD_9_code_groups.REGION1[[i]]$ESOP1+
                                                                   ICD_9_code_groups.REGION1[[i]]$MARSTAT+
                                                                   ICD_9_code_groups.REGION1[[i]]$REGION+
                                                                   ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                                   ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                                   ICD_9_code_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                                   ICD_9_code_groups.REGION1[[i]]$KT+
                                                                   ICD_9_code_groups.REGION1[[i]]$PRECTOT+
                                                                   ICD_9_code_groups.REGION1[[i]]$PS+
                                                                   ICD_9_code_groups.REGION1[[i]]$QV2M+
                                                                   ICD_9_code_groups.REGION1[[i]]$RH2M+
                                                                   ICD_9_code_groups.REGION1[[i]]$T2M+
                                                                   ICD_9_code_groups.REGION1[[i]]$T2MWET+
                                                                   ICD_9_code_groups.REGION1[[i]]$WS2M+
                                                                   ICD_9_code_groups.REGION1[[i]]$mean.pollution
                                                                 ICD_9_code_groups.REGION1[[i]]$mean_solar_noon+
                                                                   ICD_9_code_groups.REGION1[[i]]$mean_sunrise_time+
                                                                   ICD_9_code_groups.REGION1[[i]]$mean_sunset_time+
                                                                   ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                   ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                   ICD_9_code_groups.REGION1[[i]]$sd_sunset_time)}


for(i in 1:86){model_output_all_diagnoses_REGION1.n[[i]] <- lm(ICD_9_code_groups.REGION1[[i]]$DOC ~ ICD_9_code_groups.REGION1[[i]]$n + 
                                                                 ICD_9_code_groups.REGION1[[i]]$AGE +
                                                                 ICD_9_code_groups.REGION1[[i]]$SEX + 
                                                                 ICD_9_code_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                                 ICD_9_code_groups.REGION1[[i]]$SVYEAR + 
                                                                 ICD_9_code_groups.REGION1[[i]]$BEDSIZE +
                                                                 ICD_9_code_groups.REGION1[[i]]$RACE+
                                                                 ICD_9_code_groups.REGION1[[i]]$OWNER+
                                                                 ICD_9_code_groups.REGION1[[i]]$ESOP1+
                                                                 ICD_9_code_groups.REGION1[[i]]$MARSTAT+
                                                                 ICD_9_code_groups.REGION1[[i]]$REGION+
                                                                 ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                                 ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                                 ICD_9_code_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                                 ICD_9_code_groups.REGION1[[i]]$KT+
                                                                 ICD_9_code_groups.REGION1[[i]]$PRECTOT+
                                                                 ICD_9_code_groups.REGION1[[i]]$PS+
                                                                 ICD_9_code_groups.REGION1[[i]]$QV2M+
                                                                 ICD_9_code_groups.REGION1[[i]]$RH2M+
                                                                 ICD_9_code_groups.REGION1[[i]]$T2M+
                                                                 ICD_9_code_groups.REGION1[[i]]$T2MWET+
                                                                 ICD_9_code_groups.REGION1[[i]]$WS2M+
                                                                 ICD_9_code_groups.REGION1[[i]]$mean.pollution
                                                               ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                 ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                 ICD_9_code_groups.REGION1[[i]]$sd_sunset_time)}


#####The model for phase: primary diagnoses#####
#Convert the catagorical variables into factors
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RACE <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RACE)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$OWNER <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$OWNER)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SEX <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SEX)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ESOP1 <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ESOP1)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ADM_TYPE <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ADM_TYPE)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ASOURCE <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ASOURCE)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$DISCSTAT <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$DISCSTAT)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(primary.diagnosis.ICD_9_code_groups.REGION1)){primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$REGION <- factor(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output <- list()

#The model. Multiple regression.
for(i in 1:86){model_output_primary_diagnosis_REGION1.DOC[[i]] <- lm(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$DOC ~ primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$n + 
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$AGE +
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SEX + 
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SVYEAR + 
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$BEDSIZE +
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RACE+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$OWNER+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ESOP1+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$REGION+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$KT+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$PRECTOT+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$PS+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$QV2M+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RH2M+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2M+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2M_RANGE+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2MWET+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$WS2M+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean.pollution+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_solar_noon+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_sunrise_time+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_sunset_time+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                       primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time)}

for(i in 1:86){model_output_primary_diagnosis_REGION1.n[[i]] <- lm(primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$DOC ~ primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$n + 
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$AGE +
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SEX + 
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$SVYEAR + 
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$BEDSIZE +
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RACE+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$OWNER+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ESOP1+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$MARSTAT+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$REGION+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$KT+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$PRECTOT+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$PS+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$QV2M+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$RH2M+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2M+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2M_RANGE+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$T2MWET+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$WS2M+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean.pollution+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_solar_noon+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_sunrise_time+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$mean_sunset_time+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time+
                                                                     primary.diagnosis.ICD_9_code_groups.REGION1[[i]]$sd_sunset_time)}


#####Model for phase:procedures#####
#Convert the catagorical variables into factors
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$RACE <- factor(procedure_groups.REGION1[[i]]$RACE)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$OWNER <- factor(procedure_groups.REGION1[[i]]$OWNER)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$SEX <- factor(procedure_groups.REGION1[[i]]$SEX)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$ESOP1 <- factor(procedure_groups.REGION1[[i]]$ESOP1)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$ADM_TYPE <- factor(procedure_groups.REGION1[[i]]$ADM_TYPE)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$ASOURCE <- factor(procedure_groups.REGION1[[i]]$ASOURCE)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$DISCSTAT <- factor(procedure_groups.REGION1[[i]]$DISCSTAT)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$MARSTAT <- factor(procedure_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$MARSTAT <- factor(procedure_groups.REGION1[[i]]$MARSTAT)}
for(i in 1:length(procedure_groups.REGION1)){procedure_groups.REGION1[[i]]$REGION <- factor(procedure_groups.REGION1[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output <- list()

#The model. Multiple regression.
for(i in 1:86){model_output_procedures_REGION1.DOC[[i]] <- lm(procedure_groups.REGION1[[i]]$DOC ~ procedure_groups.REGION1[[i]]$n + 
                                                                procedure_groups.REGION1[[i]]$AGE +
                                                                procedure_groups.REGION1[[i]]$SEX + 
                                                                procedure_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                                procedure_groups.REGION1[[i]]$SVYEAR + 
                                                                procedure_groups.REGION1[[i]]$BEDSIZE +
                                                                procedure_groups.REGION1[[i]]$RACE+
                                                                procedure_groups.REGION1[[i]]$OWNER+
                                                                procedure_groups.REGION1[[i]]$ESOP1+
                                                                procedure_groups.REGION1[[i]]$MARSTAT+
                                                                procedure_groups.REGION1[[i]]$REGION+
                                                                procedure_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                                procedure_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                                procedure_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                                procedure_groups.REGION1[[i]]$KT+
                                                                procedure_groups.REGION1[[i]]$PRECTOT+
                                                                procedure_groups.REGION1[[i]]$PS+
                                                                procedure_groups.REGION1[[i]]$QV2M+
                                                                procedure_groups.REGION1[[i]]$RH2M+
                                                                procedure_groups.REGION1[[i]]$T2M+
                                                                procedure_groups.REGION1[[i]]$T2M_RANGE+
                                                                procedure_groups.REGION1[[i]]$T2MWET+
                                                                procedure_groups.REGION1[[i]]$WS2M+
                                                                procedure_groups.REGION1[[i]]$mean.pollution+
                                                                procedure_groups.REGION1[[i]]$mean_solar_noon+
                                                                procedure_groups.REGION1[[i]]$mean_sunrise_time+
                                                                procedure_groups.REGION1[[i]]$mean_sunset_time+
                                                                procedure_groups.REGION1[[i]]$sd_sunset_time+
                                                                procedure_groups.REGION1[[i]]$sd_sunset_time+
                                                                procedure_groups.REGION1[[i]]$sd_sunset_time)}

for(i in 1:86){model_output_procedures_REGION1.n[[i]] <- lm(procedure_groups.REGION1[[i]]$DOC ~ procedure_groups.REGION1[[i]]$n + 
                                                              procedure_groups.REGION1[[i]]$AGE +
                                                              procedure_groups.REGION1[[i]]$SEX + 
                                                              procedure_groups.REGION1[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                              procedure_groups.REGION1[[i]]$SVYEAR + 
                                                              procedure_groups.REGION1[[i]]$BEDSIZE +
                                                              procedure_groups.REGION1[[i]]$RACE+
                                                              procedure_groups.REGION1[[i]]$OWNER+
                                                              procedure_groups.REGION1[[i]]$ESOP1+
                                                              procedure_groups.REGION1[[i]]$MARSTAT+
                                                              procedure_groups.REGION1[[i]]$REGION+
                                                              procedure_groups.REGION1[[i]]$ALLSKY_SFC_LW_DWN+
                                                              procedure_groups.REGION1[[i]]$ALLSKY_SFC_SW_DWN+
                                                              procedure_groups.REGION1[[i]]$ALLSKY_TOA_SW_DWN+
                                                              procedure_groups.REGION1[[i]]$KT+
                                                              procedure_groups.REGION1[[i]]$PRECTOT+
                                                              procedure_groups.REGION1[[i]]$PS+
                                                              procedure_groups.REGION1[[i]]$QV2M+
                                                              procedure_groups.REGION1[[i]]$RH2M+
                                                              procedure_groups.REGION1[[i]]$T2M+
                                                              procedure_groups.REGION1[[i]]$T2M_RANGE+
                                                              procedure_groups.REGION1[[i]]$T2MWET+
                                                              procedure_groups.REGION1[[i]]$WS2M+
                                                              procedure_groups.REGION1[[i]]$mean.pollution+
                                                              procedure_groups.REGION1[[i]]$mean_solar_noon+
                                                              procedure_groups.REGION1[[i]]$mean_sunrise_time+
                                                              procedure_groups.REGION1[[i]]$mean_sunset_time+
                                                              procedure_groups.REGION1[[i]]$sd_sunset_time+
                                                              procedure_groups.REGION1[[i]]$sd_sunset_time+
                                                              procedure_groups.REGION1[[i]]$sd_sunset_time)}

#plot it to see whether it is linear

#####Looking at differences in admission during months with a phase shift#####
#plot admission counts (y) against admission month (x)

ICD_9_code_groups[[1]]

ICD_9_code_groups[[27]] %>%
  filter(ADM_MON < 13) %>%
  ggplot(aes(x=(ADM_MON), y= (n)))+
  geom_point(alpha=1)+
  coord_cartesian(ylim= c(), xlim= c())+
  xlab("Admission month")+
  ylab("Admission count")+
  ggtitle("The relationship between admission month and admission count", "Infectious and paracitic diseases")




#comparing admission counts in march (and, separately, november/december) with the mean admission counts across the year
ICD_9_code_groups.admission.counts[[1]]$march <- ifelse(ICD_9_code_groups.admission.counts[[1]]$ADM_MON == 3, "1", "0")

ggplot(ICD_9_code_groups.admission.counts[[1]], aes(n)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ march)+
  scale_x_log10()


t.test(n ~ march, data=ICD_9_code_groups.admission.counts[[1]])

t.test(log(n) ~ march, data=ICD_9_code_groups.admission.counts[[1]])