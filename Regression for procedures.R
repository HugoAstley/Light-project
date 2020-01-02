#####The model: for procedures#####
#Convert the catagorical variables into factors
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$RACE <- factor(procedure_groups[[i]]$RACE)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$OWNER <- factor(procedure_groups[[i]]$OWNER)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$SEX <- factor(procedure_groups[[i]]$SEX)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$ESOP1 <- factor(procedure_groups[[i]]$ESOP1)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$ADM_TYPE <- factor(procedure_groups[[i]]$ADM_TYPE)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$ASOURCE <- factor(procedure_groups[[i]]$ASOURCE)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$DISCSTAT <- factor(procedure_groups[[i]]$DISCSTAT)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$MARSTAT <- factor(procedure_groups[[i]]$MARSTAT)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$MARSTAT <- factor(procedure_groups[[i]]$MARSTAT)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$REGION <- factor(procedure_groups[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output <- list()

#The model. Multiple regression.
for(i in 1:86){model_output_procedure_groups.DOC[[i]] <- lm(procedure_groups[[i]]$DOC ~ procedure_groups[[i]]$n + 
                                                              procedure_groups[[i]]$AGE +
                                                              procedure_groups[[i]]$SEX + 
                                                              procedure_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                              procedure_groups[[i]]$SVYEAR + 
                                                              procedure_groups[[i]]$BEDSIZE +
                                                              procedure_groups[[i]]$RACE+
                                                              procedure_groups[[i]]$OWNER+
                                                              procedure_groups[[i]]$ESOP1+
                                                              procedure_groups[[i]]$MARSTAT+
                                                              procedure_groups[[i]]$REGION+
                                                              procedure_groups[[i]]$ALLSKY_SFC_LW_DWN+
                                                              procedure_groups[[i]]$ALLSKY_SFC_SW_DWN+
                                                              procedure_groups[[i]]$ALLSKY_TOA_SW_DWN+
                                                              procedure_groups[[i]]$KT+
                                                              procedure_groups[[i]]$PRECTOT+
                                                              procedure_groups[[i]]$PS+
                                                              procedure_groups[[i]]$QV2M+
                                                              procedure_groups[[i]]$RH2M+
                                                              procedure_groups[[i]]$T2M+
                                                              procedure_groups[[i]]$T2MWET+
                                                              procedure_groups[[i]]$WS2M+
                                                              procedure_groups[[i]]$mean.pollution)}


for(i in 1:86){model_output_procedure_groups.n[[i]] <- lm(procedure_groups[[i]]$n ~ procedure_groups[[i]]$DOC + 
                                                            procedure_groups[[i]]$AGE +
                                                            procedure_groups[[i]]$SEX + 
                                                            procedure_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                            procedure_groups[[i]]$SVYEAR + 
                                                            procedure_groups[[i]]$BEDSIZE +
                                                            procedure_groups[[i]]$RACE+
                                                            procedure_groups[[i]]$OWNER+
                                                            procedure_groups[[i]]$ESOP1+
                                                            procedure_groups[[i]]$MARSTAT+
                                                            procedure_groups[[i]]$REGION+
                                                            procedure_groups[[i]]$ALLSKY_SFC_LW_DWN+
                                                            procedure_groups[[i]]$ALLSKY_SFC_SW_DWN+
                                                            procedure_groups[[i]]$ALLSKY_TOA_SW_DWN+
                                                            procedure_groups[[i]]$KT+
                                                            procedure_groups[[i]]$PRECTOT+
                                                            procedure_groups[[i]]$PS+
                                                            procedure_groups[[i]]$QV2M+
                                                            procedure_groups[[i]]$RH2M+
                                                            procedure_groups[[i]]$T2MWET+
                                                            procedure_groups[[i]]$WS2M+
                                                            procedure_groups[[i]]$mean.pollution)}


#get a summary of the list of outputs

summary_model_output <- list()
for(i in 1:86){summary_model_output[[i]]<- summary(model_output[[i]])}


#bind the summaries together
condensed_summary_model_output <- list()
condensed_summary_model_output.Pvalues <- list()

for(i in 1:86){condensed_summary_model_output[[i]] <- summary_model_output[[i]]$coefficients[1:5,1]}
for(i in 1:86){condensed_summary_model_output.Pvalues[[i]] <- summary_model_output[[i]]$coefficients[1:5,4]}