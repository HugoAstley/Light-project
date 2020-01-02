library(olsrr)

ICD_9_code_groups[[1]] <- as.tibble(ICD_9_code_groups[[1]])

model <- lm(DOC ~Mean_of_Sunlight_Duration_minutes +
              n + 
              AGE +
              SEX + 
              SVYEAR + 
              BEDSIZE +
              REGION+
              PRECTOT+
              AQI+
              PS+
              RH2M+
              T2MWET+
              WS2M+
              OWNER+
              RACE+
              ESOP1+
              MARSTAT, data = ICD_9_code_groups[[1]])


ols_step_all_possible(model)
view()



class(ICD_9_code_groups[[1]])




##########Just here to copy off##############
#Convert the catagorical variables into factors
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$RACE <- factor(ICD_9_code_groups[[i]]$RACE)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$OWNER <- factor(ICD_9_code_groups[[i]]$OWNER)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$SEX <- factor(ICD_9_code_groups[[i]]$SEX)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$ESOP1 <- factor(ICD_9_code_groups[[i]]$ESOP1)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$MARSTAT <- factor(ICD_9_code_groups[[i]]$MARSTAT)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$REGION <- factor(ICD_9_code_groups[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output_all_diagnoses.DOC <- list()

#The model. Multiple regression.
for(i in 1:86){model_output_all_diagnoses.DOC[[i]] <- lm(ICD_9_code_groups[[i]]$DOC ~ICD_9_code_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                           ICD_9_code_groups[[i]]$n + 
                                                           ICD_9_code_groups[[i]]$AGE +
                                                           ICD_9_code_groups[[i]]$SEX + 
                                                           ICD_9_code_groups[[i]]$SVYEAR + 
                                                           ICD_9_code_groups[[i]]$BEDSIZE +
                                                           ICD_9_code_groups[[i]]$REGION+
                                                           ICD_9_code_groups[[i]]$PRECTOT+
                                                           ICD_9_code_groups[[i]]$AQI+
                                                           ICD_9_code_groups[[i]]$PS+
                                                           ICD_9_code_groups[[i]]$RH2M+
                                                           ICD_9_code_groups[[i]]$T2MWET+
                                                           ICD_9_code_groups[[i]]$WS2M+
                                                           ICD_9_code_groups[[i]]$OWNER+
                                                           ICD_9_code_groups[[i]]$RACE+
                                                           ICD_9_code_groups[[i]]$ESOP1+
                                                           ICD_9_code_groups[[i]]$MARSTAT)}