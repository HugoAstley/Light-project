#Just the confounding variables
library(lme4)


###Test confounders: Region; SVyear; race
model_psychiatric_diagnoses_DOC <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$AGE) + psychiatric_diagnoses$SEX + (1|psychiatric_diagnoses$REGION), weights= psychiatric_diagnoses$WEIGHT)



#Just sunlight (for testing)
model_psychiatric_diagnoses_DOC <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + psychiatric_diagnoses$SEX + scale(psychiatric_diagnoses$AGE) + (1|psychiatric_diagnoses$SVYEAR), weights= psychiatric_diagnoses$WEIGHT)





####All the variables####
model_psychiatric_diagnoses_DOC_4AQI <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes)
                                             + scale(psychiatric_diagnoses$PS)  
                                             + scale(psychiatric_diagnoses$ALLSKY_SFC_SW_DWN) 
                                             + scale(psychiatric_diagnoses$T2MWET)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)

####All minus####
model_psychiatric_diagnoses_DOC_4sun <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$PS)  
                                             + scale(psychiatric_diagnoses$ALLSKY_SFC_SW_DWN) 
                                             + scale(psychiatric_diagnoses$AQI)
                                             + scale(psychiatric_diagnoses$T2MWET)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)



model_psychiatric_diagnoses_DOC_4ps <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes)
                                             + scale(psychiatric_diagnoses$ALLSKY_SFC_SW_DWN) 
                                             + scale(psychiatric_diagnoses$AQI)
                                             + scale(psychiatric_diagnoses$T2MWET)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)


model_psychiatric_diagnoses_DOC_4sw <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes)
                                             + scale(psychiatric_diagnoses$PS)  
                                             + scale(psychiatric_diagnoses$AQI)
                                             + scale(psychiatric_diagnoses$T2MWET)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)


model_psychiatric_diagnoses_DOC_4aqi <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes)
                                             + scale(psychiatric_diagnoses$PS)  
                                             + scale(psychiatric_diagnoses$ALLSKY_SFC_SW_DWN) 
                                             + scale(psychiatric_diagnoses$T2MWET)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)


model_psychiatric_diagnoses_DOC_4wet <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes)
                                             + scale(psychiatric_diagnoses$PS)  
                                             + scale(psychiatric_diagnoses$ALLSKY_SFC_SW_DWN) 
                                             + scale(psychiatric_diagnoses$AQI)
                                             + psychiatric_diagnoses$SEX 
                                             + scale(psychiatric_diagnoses$AGE) 
                                             + (1|psychiatric_diagnoses$SVYEAR)
                                             , weights= psychiatric_diagnoses$WEIGHT)





###Ranking of the models###
# based on f-test
anova(reduced_model, full_model)

#AIC
AIC(model_psychiatric_diagnoses_DOC_4sun,
    model_psychiatric_diagnoses_DOC_4ps,
    model_psychiatric_diagnoses_DOC_4sw,
    model_psychiatric_diagnoses_DOC_4aqi,
    model_psychiatric_diagnoses_DOC_4wet)

# based on chi-square test
anova(reduced_model, full_model, test = "LRT")