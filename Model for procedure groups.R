#Convert the catagorical variables into factors
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$RACE <- factor(procedure_groups[[i]]$RACE)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$OWNER <- factor(procedure_groups[[i]]$OWNER)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$SEX <- factor(procedure_groups[[i]]$SEX)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$ESOP1 <- factor(procedure_groups[[i]]$ESOP1)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$MARSTAT <- factor(procedure_groups[[i]]$MARSTAT)}
for(i in 1:length(procedure_groups)){procedure_groups[[i]]$REGION <- factor(procedure_groups[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output_all_diagnoses.DOC <- list()

#The model. Multiple regression.
for(i in 1:17){model_output_all_diagnoses.DOC[[i]] <- lm(procedure_groups[[i]]$DOC ~procedure_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                           procedure_groups[[i]]$n + 
                                                           procedure_groups[[i]]$AGE +
                                                           procedure_groups[[i]]$SVYEAR + 
                                                           procedure_groups[[i]]$BEDSIZE +
                                                           procedure_groups[[i]]$REGION+
                                                           procedure_groups[[i]]$PRECTOT+
                                                           procedure_groups[[i]]$AQI+
                                                           procedure_groups[[i]]$PS+
                                                           procedure_groups[[i]]$RH2M+
                                                           procedure_groups[[i]]$T2MWET+
                                                           procedure_groups[[i]]$WS2M+
                                                           procedure_groups[[i]]$OWNER+
                                                           procedure_groups[[i]]$RACE+
                                                           procedure_groups[[i]]$ESOP1+
                                                           procedure_groups[[i]]$MARSTAT)}

#get a summary of the list of outputs
summary_model_output_all_diagnoses.DOC <- list()
for(i in 1:17){summary_model_output_all_diagnoses.DOC[[i]]<- summary(model_output_all_diagnoses.DOC[[i]])}


#extract coefficients
coefficients_model_output_all_diagnoses.DOC <- list()
for(i in 1:17){coefficients_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(1)]}

#extract p values
p.values_model_output_all_diagnoses.DOC <- list()
for(i in 1:17){p.values_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(4)]}



###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:17){coefficients_model_output_all_diagnoses.DOC[[i]] <- t(coefficients_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:17){coefficients_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(coefficients_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
coefficients_model_output_all_diagnoses.DOC <- rbind.fill(coefficients_model_output_all_diagnoses.DOC)
coefficients_model_output_all_diagnoses.DOC$Diagnosis <- c(1:17)

#Transpose so that you can bind by ROWS
for(i in 1:17){p.values_model_output_all_diagnoses.DOC[[i]] <- t(p.values_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:17){p.values_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(p.values_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
p.values_model_output_all_diagnoses.DOC <- rbind.fill(p.values_model_output_all_diagnoses.DOC)
p.values_model_output_all_diagnoses.DOC$Diagnosis <- c(1:17)

coefficients.p.values_model_output_all_diagnoses.DOC <- merge(p.values_model_output_all_diagnoses.DOC, coefficients_model_output_all_diagnoses.DOC, by = "Diagnosis")



#Names#
#Colnames
colnames(coefficients.p.values_model_output_all_diagnoses.DOC) <- c("Diagnosis group",
                                                                    "Intercept.pvalue",
                                                                    "Sunlight duration.pvalue",
                                                                    "Admission number.pvalue",
                                                                    "Age.pvalue",
                                                                    "Year.pvalue",
                                                                    "Bedsize.pvalue",
                                                                    "REGION2.pvalue",
                                                                    "REGION3.pvalue",
                                                                    "REGION4.pvalue",
                                                                    "Precipitation.pvalue",
                                                                    "Pollution.pvalue",
                                                                    "Pressure.pvalue",
                                                                    "Relative humidity.pvalue",
                                                                    "Wet bulb temperature.pvalue",
                                                                    "Wind speed.pvalue",
                                                                    "Government owned.pvalue",
                                                                    "Nonprofit owned.pvalue",
                                                                    "Black.pvalue",
                                                                    "American Indian/ Alaskan Native.pvalue",
                                                                    "Asian.pvalue",
                                                                    "Native Hawaiian/Other Islander.pvalue",
                                                                    "Other race.pvalue",
                                                                    "Multiple.pvalue",
                                                                    "Race not stated.pvalue",
                                                                    "Medicare.pvalue",
                                                                    "Medicade.pvalue",
                                                                    "Other government.pvalue",
                                                                    "Blue cross.pvalue",
                                                                    "HMO.pvalue",
                                                                    "Other private.pvalue",
                                                                    "Self-pay.pvalue",
                                                                    "No charge.pvalue",
                                                                    "Other payment.pvalue",
                                                                    "Payment not stated.pvalue",
                                                                    "Single.pvalue",
                                                                    "Widowed.pvalue",
                                                                    "Divorced.pvalue",
                                                                    "Separated.pvalue",
                                                                    "Unkown.pvalue",
                                                                    "Status not stated.pvalue",
                                                                    "Intercept",
                                                                    "Sunlight duration",
                                                                    "Admission number",
                                                                    "Age",
                                                                    "Year",
                                                                    "Bedsize",
                                                                    "REGION2",
                                                                    "REGION3",
                                                                    "REGION4",
                                                                    "Precipitation",
                                                                    "Pollution",
                                                                    "Pressure",
                                                                    "Relative humidity",
                                                                    "Wet bulb temperature",
                                                                    "Wind speed",
                                                                    "Government owned",
                                                                    "Nonprofit owned",
                                                                    "Black",
                                                                    "American Indian/ Alaskan Native",
                                                                    "Asian",
                                                                    "Native Hawaiian/Other Islander",
                                                                    "Other race",
                                                                    "Multiple",
                                                                    "Race not stated",
                                                                    "Medicare",
                                                                    "Medicade",
                                                                    "Other government",
                                                                    "Blue cross",
                                                                    "HMO",
                                                                    "Other private",
                                                                    "Self-pay",
                                                                    "No charge",
                                                                    "Other payment",
                                                                    "Payment not stated",
                                                                    "Single",
                                                                    "Widowed",
                                                                    "Divorced",
                                                                    "Separated",
                                                                    "Unkown",
                                                                    "Status not stated")



#Diagnosis names
coefficients.p.values_model_output_all_diagnoses.DOC[,1] <- c("Operations_on_the_nervous_system",
                        "Operations_on_the_endocrine_system",
                         "Operations_on_the_eye",
                         "Operations_on_the_ear",
                         "Operations_on_the_nose_mouth_pharynx",
                         "Operations_on_the_respiratory_system",
                         "Operations_on_the_cardiovascular_system",
                         "Operations_on_the_hemic_and_lymphatic_system",
                         "Operations_on_the_digestive_system",
                         "Operations_on_the_urinary_system",
                         "Operations_on_the_male_genital_organs",
                         "Operations_on_the_female_genital_organs",
                         "Obstetrical_procedures",
                         "Operations_on_the_musculoskeletal_system",
                         "Operations_on_the_integumentary_system",
                         "Miscellaneous_diagnostic_and_therapeutic_procedures",
                         "Procedures_related_to_psyche"
)

coefficients.p.values_model_output_all_diagnoses.DOC <- merge(y= coefficients.p.values_model_output_all_diagnoses.DOC, x= Number.of.observations.procedure_groups, by.y="Diagnosis group", by.x= "Diagnosis", all.y= T)




#Function that will hide the p value columns
hide <- sapply(names(coefficients.p.values_model_output_all_diagnoses.DOC[3:42]), function(x) eval(parse(text=sub("_SUB_","`x`","_SUB_ = F"))),
               simplify=F,USE.NAMES=T)


#
format <- sapply(names(coefficients.p.values_model_output_all_diagnoses.DOC)[44:82],function(x)
{
  eval(parse(text=sub("_SUB_",paste0("`",x,".pvalue`"),"formatter(\"span\", style = ~ style(color = ifelse(_SUB_ < 0.05, \"black\", \"lightgrey\")))")))
},simplify=F,USE.NAMES = T)




#Plot the table
formattable::formattable(coefficients.p.values_model_output_all_diagnoses.DOC,
                         align = c("l", "l", rep("r", NCOL(coefficients_model_output_all_diagnoses.DOC))),
                         c(hide, format))

