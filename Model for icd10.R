#Convert the catagorical variables into factors
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$RACE <- factor(ICD_10_code_groups[[i]]$RACE)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$OWNER <- factor(ICD_10_code_groups[[i]]$OWNER)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$SEX <- factor(ICD_10_code_groups[[i]]$SEX)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$ESOP1 <- factor(ICD_10_code_groups[[i]]$ESOP1)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$MARSTAT <- factor(ICD_10_code_groups[[i]]$MARSTAT)}
for(i in 1:length(ICD_10_code_groups)){ICD_10_code_groups[[i]]$REGION <- factor(ICD_10_code_groups[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
model_output_all_diagnoses.DOC <- list()

#The model. Multiple regression.
for(i in 1:5){model_output_all_diagnoses.DOC[[i]] <- lm(ICD_10_code_groups[[i]]$DOC ~ICD_10_code_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                           ICD_10_code_groups[[i]]$n + 
                                                           ICD_10_code_groups[[i]]$AGE +
                                                           ICD_10_code_groups[[i]]$SEX + 
                                                           ICD_10_code_groups[[i]]$SVYEAR + 
                                                           ICD_10_code_groups[[i]]$BEDSIZE +
                                                           ICD_10_code_groups[[i]]$REGION+
                                                           ICD_10_code_groups[[i]]$PRECTOT+
                                                           ICD_10_code_groups[[i]]$AQI+
                                                           ICD_10_code_groups[[i]]$PS+
                                                           ICD_10_code_groups[[i]]$RH2M+
                                                           ICD_10_code_groups[[i]]$T2MWET+
                                                           ICD_10_code_groups[[i]]$WS2M+
                                                           ICD_10_code_groups[[i]]$OWNER+
                                                           ICD_10_code_groups[[i]]$RACE+
                                                           ICD_10_code_groups[[i]]$ESOP1+
                                                           ICD_10_code_groups[[i]]$MARSTAT)}



#get a summary of the list of outputs
summary_model_output_all_diagnoses.DOC <- list()
for(i in 1:5){summary_model_output_all_diagnoses.DOC[[i]]<- summary(model_output_all_diagnoses.DOC[[i]])}


#extract coefficients
coefficients_model_output_all_diagnoses.DOC <- list()
for(i in 1:5){coefficients_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(1)]}

#extract p values
p.values_model_output_all_diagnoses.DOC <- list()
for(i in 1:5){p.values_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(4)]}



###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:5){coefficients_model_output_all_diagnoses.DOC[[i]] <- t(coefficients_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:5){coefficients_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(coefficients_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
coefficients_model_output_all_diagnoses.DOC <- rbind.fill(coefficients_model_output_all_diagnoses.DOC)
coefficients_model_output_all_diagnoses.DOC$Diagnosis <- c(1:5)

#Transpose so that you can bind by ROWS
for(i in 1:5){p.values_model_output_all_diagnoses.DOC[[i]] <- t(p.values_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:5){p.values_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(p.values_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
p.values_model_output_all_diagnoses.DOC <- rbind.fill(p.values_model_output_all_diagnoses.DOC)
p.values_model_output_all_diagnoses.DOC$Diagnosis <- c(1:5)

coefficients.p.values_model_output_all_diagnoses.DOC <- merge(p.values_model_output_all_diagnoses.DOC, coefficients_model_output_all_diagnoses.DOC, by = "Diagnosis")



#Names#
#Colnames
colnames(coefficients.p.values_model_output_all_diagnoses.DOC) <- c("Diagnosis group",
                                                                    "Intercept.pvalue",
                                                                    "Sunlight duration.pvalue",
                                                                    "Admission number.pvalue",
                                                                    "Age.pvalue",
                                                                    "Female.pvalue",
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
                                                                    "Female",
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
coefficients.p.values_model_output_all_diagnoses.DOC[,1] <- c(" Manic episode",
                                                              " Bipolar affective disorder",
                                                              " Depressive episode",
                                                              " Recurrent depressive disorder",
                                                              " Persistent mood affective disorders"
)

coefficients.p.values_model_output_all_diagnoses.DOC <- merge(y= coefficients.p.values_model_output_all_diagnoses.DOC, x= Number.of.observations.ICD_10_groups, by.y="Diagnosis group", by.x= "Diagnosis", all= T)



#Function that will hide the p value columns
hide <- sapply(names(coefficients.p.values_model_output_all_diagnoses.DOC[3:43]), function(x) eval(parse(text=sub("_SUB_","`x`","_SUB_ = F"))),
               simplify=F,USE.NAMES=T)


#
format <- sapply(names(coefficients.p.values_model_output_all_diagnoses.DOC)[44:84],function(x)
{
  eval(parse(text=sub("_SUB_",paste0("`",x,".pvalue`"),"formatter(\"span\", style = ~ style(color = ifelse(_SUB_ < 0.05, \"black\", \"lightgrey\")))")))
},simplify=F,USE.NAMES = T)




#Plot the table
formattable::formattable(coefficients.p.values_model_output_all_diagnoses.DOC,
                         align = c("l", rep("r", NCOL(coefficients_model_output_all_diagnoses.DOC))),
                         c(hide, format))



#####
##Round the numbers##
#Round the numbers
#Speed it up by identifying groups
coefficients_model_output_all_diagnoses.DOC$`ICD_10_code_groups[[i]]$n` <- round(coefficients_model_output_all_diagnoses.DOC$`ICD_10_code_groups[[i]]$n`,digits=2)

#Sort
coefficients.p.values_model_output_all_diagnoses.DOC <- coefficients.p.values_model_output_all_diagnoses.DOC[sort(coefficients.p.values_model_output_all_diagnoses.DOC$`Sunlight duration`),]

########