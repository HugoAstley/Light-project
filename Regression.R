##Go back and rerun lm() with lme()

#A checker in case it has a sissy fit about there not being enough levels. When it comes to the whole of data 2 you should be able to use:
#1:86
ICD_9_code_groups[[7]][, sapply(ICD_9_code_groups[[7]], nlevels) > 0]


######How are different diagnoses affected by light######
######Before controlling for confounding variables, show the raw relationships#####


##Correlation coefficients for each diagnoses group: light and DOC

for(i in 1:length(ICD_9_code_groups)){print(cor.test(data=ICD_9_code_groups[[i]], ICD_9_code_groups[[i]]$Average, ICD_9_code_groups[[i]]$DOC))}




###Correlation coefficients for the relationship between admission count and the sunlight level###

for(i in 1:17){
  print(cor.test(x=ICD_9_code_groups.admission.counts[[i]]$Average, y=ICD_9_code_groups.admission.counts[[i]]$admission_count)
  )
}


###Correlation coefficients for the relationship of age with DOC

for(i in 1:17){print(cor.test(x=ICD_9_code_groups.REGION3.w.admission[[i]]$AGE, y=ICD_9_code_groups.REGION3.w.admission[[i]]$DOC))
}


###Correlation coefficients for the relationship of sex with DOC

for(i in 1:17){
  print(cor.test(x=ICD_9_code_groups.REGION3.w.admission[[i]]$SEX, y=ICD_9_code_groups.REGION3.w.admission[[i]]$DOC)
  )
}


###Correlation coefficients for the relationship of svyear with DOC


for(i in 1:17){
  print(cor.test(x=ICD_9_code_groups.REGION3.w.admission[[i]]$SVYEAR, y=ICD_9_code_groups.REGION3.w.admission[[i]]$DOC)
  )
}


###Correlation coefficients for the relationship of weight with DOC


for(i in 1:17){
  print(cor.test(x=ICD_9_code_groups.REGION3.w.admission[[i]]$WEIGHT, y=ICD_9_code_groups.REGION3.w.admission[[i]]$DOC)
  )
}


###Correlation coefficients for the relationship of admission count with DOC


for(i in 1:16){
  print(cor.test(x=ICD_9_code_groups.REGION3.w.admission[[i]]$admission_count, y=ICD_9_code_groups.REGION3.w.admission[[i]]$DOC)
  )
}







######Make a model that controls for the confounding variables#####

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

#get a summary of the list of outputs
summary_model_output_all_diagnoses.DOC <- list()
for(i in 1:86){summary_model_output_all_diagnoses.DOC[[i]]<- summary(model_output_all_diagnoses.DOC[[i]])}


#extract coefficients
coefficients_model_output_all_diagnoses.DOC <- list()
for(i in 1:86){coefficients_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(1)]}

#extract p values
p.values_model_output_all_diagnoses.DOC <- list()
for(i in 1:86){p.values_model_output_all_diagnoses.DOC[[i]] <- summary_model_output_all_diagnoses.DOC[[i]]$coefficients[,c(4)]}



###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:86){coefficients_model_output_all_diagnoses.DOC[[i]] <- t(coefficients_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:86){coefficients_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(coefficients_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
coefficients_model_output_all_diagnoses.DOC <- rbind.fill(coefficients_model_output_all_diagnoses.DOC)
coefficients_model_output_all_diagnoses.DOC$Diagnosis <- c(1:86)

#Transpose so that you can bind by ROWS
for(i in 1:86){p.values_model_output_all_diagnoses.DOC[[i]] <- t(p.values_model_output_all_diagnoses.DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:86){p.values_model_output_all_diagnoses.DOC[[i]] <- as.data.frame(p.values_model_output_all_diagnoses.DOC[[i]])}
#Put them end to end
p.values_model_output_all_diagnoses.DOC <- rbind.fill(p.values_model_output_all_diagnoses.DOC)
p.values_model_output_all_diagnoses.DOC$Diagnosis <- c(1:86)

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
coefficients.p.values_model_output_all_diagnoses.DOC[,1] <- c("Infectious and parasitic diseases", 
                                                              "Neoplasms", 
                                                              "Neoplasms of oral cavity",
                                                              "Neoplasms of digestive organs and peritoneum",
                                                              "Neoplasms of respiratory and intrathoracic organs",
                                                              "Neoplasms of bone connective tissue skin and breast",
                                                              "Kaposis sarcoma",
                                                              "Malignant neoplasms of genitourinary organs",
                                                              "Malignant neoplasms of other and unspecified sites",
                                                              "Malignant neoplasms of lymphatic and hematopoietic tissue",
                                                              "Benign neoplasms",
                                                              "Carcinoma in situ",
                                                              "Neoplasms of uncertain behavior",
                                                              "Neoplasms of unspecified nature",
                                                              "Malignant neoplasm of skin ",
                                                              "Endocrine metabolic immunity disorders", 
                                                              "Diseases of blood",
                                                              "Psychiatric diagnoses", 
                                                              "Organic psychotic conditions",
                                                              "Senile and presenile organic psychotic conditions",
                                                              "Alcoholic psychoses",
                                                              "Drug psychoses",
                                                              "Transient organic psychotic conditions",
                                                              "Other organic psychotic conditions chronic",
                                                              "Other psychoses",
                                                              "Schizophrenic psychoses",
                                                              "Affective psychoses",
                                                              "Paranoid states",
                                                              "Other nonorganic psychoses",
                                                              "Psychoses with origin specific to childhood",
                                                              "Neurotic disorders personality disorders and nonpsychotic mental disorders",
                                                              "Neurotic disorders",
                                                              "Personality disorders",
                                                              "Sexual deviations",
                                                              "Psychoactive substance",
                                                              "Alcohol dependence syndrome",
                                                              "Drug dependence",
                                                              "Nondependent aduse of drugs",
                                                              "Other primarily adult onset",
                                                              "Physiological malfunction arising from mental factors",
                                                              "Special symptoms or syndromes not elsewhere classified",
                                                              "Acute reaction to stress",
                                                              "Adjustment reaction",
                                                              "Specific nonpsychotic mental disorders following brain damage",
                                                              "Depressive disorder not elsewhere classified",
                                                              "Mental disorders childhood",
                                                              "Disturbance of conduct not elsewhere classified",
                                                              "Disturbance of emotions specific to childhood and adolescence",
                                                              "Hyperkinetic syndrome of childhood",
                                                              "Specific delays in development",
                                                              "Psychic factors",
                                                              "Mental retardation",
                                                              "Diseases of nervous system", 
                                                              "Diseases of circulatory system", 
                                                              "Acute rheumatic fever",
                                                              "Chronic rheumatic heart disease",
                                                              "Hypertensive disease",
                                                              "Ischemic heart disease",
                                                              "Diseases of pulmonary circulation",
                                                              "Other heart diseases",
                                                              "Cerebrovascular disease",
                                                              "Diseases of arteries and capillaries",
                                                              "Diseases of veins and lymphatics and other",
                                                              "diseases of respiratory system", 
                                                              "Acute respiratory infections",
                                                              "Other diseases of the upper respiratory tract",
                                                              "Pneumonia and influenza",
                                                              "Chronic obstructive pulmonary disease and allied conditions",
                                                              "Pneumoconioses and other lung diseases due to external agents",
                                                              "Other diseases of respiratory system",
                                                              "Diseases of digestive system", 
                                                              "Diseases of genitourinary system", 
                                                              "Nephritis nephrotic sydrome and nephrosis",
                                                              "Other diseases of urinary system",
                                                              "Diseases of breast",
                                                              "complicatoins of pregnancy and childbirth", 
                                                              "Diseases of skin", 
                                                              "Infections of skin",
                                                              "Inflammatory conditions of skin",
                                                              "Psoriasis and similar disorders",
                                                              "Other diseases of skin",
                                                              "Diseases of musculoskeletal and connective tissue", 
                                                              "Congenital anomalies", 
                                                              "Conditions of perinatal period", 
                                                              "Ill defined conditions", 
                                                              "Injury and poisoning"
)

coefficients.p.values_model_output_all_diagnoses.DOC <- merge(y= coefficients.p.values_model_output_all_diagnoses.DOC, x= Number.of.observations.ICD_9_groups, by.y="Diagnosis group", by.x= "Diagnosis", all.y= T)




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
                         align = c("l", "l", rep("r", NCOL(coefficients_model_output_all_diagnoses.DOC))),
                         c(hide, format))




######
for(i in 1:86){model_output_all_diagnoses.n[[i]] <- lm(ICD_9_code_groups[[i]]$n ~ ICD_9_code_groups[[i]]$DOC + 
                                                         ICD_9_code_groups[[i]]$AGE +
                                                         ICD_9_code_groups[[i]]$SEX + 
                                                         ICD_9_code_groups[[i]]$Mean_of_Sunlight_Duration_minutes +
                                                         ICD_9_code_groups[[i]]$SVYEAR + 
                                                         ICD_9_code_groups[[i]]$BEDSIZE +
                                                         ICD_9_code_groups[[i]]$RACE+
                                                         ICD_9_code_groups[[i]]$OWNER+
                                                         ICD_9_code_groups[[i]]$ESOP1+
                                                         ICD_9_code_groups[[i]]$MARSTAT+
                                                         ICD_9_code_groups[[i]]$REGION+
                                                         ICD_9_code_groups[[i]]$ALLSKY_SFC_LW_DWN+
                                                         ICD_9_code_groups[[i]]$ALLSKY_SFC_SW_DWN+
                                                         ICD_9_code_groups[[i]]$ALLSKY_TOA_SW_DWN+
                                                         ICD_9_code_groups[[i]]$KT+
                                                         ICD_9_code_groups[[i]]$PRECTOT+
                                                         ICD_9_code_groups[[i]]$PS+
                                                         ICD_9_code_groups[[i]]$QV2M+
                                                         ICD_9_code_groups[[i]]$RH2M+
                                                         ICD_9_code_groups[[i]]$T2M+
                                                         ICD_9_code_groups[[i]]$T2MWET+
                                                         ICD_9_code_groups[[i]]$WS2M+
                                                         ICD_9_code_groups[[i]]$mean.pollution)}








##Lots of work integrating the other data types. Fruition here?