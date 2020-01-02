library(multcomp)

##Creating the variables for the models




##Weighted model

##Data2
#DOC
model_data2_DOC <- lmer(data2$DOC ~ scale(data2$Mean_of_Sunlight_Duration_minutes) + scale(data2$AGE) + (1|data2$SEX) + (1|data2$REGION), weights= data2$WEIGHT)

#Number of diagnoses
model_data2_DIAGNOSn <- lmer(data2$DIAGNOSn ~ scale(data2$Mean_of_Sunlight_Duration_minutes) + scale(data2$AGE) + (1|data2$SEX) + (1|data2$REGION), weights= data2$WEIGHT)

#Number of procedures
model_data2_PROCEDUn <- lmer(data2$PROCEDUn ~ scale(data2$Mean_of_Sunlight_Duration_minutes) + scale(data2$AGE) + (1|data2$SEX) + (1|data2$REGION), weights= data2$WEIGHT)

#Proportion discharged home

#Proportion of elective admissions

##Psych patients
#DOC
model_psychiatric_diagnoses_DOC <- lmer(scale(psychiatric_diagnoses$DOC) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(psychiatric_diagnoses$AGE) + (1|psychiatric_diagnoses$SEX) + (1|psychiatric_diagnoses$REGION), weights= psychiatric_diagnoses$WEIGHT, control = lmerControl(optimizer ="Nelder_Mead"))

#Number of diagnoses
model_psychiatric_diagnoses_DIAGNOSn <- lmer(scale(psychiatric_diagnoses$DIAGNOSn) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(psychiatric_diagnoses$AGE) + (1|psychiatric_diagnoses$SEX) + (1|psychiatric_diagnoses$REGION), weights= psychiatric_diagnoses$WEIGHT)

#Number of procedures
model_psychiatric_diagnoses_PROCEDUn <- lmer(scale(psychiatric_diagnoses$PROCEDUn) ~ scale(psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(psychiatric_diagnoses$AGE) + (1|psychiatric_diagnoses$SEX) + (1|psychiatric_diagnoses$REGION), weights= psychiatric_diagnoses$WEIGHT)


##Nonpsych patients
#DOC
model_data2.minus.psychiatric_diagnoses_DOC <- lmer(scale(data2.minus.psychiatric_diagnoses$DOC) ~ scale(data2.minus.psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(data2.minus.psychiatric_diagnoses$AGE) + (1|data2.minus.psychiatric_diagnoses$SEX) + (1|data2.minus.psychiatric_diagnoses$REGION), weights= data2.minus.psychiatric_diagnoses$WEIGHT)

#Number of diagnoses
model_data2.minus.psychiatric_diagnoses_DIAGNOSn <- lmer(scale(data2.minus.psychiatric_diagnoses$DIAGNOSn) ~ scale(data2.minus.psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(data2.minus.psychiatric_diagnoses$AGE) + (1|data2.minus.psychiatric_diagnoses$SEX) + (1|data2.minus.psychiatric_diagnoses$REGION), weights= data2.minus.psychiatric_diagnoses$WEIGHT)

#Number of procedures
model_data2.minus.psychiatric_diagnoses_PROCEDUn <- lmer(scale(data2.minus.psychiatric_diagnoses$PROCEDUn) ~ scale(data2.minus.psychiatric_diagnoses$Mean_of_Sunlight_Duration_minutes) + scale(data2.minus.psychiatric_diagnoses$AGE) + (1|data2.minus.psychiatric_diagnoses$SEX) + (1|data2.minus.psychiatric_diagnoses$REGION), weights= data2.minus.psychiatric_diagnoses$WEIGHT)



#Graph the estimate with the confidence interval
tmp1 <- as.data.frame(confint(glht(model_psychiatric_diagnoses_DOC))$confint)
tmp2 <- as.data.frame(confint(glht(model_psychiatric_diagnoses_DIAGNOSn))$confint)
tmp3 <- as.data.frame(confint(glht(model_psychiatric_diagnoses_PROCEDUn))$confint)
tmp4 <- as.data.frame(confint(glht(model_data2.minus.psychiatric_diagnoses_DOC))$confint)
tmp5 <- as.data.frame(confint(glht(model_data2.minus.psychiatric_diagnoses_DIAGNOSn))$confint)
tmp6 <- as.data.frame(confint(glht(model_data2.minus.psychiatric_diagnoses_PROCEDUn))$confint)

tmp_collated <- rbind(tmp1[2,], tmp2[2,], tmp3[2,], tmp4[2,], tmp5[2,], tmp6[2,])

tmp_collated$Comparison <- rownames(tmp_collated)
tmp_collated$Group <- c("Psychiatric","Psychiatric","Psychiatric","Non-Psychiatric","Non-Psychiatric","Non-Psychiatric")

tmp_collated





ggplot(tmp_collated, aes(x = factor(Comparison), y = Estimate, ymin = lwr, ymax = upr))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(Estimate, digits=4))), hjust = -0.2) +
  geom_hline(yintercept= 0, linetype="dashed", colour="skyblue")+
  scale_x_discrete(name= "",
                   labels= c("DOC", "Diagnoses", "Procedures", "DOC", "Diagnoses", "Procedures"))+
  facet_wrap(~Group, strip.position = "bottom", scales = "free_x") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside")





############ICD 9 diagnoses####################

#Convert the catagorical variables into factors
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$RACE <- factor(ICD_9_code_groups[[i]]$RACE)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$OWNER <- factor(ICD_9_code_groups[[i]]$OWNER)}
for(i in 1:length(ICD_9_code_groups.short)){ICD_9_code_groups.short[[i]]$SEX <- factor(ICD_9_code_groups.short[[i]]$SEX)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$ESOP1 <- factor(ICD_9_code_groups[[i]]$ESOP1)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$MARSTAT <- factor(ICD_9_code_groups[[i]]$MARSTAT)}
for(i in 1:length(ICD_9_code_groups)){ICD_9_code_groups[[i]]$REGION <- factor(ICD_9_code_groups[[i]]$REGION)}

#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
disease_model_DOC <- list()

#The model. Multiple regression.
for(i in 1:length(ICD_9_code_groups.short)){disease_model_DOC[[i]] <- lmer(scale(ICD_9_code_groups.short[[i]]$DOC) ~ scale(ICD_9_code_groups.short[[i]]$Mean_of_Sunlight_Duration_minutes) + scale(ICD_9_code_groups.short[[i]]$AGE) + (1|ICD_9_code_groups.short[[i]]$SEX) + (1|ICD_9_code_groups.short[[i]]$REGION),
                                             weights= ICD_9_code_groups.short[[i]]$WEIGHT)
}
  


#################################################################
#Graph the estimate with the confidence interval
tmp.list <- list()
for(i in 1:length(disease_model_DOC)){
tmp.list[[i]] <- as.data.frame(confint(glht(disease_model_DOC[[i]]))$confint)#extract
tmp.list[[i]] <- tmp.list[[i]][,2]#cut down to coloumn 2
}

tmp_collated <- do.call(rbind, tmp.list)#bind the rooms together
tmp_collated <- as.data.frame(tmp_collated)
tmp_collated$Comparison <- rownames(tmp_collated) #name it


tmp_collated


#########Different methods to get the coefficients###############

#get a summary of the list of outputs
summary_disease_model_DOC <- list()
for(i in 1:16){summary_disease_model_DOC[[i]]<- summary(disease_model_DOC[[i]])}



#extract coefficients
coefficients_disease_model_DOC <- list()
for(i in 1:16){coefficients_disease_model_DOC[[i]] <- summary_disease_model_DOC[[i]]$coefficients[,c(1)]}


#extract std. error values
se.values_disease_model_DOC <- list()
for(i in 1:16){se.values_disease_model_DOC[[i]] <- summary_disease_model_DOC[[i]]$coefficients[,c(2)]}


###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:16){coefficients_disease_model_DOC[[i]] <- t(coefficients_disease_model_DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:16){coefficients_disease_model_DOC[[i]] <- as.data.frame(coefficients_disease_model_DOC[[i]])}
#Put them end to end
coefficients_disease_model_DOC <- rbind.fill(coefficients_disease_model_DOC)
coefficients_disease_model_DOC$Diagnosis <- c(1:16)

#Transpose so that you can bind by ROWS
for(i in 1:16){se.values_disease_model_DOC[[i]] <- t(se.values_disease_model_DOC[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:16){se.values_disease_model_DOC[[i]] <- as.data.frame(se.values_disease_model_DOC[[i]])}
#Put them end to end
se.values_disease_model_DOC <- rbind.fill(se.values_disease_model_DOC)
se.values_disease_model_DOC$Diagnosis <- c(1:16)

coefficients.se.values_disease_model_DOC <- merge(se.values_disease_model_DOC, coefficients_disease_model_DOC, by = "Diagnosis")

colnames(coefficients.se.values_disease_model_DOC) <- c("Diagnoses", "Intercept", "SE", "Age_coefficient", "Intercept1", "Sun_coefficient", "Age_coefficient1")

summary_disease_model_DOC[[1]]


coefficients_for_plot <- coefficients.se.values_disease_model_DOC[,c(1,3,6)] #select the columns that have the diagnoses, coefficient and the SE

#Get the confidence interval from the SE
coefficients_for_plot$confidence <- (coefficients_for_plot$SE*1.96)


#Name the rows
coefficients_for_plot[,1] <- c("Organic_psychotic_conditions",
                                      "Senile_and_presenile_organic_psychotic_conditions",
                                      "Alcoholic_psychoses",
                                      "Drug_psychoses",
                                      "Transient_organic_psychotic_conditions",
                                      "Other_organic_psychotic_conditions_chronic",
                                      "Other_psychoses", 
                                      "Schizophrenic_psychoses", 
                                      "Affective_psychoses", 
                                      "Paranoid_states", 
                                      "Other_nonorganic_psychoses", 
                                      "Psychoses_with_origin_specific_to_childhood", 
                                      "Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders", 
                                      "Neurotic_disorders", 
                                      "Personality_disorders", 
                                      "Sexual_deviations", 
                                      "Psychoactive_substance", 
                                      "Alcohol_dependence_syndrome", 
                                      "Drug_dependence", 
                                      "Nondependent_aduse_of_drugs", 
                                      "Other_primarily_adult_onset", 
                                      "Physiological_malfunction_arising_from_mental_factors", 
                                      "Special_symptoms_or_syndromes_not_elsewhere_classified", 
                                      "Acute_reaction_to_stress", 
                                      "Adjustment_reaction", 
                                      "Specific_nonpsychotic_mental_disorders_following_brain_damage", 
                                      "Depressive_disorder_not_elsewhere_classified", 
                                      "Mental_disorders_childhood", 
                                      "Disturbance_of_conduct_not_elsewhere_classified", 
                                      "Disturbance_of_emotions_specific_to_childhood_and_adolescence", 
                                      "Hyperkinetic_syndrome_of_childhood", 
                                      "Specific_delays_in_development", 
                                      "Psychic_factors", 
                                      "Mental_retardation") 

################################################################










ggplot(coefficients_for_plot, aes(x = factor(reorder(coefficients_for_plot$Diagnoses, -coefficients_for_plot$Sun_coefficient)), y = coefficients_for_plot$Sun_coefficient, ymin = (coefficients_for_plot$Sun_coefficient-coefficients_for_plot$confidence), ymax = (coefficients_for_plot$Sun_coefficient+coefficients_for_plot$confidence), colour = coefficients_for_plot$Sun_coefficient > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot$Sun_coefficient, digits=4))), hjust = -0.2)+
  geom_hline(yintercept= 0, linetype="dashed", colour="skyblue")+
  scale_x_discrete(name= "")+
  scale_y_continuous(name= "Correlation coefficient with light duration")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")




#Plot the table
formattable::formattable(coefficients.p.values_disease_model_DOC,
                         align = c("l", "l", rep("r", NCOL(coefficients_disease_model_DOC))),
                         c(hide, format))








#######trash######
ggplot(data2.short, aes(x=data2.short$Mean_of_Sunlight_Duration_minutes, y= data2.short$DOC))+
  geom_point()+
  geom_smooth()+
  coord_cartesian(ylim= c(3,11))


test.model <- lmer(DOC ~ data2.short$Mean_of_Sunlight_Duration_minutes + data2.short$AGE + (data2.short$Mean_of_Sunlight_Duration_minutes|data2.short$REGION), data= data2.short)
coef(test.model)



tmp <- as.data.frame(confint(glht(test.model))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()


stargazer(test.model, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
