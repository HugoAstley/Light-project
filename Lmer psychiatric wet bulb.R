#icd9 psychiatric
#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
disease_model_DOC_psychiatric <- list()

#The model. Multiple regression.
for(i in 1:length(ICD_9_code_groups_psychiatric)){disease_model_DOC_psychiatric[[i]] <- lmer(scale(ICD_9_code_groups_psychiatric[[i]]$DOC) ~ scale(ICD_9_code_groups_psychiatric[[i]]$T2MWET) + scale(ICD_9_code_groups_psychiatric[[i]]$AGE) + (1|ICD_9_code_groups_psychiatric[[i]]$SEX) + (1|ICD_9_code_groups_psychiatric[[i]]$REGION),
                                                                                             weights= ICD_9_code_groups_psychiatric[[i]]$WEIGHT)
}



#########Different methods to get the coefficients###############

#get a summary of the list of outputs
summary_disease_model_DOC_psychiatric <- list()
for(i in 1:length(disease_model_DOC_psychiatric)){summary_disease_model_DOC_psychiatric[[i]]<- summary(disease_model_DOC_psychiatric[[i]])}



#extract coefficients
coefficients_disease_model_DOC_psychiatric <- list()
for(i in 1:length(disease_model_DOC_psychiatric)){coefficients_disease_model_DOC_psychiatric[[i]] <- summary_disease_model_DOC_psychiatric[[i]]$coefficients[,c(1)]}


#extract std. error values
se.values_disease_model_DOC_psychiatric <- list()
for(i in 1:length(disease_model_DOC_psychiatric)){se.values_disease_model_DOC_psychiatric[[i]] <- summary_disease_model_DOC_psychiatric[[i]]$coefficients[,c(2)]}


###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_psychiatric)){coefficients_disease_model_DOC_psychiatric[[i]] <- t(coefficients_disease_model_DOC_psychiatric[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_psychiatric)){coefficients_disease_model_DOC_psychiatric[[i]] <- as.data.frame(coefficients_disease_model_DOC_psychiatric[[i]])}
#Put them end to end
coefficients_disease_model_DOC_psychiatric <- rbind.fill(coefficients_disease_model_DOC_psychiatric)
coefficients_disease_model_DOC_psychiatric$Diagnosis <- c(1:length(disease_model_DOC_psychiatric))

#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_psychiatric)){se.values_disease_model_DOC_psychiatric[[i]] <- t(se.values_disease_model_DOC_psychiatric[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_psychiatric)){se.values_disease_model_DOC_psychiatric[[i]] <- as.data.frame(se.values_disease_model_DOC_psychiatric[[i]])}
#Put them end to end
se.values_disease_model_DOC_psychiatric <- rbind.fill(se.values_disease_model_DOC_psychiatric)
se.values_disease_model_DOC_psychiatric$Diagnosis <- c(1:length(disease_model_DOC_psychiatric))

coefficients.se.values_disease_model_DOC_psychiatric <- merge(se.values_disease_model_DOC_psychiatric, coefficients_disease_model_DOC_psychiatric, by = "Diagnosis")

colnames(coefficients.se.values_disease_model_DOC_psychiatric) <- c("Diagnoses", "Intercept", "SE", "Age_coefficient", "Intercept1", "wettemp_coef", "Age_coefficient1")




coefficients_for_plot <- coefficients.se.values_disease_model_DOC_psychiatric[,c(1,3,6)] #select the columns that have the diagnoses, coefficient and the SE



#Get the confidence interval from the SE
coefficients_for_plot$confidence <- (coefficients_for_plot$SE*1.96)





#Name the rows
coefficients_for_plot[,1] <- c("Organic_psychotic_conditions.short",
                               "Senile_and_presenile_organic_psychotic_conditions.short",
                               "Alcoholic_psychoses.short",
                               "Drug_psychoses.short",
                               "Transient_organic_psychotic_conditions.short",
                               "Other_organic_psychotic_conditions_chronic.short",
                               "Other_psychoses.short", 
                               "Schizophrenic_psychoses.short", 
                               "Affective_psychoses.short", 
                               "Paranoid_states.short", 
                               "Other_nonorganic_psychoses.short", 
                               "Psychoses_with_origin_specific_to_childhood.short", 
                               "Neurotic_disorders_personality_disorders_and_nonpsychotic_mental_disorders.short", 
                               "Neurotic_disorders.short", 
                               "Personality_disorders.short", 
                               "Sexual_deviations.short", 
                               "Psychoactive_substance.short", 
                               "Alcohol_dependence_syndrome.short", 
                               "Drug_dependence.short", 
                               "Nondependent_aduse_of_drugs.short", 
                               "Other_primarily_adult_onset.short", 
                               "Physiological_malfunction_arising_from_mental_factors.short", 
                               "Special_symptoms_or_syndromes_not_elsewhere_classified.short", 
                               "Acute_reaction_to_stress.short", 
                               "Adjustment_reaction.short", 
                               "Specific_nonpsychotic_mental_disorders_following_brain_damage.short", 
                               "Depressive_disorder_not_elsewhere_classified.short", 
                               "Mental_disorders_childhood.short", 
                               "Disturbance_of_conduct_not_elsewhere_classified.short", 
                               "Disturbance_of_emotions_specific_to_childhood_and_adolescence.short", 
                               "Hyperkinetic_syndrome_of_childhood.short", 
                               "Specific_delays_in_development.short", 
                               "Psychic_factors.short", 
                               "Mental_retardation.short")



####
coefficients_for_plot_positive <- coefficients_for_plot[coefficients_for_plot$wettemp_coef > "0",]

ggplot(data= coefficients_for_plot_positive, aes(x = factor(reorder(coefficients_for_plot_positive$Diagnoses, -coefficients_for_plot_positive$wettemp_coef)), y = coefficients_for_plot_positive$wettemp_coef, ymin = (coefficients_for_plot_positive$wettemp_coef-coefficients_for_plot_positive$confidence), ymax = (coefficients_for_plot_positive$wettemp_coef+coefficients_for_plot_positive$confidence), colour = coefficients_for_plot_positive$wettemp_coef > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_positive$wettemp_coef, digits=4))), hjust = -0.2)+
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


coefficients_for_plot_negative_significant <- coefficients_for_plot[(coefficients_for_plot$wettemp_coef+coefficients_for_plot$confidence) < "0",]

ggplot(coefficients_for_plot_negative_significant, aes(x = factor(reorder(coefficients_for_plot_negative_significant$Diagnoses, -coefficients_for_plot_negative_significant$wettemp_coef)), y = coefficients_for_plot_negative_significant$wettemp_coef, ymin = (coefficients_for_plot_negative_significant$wettemp_coef-coefficients_for_plot_negative_significant$confidence), ymax = (coefficients_for_plot_negative_significant$wettemp_coef+coefficients_for_plot_negative_significant$confidence), colour = coefficients_for_plot_negative_significant$wettemp_coef > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_negative_significant$wettemp_coef, digits=4))), hjust = -0.2)+
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


coefficients_for_plot_negative <- coefficients_for_plot[coefficients_for_plot$wettemp_coef < "0",]
coefficients_for_plot_negative_notsignificant <- coefficients_for_plot_negative[(coefficients_for_plot_negative$wettemp_coef+coefficients_for_plot_negative$confidence) >= "0",]

ggplot(coefficients_for_plot_negative_notsignificant, aes(x = factor(reorder(coefficients_for_plot_negative_notsignificant$Diagnoses, -coefficients_for_plot_negative_notsignificant$wettemp_coef)), y = coefficients_for_plot_negative_notsignificant$wettemp_coef, ymin = (coefficients_for_plot_negative_notsignificant$wettemp_coef-coefficients_for_plot_negative_notsignificant$confidence), ymax = (coefficients_for_plot_negative_notsignificant$wettemp_coef+coefficients_for_plot_negative_notsignificant$confidence), colour = coefficients_for_plot_negative_notsignificant$wettemp_coef > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_negative_notsignificant$wettemp_coef, digits=4))), hjust = -0.2)+
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
