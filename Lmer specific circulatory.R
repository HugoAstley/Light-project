#icd9 circulatory
#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
disease_model_DOC_circulatory <- list()

#The model. Multiple regression.
for(i in 1:length(ICD_9_code_groups_circulatory)){disease_model_DOC_circulatory[[i]] <- lmer(scale(ICD_9_code_groups_circulatory[[i]]$DOC) ~ scale(ICD_9_code_groups_circulatory[[i]]$Mean_of_Sunlight_Duration_minutes) + scale(ICD_9_code_groups_circulatory[[i]]$AGE) + (1|ICD_9_code_groups_circulatory[[i]]$SEX) + (1|ICD_9_code_groups_circulatory[[i]]$REGION),
                                                                               weights= ICD_9_code_groups_circulatory[[i]]$WEIGHT)
}



#########Different methods to get the coefficients###############

#get a summary of the list of outputs
summary_disease_model_DOC_circulatory <- list()
for(i in 1:length(disease_model_DOC_circulatory)){summary_disease_model_DOC_circulatory[[i]]<- summary(disease_model_DOC_circulatory[[i]])}



#extract coefficients
coefficients_disease_model_DOC_circulatory <- list()
for(i in 1:length(disease_model_DOC_circulatory)){coefficients_disease_model_DOC_circulatory[[i]] <- summary_disease_model_DOC_circulatory[[i]]$coefficients[,c(1)]}


#extract std. error values
se.values_disease_model_DOC_circulatory <- list()
for(i in 1:length(disease_model_DOC_circulatory)){se.values_disease_model_DOC_circulatory[[i]] <- summary_disease_model_DOC_circulatory[[i]]$coefficients[,c(2)]}


###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_circulatory)){coefficients_disease_model_DOC_circulatory[[i]] <- t(coefficients_disease_model_DOC_circulatory[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_circulatory)){coefficients_disease_model_DOC_circulatory[[i]] <- as.data.frame(coefficients_disease_model_DOC_circulatory[[i]])}
#Put them end to end
coefficients_disease_model_DOC_circulatory <- rbind.fill(coefficients_disease_model_DOC_circulatory)
coefficients_disease_model_DOC_circulatory$Diagnosis <- c(1:length(disease_model_DOC_circulatory))

#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_circulatory)){se.values_disease_model_DOC_circulatory[[i]] <- t(se.values_disease_model_DOC_circulatory[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_circulatory)){se.values_disease_model_DOC_circulatory[[i]] <- as.data.frame(se.values_disease_model_DOC_circulatory[[i]])}
#Put them end to end
se.values_disease_model_DOC_circulatory <- rbind.fill(se.values_disease_model_DOC_circulatory)
se.values_disease_model_DOC_circulatory$Diagnosis <- c(1:length(disease_model_DOC_circulatory))

coefficients.se.values_disease_model_DOC_circulatory <- merge(se.values_disease_model_DOC_circulatory, coefficients_disease_model_DOC_circulatory, by = "Diagnosis")

colnames(coefficients.se.values_disease_model_DOC_circulatory) <- c("Diagnoses", "Intercept", "SE", "Age_coefficient", "Intercept1", "Sun_coefficient", "Age_coefficient1")




coefficients_for_plot <- coefficients.se.values_disease_model_DOC_circulatory[,c(1,3,6)] #select the columns that have the diagnoses, coefficient and the SE



#Get the confidence interval from the SE
coefficients_for_plot$confidence <- (coefficients_for_plot$SE*1.96)





#Name the rows
coefficients_for_plot[,1] <- c(
  "Acute_rheumatic_fever", 
  "Chronic_rheumatic_heart_disease",
  "Hypertensive_disease", 
  "Ischemic_heart_disease", 
  "Diseases_of_pulmonary_circulation", 
  "Other_heart_diseases", 
  "Cerebrovascular_disease", 
  "Diseases_of_arteries_and_capillaries", 
  "Diseases_of_veins_and_lymphatics_and_other") 




####
coefficients_for_plot_positive <- coefficients_for_plot[coefficients_for_plot$Sun_coefficient > "0",]

ggplot(data= coefficients_for_plot_positive, aes(x = factor(reorder(coefficients_for_plot_positive$Diagnoses, -coefficients_for_plot_positive$Sun_coefficient)), y = coefficients_for_plot_positive$Sun_coefficient, ymin = (coefficients_for_plot_positive$Sun_coefficient-coefficients_for_plot_positive$confidence), ymax = (coefficients_for_plot_positive$Sun_coefficient+coefficients_for_plot_positive$confidence), colour = coefficients_for_plot_positive$Sun_coefficient > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_positive$Sun_coefficient, digits=4))), hjust = -0.2)+
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


coefficients_for_plot_negative_significant <- coefficients_for_plot[(coefficients_for_plot$Sun_coefficient+coefficients_for_plot$confidence) < "0",]

ggplot(coefficients_for_plot_negative_significant, aes(x = factor(reorder(coefficients_for_plot_negative_significant$Diagnoses, -coefficients_for_plot_negative_significant$Sun_coefficient)), y = coefficients_for_plot_negative_significant$Sun_coefficient, ymin = (coefficients_for_plot_negative_significant$Sun_coefficient-coefficients_for_plot_negative_significant$confidence), ymax = (coefficients_for_plot_negative_significant$Sun_coefficient+coefficients_for_plot_negative_significant$confidence), colour = coefficients_for_plot_negative_significant$Sun_coefficient > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_negative_significant$Sun_coefficient, digits=4))), hjust = -0.2)+
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


coefficients_for_plot_negative <- coefficients_for_plot[coefficients_for_plot$Sun_coefficient < "0",]
coefficients_for_plot_negative_notsignificant <- coefficients_for_plot_negative[(coefficients_for_plot_negative$Sun_coefficient+coefficients_for_plot_negative$confidence) >= "0",]

ggplot(coefficients_for_plot_negative_notsignificant, aes(x = factor(reorder(coefficients_for_plot_negative_notsignificant$Diagnoses, -coefficients_for_plot_negative_notsignificant$Sun_coefficient)), y = coefficients_for_plot_negative_notsignificant$Sun_coefficient, ymin = (coefficients_for_plot_negative_notsignificant$Sun_coefficient-coefficients_for_plot_negative_notsignificant$confidence), ymax = (coefficients_for_plot_negative_notsignificant$Sun_coefficient+coefficients_for_plot_negative_notsignificant$confidence), colour = coefficients_for_plot_negative_notsignificant$Sun_coefficient > 0))+
  scale_colour_manual(name = 'PC1 > 0', values = setNames(c('skyblue','grey'),c(T, F)))+
  geom_errorbar()+
  geom_point()+
  geom_text(aes(label = paste(round(coefficients_for_plot_negative_notsignificant$Sun_coefficient, digits=4))), hjust = -0.2)+
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