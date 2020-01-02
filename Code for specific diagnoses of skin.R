#icd9 skin
#get a list ready to recieve the outputs from the model (correlation coefficients for each disease)
disease_model_DOC_skin <- list()

#The model. Multiple regression.
for(i in 1:length(ICD_9_code_groups_skin)){disease_model_DOC_skin[[i]] <- lmer(scale(ICD_9_code_groups_skin[[i]]$DOC) ~ scale(ICD_9_code_groups_skin[[i]]$Mean_of_Sunlight_Duration_minutes) + scale(ICD_9_code_groups_skin[[i]]$AGE) + (1|ICD_9_code_groups_skin[[i]]$SEX) + (1|ICD_9_code_groups_skin[[i]]$REGION),
                                                                                             weights= ICD_9_code_groups_skin[[i]]$WEIGHT)
}



#########Different methods to get the coefficients###############

#get a summary of the list of outputs
summary_disease_model_DOC_skin <- list()
for(i in 1:length(disease_model_DOC_skin)){summary_disease_model_DOC_skin[[i]]<- summary(disease_model_DOC_skin[[i]])}



#extract coefficients
coefficients_disease_model_DOC_skin <- list()
for(i in 1:length(disease_model_DOC_skin)){coefficients_disease_model_DOC_skin[[i]] <- summary_disease_model_DOC_skin[[i]]$coefficients[,c(1)]}


#extract std. error values
se.values_disease_model_DOC_skin <- list()
for(i in 1:length(disease_model_DOC_skin)){se.values_disease_model_DOC_skin[[i]] <- summary_disease_model_DOC_skin[[i]]$coefficients[,c(2)]}


###Get them into one dataframe
#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_skin)){coefficients_disease_model_DOC_skin[[i]] <- t(coefficients_disease_model_DOC_skin[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_skin)){coefficients_disease_model_DOC_skin[[i]] <- as.data.frame(coefficients_disease_model_DOC_skin[[i]])}
#Put them end to end
coefficients_disease_model_DOC_skin <- rbind.fill(coefficients_disease_model_DOC_skin)
coefficients_disease_model_DOC_skin$Diagnosis <- c(1:length(disease_model_DOC_skin))

#Transpose so that you can bind by ROWS
for(i in 1:length(disease_model_DOC_skin)){se.values_disease_model_DOC_skin[[i]] <- t(se.values_disease_model_DOC_skin[[i]])}
#As dataframes so you can yse rbind.fill
for(i in 1:length(disease_model_DOC_skin)){se.values_disease_model_DOC_skin[[i]] <- as.data.frame(se.values_disease_model_DOC_skin[[i]])}
#Put them end to end
se.values_disease_model_DOC_skin <- rbind.fill(se.values_disease_model_DOC_skin)
se.values_disease_model_DOC_skin$Diagnosis <- c(1:length(disease_model_DOC_skin))

coefficients.se.values_disease_model_DOC_skin <- merge(se.values_disease_model_DOC_skin, coefficients_disease_model_DOC_skin, by = "Diagnosis")

colnames(coefficients.se.values_disease_model_DOC_skin) <- c("Diagnoses", "Intercept", "SE", "Age_coefficient", "Intercept1", "Sun_coefficient", "Age_coefficient1")




coefficients_for_plot <- coefficients.se.values_disease_model_DOC_skin[,c(1,3,6)] #select the columns that have the diagnoses, coefficient and the SE



#Get the confidence interval from the SE
coefficients_for_plot$confidence <- (coefficients_for_plot$SE*1.96)





#Name the rows
coefficients_for_plot[,1] <- c(
  "Infections_of_skin", 
  "Inflammatory_conditions_of_skin", 
  "Other_diseases_of_skin", 
  "Psoriasis_and_similar_disorders") 




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

#skin or whatever
#icd10 skin