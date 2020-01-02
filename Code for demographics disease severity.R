##Admission source####

#Data1
demographicplot_ASOURCE_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$ASOURCE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Physician referral", "Clinical referral", "HMO referral", "Hospital", "Skilled nursing facility", "Other health facility", "Emergency room", "Court", "Other", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Data2
demographicplot_ASOURCE_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$ASOURCE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Physician referral", "Clinical referral", "HMO referral", "Hospital", "Skilled nursing facility", "Other health facility", "Emergency room", "Court", "Other", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Psychiatric diagnoses
demographicplot_ASOURCE_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$ASOURCE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Physician referral", "Clinical referral", "HMO referral", "Hospital", "Skilled nursing facility", "Other health facility", "Emergency room", "Court", "Other", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_ASOURCE_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$ASOURCE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Physician referral", "Clinical referral", "HMO referral", "Hospital", "Skilled nursing facility", "Other health facility", "Emergency room", "Court", "Other", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))



###Admission type####
#Data1
demographicplot_ADM_TYPE_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$ADM_TYPE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Emergency", "Urgent", "Elective", "Newborn", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission type") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Data2
demographicplot_ADM_TYPE_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$ADM_TYPE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Emergency", "Urgent", "Elective", "Newborn", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission type") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Psychiatric diagnoses
demographicplot_ADM_TYPE_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$ADM_TYPE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Emergency", "Urgent", "Elective", "Newborn", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission type") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_ADM_TYPE_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$ADM_TYPE), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Emergency", "Urgent", "Elective", "Newborn", "Not available"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Admission type") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


####Discharge status####
#data1
demographicplot_DISCSTAT_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$DISCSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Routine", "Against medical advice", "Short-term facility", "Long-term facility", "Alive- not stated", "Dead", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Discharge status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Data2
demographicplot_DISCSTAT_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$DISCSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Routine", "Against medical advice", "Short-term facility", "Long-term facility", "Alive- not stated", "Dead", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Discharge status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Psychiatric diagnoses
demographicplot_DISCSTAT_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$DISCSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Routine", "Against medical advice", "Short-term facility", "Long-term facility", "Alive- not stated", "Dead", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Discharge status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_DISCSTAT_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$DISCSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Routine", "Against medical advice", "Short-term facility", "Long-term facility", "Alive- not stated", "Dead", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Discharge status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))





###Plot in one page
grid.arrange(demographicplot_ASOURCE_data1, demographicplot_ASOURCE_data2, demographicplot_ASOURCE_psychiatric, demographicplot_ASOURCE_minuspsychiatric,
             demographicplot_ADM_TYPE_data1, demographicplot_ADM_TYPE_data2, demographicplot_ADM_TYPE_psychiatric, demographicplot_ADM_TYPE_minuspsychiatric,
             demographicplot_DISCSTAT_data1, demographicplot_DISCSTAT_data2, demographicplot_DISCSTAT_psychiatric, demographicplot_DISCSTAT_minuspsychiatric,
             ncol=4)

