##Region
#Data1
demographicplot_REGION_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$REGION), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Northeast", "Midwest", "South", "West"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Region") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


demographicplot_REGION_data1

#Data2
demographicplot_REGION_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$REGION), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Northeast", "Midwest", "South", "West"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Region") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Psychiatric diagnoses
demographicplot_REGION_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$REGION), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Northeast", "Midwest", "South", "West"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Region") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_REGION_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$REGION), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Northeast", "Midwest", "South", "West"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Region") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 





####Method of payment (principal)####
#Data1
demographicplot_ESOP1_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$ESOP1), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Worker's comp", "Medicare", "Medicaid", "Other government", "Blue Cross/Shield", "HMO/PPO", "Other private", "Self-pay", "No charge", "Other", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Payment source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2
demographicplot_ESOP1_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$ESOP1), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Worker's comp", "Medicare", "Medicaid", "Other government", "Blue Cross/Shield", "HMO/PPO", "Other private", "Self-pay", "No charge", "Other", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Payment source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Psychiatric diagnoses
demographicplot_ESOP1_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$ESOP1), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Worker's comp", "Medicare", "Medicaid", "Other government", "Blue Cross/Shield", "HMO/PPO", "Other private", "Self-pay", "No charge", "Other", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Payment source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_ESOP1_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$ESOP1), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Worker's comp", "Medicare", "Medicaid", "Other government", "Blue Cross/Shield", "HMO/PPO", "Other private", "Self-pay", "No charge", "Other", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Payment source") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 




#####Marriage status#####
#Data1
demographicplot_MARSTAT_data1 <- data1 %>%
  ggplot(aes(x= as.factor(data1$MARSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Married", "Single", "Widowed", "Divorced", "Separated", "Unknown", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Marriage status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 

#Data2
demographicplot_MARSTAT_data2 <- data2 %>%
  ggplot(aes(x= as.factor(data2$MARSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Married", "Single", "Widowed", "Divorced", "Separated", "Unknown", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Marriage status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 



#Psychiatric diagnoses
demographicplot_MARSTAT_psychiatric <- psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(psychiatric_diagnoses$MARSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Married", "Single", "Widowed", "Divorced", "Separated", "Unknown", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Marriage status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60)) 


#Data2 without psychiatric diagnoses
demographicplot_MARSTAT_minuspsychiatric <- data2.minus.psychiatric_diagnoses %>%
  ggplot(aes(x= as.factor(data2.minus.psychiatric_diagnoses$MARSTAT), y = (((..count..)/sum(..count..)))*100))+
  geom_bar(alpha=0.7, color= "black", fill= "skyblue")+
  scale_x_discrete(labels = c("Married", "Single", "Widowed", "Divorced", "Separated", "Unknown", "Not stated"))+
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.background = element_rect(fill= "white"),
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"),
    panel.grid.minor.y = element_line(size = 0.5, linetype = 'solid',
                                      colour = "Grey"))+
  xlab("Marriage status") +
  ylab("Frequency (%)")+
  coord_cartesian(ylim = c(0, 60))




###Plot in one page
grid.arrange(demographicplot_REGION_data1, demographicplot_REGION_data2, demographicplot_REGION_psychiatric, demographicplot_REGION_minuspsychiatric,
             demographicplot_ESOP1_data1, demographicplot_ESOP1_data2, demographicplot_ESOP1_psychiatric, demographicplot_ESOP1_minuspsychiatric,
             demographicplot_MARSTAT_data1, demographicplot_MARSTAT_data2, demographicplot_MARSTAT_psychiatric, demographicplot_MARSTAT_minuspsychiatric,
             ncol=4)
